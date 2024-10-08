;This name space contains functions for performing targeting model supply variations with multiple random runs.
(ns marathon.analysis.random
  (:require [marathon.analysis :as a]
            [marathon.analysis.util :as util]
            [marathon.analysis.experiment :as e]
            [marathon.analysis.nolh :as nolh]
            [marathon.ces.core :as c]
            [spork.util.table :as tbl]
            [spork.util.general :as gen]
            [clojure.spec.alpha :as s]))

;;number of threads to use for pmap.
(def ^:dynamic *threads* 4)
;;how many times a rep can fail before we toss the whole thing.
(def ^:dynamic *retries* 2)
;;probability of printing a message.
(def ^:dynamic *noisy* 0.5)

;;Default constant PRNG seed for reproducibility
(def +default-seed+ 42)

(defn individual-record
  "Given an index and an originating record,
   create a custom individual record the supply representing
   the nth unit of this record, where n <= (:Quantity rec)."
  [n rec]
  (assoc rec :Quantity 1 :Name (str n "_" (:SRC rec))))

(def ^:dynamic *expand-supply?* true)

(defn record->records
  "Given a base supply record rec, and an optional transformation function f,
  expands the supply record into n records, where n is in [0 ... Quantity]
  relative to the SupplyRecord's quantity. Each record is supplied to the
  transformation f to allow for custom processing of each generated record.
  Defaults to a transformation where quantity and unit names are altered to
  coerce a batch record into a sequence of individual unit records, but
  no other fields are changed.."
  ([rec f]
   (if *expand-supply?*
     (for [i (range (:Quantity rec))]
       (f (individual-record i rec)))
     ;;This was the easiest way to do an even lifecycle distribution
     ;;in the current pipeline.  This ignores f and just returns the
     ;;record.
     [rec]))
  ([rec] (record->records rec identity)))

;;this is brittle, but conforms to the original proof of concept.
;;we actually want to compute these from the project input, so
;;separating them out makes our job easier, while preserving
;;the option to supply them as args.
(def default-compo-lengths {"AC" 1095 "RC" 1825 "NG" 1825})

(defn ->cycletime-randomizer
  "Given an marathon.analyis.util/IGen instance, and a specification of component
  to cyclelengths, creates a function that transforms supply records into ones
  with random cycles generated using random draws from the supplied generator
  via marathon.analysis.util/gen-rand-int, which allows control over the seeds
  used vs. clojure.core/rand-int ."
  ([gen compo-lengths]
   (let [f (util/map-val compo-lengths  #(util/gen-rand-int gen %))]
     (fn cycletime-randomizer [{:keys [Component] :as r}]
       (let [ct (f Component)]
         (assoc r :CycleTime ct)))))
  ([gen] (->cycletime-randomizer gen default-compo-lengths)))

(defn rand-recs
  "Takes a supply record and creates multiple supply records with a supplied randomize
   function, of the type record -> record."
  ([rec randomize] (record->records rec randomize))
  ([rec]           (rand-recs rec identity)))

(defn add-transform
  "Add a project transformation to the project. The project will have
  a :after-split-transforms key where the value is a vector that
  contains a vector of [fn1 args-fn1 fn2 args-fn2 ... etc] where each
  function takes a project as the first argument and the corresponding
  args as the rest of the arguments."
  [{:keys [after-split-transform] :or
    {after-split-transform []} :as proj} f args]
  (assoc proj :after-split-transform
         (conj after-split-transform f args)))

(defn adjust-rc ;;new
  [rc-demand rec]
  (if (= (:DemandGroup rec) "RC_NonBOG-War")
    (assoc rec :Quantity rc-demand)
    rec))	

(defn rc-record
  "Returns the rc-record in the project.  Throw an error if we don't
  find an rc record because we need the rc record for
  rc-unavailability so we should have 1 RC record for every SRC, even
  if the quantity is 0.  The project has been filtered down to one SRC
  only at this point."
  [proj]
  (let [rec (->> proj
                 :tables
                 :SupplyRecords
                 tbl/table-records
                 (filter #(= "RC" (:Component %)))
                 first)]
    (if rec
      rec
      (throw (Exception. (str "There was no RC record."))))))

(defn get-rc-unavailable
  "Returns the :rc-unavailable from the SupplyRecord Tags.  If
  :rc-unavailable does not exist, throw an exception because we should
  have set that beforehand during preprocessing when calling this."
  [{:keys [rc-unavailable]}]
  (if rc-unavailable
    rc-unavailable
    ;;Careful!! This won't throw 
       (throw (Exception. (str ":rc-unavailable missing from
  SupplyRecord Tags.")))))

;;If RC supply is 1, RC available should be 1.
(defn cannibal-quantity
  "Given a percentage of RC unavailable as a ratio or decimal and the
  RC supply, return the quantity for the cannibalization demand.
  Current assumption is that if we have a fractional unit, we round
  unavailability up.  If the ratio says we should cannibalized all
  units, then cannibalize all but one.
  Moved this to a function so that we can make
  sure we are doing the same thing in taa.capacity and here."
  [unavail-percent rc-supply]
  (let [num-cannibals (* unavail-percent rc-supply)]
    ;;when rc-supply is 1, we cannibalize none
    (cond (= rc-supply 1) 0
          (and (pos? rc-supply) (< (- rc-supply num-cannibals) 1))
          ;;cannibalize all but one
          (- rc-supply 1)
          :else (Math/ceil num-cannibals))))

(defn adjust-cannibals
  "Adjust the cannibalization demand based on a percentage of
  uavailable RC supply."
  [proj]
  (let [{:keys [Quantity Tags]} (rc-record proj) 
        percent (get-rc-unavailable Tags)
        rc-demand (cannibal-quantity percent Quantity)]
    (a/change-records proj
                    (map #(adjust-rc rc-demand %))
                    :DemandRecords)))

(defn random-initials
  "Takes a project and makes a new project with random unit initial cycle times.
   If the project supplies a:supply-record-randomizer key associated to a
   function of supply-record -> supply-record, that function will be supplied
   as a transformation when generating random records.  Otherwise, no transformation
   will occur beyond the normal expansion of a batch supply record into multiple records
   with custom names."
  [proj supply-record-randomizer]
  (let [supply-record-randomizer (if supply-record-randomizer
                                   supply-record-randomizer
                                   identity)]
    (a/change-records proj
                    (mapcat #(rand-recs % supply-record-randomizer))
                    :SupplyRecords)))
  
;;it probably makes sense to fence out a general scheme to apply this pipeline
;;to demand records, policy records, etc.  There are probably many types of
;;transformations (or compiler passes) we'd like to apply.
;;this is really a random supply project.
(defn rand-proj
  "Apply multiple project transforms on the project from the functions
  and arguments supplied inside the project at the
  :after-split-transforms key."
  [{:keys [after-split-transform] :or
    {after-split-transform []} :as proj}]
  (let [func-tuples (partition 2 after-split-transform)]
    (reduce (fn [proj [func args]]
              (apply func
                     proj
                     args)) proj func-tuples)))

;;instead of adding fills and overlap and required, we just add
;;any fill information to not-ready. indicated units filling
;;these kinds of demands are considered unavailable and do not
;;count toward fill (e.g. cannibalized units), and their
;;demand requirement is not added to the total required.
(defn accumulate-not-ready
  [acc {:keys [ACFilled NGFilled RCFilled ACOverlap
               NGOverlap RCOverlap]}]
  (-> acc
      (update :AC-not-ready   #(+ % ACFilled ACOverlap))
      (update :NG-not-ready   #(+ % NGFilled NGOverlap))
      (update :RC-not-ready   #(+ % RCFilled RCOverlap))))

;;This is our normal fill stats accumulator, pulled out from the
;;original reduction.  By default, a demantrend record will
;;increment compo-relative fills proporationally, as well as the
;;total required demand.
(defn accumulate-fill
  [acc {:keys [ACFilled NGFilled RCFilled ACOverlap
               NGOverlap RCOverlap TotalRequired]}]
  (-> acc
      (update :AC-fill     #(+ % ACFilled))
      (update :NG-fill     #(+ % NGFilled))
      (update :RC-fill     #(+ % RCFilled))
      (update :AC-overlap  #(+ % ACOverlap))
      (update :NG-overlap  #(+ % NGOverlap))
      (update :RC-overlap  #(+ % RCOverlap))
      (update :total-quantity #(+ % TotalRequired))))

;;an extensible way to define demand groups that we consider
;;not-ready fills, in case we need to expand this in the future.
(def demandgroup-type {"RC_NonBOG-War" :not-ready})
;;determines if we consider this a normal fill, or if we should
;;funnel the fill stats elsewhere.
(defn normal-fill? [dtrend]
   (not (some-> :DemandGroup dtrend demandgroup-type (= :not-ready))))

;;Central accumulator for reduction in fill-stats.
(defn accumulate-fill-stats [acc dtrend]
  (if (normal-fill? dtrend)
    (accumulate-fill acc dtrend)        ;;fills -> deployed, inc total reqd.
    (accumulate-not-ready acc dtrend))) ;;fills -> not-ready

;;Decoupled from fill-stats.  Computes the initial record of fill stats
;;for the reduction to build on.
(defn initial-fill-state [ctx]
  (let [units        (c/units ctx)
        compos       (group-by :component units)
        compo-totals (reduce-kv (fn [acc k v]
                                  (assoc acc k (count v))) compos compos)
        compo-states (->> (for [[c xs] compos]
                            [c (frequencies (map util/state-key xs))])
                          (into {}))
        state-count  (fn [compo state]
                       (or (some-> (compo-states compo)
                                   (get state))
                           0))]
    {:AC-fill 0, :NG-fill 0, :RC-fill 0,
     :AC-overlap 0, :NG-overlap 0, :RC-overlap 0,
     :total-quantity 0
     :AC-deployable (state-count "AC" :deployable)
     :NG-deployable (state-count "NG" :deployable)
     :RC-deployable (state-count "RC" :deployable)
     :AC-not-ready  (state-count "AC" :not-ready)
     :NG-not-ready  (state-count "NG" :not-ready)
     :RC-not-ready  (state-count "RC" :not-ready)
     :AC-total      (compo-totals "AC" 0)
     :NG-total      (compo-totals "NG" 0)
     :RC-total      (compo-totals "RC" 0)
     :period        (c/current-period ctx)}))

(defn fill-stats
  "Computes time-veried fill statistics by component, producing a record of fills,
  deployables, not-ready, total required, and inventory for each sample. By
  default, demand trends will convert Filled, Overlap, and TotalRequired
  statistics into corresponding fields in the fill-stats record. Any demand with
  a DemandGroup value existing in the map
  marathon.analysis.random/demandgroup-type will instead have its fill stats
  added to the corresponding not-ready fields, and will not increase the total
  required demand, to model the effects of demands that absorb units without
  counting toward fill."
  [ctx]
  (->> ctx
       util/demand-trends-exhaustive
       (reduce accumulate-fill-stats (initial-fill-state ctx))))

(defn weighted-fill-stats
  "Truncated copy of this function from the marathon.analysis.experiment namespace.
  Takes a series of frames and gets the amount of time associated with them (dt)."
  [frames]
  (->> frames
       (map (fn [[t ctx]] (assoc (fill-stats ctx) :t t)))
       (gen/time-weighted-samples :t)))

(defn in-phase?
  "Tests if x is between a and b."
  [[a b] x]
  (and (>= x a) (<= x b)))

;;Not happy that this allows nil, but meh.
(defn phase-fill
  "Gets the amount of overlap between the intervals [x, x+d] and [a, b]."
  [[phase a b] [m x d]]
  (let [y (dec (+ x d))]
    (cond (and (<= x a) (in-phase? [a b] y))
             (assoc m :phase phase :dt (inc (- y a)))
          (and (in-phase? [a b] x) (>= y b))
            (assoc m :phase phase :dt (inc (- b x)))
          (and (in-phase? [a b] x) (in-phase? [a b] y))
            (assoc m :phase phase :dt (inc (- y x)))
          (and (<= x a) (>= y b))
            (assoc m :phase phase :dt (inc (- b a))))))

(defn phase-fill-stats
  "Gets the amount of overlap between the interval [x, x+d] and each phase."
  [phases [m x d]]
  (remove nil? (map phase-fill phases (repeat [m x d]))))

(defn weighted-phase-fill
  "Gets fill stats weighted by the duration of each frame (dt)."
  [{:keys [dt] :as fill}]
  (assoc fill
         :AC-fill          (* (:AC-fill fill) dt)
         :NG-fill          (* (:NG-fill fill) dt)
         :RC-fill          (* (:RC-fill fill) dt)
         :AC-overlap       (* (:AC-overlap fill) dt)
         :NG-overlap       (* (:NG-overlap fill) dt)
         :RC-overlap       (* (:RC-overlap fill) dt)
         :total-quantity   (* (:total-quantity fill) dt)
         :AC-deployable    (* (:AC-deployable fill) dt)
         :NG-deployable    (* (:NG-deployable fill) dt)
         :RC-deployable    (* (:RC-deployable fill) dt)
         :AC-not-ready     (* (:AC-not-ready fill) dt)
         :NG-not-ready     (* (:NG-not-ready fill) dt)
         :RC-not-ready     (* (:RC-not-ready fill) dt)
         :AC-total         (* (:AC-total fill) dt)
         :NG-total         (* (:NG-total fill) dt)
         :RC-total         (* (:RC-total fill) dt)))

(defn project->phase-fill
  "Takes a project and returns weighted fill stats by phase."
  [proj phases]
  (->> proj
       a/load-context
       a/as-stream
       weighted-fill-stats
       (mapcat phase-fill-stats (repeat phases))
       (map weighted-phase-fill)
       (group-by :phase)
       (map (fn [[phase xs]]
              {:phase            phase
               :AC-fill         (reduce + (map :AC-fill xs))
               :NG-fill         (reduce + (map :NG-fill xs))
               :RC-fill         (reduce + (map :RC-fill xs))
               :AC-overlap      (reduce + (map :AC-overlap xs))
               :NG-overlap      (reduce + (map :NG-overlap xs))
               :RC-overlap      (reduce + (map :RC-overlap xs))
               :total-quantity  (reduce + (map :total-quantity xs))
               :AC-deployable   (reduce + (map :AC-deployable xs))
               :NG-deployable   (reduce + (map :NG-deployable xs))
               :RC-deployable   (reduce + (map :RC-deployable xs))
               :AC-not-ready    (reduce + (map :AC-not-ready xs))
               :NG-not-ready    (reduce + (map :NG-not-ready xs))
               :RC-not-ready    (reduce + (map :RC-not-ready xs))
               :AC-total        (reduce + (map :AC-total xs))
               :NG-total        (reduce + (map :NG-total xs))
               :RC-total        (reduce + (map :RC-total xs))}))))

(defn min-samples
  "If lower and upper result in only one inventory, for instance,
  any upper times 0 is 0, we may want to explore some inventories
  above 0 or around the interval.  Hence, we can expand our interval
  to cover a minimum distance.  We try to split the distance evenly
  around the original interval."
  [[low-bound high-bound :as interval] min-distance]
  (let [interval-size (inc (- high-bound low-bound))
        ;;how far should we extend on either side?
        extension-amount (- min-distance interval-size)
        ;;more likely to cut than grow
        low-delta (long (Math/ceil (/ extension-amount 2)))
        new-low-bound (max (- low-bound low-delta) 0)
        ;;how much do we have left to go above high-bound?
        high-extension (- extension-amount
                          (- low-bound new-low-bound))]
  (if (< interval-size min-distance)
    ;;expand-interval
    [new-low-bound (+ high-bound high-extension)]
    interval)))
  
(defn bound->bounds
  "Return the lower and upper supply quantities given the initial
  supply and lower and upper proportions of that supply, ensuring
  that the returned lower and upper supply quantities
  cover at least min-distance supply quantities."
  [n [lower upper] & {:keys [min-distance] :or
                                        {min-distance 0}}]
  (let [bounds (if n
                 [(long (Math/floor (* lower n)))
                  (long (Math/ceil  (* upper n)))]
                 [0 0])]
    (min-samples bounds min-distance)))

(defn compute-spread-descending
  [levels low high]
  (let [delta (- high low)
        step  (/ delta (dec levels))]
    (if (<= step 1)
      (range high (dec low) -1)
      (map long (range high (dec low) (- step))))))

(defn ac-rc-supply-reduction-experiments  ;;new
  "This is a copy of this function from the marathon.analysis.experiment namespace.
  Upper and lower bounds have been modified so we can look at AC supply levels
  above the current inventory, and we account for RC supply levels as well."
  [tables lower upper & {:keys [levels step min-distance
                                lower-rc upper-rc] :or
                         {step 1
                          lower-rc lower
                          upper-rc upper}}]
  (let [init         (-> tables :SupplyRecords)
        groups       (-> init e/grouped-supply)
        [lowAC highAC]   (bound->bounds (-> "AC" groups :Quantity)
                                          [lower upper]
                                          :min-distance min-distance)
        [lowRC highRC]   (bound->bounds (-> "RC" groups :Quantity)
                                          [lower-rc upper-rc]
                                          :min-distance min-distance)]
    (cond 
      (and (not= lowAC highAC) (not= lowRC highRC))
        (for [n (compute-spread-descending (or levels (inc highAC)) lowAC highAC)
              m (compute-spread-descending (or levels (inc highRC)) lowRC highRC)
              :let [groups2 (-> (assoc-in groups ["AC" :Quantity] n)
                                (assoc-in ["RC" :Quantity] m))]]
          (->> groups2
               vals
               tbl/records->table
               (assoc tables :SupplyRecords)))
      (not= lowAC highAC)
        (for [n (compute-spread-descending (or levels (inc highAC)) lowAC highAC)]
          (->> (assoc-in groups ["AC" :Quantity] n)
               vals
               tbl/records->table
               (assoc tables :SupplyRecords)))
      (not= lowRC highRC)
        (for [n (compute-spread-descending (or levels (inc highRC)) lowRC highRC)]
          (->> (assoc-in groups ["RC" :Quantity] n)
               vals
               tbl/records->table
               (assoc tables :SupplyRecords)))
      :else     
      [tables])))

(defn ac-supply-reduction-experiments
  "This was the original function before we generalized for the ac and
  rc.  Now, it's a special case."
  [tables lower upper & {:keys [levels step] :or {step 1}}]
    (ac-rc-supply-reduction-experiments tables lower upper :levels
                                        levels
                                        :step step
                                        :min-distance 0
                                        :lower-rc 1
                                        :upper-rc 1))

(defn project->experiments-ac-rc  ;;new
  "We bind this to the dynamic var *projects->experiments* with a
  partial for the first three args so that we
  can look at rc and ac supply variations.  lower and upper are for
  the ac."
  [min-distance lower-rc upper-rc prj lower upper]
  (for [tbls (ac-rc-supply-reduction-experiments
              (:tables prj) lower upper
              :levels (:levels prj) :min-distance min-distance
              :lower-rc lower-rc :upper-rc upper-rc)]
    (assoc prj :tables tbls)))

(defn project->experiments
  "We bind this to the dynamic var *projects->experiments* so that we
  can look at ac supply variations."
  [prj lower upper & {:keys [levels step] :or {step 1}}]
  (project->experiments-ac-rc 0 1 1 prj lower upper))

(defn project->nolh-experiments
  "Constructs a series of supply variation experiments by using eithe a Nearly Orthogonal
   Latin Hypercube (NOLH) 65-level design, or if the empirical levels from the supply
   are < 65, does a full factorial design.  Like project->experiments, returns
   a sequence of projects with updated :tables for each new supply design."
  [prj lower upper]
  (let [tbls          (:tables prj)
        init-records  (-> prj :tables :SupplyRecords tbl/table-records)
        compo-records (group-by :Component init-records)
        _ (assert (every? #{1} (map count (vals compo-records)))
                  "Expected a single record per component for NOLH supply!")
        compo-records (zipmap (keys compo-records) (map first (vals compo-records)))
        init-supply   (into {} (map (juxt :Component :Quantity) init-records))
        bound-names   (for [[compo quantity] init-supply]
                        [compo [(long (* lower quantity))
                                (long (* upper quantity))]])]
    (->> bound-names
         nolh/designs
         (map (fn [r]
                (->> (for [[compo quantity] r]
                      (let [from (get compo-records  compo)
                            rec  (assoc from :Quantity quantity)]
                        rec))
                     tbl/records->table
                     (assoc tbls :SupplyRecords)
                     (assoc prj :tables)))))))

(defn project->full-factorial-experiments
  "Constructs a series of supply variation experiments by using full factorial
  design. Like project->experiments, returns a sequence of projects with
  updated :tables for each new supply design."
  [prj lower upper]
  (let [tbls          (:tables prj)
        init-records  (-> prj :tables :SupplyRecords tbl/table-records)
        compo-records (group-by :Component init-records)
        _ (assert (every? #{1} (map count (vals compo-records)))
                  "Expected a single record per component for NOLH supply!")
        compo-records (zipmap (keys compo-records) (map first (vals compo-records)))
        init-supply   (into {} (map (juxt :Component :Quantity) init-records))
        bound-names   (for [[compo quantity] init-supply]
                        [compo [(long (* lower quantity))
                                (long (* upper quantity))]])]
    (->> bound-names
         nolh/full-factorial-records
         (map (fn [r]
                (->> (for [[compo quantity] r]
                      (let [from (get compo-records  compo)
                            rec  (assoc from :Quantity quantity)]
                        rec))
                     tbl/records->table
                     (assoc tbls :SupplyRecords)
                     (assoc prj :tables)))))))

;;if we can't copmute a fill, we should log it somewhere.
(defn try-fill
  ([proj src idx phases]
   (loop [n *retries*]
     (let [_   (when (and *noisy*
                          (not (zero? *noisy*))
                          (or (= *noisy* 1.0)
                              (< (rand) *noisy*)))
                 (util/log [:trying src :level idx :rep (proj :rep)]))
           res (try (let [seed (:rep-seed proj)
                          fill (project->phase-fill (rand-proj proj) phases)]
                      (mapv #(assoc % :rep-seed seed)
                            (e/summary-availability-records src proj
                                                            fill)))
                    ;;If we are distributed (like with pmap!), the error won't
                    ;;throw on the host computer,  so we catch the
                    ;;exception and print it.
                    (catch Exception e (.getMessage e))
                    )]
       ;;Should be a sequence of records, but will be a string if it
       ;;was an error
       (cond
         (string? res) (if (pos? n)
                  (do (util/log {:retrying n :src src :idx idx})
                      (recur (dec n)))
                  (let [err {:error (str "unable to compute fill " src)
                             :src   src
                             :idx   idx
                             :reason res}
                        _    (util/log err)]
                    err))
         :else res)))))

(def ^:dynamic *project->experiments* marathon.analysis.random/project->experiments)

(defn total-supply [proj]
  (->> proj
       (:tables)
       (:SupplyRecords)
       (tbl/table-records)
       (map :Quantity)
       (reduce +)))

;;Number of reps are based on Sarah's rep analysis
(defn rep-count [ra+rc]
  (cond
    (> ra+rc 100) 10
    (> ra+rc 46) 20
    (> ra+rc 12) 30
    (> ra+rc 5) 80
    (> ra+rc 0) 100
    (zero? ra+rc) 1))

(defn portion-of-reps [portion proj]
  (->> (total-supply proj)
       (rep-count)
       (* portion)))

;;A replicator takes a proj and returns multiple projects for multiple reps.
;;Shifting a lot of these ops to eductions since that retains reducible
;;and seqable paths.
(defn repeat-proj
  "Examines the input project map for (in order of preference) a
   :replicator :: (proj->int), or :reps :: int to determine
   replications of proj.  Yields an eduction where the project
   (stripped of the replicator for ease of serialization)
   is repeated with an associated :rep entry corresponding to
   the replication count."
  [proj]
  (let [reps      (if-let [replicator (:replicator proj)]
                    (replicator proj)
                    (:reps proj))]
    (assert (number? reps)
            "expected reps produced by :reps or :replicator to be a number!")
    (->> (repeat reps (dissoc proj :replicator)) ;don't want to serialize replicator if distributed.
         (eduction (map-indexed (fn [idx proj] (assoc proj :rep idx)))))))

(defn repeat-projects [projects]
  (eduction (mapcat repeat-proj) projects))

;;slight api change, we were just inlining this runnnig locally
;;because no serialization.  Now we compute the rep-seed outside
;;and pass it along as data.  Also we now take a map to simplify life
;;vs vector args (simpler for the cluster side too).
(defn supply-experiment [{:keys [src phases seed->randomizer idx rep-seed] :as proj}]
  (let [supply-randomizer (seed->randomizer rep-seed)]
    (-> proj
        (assoc :supply-record-randomizer supply-randomizer )
        (add-transform random-initials [supply-randomizer])
        (try-fill src idx phases))))

(defn parallel-exec
  "Given a collection of supply designs of
   {:keys [src phases seed->randomizere idx rep-seed]}
   map supply-experiment in parallel.  Default binding for
   *exec-experiments* for simulation runs."
  [xs]  (util/pmap *threads* supply-experiment xs))

;;defines a hook for us to wire in cluster execution or local without
;;m4 knowing about it.  by default, we excute our pmap replacement
;;and return a lazy sequence of the results.
(def ^:dynamic *exec-experiments* parallel-exec)

#_ ;;pending, move to m4peer.
(defn exec-experiments [xs]
  (case *run-site*
    :local   (util/pmap! *threads* supply-experiment xs)
    :cluster (hd/fmap marathon.analysis.random/supply-experiment xs) ;;naive
    (throw (ex-info "unknown *run-site*" {:in *run-site*}))))

;;added exec-experiments hook to allow dry runs.  defaults to *exec-experiments*
;;binding.
(defn rand-target-model
  "Uses the target-model-par-av function from the marathon.analysis.experiment
  namespace as a base. This function is modified to perform a random run for
  each level of supply."
  [proj & {:keys [phases lower upper levels gen seed->randomizer exec-experiments]
           :or   {lower 0 upper 1 gen util/default-gen
                  seed->randomizer (constantly identity)}}]
  (let [project->experiments *project->experiments*
        exec-experiments     (or exec-experiments *exec-experiments*)]
     (->> (assoc proj :phases phases :lower lower :upper upper :levels levels
                 :gen gen  :seed->randomizer seed->randomizer)
          (e/split-project)
          (mapcat (fn [[src proj]] ;;generates seeded projects now with src info.
                    (->> (project->experiments (assoc proj :src src) lower upper)
                         (map-indexed (fn [idx proj] (assoc proj :idx idx)))
                         repeat-projects
                         (map #(assoc % :rep-seed (util/next-long gen))))))
          exec-experiments
          (apply concat))))
#_
(defn rand-target-model
  "Uses the target-model-par-av function from the marathon.analysis.experiment
  namespace as a base. This function is modified to perform a random run for
  each level of supply."
  [proj & {:keys [phases lower upper levels gen seed->randomizer]
           :or   {lower 0 upper 1 gen util/default-gen
                  seed->randomizer (fn [_] identity)}}]
  (let [project->experiments *project->experiments*]
     (->> (assoc proj :phases phases :lower lower :upper upper :levels levels
                 :gen gen  :seed->randomizer seed->randomizer)
          (e/split-project)
          (reduce
           (fn [acc [src proj]]
             (let [experiments
                   (repeat-projects (project->experiments proj lower upper))] ;;CHANGED
               (into acc
                     (filter (fn blah [x] (not (:error x))))
                     (util/pmap! *threads*
                                 (fn [[idx proj]]
                                   (let [rep-seed   (util/next-long
                                                     gen)
                                         supply-randomizer
                                         (seed->randomizer rep-seed)]
                                     (-> proj
                                         (assoc :rep-seed rep-seed
                                                :supply-record-randomizer
                                                supply-randomizer)
                                         (add-transform
                                          random-initials [supply-randomizer])
                                         (try-fill src idx phases))))
                                 (map-indexed vector experiments))))) [])
          (apply concat)
          vec)))

(defn default-randomizer [seed compo-lengths]
  (->cycletime-randomizer (util/->gen seed) compo-lengths))

(defn rand-cycles
  "Takes a project and make a new project with random unit initial
  cycle times and sets the seed."
  [proj & {:keys [seed compo-lengths] :or {seed (rand Long/MAX_VALUE)
                             compo-lengths default-compo-lengths}}]
  (let [seed->randomizer #(default-randomizer % compo-lengths)]
    (random-initials proj (seed->randomizer seed))))

;;specs to catch an error I ran into with empty phases..
(s/def ::phase  (s/tuple string? int? int?))
(s/def ::phases (s/+ ::phase))

(defn rand-runs
  "Runs replications of the rand-target-model function.
   Mid-level function meant to be invoked from higher-level APIs.
   Caller may supply
   :reps - int, number of random replications
   :phases - optional, sequence of [phase from to] :: [string int int],
             derived from PeriodRecords if nil
   :lower - lower bound for the supply variation multiplier, defaut 0.
   :upper - upper bound for the supply variation multipler, default 1.
   :seed - integer, random seed to use for all the replications, default +default-seed+.
   :compo-lengths optional, map of {compo cyclelength} used for distribution
                  random initial cycletimes, default default-compo-lengths ."
  [proj & {:keys [reps phases lower upper seed levels compo-lengths
                  seed->randomizer]
           :or   {lower 0 upper 1 seed +default-seed+
                  compo-lengths default-compo-lengths}}]
  ;;input validation, we probably should do more of this in general.
  (assert (s/valid? ::phases phases) (s/explain-str ::phases []))
  (let [seed->randomizer (or seed->randomizer
                             (fn [x] (default-randomizer x compo-lengths)))
        gen              (util/->gen seed)
        phases           (or phases (util/derive-phases proj))]
    (rand-target-model (assoc proj :reps reps)  ;;CHANGED
                       :phases phases :lower lower :upper upper
                       :gen   gen     :seed->randomizer seed->randomizer
                       :levels levels)))  ;;;CHANGED.

(defn rand-runs-ac-rc
  [min-samples lower-rc upper-rc proj & {:as optional-args}]
  (binding [*project->experiments* (partial project->experiments-ac-rc
                                            min-samples lower-rc upper-rc)]
    (apply rand-runs proj (mapcat identity optional-args))))

(def fields
  [:rep-seed :SRC :phase :AC-fill :NG-fill :RC-fill :AC-overlap :NG-overlap
   :RC-overlap :total-quantity :AC-deployable :NG-deployable :RC-deployable
   :AC-not-ready :NG-not-ready :RC-not-ready :AC-total :NG-total :RC-total])

(defn write-output [file-name results]
  (tbl/records->file results file-name :field-order fields))

(defn runtime [s]
  (let [h (quot s (* 60 60))
        m (quot (- s (* h 60 60)) 60)
        s (- s (* 60 60 h) (* 60 m))]
    [h m s]))

(defn run
  "High level entry point to produce random runs, with optional
   parameters for supply bounds by percentage, phases for output
   statistics, the number of random replications per level,
   cyclelengths to use for initial conditions by component,
   the number of levels (e.g. supply variations) to examine,
   and a global PRNG seed.  If a seed is not provided, will
   default to the constant +default-seed+, leading to
   consistent replications for the same inputs."
  [proj-or-path & {:keys [target lower upper phases reps
                          compo-lengths levels seed]
                   :or   {target "results.txt"
                          lower  0
                          upper  1.5
                          reps   1
                          seed +default-seed+}}]
  (let [proj    (a/load-project proj-or-path)
        t0      (System/currentTimeMillis)
        results (rand-runs proj :reps reps :phases phases :lower lower :upper upper
                           :levels levels :compo-lengths compo-lengths)
        _        (write-output target results)
        tf       (double (- (System/currentTimeMillis) t0))
        [h m s]  (runtime (/ tf 1000.0))]
    (println [:completed-in (str h "h:" m "m:" s "s") :wrote-to target])))

(defn random-run
  "Convenience function to produce arbitrary random replications without a
   consistent root PRNG seed.  This is useful for distributing runs across
   multiple machines, such that replications are independent and may be
   combined into a sample afterward."
  [proj-or-path & {:keys [target lower upper phases reps
                          compo-lengths levels seed]
                   :or   {target "results.txt"
                          lower  0
                          upper  1.5
                          reps   1}}]
  (let [seed (or seed (util/next-long (java.util.Random.)))]
    (run proj-or-path
      :target target :lower lower :upper upper :phases phases
      :levels levels :reps reps :compo-lengths compo-lengths :seed seed)))

(comment
  ;way to invoke functions
  (def path "~/repos/notional/supplyvariation-testdata.xlsx")
  (def proj (a/load-project path))
  (def phases [["comp" 1 821] ["phase-1" 822 967]])

  (def results (rand-runs proj :reps 1 :phases phases
                               :lower 0 :upper 1.5
                               :compo-lengths default-compo-lengths))

  (write-output "results.csv" results)

  (def seeds (->> results (filter (fn [{:keys [phase]}] (#{"comp"} phase))) (map :rep-seed)))
  (def results2 (rand-runs proj :reps 1 :phases phases
                           :lower 0 :upper 1.5
                           :compo-lengths default-compo-lengths))
  (def seeds2 (->> results (filter (fn [{:keys [phase]}] (#{"comp"} phase))) (map :rep-seed)))

  ;;move this into a deftest...
  (assert (or (= seeds seeds2)
              (= (sort seeds) (sort seeds2))))

  ;;We can just use run....and pass it high level args to control behavior.
  ;;This run will run 1 rep, degrading supply by 1, from a range of 1.5*initial AC supply,
  ;;to 0 AC, with the default component lengths to distribute random cycles by, using
  ;;a predefined set of phases to report results.
  (def run1
    (binding [*noisy* 1.0]
      (run "~/repos/notional/supplyvariation-testdata.xlsx"
        :reps 1
        :phases phases
        :lower 0 :upper 1.5
        :compo-lengths default-compo-lengths)))

  ;;This run will run 1 rep, degrading supply by 1, from a range of 1.5*initial AC supply,
  ;;to 0 AC, with the default component lengths to distribute random cycles by, using
  ;;a predefined set of phases to report results.  It will only perform 3 levels of
  ;;supply experiments, distributing the levels evenly across the range [lower*supply upper*supply].
  ;;It will perform 5 replications at each level.  All messages will be logged and
  ;;printed to output cleanly as well.
  (def run2
    (binding [*noisy* 1.0]
      (run "~/repos/notional/supplyvariation-testdata.xlsx"
        :reps 5
        :phases phases
        :lower 0 :upper 1.5
        :compo-lengths default-compo-lengths
        :levels 3)))

  ;;this will generate a different root seed every time, as opposed
  ;;to the previous 2 examples, which will always use +default-seed+
  ;;or 42, unless supplied.
  (def random-run-ex
    (binding [*noisy* 1.0]
      (random-run "~/repos/notional/supplyvariation-testdata.xlsx"
        :reps 5
        :phases phases
        :lower 0 :upper 1.5
        :compo-lengths default-compo-lengths
        :levels 3)))

  ;;This will change out the default ac supply reduction experiments.
  ;;We now leverage at most a 65-level NOLH design varying all
  ;;components.  At best, we do a full-factorial design of < 65.
  (def random-run-nolh
    (binding [*noisy* 1.0
              *project->experiments* project->nolh-experiments]
      (random-run "~/repos/notional/supplyvariation-testdata.xlsx"
                  :reps 1
                  :phases phases
                  :lower 0 :upper 1.5
                  :compo-lengths default-compo-lengths)))

  (def random-run-exhaustive
    (binding [*noisy* 1.0
              *project->experiments* project->full-factorial-experiments]
      (random-run "~/repos/notional/supplyvariation-testdata.xlsx"
                  :reps 1
                  :phases phases
                  :lower 0 :upper 1.5
                  :compo-lengths default-compo-lengths)))

  ;;some defaults...
  (def phases [["comp-1"  1   730]
               ["phase-1" 731 763]
               ["phase-2" 764 883]
               ["phase-3" 884 931]
               ["phase-4" 932 1699]
               ["comp-2"  1700 2430]])

)
