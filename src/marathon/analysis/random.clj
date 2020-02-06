;This name space contains functions for performing targeting model supply variations with multiple random runs.
(ns marathon.analysis.random (:require [marathon.analysis :as a]
                                       [marathon.analysis.util :as util]
                                       [marathon.analysis.experiment :as e]
                                       [marathon.ces.core :as c]
                                       [spork.util.table :as tbl]
                                       [spork.util.general :as gen]
                                       [clojure.spec.alpha :as s]))

(defn individual-record
  "Given an index and an originating record,
   create a custom individual record the supply representing
   the nth unit of this record, where n <= (:Quantity rec)."
  [n rec]
  (assoc rec :Quantity 1 :Name (str n "_" (:SRC rec))))

(defn record->records
  "Given a base supply record rec, and an optional transformation function f,
  expands the supply record into n records, where n is in [0 ... Quantity]
  relative to the SupplyRecord's quantity. Each reord is supplied to the
  transformation f to allow for custom processing of each generated record.
  Defaults to a transformation where quantity and unit names are altered to
  coerce a batch record into a sequence of individual unit records, but
  no other fields are changed.."
  ([rec f]
   (for [i (range (:Quantity rec))]
     (f (individual-record i rec))))
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

#_(defn rand-recs
    "Takes a supply record and creates multiple supply records with random initial cycle time."
    [rec]
    (for [i (range (:Quantity rec))]
      (if (= (:Component rec) "AC")
        (assoc rec :CycleTime (rand-int 1095) :Quantity 1 :Name (str i "_" (:SRC rec)))
        (assoc rec :CycleTime (rand-int 1825) :Quantity 1 :Name (str i "_" (:SRC rec))))))

;;it probably makes sense to fence out a general scheme to apply this pipeline
;;to demand records, policy records, etc.  There are probably many types of
;;transformations (or compiler passes) we'd like to apply.
(defn rand-proj
  "Takes a project and makes a new project with random unit initial cycle times.
   If the project supplies a:supply-record-randomizer key associated to a
   function of supply-record -> supply-record, that function will be supplied
   as a transformation when generating random records.  Otherwise, no transformation
   will occur beyond the normal expansion of a batch supply record into multiple records
   with custom names."
  [proj]
  (let [supply-record-randomizer (get proj :supply-record-randomizer identity)]
    (->> proj
         :tables
         :SupplyRecords
         tbl/table-records
         (mapcat #(rand-recs % supply-record-randomizer))
         tbl/records->table
         (assoc-in proj [:tables :SupplyRecords]))))

(defn fill-stats
  "Copy of this function from the marathon.analysis.experiment namespace.
  This function has been updated to include statistics by component."
  [ctx]
  (->> ctx
       a/demand-trends ;;if we swap this with util/demand-trends-exhaustive trends we'll get a different result.
       (remove #(= (:DemandGroup  %) "RC_NonBOG-War"))
       (reduce (fn [acc {:keys [ACFilled NGFilled RCFilled ACOverlap NGOverlap RCOverlap TotalRequired]}]
                 (-> acc
                     (update :AC-fill     #(+ % ACFilled))
                     (update :NG-fill     #(+ % NGFilled))
                     (update :RC-fill     #(+ % RCFilled))
                     (update :AC-overlap  #(+ % ACOverlap))
                     (update :NG-overlap  #(+ % NGOverlap))
                     (update :RC-overlap  #(+ % RCOverlap))
                     (update :total-quantity #(+ % TotalRequired))))
               {:AC-fill 0, :NG-fill 0, :RC-fill 0,
                :AC-overlap 0, :NG-overlap 0, :RC-overlap 0,
                :total-quantity 0
                :AC-deployable (get (update (->> ctx c/units util/deployables (group-by :component)) "AC" count) "AC")
                :NG-deployable (get (update (->> ctx c/units util/deployables (group-by :component)) "NG" count) "NG")
                :RC-deployable (get (update (->> ctx c/units util/deployables (group-by :component)) "RC" count) "RC")
                :AC-total      (get (update (->> ctx c/units (group-by :component)) "AC" count) "AC")
                :NG-total      (get (update (->> ctx c/units (group-by :component)) "NG" count) "NG")
                :RC-total      (get (update (->> ctx c/units (group-by :component)) "RC" count) "RC")
                :period        (c/current-period ctx)})))

(defn weighted-fill-stats
  "Truncated copy of this function from the marathon.analysis.experiment namespace.
  Takes a series of frames and gets the amount of time associated with them (dt)."
  [frames]
  (->> frames
       (map (fn [[t ctx]] (assoc (fill-stats ctx) :t t)))
       (filter #(pos? (:total-quantity %)))
       (gen/time-weighted-samples :t)))

(defn in-phase?
  "Tests if x is between a and b."
  [[a b] x]
  (if (and (>= x a) (<= x b)) true false))

(defn phase-fill
  "Gets the amount of overlap between the intervals [x, x+d] and [a, b]."
  [[phase a b] [m x d]]
  (let [y (dec (+ x d))]
    (if (and (<= x a) (in-phase? [a b] y))
      (assoc m :phase phase :dt (inc (- y a)))
      (if (and (in-phase? [a b] x) (>= y b))
        (assoc m :phase phase :dt (inc (- b x)))
        (if (and (in-phase? [a b] x) (in-phase? [a b] y))
          (assoc m :phase phase :dt (inc (- y x)))
          (if (and (<= x a) (>= y b))
            (assoc m :phase phase :dt (inc (- b a)))))))))

(defn phase-fill-stats
  "Gets the amount of overlap between the interval [x, x+d] and each phase."
  [phases [m x d]]
  (remove nil? (map phase-fill phases (repeat [m x d]))))

(defn weighted-phase-fill
  "Gets fill stats weighted by the duration of each frame (dt)."
  [fill]
  (assoc fill
         :AC-fill          (* (:AC-fill fill) (:dt fill))
         :NG-fill          (* (:NG-fill fill) (:dt fill))
         :RC-fill          (* (:RC-fill fill) (:dt fill))
         :AC-overlap       (* (:AC-overlap fill) (:dt fill))
         :NG-overlap       (* (:NG-overlap fill) (:dt fill))
         :RC-overlap       (* (:RC-overlap fill) (:dt fill))
         :total-quantity   (* (:total-quantity fill) (:dt fill))
         :AC-deployable    (* (:AC-deployable fill) (:dt fill))
         :NG-deployable    (* (:NG-deployable fill) (:dt fill))
         :RC-deployable    (* (:RC-deployable fill) (:dt fill))
         :AC-total         (* (:AC-total fill) (:dt fill))
         :NG-total         (* (:NG-total fill) (:dt fill))
         :RC-total         (* (:RC-total fill) (:dt fill))))

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
       (map (fn [x] {:phase (key x)
                     :AC-fill         (reduce + (map :AC-fill (val x)))
                     :NG-fill         (reduce + (map :NG-fill (val x)))
                     :RC-fill         (reduce + (map :RC-fill (val x)))
                     :AC-overlap      (reduce + (map :AC-overlap (val x)))
                     :NG-overlap      (reduce + (map :NG-overlap (val x)))
                     :RC-overlap      (reduce + (map :RC-overlap (val x)))
                     :total-quantity  (reduce + (map :total-quantity (val x)))
                     :AC-deployable   (reduce + (map :AC-deployable (val x)))
                     :NG-deployable   (reduce + (map :NG-deployable (val x)))
                     :RC-deployable   (reduce + (map :RC-deployable (val x)))
                     :AC-total        (reduce + (map :AC-total (val x)))
                     :NG-total        (reduce + (map :NG-total (val x)))
                     :RC-total        (reduce + (map :RC-total (val x)))}))))

(defn change-upper-bound
  "Modifies the upper bound of supply experiments. Usefull for looking
  at supply levels greater than the current inventory."
  [bound fraction]
  (if (some? bound) (int (Math/ceil (* bound fraction))) bound))

(defn change-lower-bound
  "Modifies the lower bound of supply experiments. Usefull for looking
  at supply levels greater than the current inventory."
  [bound fraction]
  (if (some? bound) (int (Math/floor (* bound fraction))) bound))

(defn ac-supply-reduction-experiments
  "This is a copy of this function from the marathon.analysis.experiment namespace.
  Upper and lower bounds have been modified so we can look at AC supply levels
  above the current inventory."
  [tables lower upper & {:keys [step] :or {step 1}}]
  (let [init      (-> tables :SupplyRecords)
        groups    (-> init e/grouped-supply)
        low       (-> "AC" groups :Quantity (change-lower-bound lower))
        high      (-> "AC" groups :Quantity (change-upper-bound upper))]
    (for [n (range high (dec low) (- step))]
      (->> (assoc-in groups ["AC" :Quantity] n)
           vals
           tbl/records->table
           (assoc tables :SupplyRecords)))))

(defn project->experiments
  "This is a copy of this function from the marathon.analysis.experiment namespace.
  This function is modified so we can look at AC supply levels above the
  current inventory."
  [prj lower upper & {:keys [step] :or {step 1}}]
  (for [tbls (ac-supply-reduction-experiments (:tables prj) lower upper :step step)]
    (assoc prj :tables tbls)))

(defn rand-target-model
  "Uses the target-model-par-av function from the marathon.analysis.experiment
  namespace as a base. This function is modified to perform a random run for
  each level of supply."
  [proj & {:keys [phases lower upper gen seed->randomizer]
           :or   {lower 0 upper 1 gen util/default-gen
                  seed->randomizer (fn [_] identity)}}]
  (->> (e/split-project proj)
       (reduce
        (fn [acc [src proj]]
          (let [experiments (project->experiments proj lower upper)
                n           (count experiments)]
            (into acc
                  (map
                   (fn [[idx proj]]
                     (try (let [rep-seed   (util/next-long gen)
                                proj       (assoc proj :supply-record-randomizer
                                              (seed->randomizer rep-seed))
                                fill       (project->phase-fill (rand-proj proj) phases)]
                            ;;fill (e/project->period-fill proj) period fill and no randomness
                            (mapv #(assoc % :rep-seed rep-seed)
                                   (e/summary-availability-records src proj fill)))
                          (catch Exception e
                            (throw (ex-info (str "unable to compute fill " src)
                                           {:str src :idx idx})))))
                   (map-indexed vector experiments))))) [])
       (apply concat)
       vec))

(defn default-randomizer [seed compo-lengths]
  (->cycletime-randomizer (util/->gen seed) compo-lengths))

;;specs to catch an error I ran into with empty phases..
(s/def ::phase  (s/tuple string? int? int?))
(s/def ::phases (s/+ ::phase))

(defn rand-runs
  "Runs replications of the rand-target-model function."
  [proj & {:keys [reps phases lower upper seed compo-lengths seed->randomizer]
           :or   {lower 0 upper 1 seed 42 compo-lengths default-compo-lengths}}]
  ;;input validation, we probably should do more of this in general.
  (assert (s/valid? ::phases []) (s/explain-str ::phases []))

  (let [seed->randomizer (or seed->randomizer #(default-randomizer % compo-lengths))
        gen              (util/->gen seed)]
    (apply concat
           (pmap (fn [n] (rand-target-model proj
                            :phases phases :lower lower :upper upper
                            :gen   gen    :seed->randomizer seed->randomizer))
                 (range reps)))))

#_(defn rand-runs
    "Runs replications of the rand-target-model function."
    [proj reps phases lower upper]
    (apply concat (pmap rand-target-model (repeat reps proj) (repeat reps phases) (repeat lower) (repeat upper))))

(def fields
  [:rep-seed
   :SRC
   :phase
   :AC-fill
   :NG-fill
   :RC-fill
   :AC-overlap
   :NG-overlap
   :RC-overlap
   :total-quantity
   :AC-deployable
   :NG-deployable
   :RC-deployable
   :AC-total
   :NG-total
   :RC-total])

#_(defn write-output
  "Writes formatted output to a file."
  [file-name results]
  (let [format-string (str (clojure.string/join "," (repeat (count (first results)) "%s")) "\n")
        values (map vals results)
        formatted-values (clojure.string/join (map #(apply format format-string %) values))
        headers (clojure.string/replace (apply format format-string (keys (first results))) ":" "")]
    (spit file-name (str headers formatted-values))))

(defn write-output [file-name results]
  (tbl/records->file results file-name :field-order fields))

(comment
  ;way to invoke functions
  (def path "~/repos/notional/supplyvariation-testdata.xlsx")
  (def proj (a/load-project path))
  (def phases [["comp" 1 821] ["phase-1" 822 967]])

  #_(def results (rand-runs proj 1 phases 0 1.5))
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

)
