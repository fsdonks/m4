;;A namespace for defining typical experimental designs
;;e.g. supply variation, demand variation, maybe both.
(ns marathon.analysis.experiment
  (:require [marathon.analysis :as a]
            [marathon.analysis.util :as util]
            [marathon.ces [core :as c] [query :as query]]
            [spork.util [table :as tbl]
                        [io :as io]
                        [general :as gen]]))

;;We'll start with a hacky, very specific form of supply experimentation
;;motivated by generating output for the an existing model.

;;Specifically, we'll perform a series of capacity analysis runs (unaudited) and
;;collect expected fill for each run.

;;In general, we want a function to take some input, and generate design points.

;;Then we want another function that will experiment with the given design.

;;Finally, a function that collates the results of the experiments.

;;This is the basic map/reduce paradigm. We project an input onto multiple
;;datums, then map some processing onto those, then reduce the processed
;;results.

;;In our case, we'll generate multiple M4 projects from an initial project.
;;We'll split the project by SRC. For each SRC, we'll generate multiple projects
;;as well, based on a design function. The design function will generate
;;variable supply from the root. We'll run capacity analyses on each design,
;;generating an expected fill percentage. We may even want to generate a fill
;;distribution/curve vs. a single point.

;;Then we'll reduce the results into a table of SRC, AC, RC, NG, Fill

;;Note: we already have a basic implementation for DOE from TMAS if we want to
;;leverage it.

;;this is a fast way to scrape scoping information
;;from a project.  We should probably publish it as a core
;;method for pre-processing.
;;TODO: Collect in a preprocessing namespace?
(defn srcs-in-scope-from
  "Leverages the functions from marathon.ces.setup
   to coerce project tables into a marathon fillstore
   with its requisite fillgraph.  Since this can
   happen without needing to create a simulation
   context, and it only depends on demand, supply,
   and relation records, it provides a fast way
   to predetermine scoping of SRCs.  We extract
   the set of scoping criteria."
  [tables]
   (binding [marathon.ces.setup/*tables* tables]
     (->> (marathon.ces.setup/default-fillstore)
           :fillgraph
           marathon.ces.fill.scope/derive-scope
           :in-scope
           keys
           set)))

;;we can use scoping information to split up the project into work chunks. If
;;there are substitutions, we have to have strongly connected components
;;together. Our default supply variation assumptions preclude this (via
;;independence) though.
(defn split-project [prj]
  "Given a project prj, returns a map of {src proj'} where
   proj' is a 'sub-project' with the supply/demand/GhostProportions
   records limited to the those with SRC field = src.  Provides
   a convenient way to eagerly decompose a project for distributed
   runs by SRC."
  (let [tbls     (:tables prj)
        srcs     (srcs-in-scope-from tbls)
        group-fn (a/group-by-srcs srcs
                    :tables [:SupplyRecords :DemandRecords :GhostProportionsAggregate])]
    (->> (group-fn tbls)
         (reduce-kv (fn [acc src tbls]
                      (assoc acc src (assoc prj :tables tbls))) {}))))

;;Metrics
;;=======

;;Given a split set of tables, we can now map an experiment function.
;;For our purposes, this will be simply to perform a capacity analysis.
;;The metric we care about will be total fill, e.g. %DaysFilled.
;;THis is a time weighted percentage, so we'll account for deltat in
;;the weighting.

;;using demand trends does a bit of extra work here, but we get reuse out of the
;;box. Note: we could also re-use our interpolating functions from tacmm out of
;;the box too, but we don't need to in this instance (less work required, just
;;scale by dt)
(defn fill-stats
  "Computes a scalar quantity of unfilled demand from a simulation
   context."
  [ctx]
  (->> ctx
       a/demand-trends
       (reduce (fn [acc {:keys [TotalRequired Deployed]}]
                 (-> acc
                     (update :total-fill #(+ % Deployed))
                     (update :total-quantity #(+ % TotalRequired))))
               {:total-fill 0 :total-quantity 0})))

;;We may want to break out readiness as well...
;;This will only cover simulation periods where demand is
;;active.
(defn available-stats
  "Computes surplus supply by compo from a simulation context.
   {:keys [compo]}"
  [ctx]
  )

(defn weighted-fill-stats
  "Given a simulation history expressed as a sequence of [t ctx] frames,
   where t is the integer end-time of the ctx, and ctx is the state,
   projects the histotry onto a sequence of time-weighted
   samples of [ctx t dt], and computes a resulting sequence of
   fill-stats, returning
   {:keys [total-fill total-quantity
           weighted-total-fill weighted-total-quantity]}
   where the weighted-* keys are scaled by the time weight
   dt."
  [frames]
  (->> frames
       (map     (fn [[t ctx]] (assoc (fill-stats ctx) :t t)))
       (filter #(pos? (:total-quantity %)))
       (gen/time-weighted-samples :t)
       (map (fn [[{:keys [total-fill total-quantity] :as stats} t dt]]
              (-> stats
                  (assoc :weighted-total-fill (* total-fill dt))
                  (assoc :weighted-total-quantity (* total-quantity dt)))))))

;;core metric.
(defn percent-fill
  "Given a sequence of weighted samples of
  {:keys [weighted-total-fill weighted-total-quantity]}
   reduces the samples into a simple percent fill based on
   (sum weighted-total-fill) / (sum weighted-total-quantity).
   If no samples are present to reduce, returns 0."
  [weighted-stats]
  (->> weighted-stats
       (reduce (fn [{:keys [fill quantity]}
                    {:keys [weighted-total-fill weighted-total-quantity]}]
                 {:fill (+ fill weighted-total-fill)
                  :quantity (+ quantity weighted-total-quantity)})
               {:fill 0 :quantity 0})
       (#(/  (get  % :fill)
             (let [q (get % :quantity)]
               (if (zero? q) 1 q))))
        double))

(defn ctx->percent-fill
  "Convenience function to directly computed a fill percentage
   from a context, useful for mapping over experiments."
  [ctx]
  (-> ctx a/as-stream weighted-fill-stats percent-fill))

(defn project->fill
  "Convenience function to directly computed a fill percentage
   from a project, useful for mapping over experiments."
  [prj]
  (-> prj a/load-context ctx->percent-fill))

;;Defining Experiments
;;====================

;;Now we need to compute multiple experiments from a root project.
;;The simplest experiment will be supply variation.
;;Here, we just apply a function to the project that generates
;;more projects by altering the :SupplyRecords table.

;;Specifically, we seek to alter the AC supply programatically.
;;We'll define a simple reduction in supply that drives to
;;0.

(defn grouped-supply
  "Projects a table of SupplyRecords onto a map of {compo supply-record} for
  convenient processing."
  [supply-table]
  (->> supply-table
       tbl/table-records
       (group-by :Component)
       (reduce-kv (fn [acc compo xs]
                    (if (> (count xs) 1)
                      (throw (ex-info "expected 1 record" {:compo compo :xs xs}))
                      (assoc acc compo (first xs)))) {})))

(defn ac-supply-reduction-experiments
  "Given a map of the form {:keys [SupplyRecords]},
   where SupplyRecords is a spork.util.table table, yields a sequence of maps
  corresponding to alterations of the original input tables. The resulting
  sequence is generated via
  [tables tables_1 tables_2 ...tables_n] where tables_n corresponds to a
  reduction of the AC supply by n. These form a sequence of experimental designs
  in which the input AC supply decreases to 0.  The size of decrease is
  parameterized by step, defaulting to 1.  step should be a non-negative
  int."
  [tables & {:keys [step] :or {step 1}}]
  (let [init          (-> tables :SupplyRecords)
        groups        (-> init grouped-supply)
        ;;have to account for ac-only supply.
        upper         (-> "AC" groups :Quantity)]
    (concat [tables]
            (when (and upper (> upper 1))
              (for [n (range (dec upper) -1 (- step))]
                (->> (assoc-in groups ["AC" :Quantity] n)
                     vals
                     tbl/records->table
                     (assoc tables :SupplyRecords)))))))

;;In the near future, we'd like to compose that by generating additional samples
;;by perturbing the initial conditions, then generate n capacity analysyses

;;We want to run an experiment where we decrease the supply to 0.
;;Naively, we just decrement the supply until we hit 0.
(defn project->experiments
  "Projects a project prj onto a sequence of projects with AC supply
   decreased by step, tending toward 0."
  [prj & {:keys [step] :or {step 1}}]
  (for [tbls (ac-supply-reduction-experiments (:tables prj) :step step)]
    (assoc prj :tables tbls)))

;;{:SRC :AC :RC :NG :Fill}
(defn summary-record
  "Record that summarizes the output of an experimental design
   in a format familiar to extant model data."
  [src proj fill]
  (let [grouped (-> proj :tables :SupplyRecords grouped-supply)]
    {:SRC src
     :AC  (get-in grouped ["AC" :Quantity] 0)
     :RC  (get-in grouped ["RC" :Quantity] 0)
     :NG  (get-in grouped ["NG" :Quantity] 0)
     :Fill fill}
    ))

;;Added data for shave charts and availability...

;;this is cribbed almost directly...
(def zero-fills
  {:TotalRequired 0
   :TotalFilled 0

   :ACFilled    0
   :RCFilled    0
   :NGFilled    0

   :Overlapping 0
   :Deployed 0
   :Unfilled 0
   })

(defn echo [this msg] (println msg) this)
(defn frame->fills [[t ctx]]
  (->>
   (for [[src ds] (group-by :SRC (util/demand-trends-exhaustive t ctx))]

     [src
      (reduce (fn [acc {:keys [TotalRequired TotalFilled Overlapping ACFilled RCFilled NGFilled
                               Deployed] :as r}]
                (-> acc
                    (update :TotalRequired      #(+ % TotalRequired))
                    (update :TotalFilled        #(+ % TotalFilled))
                    (update :ACFilled           #(+ % ACFilled))
                    (update :RCFilled           #(+ % RCFilled))
                    (update :NGFilled           #(+ % NGFilled))
                    (update :Overlapping        #(+ % Overlapping))
                    (update :Deployed           #(+ % Deployed))
                    (update :Unfilled           #(+ % (- TotalRequired Deployed)))
                    ))
              zero-fills
              ds)])
   (into {})))

(defn frame->target-trends [[t ctx]]
  (let [fills (frame->fills [t ctx])]
    (for [[src us] (->> ctx c/units (group-by :src))]
      (let [{:keys [deployable modernizing not-ready deployed]} (group-by util/state-key us)
            availables (group-by :component deployable)
            {:strs [AC RC NG]}  availables
            {:keys [c1 c2 c3 c4] :or {c1 0 c2 0 c3 0 c4 0 c5 0}}
            (->> us (map util/readiness) frequencies)]
        (merge
         {:t t
          :period (c/current-period ctx)
          :src  src
          :not_ready     (count not-ready)
          :deployed      (count deployed)
          :deployable    (+ (count AC) (count RC) (count NG))
          :deployable_ac (count AC)
          :deployable_rc (count RC)
          :deployable_ng (count NG)
          :C1 c1
          :C2 c2
          :C3 c3
          :C4 c4}
         (get fills src zero-fills))))))

(def target-fields
  [:t :period :src :not_ready :deployed
   :deployable :deployable_ac :deployable_rc :deployable_ng ;;availables...
   :C1 :C2 :C3 :C4
   :TotalRequired
   :TotalFilled
   :ACFilled
   :NGFilled
   :RCFilled
   :Overlapping
   :Deployed
   :Unfilled
   ])

;;ugh, this is an ugly, TEMPORARY, copy-paste job.  We should be using the generic
;;stuff in other namespaces, but I'm pressed for time...
(defn lerps [rs]
  (let [final (atom nil)]
    (concat
     (for [[xs ys] (partition 2 1 rs)]
       (do (reset! final [ys 1])
           [xs (- (:t (first ys))
                  (:t (first xs)))]))
     (lazy-seq (vector @final)))))

(defn interpolate [rs]
  (apply concat
         (for [[xs dt] (lerps rs)]
           (if (= dt 1)
             xs
             (let [t0 (:t (first xs))]
               (apply concat
                      (for [n (range dt)]
                        (map (fn [r] (assoc r :t (+ t0 n))) xs))))))))

(defn history->target-trends [h tgt]
  (-> (map frame->target-trends h)
      interpolate
      (tbl/records->file tgt :field-order target-fields)))

;;simple single-core version.
;;Takes about 6 minutes for a large TAA run, with no
;;perturbation of readiness conditions.
(defn target-model
  "Given an initial project, decomposes the project into
   K sub projects by SRC.  For each sub project, computes
   a supply variation experiment that decrements the initial
   supply to 0.  Results are returned in the from of
   summary records.  Yields a sequence of {:keys [SRC AC NG RC Fill]}
   where AC NG RC are the relative quantities of supply for each compo.
   Fill is the %Fill Days for the entire simulation."
  [proj]
  (->> (split-project proj)
       (reduce
        (fn [acc [src proj]]
          (let [_ (println [:starting src])
                experiments (project->experiments proj)
                n           (count experiments)
                _ (println [:experiment-count n])]
            (into acc
              (map-indexed
               (fn [idx proj]
                 (try (let [_    (println [:processing :src src :experiment idx :of n])
                            fill (project->fill proj)]
                        (summary-record src proj fill))
                      (catch Exception e
                        (throw (ex-info (str "unable to compute fill " src)
                                        {:src src :idx idx})))))
               experiments)))) [])))

;;This gives us about a 3x speedup out of the box.  We're
;;theoretically using 8 threads, one per core, so
;;we ought to be getting closer to linear scaling.  I think
;;we can do better.
(defn target-model-par
  "Given an initial project, decomposes the project into
   K sub projects by SRC.  For each sub project, computes
   a supply variation experiment that decrements the initial
   supply to 0.  Results are returned in the from of
   summary records.  Yields a sequence of {:keys [SRC AC NG RC Fill]}
   where AC NG RC are the relative quantities of supply for each compo.
   Fill is the %Fill Days for the entire simulation."
  [proj]
  (->> (split-project proj)
       (reduce
        (fn [acc [src proj]]
          (let [_ (println [:starting src])
                experiments (project->experiments proj)
                n           (count experiments)
                _ (println [:experiment-count n])]
            (into acc
                  (util/pmap! 
                    (fn [[idx proj]]
                      (try (let [_    (println [:processing :src src :experiment idx :of n])
                                 fill (project->fill proj)]
                             (summary-record src proj fill))
                           (catch Exception e
                             (throw (ex-info (str "unable to compute fill " src)
                                             {:src src :idx idx})))))
                    (map-indexed vector experiments))))) [])))

;;basic execution
(comment
  (def the-path "../notional/supplyvariation-testdata.xlsx")
  (def proj (a/load-project the-path))
  (history->target-trends (a/as-stream (a/load-context proj))
                          (io/file-path "~/workspacenew/notional/target.txt"))
  (def res  (target-model-par proj))
  )



;;we want to do these as lazy as possible for now.
;;This original version is lazy, but using chunked-sequences
;;underneath.  We'll probably want something akin to
;;requirements analysis's producer/consumer queue.
(comment 
  (defn target-model [proj]
    (apply concat
           (for [[src proj] (split-project proj)]
             (let [experiments (project->experiments proj)
                   n           (count experiments)]
               (map-indexed (fn [idx proj]
                              (println [:processing :src src :experiment idx :of n])
                              (summary-record src proj (project->fill proj)))
                            experiments)
               ))))
  )
