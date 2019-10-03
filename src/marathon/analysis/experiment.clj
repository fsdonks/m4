;;A namespace for defining typical experimental designs
;;e.g. supply variation, demand variation, maybe both.
(ns marathon.analysis.experiment
  (:require [marathon.analysis :as a]
            [spork.util [table :as tbl]
                        [io :as io]
                        [general :as gen]]))


;;We'll start with a hacky, very specific form of
;;supply experimentation motivated by generating
;;output for the an existing model.

;;Specifically, we'll perform a series of capacity
;;analysis runs (unaudited) and collect expected
;;fill for each run.

;;In general, we want a function to take some input,
;;and generate design points.

;;Then we want another function that will experiment
;;with the given design.

;;Finally, a function that collates the results
;;of the experiments.

;;This is the basica map/reduce paradigm.
;;We project an input onto multiple datums,
;;then map some processing onto those,
;;then reduce the processed results.

;;In our case, we'll generate multiple
;;M4 projects from an initial project.
;;We'll split the project by SRC.
;;For each SRC, we'll generate multiple projects as well,
;;based on a design function.
;;The design function will generate variable supply
;;from the root.
;;We'll run capacity analyses on each design,
;;generating an expected fill percentage.
;;We may even want to generate a fill distribution/curve vs.
;;a single point.

;;Then we'll reduce the results into a table of
;;SRC, AC, RC, NG, Fill

;;Note: we already have a basic implementation for DOE from
;;TMAS if we want to leverage it.

;;This will be somewhat dumb (intentionally).
(defn bounds->experiment [l r]
  (range l r))

(defn load-src [tbls src]
  (let [src-filter (a/filter-srcs [src])
        src-tables (src-filter     tbls)] ;alters SupplyRecords, DemandRecords
    src-tables))

;;this is a fast way to scrape scoping information
;;from a project.  We should probably publish it as a core
;;method for pre-processing.
(defn srcs-in-scope-from
  [tables]
   (binding [marathon.ces.setup/*tables* tables]
     (->> (marathon.ces.setup/default-fillstore)
           :fillgraph
           marathon.ces.fill.scope/derive-scope
           :in-scope
           keys
           set)))

;;we can use scoping information to split up the project into
;;work chunks.  If there are substitutions, we have to have strongly
;;connected components together.  Our default supply variation
;;assumptions preclude this (via independence) though.
(defn split-project [prj]
  "Given a project prj, returns a map of {src proj'} where
   proj'"
  (let [tbls     (:tables prj)
        srcs     (srcs-in-scope-from tbls)
        group-fn (a/group-by-srcs srcs
                    :tables [:SupplyRecords :DemandRecords :GhostProportionsAggregate])]
    (->> (group-fn tbls)
         (reduce-kv (fn [acc src tbls]
                      (assoc acc src (assoc prj :tables tbls))) {}))))

;;Given a split set of tables, we can now map an experiment function.
;;For our purposes, this will be simply to perform a capacity analysis.
;;The metric we care about will be total fill, e.g. %DaysFilled.
;;THis is a time weighted percentage, so we'll account for deltat in
;;the weighting.

(defn fill-stats
  "Computes a scalar quantity of unfilled demand from a simulation
   context."
  [ctx]
  (->> ctx
       (marathon.ces.demand/unfilled-demand-count)
       (map (juxt :quantity :unfilled))
       (reduce (fn [[qtot ftot] [q f]]
                 [(+ qtot q) (+ ftot f)]) [0 0])))

;;using demand trends does a bit of extra work here,
;;but we get reuse out of the box.
;;Note: we could also re-use our interpolating functions
;;from tacmm out of the box too, but we don't need to in
;;this instance (less work required, just scale by dt)
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

(defn weighted-fill-stats
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
(defn percent-fill [weighted-stats]
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

(defn ctx->percent-fill [ctx]
  (-> ctx a/as-stream weighted-fill-stats percent-fill))

#_(defn some-supply? [prj]
  (->> prj
      :tables
      :SupplyRecords
      tbl/table-records
      (some (fn [r] (pos? (:Quantity r))))))

(defn project->fill [prj]
  (-> prj a/load-context ctx->percent-fill))

;;Now we need to compute multiple experiments from a root project.
;;The simplest experiment will be supply variation.
;;Here, we just apply a function to the project that generates
;;more projects by altering the :SupplyRecords table.

;;Specifically, we seek to alter the AC supply programatically.
;;We'll define a simple reduction in supply that drives to
;;0.

(defn grouped-supply [supply-table]
  (->> supply-table
       tbl/table-records
       (group-by :Component)
       (reduce-kv (fn [acc compo xs]
                    (if (> (count xs) 1)
                      (throw (ex-info "expected 1 record" {:compo compo :xs xs}))
                      (assoc acc compo (first xs)))) {})))

(defn ac-supply-reduction-experiments [tables & {:keys [step] :or {step 1}}]
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
(defn project->experiments [prj & {:keys [step] :or {step 1}}]
  (for [tbls (ac-supply-reduction-experiments (:tables prj) :step step)]
    (assoc prj :tables tbls)))

;;{:SRC :AC :RC :NG :Fill}

(defn summary-record [src proj fill]
  (let [grouped (-> proj :tables :SupplyRecords grouped-supply)]
    {:SRC src
     :AC  (get-in grouped ["AC" :Quantity] 0)
     :RC  (get-in grouped ["RC" :Quantity] 0)
     :NG  (get-in grouped ["NG" :Quantity] 0)
     :Fill fill}
    ))

;;simple single-core version.
;;Takes about 6 minutes for a large TAA run, with no
;;perturbation of readiness conditions.
(defn target-model [proj]
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

;;basic execution
(comment
  (def the-path "../notional/base-testdata-v8.xlsx")
  (def proj (a/load-project the-path))
  (def res  (target-model proj))
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
