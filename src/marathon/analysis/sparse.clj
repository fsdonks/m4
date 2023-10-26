;;Namespace designed to generate sparse samples across a design space
;;with multiple factors as opposed to full factorial designs.
;;The general sampling bits could be moved outside of m4 if another
;;use case pops up.
(ns marathon.analysis.sparse
  (:require [marathon.analysis.nolh :as nolh])
  (:import [org.apache.commons.math3.random
            SobolSequenceGenerator]))

(defn sobol
  "Given a quantity of dimensions, dims, return the first n samples of
  the sobol sequence."
  [dims n]
  (let [gen (SobolSequenceGenerator. dims)]
    (->> (for [index (range n)]
           (.nextVector gen))
         vec)))

(defn between-interval
  "Given a low-bound and a high-bound, returns the proportional
  position from low-bound to high-bound based on a distance between 0
  and 1."
  [[low-bound high-bound] distance]
  (let [total-distance (- high-bound low-bound)
        mapped-distance (* distance total-distance)]
    (+ low-bound mapped-distance)))

(defn bound-sobol
  "Given a sequence of [low-bound high-bound] for each dimension,
  and a sequence of sobol sequence vectors with values in [0, 1], map
  the values in [0, 1] to [low-bound high-bound]."
  [bounds sobol]
  (for [sample sobol]
    (map between-interval bounds sample)))

(defn bounded-sobol
  "Given a sequence of [low-bound high-bound] ints for each dimension,
  generate the first n samples of the sobol sequence.  type-fn will be
  called on each number afterwards."
  [bounds n & {:keys [type-fn] :or {type-fn identity}}]
  (let [sobol (sobol (count bounds) n)
        bounded (bound-sobol bounds sobol)]
    (map (fn [vector] (map type-fn vector)) bounded )))

(defn bounded-sobol-round
  [bounds n]
  (bounded-sobol bounds n :type-fn (fn [val] (int (Math/round val)))))

;;given a seq of [k [lower upper]] - equivalently a seqable map, computes
;;a sequence of records of {k x..} for each k where x
;;is drawn from a multidimensional sobol sequence projected onto the bounds.
(defn bounded-sobol-records
  [name-bounds  n  & {:keys [type-fn] :or {type-fn identity}}]
  (let [bounds (mapv second name-bounds)
        names  (mapv first name-bounds)]
    (for [xs (bounded-sobol bounds n :type-fn type-fn)]
      (zipmap names xs))))

;;per the implicit api in marathona.analysis.random, we provide a function
;;compatible with the API expected by marathon.analysis.random/*project->experiments*.

;;TBD - this is a bit janky, copy paste.  It looks like a lot of the same functionality could
;;be factored out into a helper function.
(defn project->sobol-experiments
  "Constructs a series of supply variation experiments by using a Sobol sequence
   of low discrepancy to generate parametrically sized space filling designs.
   Like project->experiments, returns a sequence of projects with
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
         bounded-sobol-records
         (map (fn [r]
                (->> (for [[compo quantity] r]
                      (let [from (get compo-records  compo)
                            rec  (assoc from :Quantity quantity)]
                        rec))
                     tbl/records->table
                     (assoc tbls :SupplyRecords)
                     (assoc prj :tables)))))))

(comment
  (require '[incanter
             [charts :as charts]
             [core :as incore]])

  ;;sample of a Sobol sequence for two dimensions from 0 to 1.
  (let [sobs (sobol 2 65)]
    (incore/view (charts/scatter-plot (map first sobs) (map second
                                                            sobs))))
  ;;Example of a Sobol sequence between [low-bound high-bound]
  (let [sobs (bounded-sobol [[0 43] [45 60]] 65)]
    (incore/view (charts/scatter-plot (map first sobs) (map second
                                                            sobs))))
  ;;Rounding will give us a less good spread with some points closer
  ;;to each other.
  (let [sobs (bounded-sobol-round [[0 43] [45 60]] 65)]
    (incore/view (charts/scatter-plot (map first sobs) (map second
                                                            sobs))))
  ;;Similar issue with rounding, just in different spots.
  (let [sobs (bounded-sobol [[0 43] [45 60]] 65 :type-fn int )]
    (incore/view (charts/scatter-plot (map first sobs) (map second sobs))))

  ;;looking at bounded-sobol-records...
  (let [recs (bounded-sobol-records {:x [0 43] :y [45 60]} 65)]
    (incore/view (charts/scatter-plot (map :x recs) (map :y recs)))
    )
  )
          
