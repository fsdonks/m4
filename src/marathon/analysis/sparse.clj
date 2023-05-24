;;Namespace designed to generate sparse samples across a design space
;;with multiple factors as opposed to full factorial designs.
;;The general sampling bits could be moved outside of m4 if another
;;use case pops up.
(ns marathon.analysis.sparse
  (:require [marathon.analysis.nolh :as nolh]
            [incanter
             [charts :as charts]
             [core :as incore]])
  (:import [org.apache.commons.math3.random
            SobolSequenceGenerator]))

(defn sobol
  "Given a quantity of dimensions, dims, return the first n samples of
  the sobol sequence."
  [dims n]
  (let [gen (SobolSequenceGenerator. dims)]
    (for [index (range n)]
      (.nextVector gen))))

(defn between-interval
  "Given a low-bound and a high-bound, returns the proportional
  position from low-bound to high-bound based on a distance between 0
  and 1."
  [[low-bound high-bound] distance]
  (let [total-distance (- high-bound low-bound)
        mapped-distance (* distance total-distance)]
    (+ low-bound mapped-distance)))

(defn bound-sobols
  "Given a sequence of [low-bound high-bound] for each dimension,
  and a sequence of sobol sequence vectors with values in [0, 1], map
  the values in [0, 1] to [low-bound high-bound]."
  [bounds sobols]
  (for [sample sobols]
    (map between-interval bounds sample)))
           
(defn bounded-sobols
  "Given a sequence of [low-bound high-bound] ints for each dimension,
  generate the first n samples of the sobol sequence.  type-fn will be
  called on each number afterwards."
  [bounds n & {:keys [type-fn] :or {type-fn identity}}]
  (let [sobols (sobol (count bounds) n)
        bounded (bound-sobols bounds sobols)]
    (map (fn [vector] (map type-fn vector)) bounded )))

(defn bounded-sobols-round
  [bounds n]
  (bounded-sobols bounds n "type-fn" (fn [val] (int (Math/round val)))))
  
(comment
  ;;sample of a Sobol sequence for two dimensions from 0 to 1.
  (let [sobs (sobol 2 65)]
    (incore/view (charts/scatter-plot (map first sobs) (map second
                                                            sobs))))
  ;;Example of a Sobol sequence between [low-bound high-bound]
  (let [sobs (bounded-sobols [[0 43] [45 60]] 65)]
    (incore/view (charts/scatter-plot (map first sobs) (map second
                                                            sobs))))
  ;;Rounding will give us a less good spread with some points closer
  ;;to each other.
  (let [sobs (bounded-sobols-round [[0 43] [45 60]] 65)]
    (incore/view (charts/scatter-plot (map first sobs) (map second
                                                            sobs))))
  ;;Similar issue with rounding, just in different spots.
  (let [sobs (bounded-sobols [[0 43] [45 60]] 65 :type-fn int )]
    (incore/view (charts/scatter-plot (map first sobs) (map second sobs))))
  )
          
