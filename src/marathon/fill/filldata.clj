(ns marathon.fill.filldata
  (:use [util.record]))

(defrecord+ fill [rule fillPath pathlength followon source])
(defn quality [f] (if (zero? (:pathLength f)) :Primary :Substitute))
                       