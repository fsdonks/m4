(ns marathon.fill.filldata
  (:use [spork.util.record]))

(defrecord+ fill [rule fillPath pathlength followon source])
(defn quality [f] (if (zero? (:pathlength f)) "Primary" "Substitute"))
                       
