(ns marathon.fill.fillstore
  (:use [spork.util.record :only [defrecord+ with-record]]))

;Container to store all the data associated with matching supply to demand, 
;namely substitution rules, scoping (both in and out of scope) for the current
;run, and any other associated data.
(defrecord+ fillstore [[name :FillStore] 
                       fillgraph 
                       fillfunction 
                       [fills {}]
                       rendergraphs  
                       [outofscope {}]
                       allgraphs])
(def empty-fillstore (make-fillstore))
