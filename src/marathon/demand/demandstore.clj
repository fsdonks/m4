;A generic container for data necessary to manage a set of demanddata.
(ns marathon.demand.demandstore
  (use [spork.util.record :only [defrecord+ with-record]]))

(defrecord+ demandstore 
  [[name :DemandStore] 
   [demandmap  {}]
   [infeasible-demands {}] 
   unfilledq 
   [activations {}]
   [deactivations {}]
   [activedemands {}]
   [eligbledemands {}]
   [changed true]
   demandtraffic  
   tryfill  
   loginfeasibles 
   [tags {"Sinks" {}}]
   fillables 
   verbose 
   [tlastdeactivation 0]])

(def empty-demandstore (make-demandstore)) 
