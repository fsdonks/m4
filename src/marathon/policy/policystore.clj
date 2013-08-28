;;Storage for domain specific data used by the policy system.
(ns marathon.policy.policystore
  (:use [spork.util.record :only [defrecord+]]))

;This is for centralizing control over rotational policy, as well as substition 
;policy. 
;We maintain all the data structures necessary for managing this stuff.
;    Also manage all feasible locations through this object.
(defrecord+ policystore   
  [[name :PolicyStore] 
   [locationmap #{} ];kvp mapping of location names to indices ...
   ;VESTIGIAL
   [positions #{}] ;set of all known positions. <couldn't find any refernece to this.
   ;VESTIGIAL
   [locations #{}] ;set of all known locations, superset of positions.
   [policies {}] ;Kvp mapping of policy names to policy data
   [periods {}] ;Set of simulation periods...probably need to re-think this guy.
;   [highest {}] ;deprecated 
   policytraffic  
;   rules ;not sure we need this....fillgraph handles everything  
;   rulegraph 
;   ruledelim 
   activeperiod ;the current period 
   [periodchanges {}] ;the set of scheduled period changes....re-think this.
   schedules ;no idea....deprecate.
   [composites {}]]) ;set of composite policies....only really mattered for resetting.
;   [permanents {}] 
;   canghost ;eh...probably don't need to store this here.])
(def empty-policystore (make-policystore))
