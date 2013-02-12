;A generic container for data necessary to manage a set of demanddata.
(ns marathon.demand.demandstore
  (use [util.record :only [defrecord+ with-record]]))


(rec/defrecord+ demandstore 
  [[name "DemandStore"] 
   [demandmap  {}]
   [infeasible-demands {}] 
   unfilledq 
   [activations {}]
   [deactivations {}]
   [eligbledemands {}]
   [changed true]
   demandtraffic  
   tryfill  
   loginfeasibles 
   [tags {"Sinks" {}}]
   fillables 
   verbose 
   tlastdeactivation])

(def empty-demandstore (make-demandstore)) 

;demandmap - KVP mapping of demand names demand data.
;infeasibledemand - Set infeasible (non substitutable) Demand SRCs.
;unfilledq - UnfilledQ is, on the surface, a map of key,value pairs. The keys \
;  are simple strings (or integers even) that define:
;     A Heterogenous set of
;        [Homogeneous Collections of
;            [Substitutable/Interchangeable Resources] in Priority Order
;            
;The point of all this is to map our notion of priority, specifically in the 
;filling of demand, into something that can fit with substitution, the idea 
;that we can utilize multiple resources to fill a slot, given appropriate rules.
;We want to be able to do all this really friggin fast too.
;At the base, UnfilledQs keys then, point to sorted maps of DemandNames (strings)
;The DemandNames are grouped or partitioned according to the substitution 
;possibilities inherent in their SRC. This allows us to enforce the mechanism 
;of prioritization of demand, without stopping after the first unfilled demand.

;Were using a heap to model this underneath, so if we separate the Demands into 
;what appear to be homogeneous collections (substitutible SRCs) , we can then 
;judge them evenly on priority. In the case of no substitution, we have purely 
;independent demands, which are already homogeneous, which we process the same 
;way.

;NOTE - * Right now, our categories are just the SRC string associated with the 
;demand. This creates the latter environment:
;a set of purely independent demands (heterogenous by SRC) , which are worked 
;on in parallel.


;activations - set of Days when demands are to be activated (start times).
;deactivations - set of Days when demands are to be deactivated (end times).
;activedemands - working set of active demands.
;(may rework this ).
;eligibleDemands - set of active demands eligible for follow-on supply. 


;changed - set of active demands that changed fill count during the course of 
;the day.


;------>note 

;'Fill routine will be modifed to support advanced substitution rules....
;'Basically, the substitution rules form a directed graph that guides the search for fills.
;'In practice, this graph will be static (we can cache the results).
;'From a given start node (a fill rule associated with the demand), we can traverse the graph, trying to
;'find the shortest path to a universal terminal node called fill.
;'Actually....when we add supply, we can very quickly check to see if a fill is possible...
;'Given a fill rule, we see if there are any active paths to get to fill.
;'From there, we branch back to find a fill for the given demand rule.
