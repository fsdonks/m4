;;marathon.sim.core is a glue library that provides access to a common set of 
;;subsystems upon which the more complicated federated simulation infrastructure
;;is implemented.  The general design for marathon is to provide a simulation 
;;context, which provides primitive access to heterogenous chunks of data 
;;relevant to different domains of the simulation.  Systems are then defined 
;;over this shared architecture, and further elaborate on means to affect a 
;;simulation in their particular domain.  At the highest level, a coordinating 
;;function, or an engine, orders and composes the systems into a comprehensive 
;;state transition function that maps one simulation context to the next.
(ns marathon.sim.core
  (:require [util [metaprogramming :as util]]))

;A collection of shared functions, might turn into protocols someday...
;This should contain only core functionality shared across subsystems defined 
;in other namespaces, to eliminate redunancy.
;I started this due to the fact that several functions - primarily accessors, 
;were bubbling up in each of the domain-specific modules.  As a result, we'll 
;just shove them in here.  If we don't, we'll end up with circular dependencies.
;Nobody wants that...

;;Stubs were used during the initial port to toss exceptions if an unimplemented
;;or deferred function was called.  
(defmacro stub [msg & empty-body]  
  `(~'fn ~'[& args] (~'throw (~'Exception. ~msg))))

;;#Providing Common Access to the State in the Simulation Context#
;;The simulation context contains a large nested map of information that 
;;is 'typically' partitioned by a particular domain.  However, certain systems
;;like the fill function, need access to multiple domains to fulfill their 
;;purpose.  We maintain an open, broad structure for the state portion of the 
;;context, but provide a set of common paths or mappings to resources embedded
;;in the state.  As a result, subsystems can query resources by predefined 
;;paths - a higher level of abstraction and clarity - OR they may dip down to 
;;low level operations on the raw state.  This should maintain a balance between
;;flexibility and clarity.


;;#Common Paths to Simulation Resources #

;;In the previous implementation, the 'state' was implemented as a class, with 
;;concrete members to access each piece.  We retain that level of commonality
;;via the paths, but the underlying representation is based on a dynamic map 
;;structure, so we can still add new data as needed in a flexible manner.

;;Creates a set of getters for our simulation state.  This allows us to 
;;dissect our nested map of state a bit easier.  Each symbol in the defpath 
;;binding returns a function that, given a simulation context, points to the 
;;named resource using standard clojure map operations via clojure.core/get-in.
(util/defpaths [:state] 
  {get-parameters    [:parameters]
   get-supplystore   [:supplystore]
   get-demandstore   [:demandstore]
   get-policystore   [:policystore]
   get-fillstore     [:fillstore]
   get-fill-function [:fillstore :fillfunction]})


;;#Shared Functions#

;;The name here is a bit generic.  We're really trying to acquire the keys of 
;;a map that contains information about followon-eligible supply.  In this 
;;context, the keys are actually the followon-code of the unit, (typically a
;;demandgroup). 
(defn get-followon-keys
  "Returns a sequence of followon codes that are currently present in the 
   supply.  Used for constraining or preferring supply during the followon 
   fill phase."
  [supplystore] (keys (:followon-buckets supplystore)))

(defn update-unit
  "A unit in the supplystore."
  [u ctx]
  (assoc-in ctx [:state :supplystore :unitmap (:name u)] u))


;;#Tag Related Functions#
;;Another useful bit of higher order, or meta data, is the notion of simple 
;;tags.  We use tags as a simple mechanism to track all kinds of effects and 
;;information, without having to embed data in lower classes.  This way, when
;;an entity - or even a system - needs to be grouped or categorized we can 
;;use tags.  As a result, we see a lot of shared tags that represent common 
;;effects, like whether an entity is enabled or disabled.

;Generic tag-related....These apply equally to supplystore, demandstore, etc.
;The interface is identical.  Only the interpretation of the tags is different.
(defn is-enabled [store item] 
  (tag/has-tag? (:tags store) :enabled item))
(defn enable [store item]
  (update-in store [:tags] tag/tag-subject :enabled item))
(defn disable [store demandname]
  (update-in store [:tags] tag/untag-subject :enabled item))

;find-eligible-demands is implemented as multimethod that dispatches based on 
;type of the category that's passed in.  category-type provides a simple 
;dispatch mechanism.  Note-> could use built-in hierarchy functions to do this.
(defn category-type [x]
  (cond (or (keyword? x) (string? x))  :simple            
        (vector? x) :src-and-group 
        (map? x)    :rule-map
        :else (throw (Exception. "Unknown category type " (type x)))))

;This is a general utility function, that allows us to derive predicates based
;on atomic values, sets, or maps.  Used for filtering (currently in demand 
;categories and supply categories).
;Can probably extend this to allow for arbitrary predicate functions (later).
(defn make-member-pred [g]
  (if (or (set? g) (map? g))
    #(contains? g %)
    #(= g %)))