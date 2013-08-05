;A collection of shared functions, might turn into protocols someday...
;I started this due to the fact that several functions - primarily accessors, 
;were bubbling up in each of the domain-specific modules.  As a result, we'll 
;just shove them in here.  If we don't, we'll end up with circular dependencies.
;Nobody wants that...
(ns marathon.sim.core
  (:require [util [metaprogramming :as util]]))

(defmacro stub [msg & empty-body]  
  `(~'fn ~'[& args] (~'throw (~'Exception. ~msg))))

;Creates a set of getters for our simulation state.  This allows us to 
;dissect our map a bit easier.
(util/defpaths [:state] 
  {get-parameters    [:parameters]
   get-supplystore   [:supplystore]
   get-demandstore   [:demandstore]
   get-policystore   [:policystore]
   get-fillstore     [:fillstore]
   get-fill-function [:fillstore :fillfunction]})

;The name here is a bit generic.  We're really trying to acquire the keys of 
;a map that contains information about followon-eligible supply.  In this 
;context, the keys are actually the followon-code of the unit, (typically a
;demandgroup). 
(defn get-followon-keys
  "Returns a sequence of followon codes that are currently present in the 
   supply.  Used for constraining or preferring supply during the followon 
   fill phase."
  [supplystore] (keys (:followon-buckets supplystore)))

(defn update-unit [u ctx]
  (assoc-in ctx [:state :supplystore :unitmap (:name u)] u))

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