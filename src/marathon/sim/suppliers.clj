;;A namespace for defining rules that order units in a supply store, called 
;;suppliers.  Suppliers provide an implementation of the ISupplier protocol that
;;can process queries against a supplystore.  Their return is a supply promise, 
;;which is a function that must be applied to a context to realize an element 
;;of supply.  Many times, the application of the supply promise will involve 
;;secondary changes and events, possibly the creation of a new element of 
;;supply.

(ns marathon.sim.suppliers
  (:require [marathon.sim [fill :as fill]]))

;;Supply functions are passed a pair of arguments:   
;;a rule and a supplystore.   

;;Their task is to intepret the rule, use the information to find supply 
;;in the store, and return a sequence of supply promises. 
;;Supply promises are functions-to-be-applied to a simulation context, 
;;in order to realize the fill.
;;The result of applying a fill-promise looks like this: 
;;(fill-promise ctx) => [promised-unit, new-ctx] 

;;Assumably, the fill-promise performs all the necessary intermediate transforms
;;necessary to select the unit as a valid candidate for filling.

;;For now, we implement default operations for all suppliers, and then see 
;;if we can extract a useful language to define or extend them. 

;;There's a separation of concerns here: The actual selection of units, 
;;and the sequence of actions required to generate the unit.

;;Selecting Units
;;===============
;;SelectionRequest->Unit
(defprotocol IEntitySelector 
  (select-entity [selector entity-order]))

(defn select-all [supplystore]
  (get supplystore [:unitmap]))

;;We need different ways of comparing units. 
;;That's pretty fundamental to unit selection and ordering.

(defn compare-unit [x y]) 
  

;;EntityRequest->SpawnUnit

;;Generating Unit Entities
;;========================
(defprotocol IEntityGenerator
  (generate-entity [gen entity-order])
  (capacity        [gen]))

;;We specify how to generate entities in a generic fashion.  A nice way to do
;;this is to decouple the process of specification into a notion of ordering 
;;and fulfilling orders.  This is identical to ordering supply to fill a demand.

;;The concept is no different; we're merely ordering supply to match a 
;;specification, in this case.  In general, we're just translating requests 
;;into responses; 

(defrecord supplygenerator [id rules bound generate])

;;For the simplest case, where units already exist in the supply, we merely 
;;return the unit, and the context..This is effectively identity.
(defn existing-unit [rule store u ctx] [u ctx])

;;A more complicated case is to create and initialize a new unit.  We have 
;;the concept of just-in-time units, canonically referred to as ghosts.

;;We'll use just-in-time units quite a bit.  In order to create them, we 
;;probably need to supply some initializing function.
(defn jit-unit [rule store u ctx])



