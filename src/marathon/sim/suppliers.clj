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
(defn jit-unit [rule store u ctx]
  (let [[new-unit ctx] (generate-unit! rule store ctx)]    
    [new-unit ctx]))


;;FMCA represents a complex set of rules for generating units...
;;For a rule like:

;;(defcomparer initial-demand [[AC-First MaxDwell]
;;                             [RC-AD MaxDwell]
;;                             Generate-AC
;;                             Generate-RCAD])

;;(defcomparer rotational-demand [[[RC-First MaxDwell] 
;;                                 [AC-First MaxDwell]]
;;                                RCAD
;;                                Generate-RC
;;                                Generate-AC
;;                                Generate-RCAD])

;;Note -> Generate-X implies that the unit does not exist.
;;So, the first couple of rules actually apply to a comparison function.
;;The next n rules apply to some generating function for new entities. 
;;We may wish to keep them separate.

;;(defcomparer initial-demand [[AC-First MaxDwell]
;;                             [RC-AD MaxDwell]])

;;(defgenerator initial-demand-gen 
;;                             [existing-supply ;;sorting matters
;;                              Generate-AC     ;;sorting may not matter ....
;;                              Generate-RCAD])

;;(defcomparer rotational-demand [[[RC-First MaxDwell] 
;;                                 [AC-First MaxDwell]]
;;                                RCAD])

;;(defgenerator rotational-demand [existing-supply ;;sorting matters
;;                                 Generate-RC     ;;sorting may not matter ....
;;                                 Generate-AC
;;                                 Generate-RCAD])

;;So then, a supplier is something that knows to combine a suitability 
;;function, and a generating function.
;;The supplier should consult one or more eligible "supplies", or sources of 
;;entities.
;;We'll typically have an existing supply of units (in the entity store)...
;;(defsupplier existing-initial-supply :comparer initial-demand :generator nil)
;;(defsupplier existing-rotational-supply :comparer rotational-demand :generator nil)


;;We can probably use unit streams for this...

(defn order-cost [m] (get m :cost 0))

;;Given an order for an entity,
(defn constrained-gen [order gen]
  (if (>= (capacity gen) (:cost order))   
    [(create-new-unit (new-id))
     (if-let [cost (order-cost order)]
       (update-in gen [:remaining] - (order-cost order))
       gen)]
    nil))

;;Stub....
(defn recognizes? [g order])

(defn can-generate? [order g]
  (and (>= (capacity g) (order-cost order))
       (recognizes? g order)))

;;The spec supplied to the generator is responsible for communicating the cost 
;;information. 
(defrecord entity-generator [remaining generate]
  IEntityGenerator 
  (generate-entity  [gen order] (generate order gen))
  (capacity  [gen] remaining))

;;Weak description.
;;creates an entity generator with defaults.
(defn ->basic-generator [& {:keys [remaining generate] 
                             :or {remaining 0 constrained-gen}}]
  (->entity-generator remaining generate))

;;Creates an entity generator that composes multiple entity generators, and 
;;provides a shared resource between them.  When trying to generate entities, 
;;the shared-generator will first determine if it has the ability to build 
;;supply; if its capacity is insufficient, then by virtue no child can build 
;;either.  This allows us to constrain groups of child generators along 
;;arbitrarily complex rules.  __Note__ we should probably model this as a 
;;constraint graph, rather than a simple tree, but for now this seems okay.
(defn shared-generator [remaining gen-rules child-generators] 
  (reify IEntityGenerator 
    (generate-entity [gen order] 
      (when (can-generate? gen) 
        (let [g (find-generator gen-rules child-generators order)
              [u gnext] (generate-entity g order)]
          (shared-generator (- (order-cost order) remaining)
                            gen-rules
                            (assoc child-generators (:name gnext)
                                   gnext)))))
    (capacity [gen] remaining)))

;;Just like unit selection rules, we also have unit generation rules...
;;The ability to generate entities, and to define high-level, possibly 
;;constrained entity generation semantics, is REALLY important for flexibility.

;;The default semantics are that, if an entity order has no associated cost, 
;;then it costs nothing to generate, and capacity is effectively unlimited 
;;(even if a capacity is supplied).

;;Otherwise, we relate the cost of an entity generation in some manner, either 
;;in whole-unit quantities, or in terms of a number relative to the size of 
;;the entity (like pax), which allows us to directly represent the complex 
;;rules in portfolio analyses.

;;We can view pre-existing supply as a unit-cost entity generator.
;;As entities are generated, they are decremented from the generatable supply.
;;If every entity is consumed, we have no capacity, and the generator returns 
;;nil.

;;Then, we can compose complex entity generators from simpler entity generators,
;;and simply have a means of matching on a rule.

;;This is similar to composing unit preferences. 

;;In fact, we will blend the two concepts: 
;;When finding supply (i.e. querying to build supply promises), we will 
;;feed the desired supply to a generator. 

;;The generator is then responsible for selecting from a sequence of generators 
;;that have some preferred order.  This is the notion of generating units. 
;;

;;This allows us to have some shared resource ; for instance, we can have the 
;;nesting generator keep track of how much resources have been expended, and 
;;some notion of shared resourcing can be imposed; This allows us to enforce 
;;"compo balancing" when we generate entities. 

;;The naive view is that there is a single generator, that sits atop the 
;;deployable supply in the supplystore.  



(def ac 
(generate-unit Generate-AC 

;;(defsupplyier Generate-RCAD :constraint 

;;(defsupplier jit-initial-supply 
;;   :supplies [Generate-AC 
;;              Generate-RCAD





