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

(defn select-all   [supplystore]
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

(defn constrained-gen [order gen]
  (if (>= (capacity gen) (order-cost order))   
    [(create-new-unit (new-id))
     (if-let [cost (order-cost order)]
       (update-in gen [:remaining] - (order-cost order))
       gen)]
    nil))

;;Stub....
(defn recognizes? [g order])

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

;;(def ac 
;;(generate-unit Generate-AC 

;;(defsupplyier Generate-RCAD :constraint 

;;(defsupplier jit-initial-supply 
;;   :supplies [Generate-AC 
;;              Generate-RCAD

;;=================

;;From my mockups...
;;If we want to represent the supply-side process of filling an order it 
;;looks like this: 
;;the order - {:SRC 2 :Quantity 10} ---> The Supplier ---> [[Entity1 & Actions] 
;;                                                          [Entity2 & Actions]
;;                                                          [Entity3 & Actions]]

;;This isn't too far from the goal-based behavior that Norvig uses in Paip.
;;Given a goal - fill the order {:SRC 2 :Quantity 10}, return a sequence of 
;;states that get us to the goal using available knowledge. 
;;In this case, the supplier is believed to contain information necessary to 
;;process such a query, and return a sequence of entities and maybe actions 
;;required to generate each entity.  

;;We'll implement the supplier via a composite design, in that individual
;;suppliers can be combined to form other types of suppliers.  The typical 
;;supplier will be a hierarchical supplier, which basically moves over a 
;;pre-defined sequence of suppliers to try to fill orders.  The order of 
;;the suppliers implies a preference, so that the filling of orders corresponds
;;to some optimum fill, represented by a hierarchical objective function .
;;If each supplier is filling the order optimally, and we have a sequence of 
;;suppliers that are ordered from "best" to "worst", then we optimally fill 
;;the whole order by visiting each supplier in order.  Easy.  

;;This design lets us create other types of suppliers.  The order that we visit
;;suppliers may be determined by any function, and it could be arbitrary.  We
;;may wish to balance the filling of orders across different elements of supply.
;;Many suppliers may share a resource that constraints the total amount of 
;;things supplied.  There may be preferences assigned to specific types of 
;;orders for specific supply, which changes the order in which suppliers are 
;;visited.  On and on...

;;Just having a simple hierarchical fill, and an atomic supplier will be 
;;enough to get us significant traction for now. 

;;So, given an order, a supplier should be able to produce a sequence of 
;;entities.  We will want to communicate extra contextual information, such 
;;as the need to possibly create a new entity.  I refer to this context as the
;;actions required to fulfill the supply.

;;At a later stage, something "else" will consume the entity name, and any 
;;actions required, and will reify that into a "promise" for the supply that 
;;can be provided to our fill routine.

;;Selection vs. Generation 
;;========================

;;From the order-filling point of view, the result of processing an order 
;;is just a sequence of [entity action] pairs.  
;;From the supplier view, the actions required to supply an entity may be 
;;significant, or they may be nonexistent.  For instance, if we have pre-existing
;;supply, then the simplest supplier just applies some ordering to the supply 
;;and tries to fill the order, drawing from best-to-worst.  The result is just a 
;;simple entity list.

;;Entity Selector
;;===============

;;We'll call this simple guy an entity-selector, since all he's doing is 
;;imposing some ordering an a set of pre-existing entities, and there are 
;;no actions required (specifically no entity creation).  It's almost like a 
;;SQL Select query is being executed, albeit with some potentially complicated
;;ordering rules applied.
(defn entity-selector [comparison rules] 
  (fn [order ctx] (order-units (get-units order ctx) :comparer comparison)))


;;Entity Generator
;;================

;;Just like unit selection rules, we also have unit generation rules...
;;The ability to generate entities, and to define high-level, possibly 
;;constrained entity generation semantics, is REALLY important for flexibility.

;;Queries that may result in [entity action] pairs, where the actions contain 
;;some additional cost or pre-condition that must be satisifed, are 
;;entity-generators.

;;The default semantics are that, if an entity order has no associated cost, 
;;then it costs nothing to generate, and capacity is effectively unlimited 
;;(even if a capacity is supplied).

;;Otherwise, we relate the cost of an entity generation in some manner, either 
;;in whole-unit quantities, or in terms of a number relative to the size of 
;;the entity (like pax), which allows us to directly represent the complex 
;;rules in portfolio analyses.

;;If we wanted to unify selection and generation, we could view the 
;;entity-selector as an entity-generator, with a capacity identical to the 
;;number of units in the total supply of entities being queried, and unit-cost 
;;action associated with each entity generated.  The implication is that each 
;;entity generated reduces the capacity of the generator by 1.

(defn can-generate? [order g]
  (or (= (capacity g) :infinite) 
      (and (>= (capacity g) (order-cost order))
           (recognizes? g order))))

;;The spec supplied to the generator is responsible for communicating the cost 
;;information.
;;Generated is the cumulative number of entities generated from here.
;;Remaining is the capacity remaining, in whatever unit of measure we decide.
;;Remaining may actually be a composite data type, or more typically, a number.
;;order->entity is a function that maps orders for entities, with the a value 
;;for the generator, and produces a pair of [[entity & actions] new-gen], where
;;new-gen is a generator representing the effect (like a decrease in capacity) 
;;of generating an entity.
(defrecord entity-generator [name generated remaining order->entity]
  IEntityGenerator 
  (generate-entity  [gen order] (order->entity order gen))
  (capacity         [gen] remaining))

;;Weak description.
;;creates an entity generator with defaults.
(defn ->basic-generator [& {:keys [name generated remaining order->entity] 
                             :or {name (gensym "_Generator")
                                  generated 0
                                  remaining 0 
                                  order->entity constrained-gen}}]
  (->entity-generator remaining generate))

(defn ->infinite-generator [order->entity] 
  (->basic-generator :remaining :infinite                     
                     :order->entity order->entity))


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

;;------------------Pending---------------------------------------


          
            
      
;;Given these rules....we should be able to find supply for each.
;; ([:fillrule "SRC3"] 
;;  [:fillrule "SRC3"] 
;;  [:fillrule "SRC2"] 
;;  [:fillrule "SRC1"] 
;;  [:fillrule "SRC3"] 
;;  [:fillrule "SRC3"] 
;;  [:fillrule "SRC1"]
;;  [:fillrule "SRC3"] 
;;  [:fillrule "SRC2"]) 
            
;;so a fill query is just a sophisticated entity query....
;;we're looking at entities from the supply store.
;;Another option here is to wrap each of the stores as an entity
;;store....and define some operations that concatenate the entity
;;store.
;;This could greatly unify entity querying....
;;Later on, when we go to "update" the entities and add components, 
;;we can port the backend into an actual component store.

;;given a rule, how do we look up supply that matches that rule? 
;;under the old scheme, we have a set of "supply buckets"; basically 
;;a map in the supplystore that maintains the deployable supply as
;;units move around.

;;This is an optimization....most of the simulation activity will be
;;dominated by supply updates (i.e. entity movement).

;;High level concern is to query a supply store given a fillrule and a
;;fillstore.

;;The default fill function.  We'll look at one that can interpret
;;rules internally later.
;;The simplest fill function we can have is to take a fill rule 
;;and match it to all supply via the fill map.  From here, we 
;;have supply partitioned into buckets automatically.  We just 
;;search deployable buckets from there.
;; (defn fill-function [fillgraph]
;;   (let [rules   (fg/fill-map fillgraph)
;;         src-map (reduce-kv (fn [acc [_ snk] sources]
;;                              (assoc acc 
;;                                (reduce (fn [xs [[_ source] cost]]
;;                                          (conj xs [source cost])) [] sources)))
;;                            {} rules)
;;         rule->src (memoize (fn [rule] (let [[nd src] (fill/derive-supply-rule nil)]
;;                                        src)))
;;         rule->buckets (memoize (fn [rule store]
;;                                  (supply/get-buckets store)]
;;   (reify fill/ISupplier 
;;     (query [s rule store] 
;;       (let [buckets  (supply/get-buckets store)  


;;The dumb way is to just map over all deployable units....
;;Basically reduce over then entire set of units, and filter out 
;;the ones that are deployable.  We normally cache these in the
;;supplystore/buckets...note: buckets are keyed by src.
;;Note: we have a separate set of buckets for followons...
;;followonbuckets...

;;It may be useful to just add a "generic" category of 
;;buckets; ala the followon buckets, and unify them all together.
;;For instance: 
;;{:buckets {:generic #{src1 src2 src3....} 
;;           :group1  #{src1 src3 .....}
;;           :group2  #{src3 ...}}}
;;if we opt to use tags, we already have things managed for us, 
;;plus the benefit of a mutable interface...

;;Unifying the buckets is probably a good idea...

(defn dumb-fill-function [fillgraph]
  (let [rules   (fg/fill-map fillgraph)
        src-map (reduce-kv (fn [acc [_ snk] sources]
                             (assoc acc 
                               (reduce (fn [xs [[_ source] cost]]
                                         (conj xs [source cost])) [] sources)))
                           {} rules)
        rule->src (memoize (fn [rule] (let [[nd src] (fill/derive-supply-rule nil)]
                                       src)))
        ;;compute the ordered set of resource buckets that we should
        ;;look at.
        rule->buckets (memoize (fn [rule store]
                                 (supply/get-buckets store))
                               
  (reify fill/ISupplier 
    (query [s rule store] 
      (let [buckets  (supply/get-buckets store)
