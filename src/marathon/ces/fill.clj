;;##Filling
;;A collection of functions that define the process of mapping classes of demand
;;to eligible supply, and filling demand with suitable supply.  The fill system
;;acts as a service for the engine, and provides operations used - primarily - 
;;by the demand simulation.  The process of filling pushes changes on the 
;;supply, since the act of filling usually consumes some resources in supply and
;;motivates a kind of motion in the supply simulation.

;;Note: This is one of the more involved pieces of documentation, and could 
;;benefit from refinement.  However, filling has proven to be
;;a sensitive subject, so the extra layer of commentary is still useful.
(ns marathon.ces.fill
  (:require [marathon.data   [protocols :as protocols]]
            [marathon.fill   [filldata :as filldata]]
            [marathon.demand [demanddata :as d]]
;;TODO [Verify and delete if not used!]
;;           [marathon.supply [unitdata :as udata]]
            [marathon.ces [core :as core] [demand :as dem] [supply :as supply]
                          [policy :as policy] [unit :as u] 
                          [deployment :as deployment]
                          [query :as query]]
            [marathon.ces.fill [fillgraph :as fg]]
            [spork.sim  [simcontext :as sim] [updates :as updates]]
;;TODO [Verify and delete tags if not used!]
;;           [marathon.supply [unitdata :as udata]]
            [spork.util [tags :as tag] [general]]
            [spork.entitysystem.store :as store]
            [spork.util.reducers]
            [clojure.core [reducers :as r]]))

;The old ManagerOfFill class actually handled the creation of a couple of
;dependent chunks of data; namely a FillFunction, a FillGraph,
;and a SupplyGenerator.  Functions in this module fill the role
;of creating and composing each of these elements in kind.

;;#Primitive Operations
;account for the fill in the fillstore, basically just conj it to the history.
(defn record-fill [fillstore fill] 
  (let [fillcount (count (:fills fillstore))]
    (assoc-in fillstore [:fills] (inc fillcount) fill)))

;;#Fill-Related Notifications

;notify everyone that we've filled a demand...
(defn filled-demand! [demand-name unit-name ctx] 
  (core/trigger-event :FillDemand demand-name unit-name "Filled Demand" nil ctx))  

;ghosts raise special attention when they deploy.
(defn ghost-deployed! [demand-src ctx]
  (core/trigger-event :GhostDeployed demand-src demand-src 
       "Filled demand with  ghost"  :normal ctx))

;ghosts raise special attention if they followon.
(defn ghost-followed! [demand-src ctx]
  (core/trigger-event :GhostDeployed demand-src demand-src 
     "Ghost followed on to another demand" :followon ctx))

;;Auxillary function to broadcast information about just-in-time, or "ghost" 
;;unit utilization.  May be replaced with something more general in the future.
(defn check-ghost [unit ctx]
  (do ;(println unit)
      (if (not (core/ghost? unit)) ctx
          (if (core/followon? unit) 
            (ghost-followed! unit ctx) 
            (ghost-deployed! unit ctx)))))

;##Decomposing the Fill Process....
;Sourcing a demand is really the composition of three simpler tasks: 
;find-supply, take n items from the supply, fill the demand with the n items.
;The following text dissects each of these compenents into atomic, composable 
;elements, and defines the higher-order demand-filling behavior from the
;primitive elements.  Before that, we take a brief detour through history to 
;examine the original object-based mechanisms for the fill process, and how 
;they remain in their functional counterparts.

;##Understanding the Legacy Implemention of Fill Functions
;The legacy notion of a fill-function is central to the idea of finding an 
;ordered set of candidate supply. In the old object model, the fill-function was
;effectively a partially-applied function that closed over a fillgraph and a 
;supplygenerator.  Since we only had one type of supply generator, the supply 
;generator was really just a function that used the fill graph to provide an 
;ordered list of supply on demand, where the fill graph is a set of relations 
;between elements of supply and elements of demand envisioned as a directed 
;acyclic graph.

;#Legacy Filling Via the FillFunction Object
;The FillFunction used to be an object that provided a high-level interface for 
;querying a set of rules about feasible and desired relations between elements
;of supply & demand, aka. Fill Rules, along with a candidate supply of
;units, provided a prioritized sequence of units that can fill said
;demand.  Fill Rules were embodied in a Directed Acyclic Graph,
;known as the FillGraph, the topology of which encoded weighted paths
;from demand sinks to sources of supply.  These abstractions were necessary,
;because the concept of priority is highly variable (even time/event dependent),
;and may change from study to study or run to run.  Hence, the desire to make 
;the rules and semantics for filling supply highly variable and data-driven.
;In the modern functional variant, we still have the notion of a fill-function, 
;but it does not perform the same amount of "heavy lifting" the old object did.
;In fact, the fill-function is really a simple chunk of data that contains the 
;contextual rules for filling any demand.  It is used in explicit queries, vs. 
;containing a query method relative to a fillfunction object.
;
;#Fill Rules and The Fill Graph
;FillRules, encoded in a FillGraph, actually tell us a lot before we simulate.
;The FillGraph is generated, as a pre-process step, by analyzing the supply 
;entity records, the demand entity records, and the relation records for 
;Marathon.  Each source provides a unique element of the graph: supply populates
;the graph with source nodes, or terminal nodes that can supply units of a 
;specific type (usually encoded as an SRC).  Demand populates sink nodes of 
;Demand on the graph, which consume units of a specific type.  Relations add 
;nodes to the interior region between source and sink nodes, creating new paths 
;(via substitution and equivilancies), which further relate supply and demand.  
;Together, each dataset is parsed to derive a set of rules, which form the
;topology (or connections) of the graph.  We can then bash the graph with some
;useful algorithms that make it easy to search, scope out useless rules, and
;even do some error checking. Unlike the fill-function, the fill graph maintains
;its structure and uses from the legacy version, since it was pure data then as
;well.

;When building the FillGraph, we actually create an implicit dependency graph
;that can tell us which elements of supply and demand are A) Reachable B)Not 
;Reachable. Nodes (usually coded as SRCs, but any string is valid) that are
;Reachable also provide information on how many other nodes they can reach.  
;In most cases, there will be a 1:1 match between a source node, say a Supply 
;of SRC1, and a sink node, say a Demand for SRC1.  In this case, we know that 
;both Supply and Demand contain SRC1, there will be a zero-cost path from 
;SINK_SRC1 -> SRC1 -> SOURCE_SRC1.  Since we allow the possibility of 
;substitutions in our ruleset, there may be more than 1 path from SINK_SRC1, 
;maybe to another source of substitutible supply like SRC2 
;(SINK_SRC1->SRC1_SRC2->SOURCE_SRC2).
;In this scenario, supply for both SRC1 and SRC2 are related, in that there's a 
;dependency introduced by the fill rules.  At a minumum, any simulation for SRC1
;must include SRC2, even if there's no demand for SRC2, because SRC2 "may" serve
;as supply for SRC1.  In this case, the set #{SRC1 SRC2} can be said to form an 
;equivalence class, or they form a strongly connected component.

;#Ancillary Pre-Processing Via the FillGraph
;While strictly related to the higher-level notion of filling demands, we can 
;exploit properties of the fillgraph - during a pre-processing phase - to make 
;searching the fillgraph more efficient, identify possible problems with the 
;data, and identify possible areas for exploting data-parallelism.  

;#Islands Denote Possible Data Errors
;In the case where there is NO path from either supply or demand, we have 
;"islands", or nodes that are unreachable (class B from the paragraph above).  
;These islands are usually the result of data errors, and indicate missing 
;supply (in the case of unfillable demand), missing demand (in the case of 
;unusable supply), or missing relations (in either case). Pre-processing will 
;automatically find islands, and all equivalence classes / strongly connected
;components in the FillGraph.

;#PreProcessing Identifies Independent Data and Simplifies The Fill Graph
;One of the benefits of finding equivalence classes is that we can choose to 
;only simulate SRCs that are dependent. This lets us reduce the amount of work 
;into independent batches and divide the simulation into N smaller simulation 
;runs. This can provide a big cost savings for certain analyses by reducing the 
;total number of events and entity updates that must be processed, and 
;independent simulations can be computed in parallel.  Finally, pre-processing 
;the FillGraph actually reduces the complex web of interior nodes, and provides
;a simplified graph that is very quick to search due to every path in the 
;reduced graph having, at most, 2 steps from unfilled to filled.

;##Default Implementation for Querying Rules to Find the Most Suitable Supply
;The default scheme for prioritizing supply is to query the ruleset to
;find an ordered set of matches between sets, or buckets, of supply and the
;demand in need of filling.  Due to substitution and other criteria, the buckets
;of supply may be of lower "cost" to utilize for the demand than others.  This 
;corresponds to a weighted path in the FillGraph.  The query is essentially a 
;variation of the K-shortest paths algorithm, where the shortest path is found, 
;then the next, ... as needed.  These paths describe a context, or a 
;justification for selecting a sub set of supply, and serve to classify the 
;entire subset of supply as a certain class, with a uniform priority.  This is
;desirable, as it effectively partitions the search space and provides an 
;efficient means of selecting sets of units for possible deployment.    

;;When time permits, I intend to take this to the next logical step and just
;;implement a min-cost max-flow fill algorithm instead of the k shortest   
;;paths.

;;#Fill Rule Interpretation
;;Given a common description of a demand category, the fill function should be 
;;able to interpret the demand rule into a corresponding supply rule.  After
;;converting to a common supply rule, different fill functions may interpret 
;;the same rule in completely different manners, or we can have a robust 
;;language for describing and interpreting rules.  The end result should be a 
;;way to map classes of demand to ordered sets of supply in a general and 
;;flexible fashion.

;;In the legacy implementation, the rule structure is actually embedded in the 
;;topology of a directed graph.  Labelled 'sink nodes' on the graph represent 
;;possible demand categories, while primitive 'source nodes' are primitive 
;;elements of supply.  Complex rules for substitution and equivalence are 
;;encoded in the intermediate arcs of the graph.  These functions help interpret
;;to and from the graph encoding in the legacy implementation.

;;Standard labels for defining source and sink rules.  Maybe memoize these.
(def sink-label   (memoize (fn [x] (fg/sink-label x))))
(def source-label (memoize (fn [x] (fg/source-label x))))


;;Dispatch function for reading demands as demand rules.
(defn supply-cat 
  ([demand]                    (core/category-type (get demand :src)))
  ([demand category] (core/category-type category)))


;;Old...no need for the fillstore as of yet.
;; (defn supply-cat 
;;   ([demand]                    (core/category-type (get demand :src)))
;;   ([demand fillstore]          (core/category-type (get demand :src)))
;;   ([demand fillstore category] (core/category-type category)))

;Not yet implemented, but the intent is to have a simple idiom for parsing 
;abstract categories into rules that can be used to query supply or demand 
;or anything.
(defn rule->supply-rule [rule]
  (throw (Exception. "Not implemented!")))

;;Refactor -> we don't need a separate rule here really, just wrapping 
;sink-label.  Note: we're passing in a new notion of categories when we 
;fill demands now.  This means we can't just derive the rule and be done.
;We need to dispatch on the category, and map the demand category into an 
;appropriate rule that query can apply to the supply to find elements
;of supply.

;;Originally implemented as a multimethod, currently refactored into a
;;function for performance (and ostensibly simpler).  We'll see if we
;;need to expand beyond this and go with a protocol or a multimethod.  
;;Doubtful.
;;TODO# determine if fillstore is necessary here.  It may just be a
;;stub.  Also revisit different types of rules, currently only
;;handling simply rules, where the rule is the demand's src.  Note: 
;;we could opt to use complex rules in lie of the SRC for the demand, 
;;or have the SRC map to a pattern....
(defn derive-supply-rule
  ([demand category] 
    (case (supply-cat demand category)
                                        ;for simple categories, ala "SRC_1" or :SRC_1, we just use the existing 
                                        ;label for the demand, which maps to a node in the fill graph.  This label is 
                                        ;typically a standard alphanumeric SRC, but it could be any identifier the user
                                        ;chooses.
      :simple         category ;(sink-label category)
                                        ;For categories that require a demand group, namely follow-on fills, we 
                                        ;just inject the sink-label into the vector: 
                                        ;     [src group] ->  [(sink-label src) group]
      :src-and-group  (rule->supply-rule category) ;[(sink-label (first category)) (second category)]
                                        ;For complex categories that contain information in a map structure, we 
                                        ;will have a way to parse the supply rule, which could be arbitrarily complex.
      :rule-map       (rule->supply-rule category)
      (throw (Exception. (str [:unknown-demand-category (supply-cat demand category)
                               {:demand demand :category category}])))))
  ([demand] (derive-supply-rule demand  (get demand :src))))
;  ([demand ctx] (derive-supply-rule demand (marathon.sim.core/get-fillstore ctx) (get demand :src))))


;;aux functions for coercing from "user defined input"
;;for sourcing rules into clean clojurfied input.
(defn clean-source-rule [v]
  (-> v
      (clojure.string/upper-case)
      (clojure.string/replace "_" "-")))

(let [clean! (spork.util.general/memo-1
                clean-source-rule)]
  (defn parse-source-rule [k]
    (if (keyword? k)
      k
      (clean! k))))

(defn resolve-source-first [sf]
  (if-let [r  (get @query/stock-queries (parse-source-rule sf))]
    r
    (throw (Exception. (str "unknown source-first rule: " sf)))))

;;Tom Hack 26 May 2016
;;If we're not SRM demand, i.e. the category is something other than
;;SRM, we use the default category so as to not restrict our fill.
(def restricted-categories
  {"SRM" "SRM"
   :SRM  :SRM
   "NonBOG" "NonBOG"
   "NonBOG-RC-Only" "NonBOG"
   :NonBOG :NonBOG})

;;Ensures that we only allow StartStates
;;that exist in the unit's policy....
(defn has-transition? [st]
  (fn [u]
    (protocols/next-position (:policy u) st)))

;;Note: this is pretty crucial for the fill process, it provides all of
;;the ordering and filtering context, derived from the demand record.

;;compute a compatible supply catesgory based on the demand.
(defn derive-category [d supply-category]
  (if (vector? supply-category)
    (let [[src groups] supply-category]
      (groups (:demandgroup d)))
    (let [c (get d :category :default)]
      (or (when (restricted-categories c) c)
          :default))))

;;TODO# flesh this out, for now it fits with our match-supply expressions.
(defn demand->rule
  ([d supply-category]
   (let [category (derive-category d supply-category)
         r   {:src  (get d :src)
              :cat   category ;;
              :name (get d :name)
              :order-by (resolve-source-first (get d :source-first "uniform"))
              :required (d/required d)
              }]
     (if  (or (= category :default) (nil? (:StartState d)))
       r
       ;;we have a preference for startstate...
       (assoc r :where  (has-transition? (:StartState d))))))
  ([d] (demand->rule d :default)))
      

;;##Finding and Ordering Supply  

;#Legacy Means For Generating A Stream of Deployable Supply
;In the legacy implementation,  a SupplyGenerator object served as a poor man's 
;version of a sequence abstraction (or an iterator in other languages).  It used
;internal state, combined with a reference to one or more buckets of supply, to
;walk the shortest possible paths defined by an external query, visiting each
;set of supply and stitching together what appeared to be a single sequence of 
;units.  The abstract sequence of units returned by the generator was thus an 
;ordered traversal of the supply, as dictated by the fillgraph.

;The SupplyGenerator also used a secondary prioritization function to determine
;the order for each subsequence of units it found. When the generator visited 
;each "bucket" of supply, it used a prioritization function to sort the bucket, 
;then collated the ordered subsequence of units into an abstract 
;"total ordering" of all supply, using fairly complex and dynamic prioritization
;rules and supply->demand relationships.  Thankfully, the underlying traversal 
;and ordering mechanism was hidden behind a simple stream-like API, which 
;facilitated higher-order expressions like "take" and "next" to allow consumers
;to view the SupplyGenerator as a sequence of units.

;#Default Unit Comparison
;To deal with highly variable supply ordering rules, the SupplyGenerator had a 
;modular unit prioritization object (a UnitComparer) for establishing the 
;fine-grained ordering of subsequences of units.  The default UnitComparer 
;prioritization was (and remains) based on a Unit's absolute position in its 
;current rotational policy.  This position, or policy coordinate, is a value 
;between [0.0 1.0]. The policy coordinate is computed by comparing the unit's 
;time in the current cycle, with the expected length of the cycle, or 
;CycleTime / CycleLength, where CycleTime <= CycleLength.  Computing the policy 
;coordinate provides a normalized representation of each unit's "progress" in 
;its lifecycle.  Since typical rotational policies indicate a positive relation 
;between cycle time and readiness, the proportional representation of the policy
;coordinate provides a convenient measure of readiness as well.
;
;__Note__ , this assumption holds for known rotational policies, but may fail if 
;readiness is not a function of time in cycle.  Also, other unit prioritization 
;functions exist, including preferences by component (either AC or RC first).  
;In the end, notions of prioritization are highly context sensitive, depending 
;on the goals of the study.  They are designed to be modular, easily changed,
;composed, and selectively applied.

;##Default Legacy Total Ordering of Supply
;The default notion of ordering used in the legacy object model will remain the 
;default in the functional programming version, with ostensibly identical 
;mechanisms (minus the reliance on objects and mutable state).
;Using the policy coordinate as a comparator, the FillFunction orders each 
;subset of units so that their policy coordinate, and thus readiness, is sorted
;in descending order.    
;Thus, the total ordering is a sorting of units by Min Path Length, then Max 
;policy coordinate.  This provides a natural ordering that corresponds with 
;rotational policy, in that units, regardless of component, are drawn evenly 
;according to relative readiness, starting with units that directly match the
;capability demanded, and that have had the most time to increase readiness 
;(most capable, most ready), ending with units that least match the capability 
;demanded, with the least amount of time to increase readiness (least capable,  
;least ready). When units deploy, the context of the fill path is annotated on 
;their deployment record, in addition to other stats such as path length.  

;#Effectful Filling Under the Legacy FillFunction
;Under the legacy FillFunction object, we typically prepped it with a query, 
;which loaded the supply generator (providing a total ordering of eligible 
;supply), and applied the "take" method of the FillFunction to a numeric 
;argument.  The result of "taking" 10 items, for example, would provide a 
;collection - implicitly the 10 (or less) most "suitable" elements of supply, 
;represented by a collection of FillData objects.    

;Some forms of fill could cause the creation of just-in-time units, or Ghosts,
;which caused the side-effect of actually creating supply upon generation 
;(inside the SupplyGenerator). Going forward in the functional design, we make 
;such side effects explicit data that must be interpreted and evaluated, rather
;than using hidden mutation.  

;With a valid, prioritized order of units in hand, the FillFunction tried to 
;fill the demand by selecting units in order until demand is filled, or no more
;units exist.  Completely filling a demand would trigger additional effects.  
;Under the functional version, we maintain the spirit of these features - i.e. 
;the communication of success or failure to fill - but again, we use explicit 
;data structures to communicate effects.

;##Legacy (Mutable Object-Based) Fill Summary
;All the fill function did was wrap both the fill graph and a mutable generator, 
;where the generator served as a live "cursor" to buckets or partitions of 
;supply.  This was effectively a poor man's inelegant version of a stream.
;It provided an interface to initialize queries and maintain state.  Since we 
;were pulling from multiple "buckets" of supply, according to an ordering 
;dictated by shortest paths in the fillgraph, we had the "generator" 
;automatically pointing at multiple buckets

;##Filling Functionally
;Under the new functional design, the fill-function is a chunk of data that 
;contains the rules necessary for ordering a set of units relative to a demand. 
;When combined with a query function, the data in the fill-function will 
;inform the ordering of any supply relative to any demand, and provide an 
;ordered lazy sequence of candidates. This eliminates the complexity from have 
;multiple mutable buckets to draw from in the legacy verison.  Now, we simply 
;have an abstract "sequence" of candidates to draw from while we fill.  The 
;function that generates said sequence, __query__, may be very complex, but it 
;hides all of the complexity for us and allows us to trivially change how supply
;is ordered.

;;The protocol ISupplier provides an abstraction for systems that can interpret
;;a rule that describes classes of supply, find and order eligible supply in a 
;;given store.  While rules are abstract, they will be implemented in a common
;;language for describing fill rules, which should facilitate communication 
;;between a party asking for supply, and the supplier who can provide it.  This 
;;should allow for a very flexible arrange of rule descriptions, as well as
;;varying degrees of interpretation rule interpretation, and the generation of 
;;promised supply.
(defprotocol ISupplier 
  (query [s rule store] 
  "Given a rule that orders eligible supply, s applies the
   rule to store to return an ordered sequence of promised supply."))
    
  
;;__TODO__ Provide a default implementation of the ISupplier that can parse 
;;simple rules.  Specifically, one that can match src to src for instance.
;;__TODO__ Provide a set of extended Supplier definitions, possibly combinators
;;for compound supply rules, that allow users to easily define prioritization 
;;and possible supply generation criteria.  For instance, we need a way to 
;;implement the existing default stack of preferences: ordered by fence, 
;;followon status, capability (i.e. substitution), max normalized dwell.  
;;Another would be a function that can generate new supply according to some 
;;constraint (possibly unconstrained).

;;##High Level Fill 

;#It's all about finding supply.
;The ultimate purpose of querying a fill-function is to answer a simple query:  
;Given a demand (or a rule that describes the demand), and a supplystore
;(which contains supply), which elements of supply are are most suitable to fill 
;the demand, based on the interpretation of the fill-function?

;#First: "Find the most suitable supply".
;This represents an ordered sequence of candidate fills....we may not, in fact,
;utilize every candidate.  A better description is that __find-supply__ provides
;a list of  fill-promises, which are realized as needed.  A fill-promise is a 
;function that consumes the current context and returns a pair of 
;[promised-unit, new-context].  That way we can update the context by realizing
;the fill-promise (i.e. applying it against a context we thread through), and 
;then do something with the unit that was promised.  Since these are just 
;promises, i.e. potential supply, we don't mutate anything or make any changes
;to the context until we need to.  

;find-supply::(rule->demand->demandgroup->name->supply->phase->[fill-promise])  
;             ->supply->rule->[fill-promise]  
;where fill-promise::(simcontext->'a->[filldata,simcontext])  


;;Note: filldata == {:keys [rule fillPath pathlength followon source]}
;;simple....
;;TODO# replace with record or type.
;;BUGFIX: we strip out the :dt here if necessary.
(defn unit->filldata [cat src length u]
  ;;we're out of position here..
  ;;rule ;fillPath ;pathLength
  (->> (dissoc u :dt)
       (filldata/->fill  cat src length
          (if (or (:followon u)
                  (= (:state u) :followon))
            "TRUE" "FALSE"))))

;;all we expect from fills is that there is a quantity
;;if there is a key for :actions, then we have some requirement.
;;otherwise, it's a default fill, we just do what's necessary to
;;deploy the unit.


;;Do we need unit->filldata?  Maybe for future supply....i.e.
;;intended fills.  Dunno.  It's okay to go greedy atm...
;;Note: we can retroactively go back in time (using imm data)
;;and update the unit's status (i.e. require it to go to mob/demob,
;;ctc training, change it's state, etc....this makes us rewind
;;the simulation though, invalidating any currently rendered
;;state.  Generalizes into searching though...which may be useful.

(defn find-supply
  "Returns an ordered sequence of actions that can result in supply.
   This effectively applies the suitability function related to fillfunc to the 
   rule, the demand, and the supply.  The result is a sequence of 
   potential fills....where potential fills are data structures that contain 
   the context of the fill (i.e. the unit, the actions required to realize the 
   fill, and other meta data), typically a filldata record."
  ([fillfunc supplystore rule]     
     (query fillfunc rule supplystore))
  ([supplystore rule]
     (map (fn [[[cat src length] u]] 
            (unit->filldata cat src length u))
          (query/match-supply rule supplystore))))

;;TODO# fillPath in our supply query is currently pretty rudimentary.
;;It may be nice, for feature parity, to have then entire fillpath
;;relayed rather than the endpoint.



;;An element of supply has a quantity associated with it.
;;It also has a set of actions associated with delivering said supply.

;;A supplier provides units at the cost of updates...
;;A supplier can provide 1 or more (possibly infinite) units.
;;Suppliers may nest inside each other (i.e. a unit living inside
;;another).

;We can coerce our list of fill promises into actual fills by applying 
;__realize-fill__ to them.  Assuming a fill promise consumes a context, we  
;apply the promise to a given context.  This should produce a pair of the 
;filldata --information about the unit realized for filling-- and an updated 
;simulation context.
;;Note: we need to alter this...I understand the reason it exists, namely
;;to allow for things like supply-generation in the face of constraints,
;;but we're losing information from the filldata as a consequence.
;;we get a fill-promise :: marathon.fill.filldata, and
;;we project it onto :: [marathon.supply.unitdata ctx]
(defn realize-fill
  "Applies the a function, fill-promise, that maps a context to a pair of 
   [filldata, updated-context].  The updated-context should represent the result
   of realizing the promised fill."
  [fill-promise ctx] 
  [(if (map? fill-promise) fill-promise
       (throw (Exception. (str "no other promise types supported"))))
                                        ;(get fill-promise :source)
   (if-let [actions (get fill-promise :actions)]
     (actions ctx) ;;perform any actions necessary 
     ctx)])
    
;#Second: Allocate a candidate fill against a demand.
;Assuming we have a candidate fill, and a demand that needs filling, we define
;the consequences of using the candidate (via some filldata) to logically "fill"
;the demand.  The result is a new context, since there may be additional 
;consequences to the context due to a fill.

;While the demand is assumed to have inspired the list of candidates, it's 
;not consequential for allocation purposes.  In fact, the list of candidates 
;could be completely random or drawn in an otherwise arbitrary fashion.
;__Note__ that would make a great _test_, having a random fill function.  

;Assuming we have a chunk of realized filldata, we define a way to apply it to 
;a demand to accomplish any updates necessary for filling.  This is a primitive
;function that will support the higher-order notion of "filling" a demand.  
;__apply-fill__ should represent the new context emerging from applying a 
;realized fill, in the form of filldata, to a demand.  

;Originally, this meant that fills would always result in a deployment at the
;end.  By elevating apply-fill into the API, we can actually implement things 
;that would otherwise be difficult, i.e. delayed fills (pre-allocating units 
;and scheduling them to deploy at a later date, while nominally "filling" the 
;demand).

(def ^:dynamic *fill-testing* true)

(def fd (atom nil))

(defn fill!
 "Deploys the unit identified in filldata to demand via the supply system."
  [t period demand deployment-count filldata  ctx]
  (let [;unit (or (:unit filldata) filldata)
        unit     (:source filldata) ;;this is not an updated unit.
        ctx      (deployment/deploy-unit  ctx unit  t demand                                
                                          (core/followon? unit))
        new-unit (store/get-entity ctx (:name unit))]        
      (supply/log-deployment! t (:locationname unit) demand new-unit   
                              deployment-count filldata nil  period ctx)))

;;temporarily changing this
#_(defn fill!
 "Deploys the unit identified in filldata to demand via the supply system."
  [t period demand deployment-count filldata  ctx]
  (let [;unit (or (:unit filldata) filldata)
        unit   (:source filldata) ;;this is not an updated unit.
        ]
    (->> (deployment/deploy-unit  ctx unit  t demand                                
                                 (core/followon? unit))
         (supply/log-deployment! t (:locationname unit) demand unit   
                                 deployment-count filldata nil  period))))

;;#Incremental Demand Filling

;;The atomic fill process rests inside a high-level function, __fill-demand__ .
;;__fill-demand__ takes any fill-promise, realizes the fill-promise, and applies
;;the the realized filldata to fill a demand. It wraps the low-level context
;;shuffling that is necessary behind a simple, high-level interface amenable to 
;;use in a reduction ala __clojure.core/reduce__ .  

;;The end result is a new context, representing the consequences of filling said 
;;demand with the promised fill.  Under this scheme, __apply-fill__ will 
;;automatically handle the realization of a fill-promise, and thread its updated
;;context through the process of deploying the unit associated with the realized 
;;filldata.  Since fill-promises are typically for single elements of supply,  
;;__fill-demand__ will typically only apply a single unit towards a demand.
;;Note: we have the demand here...so, we can pass along the details..
(defn fill-demand*
  "Enacts filling a demand, by realizing a vector of promised fills, logging if any ghosts 
   were used to fill (may change this...) and updating the context.  Applies the
   result of one promised fill to the demand, which may or may not satisfy the 
   demand."
  [t period demand  promised-fills ctx]
  (let [deployment-count (atom (or (store/gete ctx :SupplyStore :deployment-count) 0))]
    (->  (reduce (fn [ctx promised-fill]            
                   (let [[filldata ctx] (realize-fill promised-fill ctx) ;reify our fill.
                         unit     (:source filldata)
                         cnt      (swap! deployment-count unchecked-inc)
                         ] 
                     (->> ctx 
                          (filled-demand! (:name demand) (:name unit))
                          (check-ghost unit)
                          (fill! t period demand cnt filldata))))
                 ctx promised-fills)
         (store/assoce :SupplyStore :deployment-count @deployment-count))))

;;#Trying to Completely Satisfy a Demand

;Since we know how to effectively apply promised fills towards demands via 
;__fill-demand__ in an atomic fashion, we can define the notion of completely 
;filling a demand as finding the supply for the demand, using __fill-demand__ on 
;each candidate element of supply, and drawing from the supply until either the
;demand is filled, or the candidates are exhausted.  

;1. Assumes candidate preference is invariant.  There may be a time when we need
;   to re-evaluate the ordering of candidates while we're filling, i.e. the 
;   amount of fill may impact the order of candidates.  For now, we assume that 
;   the ordering of candidates is independent of the demand fill.

(def last-sel (atom nil))

;;Filling in batch now.  Should be mo betta.
;;We need to account for category values that are potentially
;;vectors like [src #{SomeDemandGroup}] that specify a desire to
;;use a follow-on category.
(defn satisfy-demand
  "Attempts to satisfy the demand by finding supply and applying promised 
   fills to the demand.  Returns a result pair of 
   [:filled|:unfilled updated-context], where :filled indicates the demand is 
   satisifed, and updated-context is the resulting simulation context."
  [demand category ctx]
  (let [rule        (demand->rule demand category)
        period      (:name (policy/get-active-period (core/get-policystore ctx)))
        t           (core/get-time ctx)
        demand-name (:name demand)  
                                        ;1)
        req      (d/required demand)
        selected (->> (find-supply ctx rule)
                      (into []    (take req)))
        _           (reset! last-sel selected)
        actual-fill (count selected)
        status      (cond (== actual-fill req) :filled
                          (zero?  actual-fill) :unfilled
                          :else :changed)
        ;; _ (when (seq selected)
        ;;     (println [:filling rule (map (comp :name :source) selected)
        ;;                                 ;(first selected)
        ;;              ]))
        ]
    [status (fill-demand* t period demand selected ctx)]))


;;testing
(comment 

(require '[marathon.sim.testing :as test])
(require '[clojure.test :refer [deftest is run-tests]])

(defn count-by [keyf xs] 
  (reduce-kv (fn [acc k v] 
               (assoc acc k (count v))) {} (group-by keyf xs)))

(def fillstore (core/get-fillstore test/testctx))
(def ds (vals (:demandmap test/test-dstore)))
(def rules (map #(derive-supply-rule % fillstore) ds))
(def complex-rule (derive-supply-rule (first ds) fillstore :category ["Alpha" "SomeGroup"]))

(deftest supply-rules 
  (is (= (count-by second rules)
         {"SRC1" 21, "SRC2" 14, "SRC3" 36})
      "Should have consistent number of srcs in supply rules.")
  (is (= complex-rule 
         [[:fillrule "Alpha"] "SomeGroup"])
      "Should have a simple pair of [fillrule group] as a result."))
        

;some dumb demands 
;(def demands 
)

;;#Pending
;;Port fill store generation functions and IO functions/constructors from legacy 
;;code.


;Process that encapsulates creating a new fillstore from coredata, appending 
;the fillstore to the core data, and then returning a scoped set of core data, 
;where the supply and demand have been reduced according to the relations
;embodied by the fillgraph.
;Assumes simState has valid supply, demand, and policystore instances 
;(i.e. they've been initialzed, probably from tables). Returns updated simState.
;Public Function simStateToScopedSimState(simstate As TimeStep_SimState, 
;    Optional generator As TimeStep_SupplyGenerator) As TimeStep_ManagerOfFill
;
;Dim ff As TimeStep_FillFunction
;Dim fg As TimeStep_FillGraph
;Dim fs As TimeStep_ManagerOfFill
;
;With simstate
;    Set fg = composeFillGraph(.supplystore, .demandstore, .policystore)
;    If generator Is Nothing Then _
;        Set generator = makeSupplyGenerator(simstate, , , 
;             simstate.parameters.getKey("DefaultSupplyPriority") = "RCPreSurge")
;    Set ff = makeFillFunction("FillFunction", .supplystore, fg, .parameters, 
;                                .context, generator)
;    Set fs = makeFillStore(fg, ff, generator)
;End With
;
;Set simstate.fillstore = fs
;'Scopes the data as a final step, since we have a handle on the fillgraph.
;Set simstate = scopeSimState(fg, simstate)
;Set ff = Nothing
;Set fg = Nothing
;Set fs = Nothing
;
;End Function

;'Constructor for building fill stores from component pieces.
;'Note, the fill store has a fillgraph, a fill function, and a supply generator.
;Public Function makeFillStore(fillgraph As TimeStep_FillGraph, fillfunction As 
;   TimeStep_FillFunction, generator As TimeStep_SupplyGenerator) 
;         As TimeStep_ManagerOfFill
;
;Set makeFillStore = New TimeStep_ManagerOfFill
;With makeFillStore
;    Set .fillgraph = fillgraph
;    Set .fillfunction = fillfunction
;End With
;End Function
;
;


;'Produces a new fill function from inputs.
;Public Function makeFillFunction
; (nm As String, supplystore As TimeStep_ManagerOfSupply, _
;    graph As IGraph, parameters As TimeStep_Parameters, _
;       context As TimeStep_SimContext, _
;           generator As TimeStep_SupplyGenerator) As TimeStep_FillFunction
;
;Set makeFillFunction = New TimeStep_FillFunction
;With makeFillFunction
;    .name = nm
;    'Decoupled
;    Set .parent = supplystore
;    'Eh...this is dubious....TODO -> separate further.
;    Set .FillRules = .AddFill(graph)
;    'Decoupled
;    Set .generator = generator
;End With
;
;End Function


;'This is the simplest initializer for building and initializing a fill store.  
;'Closest to the legacy stuff as well.
;Public Function fillStoreFromTables(simstate As TimeStep_SimState, 
;   sources As GenericTable, sinks As GenericTable, relations As GenericTable) 
;      As TimeStep_ManagerOfFill
;Dim fg As TimeStep_FillGraph
;Dim ff As TimeStep_FillFunction
;Dim sg As TimeStep_SupplyGenerator
;
;Set fg = New TimeStep_FillGraph
;Set fg = FillGraphFromTables(fg, sources, sinks, relations)
;
;Set sg = makeSupplyGenerator(simstate)
;Set ff = makeFillFunction("FillFunction", simstate.supplystore, fg.graph, 
;                              simstate.parameters, simstate.context, sg)
;Set fillStoreFromTables = makeFillStore(fg, ff, sg)
;
;Set sg = Nothing
;Set ff = Nothing
;Set fg = Nothing
;
;End Function


;Private Function getDependencies(gr As IGraph) As Dictionary
;Dim nd
;Dim x As String
;
;Set getDependencies = New Dictionary
;For Each nd In GraphLib.getNodes(gr)
;    x = translateRule(CStr(nd))
;    If Not (getDependencies.exists(x)) Then
;        getDependencies.add x, 0
;    End If
;Next nd
;    
;End Function



;Public Function translateRule(ByRef inrule As String) As String
;Dim tmp
;tmp = Split(inrule, "_")
;If UBound(tmp, 1) = 1 Then
;    translateRule = tmp(1)
;Else
;    Err.Raise 101, , 
;      "Irregular rule :" & inrule & " should be delimited by a single _ "
;End If
;End Function

;'Aux function to describe the type of island, whether a source or a sink.
;Private Function islandType(nodename As String) As String
;If InStr(1, nodename, "SOURCE") > 0 Then
;    islandType = "Supply"
;ElseIf InStr(1, nodename, "FILLRULE") > 0 Then
;    islandType = "Demand"
;Else
;    Err.Raise 101, , "Island is neither supply nor demand"
;End If
;
;End Function

;#Constructors and Data Munging Functions
;Constructors to create all three, independently, now exist in this module.  
;Along with decoupled construction, operations for sourcing demands, relative to
;rules specified in a FillFunction, from a supply to a demand, are provided.  
;sourceDemand is probably the most notable/used function, as it...sources 
;demand!

;;Implement defquery or defgenerator, to allow easy composition of fill 
;;functions.  This requires cljgraph.


;; (defn apply-fill
;;   "Deploys the unit identified in filldata to demand via the supply system."
;;   [filldata demand ctx]
;;   (core/with-simstate [[fillstore supplystore policystore parameters] ctx]
;;     (let [unit        (or (:unit filldata) filldata)
;;           ou          (spork.entitysystem.store/get-entity ctx (:name unit))
;; ;          _ (println [:apply-fill (:locationname unit) (:locationname ou)])
;;           t           (sim/get-time ctx)
;;           ]
;;       (deployment/deploy-unit ctx unit t demand
;;                               (core/followon? unit)))))

;; (defn fill-demand
;;   "Enacts filling a demand, by realizing a promised fill, logging if any ghosts 
;;    were used to fill (may change this...) and updating the context.  Applies the
;;    result of one promised fill to the demand, which may or may not satisfy the 
;;    demand."
;;   [demand ctx promised-fill]
;;   (let [[fd ctx] (realize-fill promised-fill ctx) ;reify our fill.
;;         ;_ (println fd)
;;         unit     (or (:unit fd) fd)
        
;;         ;_ (println [:fill-demand (:locationname unit)])
;;         ] 
;;     (->> ctx 
;;          (filled-demand! (:name demand) (:name unit))
;;          (check-ghost unit)
;;          (apply-fill fd demand))))

;; ;; think category is akin to rule here...
;; (defn satisfy-demand-  "Attempts to satisfy the demand by finding supply and applying promised 
;;    fills to the demand.  Returns a result pair of 
;;    [:filled|:unfilled updated-context], where :filled indicates the demand is 
;;    satisifed, and updated-context is the resulting simulation context."
;;   [demand category ctx]
;;   (let [;fillstore   ;(core/get-fillstore     ctx)
;;         ;fillfunc    ;(core/get-fill-function ctx)
;;         ;supplystore (core/get-supplystore   ctx)
;;         rule        (demand->rule demand)
;;         demand-name (:name demand)
;;        ; _ (println [:satisfying-demand demand (d/required demand)])
;;         ;1)
;;         candidates  (find-supply ;fillfunc
;;                                         ;supplystore

;;                                 ctx
;;                                  rule)]
;;     (loop [d           demand           
;;            xs          candidates 
;;            fill-status :unfilled
;;            remaining   (d/required d)
;;            current-ctx ctx]
;;       (do ;(println remaining)
;;           (cond (zero? remaining)      [:filled     current-ctx]
;;                 (empty? xs)            [fill-status current-ctx]
;;                 :else  
;;                 (let [
;;                       nextctx (fill-demand d current-ctx (first xs))   ;;first/next recursion slow.
;;                       nextd (-> (core/get-demandstore nextctx)
;;                                 (dem/get-demand demand-name))]
;;                   (recur nextd (rest xs) :added-fill (unchecked-dec remaining) nextctx)))))))
