;;A collection of functions that define the process of mapping classes of demand
;;to eligible supply, and filling demand with suitable supply.  The fill system
;;acts as a service for the engine, and provides operations used - primarily - 
;;by the demand simulation.  The process of filling pushes changes on the 
;;supply, since the act of filling usually consumes some resources in supply and
;;motivates a kind of motion in the supply simulation.
(ns marathon.sim.fill
  (:require [marathon.data   [protocols :as protocols]]
            [marathon.demand [demanddata :as d] [demandstore :as dstore]]
            [marathon.supply [unitdata :as udata]]
            [marathon.sim [core :as core] [demand :as dem] [supply :as supply]
                          [policy :as policy] [unit :as u] 
                          [deployment :as deployment]]           
            [sim  [simcontext :as sim] [updates :as updates]]
            [util [tags :as tag]]))

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
  (sim/trigger-event :FillDemand demand-name unit-name "Filled Demand" nil ctx))  

;ghosts raise special attention when they deploy.
(defn ghost-deployed! [demand-src ctx]
  (sim/trigger-event :GhostDeployed demand-src demand-src 
       "Filled demand with  ghost"  :normal ctx))

;ghosts raise special attention if they followon.
(defn ghost-followed! [demand-src ctx]
  (sim/trigger-event :GhostDeployed demand-src demand-src 
     "Ghost followed on to another demand" :followon ctx))

;;Auxillary function to broadcast information about just-in-time, or "ghost" 
;;unit utilization.  May be replaced with something more general in the future.
(defn check-ghost [unit ctx]
  (if (not (core/ghost? unit)) ctx
      (if (core/followon? unit) 
        (ghost-followed! unit ctx) 
        (ghost-deployed! unit ctx))))

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

;#Legacy Filling Via the FillFunction Object#
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
;#Fill Rules and The Fill Graph#
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
;*Islands Denote Possible Data Errors*
;In the case where there is NO path from either supply or demand, we have 
;"islands", or nodes that are unreachable (class B from the paragraph above).  
;These islands are usually the result of data errors, and indicate missing 
;supply (in the case of unfillable demand), missing demand (in the case of 
;unusable supply), or missing relations (in either case). Pre-processing will 
;automatically find islands, and all equivalence classes / strongly connected
;components in the FillGraph.

;*PreProcessing Identifies Independent Data and Simplifies The Fill Graph
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

;;#Fill Rule Interpretation
;;Given a common description of a demand category, the fill function should be 
;;able to interpret the demand rule into a corresponding supply rule.  After
;;converting to a common supply rule, different fill functions may interpret 
;;the same rule in completely different manners, or we can have a robust 
;;language for describing and interpreting rules.  The end result should be a 
;;way to map classes of demand to ordered sets of supply in a general and 
;;flexible fashion.

;;In the legacy implementation, the rule structure is actual embedded in the 
;;topology of a directed graph.  Labelled 'sink nodes' on the graph represent 
;;possible demand categories, while primitive 'source nodes' are primitive 
;;elements of supply.  Complex rules for substitution and equivalence are 
;;encoded in the intermediate arcs of the graph.  These functions help interpret
;;to and from the graph encoding in the legacy implementation.

;;Standard labels for defining source and sink rules.  Maybe memoize these.
(defn sink-label   [x] [x :sink])   
(defn source-label [x] [x :source]) 

;;Refactor -> we don't need a separate rule here really, just wrapping 
;sink-label.  Note: we're passing in a new notion of categories when we 
;fill demands now.  This means we don't just derive the rule and be done.
;We need to dispatch on the category, and map the demand category into an 
;appropriate rule that query can apply to the supply to find elements of supply.
(defmulti derive-supply-rule 
  (fn [demand fillstore & [category]]  
    (let [cat (or category (:src demand))]  (core/category-type category))))

;for simple categories, ala "SRC_1" or :SRC_1, we just compute existing 
;label for the demand, which maps to a node in the fill graph.
(defmethod derive-supply-rule :simple 
  [demand fillstore & [category]]
  (sink-label category))

;For categories that require a demand group, namely follow-on fills, we 
;just inject the sink-label into the vector: 
;     [src group] ->  [(sink-label src) group]
(defmethod derive-supply-rule :src-and-group 
  [demand fillstore & [category]]
  [(sink-label (first category)) (second category)])

;Not yet implemented, but the intent is to have a simple idiom for parsing 
;abstract categories into rules that can be used to query supply or demand 
;or anything.
(defn rule->supply-rule [rule]
  (throw (Exception. "Not implemented!")))

;For complex categories that contain information in a map structure, we 
;will have an way to parse the supply rule, which could be arbitrarily complex.
(defmethod derive-supply-rule :rule-map
  [demand fillstore & [category]]
  (rule->supply-rule category))


;##Legacy Means For Generating A Stream of Deployable Supply
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

;*Default Unit Comparison
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

;##Default Legacy Total Ordering of Supply
;The default notion of ordering used in the legacy object model will remain the 
;default in the functional programming version, with ostensibly identical 
;mechanisms (minus the reliance on objects and mutable state).
;Using the policy coordinate as a comparator, the FillFunction orders each 
;subset of units so that their policy coordinate, and thus readiness, is sorted
;in descending order.  The total ordering then, is a sorting of units by Min 
;Path Length, then Max policy coordinate.  This provides a natural ordering that
;corresponds with rotational policy, in that units, regardless of component, are
;drawn evenly according to relative readiness, starting with units that directly
;match the capability demanded, and that have had the most time to increase 
;readiness (most capable, most ready), ending with units that least match the 
;capability demanded, with the least amount of time to increase readiness (least 
;capable, least ready). When units deploy, the context of the fill path is 
;annotated on their deployment record, in addition to other stats such as path 
;length.
;
;#Effectful Filling Under the Legacy FillFunction
;Under the legacy FillFunction object, we typically prepped it with a query, 
;which loaded the supply generator (providing an total ordering of eligible 
;supply), and applied the "take" method of the FillFunction to a numeric 
;argument.  The result of "taking" 10 items, for example, would provide a 
;collection - implicitly the 10 (or less) most "suitable" elements of supply, 
;represented by a collection of FillData objects.  Some forms of fill could 
;cause the creation of just-in-time units, or Ghosts, which caused the 
;side-effect of actually creating supply upon generation 
;(inside the SupplyGenerator).  Going forward in the functional design, we 
;make such side effects explicit data that must be interpreted and evaluated, 
;rather than using hidden mutation.
;With a valid, prioritized order of units in hand, the FillFunction tried to 
;fill the demand by selecting units in order until demand is filled, or no more
;units exist.  Successfully or completely filling a demand would trigger 
;additional effects.  Under the functional version, we maintain the spirit of 
;these features - i.e. the communication of success or failure to fill - but 
;again, we use explicit data structures to communicate effects.

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
;function that generates said sequence, query, may be very complex, but it hides
;all of the complexity for us and allows us to trivially change how supply is 
;ordered.

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
;utilize every candidate.  A better description is that find-supply provides a 
;list of  fill-promises, which are realized as needed.  A fill-promise is a 
;function that consumes the current context and returns a pair of 
;[promised-unit, new-context].  That way we can update the context by realizing
;the fill-promise (i.e. applying it against a context we thread through), and 
;then do something with the unit that was promised.  Since these are just 
;promises, i.e. potential supply, we don't mutate anything or make any changes
;to the context until we need to.
;find-supply::(rule->demand->demandgroup->name->supply->phase->[fill-promise])
;             ->supply->rule->[fill-promise]
;where fill-promise::(simcontext->'a->[filldata,simcontext])

(defn find-supply
  "Returns an ordered sequence of actions that can result in supply.
   This effectively applies the suitability function related to fillfunc to the 
   rule, the demand, and the supplybucket.  The result is a sequence of 
   potential fills....where potential fills are data structures that contain 
   the context of the fill (i.e. the unit, the actions required to realize the 
   fill, and other meta data), typically a filldata record."
  [fillfunc supplystore rule]
  (query fillfunc rule supplystore))

;We can coerce our list of fill promises into actual fills by applying 
;realize-fill to them.  Assuming a fill promise consumes a context, we simply 
;apply the promise to a given context.  This should produce a pair of the 
;filldata --the context of the unit realized for filling-- and an updated 
;context.

(defn realize-fill
  "Applies the a function, fill-promise, that maps a context to a pair of 
   [filldata, updated-context].  The updated-context should represent the result
   of realizing the promised fill."
  [fill-promise ctx] (fill-promise ctx))

;#Second: Allocate a candidate fill against a demand.
;Assuming we have a candidate fill, and a demand that needs filling, we define
;the consequences of using the candidate (via some filldata) to logically "fill"
;the demand.  The result is a new context, since there may be additional 
;consequences to the context due to a fill.

;While the demand is assumed to have inspired the list of candidates, but it's 
;not consequential for allocation purposes--in fact the list of candidates could
;be completely random or drawn in an otherwise arbitrary fashion.
;-Note- that would make a great _test_, having a random fill function.

;Assuming we have a chunk of realized filldata, we define a way to apply it to 
;a demand to accomplish any updates necessary for filling.  This is a primitive
;function that will support the higher-order notion of "filling" a demand.  
;apply-fill should represent the new context emerging from applying a realized 
;fill, in the form of filldata, to a demand.  Originally, this meant that fills 
;would always result in a deployment at the end.  By elevating apply-fill into
;the API, we can actually implement things that would otherwise be difficult, 
;i.e. delayed fills (pre-allocating units and scheduling them to deploy at a 
;later date, while nominally "filling" the demand).

(defn apply-fill
  "Deploys the unit identified in filldata to demand via the supply system."
  [filldata demand ctx]
  (let [unit        (:unit filldata)
        fillstore   (core/get-fillstore ctx)
        supplystore (core/get-supplystore ctx)
        policystore (core/get-policystore ctx)
        params      (core/get-parameters ctx)
        t           (sim/get-time ctx)]
    (deployment/deploy-unit ctx unit t demand filldata 
                            (core/interval->date t ctx) (core/followon? unit))))

;;#Incremental Demand Filling

;;We wrap the atomic fill process inside a high-level function, fill-demand.
;;fill-demand takes any fill-promise, realizes the fill-promise, applies the 
;;the realized filldata to fill a demand. It wraps some of the low-level context
;;shuffling that is necessary behind a simple, high-level interface amenable to 
;;use in a reduction.
;;The end result is a new context, representing the consequences of filling said 
;;demand with the promised fill.  Under this scheme, apply-fill will 
;;automatically handle the realization of a fill-promise, and thread its updated
;;context through the process of deploying the unit associated with the realized 
;;filldata.  Since fill-promises are typically for single elements of supply,  
;;fill-demand will typically only apply a single unit towards a demand.

(defn fill-demand
  "Enacts filling a demand, by realizing a promised fill, logging if any ghosts 
   were used to fill (may change this...) and updating the context.  Applies the
   result of one promised fill to the demand, which may or may not satisfy the 
   demand."
  [demand ctx promised-fill]
  (let [[filldata ctx] (realize-fill promised-fill ctx) ;reify our fill.
         unit    (:source filldata)] 
    (->> ctx 
         (filled-demand! (:name demand) (:name unit))
         (check-ghost unit)
         (apply-fill filldata demand)))) 

;;#Trying to Completely Satisfy a Demand

;Since we know how to effectively apply promised fills towards demands via 
;fill-demand in an atomic fashion, we can define the notion of completely 
;filling a demand as finding the supply for the demand, using fill-demand on 
;each candidate element of supply, and drawing from the supply until either the
;demand is filled, or the supply runs out.
;1)Assumes candidate preference is invariant.  There may be a time when we need
;  to re-evaluate the ordering of candidates while we're filling, i.e. the 
;  amount of fill may impact the order of candidates.  For now, we assume that 
;  the ordering of candidates is independent of the demand fill.
;satisfy-demand::demand->category->ctx->[fill-status ctx]
;                where fill-status = :filled | :unfilled

(defn satisfy-demand
  "Attempts to satisfy the demand by finding supply and applying promised 
   fills to the demand.  Returns a result pair of 
   [:filled|:unfilled updated-context], where :filled indicates the demand is 
   satisifed, and updated-context is the resulting simulation context."
  [demand category ctx]
  (let [fillstore   (core/get-fillstore ctx)
        fillfunc    (core/get-fill-function ctx)
        supplystore (core/get-supplystore ctx)
        candidates  (find-supply fillfunc supplystore ;1)
                      (derive-supply-rule demand fillstore category))
        demand-name (:name demand)]
    (loop [d demand           
           xs candidates 
           fill-status :unfilled
           current-ctx ctx]
      (cond (zero? (:required d)) [:filled     current-ctx]
            (empty? xs)           [fill-status current-ctx]
            :else  
              (let [nextctx (fill-demand d current-ctx (first xs))
                    nextd (-> (core/get-demandstore nextctx)
                              (dem/get-demand demand-name))]
                (recur nextd (rest xs) :added-fill nextctx))))))

;;#Pending
;;Port fill store generation functions and IO functions/constructors from legacy 
;;code.

;#Constructors and Data Munging Functions
;Constructors to create all three, independently, now exist in this module.  
;Along with decoupled construction, operations for sourcing demands, relative to
;rules specified in a FillFunction, from a supply to a demand, are provided.  
;sourceDemand is probably the most notable/used function, as it...sources 
;demand!

;;Implement defquery or defgenerator, to allow easy composition of fill 
;;functions.  This requires cljgraph.

