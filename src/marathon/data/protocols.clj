;This is a conglomeration of Marathon protocols, derived from what used to 
;be VBA interface classes.  I'll probably be trimming these down significantly, 
;since much of the functionality can be replaced with simpler functions on the 
;core data structures....especially if the core data structures are just 
;maps or records (which support a map API).
(ns marathon.data.protocols
  (:use [spork.util.metaprogramming :only [keyvals->constants]]
        [spork.cljgraph.core :as graph]))

;This is the policy interface.  We have both constant and composite 
;(or time/event variant) policies. This interface is used to implement a common 
;design pattern, the composite pattern. Basically, the old TimeStep_Policy 
;elements are considered to be atomic implementations of this interface.

;They provide the building blocks of more complex, or composite, policies.  
;On their own, they never actually change, and are oblivious to time or events.

;Conversely, composite policies implement the interface differently, and provide
;some extra methods to deal with the possibility of time-driven, or event-driven 
;policy changes. We care about this because there will be event-driven policy 
;changes via a surge.
    
;Composite policies allow us to easily implement a "policy schedule" without 
;having it look any different from a normal policy.  In effect, units subscribe 
;to a unique policy, and the policy implementation takes care of the rest.
;    When an "internal" policy change or variation takes place, the composite 
;    policy will be responsible for updating the unit.  It performs this update 
;    via telling the unit to change policies from the active policy to the next 
;    scheduled policy.  This is all done via atomic policy operations. It then 
;    resets the unit's policy pointer to itself.
   

;TOM Change 20 May 2011 -> Policy nodes are NOW POSITIONS, no longer Locations.
;    'Change in nomenclature.
;    'Has significant consquences, in that it separates spatial location from policy position.

;TOM change 3 Jan 2011
;Class for encapsulating implementations of policies ....
;This is a datastructure designed to allow us to flexibly define Multiple policies.
;Policy is = a state transition graph, the sequence of Positions a unit will cycle through
;and parameters that guide said policy.
;It is my contention that we can describe a vast number of policy types, including those currently
;employed, by a parameterized function.
;The crucial parameters are ....
;    'Name
;    'CycleLength 'duration of a standard cycle, absent interference
;    'Minimum Dwell
;    'Maximum Dwell
;    'Maximum BOG
;    'Lower Deployment Window
;    'Upper Deployment Window
;    'Overlap
;Constants used for policy definition, among other things.  Imported from the 
;original VBA implementation for the policystore. 
;Might re-think this, for now it's a way of porting the existing implementation
(def policyconstants 
  {:Bogging "Bogging"
   :Dwelling "Dwelling"
   :BogDeployable "BoggingDeployable"
   :DwellDeployable "DwellingDeployable"
   :Deployable "Deployable"
   :Spawning   "Spawning"
   :Deploying  "Deploying"
   :Overlapping "Overlapping"
   :Waiting     "Waiting"
   :NotDeployable "NotDeployable"
   :ReturnToDeployable "ReturnToDeployable"
   :AC12 "AC12" 
   :AC13 "AC13" 
   :RC14 "RC14" 
   :RC15 "RC15" 
   :AC11 "AC11"
   :RC11 "RC11"
   :RC12 "RC12"
   :GhostPermanent12 "GhostPermanent12"
   :GhostPermanent13 "GhostPermanent13"
   :GhostTransient12 "GhostTransient12"
   :GhostTransient13 "GhostTransient13"   
   :reset "Reset"
   :train "Train"
   :ready "Ready"
   :available "Available"
   :deployed "Deployed"
   :demobilization "DeMobilization"
   :SubSymbol  "{>"
   :EquivSymbol "="})

(keyvals->constants policyconstants) ;make the constants first class symbols.
;inherited from substitution rules, may be vestigial.
(keyvals->constants {:Equivalence :Equivalence :Substitution :Substitution})


;RECONCILE BETWEEN THIS ONE AND ABOVE
;need a protocol for policies...
(defprotocol IRotationPolicy 
  (atomic-name       [p])
  (bog-budget        [p])
  (get-active-policy [p])
  (get-policy        [p period])
  (policy-name       [p])
  (next-position     [p position])
  (overlap           [p])
  (get-position-graph   [p]) ;if we have this, we can build the rest... 
  (previous-position    [p position])
  (start-deployable     [p])
  (stop-deployable      [p])
  (start-state          [p])
  (transfer-time    [p start-position end-position])
  (cycle-length     [p])
  (end-state        [p])
  (get-cycle-time   [p position])
  (get-policy-type  [p])
  (get-position     [p cycletime])
  (get-state        [p position])
  (max-bog          [p])
  (max-dwell        [p])
  (max-mob          [p])
  (min-dwell        [p])
  (get-locations    [p]))

;;Functions for working with policies that can be changed after creation.
(defprotocol IAlterablePolicy
  (set-deployable       [p tstart tfinal] )
  (set-deployable-start [p cycletime]     )
  (set-deployable-stop  [p cycletime]     )
  (add-position         [p name state]    )
  (add-route            [p start destination transfer-time] )
  (set-position-graph   [p g])
  (merge-policy-stats   [p m]))

;;Functions for working with composite policies.
(defprotocol IPolicyContainer
  (add-policy       [p policy] 
                    [p period policy]))

;;Helper function to allow us to push maps into policies as positions.
;;Basically sets the state associated with a policy position.
(defn add-positions [p xs]
  (if (map? xs)
      (reduce-kv (fn [acc pos state] (add-position acc pos #{state})) 
                 p xs)
      (reduce (fn [acc pos] (add-position acc pos {})))))

(defn add-routes [p rs]
  (reduce (fn [acc [from to t]]
            (add-route acc from to t))
          p rs))

(definline between? [t l r]
  `(and (>= ~t ~l) (<= ~t ~r)))

(def modifiers #{:deployable :not-deployable})
(defn modifier? [pos] (contains? modifiers pos))

;;Might want to cache this stuff....
;;Include a facility for defining deployable states..
(defn deployable-state? [s] (:deployable s))

;;used to be isDeployable
(defn deployable-when?      [p cycletime]
  (between? cycletime (start-deployable p) (stop-deployable p)))

;;used to be deployable
;;A position is deployable in a policy if the node data associated 
;;with the position is deployable.  We just store deployability in 
;;each node.
(defn  deployable-at?       [p position]  
  (deployable-state? (get-state p position)))

;;used to be isDwell
;;this is inconsistent.  Need to alter...
(defn dwell-at? [p position] (= (get-state p position) Dwelling))



;Note ---->
;Deployability is no longer just a function of Position....it's also a function of
;cycle time....this is based on an outdated assumption that a units availability would be associated with
;its Position.  We could validate this assumption by creating more Positions....this is simple enough, although
;it creates additional complexity.  On the other hand, when a unit changes Positions, we can check to see if
;it will become available during the course of waiting at the next Position.  If so, we have the unit
;behavior request an update for the unit at the specified avaiable time.
;TOM Change 21 Mar 2011 -> I am going to adopt the convention that any Position with the substring "_Deployable"
;is a valid deployment point.  We can use this to perform constant-time lookup to determine if a policy
;allows a unit to deploy from a Position.
;Consequently, deployability is driven ENTIRELY by the parameters for min deployable and max deployable.
;When we assert, via the policy manager, a minimum deployable time and a maximum deployable time, we will
;alter the structure of the Positiongraph to add additional nodes.
;    Essentially, we add 2 new nodes -> StartDeployable and StopDeployable
;We can add ANY Number of these nodes, by subverting existing nodes in the graph.  This is great, because it
;allows us to potentially have highly variable, and dynamic policies.  Rather than a general window of deployable,
;we could have multiple pockets of deployability.  Who knows.
;To determine if a Position is deployable, we just do a DFS from the policystart to the currentPosition.
;We then parse the resulting path (from the last Position), to determine the deployable state.  Simple.

;Note -----> from insertModifier
; 'algorithm for transforming a graph into a graph with modified nodes existing after tstart
; 'This basically communicates meta-information about some temporally-dependent change in the policy.
; 'This allows us to schedule things like deployable windows, etc. formally in the structure of the graph.
; 'Makes it visual as well.  Plus, we can add multiple modifications to the policy.  This is pretty useful.
; 'Right now, it's primarily used to communicate the existence of the deployable window, but the cases can
; 'be drastically expanded.  The operative convention is that Anything delimited by a _ following the
; 'initial Position name is a modification.
; 'find the intersection of edges affected by the change.
; 'GetPosition at cycletime.
;'This is the affected Position.
;'create a new node subverting this Position...
;    'utilize native graph function subert the node with a "Deployable".
;    'new node automatically links to all subsequent nodes (should be 1 edge).
;    'since we're conserving the cost, we update the edge costs for the two nodes...

;;more general, stick this into cljgraph...
(defn update-node [g label f]
  (graph/set-node g label (f (graph/get-node g label))))
;;also should be in cljgraph...should define path reducers too...
(defn update-nodes [g nodes f] 
  (reduce (fn [acc nd] (update-node acc nd f)) g nodes))

;;Adds or removes tg from s, depending on s's presence 
(defn toggle-tag [s tg] 
  (if (set? s)      
    (if (contains? s tg) (disj s tg) (conj s tg))
    #{tg}))

(defn insert-modifier 
  ([policy cycletime {:keys [name weight] :or {name :modified weight 0}}]
     (let [x     (get-position policy cycletime)
           nxt   (next-position policy x)      
           pg    (get-position-graph policy)
           tprev (-> (graph/depth-first-search pg (start-state policy) x {:weightf graph/arc-weight})
                     (get :distance)
                     (get x))
           offset (- cycletime tprev)
           dnxt   (- (graph/arc-weight pg x nxt) offset)]                          
       (set-position-graph policy
            (-> pg 
                (graph/disj-arc x nxt)
                (graph/add-arcs [[x name offset]
                                  [name [x name] weight]
                                  [[x name] nxt dnxt]])))))
  ([policy cycletime] (insert-modifier policy cycletime {})))

(defn drop-ends [xs] (drop 1 (butlast xs)))

(defn mark-deployable-region 
  "Adds modifiers to each node in the position graph of a policy between :deployable and :non-deployable nodes, 
   indicating the position is an eligible deployable state."
  [policy] 
  (let [pg (get-position-graph policy)]
    (if-let [path (graph/first-path (graph/depth-first-search pg :deployable :non-deployable))]    
      (->> (update-nodes pg (drop 1 (butlast path)) #(toggle-tag % :deployable))
           (set-position-graph policy))
      (throw (Exception. (str "No deployable range found between in " policy))))))

(defprotocol IUnitBehavior
  (behavior-name [b] "Return the name of the behavior...duh.")
  (init-behavior [b state] "Used for stateful initializaion, may tank this one.")
  (update [b deltat unit] "Update unit with behavior b, given a time delta.")
  (change-state [b unit to-state deltat duration following-state] 
      "Return the result of changing the unit's finite state machine to a 
       new state using behavior b."))
    


;;Legacy...
    

;This is a pretty huge protocol to use....we can probably dispense with 
;most of it if we promise to use maps....
;Assume properties are just map calls....
(comment 
(defprotocol IRotationPolicy 
  (policy-name [p] "Name of policy p.")
  (policy-cyclelength [p] "Prescribed cycle length of policy p.")
  (policy-mindwell [p] 
     "Minimum required Time spent dwelling prior to deploying.")
  (policy-maxdwell [p] 
     "Maximum allowed Time spent dwelling, will return to CycleStart")
  (policy-maxbog [p] 
     "Maximum contiguous amount of time a unit can spend in a bog state.") 
  (policy-MaxMOB [p] 
     "Maximum contiguous amount of time a unit can spend in a mobilized state.")
  (policy-startdeployable [p] "Earliest time an entity can deploy in the policy") 
  (policy-stopdeployable [p] "Latest time an entity can deploy in the policy")
  (policy-PositionGraph [p] 
    "DIRECTED graph, where edges are Position transitions, weights are 
     transition time, Node Data describes unit's state (bogging/dwelling, etc.)")
  (policy-startstate [p] "Entry position for the start of a policy cycle" ) 
  (policy-endstate [p] "Penultimate node on a policy cycle." ) 
  (policy-overlap [p] 
    "The amount of time a unit will spend in overlap when deployed.")
  (policy-subscribers [p] "Set of entities that subscribe to this policy.  
                           Deprecated.")
  (get-active-policy [p] "Returns the activepolicy of the current policy.  
                          Important for composite policies. 
                          Atomic policies don't get anything.")
  (atomic-name [p] 
   "return the name of the active atomic policy if it's a composite policy.")
  (get-policy [p period] "Provide a mapping of period to policy.")
  (on-period-change [p period] 
    "handle transitioning of periods...probably move this.")
  (get-policy-type [p] "Return an type indicator for the policy.")
  (subscribe-policy [p x] "units subscribe to policies, so when we change, 
                           we can notify only the affected units.") 
  (add-position [p position state] "Assoc a policy position/state")
  (add-route [p start dest weight] 
   "Routes are edges in the graph. They represent some sequence that a unit 
    following this policy must pass through.  Description is used to add more 
    information about the nature of the transfer (is it definite or 
    conditional?) Generally, we should have one definite state transfer, and any 
    number of conditional/other transfers (arcs) leading from a node. Transfers 
    can even be probablistic, carrying information on the chance of an edge 
    being chosen.")
  (get-state [p position] 
     "Get the state of the current Position (generally bogging,dwelling, etc., 
      but potentially anything). State information is stored at the Position's 
      node")
  (next-position [p position] 
   "Get the next Position as a result of current Position and policy structure.
    If the Position has no arcs leading out (it's an island), then it is 
    considered an absorbing state and will return itself as the next Position.
    Rule of Thumb I instantiated is that if a Position does not exist in the
    Position nodes, then it must be coming from Deployed (demands are not 
    explicitly registered).")
  (previous-position [p position] 
     "Get the previous position in the policy sequence.")
  (transfer-time [p start dest] 
     "Returns the weight (assumably time) between two policy positions.")
  (get-position [p cycletime] 
      "Given a cycletime, what is the resulting Position? Doing a simple linear 
       search, this is OK for small N. Our cycles shouldn't be that big ... will 
       have much less than 20, usually 6 Positions tops.")
  (get-cycletime [p position] 
     "Given a position, returns the cycletime that intersects with the beginning 
      of the position.")
  (deployable? [p cycletime] 
      "Returns true if the cycletime corresponds to a deployable state in the
       policy.")
  (dwell?  [p position] 
       "Returns true if the policy position corresponds to a dwell state.")
  (deployable? [p position] 
     "Returns true if a unit can deploy from the policy position.")
  (insert-modifier [p cycletime modifier] 
     "Inserts a modifier node at the cycletime, which effectively creates a 
      modified pocket of policy space.  Modifiers were typically used to splice
      in deployable and non-deployable windows....may move this to a general 
      library.") 
  (set-deployable-start [p cycletime] 
     "Insert a node that defines the start point in the lifecycle for 
      deployment.  By default, if there is no Deployable node in the policy, 
      then this policy will not deploy units.")
  (set-deployable-stop [p cycletime] 
   "insert a node that defines the stop point in the lifecycle for deployment.")
  (set-deployable [p tstart tfinal]
    "sets the deployable window.  I think this may be redundant. ")
  (get-bogbudget [p] 
     "The BOG Budget set by the policy.  In most policies, this will default to 
      MAXBog if the policy has 0 for a BOGBudget.  This allows us to separate 
      the concept of total time allowed to bog in an entire cycle, along with
      most time PER deployment"))                 
(defn add-positions
  "Nodes are input as a sequence of key value pairs. For our purposes, this will 
   be PositionName, State(bog/dwell/bogeployable/dwelldeployable/etc.)"  
  [p xs]
  (reduce (fn [acc [k v]] (add-position acc k v)) p (seq xs))) 
)
