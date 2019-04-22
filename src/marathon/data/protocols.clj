;This is a conglomeration of Marathon protocols, derived from what used to 
;be VBA interface classes.  I'll probably be trimming these down significantly, 
;since much of the functionality can be replaced with simpler functions on the 
;core data structures....especially if the core data structures are just 
;maps or records (which support a map API).
(ns marathon.data.protocols
  (:require [spork.cljgraph [core :as graph]]
            [spork.util.metaprogramming :refer [keyvals->constants]]))

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
   :EquivSymbol "="
   ;;SRM constants...
   :PB_C3  "PB_C3"
   :PB_C4  "PB_C4"
   :PT_C4  "PT_C4"
   :PL_C4  "PL_C4"
   :R_C1   "R_C1"
   :R_C2   "R_C2"
   :MP_DA_C1   "MP_DA_C1"
   :MP_NDA_C3  "MP_NDA_C3"
   :MA_DA_C1   "MA_DA_C1"
   :MA_DA_C2   "MA_DA_C2"
   :MA_NDA_C3  "MA_NDA_C3"
   :MD_DA_C1   "MD_DA_C1"
   :MD_DA_C2   "MD_DA_C2"
   :MD_NDA_C3  "MD_NDA_C3"

   })

(keyvals->constants policyconstants) ;make the constants first class symbols.
;inherited from substitution rules, may be vestigial.
(keyvals->constants {:Equivalence :Equivalence :Substitution :Substitution})


;;look into replacing this with a universal constant, or upperbound
;;for longs
(def ^:constant +inf+ 9999999)

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

;;This is a compatibility hack at the moment, I'll probably rip this
;;out.
(defprotocol IPeriodicPolicy
  (change-period [p period]))

;;changed to extends? for performance.
(defn on-period-change [p period]
  (if (extends?  IPeriodicPolicy (class p))
    (change-period p period)
    p))

;;Helper function to allow us to push maps into policies as positions.
;;Basically sets the state associated with a policy position.
(defn add-positions [p xs]
  (if (map? xs)
    (reduce-kv (fn [acc pos state]
                 (add-position acc pos (cond (set? state) state
                                             (coll? state) (set state)
                                             :else #{state}))) 
                 p xs)
      (reduce (fn [acc pos] (add-position acc pos {})))))

(defn add-routes [p rs]
  (reduce (fn [acc [from to t]]
            (add-route acc from to t))
          p rs))

(definline between? [t l r]
  `(and (>= ~t ~l) (<= ~t ~r)))

;;Determining if we want to have this guy defined....
;; (defn state-at [p position]
;;   (if-let [res (get-state p position)]
;;     res
;;     (throw (Exception. (str [:position position :unknown-in-policy (:name p)])))))
(defn state-at [p position]
  (or (get-state p position) position))

(def modifiers #{:deployable :not-deployable})
(defn modifier? [pos] (contains? modifiers pos))

;;Might want to cache this stuff....
;;Include a facility for defining deployable states..
(defn deployable-state? [s] 
  (when s (:deployable s)))

;;used to be isDeployable
(defn deployable-by?      [p cycletime]
  (between? cycletime (start-deployable p) (stop-deployable p)))

;;used to be deployable
;;A position is deployable in a policy if the node data associated 
;;with the position is deployable.  We just store deployability in 
;;each node.
(defn  deployable-at?       [p position]  
  (deployable-state? (state-at p position)))

;;used to be isDwell
;;this is inconsistent.  Need to alter...
(defn dwell-at? [p position]
  (let [s (state-at p position)]
    (or (= s Dwelling)
        (= s :dwelling)
        (:dwelling s))))

;;#Optimize
;;this may be bogging us down....
(defmacro find-position [& body]
  `(get-position ~@body))
;;memoization was not helping us here, at least not
;;this version.
;(def find-position (memoize get-position))

;;Returns the deployable window for the policy [start stop]
(defn deployable-window [policy]
  [(start-deployable policy)  (stop-deployable policy)])

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
  (let [nd  (graph/get-node g label)
        res (f nd)
        ;_ (println [:updating label :from nd :to res])
        ]
    (graph/set-node g label res)))

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
     (let [x     (find-position policy    cycletime)
           nxt   (next-position policy    x)      
           pg    (get-position-graph policy)
           tprev (-> (graph/depth-first-search pg (start-state policy) x {:weightf graph/arc-weight})
                     (get :distance)
                     (get x))
           offset  (- cycletime tprev)
           dnxt    (- (graph/arc-weight pg x nxt) offset)
           xdata   (graph/get-node pg x)
           nxtdata (graph/get-node pg nxt)
           newnode [x name]
           newstate (if (set? xdata)
                      (conj xdata name)
                      #{name xdata})
          ; _ (println [newnode newstate])
           ]                          
       (set-position-graph policy
            (-> pg 
                (graph/disj-arc x nxt)
                (graph/conj-node name #{name})
                (graph/conj-node newnode  newstate)
                (graph/add-arcs [[x name offset]
                                 [name newnode weight]
                                 [newnode nxt dnxt]])))))
  ([policy cycletime] (insert-modifier policy cycletime {})))

(defn drop-ends [xs] (drop 1 (butlast xs)))

(defn compute-cycle-length [p]
  (let [pg (get-position-graph p)]
    (reduce + 
            (let [[hd & tl] (first (spork.cljgraph.core/cyclical-components pg))
                  full (into [(last tl) hd] tl)] 
              (map (fn [[from to]]  
                     (graph/arc-weight pg from to)) (partition 2 1 full))))))

(defn mark-deployable-region 
  "Adds modifiers to each node in the position graph of a policy between :deployable and :non-deployable nodes, 
   indicating the position is an eligible deployable state."
  [policy] 
  (let [pg (get-position-graph policy)]
    (if-let [path (graph/first-path (graph/depth-first-search pg :deployable :non-deployable))]    
      (->> (update-nodes pg (drop 1 (butlast path)) (fn [nd]  
                                                       (conj (if (coll? nd) (set nd) #{nd}) :deployable)))
           (set-position-graph policy))
      (throw (Exception. (str "No deployable range found between in " policy))))))

(defn leads-to-start? [p position]
  (-> (get-position-graph p)
      (graph/depth-first-search position (start-state p))
      (graph/path?)))       

(defprotocol IUnitBehavior
  (behavior-name [b] "Return the name of the behavior...duh.")
  (init-behavior [b state] "Used for stateful initializaion, may tank this one.")
  (update [b deltat unit] "Update unit with behavior b, given a time delta.")
  (change-state [b unit to-state deltat duration following-state] 
      "Return the result of changing the unit's finite state machine to a 
       new state using behavior b."))

(defn policy? [obj]
  (extends? IRotationPolicy (class obj)))

(defn expected-dwell [p]
  (or (-> p get-active-policy meta :expected-dwell)
      (max-dwell p)))
