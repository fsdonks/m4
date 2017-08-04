;;The entity store for MARATHON and some supplemental
;;entity definitions and aux functions.
(ns marathon.data.store
   (:require 
    [spork.entitysystem.store :refer :all]
    [spork.sim.core :as sim]
    [spork.util.tags :as tag]
    [marathon.data    [period :as p]]
    [spork.ai.machine :as fsm]
    ))

;;Cleanup - WORK IN PROGRESS
;;==========================

;;The original implementation of the entitystore
;;originated in a parallel, experimental build of a lightweight
;;simulation.  Much of the functionality that now resides in
;;spork.entitystore and marathon.ces.core was colocated with this
;;namespace (hence the entity definitions, event "notifications"
;;and other auxillary functions).

;;As I've migrated generic things out into spork, and integrated
;;the basic entity store into the overarching simulation infrastructure,
;;we're left with relatively little here.  Technically, we don't need
;;special entitystore implementation, we could use the barebones from
;;spork.entitysystem.store in theory.

;;We're also still using legacy entity definitions in other "data"
;;namespaces.  Since the entity store will happily accept records
;;as entities (infering keys map to components) this is no problem
;;and allows a smooth migration to useing the entity store API
;;for operating on our simstate.

;;I'll leave the entity definitions in place for now, although they
;;are not technically in use.

;;Additionally pruning to follow....

;;The only functions we "actually" use here are ->store to
;;create the entity store....


;;Pending DEPRECATION
;;===================
;;Originally a simple API to process the act of handling
;;"events" in an entity store-based simulation.  Supplanted
;;by the functionality in spork.sim.simcontext.
(defprotocol INotification
  (notify! [obj e msg]
           [obj m])) 

;;Pending DEPRECATION
;;===================

;;provide a constant set of components that
;;will be cleared each day.
;;Basically, we'll perform all of our processing
;;and allocations, then drop anything that's considered
;;ephemeral.  Typically, we'll have multiple changes
;;occuring over the same time instant, but spanning
;;multiple event or update-instants.  We only care
;;about the initial and the final value.
(def delta-components
  #{:position-delta :movement-delta :state-delta})
;;temporary hack to abstract out entity containers...

;;Pending DEPRECATION
;;===================
;;defines a unit-entity selector.  This is a little
;;brittle, which is why I'm pushing it into a redefinable
;;function.  The stats stuff is dependent on the entity
;;records having these fields.
(defn unit-entities [obj]
  (all-entities obj [:state :component :icon :color :position]))


;;Description
;;===========
;;simstate is a consolidation of all the simulation state required for Marathon 
;;to do its thing.  Each of these bits used to be part of a hierarchical object 
;;model with parent-child relationships, where each child manager could get at 
;;the state for another child via its parent, the simulation Engine.  This worked
;;okay in the beginning, and fit with some naive object oriented programming 
;;notions, but ended up leading to coupling and some other phenomena. As a result
;;I've re-organized the code base to conform to a more functional-programming 
;;paradigm: specifically, data is maintained separate from functionality, rather 
;;than classic OOP where data is encapsulated and bundled with 
;;methods/properties.  The result is a simplification of the data model, as well
;;as functions that can produce and consume bits of data necessary for the 
;;simulation.  This simstate object really just gathers all the data in one 
;;place, so that functions that need to access multiple components of data
;;simultaneously CAN.  Since it's a record, and most of the elements are also 
;;records, we get the benefit of using clojure's associative map functions and 
;;sequence libraries to access and modify our state, rather than having to deal
;;with a special set of one-off functions.

;;Since simstate implements entity store, it can be used as the :state key of
;;a simcontext, and simcontext will forward all entity-store-related implementation
;;calls to it.  This gives us a fairly flexible way to manage our simulation context,
;;specifically the entity state: use the spork.entitysystem.store API to query
;;and update the simcontext directly.  marathon.ces.core is full of idioms that
;;do this very thing, although many namespaces will use the spork.entitysystem.store
;;directly.

(defrecord simstate [store width height notifier]
  IEntityStore
  (add-entry [db id domain data] 
    (simstate. (add-entry store id domain data)
               width height notifier))
  (drop-entry [db id domain] 
    (simstate. (drop-entry store id domain) width height notifier)) 
  (get-entry      [db id domain] (get-entry store id domain))
  (entities       [db]     (entities store))
  (domains        [db]     (domains  store))
  (domains-of     [db id]  (domains-of     store id))
  (components-of  [db id]  (components-of  store id))
  (get-entity     [db id]  (get-entity     store id))
  (conj-entity    [db id components] 
    (simstate. (conj-entity store id components) width height notifier))
  IColumnStore
  (swap-domain [db c x]
    (simstate. (swap-domain store c x)
               width height notifier))
  INotification
  (notify! [obj e msg]
    (if (fn? notifier)
      (notifier e msg)
      (notify! notifier e msg)))
  (notify! [obj m]
     (if (fn? notifier) (notifier m)
         (notify! notifier m))))


;;Currently Out of Use / Possible Migration / Documention
;;=======================================================


;;__Entity Definitions__
(defentity physical-entity
  "A simple entity with 2D physics information, position and velocity. "
  [id {:keys [position velocity]
       :or {position [0 0]
            velocity [0 0]}}]
  {:components [:position position
                :velocity velocity
                :physical true]})

;;update information tracking the last time the entity was examined.
(defentity interactive-entity
  "An entity that can participate in messaging and behavior, keeps track of time."
  [id {:keys [messages behavior statedata last-update t] 
       :or {messages nil
            behavior :default
            statedata fsm/spawning-data}}]
  {:components [:behavior    behavior
                :messages    messages
                :interactive true
                :statedata   statedata
                :spawntime   -1]})

;;Note: nil initial values don't work well...
(defentity unit
  "Defines a specification for entities that correspond to force structure."
  [id name type component policy state icon label position velocity color home
   & {:keys [speed location behavior unit-index] :or {speed 8}}]
  {:specs [(physical-entity id {:position position
                                :velocity velocity})
           (interactive-entity id {:behavior (or behavior :default)})]
   :components
   [:name        name      ;unit entity's unique name. corresponds to a UIC 
    :type        type
    :src         type      ;unit entity's type, or capability it can supply.
    :component   component ;unit entity's membership in supply.
    :policy      policy    ;the policy the entity is currently following.
    :policystack []       ;a stack of any pending policy changes.
    :state       state
    :icon        icon
    :label       label
    :color       color
    :unit-entity true
    :speed       speed
    :home        home
    :deployable  false  ;unit's deployable status.
    :location    (or location home) ;physical location of the unit.
    :cycletime    0    ;the unit's current coordinate in lifecycle space.
    :followoncode 0 ;description of the categories this unit serve as a followon to.
    :positionpolicy :spawn
    :currentcycle nil ;the current cycle data structure for the unit.
    :cycles   []
    :oi-title "no-description" ;the description of the unit.
    :locationhistory [] ;list of all the locations visited.  ;:dwell-time-when-deployed nil
    :unit-index (or unit-index nil)
    ]
   })

(defentity demand
  "Defines a specification for entities that correspond to force structure demands."
  [id name type priority startday duration overlap category source-first quantity title
   vignette operation demandgroup 
   & {:keys [location behavior fills theater
             BOG StartState EndState MissionLength]}]
  {
   :components
   [:name name ;unique name associated with the demand entity.
    :src  type ;demand-type, or other identifier of the capability demanded.
    :priority priority ;numerical value representing the relative fill priority.
    :startday startday ;the day upon which the demand is activated and requiring fill.
    :duration duration ;the total time the demand is activated.
    :overlap overlap  ;the demand-specific overlap requirement, if any
    :category (or category :rotational) ;descriptor for deployed unit behavior over-rides.
    :source-first (or source-first :uniform)  ;descriptor for supply preference. 
    :quantity (or quantity 0) ;the total amount of entities required to fill the demand.
    :title  title  ;formerly OITitle.  Long-form description of the src capability.
    :vignette vignette ;Descriptor of the force list that generated this demand entity.
    :operation operation ;Fine-grained, unique description of the demand.
    :demandgroup demandgroup ;Ket that associates a demand with other linked demands.  
    :fills (or fills {}) ;an ordered collection of all the fills recieved over time.                   
    :units-assigned {} ;map of the units currently associated with the demand.
    :units-overlapping {};map of the units currently associated with this demand, 
                           ;that are not actively contributing toward filling the 
                                        ;demand, due to a relief-in-place state.
    :location   (or location name) ;;physical location of the demand.
    :theater     theater ;;newly added for SRM. Describes the geolocal theater of the demand
    :BOG           BOG  ;;newly added for SRM. Describes the geolocal theater of the demand
    :StartState    StartState ;;Defines a possible starting state for the demand, if any.
    :EndState      EndState ;;Defines the implication of leaving the demand on any associated entity.
    :MissionLength MissionLength ;;Defines the length of duration of a local assignment to the demand.
    ]})

;;Actively In Use
;;===============

;;not sure how much of this needs to stick around...
;;Since entities are dispersed now, we may not need these guys
;;any longer.
;;It'd be nice to define some queries/views too..
;;Specifically, we can provide an associated type that knows how to
;;query an entity store; we can use said view against it and get
;;a resulting seq of entities.

;;need a changed component.
(defentity supplystore
  "defines a singleton container for supply information"
  [id]
  {:components
   [:name :SupplyStore
    :srcs-in-scope {} ;Set of unique SRCs in scope.
    :deployable-buckets {} ;{category entity}, indicates entities that can fill demand.
;   :unitmap        ;{entity-name unitdata}, map of unique unit entities.
;   :unit-behaviors ;map of named unit behaviors.  may move this out...
;   :unit-updates ;set of eventful unit-days....might be able to handle this outside.
                ;this was listed as a tag structure earlier...not certain..
    :tags tag/empty-tags;set of supply tags...should move to a global tag-store.
    :has-ghosts ;boolean flag to determine if the supply can generate ghosts..might change.
    :follow-ons  {}]})

(defn get-supplystore [ces]  (gete ces :SupplyStore))

(defentity demandstore
  "Defines a singleton container for demand information"
  [id]
  {:components
   [:name :DemandStore 
    ;:demandmap         {}
    :infeasible-demands {} 
    :unfilledq          nil
    :activations        {}
    :deactivations      {}  
    :activedemands      {}  ;possibly replace with active component
    :eligbledemands     {}  ;forgot what this is...
    :changed            {}  ;indicates demands that changed, we can handle this better.
    :demandtraffic      nil ;supress demand traffic, maybe old
    :tryfill            true ;no idea...
    :loginfeasibles     true ;logging info
    :tags               (tag/add-tag tag/empty-tags "Sinks")
    :fillables          nil ;;forgot what this is for
    :verbose            nil ;logging information...
    :tlastdeactivation 0]})

(defn get-demandstore [ces] (gete ces :DemandStore))

(defentity policystore
  "Defines a singleton container for policy information,
   periods, and policy schedules."
  [id]
  {:components
   [:name :PolicyStore 
    :positions #{} ;set of all known positions.
    :locations #{}  ;set of all known locations, superset of positions.
    :periods  {"Initialization" p/+default-period+} ;Set of simulation periods...probably need to re-think this guy.
    :policies {} ;Kvp mapping of policy names to policy data
    :policytraffic       false   
    :activeperiod p/+default-period+ ;the current period 
    :periodchanges {}  ;the set of scheduled period changes....re-think this.
    :subscriptions {}  ;map of policy->client
    :composites    {}  ;map of composite policies
    ;schedules    
    ]})

(def empty-policystore (policystore :PolicyStore))

(defn get-policystore [ces] (gete ces :PolicyStore))

;;Container to store all the data associated with matching supply to demand, 
;;namely substitution rules, scoping (both in and out of scope) for the current
;;run, and any other associated data.
(defentity fillstore
  "Defines a singleton container for fill information"
  [id]
  {:components
   [:name         :FillStore 
    :fillgraph    nil    
    :fillmap      nil
    :fillfunction nil
    :fills        {} 
    :rendergraphs false 
    :outofscope   {} 
    :allgraphs    
    :rawfillgraph  nil
    ]})

(defn make-fillstore [& {:keys [fillgraph fillmap rawfillgraph]}]
  (merge (fillstore :FillStore)
         {:fillgraph fillgraph
          :fillmap fillmap
          :rawfillgraph rawfillgraph}))

(defentity parameters
  "Defines a entity with canonical simulation parameters for components."
  [id]
  {:components
   [:name :parameters 
    :SRCs-In-Scope     {} ;set of srcs that can be used/filled.
    :SRCs-Out-Of-Scope {} ;set of srcs that cannot be filled.
   ;imported parameters from timestep_engine, to be deprecated.   
   :pause       false  ;indicates if the simulation is currently paused, suspending simulation.
   :time-start  nil    ;Wall-clock Start time of the simulation.  
   :time-finish nil    ;Wall-clock Stop time of the simulation.
   :maximum-traffic true ;Flag to enable a debugging mode, with maximum event traffic.  
                   ;Slower and lots of I/O.
   :interactive  true ;Indicate the presence of a linked GUI form.  
                      ;Cedes control to the Form.  Maybe deprecated.
   :no-io false ;Forces simulation to try to suppress its I/O, particularly in output 
                ;metrics and event history.
   :moderate-io false ;only record the lightweight stuff, ala summaries, cyclerecords, deployments
               ;sandtrends are dumped out to csv.
   :no-supply-warning true ;Ignore lack of supply warnings when set.  
                     ;Necessary for requirements analysis.
   :no-demand-warning true ;Ignore lack of demand warnings when set.  
                     ;Necessary for supply-only analysis.
   :earlyscoping true ;Flag that tells the preprocessor to try to eliminate 
                       ;unusable data early on.
   :truncate-time true    ;used for requirements analysis.  Allows us to tell 
                          ;Marathon to stop the simulation AS SOON as the last demand 
                          ;has ended.  If there are no pending demand activations
                          ;we assume that we can stop.  This assumption holds for 
                          ;requirements analysis only....    
   :found-truncation false]})

(defn get-parameters [ces] (gete ces :parameters))

;;__Transient Data__
;;We define entities with a transient component as being ephemeral or mutable...
;;For things like logging, or other activities, we'd like to retain
;;an  append-only event history.  It's easy to do this if we have
;;a component tagging the mutable structure, so that we may clear it between
;;frames.  For instance, we may have a system whose job is to traverse
;;transient components and finalize them.
;;So...for entities that have a transient and a finalize component,
;;we can process them in a controlled manner.  This makes it easy
;;to use different strategies for retaining things like log data,
;;or events, or other things.

;;__Location Definitions__
;;Another unique entity that maintains data (primarily for lookup), that maps locations to
;;coordinates (in some coordinate system)
(defentity locations [id & {:keys [location-map]
                            :or   {location-map {:reset [0 0]}}}]
  {:components [:name :locations
                :location-map location-map]})

;;__Location Operations__
(defn add-location [ces locname coords]
  (updatee ces :locations :location-map assoc locname coords))

;;generic location-based operations.
(defn find-loc [ces loc]
  (if-let [res (get (gete ces :locations :location-map) loc)]
    res
    (throw (Exception. (str "Location not found: " loc)))))

;;Only Function Currently In Active Use For MARATHON
;;==================================================
;;Note: we do call on the entity definitions for the various stores.


;;REFACTOR: THe old width,height, etc. are no longer necessary.
;;We aren't really using simstate like we were before, even
;;notify is OBE.  TODO: Revert to using an empty store,
;;no need to wrap a specific simstate type....

;;The width/height is 1095, wonder if that's screwing up our sizing.
;;__Entity Store Constructor__
(defn ->store
  "Given a set of initial entities (currently units), creates and initializes the 
   default entitystore.  width and height determine the rendering canvas and the 
   size of things like the entity board."
  [& {:keys [width height notify init-store]
      :or {width 1095 height 730
           notify (fn [e msg] (println [:notification e msg]))
           init-store emptystore}}]
;  (let [emap    (reduce (fn [m e] (assoc m (:name e) e)) {} es)
;        trails  (->rec '() width height)]
    (-> (->simstate init-store width height notify)
       ; (add-entities es)
        (add-entity  (parameters   :parameters))
        (add-entity  (supplystore  :SupplyStore))
        (add-entity  (demandstore  :DemandStore))
        (add-entity  (policystore  :PolicyStore))
        (add-entity  (locations    :locations))
        (add-entity  (fillstore    :FillStore))
;       (add-entity  (game-board     :board (keys emap) width height trails))        
;       (add-entity  (animated-board :animated-board))
;       (add-entity  (animated-map   :animated-map))
;       (initialize-nodes)
;       (initialize-map)
        ;)
        ))


;;PENDING DEPRECATION
;;===================
;;WE should decouple this from the original width/height convenience.  It's not
;;how we render things today.
    
(defn ents [brd] (val (get-entry brd  :board :entities)))
(defn uics [brd] (map #(get-entity brd %) (ents brd)))

;;get a sequence of entities...this should be using lightweight containers...
;;basically select-all..
(defn eseq [brd] (map #(entity-at  brd %) (ents brd)))
;;__Entity Operations API__
;;we may define a high-level interface for getters and setters...
;;to include notifications...

;;Alter the [x y] position of the entity.
(defn set-entity-position [brd nm x y] (assoce brd nm :position [x y]))
;;Return the [x y] position of the entity.
(defn get-position        [brd nm]     (gete brd  nm :position))
;;Alter the shape associated with the entity.
(defn set-entity-shape    [brd nm shp] (assoce brd  nm :shape shp))
;;Alter the entity's state.
(defn set-entity-state    [brd e state]
  (-> brd
      (assoce  e :state state)
      (notify! :state-change [e state])))

;;we should abstract this out to something a bit more useful.
(defn set-entity-color    [brd e clr]
  (->  brd
       (assoce  e :color clr)
       (notify!   :color-change [e clr])))

(defn get-trails          [brd] (gete brd :board :trails))

;;Movement is a primitive. We simply add the "current" velocity to the
;;entity's position.
(defn move-entity! [brd nm x y dx dy]
  (assoce brd nm :position
          [(+ x dx)
           (+ y dy)]))

;;this comes later...
(declare nodes!)
;;debug helper.
(comment 
(defn node-info [lbl e x y x2 y2]
  (when @shared/debug
    (println [lbl e
              :from [x y]
              :to [x2 y2]
              :displacement [(- x2 x) (- y2 y)]
              :node (let [bnd (.getFullBounds ^org.piccolo2d.PNode (get (nodes!) e))]
                      [(.getX bnd) (.getY bnd)])
              ]))
  )
)

;;These are fundamental primitives for handling visual aspects of
;;the entity.

;;note, this combines movement (direction change) with displacement...
;;computes a displacement based on the entity's current position.
(defn move-to!
  ([ces e pos loc]  
   (let [[x  y ] pos 
         [x2 y2] (cond (vector? loc)   loc
                       (keyword? loc) (find-loc ces loc)
                       :else (throw (Exception. (str "unmatched condition " loc))))
         ;_      (node-info :move-to e x y x2 y2)
         ]
     (mergee ces e {:position     [x2 y2]
                    :displacement [(- x2 x) (- y2 y)]})))
  ([ces e loc] (move-to! ces e (gete ces e :position) loc)))

;;We may want to rephrase our movement so that everything is in terms of primitive
;;instructions, specifically, move-to.
(defn deploy-entity! [brd e state x y & {:keys [destination location]}]
  (let [;_ (node-info :deploy-ent e x y x y)
        destination (or destination  :default) ;(board/random-region))
        ]
    (-> brd
        (mergee  e {:state           :deploying
                    :location        destination
                    :velocity        [0  1]
                    :displacement    [0 -1] ;temporary debugging....                   
                    })        
        (notify!    {:state-change    [e state]
                     :location-change [e location destination]
                     :deployed       {:e e
                                      :component (gete brd e :component)
                                      :state state :x x  :y y}}))))



;;Interstingly, we "could" take notifications to just queue messages up, ephemerally,
;;and then distribute them onto the event bus in between...
;;Separates the line between pure and impure, if we want purity, we can simulate channels
;;(kind of an ass pain though).
;;If we allow impurity, we can maintain a consistent state, and allow systems to run
;;concurrently.  We may need to synch though...

;;can we combine impure action with pure storage? 

;;when entities go back to reset (or prepare, depending on policy)
(defn reset-entity!  [brd {:keys [name state position component home location] :as ent}]
  (let [[x y] position]
    (-> brd
        (mergee name {;;encoding multiple velocities indicates instantaneous change in
                      ;;velo.  Any components beyond the first indicate previous velocities.
                      :velocity  (case component
                                   :AC [1   0]
                                   [0.2 0])                      
                      :state          :dwelling
                      :location   home
                      })
        (move-to! name  position :reset) ;;abstracted movement.  Adds a displacement component
        (notify!  {:reset {:e name :state state :x x :y y}
                   :state-change [name state]                   
                   :location-change [name location home]
                   }))))



;;Entity Board and stuff (currently deferred until quilsample code is generalized; trivial
(comment 
;;the game-board is not a physical entity, but we can store it in the ECS as a
;;unique entity and grab it pretty easily.  There may be some overhead, but i'm not too worried about it.
(defentity game-board [id entities width height trails]
  {:components [:entities entities
                :width width
                :height height
                :trails trails]})

;;__Entity Rendering__
(defentity animated-board
  "Defines a container for the animated graphical nodes and the rendering system for all 
   the pieces on the board."
  [id & {:keys [nodes]
         :or   {nodes board/empty-board}}]
  {:components [:nodes nodes]})


(defn get-animated-board [ces]
  (gete ces :animated-board :nodes))

;;This reuses the animated-board concept.
;;Except we'll likely need to scale the icons down
;;a bit...
(defentity animated-map
  "Defines a container for the animated graphical nodes and the rendering system for all 
   the pieces on the world map."
  [id & {:keys [nodes]
         }]
  {:components [:nodes (or nodes
                           (board/empty-map))]})

(defn get-animated-map [ces] (gete ces :animated-map :nodes))

;;there's a problem atm...
;;we're not computing displacement every frame.

;;we have a more broadly-based rendering system.
;;anything that is visible is updated and rendered.
;;perhaps we can break it up into dynamic objects,
;;objects which have a displacement computed each frame,
;;and static objects, which have no displacement (we just re-render them).
(def ^:constant +zero-2d+ [0.0 0.0])
(defn render-nodes! [ces]
    (->> [:name :velocity :position :node]
         (only-entities ces)         
         (reduce-kv (fn [acc id {:keys [name velocity position node]}]
                      (let [[^double dx ^double dy]       velocity
                            [x y]                         position
                            [^double xoff ^double yoff]  (or (gete acc name :displacement)
                                                             +zero-2d+)
                            endx (+  dx xoff)
                            endy (+  dy yoff) 
                            ]
                        (do (board/shift-node! node endx endy)                        
                            acc)))
                    ces)))

;;just define a system that listens for changes.
;;same with the board.

;;Convenience wrappers.


;;this lets us delegate to the board stored in the simstate.
;;when we add nodes, we also update the simstate by adding
;;a node to the entity's components (the actual mutable
;;piccolo2d node that's attached to the gameboard).  We also
;;enable the ces to be seen as a single layer (via the gameboard).
(extend-type quilsample.store.simstate 
  board/IBoard
  (add-node  [ab id icon x y]
    (let [brd (board/add-node (get-animated-board ab)
                        id icon x y)
          nd (get (.node-map brd) id)]
      (-> ab
          (assoce id :node nd) ;add the node as a component
          (assoce :animated-board :nodes brd) ;update the new board (not "really" necessary)
          )))
  (drop-node [b id]
    (let [brd (board/drop-node (get-animated-board b) id)
          nd (get (.node-map brd) id)]
      (-> b
          (dissoce id :node nd) ;add the node as a component
          (assoce :animated-board :nodes brd) ;update the new board (not "really" necessary)
          )))
  (get-layer [b] (board/get-layer (get-animated-board b)))
  (move-node [b nd x y] (do (board/move-node (get-animated-board b) nd x y)
                            b))
  picc/IPiccNode
  (as-node   [nd]       (picc/as-node (get-animated-board nd)))
  (add-child [nd child] (do (picc/add-child (get-animated-board nd) child)
                            nd)))
;;__Mutable Observers and Side-Effects__
;;This a little hacky at the moment; we should add support for multiple layers
;;or node targets.  We need to project these positions onto
;;map coordinates.  They're positions in policy space, not
;;physical locations.
(defn initialize-map
  "Associates map node components with each entity, and registers the node components in the 
   root layer of the animated board entity in the entity store."
  ([ces] (initialize-map ces events/event-bus))
  ([ces bus]
   (let [the-map  (get-animated-map ces)
         us-locs  (vec (keys quilsample.maps/uslocs))
         new-map  (->>  [:name :position :state :color :icon :speed :home]
                        (only-entities ces) ;;note we're using reduce.        
                        (reduce (fn [acc {:keys [position name icon color home]}]
                                  (let [home (or home
                                                 (rand-nth us-locs))                                                                                        
                                        token   (board/->token name icon (or color :white))]
                                    (-> acc
                                        (board/add-token name token)
                                        (board/place-node name home))))
                                the-map)                      
                        )
         ;;we setup a system to synchronize the map with changes to the store.
         ;;specifically, we want to listen for entity location changes
         ;;and color changes.  These events are translated into motion
         ;;on the map.  [todo - find a more expressive way to do this.]
         entity-changes
         (entevents/entity-channel :bus bus
           :interests [:location-change :color-change]
           :xform ;filter out location changes that result in zero movement.
           (filter (fn [{:keys [msg-type data] :as msg}]
                     (case msg-type
                       :location-change (let [[_ from to] data]
                                          (not= from to))
                       true))))
                                                            
         ;;having a consistent name ensures we only ever consume a single thread for this.
         _  (sys/thread-pull :map-system entity-changes v
                             (let [{:keys [msg-type data] :as msg} v]                               
                               (case (get msg :msg-type)
                                 :location-change
                                 (let [[e from to] data]
                                   (try (board/send-to the-map e to  :arc? true :duration 20)
                                        (catch Exception e nil)))
                                 :color-change
                                 (let [[e x] data]
                                   (board/color-token the-map e x))                                     
                                 (throw (Exception. (str [:unhandled-msg-type msg 'map-handle]))))))
         ]
     (assoce ces :animated-map :nodes  new-map))))
          
(defn initialize-nodes
  "Associates node components with each entity, and registers the node components in the 
   root layer of the animated board entity in the entity store."
  [ces]
  (->>  [:name :position :state :color :icon]
        (only-entities ces) ;;note we're using reduce.        
        (reduce (fn [acc {:keys [position name icon]}]
                  (let [[x y] position]
                    (board/add-node acc name icon x y)))  ces)))


)

(comment 

;;note: this is somewhat coupled. and it's less than optimal for
;;performance atm.  Ideally, we'd use some variant of the
;;nodecache directly within piccolo, and decouple the
;;rendering from the notification of the trail change.
(defn set-entity-trail
  [brd e x y clr prev-clr]
  (let [brd (if (= prev-clr clr) brd ;no change in color
                ;(->
                 (set-entity-color brd e clr)
                    ;(notify! :color-change [e prev-clr clr])
                 )
        ;)
    ]
    (when (and (pos? y)
               clr @shared/*trail*
               (< (rand) 0.3))
      (let [trail-layer  (get-trails brd)]
        (canvas/push-shape  trail-layer (sketch/fade 0.1
                                              (->colored-ring clr x y)                                              
                                              ))))    
    brd))
)
