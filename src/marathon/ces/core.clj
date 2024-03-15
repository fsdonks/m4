;;marathon.sim.core is a glue library that provides access to a common
;;set of subsystems upon which a more complicated federated simulation
;;infrastructure is implemented.

;;Marathon utilizes a generic simulation context, defined in
;;__sim.simcontext__, which provides primitive access to a generic
;;simulation state - heterogenous chunks of data relevant to different
;;domains of the simulation - and a basic discrete event simulation
;;framework.  Systems are then defined over this shared architecture,
;;and contribute special means to affect a simulation in their
;;particular domain.  At the highest level, a coordinating function,
;;or an engine, orders and composes the systems into a comprehensive
;;state transition function that maps one simulation context to the
;;next.

;;This core namespace started as a dumping ground for shared or
;;duplicate functionality from the original object oriented design.
;;It continues to evolve as the source is reorganized along clearer
;;lines, and serves as a staging ground for general pending tasks and
;;observations.  To that end, the casual reader may skim the rest of
;;this section without losing a great deal of insight.  Even for
;;intrepid readers, the section header __Developer Notes__ may be
;;largely ignored.
(ns marathon.ces.core
  (:require 
            [spork.util [metaprogramming :as util]
                        [general  :as gen]
                        [tags     :as tag]
                        [table    :as tbl]
                        [reducers]
                        [cellular :as cells]
                        [inspection :as inspect]
                        [temporal :as temp]]
            [spork.cljgraph [jungapi :as jung]]
            [spork.sketch :as sketch]                        
            [spork.entitysystem.store :refer :all :exclude
             [entity-name merge-entity entity?] :as store]
            [spork.sim [simcontext :as sim] [core :as simcore]]
            [spork.ai [core        :as ai]
                      [behaviorcontext :as b]
                      [messaging]]             
            [marathon.data [store       :as simstate]
                           [protocols   :as protocols]]
            [clojure.core.reducers     :as r]
            [clojure.pprint :as pprint]))

;;Convenience Imports from spork.sim.core
;;=======================================
;;spork.sim.core serves as a general hub for simulation-related
;;programming.  As such, it provides (and imports/exposts) a bevy of
;;vars from other libraries to provide a unified, accessible
;;presentation for downstream consumers.  Much of the content in
;;sim.core originated from usage here, and transitioned over time.

;;To facilitate the current architecture, which held marathon.ces.core
;;in the same vein as spork.sim.core (a provider of simulation stuff
;;and generally useful things across multiple domains), we're going to
;;import vars from spork.sim.core, and transitively provide them via
;;marathon.ces.core.  This will enable the legacy design to retain a
;;dependency on marathon.ces.core, while behind the scenes, most of
;;the vars have been moved over to spork.sim.core.

;;We're importing so that down-stream dependencies will still work in
;;the refactor.

;;For reference, I've included the namespaces and ordered the symbols
;;by their namespace and line appearance.  So, if you want to goo look
;;at the symbols in source, you have and idea of where they're
;;defined.  Otherwise, the REPL will reflect the referenced var,
;;including ns, and provide doc/source as well.  Note: this list was
;;generated at the REPL by spork.util.metaprogramming/symbols-by-ns
;;....
(util/import-vars
 [spork.sim.core
  ;spork.sim.core
  noisy?  toggle-noisy entity?  set-parameter merge-parameters
  emptysim *debug* *verbose* *ignored* debugging debugging!
  debug-entity ignoring noisy visible?  *event-filter*
  with-event-filter debug-listener debugsim debug!  debug-by!  events
  times segments updates update-events visualize-store visualize-data
  current-entity now msg key= key-tag-maker defkey next-idx
  trigger-event context?
  ;spork.sim.simcontext 
  add-time request-update get-time merge-updates merge-entity

  ;spork.util.general
  collect atom?  float-trunc deep-assoc deep-get deep-update
  deep-dissoc    

  ;spork.ai.messaging
  ->msg handle-message! send!!

  ;spork.entitysystem.ephemeral
  get-ephemeral conj-ephemeral reset-ephemeral swap-ephemeral
  some-ephemeral swap-counter inc-counter get-counter persist-counters
  get-count
  
  ;spork.util.inspection
  tree-view
  
  ;spork.entitysystem.store
  entity-name]
  [spork.util.general
   print-float
   prune-in ;;TODO do we need this?
   deref!!
   ])

;;Common Paths to Simulation Resources
;;====================================
;;Creates a set of accessors for our simulation state.  This allows us
;;to dissect our nested map of state a bit easier.  Each symbol in the
;;defpath binding returns a function that, given a simulation context,
;;points to the named resource using standard clojure map operations
;;via clojure.core/get-in.

;; (defpaths   [:state] 
;;   {parameters    [:parameters]
;;    supplystore   [:supplystore]
;;    demandstore   [:demandstore]
;;    policystore   [:policystore]
;;    fillstore     [:fillstore]
;;    fill-function [:fillstore :fillfunction]
;;    fillmap       [:fillstore :fillmap]
;;    behaviors     [:behaviormanager]
;;    supply-tags   [:supplystore :tags]
;;    demand-tags   [:demandstore :tags]})

;;generic get/set path functions.
(defpaths
  parameters    :parameters
  supplystore   :SupplyStore
  demandstore   :DemandStore
  policystore   :PolicyStore
  fillstore     :FillStore
  fill-function [:FillStore :fillfunction]
  fillmap       [:FillStore :fillmap])

;;mutable context
;;===============

(defn mutate-ctx [ctx]
  (update-in ctx [:state :store] store/mutate!))
(defn persist-ctx [mutable-ctx]
  (-> mutable-ctx
      (update-in  [:state :store] store/persist!)
      (store/drop-domain :spork.data.eav/name)))

;;probably not useful.
(defn get-behaviors   [ctx] (get-entity ctx :behaviormanager))
(defn get-demand-tags [ctx] (get (get-demandstore ctx) :tags))
(defn get-supply-tags [ctx] (get (get-supplystore ctx) :tags))

(defn demands [ctx]
  (for [id  (keys (:demandmap (get-demandstore ctx)))]
    (get-entity ctx id)))

(defn active-demands [ctx]
  (for [id (keys (store/gete ctx :DemandStore :activedemands))]
    (get-entity ctx id)))

(defn units   [ctx]
    (for [id  (keys (:unitmap   (get-supplystore ctx)))]
      (get-entity ctx id)))

;;Wow...this is really really easy to do now....constructing queries on the
;;entity store is nice...
(defn locations
  ([t ctx]
   (->> (store/only-entities ctx [:name :locationname :location :positionpolicy :src :component])
        (into [] (map #(assoc % :t t)))))
  ([ctx] (locations  (sim/get-time ctx) ctx)))

(defn fills
  ([t ctx]
   (let [ds (store/gete ctx :DemandStore :activedemands)]
         (->> (store/only-entities ctx [:name :locationname :location])              
              (into [] (comp (filter (fn [r] (get ds (:locationname r))))
                             (map #(assoc % :t t))
                             )))))
  ([ctx] (fills  (sim/get-time ctx) ctx)))

;;Tells us is the location is a demand
(defn demand? [ctx location]
  (contains? (store/gete ctx :DemandStore :demandmap) location))

(defn active-fills [ctx]
  (persistent!
   (reduce-kv (fn [acc id _]
                (let [d (store/get-entity ctx id)]
                  (as-> acc inner
                    (reduce conj! inner (keys (:units-assigned d)))
                    (reduce conj! inner (keys (:units-overlapping d))))))
              (transient [])
              (store/gete ctx :DemandStore :activedemands))))

;;This is a pretty useful deployment query....
;;provides us with a map of 
(defn deployments
  ([t ctx]
   (->> (get-demandstore ctx)
        (:activedemands)
        (keys)
        (map #(store/get-entity ctx %))
        (map (fn [{:keys [category demandgroup operation vignette Command] :as d}]
               {:t t
                :name        (:name d)
                :src         (:src  d)
                :assigned    (count  (:units-assigned d))
                :overlapping (count  (:units-overlapping d))
                :quantity    (:quantity d)
                :filled      (count (:units-assigned d))
                :unfilled    (- (:quantity d) (count (:units-assigned d)))
                :category    category
                :demandgroup demandgroup
                :operation   operation
                :vignette    vignette
                :command     Command
                :total       (+ (count (:units-assigned d))  (count (:units-overlapping d)))}
               ))))
  ([ctx]  (deployments (sim/get-time ctx) ctx)))

;;query the entity store to find deployments
(defn deployment-records  [ctx]
  
  )

(defn location-table [ctx]
  (let [t (sim/get-time ctx)]
    (->> (store/only-entities ctx [:name :locationname :location])
         (map #(assoc % :t t))
         (tbl/records->table))))

;;Terrible, short-sighted hack just to get things working.
(defn srm-demand? [ctx nm]
  (when-let [c (gete ctx nm :Category)]
    (= c "SRM")))


;;Functions for dealing with subsets of supply/demand,
;;for defining smaller simulations.
#_(defn entity? [x] (instance? spork.entitysystem.store.entity x))
(defn prune-children [prune? parent]
  (reduce-kv (fn [acc p c]
               (if (prune? p)
                 (dissoc acc p)
                 (cond (and (map? c) (not (entity? c)))  (assoc acc p (prune-children prune? c))
                                        ;(vector? c) (into [] (filter prune? c))
                                        ;(seq c) (filter prune? c)
                       :else 
                       acc)))
             parent parent))

(defn drop-units [names ctx]
  (let [drops (set names)
        prune! #(prune-children drops %)]
    (-> ctx
        (store/updatee  :SupplyStore :unitmap prune!)
        (store/updatee  :SupplyStore :deployable-buckets prune!))))

(defn drop-demands [names ctx]
  (let [drops (set names)
        prune! #(prune-children drops %)]
    (-> ctx
        (store/updatee  :DemandStore :demandmap prune!)
        (store/updatee  :DemandStore :activations prune!)
        (store/updatee  :DemandStore :deactivations prune!)
        (store/updatee  :DemandStore :activedemands  prune!)
        )))
;;we need to remove the entities from any reference too..
;;THis is akin to enabling/disabling....
(defn solo-src [solo ctx]
  (let [dropped (set (map :name
                          (store/select-entities ctx
                                                 :from [:src]
                                                 :where (fn [{:keys [src]}]
                                                          (not= src solo)))))
        
        ]
    (as->    (->> (reduce (fn [acc e]
                               (store/drop-entity acc e)) ctx dropped)
                     (drop-units dropped)
                     (drop-demands dropped)
                     )
             dropped-ctx 
        (reduce (fn [ctx nm]
                  (store/add-entity ctx {:name nm :disabled true}))
               dropped-ctx 
               dropped))))

;;We're starting to build stats and queries...muahaha...this is where clojure kicks ass.
;; (defn deployed-population [ctx]
;;   (for [{:keys [name assigned overlapping quantity]}]
;;     (deployments ctx)))

(defn periods [ctx] (:periods   (get-policystore ctx)))
(defn current-period [ctx]
  (-> ctx (store/get-ine [:PolicyStore :activeperiod :name])))

;;THis is less useful now that we're in an entitystore...
;;might migrate away from this, treat it as a code smell
;;possibly.
(defmacro with-simstate 
  "Given a destructuring of [[path1 path2...] the-simstate], paired
   with an expression, evaluates the expression under a lexical context
   where the path symbols are bound to corresponding get-path variants 
   as defined for simstate."
  [[path-symbs state]  & expr]
  (let [symb->path {'parameters    get-parameters
                    'supplystore   get-supplystore
                    'demandstore   get-demandstore
                    'behaviors     get-behaviors
                    'policystore   get-policystore
                    'fillstore     get-fillstore
                    'fill-function get-fill-function
                    }]
    `(let [~@(reduce (fn [acc [p v]]
                       (-> acc (conj p) (conj v)))
                     []
                     (for [p path-symbs]
                       [p (if-let [res (get symb->path p)] 
                            `(~res ~state)                                   
                            (throw (Exception. (str "Unknown path " p))))]))]
       ~@expr)))

;;#Protocols 
;;Alias for entity protocol.  Helps us unify name access.

;(defn entity-name [x] (get x :name))

;;#Operations for working with mutable references
;;particularly working with pieces of state in a nested associative
;;structure.

;;Imports from spork.util.cellular and simcontext
#_(util/import-vars
 #_[spork.util.inspection
    tree-view]
 #_[spork.util.cellular
    with-cells
    with-transient-cells
    swap-cell!
    reset-cell!
    ->cell]
 #_[spork.entitysystem.store
    entity-name]
 #_[spork.sim.simcontext ;minimal amount of stuff pulled in..
    merge-updates
    merge-entity
    get-time
    add-time])

;;Messaging convenience functions.
#_(util/import-vars 
   [spork.ai.messaging  
    ->msg
    handle-message!
    send!!])

#_(defn set-parameter    [s p v] (assoce  s :parameters p v))
#_(defn merge-parameters [s ps]  (mergee  s :parameters  ps))

;;Operations for recording new units in the context.
;;We now just merge the new entity into the context; no need to
;;mess with the supplystore.  We may also get away with only
;;merging things that actually changed.
(defn set-unit
  ([u ctx]   (mergee ctx (:name u) u)) 
  ([u s ctx] (mergee ctx (:name u) u)))

;;#Empty Simulation Contexts
;;altered.
;;TODO Deprecate?
(def emptystate
  "An empty simulation state"
  (simstate/->store)) ;;now using ces

;;we could make emptysim dynamic...
(def emptysim
  "A custom simulation context that shadows the spork.sim.core/emptysim"
  (sim/add-time 0 (sim/make-context :state emptystate)))

;;Note:
;;This is the primary simulation context we use from marathon.analysis,
;;particularly when we generate marathon-streams or histories.
(def debugsim
  "A shadowed version of spork.sim.core/debugsim, using our custom simstate."
  (->> (-> (sim/make-debug-context 
            :debug-handler  
            debug-listener)
           (assoc :state emptystate))
       (sim/add-time 0)))

;;#State-wide queries...
;;TODO Deprecate or move?
(defn features [ctx & {:keys [where] :or {where identity}}]
  [:simstate
   (for [[nm obj] (seq (:state ctx))
         :when (where nm)]
     [nm (if (map? obj) (keys obj) obj)]
     )])

;;common counters using the functionality from entitystore.ephemeral via
;;spork.sim.core 
(defn deployment-count     [ctx]  (get-count ctx :deployment-count))
(defn inc-deployment-count [ctx]  (inc-counter ctx :deployment-count))

;;Acts like juxt, except it returns the results in a map.
;;The map is implied by the args in kvps, where simple keys - numbers,
;;strings, keywords, are assumed to be field names. A map is built 
;;from the fields by getting the corresponding field in an input
;;record.  Vector keys are assumed to imply [key function-to-apply]
;;TODO: Check for dependencies and prune...
(defn juxtmap [& ks]
  (let [fs  (reduce (fn [acc x]
              (cond (or  (keyword? x)  (string? x) (number? x))   (assoc acc x #(get % x))
                    (vector? x)  
                      (let [fld (first x) 
                            getter (second x)]
                        (assoc acc fld 
                          (cond (fn? getter) getter 
                                (or  (keyword? getter)  (string? getter) (number? getter))   
                                 #(get % getter)
                                :else (throw (Exception. (str "unknown juxt-map getter " getter))))))
                      :else (throw (Exception. (str "unknown juxt-map arg " x)))))
                   {} ks)]
    (fn [x] 
      (reduce-kv (fn [m fld f] (assoc m fld (f x))) {} fs))))
;;TODO: Check for dependencies and prune...
(defmacro fields [xs]
  (cond (map? xs)
        `(marathon.sim.core/juxtmap ~@(:fields xs))
        (vector? xs) 
        `(juxt ~@xs)))

(defn demands->track [xs] 
  ;(sketch/->track (map (fn [r] (merge r {:start (:startday r)})) xs) :track-name name)
  )

(defn demand->tracks  
  [xs & {:keys [keyf] :or {keyf (juxt :demandgroup :src)}}]
  (for [[g xs] (group-by keyf xs)]
    [g (map (fn [r] (merge r {:start (:startday r)})) xs)]))


(defn visualize-events [es track-keyf color-keyf]  
;;   (let [coloring (zipmap (map color-keyf es) (take (count es) (sketch/palette)))
;;         tracks   (demand->tracks es :keyf track-keyf)
;;         rendered-tracks (sketch/->tracks tracks)
;;         track-width     (:width (spork.graphics2d.canvas/shape-bounds rendered-tracks))
;; ;        lgnd     (sketch/->legend coloring)
;; ;        lwidth   (:width (spork.graphics2d.canvas/shape-bounds lgnd))
;;         ]
;;     (sketch/with-event->color (fn [e] (get coloring (color-keyf e)))
;;       (sketch/sketch-image
;;        (sketch/scale 1.0 1.5
;;                      (sketch/stack [(sketch/->tracks tracks)
;;                                    ; (sketch/translate 10 5 
;;                                    ;    (sketch/scale (float (/ track-width lwidth)) 2.0
;;                                    ;      lgnd))
;;                                     ])))))
  )

(defn visualize-demands [ctx  & {:keys [track-keyf color-keyf] :or {track-keyf (juxt :demandgroup :src)
                                                                    color-keyf (juxt :vignette)}}]
  (visualize-events (vals (demands ctx)) track-keyf color-keyf))

(defn visualize-updates [ctx  & {:keys [track-keyf color-keyf] :or {track-keyf (juxt :requested-by)
                                                                    color-keyf (juxt :update-type)}}]
  (visualize-events (update-events ctx) track-keyf color-keyf))

(defn visualize-supply [ctx]
  (->> (units ctx)
       (map (fn [u] (update-in u [:policy] :name)))
       (spork.util.table/records->table)
       (spork.util.table/visualize)))

(defn visualize-fillmap [ctx]
  (if-let [fm (:fillmap (get-fillstore ctx))]
    (->> (for [[parent children] fm
               [child cost] children]
           {:donor parent :recepient child :cost cost})
         (spork.util.table/records->table)
         (spork.util.table/select :fields [:donor :recepient :cost] :from)
         (spork.util.table/visualize))
    (throw (Exception. "No fill map to visualize!"))))

(defn visualize-graph [g]
  (jung/view-graph g jung/fr))

(defn visualize-fillgraph [ctx]
  (if-let [fg (:fillgraph (get-fillstore ctx))]
    (visualize-graph fg)
    (throw (Exception. "No fillgraph to visualize!"))))

;;Short queries...we should move these away from being a map for
;;entities, and into sets. Set access is actually faster than
;;maps, so bonus.
(defn demand-names    [ctx] (keys (gete ctx :DemandStore :demandmap)))
(defn unit-names      [ctx] (keys (gete ctx :SupplyStore :unitmap)))
(defn unit-entities   [s]   (store/get-domain s :unit-entity))
(defn unit-records    [ctx] (store/get-entities ctx (unit-names ctx)))
(defn demand-entities [s]   (store/get-domain s :demand-entity))

;;fetch units with appended :dt information for
;;potential synchronization purposes.
(defn current-units
  ([ctx t]
    (for [id  (keys (:unitmap   (get-supplystore ctx)))]
      (current-entity ctx id t)))
  ([ctx] (current-units ctx (sim/current-time ctx))))

;;this is actually using our new positional stuff.
;;we need to map deployed-trend to risk....
;;I think we just want to use demand-trends...
;;demand-changes gets us the units...
;;we also need to compute the peak demand.
(defn all-demands [store]
  (store/select-entities store :from [:startday :duration :quantity :demand-entity]))

;;not sure about this, we may want to break out by trends...
;;We'll see what use case pops up for this type of query.
(defn demand-profile [store]
  (->> (temp/activity-profile 
        (all-demands store)
        :start-func :startday :duration-func :duration
        )
       (map (fn [[t {:keys [actives]}]]
              [t (reduce + 0 (map :quantity actives))]))))

(defn peak-demand [store]
  (->> (temp/peak-activities
        (all-demands store)
        :start-func :startday :duration-func :duration
        :peak-function (fn [{:keys [actives]}]
                         (reduce + 0 (map :quantity actives))))
       (first)
       (val)
       (:actives)
       (map :quantity)
       (reduce + 0)))


;;This is fairly close....
(defn visualize-entities [ctx]
  (let [demandnames    (demand-names ctx)
        unitnames      (unit-names   ctx)
        disabled       (keys (store/get-domain ctx :disabled))
        basic-entities (set  (concat demandnames unitnames disabled))
        stores         (clojure.set/difference
                        (set  (keys (store/entities ctx)))
                        basic-entities)
        named-entry (fn [e]
                      (if (instance? clojure.lang.MapEntry e) (inspect/entryvis e)
                          (inspect/entryvis (clojure.lang.MapEntry. (or (:name e)
                                                                        (store/entity-name e)) e))))]         
    (inspect/tree-view
       {:stores  (map (comp inspect/entryvis named-entry)  (sort-by :name inspect/generic-comp (store/get-entities ctx stores)))
        :demands (map (comp inspect/entryvis named-entry)  (sort-by :name inspect/generic-comp (store/get-entities ctx demandnames)))
        :units   (map (comp inspect/entryvis named-entry)  (sort-by :name inspect/generic-comp (store/get-entities ctx unitnames)))
        :disabled (map (comp inspect/entryvis named-entry)  (sort-by :name inspect/generic-comp (store/get-entities ctx disabled))) })))

(defn visualize-subscriptions [ctx] 
  (inspect/tree-view (:subscriptions (get-policystore ctx))))

;;TODO# define a visualization protocol, extend it to core datatypes...
(defn visualize-unit [u]   (inspect/tree-view u))    
(defn visualize-policy [p] (jung/view-graph (protocols/get-position-graph p) jung/fr))

       
;;#Shared Functions

;;These functions were extracted due to use across multiple domains.  I may 
;;refactor them into core services, but for now, relocating them in sim.core 
;;allows every system access to them.

;;Tom hack 26 MAy 2016
;;We discriminate between known or canonical buckets, and
;;ad-hoc buckets (buckets that are created as ephemeral supply
;;for followon-demands.  In contrast, we will likely always have
;;:default and :SRM categories of supply, i.e. they never go away.
(def known-buckets #{:default :SRM "SRM"})

;;PERFORMANCE NOTE: This is on the HotSpot, Apparently....
;;maybe just clojure 1.7, but we're creating a keyseq for clojure.core/keys,
;;which is exploiting this hotspot.  Should be way faster.  we'll use
;;reduce-kv to alleviate it....

;;The name here is a bit generic.  We're really trying to acquire the keys of 
;;a map that contains information about followon-eligible supply.  In this 
;;context, the keys are actually the followon-code of the unit, (typically a
;;demandgroup). 
(defn get-followon-keys
  "Returns a sequence of followon codes that are currently present in the 
   supply.  Used for constraining or preferring supply during the followon 
   fill phase."
  [ctx]
  (let [m  (reduce-kv (fn [acc k _]
                        (if-not (known-buckets k) (conj! acc k) acc)) (transient #{})
                        (store/gete ctx :SupplyStore :deployable-buckets))]
    (when (pos? (count m)) (persistent! m))))

;;Check the validity here...
;;Do we need so much redundancy now?
(defn update-unit
  "Associates unit u into the context's supply store, using the unit's name 
   as a key."
  [u ctx]                    
  (set-unit u ctx)  
  )

(defn ghost? [unit] (= (clojure.string/upper-case (:src unit)) "GHOST"))
(defn followon? [u] (:followoncode u))
(defn ghost-followon?  [u] (and (ghost? u) (followon? u)))
(defn default-recovery [ctx]
  (or (:DefaultRecoveryTime (get-parameters ctx))
      0))

;;#TODO get this working like it used to, right now it's not important.
;; (defn interval->date  [t ctx]
;;   (let [ps (get-parameters ctx)
;;         start-date (get ps :StartDate)
;;         time-scale (get ps :time-scale 1)] 
;;     (+ start-date (* time-scale t))))

;;interval->date is a simple stub, maybe unnecessary (although I like
;;having it around for records in the output)
(defn interval->date  [t ctx]  t)

(defn in-scope? [params src]
  (and (not (get-in params [:SRCs-Out-Of-Scope src]))
       (get-in params [:SRCs-In-Scope src])))

(defn scope-info [ctx]
  {:in-scope     (get (get-parameters ctx) :SRCs-In-Scope)
   :out-of-scope (get (get-parameters ctx) :SRCs-Out-Of-Scope)})

;;#Tag Related Functions#
;;Another useful bit of higher order, or meta data, is the notion of simple 
;;tags.  We use tags as a simple mechanism to track all kinds of effects and 
;;information, without having to embed data in lower classes.  This way, when
;;an entity - or even a system - needs to be grouped or categorized we can 
;;use tags.  As a result, we see a lot of shared tags that represent common 
;;effects, like whether an entity is enabled or disabled.

;Generic tag-related....These apply equally to supplystore, demandstore, etc.
;The interface is identical.  Only the interpretation of the tags is different.
(defn enabled? [store item]
  (tag/has-tag? (:tags store) :enabled item))
(defn disabled? [store item] (not (enabled? store item)))
(defn enable [store item]
  (update-in store [:tags] tag/tag-subject :enabled item))
(defn disable [store item]
  (update-in store [:tags] tag/untag-subject :enabled item))
(defn special-src? [tags src] (when tags (tag/has-tag? tags src "Special")))

;;#Fill Related Functions

;find-eligible-demands is implemented as multimethod that dispatches based on 
;type of the category that's passed in.  category-type provides a simple 
;dispatch mechanism.  Note-> could use built-in hierarchy functions to do this.
(defn category-type [x]
  (cond (or (keyword? x) (string? x))  :simple            
        (vector? x) :src-and-group 
        (map? x)    :rule-map
        :else (throw (Exception. "Unknown category type " (type x)))))

;;#Utility Functions and Macros

;;A collection of shared functions, might turn into protocols someday...
;;This should contain only core functionality shared across subsystems defined 
;;in other namespaces, to eliminate redunancy.
;;I started this due to the fact that several functions - primarily accessors, 
;;were bubbling up in each of the domain-specific modules.  As a result, we'll 
;;just shove them in here.  If we don't, we'll get circular dependencies.

;;Stubs were used during the initial port to toss exceptions if an unimplemented
;;or deferred function was called.  
(defmacro stub [msg & empty-body]  
  `(~'fn ~'[& args] (~'throw (~'Exception. ~msg))))

;This is a general utility function, that allows us to derive predicates based
;on atomic values, sets, or maps.  Used for filtering (currently in demand 
;categories and supply categories).
;Can probably extend this to allow for arbitrary predicate functions (later).
(defn make-member-pred [g]
  (if (or (set? g) (map? g))
    #(contains? g %)
    #(= g %)))

;;Predicate for determining if a map has a key equal to a val.
(defn key=
  "Returns a predicate function that, when applied to a map, 
   will determine if the value associated with k in the map 
   is equal to v."
    [k v] (fn [m] (= (get m k) v)))

;;#TODO evaluate memoize here and see if we're paying a useless
;penalty.
(defn key-tag-maker
  "Creates a little keyword factory that allows to to define descriptive 
   tags using keywords efficiently and consistently."
  [base] (gen/memo-1
          (fn [tag] (keyword (str base tag)))))

;helper macro for defining key-building functions.
(defmacro defkey [name base] `(def ~name (key-tag-maker ~base)))

;;#Utils

;;Common record validation....a little janky, but centralized.
(defn valid-record?
  [r]
  (and (:Enabled r)
       (pos? (:Quantity r))
       ;;Ensure 0-duration demands aren't included.
       (if (not (:Name r)) ;;demand record
         (if-let [dur (get r :Duration)] ;;has duration 
           (pos? dur) true)
         true)))

(defn ensure-name
  "We tend to prefer unique names, and often times we accomplish that by concatenating the 
   count of a container onto a non-unique name.  ensure-names generalizes this stuff."
  [named names]                  
  (let [nm (entity-name named)]
    (if (contains? names nm)
      (assoc named :name 
             (msg nm "_" (count names)))
    named)))

(definline empty-string? [x] `(= ~x ""))
(defn debug-print [msg obj] (do (println msg) obj))

(defn as-records [record-source]
  (if (and (seq? record-source) (map? (first record-source))) record-source      
      (tbl/record-seq record-source)))

(let [idx (atom 0)]
  (defn next-idx 
    "Utility function for getting incrementing indices.  Used for 
     naming entities.  Passing an argument resets the index to the arg,
     and returns the arg.  Otherwise, a mutable counter is incremented 
     and the result is returned."
    ([] (let [i @idx]
          (do (swap! idx unchecked-inc)
              i)))
    ([new-idx] (do (reset! idx new-idx)
                   new-idx))))

;;TODO de-duplicate this from marathon.data.protocols
;;look into replacing this with a universal constant, or upperbound
;;for longs
(def ^:constant +inf+ 9999999)

(defn finite-else
  "Ensures that x is a 'finite' value, relative to
   the sentinel constant for infinity +inf+, in that
   x < +inf+.  If not, returns a default, assumably
   finite value."
  [x default]
  (if (< x +inf+)
    x
    default
    ))

;;moved from marathon.ces.entityfactory
(def ^:constant +max-cycle-length+     10000)
(def ^:constant +default-cycle-length+ 1095)

;;moved from marathon.ces.entityfactory
(defn finite-cycle-length [policy]
  (let [clength (protocols/cycle-length policy)]
    (if (> clength +max-cycle-length+)
      (or (finite-else (protocols/expected-dwell policy)
                            +default-cycle-length+))
      clength)))

;;##Developer Notes

;;#Transitioning from Effectful Simulation and State Updating#
;;One problem area in the port from VBA->Clojure was the handling of 
;;decentralized state updates.  This primarily happened via event dispatch, or 
;;through side-effects called during management functions.  The solution for our
;;pure simulation is to formalize batch updates to the simulation by using 
;;simcontext/merge-updates.  This allows us to pass maps of updates around, and 
;;the updates are merged with the simulation state in a predefined manner.  
;;We may wish to formalize this as a language feature (i.e. macro) to define 
;;different types of update.

;;#Conventions for Notifications/Events/Messaging Functions
;;Message functions are suffixed by the \! character, by convention, to denote 
;;the possibility of side effects. The function signatures are pure, in that 
;;they return a context.  The naming convention will help identify when 
;;"messaging" is occuring.  While effects may happen, in the form of logging 
;;or display updating, the internal simulation mechanics are still pure.
;;__TODO__ -> implement defmessage macro....

;;#Cries for Better Abstractions
;;Note--> I'm noticing some patterns emerging that might make for nice 
;;abstractions...
;;We seem to be doing a lot of nested updates for particular bits of a system.
;;A lot of updates also appear to be existential in nature, i.e. if the 
;;update is a dissoc, and the resulting collection is empty, we want to 
;;remove the empty entry from the parent container.

;;Also, we have a class of functions that specifically shovels context around.
;;By context, I mean the environment in which the simulation is happening, as 
;;in a simcontext from sim.simcontext.  We can clean up some of the function 
;;signatures by allowing macros to operate in this context, possibly even a 
;;special evaluator.

;;On the topic of special evaluators, in each namespace, we end up defining 
;;pure functions that act to update specific bits of a context, sometimes 
;;acting on isolated structures within the context.  There's typically a 
;;nested structure in the context's :state, which destructure and update in 
;;pieces.  It might be nice to have something like #'doto. 

;;It might be nice to have a macro that lets us define ways to address or query
;;the context's state.  This leads directly to the entity-store stuff I already 
;;built, and facilitates a component-based architecture.

;;Finally, we probably want some kind of language for specifying different 
;;types of updates to the context.  For instance, we end up -inside the local 
;;namespaces - defining functions that operate on a specific area of the context
;;often with deep nesting paths.  I already introduced the notion of updates and
;;merge-updates in the simcontext.  It might be as simple as defining
;;multimethods, or a protocol, that implements the merge protocol.

;;what would a nice abstraction look like? 
;;adjust-max-utilization!, from marathon.sim.supply is a great candidate..
;;there are a couple of bread-n-butter items of interest...
;;1) we're updating a unit functionally, storing the result of the update, 
;;   a new unit
;;2) we're updating the supplystore, to reflect the updated unit from 1).
;;3) we're merging the new supplystore into the updated context, by 
;;   passing it to a generic "update" handler that knows how to interpret 
;;   the key :supplystore, the new supply, and a context, to transition to 
;;   a new context.
;;One improvement is to wrap the operation behind a function to provide a clean
;;API....namely, update-unit 
;;This would fetch the appropriate unit, apply a function to it (like update-in),
;;and return a new supplystore with the unit updated.
;;  In this sense, we're lifting a specific function, updating the policyq of 
;;  a unit, into the context of a container for units. 
;;  In haskell speak, the supplystore is a functor, we're applying an update to 
;;  an element of the container.  It's the container's job to understand how 
;;  to lift the updating function into the proper context, and return the new 
;;  container.  monads/monoids anyone? 

;;#Policy Management Notes
;;Policy changes were encapsulated in the IRotationPolicy implementations.
;;This assumed that units would change policies, regardless of event-context.
;;That's actually a decent assumption.
;;However, when we tell unitdata to change policy, it evokes a change state 
;;evaluation.
;;Under the decoupled architecture, this requires simulation context.

;;I'm going to have the policy ops define a function (really just adapted from 
;;policymanager), that passes the context needed.  This is in-line with other 
;;decoupled, functional representations.
;;I have to rewire the IRotationPolicy implementation.....specifically taking 
;;out the onperiodchange event handling.
;;Rather, we'll let policy ops take care of changing units' composite policies.  
;;The good news is, all the bits are here.  Just need to re-organize the code.

;;Is there a way we can create shallow maps?  For instance, if we're 
;;trying to update a nested map, and we don't want to make the whole 
;;thing transient, can we pass around a temporarily mutable version? 

;;one idea here, is to have something that wraps the map and creates 
;;a map of transient info...
;;When we do reads, we use the transient, when we do writes, we 
;;use the transient.  Then, when we want to reify, 
;;we merge the transient with the original.  Ideally, this 
;;keeps the copy paths nice and small.

;;the simplest way is to have a mutable collection, i.e. 
;;a hashtable, with the values along the path.
;;Then for get-in and assoc-in, we can look at the 
;;path cache and see if the path exists there.  Then, we 
;;can apply the ops to the pathcache. 

;;Operations optimized for speed.  the -in and friends 
;;are not sufficient...


;;<<<<<<<<<<<                                    >>>>>>>>>>>>>>>>>
;;This was an early idea to improve perf, but the side-effects are
;;causing simulation history to end up out-of-order...

;;Aborted Performance Optimization Tests
;;For now, we're putting row-store operations on hold.  Quite a bit of issues here...

;;#_(println [:<<<<<TODO :TEST-ROWSTORE 'marathon.ces.core/emptystate :TODO>>>>>])
;;#_(def emptystate (simstate/->store :init-store store/empty-rowstore))

;;Shitty hacks.
;; (defn row-state! []
;;   (do ;;Testing purposes...
;;       (throw (Exception. "Currently disabled due to changes row-store incurs against reproducibility."))
;;       (def emptystate    (simstate/->store :init-store spork.entitysystem.store/empty-rowstore))
;;        ;;temporary testing purposes...
;;        ;;we could make emptysim dynamic...
;;       (def emptysim   (sim/add-time 0 (sim/make-context :state emptystate)))
;;       (def debugsim   
;;         (->> (-> (sim/make-debug-context 
;;                   :debug-handler  
;;                   debug-listener)
;;                  (assoc :state emptystate))
;;              (sim/add-time 0)))))

;; (defn col-state! []
;;     (do ;;Testing purposes...
;;       (def emptystate    (simstate/->store :init-store spork.entitysystem.store/emptystore))
;;        ;;temporary testing purposes...
;;        ;;we could make emptysim dynamic...
;;       (def emptysim   (sim/add-time 0 (sim/make-context :state emptystate)))
;;       (def debugsim   
;;         (->> (-> (sim/make-debug-context 
;;                   :debug-handler  
;;                   debug-listener)
;;                  (assoc :state emptystate))
;;              (sim/add-time 0)))))
