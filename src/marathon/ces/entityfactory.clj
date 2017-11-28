
;;The entity factory is a port of legacy functions used to read 
;;entity information from tabular sets of data according to 
;;the marathon data schemas.  The factory's primary function is 
;;to project records onto entities (i.e. units and demands) 
;;as well as serve as a hub for registering entities and 
;;scheduling things like initial starting conditions.  
;;The entityfactory could probably be broken apart, 
;;but it's legacy functionality would have to go somewhere. 
;;For now, we'll keep it in a similar namespace, but as 
;;opportunities arise, we may be able to abastract out 
;;functionality.
(ns marathon.ces.entityfactory
  (:require [marathon        [schemas :as s]]            
            [marathon.data.protocols :as generic] ;rename
            [marathon.data   [cycle :as cyc]                              
                             [store :as store]]
            [marathon.demand [demanddata :as d]]
            [marathon.ces.demand :as demand]
            [marathon.ces.unit :as unitsim]
            [marathon.ces.behavior :as b]
            [spork.ai.behaviorcontext :as base]
            [marathon.supply [unitdata :as u]]
            [marathon.ces.supply :as supply]
            [marathon.ces.policy :as plcy]            
;;            [marathon.sim.engine :as engine]
            [spork.sim.simcontext :as sim]
            [marathon.ces.core :as core]
            [spork.util      [table :as tbl]]
            [clojure.core    [reducers :as r]]))


;;*Entity Orders
;;The entity factory used to be an object that maintained 
;;a reference to a parent simulation context.  This allowed 
;;it to interpret data, like  a set of demand 
;;records, into the act of creating demand entities, 
;;notifying the simulation context that the a new demand had been
;;registered, and registering the demand with the demandstore.
;;Most of this was accomplished via side effects, so creating, 
;;and loading entities, was fairly imperative and stateful.

;;Going forward, we're going to break up the distiction between
;;these responsibilities, and communicate the results via 
;;EntityOrders.  An EntityOrder is merely a record that 
;;the entity factory can intepret, given a context, to yield 
;;a new context.  In this case, the context is our stock 
;;simcontext.  Seen as an interpreter, our entityfactory 
;;evalutates orders relative to the simulation context, returning 
;;a new simulation context.  We define new kinds of EntityOrders 
;;to allow for different types of entities to be built.

;;In the scheme of our IO processes, this means we care about 
;;a lower layer of functions that read raw data - typically 
;;in the form of tab delimited text, and convert that data 
;;into some kind of EntityOrder that our factory can recognize.

;;Ideally, we end up with a simple interface for "Creating" entities
;;in the simulation, where entity creation may involve an arbitrarily 
;;complex sequence of updates.

;;*Basic Functions
;;Many of our functions are concerned with IO, that is, reading 
;;tab-delimited text records, and creating entity orders from them.

;;For now, we're using dynamic scope...
(def ^:dynamic *ctx* core/emptysim)

;;I think we may want to make the entityfactory functions operate 
;;on a dynamic var.  That should make the calling code clearer...
;;There are events where we trigger notifications and such, for 
;;logging purposes.  Maybe we we only check if the context is 
;;actually bound, and then do something.

;;The original procedure actually mutated the context 
;;as demand records were parsed.  We can decouple this step by 
;;creating a sequence of entity creation actions, i.e. entity 
;;orders, that can be processed into state changes.

;;For instance: 

;;This is technically a bottleneck at the moment; Not terrible, but 
;;we may look at more efficient ways of doing it.  It's the check for 
;;duplicate demands that's slowing us; The partitioning may be done 
;;more smartly, or without building two collections.
(defn partition-dupes 
  "Returns a pair of [uniques, dupes] based on the key function"
  [keyfn xs]
  (let [known (java.util.HashSet.)
        res  (reduce (fn [acc x]              
                       (let [uniques (first acc)
                             dupes   (second acc)
                             k (keyfn x)]
                         (if (.contains known k)
                           [uniques (assoc! dupes k (cons x (get dupes k nil)))]
                           (do (.add known k)
                               [(conj! uniques x) dupes]))))
                     [(transient [])  (transient {})]
                     xs)]
    [(persistent! (first res)) (persistent! (second res))]))

;;added positivity check for pre-run filtering.
(defn valid-record? 
  ([r params] 
     (and (:Enabled r) 
          (core/in-scope? params (:SRC r))
          (pos? (:Quantity r))))
  ([r] (valid-record? r (core/get-parameters *ctx*))))

(defn demand-key 
  ([{:keys [SRC Vignette Operation Priority StartDay Duration]}]
     (demand-key SRC Vignette Operation Priority StartDay Duration))
  ([SRC Vignette Operation Priority StartDay Duration] 
     (clojure.string/join "" [Priority "_"  Vignette "_" SRC "_["  StartDay "..."  (+ StartDay Duration) "]"])))

(def extra-fields
  '[Command
    Location
    DemandType
    Theater
    BOG
    StartState
    EndState
    MissionLength])

;;Could inline for speed, may be unnecessary...
(defn create-demand   
  "Produces a validated demand from the inputs.  We enforce invariants about 
   demanddata here to ensure that invalid values are caught and excepted."
  [DemandKey SRC  Priority StartDay Duration Overlap Category 
   SourceFirst Quantity  OITitle Vignette Operation  DemandGroup]  
  (let [empty-op  (core/empty-string? Operation)
        empty-vig (core/empty-string? Vignette)
        idx       (if (or empty-op empty-vig) (core/next-idx) 0)
        vig       (if empty-vig (core/msg "Vig-ANON-" idx) Vignette)
        op        (if empty-op  (core/msg "Op-ANON-" idx) Operation)
        demandname (or DemandKey (demand-key SRC vig op Priority StartDay Duration))
        _         (assert (pos? Quantity) (str [:non-positive-quantity! demandname]))]
    (store/demand    ;unique name associated with the demand entity.
     demandname
     demandname
     SRC ;demand-type, or other identifier of the capability demanded.
     Priority ;numerical value representing the relative fill priority.
     StartDay ;the day upon which the demand is activated and requiring fill.
     Duration ;the total time the demand is activated.
     Overlap  ;the demand-specific overlap requirement, if any
     Category ;descriptor for deployed unit behavior over-rides.
     SourceFirst  ;descriptor for supply preference. 
     Quantity  ;the total amount of entities required to fill the demand.
     OITitle   ;formerly OITitle.  Long-form description of the src capability.
     vig  ;Descriptor of the force list that generated this demand entity.
     op ;Fine-grained, unique description of the demand.
     DemandGroup ;Ket that associates a demand with other linked demands.  
     )))

;;This is hacky, should clean on parsing...
(defn clean-nils [drec flds]
  (reduce (fn [acc k]
            (if-let [v (get acc k)]              
              (if (and (string? v)
                       (or (= v "nil")
                           (= (clojure.string/trim v) "")))
                (assoc acc k nil)
                acc)
              acc))
          drec flds))

(defmacro coerce [[vars type] & expr]
  `(let [~@(reduce (fn [acc [l r]]
                     (-> acc
                         (conj  l)
                         (conj r)))
                   []
                  (for [v vars]
                    `(~v (~type ~v))))]
     ~@expr))

(defn record->demand 
  "Basic io function for converting raw records to demanddata."
  [{:keys [DemandKey SRC  Priority StartDay Duration Overlap Category 
           SourceFirst Quantity  OITitle Vignette Operation  DemandGroup
           ] :as rec}]
  (coerce [[Priority StartDay Duration Overlap Quantity] long]
    (-> (create-demand DemandKey SRC Priority StartDay Duration Overlap Category 
                       SourceFirst (if (pos? Quantity) Quantity 1) OITitle Vignette Operation  DemandGroup)
      ;this is a slightly hacked way to do it, but it's passable...
        (merge (select-keys rec [:Command
                                 :Location
                                 :DemandType
                                 :Theater
                                 :BOG
                                 :StartState
                                 :EndState
                                 :MissionLength]))
        (clean-nils [:StartState :EndState])
        )))


;;#Optimization: we can use reducers here instead of seqs.

;;Returns a set of demand data, derived from recs, with 
;;duplicate records attached as meta data.
(defn demands-from-records [recs ctx]  
  (let [in-scope?   (get (core/get-parameters ctx) :SRCs-In-Scope identity)
        [uniques dupes]  
            (->> recs 
                 (r/filter #(and (valid-record? % (core/get-parameters ctx))
                                 (in-scope? (:SRC %))))
                 (partition-dupes demand-key))]
    (with-meta (mapv record->demand uniques)
      {:duplicates dupes})))

;;Broadcast the fact that we have duplicates.
(defn notify-duplicate-demands! [dupes ctx]
  (let [name (:name (core/get-demandstore *ctx*))]
    (reduce (fn [ctx dup]
              (sim/trigger-event :Initialize name name 
                     (core/msg "Demand " (:DemandKey dup) " had duplicates in source data.") nil ctx))
            ctx
            dupes)))

;broadcast that a demand with initialized.
(defn initialized-demand! [ctx d]
  (let [msg (core/msg  "Demand " (:name d) " Initialized")]
    (sim/trigger-event :Intialize :DemandStore :DemandStore msg nil ctx)))

;;Do we need to worry about naming here? 
;;Seems like associate-demand could be done inside demand ops...

;;Updates the demand index according to the contents of 
;;demandmanager, providing unique names for duplicate 
;;demands.  Threads the demand through the context 
;;and returns the resulting context.
(defn load-demands 
  ([record-source ctx]
     (let [rs    (demands-from-records (core/as-records record-source) ctx)
           dupes (get (meta rs) :duplicates)]
       (->> (reduce (fn [acc d] 
                      (initialized-demand!                        
                         (demand/register-demand d acc) d))
                    ctx rs)
            (notify-duplicate-demands! dupes))))
  ([record-source demandstore ctx] 
     (load-demands record-source (core/set-demandstore ctx demandstore))))

(defn load-demands! 
  ([record-source ctx]
     (let [rs    (demands-from-records (core/as-records record-source) ctx)
           dupes (get (meta rs) :duplicates)]
       (->> ctx
            (demand/register-demands! 
             (fn [d acc] 
               (initialized-demand! d acc))  rs)             
            (notify-duplicate-demands! dupes))))
  ([record-source demandstore ctx] 
     (load-demands record-source (core/set-demandstore ctx demandstore))))
 
(defn ungrouped? [grp] 
  (when grp 
      (or (core/empty-string? grp) 
          (= (clojure.string/upper-case grp) "UNGROUPED"))))

;;#Unit Entity Creation
;;Interpreting unit entities typically requires four steps:
;;Create the basic information about the unit
;;Assigning the unit a policy
;;Associating the unit with a supply (ensuring name compatibility)
;;Initializing the unit's cycle
;;Registering the unit with supply (inserting the entity into a supply store)


;;consider changing these to keywords.
;;We can probably push this off to a policy default table.  It used
;;to mean that we had this interpreted into policy objects, all
;;initialized in a singleton class at runtime initiaion.  Now that
;;it's pulled out of the class, we can probably interpret it easier 
;;and then use a policy parser to point at the correct policies in 
;;the policystore.
(def ^:constant +policy-defaults+ 
  {"Ghost" {:special :SpecialGhostPolicy
            :default :DefaultGhostPolicy}
   "AC"    {:special :SpecialACPolicy
            :default :DefaultACPolicy}          
   "RC"    {:special :SpecialRCPolicy
             :default :DefaultRCPolicy}
   "NG"   {:special :SpecialNGPolicy
           :default :DefaultNGPolicy}})

;;we need a way to derive policy-defaults
(defn default-policy [params policystore policytype component]
  (let [policyname (get-in params [component policytype])]
    (if-let [p (plcy/find-policy  policyname policystore)]
      p
      (throw (Exception. (str ["No Default Policy for " policytype component]))))))

;;Note -> we're not mutating anything here.  We can pass in a 
;;parameters.
(defn choose-policy 
  "Tries to fetch an associated policy, and upon failing, selects a 
   default policy based on the component/src and whether the SRC is 
   tagged as being special."
  [policyname component policystore parameters src]
  (let [policy-type  (if (or (core/empty-string? src) 
                             (not (core/special-src? (:tags parameters) src)))
                       :default
                       :special)]
    (if-let [p (or (get-in policystore [:policies policyname])
                   (and (not= "Auto" policyname) ;added check to policyname
                        (plcy/find-policy policyname policystore)))]
      p 
      (if-let [res (get-in +policy-defaults+ [component policy-type])]
        (plcy/find-policy  (get parameters res) policystore)
        (throw (Exception. (str "Default Policy is set at "  
                                policyname  " which does not exist!")))))))

(defn policy? [x] (satisfies? marathon.data.protocols/IRotationPolicy x))
(defn has-policy? [u] (policy? (:policy u)))

(defn assign-policy [unit policystore params]
  (if (has-policy? unit) unit
      (let [p (choose-policy (:policy unit) (:component unit) policystore params (:src unit))]
        (assoc unit :policy p))))
             

;;Given a unit, initialize it's currentcycle based on policy information

;;Units maintain direct references to policies, but they do not
;;actively subscribe directly to the policy.  Rather, the policy 
;;store maintains a map of {policyname #{subscribers}}, we 
;;can even do this via tags in the tagbase, but it's already 
;;there in the policy store.  That way, when we invoke the 
;;policy subscription service, we should have an couple of updates: 
;;the unit now has a reference to the policy it subscribes to (
;;to make unit querying and updating easier), and the policystore 
;;has an association between the unit and the policy its 
;;subscribed to.

;;I think this guy should be elevated.
;;This really needs to happen in the context of the full sim, since
;;it's touching on several areas at once: policy, event-notification, supply.
;;We need access to multiple spheres of influence.  We used to just
;;mutate away and let the changes propgate via effects.  No mas.
;;Returns a policystore update and a unit update.
;;#TODO Implement policy->cycle-record 
;;#TODO Populate unit's first cycle.
(defn policy->cycle-record [p cycletime t  name src compo]
  (cyc/make-cyclerecord :uic-name name :src src :component compo 
                        :policyname        (generic/policy-name p)
                        :duration-expected (generic/cycle-length p)
                        :available-time    (generic/max-bog p)
                        :bog-expected      (generic/max-bog p)
                        :dwell-expected    (generic/cycle-length p)
                        :bogbudget         (generic/bog-budget p)
                        :mob-expected      (generic/max-mob p)
                        :tstart            t 
                        :tfinal            (+ t (generic/cycle-length p))
                        :duration          cycletime
                        :dwell             cycletime))

;;Moving this to spawning behavior...       
(defn initialize-cycle 
  "Given a unit's policy, subscribes the unit with said policy, 
   updates the unit's state to initial conditions, broadcasts 
   any movement via event triggers, returning the 
   new unit and a new policystore."
  [unit policy ghost ctx]
  (let [;_ (println :raw-position (:name unit) (:positionpolicy unit))
        currentpos (let [p (:positionpolicy unit)]
                      (when (not= p :spawn) p))
        newpos (if (not ghost) 
                 (or currentpos (generic/get-position policy (:cycletime unit)))
                 "Spawning")
        ]
    (-> unit       
        (merge {:policy policy
                :positionpolicy newpos
                :locationname  "Spawning"
                :currentcycle (policy->cycle-record policy (:cycletime unit) 0 (:name unit) (:src unit) (:component unit))})
;        (unitsim/change-location newpos ctx)
        )))

;;Computes the intervals between units distributed along a lifecylce.
;;Used to uniformly disperse units in a deterministic fashion.
(defn compute-interval [clength unitcount]  
  (if (or (zero? clength) (zero? unitcount)) 
    (throw (Error.  "Cannot have zero length cycle or 0 units"))
    (if (< unitcount clength) 
      (quot clength unitcount)
      (quot clength (dec clength)))))

;;note: we can compute the intervals directly...
;;when distributing n units over an integer range, we want to generate
;;where n <= clength
;;we should be able to compute n points with an interval >= 1
;;if n > clength, then we actually have more units than space.
;;To minimize "stacking", we distribute units sparsely
;;until we hit the magic #  n < clength.

;;#Potential Digression from M3
;;Replaces the inner logic 
#_(defn intervals [n clength]
  (let [remaining        (atom n)
        uniform-interval (atom (compute-interval clength n))
        last-interval    (atom (- @uniform-interval))        
        next-interval    (fn [remaining]
                           (let [nxt (long (+ @last-interval @uniform-interval))                                      
                                 nxt (if (> nxt clength) 0 nxt)]
                             (do (when (and (< remaining clength)
                                            (zero? nxt))
                                   (reset! uniform-interval 
                                           (compute-interval clength  remaining)))
                                 (reset! last-interval nxt)
                                 nxt)))]
    (map next-interval  (range n 0 -1))))

;;FIX: Proposed correction for the above intervals function, which appears to be flawed and
;;compressing cycle times for large populations!
(defn intervals [n clength]
  (let [i  (/ clength n)]
    (map #(long %)
         (take (long n) (iterate #(+ % i) 0)))))

;;take the unit seq and distribute the units evenly. Pass in a
;;collection of unit names, as well as the appropriate counts, and the
;;cycles are uniformly distributed (using integer division).
;;#Potential Digression from M3
(defn distribute-cycle-times [units policy]
  (let [clength (generic/cycle-length policy)
        clength (if (> clength core/+max-cycle-length+) core/+default-cycle-length+ clength)]                                              
    (mapv  (fn [cycletime  unit]
             (assert  (not (neg? cycletime)) "Negative cycle time during distribution.")                          
             (assoc unit :cycletime cycletime))
           ;;Added for consistency with legacy eversion....not technically incorrect.
           (->> (intervals (count units) clength)
                (map identity #_inc))
             units)))

;;legacy
#_(defn distribute-cycle-times [units policy]
    (let [clength (generic/cycle-length policy)
          clength (if (> clength +max-cycle-length+) +default-cycle-length+ clength)
          uniform-interval (atom (compute-interval clength (count units)))
          last-interval (atom (- @uniform-interval))
          remaining     (atom (count units))
          next-interval (fn [] (let [nxt (long (+ @last-interval @uniform-interval))
                                     nxt (if (> nxt clength) 0 nxt)]
                                 (do (when (< @remaining clength)
                                       (reset! uniform-interval 
                                               (compute-interval clength  @remaining)))
                                     (reset! last-interval nxt)
                                     (swap! remaining dec)
                                     nxt)))]                                              
      (reduce (fn [acc  unit]
                (let [cycletime (next-interval)
                      unit      (assoc unit :cycletime cycletime)]
                  (do (assert  (not (neg? cycletime)) "Negative cycle time during distribution.")
                      (conj acc unit))))
              []
              units)))


    
    



;;This is just a stub.  We currently hardwire behaviors, 
;;but will allow more extension in the future.  The legacy 
;;behavior system required an inherited object model implementation, 
;;but now we don't have to worry about that.  We'll probably just 
;;have a map of functions, or types that can fulfill the unit behavior
;;protocol.
(defn find-behavior
  ([]  @base/default-behavior)
  ([b] (or (get @base/behaviors b)
         @base/default-behavior)))

;;create-unit provides a baseline, unattached unit derived from a set of data.
;;The unit is considered unattached because it is not registered with a supply "yet".  Thus, its parent is
;;nothing. parametrically create a new unit.
(defn create-unit [name src oititle component cycletime policy behavior & {:keys [home speed]}]
  (into 
   (store/unit
    name ;id of the unit entity; for now we'll stick with the name.
    name ;unit entity's unique name. corresponds to a UIC 
    src ;unit entity's type, or capability it can supply.
    component ;unit entity's membership in supply.
    policy  ;the policy the entity is currently following.
    ;;[] ;a stack of any pending policy changes. was statestack..                                        
    :spawning ;the current physical location of the unit. ;fsm/blank-data ;generic state data for the unit's finite state machine.
    :blank-icon
    name
    [0 0]     ;the current position of the unit in its 2D space.
    [0 0]     ;velocity of the unit
    nil       ;color of the unit
    (or home  :default) ;unit's home station.
    :location :spawning ;the current physical location of the unit.
    :speed    (or speed 8) ;speed per unit of time the unit can move.
    :behavior behavior ;the behavior the unit uses to interpret policy and messages.
    )
   (merge {:cycletime    (long cycletime) ;the unit's current coordinate in lifecycle space.
           :followoncode nil       ;description of the categories this unit serve as a followon to.                      
           :currentcycle nil ;the current cycle data structure for the unit.
           :cycles [] ;an ordered collection of the cycles that the unit has completed.
           :spawntime -1 ;the time in which the unit spawned.
           :oititle oititle ;the description of the unit.
           :locationhistory [] ;list of all the locations visited.
           :dwell-time-when-deployed 0}  ;dwell time before deployment  
          )))

(declare srm-record->unitdata)
;;Derives a default unit from a record that describes unitdata.
;;Vestigial policy objects and behavior fields are not defined.  We
;;may allow different behaviors in the future, but for now they are
;;determined at runtime via the legacy processes (by component).
(defn record->unitdata
  [{:keys [Name SRC OITitle Component CycleTime Policy Command Origin Duration Behavior
           ] :as r}]
  (if (= Behavior "SRM")    ;;hackish way to go about things...
   (srm-record->unitdata r)
   (create-unit  Name SRC OITitle Component CycleTime Policy (find-behavior Behavior) :home Origin)))

;;Ideally, we'll unify this in the near future, for now it's srm specific.
;;we can have the unit behavior handle assigning policy.  From the start, we know
;;at least the start location, state, and duration.  We need policy to figure out
;;where to go next though.

;;Special extension to handle the spawn-time requirements of the SRM
(defn srm-record->unitdata [{:keys [Name SRC OITitle Component CycleTime Policy Command Origin Duration Behavior
                                    Location Position Command Duration]}]
  (do ;(println [:loading-srm-unit Name Position Location])
      (-> (create-unit  Name SRC OITitle Component CycleTime Policy (find-behavior Behavior))
                                        ;SRM specific spawning information.
           (merge {:home                  Origin
                   :locationname          Location
                   :positionpolicy        Position
                   :command               Command
                   :spawn-info {:location Location  ;;we have starting information, but not the next state...
                                :position Position
                                :duration Duration 
                                }
                   :default-bucket "SRM"}))))

;;Changed to 1-based indexing for compatibility with M3.
(defn generate-name 
  "Generates a conventional name for a unit, given an index."
  ([idx unit]
     (core/msg (inc idx) "_" (:SRC unit) "_" (:component unit)))
  ([idx src compo]
     (core/msg (inc idx) "_" src "_" compo)))
      
(defn check-name 
  "Ensures the unit is uniquely named, unless non-strictness rules are 
   applied."
  [name supply strictname]  
    (if (supply/unit? supply name)
      (if strictname 
        (throw (Error. (str  "A unit already exists with the name " 
                             name " in SupplyManager " (:name supply)  
                             ", unit names must be unique")))
        (str name (count (:unit-map supply))))
      name))
        
;;This does not actually attach the unit inside the supplystore, only 
;;preps the unit's name.  It is associated, in the legacy sense that
;;the unit would gain a "pointer" to the parent supply, as well as
;;having its name preconditioned to ensure compatibility or catch 
;;errors.  In the newer version, we may consider composing this with 
;;marathon.sim.supply/register-unit , which actually attaches the 
;;unit to the supply store.  Note: we could handle parent-child 
;;assocation there as well...The unique functionality here is that 
;;we are either generating or altering the name of the unit to ensure 
;;it is unique in the supply.
(defn associate-unit   
  "Associate or attach a new unit to a particular supply. If the new unit's name is 
  not unique, it will be changed to accomodate uniqueness requirement."
  [unit supply strictname]
  (assoc unit :name (check-name (:name unit) supply strictname)))
        
;;Note -  register-unit is the other primary thing here.  It currently 
;;resides in marathon.sim.su

;;Adds multiple units according to a template.
;;Associates each unit with a

;;Breaking apart add-units into three discrete steps: 
;;1) create n units, with empty cycles, according to the demand
;;   record
;;2) push that batch of units through cycle distribution. 
;;3) Prep
;;4) Register


;; ;(if ghost (-> supply :behaviors :defaultGhostBehavior)
;; ;                            (-> supply :behaviors
;; ;                            :defaultACBehavior))

;;need to define extensible ways to create batches of units,
;;possibly using multimethods..

(defn pstore->params [pstore] (core/get-parameters (:ctx (meta pstore))))
(defn index-unit     [idx  u] (assoc u :unit-index idx))

;;We can extract the supply dependencies, and the ghost arg, 
;;if we provide the behavior to be used.
(defn create-units
  [idx amount pstore base-record]
  (let [bound     (+ idx (quot amount 1))
        pname     (:Policy base-record)
        policy    (case pname
                    "Auto" (choose-policy pname (:Component base-record) pstore
                                          (pstore->params pstore) (:SRC base-record))
                    (plcy/find-policy pname pstore))]
    (if (pos? amount)
        (loop [idx idx
               acc []]
          (if (== idx bound)  
            (distribute-cycle-times acc policy)
            (let [nm       (generate-name idx (:SRC base-record) (:Component base-record))
                  new-unit (-> (assoc base-record :Name nm)
                               (record->unitdata)
                               (assoc :unit-index idx)) ;;added UnitIndex
                  ]                
              (recur (unchecked-inc idx)
                     (conj acc new-unit))))))))

(defn valid-supply-record? [r]
  (and (:Enabled r) (pos? (:Quantity r))))

;;so, we'd like to maintain a deterministic sequence of random weights.
(defn rands-from [seed]
  (let [prng (java.util.Random. seed)]
    (repeatedly  (fn []
                   (.nextDouble prng)))))
(defn ints-from [n seed]
  (map #(int (* % n)) (rands-from seed)))

;;Alleviates the problem with using :unit-index for weighting (tie breaking)
;;during fills.
(defn weights-from
  "Creates a determinstic sequence of weights, based on an initial count
   of entities.  This lets us incrementally add entities without
   losing our place in the random number stream.  We typically
   won't call this more than once - during initialization -
   so most of the time we won't be dropping.  Should not be a
   bottleneck.  Provides a psuedo-random uniform basis for
   selecting entities based on a weight.  Returns a weight in the
   range [0 2147483647), which is Integer/MAX_VALUE"
  [initial-count]
  (->> (ints-from 2147483647 0)
       (drop initial-count)))

;;Given a set of raw unit records, create a set of unitdata that has
;;all the information necessary for initialization, i.e. lifecycle, 
;;policy, behavior, name, etc.  We want to name the units according 
;;to the order in which they were generated, so we provide a
;;supplystore to derive the next index from.  We'll pass the output
;;from this onto a function that prepares the units.  From there 
;;we do all the minute prepatory tasks to "fill in the details", 
;;like associating the units with a supply, establishing unit 
;;subscriptions to policies, etc..
(defn units-from-records
  ([recs supply pstore filter-func]
   (let [unit-count (atom (-> supply :unitmap (count)))
         initial-count @unit-count
         ghost-beh  (-> supply :behaviors :defaultGhostBehavior)
         ;;TODO fix this...our defaults aren't that great.  
                                        ;        normal-beh @base/default-behavior ;(-> supply :behaviors :defaultACBehavior)
         conj-units (fn [acc xs] (do (swap! unit-count + (count xs))
                                     (reduce conj! acc xs)))
         conj-unit  (fn [acc x] (do (swap! unit-count inc)
                                    (conj! acc x)))
         rassoc     (fn [k v m] (assoc m k v))]
     (->> recs 
         (r/filter filter-func) ;;We need to add data validation, we'll do that later....
         (reduce (fn [acc r]                    
                   (if (> (:Quantity r) 1) 
                     (conj-units acc
                                 (create-units @unit-count  (:Quantity r) pstore r))
                     (->> (generate-name @unit-count (:SRC r) (:Component r))
                          (assoc r :Name)
                          (record->unitdata) ;;assign-policy handles policy wrangling.
                          (rassoc :unit-index @unit-count) ;;Add UnitIndex 3/8/2017
                          (conj-unit  acc)))) (transient []))
         (persistent!)
         (mapv (fn [weight unit] ;;added random-weights
                (assoc unit :unit-weight weight)) (weights-from initial-count))
         )))
  ([recs supply pstore]
   (units-from-records recs supply pstore valid-supply-record?)))

;;we have two methods of initializing unit cycles.
;;one is on a case-by-case basis, when we use create-unit
 
;;TODO - this returns a ctx, we're using it like we have a unit.
;;Alter the signature to [newunit, newctx]
(defn prep-cycle [unit ctx]
  (initialize-cycle unit (:policy unit) (core/ghost? unit) ctx))

(defn prep-unit 
  "Given a raw-unit, ensures its name is good with the supplystore, 
   assigns a policy (typically read from the existing policy field) 
   and initializes its cycle relative to the current cycletime."
  [unit supplystore policystore parameters ctx]
  (-> unit       
      (associate-unit supplystore true)  ;;may move this into supply/register-unit...
      (assign-policy policystore parameters)      
      (prep-cycle ctx)))

;;Persistent version for testing...OBE when the mutable version works.

;;All we need to do is eat a unit, returning the updated context.
;;A raw unit is a unitdata that is freshly parsed, via create-unit.
(defn process-unit [raw-unit extra-tags parameters behaviors ctx]
  (core/with-simstate [[supplystore policystore] ctx] ;could get expensive 
    (let [prepped   (-> raw-unit       
                        (associate-unit supplystore true)
                        (assign-policy policystore parameters)      
                        (prep-cycle ctx))
          newstore (plcy/subscribe-unit prepped (:policy prepped) policystore)         
          newctx   (core/set-policystore ctx newstore)]
      (supply/register-unit supplystore behaviors prepped nil extra-tags newctx))))

(defn process-units [raw-units ctx]
  (core/with-simstate [[parameters behaviors] ctx]
    (reduce (fn [acc unit]
              (process-unit unit nil parameters behaviors acc))
            ctx                 raw-units)))

;;TODO# verify mutable version.
;;We need to revisit where all the cellular mutation is occuring and
;;provide cell paths for those areas is we want to use these more
;;efficient versions.

;;Pending Mutable version for bulk updates.
(defn process-unit! [raw-unit extra-tags parameters behaviors supplystore policystore ctx]
  (let [prepped   (-> raw-unit       
                      (associate-unit supplystore true)
                      (assign-policy policystore parameters)      
                      (prep-cycle ctx))]
    (do (supply/register-unit! supplystore behaviors prepped nil extra-tags ctx)
;        (plcy/subscribe-unit prepped (get u :policy)  policystore)
        ctx)))

;;for each unit in unitmap, we just subscribe it's name to its policy.
(defn subscribe-units! [units policystore]
  (->> units
       (group-by #(plcy/policy-name (get % :policy)))
       (reduce-kv (fn [scripts pol us]
                    (let [us (r/map #(get % :name) us)]
                      (assoc scripts pol 
                             (if-let [newscripts (get scripts pol)]
                               (into newscripts us)
                               (into #{} us)))))                    
                  (get policystore :subscriptions))
       (assoc policystore :subscriptions)))

;;#TODO implement mutable versions for register-units! and subscribe-units!

;;At the highest level, we have to thread a supply around 
;;and modify it, as well as a policystore.
(defn process-units! [raw-units ctx]
  (assert false "use persistent version of process units for now!")
    ;; (core/with-simstate [[parameters behaviors] ctx]
    ;;   (core/with-cells [{supplystore   [:state :supplystore]
    ;;                      policystore   [:state :policystore]
    ;;                      subscriptions [:state :policystore :subscriptions]
    ;;                      unitmap       [:state :supplystore :unitmap]
    ;;                      unittags      [:state :supplystore :tags]
    ;;                      buckets       [:state :supplystore :deployable-buckets]
    ;;                      :as ctx2}         ctx]
    ;;     (let [newctx  (update-ctx2!
    ;;                    (core/with-transient-cells [unittags subscriptions buckets]
    ;;                      (-> (reduce (fn [acc unit]
    ;;                                    (process-unit! unit nil parameters behaviors supplystore policystore acc))
    ;;                                  ctx2 
    ;;                                  raw-units))))]
    ;;       (core/with-simstate [[supplystore policystore] newctx]
    ;;         (core/merge-entity 
    ;;          {:PolicyStore (subscribe-units! (vals (get supplystore :unitmap)) policystore)}
    ;;          newctx)))))
    )              

 ;;Are there any abstractions we can level?  Any patterns that are
  ;;becoming apparent?

  ;;We typically have a set of batch updates that work with cellular
  ;;members and (possibly) the context.

  ;;Can we create cells lazily?
  ;;For a structure...we could pack cells into the meta..
  ;;that's another option...
  ;;Another nice thing would be propogating cells..or detecting if
  ;;something is cellular...if we operate on any of its elements, maybe
  ;;we promote them to cells?

  ;;It'd be nice to just allow the caller to operate on the structure as
  ;;if it were a persistent structure, but have the cellular
  ;;optimizations available on the side...

  ;;Right now, I'm calling out specific bits or paths that can be
  ;;operated on, then defining (possibly duplicate) functions that
  ;;operate on the cells and don't actually bother to repack them
  ;;into a persistent structure.

  ;;It would be nice to have a generic, composable method for
  ;;operating in a cellular manner.

  ;;Can we do this with monads?
  ;;For instance, define cellular operations that expect
  ;;a cellular environment.

                                        ;(with-cells [[]])




  ;;- WIP
  ;;An alternative is to call register-units and subscribe-units
  ;;separately...makes a bit more sense to me from the FP perspective.
  ;; (defn process-units [raw-units ctx]
  ;;   (core/with-simstate [[parameters behaviors] ctx]
  ;;     (-> ctx
  ;;         (supply/register-units supplystore behaviors)
  ;;         (plcy/subscribe-units 
  ;;     (-> (reduce (fn [acc unit]
  ;;                   (process-unit! unit nil parameters behaviors supplystore policystore acc))
  ;;                 ctx2 
  ;;                 raw-units)
  ;;         (update-ctx2!)))))


  ;;Flesh this out, high level API for adding units to supply.  
  ;;Convenience function.
  ;;We really want to add prepped units.

  ;; (defn add-units [amount src oititle compo policy supply ghost ctx]
  ;;   (if (== amount 1))

  ;;#Entity Initialization

;;Start state is another one that we can possibly alter.   
(defn start-state [supply ctx]
  (core/with-simstate [[parameters] ctx]
    (reduce-kv (fn [acc nm unit] (unitsim/change-state unit :spawning 0 0 ctx) ;;all this does is an instant update in spawning
                 )
               (core/set-parameter ctx :TotalUnits (count (:unitmap supply)))
               (:unitmap supply))))

(comment ;testing -- incorporated in testing.clj, auto
  (require '[marathon.sim.sampledata :as sd])
  (require '[clojure.test :as test :refer [deftest is]])
  
  (def testctx  (assoc-in core/emptysim [:state :parameters :SRCs-In-Scope] {"SRC1" true "SRC2" true "SRC3" true}))
  (def debugctx (assoc-in core/debugsim [:state :parameters :SRCs-In-Scope] {"SRC1" true "SRC2" true "SRC3" true}))

  (def demand-records    (sd/get-sample-records :DemandRecords))
  (def ds       (demands-from-records demand-records testctx))
  (def first-demand (first ds))
  (def tstart   (:startday first-demand))
  (def tfinal   (+ tstart (:duration first-demand)))
  (def res      (demand/register-demand  first-demand testctx))
  (def dstore   (core/get-demandstore res))
  (deftest scheduled-demand-correctly 
    (is (= ((juxt  :startday :duration) first-demand)
           [ 901 1080])
        "Sampledata should not change.  Naming should be deterministic.")
    (is (= (first (demand/get-activations dstore tstart))
           (:name first-demand))
        "Demand should register as an activation on startday.")
    (is (= (first (demand/get-deactivations dstore tfinal)) (:name first-demand)) 
        "Demand should be scheduled for deactivation")
    (is (zero? (sim/get-time res)) "Simulation time should still be at zero.")
    (is (== (sim/get-next-time res) tstart) "Next event should be demand activation")
    (is (== (sim/get-next-time (sim/advance-time res)) tfinal) "Next event should be demand activation"))
  (def earliest (reduce min (map :startday ds)))
  (def latest   (reduce max (map #(+ (:startday %) (:duration %)) ds)))


;  (def multiple-demands (demand/register-demands! ds testctx))
;;slow way...
;  (def multiple-demands (reduce #(demand/register-demand %2 %1) testctx ds))
  (def multiple-demands (demand/register-demands! ds testctx))

  (def m-dstore (core/get-demandstore multiple-demands))
  (def times    (map sim/get-time (take-while spork.sim.agenda/still-time? (iterate sim/advance-time multiple-demands))))
  (def events   (spork.sim.data/event-seq multiple-demands))
  (def expected-events (list {:time 0, :type :time} {:time 1, :type :time} {:time 91, :type :time} {:time 181, :type :time} 
            {:time 271, :type :time} {:time 361, :type :time} {:time 451, :type :time} {:time 467, :type :time} 
            {:time 481, :type :time} {:time 523, :type :time} {:time 541, :type :time} {:time 554, :type :time} 
            {:time 563, :type :time} {:time 595, :type :time} {:time 618, :type :time} {:time 631, :type :time} 
            {:time 666, :type :time} {:time 721, :type :time} {:time 778, :type :time} {:time 811, :type :time} 
            {:time 901, :type :time} {:time 963, :type :time} {:time 991, :type :time} {:time 1048, :type :time} 
            {:time 1051, :type :time} {:time 1081, :type :time} {:time 1261, :type :time} {:time 1330, :type :time} 
            {:time 1351, :type :time} {:time 1441, :type :time} {:time 1531, :type :time} {:time 1621, :type :time} 
            {:time 1711, :type :time} {:time 1801, :type :time} {:time 1981, :type :time} {:time 2071, :type :time} 
            {:time 2095, :type :time} {:time 2341, :type :time} {:time 2521, :type :time}))
  (def activations481 (demand/get-activations m-dstore 481))
  (deftest scheduled-demands-correctly 
    (is (= times
           '(0 1 91 181 271 361 451 467 481 523 541 554 563 595 618 631 666 721 
               778 811 901 963 991 1048 1051 1081 1261 1330 1351 1441 1531 1621 1711 1801 1981 2071 2095 2341 2521))
        "Scheduled times from sampledata should be consistent, in sorted order.")
    (is (= events expected-events)           
        "The only events scheduled should be time changes.")
    (is (= activations481 
           '("1_R29_SRC3[481...554]" "1_A11_SRC2[481...554]" "1_Vig-ANON-8_SRC1[481...554]"))
        "Should have actives on 481...")       
    (is (some (fn [d] (= d (:name first-demand))) (demand/get-activations m-dstore tstart))
        "Demand should register as an activation on startday.")
    (is (zero? (sim/get-time multiple-demands)) "Simulation time should still be at zero.")
    (is (== (sim/get-next-time multiple-demands) earliest) "Next event should be demand activation")
    (is (= (last times) (:tlastdeactivation m-dstore))
        "Last event should be a deactivation time.")) 


  ;;we can't build supply without policy....initializing supply with
  ;;an understanding of policy...

  (def pstore            (core/get-policystore testctx ))
  (def supply-records    (sd/get-sample-records :SupplyRecords) pstore)
  (def us                (units-from-records supply-records testctx))
  (def first-demand      (first ds))
)


(comment ;testing
  (require '[spork.util.reducers])
  (require '[clojure.core.reducers :as r])

  ;;much faster....more efficient.
  (def xs (r/range 1000000))
;  (def xs (vec (range 1000000)))

  (defn slow-test []
    (time (dotimes [i 1] 
            (core/with-cells [{dmap [:demandmap] 
                               :as state}         dstore] 
              (let [newmap  (reduce (fn [acc n] (assoc acc n n)) dmap xs)] (update-state!))))))

  (defn fast-test []    
    (time (dotimes [i 1] 
            (core/with-cells [{dmap [:demandmap] 
                               :as state}         dstore] 
              (let [newmap (core/reset-cell! dmap 
                              (persistent! 
                               (reduce (fn [acc n] (assoc! acc n n)) 
                                 (transient @dmap) xs)))] (update-state!))))))


  ;;This costs us a bit, because we're not slamming directly on the transient.
  (defn fast-test! []    
    (time (dotimes [i 1] 
            (core/with-cells [{dmap [:demandmap] 
                               :as state}         dstore] 
             (core/with-transient-cells [dmap]
               (reduce (fn [acc n] (assoc acc n n)) 
                       dmap  xs)) 
             (update-state!)))))

  (defn add-demand [dstore demandname d]
    (core/with-cells [{dmap          [:demandmap]
                       activations   [:activations]
                       deactivations [:deactivations]
                       :as state}                     dstore]
      (do (assoc dmap demandname d)
          (assoc activations   (:startday d) demandname)
          (assoc deactivations (+ (:startday d) (:duration d)) demandname)
          (update-state!))))

  (defn add-demand! [dmap activations deactivations  d]
    (let [n (get d :name)
          start (get d :startday)]
      (do (core/swap-cell!  dmap          assoc! n  d)
          (core/swap-cell!  activations   assoc! start n)
          (core/swap-cell!  deactivations assoc! (+ start (get d :duration)) n))))

  (defn add-demand!! [dmap activations deactivations  d]
    (let [n (get d :name)
          start (get d :startday)]
      (do (core/swap-cell!  dmap          core/assoc-any n  d)
          (core/swap-cell!  activations   core/assoc-any start n)
          (core/swap-cell!  deactivations core/assoc-any (+ start (get d :duration)) n))))

  (defn add-demand!!! [dmap activations deactivations  d]
    (let [n (get d :name)
          start (get d :startday)]
      (do (assoc  dmap           n  d)
          (assoc  activations    start n)
          (assoc  deactivations  (+ start (get d :duration)) n))))

  (defn test-demands [n] (r/map (fn [n] {:name n :startday n :duration  55}) (r/range n)))
  (defn add-demands! [dstore ds]
    (core/with-cells [{dmap          [:demandmap]
                       activations   [:activations]
                       deactivations [:deactivations]
                       :as state}                      dstore] 
      (do (core/with-transient-cells [dmap activations deactivations]
            (reduce (fn [acc d] 
                      (add-demand!!! dmap activations deactivations d))
                    nil ds))
            (update-state!))))

  ;;nested cell defs also work fine.
  (defn add-demands!! [dstore ds]
    (core/with-cells [{dmap          [:demandmap]
                       activations   [:activations]
                       deactivations [:deactivations] 
                       :as state1}              dstore]
      (core/with-cells [{dmap          [:demandmap]
                         activations   [:activations]
                         deactivations [:deactivations]
                         :as state2}            state1]                                 
        (do (core/with-transient-cells [dmap activations deactivations]
              (reduce (fn [acc d] 
                        (add-demand!!! dmap activations deactivations d))
                      nil ds))
              (update-state2!)))))


;;can we define a macro that uses the cell infrastructure? 
;;i.e. a cell reducer?  or a cell transducer? 
;;A lot of these update steps are consistent: 
;;we acquire a temporary bit of cellular storage from the 
;;context.
;;We define operations that modify the storage.
;;We compose these operations into storage updates 
;;At the end of a storage update, we pack modified 
;;storage into the persistent data structure.

  ;; (defn add-demand [dstore d]
  ;;   (add-demands! dstore [d]))

  (defn add-demands [dstore ds]
    (reduce (fn [acc d] 
              (add-demand acc (:name d) d))  dstore ds))      
)


;;another option is to identify bulk operations 
;;center around the data that's being changed (i.e. may be optimized
;;for mutation) and make it easier to express how to unpack and repack 
;;the data.  something like with-transient-locations...

;; (comment 
;;   (defn load-demands! 
;;     ([record-source ctx]
;;        (let [rs    (demands-from-records (core/as-records record-source) ctx)
;;              dupes (get (meta rs) :duplicates)]
;;          (->> (reduce (fn [acc d]                       
;;                         (-> (associate-demand acc d)
;;                             (initialized-demand!  d)))
;;                       ctx rs)
;;               (notify-duplicate-demands! dupes))))
;;     ([record-source demandstore ctx] 
;;        (load-demands record-source (core/set-demandstore ctx (make-mutable! demandstore)))))

;;    ...
;;     ([rs ctx]
       
;;        (reduce (fn [acc d]                       
;;                  (-> (associate-demand acc d) ;mutation heavy...
                     
;;                      (initialized-demand!  d) ;independent....
;;                      ))
;;                ctx rs)

;;     ([rs ctx]
;;       (core/with-simstate [[parameters demandstore policystore] ctx]
;;         (let [idx     (or (:demandstart parameters) 0)
;;               demands (:demandmap demandstore)              
;;               transient-demands (transient demands)]
;;           (reduce (fn [ctx demand]                       
;;                     (->  ;mutation heavy...
;;                      (let [demand-count   (count demands)
;;                            new-idx        (+ idx  demand-count)
;;                            new-demand     (-> (if (contains? demands (:name demand))
;;                                                 (assoc demand :name 
;;                                                        (str (:name demand) "_" (inc demand-count)))
;;                                                 demand)                     
;;                                               (assoc :index new-idx))]
;;                        (demand/register-demand new-demand demandstore policystore ctx)) ;mutation heavy...                                            
;;                        (initialized-demand!  d))) ;independent....                        
;;                   ctx rs))))
;;     ....
;;     ([rs ctx]
;;       (core/with-simstate [[parameters demandstore policystore] ctx]
;;         (let [idx     (or (:demandstart parameters) 0)
;;               demands (:demandmap demandstore)              
;;               transient-demands (transient demands)]
;;           (reduce (fn [ctx demand]                       
;;                     (->  ;mutation heavy...
;;                      (let [demand-count   (count demands)
;;                            new-idx        (+ idx  demand-count)
;;                            new-demand     (-> (if (contains? demands (:name demand))
;;                                                 (assoc demand :name 
;;                                                        (str (:name demand) "_" (inc demand-count)))
;;                                                 demand)                     
;;                                               (assoc :index new-idx))]
;;                        ;;if we had another interface for registering
;;                        ;;mutable demands, using a transient
;;                        ;;demand-map, then we're golden....
;;                        (demand/register-demand! new-demand demandstore policystore transient-demands ctx)) ;mutation heavy...                         
;;                        (initialized-demand!  d))) ;independent....                        
;;                   ctx rs)))))

;;Optimized versions for bulk loading information.
;; (defn associate-demand!     
;;   [ctx demand demand-idx demandstore policystore]
;;   (-> (if (contains? (:demandmap demandstore) (:name demand))
;;         (assoc demand :name 
;;                (str (:name demand) "_" demand-idx))
;;         demand)                     
;;       (assoc :index demand-idx)
;;       (demand/register-demand demandstore policystore ctx)))

;;Mutable version designed to work with refs.
;; (defn load-demand! 
;;   [ctx demand demandstore policystore idx-ref]
;;   (initialized-demand! (associate-demand! ctx demand (core/inc! idx-ref) demandstore policystore) demand))
