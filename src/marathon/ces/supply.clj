;;An implementation of the supply simulation used by Marathon.  
;;All the operations for pushing supply, in the context of a simulation are 
;;maintained here.  Backing structure is an entitystore.  Supporting
;;Entity is the supply manager.
(ns marathon.ces.supply
  (:require [marathon.demand [demanddata :as d] [demandstore :as dstore]]
            [marathon.supply [unitdata :as udata]]
            [marathon.ces [missing :as missing]
                          [core :as core] [policy :as policy] 
                          [unit :as u]]
            [spork.entitysystem.store :as store
             :refer [gete assoce mergee assoc-ine updatee get-entity add-entity drop-entity
                     update-ine update-entity get-ine]]
            [spork.ai.core :refer [debug]]
            [spork.sim    [simcontext :as sim] [updates :as updates]]
            [spork.util   [tags :as tag]
                          [general :as gen]]))

;;#Primitive Operations and Supply Queries
;'TODO -> formalize dependencies and pre-compilation checks....
;;estore version....
(defn can-simulate? [supply]
  (-> supply
      (:tags) 
      (tag/get-tags :enabled)
      (empty?)
      (not)))
;;no change for estore.
(defn unit-msg [unit]
  (str "Updated Unit " (:name unit) " " (udata/getStats unit)))

(defn ghost?     [tags unit] (tag/has-tag? tags :ghost (:name unit)))
;;estore version
(defn set-ghosts [x ctx]  (assoce :SupplyStore :has-ghosts x))

;;estore version
(defn add-unit
  ([supply store unit]
   (-> store       
       (add-entity (:name unit) unit)
       (store/mergee :SupplyStore (assoc-in supply [:unitmap (:name unit)] (:name unit)))))
  ([store unit] (add-unit (get-entity store :SupplyStore) store unit))) 

;might be able to ditch the unit-map entirely.
(defn drop-unit  [store unitname]
  (-> store
      (drop-entity unitname)
      (update-ine  [:SupplyStore :unitmap] dissoc unitname)))

;;this has changed....we no longer need the supplystore...
(defn get-unit [store name] ;(get-in supplystore [:unitmap  name]))
  (get-entity store name))

(defn unit? [store name] (get-in store [:unitmap name])) 

(defn has-behavior? [unit] (not (nil? (:behavior unit))))

;;#TODO  Generalize this.  We have a single case statement for
;;assigning behaviors.  Works okay, but it's kind of a choke point...
(defn assign-behavior
  [behaviors unit]
   (->>  (case (clojure.string/upper-case (:component unit))
           "AC" :ac
           ("NG" "RC") :rc
           "GHOST" :ghost 
           (throw (Exception. (str "Trying to assign behavior based on unknown component: " (:component unit)))))
         (assoc unit :behavior)))

(defn empty-position? [unit] (nil? (:positionpolicy unit)))

;;Note: these may become obsolete...

;;#Keyword Tag Builders
(core/defkey source-key "SOURCE_")
(core/defkey sink-key   "SINK_")
(core/defkey compo-key  "COMPO_") 
(core/defkey behavior-key "BEHAVIOR_")
(core/defkey title-key   "TITLE_")
(core/defkey policy-key  "POLICY_")
(def ghost-source-tag (source-key "Ghost"))

;;#Tag Operations

;'TOM Change 27 Sep 2012
;Adds meta data to the tags, to identify the unit as being a member of a fenced
;group of supply.  Fenced groups of supply automatically provide a special set 
;of supply that fill functions can utilize when making demand decisions.
(defn tag-as-fenced [tags fencegroup unitname]
  (-> (tag/tag-subject tags fencegroup unitname)
      (tag/tag-subject unitname :fenced)))        

;'TOM Change 27 Sep 2012 -> using tags to delineate fence states, including 
;one-time fences, specifically for future force gen stuff.
(defn drop-fence [tags unitname]
  (if (and (tag/has-tag? tags :one-time-fence unitname) 
           (tag/has-tag? tags :fenced unitname))
    (-> (tag/untag-subject tags unitname :fenced)
        (tag/tag-subject unitname :dropped-fence))
    tags)) 
;default unit tags
(defn default-tags [{:keys [src component behavior oi-title policy]}] 
  [(compo-key component) (behavior-key behavior) (title-key oi-title) 
   (policy-key (:name policy)) (source-key src) :enabled])

;Note -> we should generalize this into some special.  Like deftag, or 
;defsupply tag, which looks at a library of tags to find out if it should do
;any special processing.  
(defn tag-extras [unit extras tags]
  (let [unitname (:name unit)]
    (reduce (fn [tags tg] 
              (case tg 
                :fenced (tag-as-fenced tags tg unitname)
                :keep-fenced (tag/tag-subject tags unitname :one-time-fence)
                (tag/tag-subject tags unitname tg))) tags extras)))
                            
;register source as being a member of sources, so it can be looked at when 
;filling supply.
(defn tag-source [source tags] (tag/tag-subject tags source :sources))

;inject appropriate tags into the supply tags.
(defn tag-unit
  ([supply unit extra-tags]
   (let [sourcename (source-key (get unit :src))]
      (->> ; (into (default-tags unit) extra-tags)) 
           (tag/multi-tag (get  supply :tags) (get unit :name) (default-tags unit))
           (tag-source sourcename)
           (tag-extras unit extra-tags)
           (assoc supply :tags))))
  ([supply unit] (tag-unit supply unit nil)))

;helper function for dropping a tag from multiple units at once.
(defn untag-units [supplystore tag units]
  (reduce #(tag/untag-subject (:tags %1) %2 tag) supplystore units))

;;#Supply Population Operations
;this might be suitable to keep in the supplymanager...
(defn add-src [supply src] 
  (let [scoped (:srcs-in-scope supply)]
    (if (contains? scoped src) supply 
      (assoc-in supply [:srcs-in-scope src] (count scoped)))))

(defn remove-src [supply src] (update-in supply [:srcs-in-scope] dissoc src))

;;Operations on units like add/remove happen at a high level, since the
;;supply store, via the unit-map, served as the logical "container" for
;;all the unit entities.

;;There might be a general container entity type...
;;If an entity is contained by another, then we want
;;to remove all of its relations.
;;Perhaps we can have a generalized relational setup.
;;As we create entities, we could specify relationships.
;;If an entity is removed, it's no longer contained by
;;any dependent entities.  Rather than manually managing
;;this stuff, we can let the framework do it.  Or we can
;;define supplemental functions that help us.

;;For instance, removing demands is probably identical to
;;removing units.

;DOUBLE CHECK....do we really mean to drop the entire src? 
(defn remove-unit [store unitname]
  (-> store
      (drop-unit unitname)
      (update-entity :SupplyStore remove-src (gete store unitname :src))
      ))

(defn get-buckets 
  ([supply bucket] (get (:deployable-buckets supply) bucket))
  ([supply]        (get (:deployable-buckets supply) :default)))

(defn add-bucket [supply bucket-name]
  (let [buckets (:deployable-buckets supply)]
    (if (contains? buckets bucket-name) supply 
      (assoc-in supply [:deployable-buckets bucket-name] {}))))

;;#Tag-Related Supply Queries
(defn get-sources [supply] (tag/get-subjects (:tags supply) :sources))
(defn multiple-ghosts? [supplytags]
  (> (count (tag/get-subjects supplytags ghost-source-tag)) 1))

;procedure that allows us to, using the fillgraph, derive a set of tags whose 
;associated units should be deactivated.  if removal is true, the units will be
;removed from memory as well. in cases where there are a lot of units, this may 
;be preferable.
(defn scope-supply [supply disable-tags & [removal]]
  (let [f (if removal remove-unit (fn [s u] s))]
    (->>  (tag/and-tags (:tags supply) (set disable-tags)) 
          (reduce (fn [s u] (-> (core/disable s u) f u)) supply))))


;----FOREIGN -> THESE SHOULD BE MOVED, THEY'RE MORE GENERAL.....
(defn conj-policy [unit policy] (update-in unit [:policy-queue] conj policy))

;came from OpFactory..
(defn create-unit [name src title component cycletime policy-id 
                    parameters policystore & [behavior]]
  (let [p (missing/choose-policy policy-id component parameters policystore src)]
    (-> {:name name :src src  :oi-title title :component component 
         :behavior behavior   :cycletime cycletime :policy p}
        (udata/map->unitdata))))
;----------END FOREIGN------


;;#Supply Updating
;;The supply simulation has many concurrent process - unit simulations - 
;;that are in various states of motion.  For efficiency, we treat these 
;;processes as relatively independent, with the exception of periodic 
;;synchronization via updating.  The supply system updates either specific 
;;entities, or all entities, by evaluating the passage of time relative to the 
;;entity.  Certain functionality, such as sampling and logging, may require 
;;the entire supply to be synchronized, and thus updated to a common point in 
;;time.  For the most part, the supply will proceed eventfully, with each 
;;process updating on an as-needed basis.

;;__TODO__ move up-to-date? to the simcontext library.
;;This is a pretty general function, we can probably elevate to the core or to
;;sim context.
;;CES version...
;;we can change sim/last-update to be a general function.
;;store/last-update
;;This is generic...we can check to see if any entity is up to date by
;;examining its update component.
(defn up-to-date? [t ctx unitname]
  (= (gete ctx unitname :last-update) t))

(defn supply-update! "Notifies the context of a supply update."
  [supply unit msg ctx]
  (sim/trigger-event :supplyUpdate (:name supply) (:name unit) msg nil ctx))

;;we wrap this accessor behind a entity called updates.
(defn get-supply-updates "Get all units with pending supply updates." 
  [t ctx]
  ;;transition this out...
                                        ;  (get-ine ctx [:updates t :supply-update]
  (sim/get-updates :supply-update t ctx
           ))

(defn request-unit-update! "Schedule an update for unit at time t." 
  [t unit ctx] 
  (sim/request-update t (:name unit) :supply-update ctx))

;;we can just get the entity here...

;;Note: We can factor this out using filter in update-units.
(defn apply-update
  "Ages an individual unit, based on how much time has elapsed - for the unit -
   between time t and its last update."
  [t supplystore unitname ctx]
  (if ;(core/disabled? supplystore unitname)
       (gete ctx unitname :disabled) ;lame hack
            (do (println [:update unitname t :disabled])
                ctx)
      (let [unit (get-unit ctx unitname)
            ;_ (println [:updating unitname])
            ]
          (->> ctx       
               (u/unit-update unit) ;(updates/elapsed t (or (sim/last-update unitname ctx) 0)))
               (supply-update! supplystore unit (unit-msg unit))))))

(defn update-units
  "Given a sequence of unit keys, xs, brings each unit up to date according to 
   day, relative to the supply and the simulation context."
  [t supply ctx xs]       
  (reduce (fn [acc x] (apply-update t (core/get-supplystore acc) x acc))  
          ctx xs))

(defn update-all
  "Forces an update for every unit in the supply to bring all entities to a 
   common point in time.  Typically used prior to sampling."
  [t supply ctx & [unitnames]]
  (->> (or unitnames (keys (get supply :unitmap)))
       (filter (partial up-to-date? t ctx))
       (update-units t supply ctx)))

;;#General Supply Notifications

(defn spawning-unit! [unit ctx]
  (sim/trigger-event :SpawnUnit (:name unit) (:name unit)
     (str "Spawned Unit " (:name unit)) nil ctx))             

(defn spawning-ghost! [unit ctx]
  (sim/trigger-event :SpawnGhost (:name unit) (:name unit)
     (str "Spawned a ghost " (:name unit)) nil ctx))  

(defn new-deployable! [unit ctx]
  (assert (not= (:policy-position unit) :Recovery) "Recovery is not deployable")
  (sim/trigger-event :NewDeployable "SupplyManager" (:name unit) 
      (str "Unit " (:name unit) " at position " (:positionpolicy unit) 
           " is deployable") nil ctx))
(defn new-followon! [unit ctx] 
  (sim/trigger-event :NewFollowOn "SupplyManager" (:name unit)
      (str "Unit " (:name unit) " able to followon for demandgroup " 
           (:followoncode unit)) nil ctx))
(defn more-src-available! [unit ctx]
  (let [src (get unit :src)]
    (sim/trigger-event :MoreSRCAvailable "SupplyManager" src 
       (str "Unit " (:name unit) " at position " (:positionpolicy unit) 
            "has just been added to deployables for SRC " src) nil ctx))) 
(defn new-src-available! [src ctx]
  (sim/trigger-event :NewSRCAvailable "SupplyManager" src 
     (str "A new category of SRC now has deployable supply " src) nil ctx))
(defn not-deployable! [unit ctx] 
  (sim/trigger-event :NotDeployable "SupplyManager" (:name unit) 
     (str "Unit " (:name unit) " at position " (:positionpolicy unit) 
          " is no longer deployable") nil ctx))
(defn out-of-stock! [src ctx]
  (sim/trigger-event :outofstock "SupplyManager" src 
     (str "SRC " src " has 0 deployable supply") (source-key src) ctx))

;Aux function for logging/recording the fact that a unit changed locations
(defn log-move!
  ([t fromloc toloc unit duration ctx]
   (sim/trigger-event :unitMoved (:name unit) toloc (core/msg (:name unit) " moved from " fromloc " to " toloc) unit ctx))
  ([t fromloc toloc unit ctx] (log-move! t fromloc toloc unit nil ctx)))

;TODO -> This should be renamed like positionEvent or something.
;Main dependencies are in the unit Behaviors.
;Unit behaviors currently use parent to refer to a supply manager.
;We can probably do better than this.
;Actually, unit behaviors aren't maintaining any state....
;So we can probably just plug them in as modules....they're all pure functions.
;'TOM Change 6 June 2011 -> Added logging for unit positioning specifically..
(defn log-position! [t frompos topos unit  ctx]
  (sim/trigger-event :PositionUnit "SupplyManager" (:name unit) 
     (core/msg "UIC " (:name unit) " has repositioned from " frompos " to " topos)
     nil ctx))

;Aux function for logging/recording the fact that a unit deployed
(defn log-deployment! 
  [t fromname demand unit fillcount filldata deploydate  period ctx]
  (sim/trigger-event :deploy "SupplyManager" (:name unit)              
     (core/msg "Deployed unit " (:name unit) 
          " from " fromname " to demand " (:name demand))
     {:fromloc   fromname  :unit unit :demand demand :fill filldata 
      :fillcount fillcount :period period :t t :deploydate deploydate}  ctx))

;When a unit engages in a followon deployment, we notify the context.
(defn unit-followon-event! [unit demand ctx]
  (sim/trigger-event :FollowingOn  (:name unit) (:name demand) 
     (core/msg "Unit " (:name unit) " is following on to demand " (:name demand))
        nil ctx))

(defn first-deployment! [supply unit ctx]
  (sim/trigger-event :firstDeployment (:name supply) (:name supply) 
       (core/msg "Unit " (:name unit) " Deployed for the First Time") nil ctx))



;;#Supply Availability


;;#TODO revamp our data model to store sets of unit names, rather than
;;map of name->unit.  This, again, fits in nicely with tags if we
;;centralize our queries off a tag data model.  It's more legible for
;;interactive debugging if we just see the names (note: I can write a
;;view that will accomplish the same thing; in some cases it might be 
;;preferable to retain the name->unit info....pending.
(defn update-availability [unit supply ctx]
  (if (contains? (get-buckets supply) (get unit :src))
    (more-src-available! unit ctx)
    (->> (new-src-available! (get unit :src) ctx)
      (new-deployable! unit))))

(defn derive-bucket [unit]
  (let [fc  (:followoncode unit)]
    (if (and fc (not (or (= fc "") (= fc "UnGrouped"))))
      fc
      :default)))

;;Rather than specifying followons manually, we let them be derived from
;;the unit's followon code.  If it has one, it's inferred we have a
;;followon supply.  We store this information in the unit's bucket component.
;;Consolidated this from update-deployability, formalized into a function.
;;We also indicate the presence of followon units at the component level,
;;rather than storing in the supply.
(defn add-deployable-supply 
  ([supply bucket src unit ctx]
   (let [components {:deployable-bucket bucket
                     :deployable-cat    src
                     :deployable true}
         components (if (identical? bucket :default) components
                        (assoc components :followon bucket))
         ;_ (println [(:name unit) components])
         ]         
   (->> ctx
        (sim/merge-entity {:SupplyStore (assoc-in supply [:deployable-buckets bucket src (:name unit)] unit)
                           (:name unit) components ;;tacking on component data to help with queries.
                           })
        (update-availability unit supply))))
  ([supply src unit ctx]                                                                  
   (add-deployable-supply supply
                          (derive-bucket unit) src unit ctx)))

;;follow on supply was treated as special, but now it's not.  We expected 
;;a function called get-followon-supply before...
;;I think it's OBE now...
;;__TODO__ Determine if get-followon-supply is OBE, and that can ignore it.

;Consolidated this from update-deployability, formalized into a function.
(defn remove-deployable-supply 
  ([supply bucket src unit ctx]
   (let [ctx (store/update-entity ctx (:name unit)
                                  (fn [m] (-> m (dissoc :deployable-bucket)
                                                (assoc  :deployable false))))] ;no longer deployable, tracking with component data.
     (if-let [newstock (-> (get-in supply [:deployable-buckets bucket src])
                           (dissoc (get unit :name)))]
       (sim/merge-entity  {:SupplyStore (assoc-in supply [:deployable-buckets bucket src] newstock)} ctx)
       (->>  (sim/merge-entity  {:SupplyStore (update-in supply [:deployable-buckets bucket] dissoc src)} ctx) ;we can phase this out maybe
             (out-of-stock! (get unit :src))))))
  ([supply src unit ctx] (remove-deployable-supply supply :default src unit ctx)))

(defn update-deployability
  "Sets a unit's deployable status, depending on the current context and the 
   unit's policy state."
  ([supply unit followon spawning ctx]
;     (assert (not (empty-position? unit)) (core/msg "invalid position!" (:positionpolicy unit)))
   (let [position (:positionpolicy unit)         
         src      (get unit :src)
         can-deploy (u/can-deploy? unit spawning)]
       (if (or followon can-deploy)                         ;1)
         (->> (if followon  ;notifiying of followon data...
                (new-followon!   unit ctx) 
                (new-deployable! unit ctx))
              (add-deployable-supply supply src unit)) ;add stuff to buckets...   
                                        ;unit is not deployable
         (->> (not-deployable! unit ctx)
              (remove-deployable-supply supply src unit)))))
  ([unit followon spawning ctx] 
     (update-deployability (core/get-supplystore ctx) unit followon spawning ctx)))

;;__TODO__Determine if we can yank this and just use update-deployability.
;;We used to have this as a firewall that would, depending onthe unit's followon 
;;status, select from an alternate set of supply buckets.  Since we're tracking 
;;supply status via tags now, we don't need to partition the buckets separately.
;;I removed the buckets args, and this function got hollowed out.  
;;__DEPRECATE__
(defn update-deploy-status
  ([supply unit followon? spawning? ctx]
   (update-deployability supply unit followon? spawning? ctx))
  ([unit followon? spawning? ctx]
   (update-deployability unit followon? spawning? ctx)))

;;#Registering New Supply


;Note -> the signature for this originally returned the supply, but we're not 
;returning the context.  I think our other functions that use this guy will be
;easy to adapt, just need to make sure they're not expecting supplystores.
;Conjoins a unit to the supply, under the context.  Optional parameters for 
;communicating whether the unit is a ghost, as well as additional tags to be 
;added on-top-of the default tags derived from the unit data.
(defn register-unit [supply behaviors unit ghost extra-tags ctx]
  (let [unit   (if (has-behavior? unit) unit (assign-behavior behaviors unit))
        newctx    (->> (-> supply
                           (tag-unit unit extra-tags)
                           (add-src   (get unit :src))
                           (add-unit  ctx unit)
                           (store/assoce (:name unit) :supply true) ;starting to shift to component tagging.                           
                           )
                       (request-unit-update!  (max (:spawntime unit) 0) unit ))        
        ]
    (if ghost 
      (->> (spawning-ghost! unit newctx)
           (set-ghosts true))
      (->> (spawning-unit! unit newctx)
           (update-deployability unit nil nil)))))

(defn register-unit! [supply behaviors unit ghost extra-tags ctx]
  (let [unit   (if (has-behavior? unit) unit (assign-behavior behaviors unit))
        supply (-> (add-unit supply unit)
                   (tag-unit unit extra-tags)
                   (add-src (get unit :src)))]
    (if ghost 
      (->> (spawning-ghost! unit ctx)
           (set-ghosts true))
      (->> (spawning-unit! unit ctx)
           (update-deployability supply unit nil nil)))))

;creates a new unit and stores it in the supply store...returns the supply 
;store.
(defn new-unit [supplystore parameters policystore behaviors name src title 
                component cycletime policy & [behavior ctx]]
  (let [new-unit (create-unit name src title component cycletime policy 
                              parameters policystore behavior)]
    (register-unit supplystore behaviors (ghost? new-unit) ctx)))
                                           
;;__TODO__ DEPRECATE
;NOTE -> replace this with a simple map lookup...
(defn get-near-max-policy [policy policystore]
  (let [id (case (policy/atomic-name policy) 
             :MaxUtilization         :NearMaxUtilization
             :MaxUtilization_Enabler :NearMaxUtilization_Enabler)]
    (get-in policystore [:policies id])))

;WEAK...hard coded, should be data driven.
(defn should-change-policy? [{:keys [component] :as unit}]
  (not= component :AC :Ghost))
(defn check-max-utilization [params] (get params :TAA1519MaxUtilizationHack))

;This is an aux function that serves as a weak patch...probably unnecessary.
;;__TODO__Remove the need for adjust-max-utilization!
(defn adjust-max-utilization! [supply unit ctx] 
  (if (and (check-max-utilization (core/get-parameters ctx))
           (should-change-policy? unit))
    (let [new-policy (get-near-max-policy (:policy unit) 
                                          (core/get-policystore ctx))]
      (update-entity ctx (:name unit) conj-policy new-policy))                                                
    ctx))

;;#Tracking Follow-On Supply

;;#Alteration 
;;I'm changing the existing scheme of storing followon unit
;;information, as well as non-followon, or generic unit information.
;;One (still) tempting idea is to just shove everything into tags, 
;;and use the tag annotation to execute queries.  We're heading
;;towards components at that point (which IS the right direction).
;;For now, a bridging strategy is to unify the buckets and followons, 
;;basically making a standard bucket, :default, and having everything
;;else be inferred as "non-generic" (currently meaning followon, or 
;;associated with some demand group).
;;Note# 
;;There is a general relation here...the fact is that the unit is
;;related to some element of demand...either the demandgroup, or
;;something else.  That makes tags an interesting prospect....
;;Further, if we shift to using tags to denote relationship facts, 
;;we get a consistent API, an option for batched mutable updates, and 
;;other features.  We have more facts associated with a unit than
;;merely its followon status....right now we're tracking lots of extra
;;state when it's simpler to just use tags....

;The call for update-deploy-status is UGLY.  Might be nice to use keyword args..
(defn add-followon
  "Registers the unit as eligible for follow on status."
  [supply unit ctx] 
                                        ;(-> ;(assoc-in supply [:followons (get unit :name)] unit)      
      (update-deploy-status unit true nil ctx))

(defn remove-followon
  "Drops the supply entity from supply store's registry of units in follow-on 
   status."
  [store unitname]
  (let [unit     (get-unit store unitname)
        fcode    (:followoncode unit)
        src      (get unit :src)]
    (-> store 
        (store/dissoce  unitname :followon)
        (core/prune-in  [:deployable-buckets fcode src] dissoc unitname)
        (assoc-in  [:unitmap unitname] 
          (assoc unit :followoncode nil)))))

(defn followon-unit?
  "Determines if a particular unit is known to be eligible for follow-on use."
  [store unit] (contains? (:followons store) (get unit :name)))

;;__TODO__Detangle release-followon-unit.

;;Relook this, I think we can manage the same effects much simpler.
(defn release-followon-unit
  "Convoluted.  Need to detangle this guy. 
   Assuming a unit associated with unitname was held in follow-on status, the 
   unit is released from holding and allowed to progress back into the global 
   supply."
  [ctx unitname]
  (let [_     (println [:releasing unitname :followon])        
        ctx   (->> ctx ;(update-entity ctx :SupplyStore remove-followon unitname)                   
                   (u/change-state unitname 
                                   :AbruptWithdraw 0 nil))
        ;store 
        ]   
    (update-deploy-status 
     (core/get-supplystore ctx)
     (store/gete ctx unitname)  nil nil ctx)))


;;Process the unused follow-on units, changing their policy to complete cycles.
(defn release-followons [fons ctx]
  (as->   (store/drop-domain ctx :followon) ctx
    (reduce release-followon-unit ctx (keys fons))))

;;__TODO__ Deprecate release-max-utilizers
;;This is probably a deprecated function.  It was a corner case to ensure that 
;;we handled a special class of follow on units, who followed a special max 
;;utilization policy.  We can probably replace it with something more general.
(defn release-max-utilizers [supplystore & [ctx]]
  (let [{:keys [followons normal]} 
           (group-by #(if (followon-unit? supplystore %) :followon :normal)
                      (tag/get-subjects (:tags supplystore) :MaxUtilizer))
         updates {:SupplyStore (untag-units supplystore :MaxUtilizer 
                                            (concat followons normal))}]   
    (reduce release-followon-unit (sim/merge-entity updates ctx)
            followons))
  )


;;#Deployment Related

;;announce that the unit is in fact following on, remove it from followons.
(defn record-followon [supply unit demand ctx]
  (->> (sim/merge-entity 
         {:supplystore (remove-followon supply (get unit :name))} ctx)
       (unit-followon-event! unit demand)))

(defn get-next-deploymentid [s] (inc (:uniquedeployments s)))
(defn tag-as-deployed [unit store] 
  (update-in store [:tags] tag/tag-subject (get unit :name) :hasdeployed))
                                         
;NOTE -> this should be OBSOLETE after the port, since we can handle policies 
;much more gracefully.
;Jeff had me put in some special case for handling initial deployment logic.
(defn first-deployment? [unit store]
  (not (tag/has-tag? (:tags store) (get unit :name) :hasdeployed))) 

;;##Supply Management
;;the supply system queries the entity store to find entities with
;;supply updates.
;;From here ,it updates each entity in turn; really it invokes the
;;unit-update system.
(defn manage-supply
  "High level hook for the supply system.  For entities that have scheduled 
   updates at time t, they are brought up to date and have their changes 
   incorporated into the context.  The entity behaviors will typically 
   use some of the supply system functions defined above to alter the context."
  ([t ctx]
   (let [supply (get-entity ctx :SupplyStore)
         ;_ (println [:supply t])
         ]
     (if-let [today-updates (keys  (get-supply-updates t ctx))]
       (update-units t supply ctx today-updates)
       ctx)))
  ([ctx] (manage-supply (core/get-time ctx) ctx)))


;; (defn manage-supply
;;   "High level hook for the supply system.  For entities that have scheduled 
;;    updates at time t, they are brought up to date and have their changes 
;;    incorporated into the context.  The entity behaviors will typically 
;;    use some of the supply system functions defined above to alter the context."
;;   [t ctx]
;;   (let [supply (core/get-supplystore ctx)]
;;     (if-let [today-updates (map :requested-by (get-supply-updates t ctx))]
;;       (update-units t supply ctx today-updates)
;;       ctx)))

;;A simple wrapper to unify the high level supply management.  We were calling 
;;this inline, it's more consistent now.
(defn manage-followons
  "Ensures that entities held in a temporary follow-on status are released and
   circulated back into supply.  Typically used after we try to fill demands."
  [day ctx]
  (let [fons (store/get-domain ctx :followon)
        fcount (count fons)]
    (if (pos? fcount)
      (do  (debug [:releasing! fcount :followon])
           (release-followons fons ctx))
      (do (debug [:No-followons-to-release!])
          ctx))))

