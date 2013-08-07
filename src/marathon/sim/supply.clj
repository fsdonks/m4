;;An implementation of the supply simulation used by Marathon.
;;All the operations for pushing supply, in the context of a simulation are 
;;maintained here.
(ns marathon.sim.supply
  (:require [marathon.demand [demanddata :as d] [demandstore :as store]]
            [marathon.supply [unitdata :as udata]]
            [marathon.sim [demand :as dem]   [policy :as pol]
                          [unit :as u]          [fill :as fill]]           
            [sim [simcontext :as sim] [updates :as updates]]
            [util [tags :as tag]]))

;TEMPORARILY ADDED for marathon.sim.demand
(declare release-max-utilizers)

;;#Primitive Operations and Supply Queries#

;'TODO -> formalize dependencies and pre-compilation checks....
(defn can-simulate? [supply] 
  (not (empty? (tag/get-subjects (:tags supply) :enabled))))
(defn unit-msg [unit] 
  (str "Updated Unit " (:name unit) " " (udata/getStats unit)))

(defn enabled? [tags unitname] (tag/has-tag? tags :enabled unitname))
(defn disable  [tags unitname] (tag/untag-subject tags unitname :enabled))
(defn ghost? [tags unit] (tag/has-tag? tags :ghost (:name unit)))
(defn set-ghosts [x ctx] (assoc-in ctx [:state :supplystore :has-ghosts] x))

(defn add-unit [supply unit]    (assoc-in supply [:unitmap (:name unit)] unit))
(defn get-unit [supplystore name] (get-in supplystore [:unit-map  name]))
(defn has-behavior? [unit] (not (nil? (:behavior unit))))
(defn assign-behavior [behaviors unit behaviorname]
  (behaviors/assign-behavior unit behaviorname))
(defn empty-position? [unit] (nil? (:position-policy unit)))


;;#Tag Keywords#
(defn key-tag [base tag] (memoize (keyword (str base tag))))
;helper function for defining key-building functions.
(defmacro defkey [name base] `(def ~name (~'partial ~'key-tag ~base))) 
(defkey source-key "SOURCE_")
(defkey sink-key   "SINK_")
(defkey compo-key  "COMPO_") 
(defkey behavior-key "BEHAVIOR_")
(defkey title-key   "TITLE_")
(defkey policy-key  "POLICY_")
(def ghost-source-tag (source-key "Ghost"))

;;#Tag Operations#
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
(defn tag-unit [supply unit [extra-tags]]
  (let [sourcename (source-key (:src unit))]
    (->> (into (default-tags unit) extra-tags) 
         (tag/multi-tag (:tags supply) (:name unit))
         (tag-source sourcename)
         (tag-extras unit extra-tags)
         (assoc supply :tags))))

;helper function for dropping a tag from multiple units at once.
(defn untag-units [supplystore tag units]
  (reduce #(tag/untag-subject (:tags %1) %2 tag)) supplystore units)

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

;;#Tag-Related Supply Queries#
(defn get-sources [supply] (tag/get-subjects (:tags supply) :sources))
(defn multiple-ghosts? [supplytags]
  (> (count (tag/get-subjects supplytags ghost-source-tag)) 1))

;;#Supply Population Operations#
;this might be suitable to keep in the supplymanager...
(defn add-src [supply src] 
  (let [scoped (:srcs-in-scope supply)]
    (if (contains? scoped src) supply 
      (assoc-in supply [:srcs-in-scope src] (count scoped)))))
(defn remove-src [supply src] (update-in supply [:srcs-in-scope] dissoc src))

(defn get-buckets [supply] (:buckets supply))
(defn add-bucket [supply bucket-name]
  (let [buckets (:deployable-buckets supply)]
    (if (contains? buckets bucket-name) supply 
      (assoc-in supply [:deployable-buckets bucket-name] {}))))

;----FOREIGN -> THESE SHOULD BE MOVED, THEY'RE MORE GENERAL.....
(defn conj-policy [unit policy] (update-in unit [:policy-queue] conj policy))
(defn set-supplystore [ctx supply] (assoc-in ctx [:state :supplystore] supply))
;came from OpFactory..
(defn create-unit [name src title component cycletime policy-id 
                    parameters policystore & [behavior]]
  (let [p (policy/choose-policy policy-id component parameters policystore src)]
    (-> {:name name :src src  :oi-title title :component component 
         :behavior behavior   :cycletime cycletime :policy p}
        (u/map->unitdata))))
;----------END FOREIGN------




;;#Supply Updating#
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
(defn up-to-date? [t ctx unitname] (= (sim/last-update unitname ctx) t))

(defn supply-update! "Notifies the context of a supply update."
  [supply unit msg ctx]
  (sim/trigger :supplyUpdate (:name supply) (:name unit) msg nil ctx))

(defn get-supply-updates "Get all units with pending supply updates." 
  [t ctx]  (sim/get-updates t :supply-update ctx))

(defn request-unit-update! "Schedule an update for unit at time t." 
  [t unit ctx] 
  (sim/request-update t (:name unit) :supply-update ctx))

(defn apply-update
  "Ages an individual unit, based on how much time has elapsed - for the unit -
   between time t and its last update."
  [t supplystore unitname ctx]
  (if (not (enabled? (:tags supplystore) unitname)) ctx 
    (let [unit (get-unit supplystore unitname)]
      (->> ctx       
        (u/update unit (updates/elapsed t (sim/last-update unitname ctx)))
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

;;#Supply Management#

;TOM Change 24 April 2012 -> decoupled the getUpdates....now we pass in a list 
;of updates from outside (usually via the engine), rather than having 
;supplymanager need visibility on it.
(defn manage-supply
  "High level hook for the supply system.  For entities that have scheduled 
   updates at time t, they are brought up to date and have their changes 
   incorporated into the context.  The entity behaviors will typically 
   use some of the supply system functions defined below to alter the context."
  [t ctx]
  (let [supply (core/get-supplystore ctx)]
    (if-let [today-updates (map :requested-by (get-supply-updates t ctx))]
      (update-units t supply ctx today-updates)
      ctx)))

;'A simple wrapper to unify the high level supply management.  We were calling 
;this inline, it's more consistent now.
(defn manage-followons [day ctx] 
  (release-followons (core/get-supplystore ctx) ctx))

;;#General Supply Notifications#

(defn spawning-unit! [unit ctx]
  (sim/trigger :spawnnit (:name unit) (:name unit)
     (str "Spawned Unit " (:name unit)) nil ctx))             

(defn spawning-ghost! [unit ctx]
  (sim/trigger :SpawnGhost (:name unit) (:name unit)
     (str "Spawned a ghost " (:name unit)) nil ctx))  

(defn new-deployable! [unit ctx]
  (assert (not= (:policy-position unit) :Recovery) "Recovery is not deployable")
  (sim/triggger :NewDeployable "SupplyManager" (:name unit) 
      (str "Unit " (:name unit) " at position " (:position-policy unit) 
           " is deployable") nil ctx))
(defn new-followon! [unit ctx] 
  (sim/trigger :NewFollowOn "SupplyManager" (:name unit)
      (str "Unit " (:name unit) " able to followon for demandgroup " 
           (:followoncode unit)) nil ctx))
(defn more-src-available! [unit ctx]
  (let [src (:src unit)]
    (sim/trigger :MoreSRCAvailable "SupplyManager" src 
       (str "Unit " (:name unit) " at position " (:position-policy unit) 
            "has just been added to deployables for SRC " src) nil ctx))) 
(defn new-src-available! [src ctx]
  (let [src (:src unit)]
    (sim/trigger :NewSRCAvailable "SupplyManager" src 
       (str "A new category of SRC now has deployable supply " src) nil ctx)))
(defn not-deployable! [unit ctx] 
  (sim/trigger :NotDeployable "SupplyManager" (:name unit) 
     (str "Unit " (:name unit) " at posiotion " (:position-policy unit) 
          " is no longer deployable") nil ctx))
(defn out-of-stock! [src ctx]
  (sim/trigger :outofstock "SupplyManager" src 
     (str "SRC " src " has 0 deployable supply") (source-key src) ctx))

;'TOM Change 3 Jan 2011
;'aux function for logging/recording the fact that a unit changed locations
;'TODO -> it'd be nice to figure out how to unify this reporting, right now 
;LogMove gets to reach directly into the tables of outputmanager and manipulate.
;This is fast and simple, but it's not pure ....
;One current problem is -> we have to transform fromloc/toloc into something 
;palatable for trending ...
(defn log-move! [t fromloc toloc unit & [duration ctx]]
  (sim/trigger :unitMoved (:name unit) toloc "" unit ctx))

;TODO -> This should be renamed like positionEvent or something.
;Main dependencies are in the unit Behaviors.
;Unit behaviors currently use parent to refer to a supply manager.
;We can probably do better than this.
;Actually, unit behaviors aren't maintaining any state....
;So we can probably just plug them in as modules....they're all pure functions.
;'TOM Change 6 June 2011 -> Added logging for unit positioning specifically..
(defn log-position! [t frompos topos unit & [duration ctx]]
  (sim/trigger :PositionUnit "SupplyManager" (:name unit) 
     (str "UIC " (:name unit) " has repositioned from " frompos " to " topos)
     nil ctx))
;aux function for logging/recording the fact that a unit deployed
(defn log-deployment! 
  [t fromname demand unit fillcount filldata deploydate  period & [ctx]]
  (sim/trigger :deploy "SupplyManager" (:name unit)               
     (str "Deployed unit " (:name unit) 
          " from " fromname " to demand " (:name demand))
     {:fromloc   fromname  :unit unit :demand demand :fill filldata 
      :fillcount fillcount :period period :t t :deploydate deploydate}  ctx))
;When a unit engages in a followon deployment, we notify the context.
(defn unit-followon-event! [unit demand ctx]
  (sim/trigger :FollowingOn  (:name unit) (:name demand) 
     (str "Unit " (:name unit) " is following on to demand " (:name demand))
        nil ctx))

(defn first-deployment! [supply unit ctx]
  (sim/trigger :firstDeployment (:name supply) (:name supply) 
       (str "Unit " (:name unit) " Deployed for the First Time") nil ctx))  

;;#Supply Availability#

(defn update-availability [unit supply ctx]
  (if (contains? (get-buckets supply) (:src unit))
    (more-src-available! unit ctx)
    (->> (new-src-available! (:src unit) ctx)
      (new-deployable! unit))))

;Consolidated this from update-deployability, formalized into a function.
(defn add-deployable-supply [supply src unit ctx]
  (->> (sim/merge-updates 
         {:supplystore (assoc-in supply [:buckets src unitname] unit)} ctx)
       (update-availability unit supply)))

;Consolidated this from update-deployability, formalized into a function.
(defn remove-deployable-supply [supply src unit ctx]
  (if-let [newstock (-> (get-in supply [:buckets src (:name unit)])
                        (dissoc (:name unit)))]
    (sim/merge-updates 
      {:supplystore (assoc-in supply [:buckets src] newstock)} ctx)
    (->> (sim/merge-updates 
           {:supplystore (update-in supply [:buckets] dissoc src)} ctx)
         (out-of-stock! (:src unit)))))

;Sub operates on uics to register them as deployable with a dictionary of 
;dictionaries Dictionary<Rule, <UICName,Unitdata>>
;UICs are tracked by a unique string name now, changed the unitdata structure 
;to reflect this. This sub is only called when necessary, updates the available 
;set, and limits the search effort required to find an available uic.

;1)NOTE -> reconcile the differences between u, udata
(defn update-deployability [unit buckets & [followon spawning ctx]]
  (assert (not (empty-position? unit)) "invalid position!")
  (let [position (:position-policy unit)
        src      (:src unit)
        supply   (core/get-supplystore ctx)]
    (if (or followon (u/can-deploy? unit spawning))                         ;1)
      (->> (if followon  ;notifiying of followon data...
             (new-followon! unit ctx) 
             (new-deployable! unit ctx))
           (add-deployable-supply supply src unit)) ;add stuff to buckets...   
      ;unit is not deployable
      (->> (not-deployable! unit ctx)
           (remove-deployable-supply supply src unit)))))

(defn update-deploy-status [supply unit [followon? spawning? ctx]]
  (let [buckets (if followon?       
                  (get-followon-bucket (:followonbuckets supply) 
                                       (:followoncode unit))
                  (:deployable-buckets supply))] 
    (update-deployability unit buckets  followon? spawning? ctx)))

;;#Registering New Supply#

;Note -> the signature for this originally returned the supply, but we're not 
;returning the context.  I think our other functions that use this guy will be
;easy to adapt, just need to make sure they're not expecting supplystores.
;Conjoins a unit to the supply, under the context.  Optional parameters for 
;communicating whether the unit is a ghost, as well as additional tags to be 
;added on-top-of the default tags derived from the unit data.
(defn register-unit [supply behaviors unit & [ghost ctx extra-tags]]
  (let [unit   (if (has-behavior? unit) unit (assign-behavior behaviors unit))
        supply (-> (add-unit supply unit)
                   (tag-unit unit extra-tags)
                   (add-src (:src unit)))
        ctx    (set-supplystore ctx supply)]
    (if ghost 
      (->> (spawning-ghost! unit ctx)
           (set-ghosts true))
      (->> (spawning-unit! unit ctx)
           (update-deploy-status supply unit)))))

;creates a new unit and stores it in the supply store...returns the supply 
;store.
(defn new-unit [supplystore parameters policystore behaviors name src title 
                component cycletime policy & [behavior ctx]]
  (let [new-unit (create-unit name src title component cycletime policy 
                              parameters policystore behavior)]
    (register-unit supplystore behaviors (ghost? new-unit) ctx)))
                                           

;This is an aux function that serves as a weak patch...probably unnecessary.
;;__TODO__Remove the need for adjust-max-utilization!
(defn adjust-max-utilization! [supply unit ctx] 
  (if (and (check-max-utilization (core/get-parameters ctx))
           (should-change-policy? unit))
    (let [new-policy (get-near-max-policy (:policy unit) 
                                          (core/get-policystore ctx))]
      (sim/merge-updates 
        {:supplystore (add-unit supply (conj-policy unit new-policy))} ctx))                                                  
    ctx))

;;#Deploying Supply#
(defn check-followon-deployer! [followon? unitname demand t ctx]
  (let [supply (core/get-supplystore ctx)
        unit   (get-unit ctx unitname)]
        (if followon? 
          (record-followon supply unit demand ctx)
          (u/re-deploy-unit unit t (:deployment-index unit) ctx))))

(defn check-first-deployer!   [supply unitname ctx]
  (let [unit (get-unit supply unitname)]  
    (if (first-deployment? unit supply)
      (->> (set-supplystore ctx (tag-as-deployed unit supply))
           (first-deployment! supply unit)
           (adjust-max-utilization! supply unit)))))

;NOTE -> replace this with a simple map lookup...
(defn get-near-max-policy [policy policystore]
  (let [id (case (pol/atomic-name policy) 
             :MaxUtilization         :NearMaxUtilization
             :MaxUtilization_Enabler :NearMaxUtilization_Enabler)]
    (get-in policystore [:policies id])))

;WEAK...hard coded, should be data driven.
(defn should-change-policy? [{:keys [component] :as unit}]
  (not= component :AC :Ghost))
(defn check-max-utilization [params] (get params :TAA1519MaxUtilizationHack))
(defn tag-as-deployed [unit supplystore] 
  (update-in supplystore [:tags] tag/tag-subject (:name unit) :hasdeployed))

(defn get-next-deploymentid [supplystore] (inc (:uniqedeployments supplystore)))
                                         
;NOTE -> this should be OBSOLETE after the port, since we can handle policies 
;much more gracefully.
;Jeff had me put in some special case for handling initial deployment logic.
(defn first-deployment? [unit supplystore]
  (not (tag/has-tag? (:tags supplystore) (:name unit) :hasdeployed))) 

;Enacts context changes and updates necessary to constitute deploying a unit.
;Critical function.
;---------CHECK PARAMETERS -> lots of unused stuff here (might be legacy fluff, 
;most of which is in context.
(defn deploy-unit [supply ctx parameters policystore unit t sourcetype demand 
                     bog fillcount filldata deploydate  & [followon?]]
  (assert  (u/valid-deployer? unit) 
    "Unit is not a valid deployer! Must have bogbudget > 0, 
     cycletime in deployable window, or be eligible or a followon  deployment")
  (let [demandname    (:name demand)
        demand        (d/assign demand unit) ;need to update this in ctx..
        demandstore   (core/get-demandstore ctx) ;Lift to a protocol.
        from-location (:locationname unit) ;may be extraneous
        from-position (:position-policy unit);
        to-location   demandname
        to-position   :deployed
        unitname      (:name unit)
        unit          (-> unit ;MOVE THIS TO A SEPARATE FUNCTION? 
                       (assoc :position-policy to-position) 
                       (assoc :dwell-time-when-deployed (udata/get-dwell unit)))
        supply        (drop-fence (:tags supply) (:name unit))] ;RE-ORDERED 
     (->> (sim/merge-updates {:demandstore (dem/add-demand demand)
                              :supplystore supply} ctx)
          (u/change-location! unit (:name demand)) ;unit -> str ->ctx -> ctx....
          (check-followon-deployer! followon? supply unitname demand t)
          (u/deploy-unit unit t (get-next-deploymentid supply))
          (check-first-deployer! supply unitname) ;THIS MAY BE OBVIATED.
          (update-deployability unit (:deployable-buckets supply) false false) 
          (log-deployment! t from-location demand unit fillcount filldata 
             deploydate (policy/find-period t policystore))
          (supply-update! supply unit nil)))) 


;UTILITY FUNCTION
;If function results in an empty map, contained within another map, 
;removes the entry associated with the empty map.
(defn prune-in [m ks f & args]
  (let [updated (apply update-in ks f args)]
    (if (empty? (get-in updated ks))
      (let [path   (butlast ks)
            parent (get-in m path)]            
        (assoc-in m path (dissoc parent (last ks))))
      updated)))
;END UTILITY 

;The call for update-deploy-status is UGLY.  Might be nice to use keyword args..
(defn add-followon [supply unit ctx] 
  (-> (assoc-in supply [:followons (:name unit)] unit)
      (update-deploy-status unit true nil ctx)))

(defn remove-followon [store unit]
  (let [unitname (:name unit)
        fcode    (:followoncode unit)
        src      (:src unit)]
    (-> store 
        (update-in [:followons] dissoc unitname)
        (prune-in  [:followonbuckets fcode src] dissoc unitname)
        (assoc-in  [:unit-map (:name unit)] 
          (assoc unit :followoncode nil)))))

(defn get-unit [supplystore unitname] (get-in supplystore [:unitmap unitname]))
(defn followon-unit? [store unit] (contains? (:followons store) (:name unit)))

;;__TODO__Detangle release-followon-unit.
;;Convoluded.  Need to detangle this guy.
(defn release-followon-unit [ctx unitname]
  (let [store (core/get-supplystore ctx)
        ctx   (->> (sim/merge-updates 
                     {:supplystore (remove-followon store unit)} ctx)
                   (u/change-state (get-unit store unitname) 
                                   :AbruptWithdraw 0 nil))]   
    (update-deploy-status 
      (core/get-supplystore ctx) (get-unit store unitname) nil nil ctx)))


;process the unused follow-on units, changing their policy to complete cycles.
;CONTEXTUAL
(defn release-followons [supplystore & [ctx]]
  (reduce release-followon-unit ctx (keys (:followons supplystore))))         

;UGLY....this could be prettier....although it refactored nicely.  
(defn release-max-utilizers [supplystore & [ctx]]
  (let [{:keys [followons normal]} 
           (group-by #(if (followon-unit? store %) :followon :normal)
                      (tag/get-subjects (:tags supplystore) :MaxUtilizer))
         store (untag-units supplystore :MaxUtilizer (concat followons normal))]   
    (reduce release-followon-unit (sim/merge-updates {:supplystore store} ctx)
            followons)))         

;announce that the unit is in fact following on, remove it from followons.
(defn record-followon [supply unit demand ctx]
  (->> (sim/merge-updates {:supplystore (remove-followon supply unit)} ctx)
       (unit-followon-event! unit demand?)))

(defn drop-unit [supply unitname] 
  (update-in supply [:unit-map] dissoc unit-name))

;DOUBLE CHECK....do we really mean to drop the entire src? 
(defn remove-unit [supply unitname]
  (-> (drop-unit supply unitname)
      (remove-src (:src (get-unit supply unitname))))) 

;procedure that allows us to, using the fillgraph, derive a set of tags whose 
;associated units should be deactivated.  if removal is true, the units will be
;removed from memory as well. in cases where there are a lot of units, this may 
;be preferable.
(defn scope-supply [supply disable-tags & [removal]]
  (let [f (if removal remove-unit (fn [s u] s))]
    (->>  (tag/and-tags (:tags supply) (set disable-tags)) 
          (reduce (fn [s u] (-> (update-in s [:tags] disable u) f u)) supply))))
