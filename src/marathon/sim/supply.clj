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

;Notifies the context of a supply update.
(defn supply-update! [supply unit msg ctx]
  (sim/trigger :supplyUpdate (:name supply) (:name unit) msg nil ctx))

;get all pending supply updates.
(defn get-supply-updates [t ctx]  (sim/get-updates t :supply-update ctx))

;----FOREIGN -> THESE SHOULD BE MOVED, THEY'RE MORE GENERAL.....
(defn add-unit [supply unit]    (assoc-in supply [:unitmap (:name unit)] unit))
(defn conj-policy [unit policy] (update-in unit [:policy-queue] conj policy))
(defn get-policystore [ctx]     (get-in ctx [:state :policystore]))
(defn set-supplystore [ctx supply] (assoc-in ctx [:state :supplystore] supply))
(defn get-demandstore [ctx] (get-in ctx [:state :demandstore]))
(defn get-parameters [ctx] (get-in ctx [:state :parameters]))
;----END FOREIGN


(defn enabled? [tags unitname] (tag/has-tag? tags :enabled unitname))
(defn disable  [tags unitname] (tag/untag-subject tags unitname :enabled))

;Unit can request an update at a specified time ....
(defn request-unit-update! [t unit ctx] 
  (sim/request-update t (:name unit) :supply-update ctx))

;'TODO -> formalize dependencies and pre-compilation checks....
(defn can-simulate? [supply] 
  (not (empty? (tag/get-subjects (:tags supply) :enabled))))

(defn unit-msg [unit] 
  (str "Updated Unit " (:name unit) " " (udata/getStats unit)))

(defn get-supplystore [ctx] (-> ctx :state :supplystore))
(defn get-unit [supplystore name] (get-in supplystore [:unit-map  name]))

;helper function for dropping a tag from multiple units at once.
(defn untag-units [supplystore tag units]
  (reduce #(tag/untag-subject (:tags %1) %2 tag)) supplystore units)

(defn up-to-date? [day ctx unitname] (= (sim/last-update unitname ctx) day))

(defn apply-update [supplystore unitname ctx]
  (if (not (enabled? (:tags supplystore) unitname)) ctx 
    (let [unit (get-unit supplystore unitname)]
      (->> ctx       
        (u/update unit (updates/elapsed t  (sim/last-update unitname ctx)))
        (supply-update! supplystore unit (unit-msg unit))))))

(defn update-units [supply ctx units]       
  (reduce (fn [acc x] (apply-update (get-supplystore acc) x acc))  ctx units))
;Update every unit in the supply, synchronizes numerical stats.
(defn update-all [day supply ctx & [unitnames]]
  (->> (or unitnames (keys (get supply :unitmap)))
       (filter (partial up-to-date? day ctx))
       (update-units supply ctx)))

;;Note -> there was another branch here originally....
;;requestUnitUpdate day, unit, context

;NOTE -> ambiguity between state and context here, need to clarify. 
;TOM Change 24 April 2012 -> decoupled the getUpdates....now we pass in a list 
;of updates from outside (usually via the engine), rather than having 
;supplymanager need visibility on it.
(defn manage-supply [day state]
  (let [supply (:supply-store state)
        ctx    (:context state)]
    (if-let [today-updates (map :requested-by (get-supply-updates day ctx))]
      (update-units supply ctx today-updates)
      ctx)))

;'A simple wrapper to unify the high level supply management.  We were calling 
;this inline, it's more consistent now.
(defn manage-followons [day ctx] (release-followons (get-supplystore ctx) ctx))

(defn spawning-unit! [unit ctx]
  (sim/trigger :spawnnit (:name unit) (:name unit)
     (str "Spawned Unit " (:name unit)) nil ctx))             

(defn spawning-ghost! [unit ctx]
  (sim/trigger :SpawnGhost (:name unit) (:name unit)
     (str "Spawned a ghost " (:name unit)) nil ctx))  

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

;REFACTOR
;maybe ignore this? Extra level of indirection.
(defn get-time [ctx] (sim/get-time ctx))
(defn get-sources [supply] (tag/get-subjects (:tags supply) :sources))
;Register a set of units that need to be utilized, or sent to reset.

;REFACTOR
;maybe ignore this? Extra level of indirection.
(defn last-update [unitname ctx] (sim/last-update unitname ctx))


(defn multiple-ghosts? [supplytags]
  (> (count (tag/get-subjects supplytags ghost-source-tag)) 1))


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
;When a unit engages in a followon deployment, we notify the event context.
(defn unit-followon-event! [unit demand ctx]
  (sim/trigger :FollowingOn  (:name unit) (:name demand) 
     (str "Unit " (:name unit) " is following on to demand " (:name demand))
        nil ctx))

(defn first-deployment! [supply unit ctx]
  (sim/trigger :firstDeployment (:name supply) (:name supply) 
       (str "Unit " (:name unit) " Deployed for the First Time") nil ctx))  

;register source as being a member of sources, so it can be looked at when 
;filling supply.
(defn tag-source [source tags] (tag/tag-subject tags source :sources))
(defn add-bucket [supply bucket-name]
  (let [buckets (:deployable-buckets supply)]
    (if (contains? buckets bucket-name) supply 
      (assoc-in supply [:deployable-buckets bucket-name] {}))))

(defn ghost? [tags unit] (tag/has-tag? tags :ghost (:name unit)))

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
                            

;inject appropriate tags into the supply tags.
(defn tag-unit [supply unit [extra-tags]]
  (let [sourcename (source-key (:src unit))]
    (->> (into (default-tags unit) extra-tags) 
         (tag/multi-tag (:tags supply) (:name unit))
         (tag-source sourcename)
         (tag-extras unit extra-tags)
         (assoc supply :tags))))

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

;this might be suitable to keep in the supplymanager...
(defn add-src [supply src] 
  (let [scoped (:srcs-in-scope supply)]
    (if (contains? scoped src) supply 
      (assoc-in supply [:srcs-in-scope src] (count scoped)))))

(defn remove-src [supply src] (update-in supply [:srcs-in-scope] dissoc src))
(defn assign-behavior [behaviors unit behaviorname]
  (behaviors/assign-behavior unit behaviorname))

(defn has-behavior? [unit] (not (nil? (:behavior unit))))
(defn empty-position? [unit] (nil? (:position-policy unit)))
(defn get-buckets [supply] (:buckets supply))

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
        supply   (get-supplystore ctx)]
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

;----------FOREIGN----------
;came from OpFactory..
(defn create-unit [name src title component cycletime policy-id 
                    parameters policystore & [behavior]]
  (let [p (policy/choose-policy policy-id component parameters policystore src)]
    (-> {:name name :src src  :oi-title title :component component 
         :behavior behavior   :cycletime cycletime :policy p}
        (u/map->unitdata))))
;----------END FOREIGN------

(defn set-ghosts [x ctx] (assoc-in ctx [:state :supplystore :has-ghosts] x))

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
                                           
;this is an aux function that serves as a weak patch...probably unnecessary.
(defn adjust-max-utilization! [supply unit ctx] 
  (if (and (check-max-utilization (get-parameters ctx))
           (should-change-policy? unit))
    (let [new-policy (get-near-max-policy (:policy unit) (get-policystore ctx))]
      (sim/merge-updates 
        {:supplystore (add-unit supply (conj-policy unit new-policy))} ctx))                                                  
    ctx))

;REFACTORING...
;trying to refactor....what would a nice abstraction look like? 
;adjust-max-utilization is a great candidate..
;there are a couple of bread-n-butter items of interest...
;1) we're updating a unit functionally, storing the result of the update, 
;   a new unit
;2) we're updating the supplystore, to reflect the updated unit from 1).
;3) we're merging the new supplystore into the updated context, by 
;   passing it to a generic "update" handler that knows how to interpret 
;   the key :supplystore, the new supply, and a context, to transition to 
;   a new context.
;One improvement is to wrap the operation behind a function to provide a clean
;API....namely, update-unit 
;This would fetch the appropriate unit, apply a function to it (like update-in),
;and return a new supplystore with the unit updated.
;  In this sense, we're lifting a specific function, updating the policyq of 
;  a unit, into the context of a container for units. 
;  In haskell speak, the supplystore is a functor, we're applying an update to 
;  an element of the container.  It's the container's job to understand how 
;  to lift the updating function into the proper context, and return the new 
;  container.  monads/monoids anyone? 

(defn check-followon-deployer! [followon? unitname demand t ctx]
  (let [supply (get-supplystore ctx)
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
        demandstore   (get-demandstore ctx) ;Lift to a protocol.
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
(defn release-followon-unit [ctx unitname]
  (let [store (get-in ctx [:state :supplystore])
        ctx   (->> (sim/merge-updates 
                     {:supplystore (remove-followon store unit)} ctx)
                   (u/change-state (get-unit store unitname) 
                                   :AbruptWithdraw 0 nil))
        unit  (get-in store [:unit-map unitname])]   
    (update-deploy-status 
      (-> ctx :state :supplystore) unit nil nil ctx)))


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
