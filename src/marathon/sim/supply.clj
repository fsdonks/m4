;We're creating circular dependencies....that's bad.
;We need to create some shared protocols, and then implement them.
(ns marathon.sim.supply
  (:require [marathon.demand [demanddata :as d] [demandstore :as store]]
            [marathon.supply [unitdata :as udata]]
            [marathon.sim [demand :as demand]   [policy :as pol]
                          [unit :as u]          [fill :as fill]]           
            [sim [simcontext :as sim] [updates :as updates]]
            [util [tags :as tag]]))

;Note--> I'm noticing some patterns emerging that might make for nice 
;abstractions...
;We seem to be doing a lot of nested updates for particular bits of a system.
;A lot of updates also appear to be existential in nature, i.e. if the 
;update is a dissoc, and the resulting collection is empty, we want to 
;remove the empty entry from the parent container.

;Also, we define operations that just trigger notifications, basically a 
;messaging system.  Might look into a nice higher-level defmessage abstraction.

;Also, we have a class of functions that specifically shovels context around.
;By context, I mean the environment in which the simulation is happening, as 
;in a simcontext from sim.simcontext.  We can clean up some of the function 
;signatures by allowing macros to operate in this context, possibly even a 
;special evaluator.

;On the topic of special evaluators, in each namespace, we end up defining 
;pure functions that act to update specific bits of a context, sometimes 
;acting on isolated structures within the context.  There's typically a 
;nested structure in the context's :state, which destructure and update in 
;pieces.  It might be nice to have something like #'doto. 

;It might be nice to have a macro that lets us define ways to address or query
;the context's state.  This leads directly to the entity-store stuff I already 
;built, and facilitates a component-based architecture.

;Finally, we probably want some kind of language for specifying different 
;types of updates to the context.  For instance, we end up -inside the local 
;namespaces - defining functions that operate on a specific aread of the context
;often with deep nesting paths.  I already introduced the notion of updates and
;merge-updates in the simcontext.  It might be as simple as defining
;multimethods, or a protocol, that implements the merge protocol.

;COUPLING 
;sim.demand => get-followon-keys release-max-utilizers

;TEMPORARILY ADDED for marathon.sim.demand
(declare get-followon-keys release-max-utilizers)

;marathonopsupply
;11 July 2012 -> recasting of supply management.
;We define a supply simulation as a set of operations on supply simulation state.
;It's basically a decoupling of the earlier object hierarchy.
;Instead of encapsulating everything in the supply manager class, we're pulling 
;out as much of the methods as possible, and providing a functional interface to
;modify supply managers.
;
;The end result is a lower-order supply manager that handles little to no 
;internal functions, and manages some state that we need.  All the operations
;for pushing supply, in the context of a simulation are maintained here.

;TOM Change->
;   This is serving as a template for reorganizing the simulation.
;   The desire is to separate operations from data.
;   We have multiple levels of operations....
;   This library groups several levels of operations along the Supply domain.
;   The primary function is the ManageSupply function....
;   Manage supply eats core data....

;Notifies the context of a supply update.
(defn supply-update! [supply unit msg ctx]
  (sim/trigger :supplyUpdate (:name supply) (:name unit) msg nil ctx))

;get all pending supply updates.
(defn get-supply-updates [t ctx]  (sim/get-updates t :supply-update ctx))

(defn enabled? [tags unitname] (tag/has-tag? tags :enabled unitname))
(defn disable  [tags unitname] (tag/untag-subject tags unitname :enabled))

;Unit can request an update at a specified time ....
(defn request-unit-update! [t unit ctx] 
  (sim/request-update t (:name unit) :supply-update ctx))

;'TODO -> formalize dependencies and pre-compilation checks....
(defn can-simulate? [supply] 
  (not (empty? (tag/get-subjects (:tags supply) :enabled))))

;Replaced by request-unit-update!
;Public Sub requestSupplyUpdate(t As Single, unit As TimeStep_UnitData, context As TimeStep_SimContext)
;SimLib.requestUpdate t, "SupplyManager", UpdateType.supply, , context
;End Sub

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

;Public Sub spawnUnitEvent(unit As TimeStep_UnitData, context As TimeStep_SimContext)
;SimLib.triggerEvent TimeStep_Msg.spawnunit, unit.name, unit.name, "Spawned Unit " & unit.name, , context
;End Sub

(defn spawning-unit! [unit ctx]
  (sim/trigger :spawnnit (:name unit) (:name unit)
     (str "Spawned Unit " (:name unit)) nil ctx))             

;Public Sub spawnGhostEvent(unit As TimeStep_UnitData, context As TimeStep_SimContext)
;SimLib.triggerEvent TimeStep_Msg.SpawnGhost, unit.name, unit.name, "Spawned a ghost", , context
;End Sub
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
;'When a unit engages in a followon deployment, we notify the event context.
;'Simple declarative event description for wrapping low level followon event notification.
(defn unit-followon-event! [unit demand ctx]
  (sim/trigger :FollowingOn  (:name unit) (:name demand) 
     (str "Unit " (:name unit) " is following on to demand " (:name demand))
        nil ctx))
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

;Public Function assignBehavior(behaviors As TimeStep_ManagerOfBehavior,
;       unit As TimeStep_UnitData, behaviorname As String) As TimeStep_UnitData
;Set assignBehavior = behaviors.assignBehavior(unit, behaviorname)
;End Function
(defn assign-behavior [behaviors unit behaviorname]
  (behaviors/assign-behavior unit behaviorname)) ;WRONG  behaviors doesn't exist yet.

(defn has-behavior? [unit] (not (nil? (:behavior unit))))
(defn empty-position? [unit] (nil? (:position-policy unit)))
(defn get-buckets [supply] (:buckets supply))

;Consolidated this from update-deployability, formalized into a function.
(defn add-deployable-supply [supply src unit ctx]
  (if (contains? (get-buckets supply) src)
    (->> (sim/merge-updates 
           {:supplystore 
            (update-in supply [:buckets src] assoc unitname unit)} ctx)
         (more-src-available! unit)
    (->> (sim/merge-updates 
           {:supplystore (assoc-in supply [:buckets src] {unitname unit})} ctx) 
         (new-src-available! (:src unit))
         (new-deployable! unit)))))
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
    (if (or followon (u/can-deploy? udata spawning))                         ;1)
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

;Note -> the signature for this originally returned the supply, but we're not 
;returning the context.  I think our other functions that use this guy will be
;easy to adapt, just need to make sure they're not expecting supplystores.
;Conjoins a unit to the supply, under the context.  Optional parameters for 
;communicating whether the unit is a ghost, as well as additional tags to be 
;added on-top-of the default tags derived from the unit data.
(defn register-unit [supply behaviors unit & [ghost ctx extra-tags]]
  (let [unit   (if (has-behavior? unit) unit (assign-behavior behaviors unit))
        supply (-> (assoc-in supply [:unitmap (:name unit)] unit)
                   (tag-unit unit extra-tags)
                   (add-src (:src unit)))
        ctx    (assoc-in ctx [:state :supplystore] supply)]
    (if ghost 
      (->> (spawning-ghost! unit ctx)
           (assoc-in [:state :supplystore :has-ghosts] true))
      (->> (spawning-unit! unit ctx)
           (update-deploy-status supply unit)))))

;creates a new unit and stores it in the supply store...returns the supply 
;store.
(defn new-unit [supplystore parameters policystore behaviors name stc title 
                component cycletime policy & [behavior]]
  (->> (create-unit name src title component cycletime policy parameters 
               policystore behavior)
       (register-unit supplystore behaviors)))

;Sub deployUnit(supply As TimeStep_ManagerOfSupply, context As TimeStep_SimContext, parameters As TimeStep_Parameters, _
;                policystore As TimeStep_ManagerOfPolicy, unit As TimeStep_UnitData, t As Single, sourcetype As String, _
;                    demand As TimeStep_DemandData, bog As Long, fillcount As Long, fill As TimeStep_Fill, _
;                        deploydate As Date, Optional isFollowon As Boolean, Optional location As Long)
;''TOM Note 23 Mar 2011 -> we'll have to revisit this, big time.
;''if a unit is found, this sub deploys the unit
;Dim FromLocation As String
;Dim ToLocation As String
;Dim FromPosition As String
;Dim ToPosition As String
;
;Dim msg As String 'TOM added 9 Sep 2012
;
;With unit
;    'TODO -> .validdeployer and friends are an abomination.  Need to rip that stuff out of unitdata....
;    'TOM Change 3 Jan 2011 -> ported this to a string name convention, we need this for plotting values.
;    If .validDeployer Then
;        FromLocation = .LocationName 'Tom Note <- this is wrong, not being updated.
;        FromPosition = .PositionPolicy 'Tom Change 24 May
;
;        ToLocation = demand.name
;        'TOM Change June 6 2011 -> Temporary bypass....
;        ToPosition = "Deployed" '.policy.deployed 'Tom Change 24 May
;
;        'demand.qualityDeployed = sourceType 'not certain about this......can we calculate this?
;        'demand.unitsDeployed = unit.Index
;        'mutation!
;        demand.Assign unit 'Tom Change 14 Mar 2011, new method in demand class.
;
;        'TOM Change 7 Jun 2011 -> modified this to reflect movement in location space.
;        'TOM note 24 May -> Need to bifurcate the move logs into 2 as well, Position Change, Spatial Change.
;        'LogMove t, FromLocation, tolocation, unit, unit.policy.MaxBOG
;    
;        'set location array to the demand Key value
;        'Decoupled
;        '.changeLocation demand.name, context 'Mutation!
;        MarathonOpUnit.changeLocation unit, demand.name, context
;        
;        '.LocationName = demand.name
;        'Decoupled
;        'TODO consider pushing this into unitsim.changelocation...does it need to be here?
;        .location = location 'parent.policymanager.locationID(.LocationName)
;        
;        'Potential BUG from conversion, check.  30 Aug 2012
;        'parent.policymanager.locationID(.LocationName)
;
;        'TOM change 6 June 2011 -> this was causing a problem with deployability....one of the casualties of
;        'the split between location and policy position.
;        'TODO consider pushing this into unitsim.changelocation...does it need to be here?
;        .PositionPolicy = ToPosition
;
;        If isFollowon Then
;            'Decoupled
;            recordFollowon supply, unit, demand, context
;            MarathonOpUnit.reDeployUnit unit, t, unit.deploymentindex, context
;        Else
;            MarathonOpUnit.deployUnit unit, t, getNextDeploymentID(supply), context
;        End If
;        
;        
;        'tom change 7 Sep -> updated for decoupling
;        'TOM Hack 13 August 2012
;        If isFirstDeployment(unit, supply) Then
;            tagAsDeployed unit, supply
;            SimLib.triggerEvent TimeStep_Msg.firstDeployment, supply.name, supply.name, "Unit " & unit.name & " Deployed for the First Time", , context
;            'parent.trigger TimeStep_Msg.firstDeployment, name, name, "Unit " & unit.name & " Deployed for the First Time"
;            If checkMaxUtilization(parameters) Then
;                If shouldChangePolicy(unit) Then unit.policyQueue.add getNearMaxPolicy(unit.policy, policystore)
;            End If
;        End If
;                    
;                    
;        'Bug fix 7 Sep 2012 -> I think followon is an enumerated type, which caused a subtle bug
;        'UpdateDeployability unit, supply.DeployableBuckets, isFollowon, False, context
;        'Further update 9 Sep 2012 ->  followon was never specified in the older code, so it defaulted to
;        'false.  The intent is to communicate the status of the unit, not the context of the deployment.
;        UpdateDeployability unit, supply.DeployableBuckets, False, False, context
;        
;        dropFence supply.tags, unit.name
;
;        'TOM change 7 Sep 2012 -> decoupled.
;        'TOM note 24 May -> Determine where we want to record the deployment happening from....
;        'Decoupled
;         msg = "Deployed unit " & unit.name & " from " & FromLocation & " to demand " & demand.name
;        LogDeployment t, FromLocation, demand, unit, fillcount, fill, deploydate, _
;                    MarathonOpPolicy.FindPeriod(t, policystore), context, msg
;
;
;        'TOM Note -this may be an error...switch cycletime to dwell.  I think i did this already.
;        .dwellTimeWhenDeployed = .cycletime
;        'TOM Change 24 April 2012 -> included extra criteria that bogbudget > 0
;
;        'TOM Change 14 July 2011
;
;''        triggerEvent supplyUpdate, supply.name, unit.name, msg, , context
;        'Decoupled
;        'Higher order
;        supplyUpdateEvent supply, unit, , context
;        'TOM Change 6 DEC 2010
;        'TOM Change 25 Mar 2011 -> we no longer need to do this for each deployed unit.  Fill is
;        'done in batches now.
;        'parent.demandmanager.UpdateFill day, demand.name, parent.demandmanager.UnfilledQ
;    'TOM Change 24 april -> Ensures that units are valid, according the criteria established 24 april
;    Else
;        Err.Raise 101, , "Unit is not a valid deployer! Must have bogbudget > 0, cycletime in " & _
;                            "deployable window, or be eligible or a followon deployment"
;    End If
;End With
;
;End Sub

;NOTE -> replace this with a simple map lookup...
(defn get-near-max-policy [policy policystore]
  (let [id (case (pol/atomic-name policy) 
             :MaxUtilization         :NearMaxUtilization
             :MaxUtilization_Enabler :NearMaxUtilization_Enabler)]
    (get-in policystore [:policies id])))

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
(defn release-maxutilizers [supplystore & [ctx]]
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
;CHECK -> might be missing something in the port.  
;1)not using toname in the original code, also no msg.
(defn log-deployment! 
  [t fromname demand unit fillcount filldata deploydate  period & [ctx]]
  ;(let [toname (:name demand)]                                              ;1)
  (sim/trigger :deploy "SupplyManager" (:name unit)
     "" {:fromloc   fromname  :unit unit :demand demand :fill filldata 
         :fillcount fillcount :period period :t t :deploydate deploydate} ctx))

;REFACTOR
;maybe ignore this? Extra level of indirection.
(defn get-time [ctx] (sim/get-time ctx))
(defn get-sources [supply] (tag/get-subjects (:tags supply) :sources))

;Register a set of units that need to be utilized, or sent to reset.
;The call for update-deploy-status is UGLY.  Might be nice to use keyword args..
(defn add-followon [supply unit ctx] 
  (-> (assoc-in supply [:followons (:name unit)] unit)
      (update-deploy-status unit true nil ctx)))

;REFACTOR
;maybe ignore this? Extra level of indirection.
(defn last-update [unitname ctx] (sim/last-update unitname ctx))

(def ghost-source-tag (source-key "Ghost"))
(defn multiple-ghosts? [supplytags]
  (> (count (tag/get-subjects supplytags ghost-source-tag)) 1))

;------------Deferred------------
;Public Function SupplyfromExcel(policystore As TimeStep_ManagerOfPolicy, parameters As TimeStep_Parameters, behaviors As TimeStep_ManagerOfBehavior, _
;                                    ctx As TimeStep_SimContext, Optional ensureghost As Boolean) As TimeStep_ManagerOfSupply
;Dim tbl As GenericTable
;Dim gunit As TimeStep_UnitData
;
;Set SupplyfromExcel = New TimeStep_ManagerOfSupply
;'TODO -> turn this into a function.
;UnitsFromSheet "SupplyRecords", SupplyfromExcel, behaviors, parameters, policystore, ctx
;
;If ensureghost Then
;    If Not SupplyfromExcel.hasGhosts Then
;        Set gunit = createUnit("Auto", "Ghost", "Anything", "Ghost", 0, "Auto", parameters, policystore)
;        Set gunit = associateUnit(gunit, SupplyfromExcel, ctx)
;        registerUnit SupplyfromExcel, behaviors, gunit, True, ctx
;        Debug.Print "Asked to do requirements analysis without a ghost, " & _
;            "added Default ghost unit to unitmap in supplymanager."
;    End If
;End If
;

;Public Sub fromExcel(supplystore As TimeStep_ManagerOfSupply, policystore As TimeStep_ManagerOfPolicy, _
;                        parameters As TimeStep_Parameters, behaviors As TimeStep_ManagerOfBehavior, _
;                            ctx As TimeStep_SimContext, Optional ensureghost As Boolean)
;
;Dim gunit As TimeStep_UnitData
;
;UnitsFromSheet "SupplyRecords", supplystore, behaviors, parameters, policystore, ctx
;
;If ensureghost Then
;    If Not supplystore.hasGhosts Then
;        Set gunit = createUnit("Auto", "Ghost", "Anything", "Ghost", 0, "Auto", parameters, policystore)
;        'Decoupled
;        Set gunit = associateUnit(gunit, supplystore, ctx)
;        'decoupled
;        Set supplystore = registerUnit(supplystore, behaviors, gunit, True, ctx)
;        Debug.Print "Asked to do requirements analysis without a ghost, " & _
;            "added Default ghost unit to unitmap in supplymanager."
;    End If
;End If
;
;End Sub
;Public Sub UnitsFromSheet(sheetname As String, supplystore As TimeStep_ManagerOfSupply, behaviors As TimeStep_ManagerOfBehavior, _
;                            parameters As TimeStep_Parameters, policystore As TimeStep_ManagerOfPolicy, _
;                                ctx As TimeStep_SimContext)
;Dim tbl As GenericTable
;
;Set tbl = New GenericTable
;tbl.FromSheet Worksheets(sheetname)
;
;MarathonOpFactory.unitsFromTable tbl, supplystore, behaviors, parameters, policystore, ctx
;
;
;End Sub
;Public Sub UnitsFromDictionary(unitrecords As Dictionary, parameters As TimeStep_Parameters, behaviors As TimeStep_ManagerOfBehavior, _
;                                policystore As TimeStep_ManagerOfPolicy, supplystore As TimeStep_ManagerOfSupply, ctx As TimeStep_SimContext)
;'Decouple
;UnitsFromRecords unitrecords, parameters, behaviors, policystore, supplystore, ctx
;
;End Sub
;



;-----------Obsolete, we don't need to have this....
;'Encapsulate? -> nah, it's already independent.
;Private Function getFollowonBucket(followonbuckets As Dictionary, followoncode As String) As Dictionary
;
;If followoncode = vbNullString Then
;    Err.Raise 101, , "No followon code! Units elligble for followon should have a code!"
;Else
;    With followonbuckets
;        If .exists(followoncode) Then
;            Set getFollowonBucket = .item(followoncode)
;        Else
;            Set getFollowonBucket = New Dictionary
;            .add followoncode, getFollowonBucket
;        End If
;    End With
;End If
;
;End Function

;-----------Obsolete, using version in unit data.

;;use the version associated with the unit....
;Private Function CanDeploy(unit As TimeStep_UnitData) As Boolean
;
;CanDeploy = unit.policy.isDeployable(unit.cycletime)
;
;End Function


;-----------Obsolete, looks like I ditched - double comments.
;''Port again
;''Private Function shouldChangePolicy(uic As TimeStep_UnitData) As Boolean
;''shouldChangePolicy = uic.component <> "AC" And uic.component <> "Ghost"
;''End Function
;'''TOM Hack 13 August 2012
;''Public Function getNearMaxPolicy(policy As IRotationPolicy) As IRotationPolicy
;''If policy.AtomicName = "MaxUtilization" Then
;''    Set getNearMaxPolicy = parent.policymanager.policies("NearMaxUtilization")
;''ElseIf policy.AtomicName = "MaxUtilization_Enabler" Then
;''    Set getNearMaxPolicy = parent.policymanager.policies("NearMaxUtilization_Enabler")
;''Else
;''    Err.Raise 101, , "Can't find policy."
;''End If
;''
;''End Function
;''Public Function checkMaxUtilization() As Boolean
;''checkMaxUtilization = parent.parameters.getKey("TAA1519MaxUtilizationHack")
;''End Function
;'''TOM Hack 13 August 2012
;''Public Function followsMaxUtilization(uic As TimeStep_UnitData) As Boolean
;''Select Case uic.policy.AtomicName
;''    Case "MaxUtilization", "MaxUtilization_Enabler"
;''        followsMaxUtilization = True
;''    Case Else
;''        followsMaxUtilization = False
;''End Select
;''
;''End Function
;''
;'''TOM Hack 13 Aug 2012
;'''Jeff had me put in some special case for handling initial deployment logic.
;''Public Function isFirstDeployment(uic As TimeStep_UnitData) As Boolean
;''isFirstDeployment = Not tags.hasTag("hasdeployed", uic.name)
;''End Function
;''Public Sub tagAsDeployed(uic As TimeStep_UnitData)
;''tags.addTag "hasdeployed", uic.name
;''End Sub
;'''TOM Change 24 April 2012
;'''Ensure that deployable units meet the following criteria:
;'''1.  bogbudget > 0, or , AccruedBOG < bogbudget (have bog left to spend)
;'''2.  deploystart <= cycletime < deploystop (are in a position to spend bog)
;'''3.  cycletime < duration (are in a position to spend bog, that will not bust their cycle.
;'''    This allows late deployments, but not repeated).
;'''process the unused follow-on units, changing their policy to complete cycles.
;''Public Sub ReleaseFollowOns()
;''Dim nm
;''Dim unitptr As TimeStep_UnitData
;''
;''For Each nm In followons
;''    Set unitptr = followons(nm)
;''    removeFollowOn unitptr 'this eliminates the followon code
;''    'TOM Change 24 July 2012 -> With no followon code, this will allow units to try to recover.
;''    unitptr.ChangeState "AbruptWithdraw", 0
;''    UpdateDeployStatus unitptr
;''Next nm
;''
;''End Sub
;''
;'''Tom Change 17 Aug 2012.
;''Public Sub ReleaseMaxUtilizers()
;''Dim nm
;''Dim unitptr As TimeStep_UnitData
;''
;''For Each nm In tags.getSubjects("MaxUtilizer")
;'''Tom Change 20 Aug 2012
;''    Set unitptr = unitmap(nm)
;''    If followons.exists(CStr(nm)) Then
;''        removeFollowOn unitptr 'this eliminates the followon code
;''        'TOM Change 24 July 2012 -> With no followon code, this will allow units to try to recover.
;''        unitptr.ChangeState "AbruptWithdraw", 0
;''    End If
;''    tags.removeTag "MaxUtilizer", CStr(nm)
;''    UpdateDeployStatus unitptr
;''Next nm
;''
;''End Sub
;''Private Sub removeFollowOn(unit As TimeStep_UnitData)
;''Dim ptr As Dictionary
;''Dim removal As Boolean
;''Dim fcode As String
;''
;''fcode = unit.followoncode
;''
;''followons.Remove unit.name
;''With getFollowonBucket(fcode)
;''    Set ptr = .item(unit.src)
;''    ptr.Remove unit.name
;''    If ptr.count = 0 Then .Remove (unit.src)
;''    If .count = 0 Then removal = True
;''End With
;''
;''If removal Then followonbuckets.Remove fcode
;''
;''unit.followoncode = vbNullString
;''End Sub
;'''announce that the unit is in fact following on, remove it from the followons list.
;''Private Sub recordFollowon(unit As TimeStep_UnitData, demand As TimeStep_DemandData)
;''removeFollowOn unit
;'''Decouple
;''parent.trigger FollowingOn, unit.name, demand.name, "Unit " & unit.name & " is following on to demand " & demand.name
;''End Sub
;
;'TOM Change 13 Aug 2012
;'Update every unit in the supply, synchronizes numerical stats.
;
;'Public Sub updateALL(day As Single, Optional unitsToUpdate As Dictionary)
;'Dim update
;'Dim unit As TimeStep_UnitData
;'Dim nm
;'Dim startloc As String
;'Dim finloc As String
;'Dim lupdate As Single
;'
;'If unitsToUpdate Is Nothing Then Set unitsToUpdate = unitmap
;'
;''find pending supply updates for today
;'For Each nm In unitsToUpdate
;'    Set unit = unitmap(nm)
;'    If isEnabled(unit.name) Then  'filters out inactive (not being simulated) units.
;'        'update the unit relative to the time of request
;''        startloc = unit.LocationName
;'        lupdate = lastupdate(unit.name)
;'        If lupdate < day Then
;'            Set unit = unit.update(day - lupdate)
;''            msg = "Updated Unit " & unit.name
;'            If Verbose Then
;'                requestUpdate day, unit
;'            Else
;'                parent.trigger supplyUpdate, name, unit.name, msg & " " & unit.getStats 'supplyPacket(unit.name)
;'            End If
;'        ElseIf lupdate > day Then
;'            Err.Raise 101, , "Should not be updating in the future! "
;'        End If
;'    End If
;'Next nm
;'
;'End Sub