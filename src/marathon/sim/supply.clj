;We're creating circular dependencies....that's bad.
;We need to create some shared protocols, and then implement them.
(ns marathon.sim.supply
  (:require [marathon.demand [demanddata :as d] [demandstore :as store]]
            [marathon.supply [unitdata :as udata]]
            [marathon.sim [demand :as demand]   [policy :as policy]
                          [unit :as u]          [fill :as fill]]           
            [sim [simcontext :as sim] [updates :as updates]]
            [util [tags :as tag]]))

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

;Replaced by request-unit-update!
;Public Sub requestSupplyUpdate(t As Single, unit As TimeStep_UnitData, context As TimeStep_SimContext)
;SimLib.requestUpdate t, "SupplyManager", UpdateType.supply, , context
;End Sub

(defn unit-msg [unit] 
  (str "Updated Unit " (:name unit) " " (udata/getStats unit)))

(defn get-supplystore [ctx] (-> ctx :state :supplystore))

(defn get-unit [supplystore name] (get-in supplystore [:unit-map  name]))
(defn apply-update [supplystore update-packet ctx]
  (let [unitname (:requested-by update-packet)]
    (if (not (enabled? (:tags supplystore) unitname)) ctx 
      (let [unit (get-unit supplystore unitname)]
        (->> ctx       
          (u/update unit (updates/elapsed t  (sim/last-update unitname ctx)))
          (supply-update! supplystore unit (unit-msg unit)))))))
;;Note -> there was another branch here originally....
;;requestUnitUpdate day, unit, context

;NOTE -> ambiguity between state and context here, need to clarify. 
;TOM Change 24 April 2012 -> decoupled the getUpdates....now we pass in a list 
;of updates from outside (usually via the engine), rather than having 
;supplymanager need visibility on it.
(defn manage-supply [day state]
  (let [supply (:supply-store state)
        ctx    (:context state)]
    (if-let [today-updates (get-supply-updates day ctx)]
      (reduce (fn [acc pckt] (apply-update (get-supplystore acc) pckt acc))
              ctx today-updates)
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

;'TOM Change 27 Sep 2012
;Adds meta data to the tags, to identify the unit as being a member of a fenced
;group of supply.  Fenced groups of supply automatically provide a special set 
;of supply that fill functions can utilize when making demand decisions.
(defn tag-as-fenced [tags fencegroup unitname]
  (-> (tag/tag-subject tags fencegroup unitname)
      (tag/tag-subject unitname :fenced)))        

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

;this might be suitable to keep in the supplymanager...
(defn add-src [supply src] 
  (let [scoped (:srcs-in-scope supply)]
    (if (contains? scoped src) supply 
      (assoc-in supply [:srcs-in-scope src] (count scoped)))))

(defn remove-src [supply src] (update-in supply [:srcs-in-scope] dissoc src))

;Public Function assignBehavior(behaviors As TimeStep_ManagerOfBehavior, unit As TimeStep_UnitData, behaviorname As String) As TimeStep_UnitData
;Set assignBehavior = behaviors.assignBehavior(unit, behaviorname)
;End Function
(defn assign-behavior [behaviors unit behaviorname]
  (behaviors/assign-behavior unit behaviorname)) ;WRONG  behaviors doesn't exist yet.

(defn has-behavior? [unit] (not (nil? (:behavior unit))))

;'TOM Change
;'Sub operates on uics to register them as deployable with a dictionary of 
; dictionaries Dictionary<Rule, <UICName,Unitdata>>
;'UICs are tracked by a unique string name now, changed the unitdata structure 
; to reflect this.
;'This sub is only called when necessary, updates the available set, and limits
; the search effort required to find an available uic.
;'UpdateDeployability also enables/disables nodes in the ruleset of the 
; policymanager.
;'Basically, if the deployable bucket is emptied (i.e. we update a unit's 
; deployability, removing it from any number of buckets,

;Private Sub UpdateDeployability(uic As TimeStep_UnitData, buckets As Dictionary, Optional followon As Boolean, _
;                                    Optional spawning As Boolean, Optional context As TimeStep_SimContext)
;
;Dim stock As Dictionary
;Dim packet As Dictionary
;
;'TODO -> rip out all the stuff from unitdata, particularly CanDeploy and 
; friends...it's a step above a rat's nest.

;With uic
;        If .PositionPolicy = vbNullString Then 'Tom Change 24 May
;            Err.Raise 101, , "invalid position!"  'Tom Change 24 May
;        'TOM Change 24 April 2012 -> using a single notion of deployable now, 
;        'derived from the unit.

;        'Note how the old check just used the position in the policy to 
;        'determine deployability.
;        'Well, we want to incorporate a stricter notion of deployable, in that 
;        'we also account for bogbudget and whatnot....The problem is that 
;        'during spawning, we don't initialize cycles off the bat, so one of the 
;        'new criteria fails (bogbudget is 0).
;        
;        'CHECK -> maybe yank .canDeploy out of the uic's methods, make it more 
;        'functional
;        ElseIf .CanDeploy(spawning) Or followon Then
;            If Not followon Then
;                If .PositionPolicy = "Recovery" Then
;                    Err.Raise 101, , "Recovery is not deployable"
;                End If
;                'Decoupled
;                 SimLib.triggerEvent NewDeployable, "SupplyManager", .name, _
;                    "Unit " & .name & " at position " & .PositionPolicy 
;                       & " is deployable", , context
;            Else
;                'Decoupled
;                 SimLib.triggerEvent NewFollowOn, "SupplyManager", .name, _
;                    "Unit " & .name & " able to followon for demandgroup " 
;                       & .followoncode, , context
;            End If
;
;            If buckets.exists(.src) Then
;                'Decoupled
;                 SimLib.triggerEvent MoreSRCAvailable, "SupplyManager", .src, _
;                    "Unit " & .name & " at position " & .PositionPolicy & _
;                        " has just been added to deployables for SRC " 
;                             & .src, , context
;
;                Set stock = buckets.item(.src)
;                'TOM change 21 july 2011
;                If Not stock.exists(.name) Then stock.add .name, uic
;            Else
;                'Decoupled
;                 SimLib.triggerEvent NewSRCAvailable, "SupplyManager", .src, _
;                    "A new category of SRC now has deployable supply " 
;                       & .src, , context
;
;                'TOM note this may be allocating alot.
;                Set stock = New Dictionary
;                stock.add .name, uic
;                buckets.add .src, stock
;                'Tom Change 24 May
;                'Decoupled
;                 SimLib.triggerEvent NewDeployable, "SupplyManager", .name, _
;                    "Unit " & .name & " at position " & .PositionPolicy & _
;                        " has just been added to deployables for SRC " 
;                            & .src, , context
;            End If
;        Else 'unit is not deployable
;            'Decoupled
;            SimLib.triggerEvent NotDeployable, "SupplyManager", .name, _
;                "Unit " & .name & " at position " & .PositionPolicy & " is no 
;                        longer deployable", , context;;
;            If buckets.exists(.src) = True Then
;                Set stock = buckets.item(.src)
;                If stock.exists(.name) Then stock.Remove (.name)
;                If stock.count = 0 Then
;                    'Decoupled
;                     SimLib.triggerEvent outofstock, "SupplyManager", .src, _
;                        "SRC " & .src & " has 0 deployable supply", "SOURCE_" 
;                            & .src, context
;                    Set stock = Nothing
;                    buckets.Remove (.src) 'mutation!
;                End If
;            End If
;        End If
;End With
;
;
;End Sub



(defn update-deploy-status [supply unit [followon? spawning? ctx]]
  (let [bucket (if followon?       
                 (get-followon-bucket (:followonbuckets supply) 
                                      (:followoncode unit))
                 (:deployable-buckets supply))] 
    (update-deployability unit buckets  followon? spawning? ctx)))



;Note -> the signature for this originally returned the supply, bet we're not 
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

;Public Function NewUnit(supply As TimeStep_ManagerOfSupply, parameters As TimeStep_Parameters, policystore As TimeStep_ManagerOfPolicy, behaviors As TimeStep_ManagerOfBehavior, name As String, src As String, OITitle As String, component As String, _
;                            cycletime As Single, policy As String, Optional behavior As IUnitBehavior)
;Dim unit As TimeStep_UnitData
;Set unit = createUnit(name, src, OITitle, component, cycletime, policy, parameters, policystore, behavior)
;Set supply = registerUnit(supply, behaviors, unit)
;
;End Function



;
;'Encapsulate.

;'Encapsulate.

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

;
;Private Function CanDeploy(unit As TimeStep_UnitData) As Boolean
;
;CanDeploy = unit.policy.isDeployable(unit.cycletime)
;
;End Function
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
;'TOM Change 27 Sep 2012 -> using tags to delineate fence states, including one-time fences, specifically
;'for future force gen stuff.
;Public Sub dropFence(tags As GenericTags, unitname As String)
;If tags.hasTag("one-time-fence", unitname) Then
;    If tags.hasTag("fenced", unitname) Then
;        tags.removeTag "fenced", unitname
;        tags.addTag "dropped-fence", unitname
;    End If
;End If
;    
;End Sub
;'TOM Hack 13 August 2012
;Public Function getNearMaxPolicy(policy As IRotationPolicy, _
;                                    policystore As TimeStep_ManagerOfPolicy) As IRotationPolicy
;If policy.AtomicName = "MaxUtilization" Then
;    Set getNearMaxPolicy = policystore.policies("NearMaxUtilization")
;ElseIf policy.AtomicName = "MaxUtilization_Enabler" Then
;    Set getNearMaxPolicy = policystore.policies("NearMaxUtilization_Enabler")
;Else
;    Err.Raise 101, , "Can't find policy."
;End If
;
;End Function
;Private Function shouldChangePolicy(uic As TimeStep_UnitData) As Boolean
;shouldChangePolicy = uic.component <> "AC" And uic.component <> "Ghost"
;End Function
;Public Function checkMaxUtilization(parameters As TimeStep_Parameters) As Boolean
;checkMaxUtilization = parameters.getKey("TAA1519MaxUtilizationHack")
;End Function
;Public Sub tagAsDeployed(uic As TimeStep_UnitData, supply As TimeStep_ManagerOfSupply)
;supply.tags.addTag "hasdeployed", uic.name
;End Sub
;
;Public Function getNextDeploymentID(supply As TimeStep_ManagerOfSupply) As Long
;With supply
;    .uniqueDeployments = .uniqueDeployments + 1
;    getNextDeploymentID = .uniqueDeployments
;End With
;End Function
;'TOM Hack 13 Aug 2012
;'Jeff had me put in some special case for handling initial deployment logic.
;Public Function isFirstDeployment(uic As TimeStep_UnitData, supply As TimeStep_ManagerOfSupply) As Boolean
;isFirstDeployment = Not supply.tags.hasTag("hasdeployed", uic.name)
;End Function
;

;
;'process the unused follow-on units, changing their policy to complete cycles.
;Public Sub ReleaseFollowOns(supply As TimeStep_ManagerOfSupply, Optional context As TimeStep_SimContext)
;Dim nm
;Dim unitptr As TimeStep_UnitData
;'
;'For Each nm In supply.followons
;'    Set unitptr = supply.followons(nm)
;'    removeFollowOn supply, unitptr
;'    'Tom change 18 July 2012
;'    'We allow the units to pass through a reentry state, to see if they can recover and re-enter
;'    'the available pool, rather than pushing them straight to a Reset state.
;'    'unitptr.ChangeState "Reset", 0
;'    'change the unit's position in its policy to enable possible reentry
;'    '  even if the policy does not have an explicit reentry state.
;'    unitptr.PositionPolicy = "ReEntry"
;'    unitptr.ChangeState "ReEntry", 0, , context
;'    UpdateDeployStatus supply, unitptr, False, False, context
;'Next nm
;
;For Each nm In supply.followons
;    Set unitptr = supply.followons(nm)
;    removeFollowOn supply, unitptr  'this eliminates the followon code
;    'TOM Change 24 July 2012 -> With no followon code, this will allow units to try to recover.
;    unitptr.ChangeState "AbruptWithdraw", 0, , context
;    UpdateDeployStatus supply, unitptr, , , context
;Next nm
;
;End Sub
;'Tom Change 17 Aug 2012.
;Public Sub ReleaseMaxUtilizers(supply As TimeStep_ManagerOfSupply, ctx As TimeStep_SimContext)
;Dim nm
;Dim unitptr As TimeStep_UnitData
;
;With supply
;    For Each nm In .tags.getSubjects("MaxUtilizer")
;    'Tom Change 20 Aug 2012
;        Set unitptr = .unitmap(nm)
;        If .followons.exists(CStr(nm)) Then
;            removeFollowOn supply, unitptr 'this eliminates the followon code
;            'TOM Change 24 July 2012 -> With no followon code, this will allow units to try to recover.
;            unitptr.ChangeState "AbruptWithdraw", 0, , ctx
;        End If
;        .tags.removeTag "MaxUtilizer", CStr(nm)
;        'TOM change 9 Sep 2012, injected a false value.
;        UpdateDeployStatus supply, unitptr, False, , ctx
;    Next nm
;End With
;
;End Sub
;Private Sub removeFollowOn(supply As TimeStep_ManagerOfSupply, unit As TimeStep_UnitData)
;Dim ptr As Dictionary
;Dim removal As Boolean
;Dim fcode As String
;
;fcode = unit.followoncode
;
;supply.followons.Remove unit.name
;With getFollowonBucket(supply.followonbuckets, fcode)
;    Set ptr = .item(unit.src)
;    ptr.Remove unit.name
;    If ptr.count = 0 Then .Remove (unit.src)
;    If .count = 0 Then removal = True
;End With
;
;'mutating the follow on buckets, this is okay...
;If removal Then supply.followonbuckets.Remove fcode
;
;unit.followoncode = vbNullString
;'resetFollowOn unit
;End Sub
;'announce that the unit is in fact following on, remove it from the followons list.
;Private Sub recordFollowon(supply As TimeStep_ManagerOfSupply, unit As TimeStep_UnitData, demand As TimeStep_DemandData, Optional context As TimeStep_SimContext)
;removeFollowOn supply, unit
;'Decoupled
;'triggerEvent FollowingOn, unit.name, demand.name, "Unit " & unit.name & " is following on to demand " & demand.name, , context
;unitFollowOnEvent unit, demand, context
;End Sub
;'When a unit engages in a followon deployment, we notify the event context.
;'Simple declarative event description for wrapping low level followon event notification.
;Public Sub unitFollowOnEvent(unit As TimeStep_UnitData, demand As TimeStep_DemandData, Optional context As TimeStep_SimContext)
;triggerEvent FollowingOn, unit.name, demand.name, "Unit " & unit.name & " is following on to demand " & demand.name, , context
;End Sub
;'TODO -> formalize dependencies and pre-compilation checks....
;'already decoupled.
;Public Function CanSimulate(supply As TimeStep_ManagerOfSupply) As Boolean
;CanSimulate = supply.tags.getSubjects("Enabled").count > 0
;End Function
;
;
;'TOM Change -> removed due to slowdown
;''Public Function supplyPacket(unitname As String) As Dictionary
;''Set supplyPacket = New Dictionary
;''supplyPacket.add "Updated", unitname
;''End Function
;

;
;
;'Decoupled.
;
;'procedure that allows us to, using the fillgraph, derive a set of tags whose associated units
;'should be deactivated.  if removal is true, the units will be removed from memory as well.
;'in cases where there are a lot of units, this may be preferable
;Public Sub scopeSupply(supply As TimeStep_ManagerOfSupply, disableTags As Dictionary, Optional removal As Boolean)
;Dim subjects As Dictionary
;Dim subject As String
;Dim subj
;Dim tg
;
;With supply
;    For Each tg In disableTags
;        Set subjects = .tags.getSubjects(CStr(tg))
;        For Each subj In subjects
;            subject = CStr(subj)
;            If .unitmap.exists(subject) Then
;                disable .tags, subject
;                If removal Then removeUnit supply, subject
;            End If
;        Next subj
;    Next tg
;End With
;
;End Sub
;'TOM Note 27 Mar 2011 -> we might have to clean up other tables too...
;'Decoupled.
;Public Sub removeUnit(supply As TimeStep_ManagerOfSupply, unitname As String)
;
;Static unitptr As IVolatile
;
;With supply.unitmap
;    If .exists(unitname) Then
;        Set unitptr = .item(unitname)
;        removeSRC supply, .item(unitname).src
;        .Remove (unitname)
;        unitptr.Terminate
;    End If
;End With
;
;End Sub
;'TOM Change 3 Jan 2011
;'aux function for logging/recording the fact that a unit changed locations
;'TODO -> it'd be nice to figure out how to unify this reporting, right now LogMove gets to
;'reach directly into the tables of outputmanager and manipulate. This is fast and simple, but it's not
;'pure ....
;'One current problem is -> we have to transform fromloc/toloc into something palatable for trending ...
;Public Sub LogMove(t As Single, fromloc As String, toloc As String, unit As TimeStep_UnitData, _
;                        Optional duration As Single, Optional context As TimeStep_SimContext)
;
;SimLib.triggerEvent UnitMoved, unit.name, toloc, "", unit, context
;'TODO -> check to see if this is equivalent.  I think it is.  If so, prefer.
;'MarathonOpUnit.unitMovedEvent unit, toloc, context
;
;End Sub
;
;'TODO -> This should be renamed like positionEvent or something.
;'Main dependencies are in the unit Behaviors.
;'Unit behaviors currently use parent to refer to a supply manager.
;'We can probably do better than this.
;'Actually, unit behaviors aren't maintaining any state....
;'So we can probably just plug them in as modules....they're all pure functions.
;
;'TOM Change 6 June 2011 -> Added logging for unit positioning specifically..
;Public Sub LogPosition(t As Single, frompos As String, topos As String, unit As TimeStep_UnitData, Optional duration As Single, Optional context As TimeStep_SimContext)
;
;
;'If SupplyTraffic Then
;    'Decouple
;    triggerEvent PositionUnit, "SupplyManager", unit.name, _
;        "UIC " & unit.name & " has repositioned from " & frompos & " to " & topos, , context  'record the move
;'End If
;
;End Sub
;
;'TOM change 3 Jan 2011
;'aux function for logging/recording the fact that a unit deployed
;'transfer 1 item
;Public Sub LogDeployment(t As Single, fromname As String, demand As TimeStep_DemandData, unit As TimeStep_UnitData, _
;                            fillcount As Long, fill As TimeStep_Fill, deploydate As Date, period As String, Optional context As TimeStep_SimContext, Optional msg As String)
;
;Dim newrec As GenericRecord
;Dim trendtarget As String
;'TOM Change 4 Jan 2011
;
;Dim toname As String
;'TOM Change 4 Jan 2011
;toname = demand.name  'TOM note -> this points to individual instances of the demand. We could produce an aggregated
;
;'TOM Change
;'msg =
;
;'TOM fix 10 Sep 2012 -> wasn't passing in FromName, causing empty fields in deploy
;'records.
;'Decoupled
;SimLib.triggerEvent _
;    deploy, "SupplyManager", unit.name, msg, _
;             newdict("FromLoc", fromname, "Unit", unit, "Demand", demand, _
;                     "Fill", fill, "FillCount", fillcount, _
;                     "Period", period, "t", t, "DeployDate", deploydate), context
;
;End Sub

;
;'TODO -> replace this call to a direct call for getTime, from the simlib module.
;Public Function getTime(context As TimeStep_SimContext) As Single
;'Decouple
;'getTime = parent.CurrentTime
;getTime = SimLib.getTime(context)
;End Function


;Public Function getSources(supply As TimeStep_ManagerOfSupply) As Dictionary
;Set getSources = supply.tags.getSubjects("Sources")
;End Function

;'TODO -> update calls to this guy in behaviors.  Should fail, currently.
;'sub to register a set of units that need to be utilized, or sent to reset.
;Public Sub addFollowOn(supply As TimeStep_ManagerOfSupply, unit As TimeStep_UnitData, _
;                        context As TimeStep_SimContext)
;
;supply.followons.add unit.name, unit
;UpdateDeployStatus supply, unit, True, , context
;
;End Sub
;Public Function lastupdate(unitname As String, ctx As TimeStep_SimContext) As Single
;'Decoupled
;lastupdate = SimLib.lastupdate(unitname, ctx)
;End Function
;
;Public Function multipleGhosts(supplytags As GenericTags) As Boolean
;multipleGhosts = supplytags.getSubjects("SOURCE_Ghost").count > 1
;End Function
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
;'TOM Change 13 Aug 2012
;'Update every unit in the supply, synchronizes numerical stats.
;Public Sub updateALL(day As Single, supplystore As TimeStep_ManagerOfSupply, ctx As TimeStep_SimContext, Optional unitsToUpdate As Dictionary)  ' supplystore as TimeStep_ManagerOfSupply )
;Dim update
;Dim unit As TimeStep_UnitData
;Dim nm
;Dim startloc As String
;Dim finloc As String
;Dim lupdate As Single
;Dim msg As String 'tom added 10 Sep 2012
;
;If unitsToUpdate Is Nothing Then Set unitsToUpdate = supplystore.unitmap
;
;'find pending supply updates for today
;With supplystore.unitmap
;    For Each nm In unitsToUpdate
;        Set unit = .item(nm)
;        If isEnabled(supplystore.tags, unit.name) Then   'filters out inactive (not being simulated) units.
;            'update the unit relative to the time of request
;    '        startloc = unit.LocationName
;            lupdate = lastupdate(unit.name, ctx)
;            If lupdate < day Then
;                Set unit = unit.update(day - lupdate)
;                'TOM fix 10 Sep 2012, was using the same msg (old)
;                msg = "Updated Unit " & unit.name
;                If supplystore.Verbose Then
;                    'requestUpdate day, unit.name, UpdateType.supply, , ctx
;                    'TOM chang 10 Sep 2012
;                    requestSupplyUpdate day, unit, ctx
;                Else
;                    SimLib.triggerEvent supplyUpdate, "SupplyManager", unit.name, msg & " " & unit.getStats, , ctx 'supplyPacket(unit.name)
;                End If
;            ElseIf lupdate > day Then
;                Err.Raise 101, , "Should not be updating in the future! "
;            End If
;        End If
;    Next nm
;End With
;
;End Sub
;
;
;
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

