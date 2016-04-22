;A module for operations on units.
(ns marathon.ces.unit
  (:require [spork.sim [simcontext :as sim]]
            [spork.util [general :as gen]
             [metaprogramming :as util]]
            [spork.entitysystem.store :as store]
            [marathon.ces.core :as core]
            [marathon.data [protocols :as pol]
                           [cycle :as cyc]]
            [marathon.policy.policydata :as pold])
  (:use [spork.util.record :only [defrecord+ inc-field dec-field get-vals]]))



;;Operations on marathon.suppply.unitdata
;;======================

;;Okay....
;;Here's where the new behavior context (and possibly messaging functions)
;;come in.  We want to be able to update a unit based on its base behavior.
;;Before, we couldn't wire in the behavior functionality easy, but now,
;;we have a behavior context.  The scheduling system is less important
;;at the moment.

;;Imported from marathon.supply.unitdata 
;pass a message to a unit, telling it to update itself.
;units pass themselves as state, along with some msg, to their referenced 
                ;behavior functions.
;;The behavior should be able to unpack the context into a behavior
;;environment, and handle it all...
;;So, we could pass the unit, the message, and context into the behavior
;;function.  Wrap the crap here...
(defn unit-update [u msg ctx]
  (core/handle-message! ctx u msg))

;;Pending.  When we move to the component entity system, we'll pull this back
;;in.
;;allow units to be entities 
;(extend unitdata IEntity (assoc default-entity :process-msg unit-update))


;Public Sub InitCycles(t As Single)

;Set CurrentCycle = CurrentCycle.NewCycle(0, policy.MaxBOG, policy.MaxDwell, policy.cyclelength, policy.MaxMOB)
;With CurrentCycle
;    .UICname = name
;    .policyname = policy.name
;    .tstart = t
;    .src = src
;    .Component = Component
;End With

;End Sub

;;__TODO__ Rename this to something cleaner.
(defn newcycle [t policy]
  (let [{:keys [maxbog maxdwell cyclelength maxMOB]} policy]    
    (cyc/make-cyclerecord :tstart t 
                          :bogbudget maxbog 
                          :dwell-expected maxdwell 
                          :duration-expected cyclelength 
                          :mob-expected maxMOB)))

(defn initCycles [u t]
  (let [policy (:policy u)
        c (merge (newcycle 0 policy) 
                 {:UICname (:name u)
                  :policyname (:name policy)
                  :tstart t
                  :src (:src u)
                  :component (:component u)})]
    (merge u {:currentcycle c})))

(comment 
;'Account for a name change in the current cycle
;'TOM Change 2 Sep 2011 -> also update the cyclerecord state to account for changed expected bog/dwell/mob
;Public Sub ChangeCycle(t As Single)

;With CurrentCycle
;    .policyname = .policyname & "->" & policy.name
;    .Traversals.add t & "_Policy Change to " & policy.name
;End With
    
;End Sub )
)

(defn ChangeCycle [u t]
  (let [c (:currentycle u)
        p (:policy u)
        newname (str (:policyname c) "->" (:name p))
        traversal (conj (:traversals u) (str t "_Policy Change to " newname))]
    (merge u {:traversals traversal :currentcycle c})))

;Public Sub addduration(dt As Single)
;CurrentCycle.duration = CurrentCycle.duration + dt
;End Sub

(defn add-duration [u t] 
  (merge u {:currentcycle (-> (:currentcycle u) 
                              (inc-field :duration t))
            :cycletime (+ (:cycletime u) t)}))

;Public Sub AddBOG(bogtime As Single)
;CurrentCycle.bog = CurrentCycle.bog + bogtime
;CurrentCycle.bogbudget = CurrentCycle.bogbudget - bogtime
;addduration bogtime
;End Sub

(defn AddBOG [u t]  
  (let [cyclerecord (-> (:currentcycle u) 
                (inc-field :bogtime t) 
                (dec-field :bogbudget t))]   
    (merge u {:currentcycle cyclerecord}) t))

(def add-bog AddBOG)
  
;Public Sub AddDwell(dwelltime As Single, Optional available As Boolean)
;If available Then CurrentCycle.availableTime = CurrentCycle.availableTime + dwelltime
;CurrentCycle.dwell = CurrentCycle.dwell + dwelltime
;addduration dwelltime
;End Sub

(defn- addavailable [cyclerec t] 
  (inc-field :availableTime cyclerec t))

(defn AddDwell [u t & [available]]
  (let [cyclerecord (-> (:currentcycle u) 
                        (inc-field :dwell t))]                                                                    
      (merge u {:currentcycle 
                (if available (addavailable cyclerecord) cyclerecord)})))

(def add-dwell AddDwell)

;Public Sub addMOB(MOBtime As Single)
;CurrentCycle.mob = CurrentCycle.mob + MOBtime
;addduration MOBtime
;End Sub

(defn addMOB [u t]
  (merge u {:currentcycle (inc-field :mob (:currentcycle u) t)}) t)

(def add-MOB addMOB)

;'Note, this will retain a good deal of data...we're keeping track of the unit's arforgen histories..
;Public Sub RecordCycle(day As Single)
;Cycles.add CurrentCycle
;Set CurrentCycle = CurrentCycle.NewCycle(day, policy.MaxBOG, policy.MaxDwell, policy.cyclelength, policy.MaxMOB)
;End Sub

(defn recordcycle [u t]
  (merge u {:cycles (conj (:cycles u) (:currentcycle u)) 
            :currentcycle (apply newcycle t 
                           (get-vals (:policy u) [:MaxBOG :MaxDwell 
                                                  :cyclelength :MaxMOB]))}))
;'TOM change 2 Sep 2011
;'mutate the current cycle object to reflect changes in expectations
;Public Function modify(bogtime As Long, dwelltime As Long, policyduration As Long, Optional MOBtime As Single) As TimeStep_CycleRecord
;Set modify = Me

;With modify
;    .BOGExpected = bogtime
;    .BDRExpected = 1 / ((bogtime + MOBtime) / (policyduration - (bogtime + MOBtime)))
;    .DurationExpected = policyduration
;    .DwellExpected = dwelltime
;    If .DwellExpected > 1095 And MOBtime = 0 Then Err.Raise 101, , "wierd "
;    .MOBexpected = MOBtime
;End With

;End Function

(defn-  modify-cyclerecord
  ([cyclerec bogtime dwelltime policyduration MOBtime] 
    (let [bogmob (+ bogtime MOBtime)]
      (merge cyclerec 
             {:bogexpected bogtime 
              :BDRExpected (/ 1 (/ bogmob (- policyduration bogmob)))
              :DurationExpected policyduration
              :DwellExpected dwelltime
              :MOBExpected MOBtime}))))

;'TOM Change 2 Sep 2011
;Public Sub ModifyCycle(plcy As IRotationPolicy)

;With CurrentCycle
;    'update the record to account for this....when we record the cycle as completed, we want to use expected
;    'values from the new policy.
;    .modify plcy.MaxBOG, plcy.MaxDwell, plcy.cyclelength, plcy.MaxMOB
;End With

;End Sub

(defn modifycycle [u newpolicy]
  (let [vs (get-vals newpolicy [:maxBOG  :maxDwell :cyclelength :maxMOB])
        newrecord (apply modify-cyclerecord (:currentcycle u) vs)]
    (merge u {:currentcycle newrecord})))

;Public Sub ChangeState(newstate As String, deltat As Single, Optional duration As Single)
;Call behavior.ChangeState(Me, newstate, deltat, duration)
;End Sub

;;These both need to be evaluated in a behavior context.
(defn changestate [u newstate dt duration ctx] ((:behavior u) u newstate dt duration)
;  (unit-update 
  )
;;A simple update based on a change in time.
(defn update      [u dt ctx] (unit-update u {:dt dt} ctx))

;Public Function CanDeploy() As Boolean

;With policy
;    CanDeploy = (followoncode <> vbNullString) Or (cycletime >= .StartDeployable And cycletime < .StopDeployable)
;End With

;End Function
;Note -> this is obsolete.....need to use the version in sim.unit instead.
(defn can-deploy? [u & [spawning policy]]
  (let [policy (or policy (:policy u))
        ct (:cycletime u)
        [start stop] [(:StartDeployable policy) (:StopDeployable policy)]] 
  (or (not= (:followoncode u) nil) 
      (and (>= ct start) (< ct stop)))))

;Public Property Get bog() As Single
;bog = CurrentCycle.bog
;End Property
(defn get-bog         [u] (-> u :currentcycle :bog))
(defn get-bog-budget  [u] (-> u :currentcycle :bogbudget))
;Public Property Get dwell() As Single
;dwell = CurrentCycle.dwell
;End Property
(defn get-dwell       [u] (-> u :currentcycle :dwell))
(defn get-cyclelength [u] (-> u :currentcycle :duration-expected))

;;TODO# Verify this statistic is accurate....
(defn normalized-dwell [u] 
 (double  (/ (get u :cycletime)
            (-> u :currentcycle :dwell-expected))))

;Public Property Get BDR() As Single
;BDR = CurrentCycle.BDR
;End Property

;NOTE -> need to define cycle-bdr
(defn- cycle-bdr [c]  0.0)
(defn get-BDR    [u] (cycle-bdr (:currentcycle u)))

;;TODO remove this or switch over to them.  MAybe more clean.  Unknown
;; at this point.

;; (defn defaccessors [paths]
;;   (doseq [p paths]
;;     (let [nm (subs (str p) 1)]
;;       (eval `(defn ~(symbol nm) [obj#] 
;;                (-> obj# ~@path))))))

;; (defaccessors [:currentcycle :bogbudget]
;;               [:currentcycle :bog]
;;               [:currentcycle :dwell])
  
;; (defn bog-budget [u] (-> u :currentcycle :bogbudget))
;; (defn bog        [u] (-> u :currentcycle :bog))
;; (defn dwell      [u] (-> u :currentcycle :dwell))

;Public Sub changePolicy(newpolicy As IRotationPolicy)
;If Not (policy Is Nothing) Then policyStack.add newpolicy

;If policyStack.count = 0 Then Err.Raise 101, , "no policy on stack!"
;'TOM Change 21 July -> account for passage of time between updates!
;If newpolicy.name <> policy.name Then
;'    If name = "32_BCT_AC" Then
;'        Debug.Print "phenomenon"
;'    End If
    
;    'TOM Change 25 July 2011
;    ChangeState "PolicyChange", (parent.getTime - parent.lastupdate(name))
;    'ChangeState "PolicyChange", 0
;End If

;End Sub

;;__TODO__ Move changepolicy to sim.unit
;probably need to figure out a way to thread time for these guys, or 
;establish parent/child relations.
;newpolicy should be an IRotationalPolicy
;Time is not threaded, shift this to the unit level simulation.
(comment 
(defn changePolicy [u newpolicy]
  (if (not= (:name newpolicy) (-> u :policy :name))
    (changestate u "PolicyChange" (- (get-time u) (last-update u)))))
)

;Public Function getStats() As String
;getStats = "Policy:" & policy.AtomicName & " Cycletime: " & cycletime
;End Function

(defn getStats [u] 
  (str "Policy: " (-> u :policy :AtomicName) "Cycletime: " (:cycletime u)))


;'force the unit to broadcast a unitmoved event if it's the first time it moved.
;'Note, we need to ensure that units are cleaned up at the end of day....using end of day
;'logic, specifically, set moved = false for every unit that moved.


;;__TODO__ Move trigger move to sim.unit, this should not be 
;;encapsulated and requires a simulation context.
;note need to define entity-trigger 
(comment 
(defn- trigger-move [u newlocation]
  (let [nm (:name u)
        t (get-time u)
        loc (:locationname u)
        msg (str nm " moved from " loc " to " newlocation " on day " t)]
    (entity-trigger u (make-packet :UnitMoved nm newlocation msg u))))
)                      

          

;;__TODO__ Move changelocation  to sim.unit, this should not be 
;;encapsulated and requires a simulation context.
;note need to define entity-trigger 
(comment 
(defn change-location [u newlocation]
  (if (or (= newlocation (:locationname u)) (not (:moved u)))
        (-> (trigger-move u newlocation) 
             (merge {:moved true :locationname newlocation}))
        u))        
)

(core/stub "Change Location"
   (defn change-location [& args]))      

;;__TODO__ Move changelocation  to sim.unit, this should not be 
;;encapsulated and requires a simulation context.
;note need to define entity-trigger 



;Note -> not implemented.
;Public Sub ChangePolicyPosition(newposition As String)
;End Sub
(core/stub "ChangePolicyPosition"
   (defn change-policy-position [& args]))
;Private Function hasParent() As Boolean
;hasParent = Not (parent Is Nothing)
;End Function



;Note -> not implemented.
;Public Sub ChangePolicyPosition(newposition As String)
;End Sub
(core/stub "ChangePolicyPosition"
   (defn change-policy-position [& args]))
;Private Function hasParent() As Boolean


;;Unit Simulation Ops
;;===================

;TEMPORARILY ADDED for marathon.sim.demand, marathon.sim.policy
(declare  update change-location! 
         re-deploy-unit deploy-unit change-policy) 

;;Copied from supply to avoid circular dependencies....
;;This is problematic.  Should be pulled into supply protocols.
(defn add-unit [supplystore unit]
  (gen/assoc2 supplystore :unitmap (:name unit) unit))

(defn get-policy [u] (:policy u))
(defn short-policy [u]  (assoc u :policy (:name (:policy u))))

;;CHANGE hiding unit policy 
;;Records unit movement between locations.
(defn unit-moved-event!  [unit newlocation ctx]
  (let [nm  (:name unit) 
        loc (:locationname unit)
        msg (core/msg nm " moved from " loc " to " newlocation " on day " (sim/get-time ctx))]
   (sim/trigger-event :UnitMoved nm newlocation  msg (short-policy unit) ctx)))

;;CHANGE hiding unit policy 
;;Records the first time a unit moved.
(defn unit-first-moved-event!  [unit newlocation ctx]
  (let [nm  (:name unit) 
        loc (:locationname unit)
        msg (str nm " Started moving from " loc " to " newlocation " on day " (sim/get-time ctx))]
   (sim/trigger-event :UnitMoved nm newlocation  msg (short-policy unit) ctx)))

;;TODO# implement change-state, so that it actually modifies the
;;entity.  In this case, it's tbased off of unit behavior.
;;Typically resides in unit/change-state, but we probably 
;;want to make it universal for any entity with an FSM.
;;Temporary Stub
;;My resolution here is to implement a messaging system that appends
;;messages for the entity.  Messages are processed at a later time,
;;but they are guaranteed to be process on the entity's logical "thread"
;;before the day is up.  If we ever use coroutines, this will still work nicely.
;;We can just alter the interface to use channels to communicate with the entity.
;;For now, we may only ever have one message for the entity, and we may always process it
;;synchronously.
(defn change-state [entity newstate deltat duration ctx] 
  ;; (do (println (str "*Warning*: marathon.sim.unit/change-state is a stub\n"
  ;;                   "we need to have it wrap an update via the unit's behavior\n"))
  ;;     (sim/trigger-event :Change-State-Stub :EntityFactory 
  ;;       (:name entity) (core/msg  "State Change " (:name entity) "to " newstate)
  ;;       [newstate deltat duration] ctx))
  ;;how about handle-message? 
  (core/handle-message! ctx entity (core/->msg :from entity :to entity)))

;;Think about changing this to use the context.
(defn push-location [unit newlocation]
  (-> unit 
      (assoc :locationname    newlocation)
      (assoc :locationhistory (conj (:locationhistory unit) 
                                    newlocation))))

;;#TODO Figure out a way more effecient way to express this, 
;;we're going to have lots of location changes.  I have a feeling 
;;the associng is going to kill us when we have a lot of movement.
(defn change-location [unit newlocation ctx]
  (if (= newlocation (:locationname unit))
    ctx
    (->> (-> (if (:moved unit)
               (assoc unit :moved true)
               unit)
             (push-location newlocation))
         (store/mergee ctx (:name unit))
         (unit-moved-event! unit newlocation))))

;;TODO -> bring this back in.
;        (unit-first-moved-event! unit newlocation ctx)))))


;Predicate to indicate unit U's ability to bog.
(defn bog-remains? [u]
  (pos? (gen/deep-get u [:currentcycle :bogbudget] 0)))

(defn unit-state [u] (-> u :statedata :currentstate))

;Consults the unit's state to determine if it's in a Bogging or Overlapping state.
;Note, this implicitly hardcodes deployed states.  We could probably yank this out into
;a data-driven definition that's more dynamic.  TBD.
(defn deployed? [u] 
  (case (unit-state u)
    (:bogging :deploying) true
    false))   

;Indicates whether unit u is eligible for a follow on deployment.
;Units eligible for follow on deployments are units that have "any" followon code.
;The followon code indicates the context of the followon deployment.
(defn can-followon? [u] (:followoncode u))

;Determine if a unit falls within the deployable window of a given policy.  If no
;policy is supplied, the unit's associated policy will be consulted.
(defn in-deployable-window? 
  ([u policy] (pol/deployable-by? policy  (:cycletime u)))
  ([u] (in-deployable-window? u (:policy u))))

(defn deployable-window [u] (pol/deployable-window (:policy u)))

;Determines if u is capable of deploying, as a function of u's associated policy.
(defn valid-deployer?
  ([u spawning? policy]
   (if spawning? 
      (pol/deployable-at? policy (:positionpolicy u))
      (and (bog-remains? u) 
           (not (deployed? u)) 
           (or (can-followon? u) (in-deployable-window? u policy)))))
  ([u] (valid-deployer? u nil (:policy u)))) 

;'TOM change 20 April 2012 - > Note, we were using cycletime here, which is the cycletime associated
;'with the unit state, i.e the empirical cycle time.  Since we've got a separation between the cycle
;'length experienced by the unit, and the nominal policy length that unit is operating under (i.e.
;'it changed rotational policies and is currently under a different policy), we need to change the
;'deployment criteria from the empirical or experienced cycletime (unitdata.cycletime), to the notion
;'of cycletime relative to active rotational policy, which is kept in currentcycle.duration.
(defn can-deploy? 
  ([u spawning? policy]  (valid-deployer? u spawning? policy))
  ([u spawning?] (valid-deployer? u spawning? (:policy u)))
  ([u] (valid-deployer? u nil (:policy u))))

;;Added for unit behavior utility
(defn add-traversal [u t from to]
  (assoc u :currentcycle 
         (cyc/cycle-add-traversal 
          (:currentcycle u) 
          t 
          from 
          to)))

;;Useful summary info for unitdata, helpful for debugging.
(defn summary [u]
  (let [{:keys [curstate nextstate timeinstate duration statestart statehistory]}
        (:statedata u)]
    {:name              (:name u) 
     :policy            (:name (:policy  u))
     :positionpolicy    (:positionpolicy u)
     :src               (:src u)
     :positionstate     (pol/get-state (:policy u) (:positionpolicy u))
     :deployable?       (can-deploy?      u)
     :cycletime         (:cycletime       u)
     :dwell             (get-dwell        u)
     :bog               (get-bog          u)
     :location          (:locationname    u)
     :location-history  (:locationhistory u)
     :curstate          curstate
     :nextstate         nextstate
     :timeinstate       timeinstate                                     
     :duration          duration
     :statestart        statestart
     :statehistory      statehistory
   }))

;;#Needs Porting#

;Option Explicit


;Public Sub requestUnitUpdate(t As Single, unit As TimeStep_UnitData, ctx As TimeStep_SimContext)
;SimLib.requestUpdate t, unit.name, UpdateType.supply
;End Sub
;
;Public Sub deployUnit(unit As TimeStep_UnitData, t As Single, Optional deploymentindex As Long, Optional context As TimeStep_SimContext)
;
;unit.deploymentindex = deploymentindex
;wakeAndBogUntilDepleted unit, t, context
;incrementDeployments unit, context
;
;End Sub

(defn unit-update! "Notifies the context of a supply update."
  [nm msg ctx]
  (sim/trigger-event :supplyUpdate  nm nm msg nil ctx))


;;Note: these are just delegating, we could probably extend the
;;protocol to cover the unit, in sim.supply.unitdata
(defn overlap [u] (pol/overlap (get u :policy)))
(defn max-bog [u] (pol/max-bog (get u :policy)))

(defn boggable-time     [u]  (- (get-bog-budget u) (overlap u)))
(defn max-boggable-time [u]  (- (max-bog u) (overlap u)))

;'Wrapper for low level calls to the unit.
;'Assumes a unit is in a followon status.  Imediately puts the unit in a bogging state, with no
;'change in time.  Instantaneous.
;'Bogs the unit for its remaining bog budget.  The unit will ask for an update
;Public Sub keepBoggingUntilDepleted(unit As TimeStep_UnitData, Optional context As TimeStep_SimContext)
;unit.ChangeState "Bogging", 0, unit.CurrentCycle.bogbudget - unit.policy.overlap, context
;End Sub

(defn keep-bogging-until-depleted [u ctx]
  (change-state u :bogging 0 (boggable-time u) ctx))

;'Assumes a unit has not yet bogged, at least not as a follow on
;'Bogs the unit for its remaining bog budget.  Accounts for the passage of time before computing
;'the unit's next update.
;Public Sub wakeAndBogUntilDepleted(unit As TimeStep_UnitData, t As Single, Optional context As TimeStep_SimContext)
;With unit
;    .ChangeState "Bogging", t - SimLib.lastupdate(.name, context), .policy.maxbog - .policy.overlap
;End With
;End Sub
(defn wake-and-bog-until-depleted [u t ctx]
  (change-state u :bogging
      (- t (sim/last-update (get u :name) ctx))
      (max-boggable-time u) ctx))

;'Increment the unit's deployment count for the current cycle.
;Public Sub incrementDeployments(unit As TimeStep_UnitData, Optional context As TimeStep_SimContext)
;unit.CurrentCycle.deployments = unit.CurrentCycle.deployments + 1
;End Sub
(defn increment-deployments [u]
  (core/deep-update u [:currentcycle :deployments] inc))

;'Increment the unit's deployment count for the current cycle.
;Public Sub incrementFollowOns(unit As TimeStep_UnitData, Optional context As TimeStep_SimContext)
;unit.CurrentCycle.followons = unit.CurrentCycle.followons + 1
;End Sub
(defn increment-followons   [u]
  (core/deep-update u [:currentcycle :followons] inc))

;Public Sub reDeployUnit(unit As TimeStep_UnitData, t As Single, Optional deploymentindex As Long, Optional context As TimeStep_SimContext)
;
;incrementFollowOns unit, context
;keepBoggingUntilDepleted unit, context
;unit.followoncode = vbNullString
;incrementDeployments unit, context
;
;End Sub

;;This updates the unit statistics, and alters (drops) the followon
;;code.  re-deployment indicates followon?
(defn  re-deploy-unit [unit t deployment-idx ctx] 
  (let [c    (:current-cycle unit)
        deps (:deployments c)
        new-unit      (-> unit
                         (increment-followons)
                         (increment-deployments)
                         (assoc :followoncode nil))]
    (->>   ctx
          (core/set-unit new-unit)
          (keep-bogging-until-depleted new-unit))))

(defn  deploy-unit [unit t deployment-idx ctx] 
  (let [c    (:current-cycle unit)
        deps (:deployments c)
        new-unit           (-> unit
                               (increment-deployments)
                               )]
    (->> ctx
         (core/set-unit new-unit)
         (keep-bogging-until-depleted new-unit))))


;'Probably pull unit's changelocation into here as well.
;Public Sub changeLocation(unit As TimeStep_UnitData, newlocation As String, Optional context As TimeStep_SimContext)
;
;With unit
;    If newlocation <> .LocationName Then
;        If Not .moved Then
;            'If .hasParent Then
;                .moved = True
;                unitFirstMovedEvent unit, newlocation, context
;            'End If
;        Else
;            unitMovedEvent unit, newlocation, context
;        End If
;        'update the location
;        .LocationName = newlocation
;        'Taken from supplymanager
;        .LocationHistory.add newlocation
;    End If
;End With
;
;End Sub

;'Initialize a unit's cycle history.
;Public Sub InitCycles(u As TimeStep_UnitData, t As Single, Optional ghost As Boolean)
;
;With u
;    Set .CurrentCycle = .CurrentCycle.NewCycle(0, .policy.maxbog, .policy.maxdwell, .policy.cyclelength, .policy.MaxMOB, ghost, .policy.bogbudget)
;    With .CurrentCycle
;        .UICname = u.name
;        .policyname = u.policy.name
;        .tstart = t
;        .src = u.src
;        .component = u.component
;    End With
;End With
;
;End Sub


;'Account for a name change in the current cycle
;'TOM Change 2 Sep 2011 -> also update the cyclerecord state to account for changed expected bog/dwell/mob
;Public Sub ChangeCycle(u As TimeStep_UnitData, t As Single)
;
;With u.CurrentCycle
;    .policyname = .policyname & "->" & u.policy.name
;    .Traversals.add t & "_Policy Change to " & u.policy.name
;End With
;
;End Sub


;'TOM Change 24 April 2012 -> When bogbudget is increased, we take into account the unit's bog history
;'as part of the projection.  Namely, we reduce! the bogbudget correspondingly.  Negative bogbudgets are
;'zeroed, which correctly prevents the unit from bogging any more until reset.
;'TOM Change 20 April 2012 -> Incoporated BOGBudget into this guy, so unit's can now grow their bog budget
;'as composite policies change over time.
;'TOM Change 2 Sep 2011
;Public Sub ModifyCycle(u As TimeStep_UnitData, plcy As IRotationPolicy)
;
;With u.CurrentCycle
;    'update the record to account for this....when we record the cycle as completed, we want to use expected
;    'values from the new policy.
;    .modify plcy.maxbog, plcy.maxdwell, plcy.cyclelength, plcy.MaxMOB, _
;            maxFloat(plcy.bogbudget - u.bog, 0)  'Note the reduction in bogbudget!
;                                               'If unit has already bogged over budget, budget is zeroed!
;End With
;
;End Sub


;'Increment unit u's accumulated cycle bog by bogtime.
;Public Sub AddBOG(u As TimeStep_UnitData, bogtime As Single)
;With u.CurrentCycle
;    .bog = .bog + bogtime
;    
;    'If CurrentCycle.BOG > CurrentCycle.BOGExpected Then Err.Raise 101, , "Added too much bog somewhere"
;    .bogbudget = .bogbudget - bogtime
;    addDuration u, bogtime
;End With
;End Sub


;'Increment unit u's accumulated cycle dwell by dwelltime.
;Public Sub AddDwell(u As TimeStep_UnitData, dwelltime As Single, Optional available As Boolean)
;With u.CurrentCycle
;    If available Then .availableTime = .availableTime + dwelltime
;    .dwell = .dwell + dwelltime
;    addDuration u, dwelltime
;End With
;End Sub


;'Increment unit u's accumulated cycle mobilization time by mobtime.
;Public Sub addMOB(u As TimeStep_UnitData, MOBtime As Single)
;u.CurrentCycle.mob = u.CurrentCycle.mob + MOBtime
;addDuration u, MOBtime
;End Sub


;'Increment unit u's time in the current cycle.
;Public Sub addDuration(u As TimeStep_UnitData, dt As Single)
;u.CurrentCycle.duration = u.CurrentCycle.duration + dt
;End Sub


;'Note, this will retain a good deal of data...we're keeping track of the unit's arforgen histories..
;'Record the current cycle for historical record.  Completing the cycle implies a new cycle starts.
;'Th new cycle inherits properties of the previous cycle.
;Public Sub RecordCycle(u As TimeStep_UnitData, day As Single, Optional ghost As Boolean, Optional cycles As Collection)
;If cycles Is Nothing Then Set cycles = u.cycles
;cycles.add u.CurrentCycle
;With u.policy
;                                      'TODO -> move this into a separate function, not an attached method.
;    Set u.CurrentCycle = u.CurrentCycle.NewCycle(day, .maxbog, .maxdwell, .cyclelength, .MaxMOB, ghost)
;End With
;End Sub






;Public Function getBog(u As TimeStep_UnitData) As Single
;getBog = u.CurrentCycle.bog
;End Function

;Public Function getDwell(u As TimeStep_UnitData) As Single
;getDwell = u.CurrentCycle.dwell
;End Function

;Public Function getBDR(u As TimeStep_UnitData) As Single
;getBDR = u.CurrentCycle.BDR
;End Function

;Public Sub changePolicy(u As TimeStep_UnitData, newpolicy As IRotationPolicy, Optional context As TimeStep_SimContext)
;If Not (u.policy Is Nothing) Then u.policyQueue.add newpolicy
;
;If u.policyQueue.count = 0 Then Err.Raise 101, , "no policy on stack!"
;'TOM Change 21 July -> account for passage of time between updates!
;If newpolicy.name <> u.policy.name Then
;    changeUnitState u, "PolicyChange", (SimLib.getTime(context) - SimLib.lastupdate(u.name, context)), , context
;End If
;
;End Sub

;Public Function getStats(u As TimeStep_UnitData) As String
;getStats = "Policy:" & u.policy.AtomicName & " Cycletime: " & u.cycletime
;End Function

;'Todo -> determine if we need context in this case...
;'Change unit u's current state.  Prompts an update from the unit behavior.
;Public Sub changeUnitState(u As TimeStep_UnitData, newstate As String, deltat As Single, Optional duration As Single, _
;                                Optional context As TimeStep_SimContext, Optional behavior As IUnitBehavior)
;If behavior Is Nothing Then Set behavior = u.behavior
;
;behavior.ChangeState u, newstate, deltat, duration
;End Sub


;'TODO -> flesh this out and experiment.
;'Experimental.  Not yet tested.  Allows for behavior changes.
;Public Sub changeUnitBehavior(u As TimeStep_UnitData, newbehavior As IUnitBehavior, Optional oldbehavior As IUnitBehavior)
;If oldbehavior Is Nothing Then Set oldbehavior = u.behavior
;changeUnitState u, "BehaviorChange", 0, 0, , newbehavior
;End Sub


;'TOM Change 27 MAr 2011 -> aren't really using day anymore, but it's in the parameter for now.
;'Note -> context isn't necessary, as behavior contains that stuff.
;Public Function updateUnit(u As TimeStep_UnitData, deltat As Single, Optional behavior As IUnitBehavior) As TimeStep_UnitData
;If behavior Is Nothing Then Set behavior = u.behavior
;Set updateUnit = behavior.update(deltat, u)
;End Function

;'Public Sub ChangePolicyPosition(newposition As String)
;'Err.Raise 101, , "Not Implemented!"
;'End Sub
;''TODO -> remove this.
;
;'Public Function hasParent() As Boolean
;''Decoupled
;'hasParent = False 'Not (parent Is Nothing)
;'End Function
;'


