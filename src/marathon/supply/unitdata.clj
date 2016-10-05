(ns marathon.supply.unitdata
  (:require [marathon.ces [core :as core]]
            [marathon.data [cycle :as cyc]
             ;[protocols :as p]
             ]
            [marathon.policy [policydata :as pol]])
  (:use [spork.util.record :only [defrecord+ inc-field dec-field get-vals]]))

;;#INCONSISTENCY#
;;unitdata is missing a deployment-index, which is expected for deployment records.

;;TODO# should we extend IRotationaPolicy to these guys? Convenience function...
;record for unitdata state.
(defrecord+ unitdata 
  [name ;unit entity's unique name. corresponds to a UIC 
   src ;unit entity's type, or capability it can supply.
   component ;unit entity's membership in supply.
   policy ;the policy the entity is currently following.
   policystack ;a stack of any pending policy changes.
   behavior ;the behavior the unit uses to interpret policy and messages.
   statedata ;generic state data for the unit's finite state machine.
   cycletime ;the unit's current coordinate in lifecycle space.
   followoncode ;description of the categories this unit serve as a followon to.
   locationname ;the current physical location of the unit.
   positionpolicy ;the current position of the unit in its policy space.
   currentcycle ;the current cycle data structure for the unit.
   cycles ;an ordered collection of the cycles that the unit has completed.
   spawntime ;the time in which the unit spawned.
   oi-title ;the description of the unit.
   locationhistory ;list of all the locations visited.
   dwell-time-when-deployed ;dwell time 
   ]
  ;marathon.data.protocols.IRotationPolicy
  
  )

(def empty-unit
  (-> (make-unitdata)
      (merge {:spawntime -1 :cycletime 0 })))    

;pass a message to a unit, telling it to update itself.
;units pass themselves as state, along with some msg, to their referenced 
;behavior functions.
(defn unit-update [u msg]
  ((:behavior u) u msg))

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
  (let [{:keys [MaxBOG MaxDwell cyclelength MaxMOB]} policy]    
    (cyc/make-cyclerecord t MaxBOG MaxDwell cyclelength MaxMOB)))

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
                            (inc-field :duration t))}))                                             

;Public Sub AddBOG(bogtime As Single)
;CurrentCycle.bog = CurrentCycle.bog + bogtime
;CurrentCycle.bogbudget = CurrentCycle.bogbudget - bogtime
;addduration bogtime
;End Sub

(defn AddBOG [u t]  
  (let [cyclerecord (-> (:currentcycle u) 
                (inc-field :bogtime t) 
                (dec-field :bogbudget t))]   
    (add-duration (merge u {:currentcycle cyclerecord}) t)))
  
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
    (add-duration 
      (merge u {:currentcycle 
                (if available (addavailable cyclerecord) cyclerecord)}))) t)

;Public Sub addMOB(MOBtime As Single)
;CurrentCycle.mob = CurrentCycle.mob + MOBtime
;addduration MOBtime
;End Sub

(defn addMOB [u t]
  (add-duration 
    (merge u {:currentcycle (inc-field :mob (:currentcycle u) t)})) t)

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

(defn changestate [u newstate dt duration]
  ((:behavior u) u newstate dt duration)) 

(defn update [u dt]
  (unit-update u {:dt dt}))

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
(defn get-bog [u] (-> u :currentcycle :bog))

(defn unit-bog-budget [u] (-> u :currentcycle :bogbudget))
;Public Property Get dwell() As Single
;dwell = CurrentCycle.dwell
;End Property
(defn get-dwell [u] (-> u :currentcycle :dwell))

(defn unit-stats [u]
  (let [c (:currentcycle u)]
    {:bog        (:bog   c)
     :dwell      (:dwell c)
     :cycle-time (:cycletime u)
     :duration   (:duration-expected c)}))

;Public Property Get BDR() As Single
;BDR = CurrentCycle.BDR
;End Property

;NOTE -> need to define cycle-bdr
(defn- cycle-bdr [c] 0.0)
(defn get-BDR [u] (cycle-bdr (:currentcycle u)))


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




;'force the unit to broadcast a unitmoved event if it's the first time it moved.
;'Note, we need to ensure that units are cleaned up at the end of day....using end of day
;'logic, specifically, set moved = false for every unit that moved.

;Public Sub ChangeLocation(newlocation As String)
;If newlocation <> LocationName Then
;    If Not moved Then
;        If hasParent Then
;            moved = True
;            parent.parent.trigger UnitMoved, name, newlocation, name & " started moving from " & _
;                        LocationName & " to " & newlocation & " on day " & parent.getTime, , , Me
;        End If
;    Else
;        parent.parent.trigger UnitMoved, name, newlocation, name & " moved from " & _
;                        LocationName & " to " & newlocation & " on day " & parent.getTime, , , Me
;    End If
;    'update the location
;    LocationName = newlocation
;End If
;End Sub


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

