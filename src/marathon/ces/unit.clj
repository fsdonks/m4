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


;;Utils
;;=====
;;convenience macro to help with intermittent debugging.
(defmacro binding-when [pred binding & expr]
  `(if ~pred
     (binding [~@binding]
       ~@expr)
     ~@expr))


;;Operations on marathon.suppply.unitdata
;;=======================================

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
(defn unit-update [e ctx]
  (let [_     (spork.ai.core/debug [:>>>>>>>>>>Begin-Update (:name e) (:last-update e)])
        res   (core/handle-message! ctx e
                                   (core/->msg (:name e) (:name e)
                                               (core/get-time ctx)
                                               :update
                                               nil))]
    (spork.ai.core/debug [:<<<<<<<<<<End-Update (:name e) (store/gete res (:name e) :last-update)])
    res))

(defn followon-code [u]
  (when-let [fc (:followoncode u)]
    (when (not (= fc ""))
      fc)))

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

;;Note: This fails with composite policies.
;;Problem is we use key destructuring to access the
;;maxbog,maxdwell, cyclength, maxMOB vals, but
;;in composite policies, they don't square.

;;To accomodate comparing potentially infinite
;;and finite cycle lengths, we introduce
;;a finiteness check, deriving a 'finite'
;;value for max-dwell for the cycle if
;;the underlying policy provides an
;;infinite maximum dwell time.  This
;;creates a better basis for normalized
;;dwell computation.  Addresses issue
;;from commit bbe385e5 regarding
;;the Finite Cycle Length Goal proposal.
(defn cycle-stats [policy]
  {:max-bog      (pol/max-bog policy)
   :max-dwell    (core/finite-else #_(pol/max-dwell policy)
                                   (pol/expected-dwell policy)
                                   core/+default-cycle-length+)
   :cycle-length (pol/cycle-length policy)
   :max-mob      (pol/max-mob policy)})

;;__TODO__ Rename this to something cleaner.
(defn newcycle [t policy]
  (let [{:keys [max-bog max-dwell cycle-length max-mob]} (cycle-stats policy)]    
    (cyc/make-cyclerecord :tstart t 
                          :bogbudget max-bog 
                          :dwell-expected max-dwell 
                          :duration-expected cycle-length 
                          :mob-expected max-mob)))

;;Note: this is causing problems, we're dropping the
;;currentcycle...
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

;;PERFORMANCE NOTE: This is a hotspot;
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
                        (inc-field :bog t) 
                        (dec-field :bogbudget t))]   
    (merge u {:currentcycle cyclerecord})))

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

;Public Sub ChangeState(newstate As String, deltat As Single, Optional duration As Single)
;Call behavior.ChangeState(Me, newstate, deltat, duration)
;End Sub

;Public Function CanDeploy() As Boolean

;With policy
;    CanDeploy = (followoncode <> vbNullString) Or (cycletime >= .StartDeployable And cycletime < .StopDeployable)
;End With

                                        ;End Function


;Note -> this is obsolete.....need to use the version in sim.unit instead.
;; (defn can-deploy? [u & [spawning policy]]
;;   (let [policy (or policy (:policy u))
;;         ct (:cycletime u)
;;         [start stop] [(:StartDeployable policy) (:StopDeployable policy)]] 
;;   (or (not= (:followoncode u) nil) 
;;       (and (>= ct start) (< ct stop)))))

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

;'Note, this will retain a good deal of data...we're keeping track of the unit's arforgen histories..
;Public Sub RecordCycle(day As Single)
;Cycles.add CurrentCycle
;Set CurrentCycle = CurrentCycle.NewCycle(day, policy.MaxBOG, policy.MaxDwell, policy.cyclelength, policy.MaxMOB)
;End Sub

(defn recordcycle [u t]
  (merge u {:cycles (conj (:cycles u) (:currentcycle u)) 
            :currentcycle (newcycle t  (:policy u) )}))
                                   
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
             {:bog-expected bogtime 
              :bog-to-dwell-expected (/ 1 (/ bogmob (- policyduration bogmob)))
              :duration-expected policyduration
              :dwell-expected  dwelltime
              :mob-expected MOBtime}))))

;;This is from data.cycle, we need to swap this out for modify-cyclerecord,
;;or delegate to it, since we're using data.cycle upon instantiation.
;;Currently the fields don't match up, which maybe causing us some problems
;;down the line when doing thing like deployability checks.
#_(defn ^cyclerecord cycle-modify 
  "Modifies oldcycle, assumably in the context of a policy change.  Returns the 
   result of the modification, as a new cycle."
  [cyclerec bogtime dwelltime policyduration & [MOBtime bogbudget]]
  (if (and (> dwelltime 1095)  (zero? MOBtime) (not (= :inf dwelltime))
           (throw (Exception. "Expected dwell time is abnormal...")))     
    (with-record cyclerec
      :bog-expected  bogtime
      :bog-to-dwell-expected (/ 1 (/ (+ bogtime MOBtime) 
                                     ( - policyduration (+ bogtime  MOBtime))))
      :duration-expected policyduration
      :dwell-expected    dwelltime
      :mob-expected      MOBtime
      :bogbudget         bogbudget)))

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
;;End Sub


(defn compute-bogbudget
  "Given a new policy to ostensibly change to,
   compares the unit's current cycle bog to determine
   what its prospective bog would be in the new cycle.
   If the bog budget would be exhausted in the new policy,
   floors to 0."
  [u newpolicy]
  (max (- (pol/bog-budget newpolicy)
          (get-bog u))
       0))

;;Modified to correspond with new cycle-stats wrapper function.
;;note: this implementation clashed with the original mod-cycle fn,
;;in that bogbudget is not updated.  This flew under the radar for
;;the bulk of vnv testing, since we never looked at policies
;;that deviated bogbudget from maxbog...until recently.  Need to
;;ensure that bogbudget is updated as the cycle changes...
(defn modify-cycle [u newpolicy]
  (let [{:keys [max-bog max-dwell cycle-length max-mob]} (cycle-stats newpolicy)
        ;;Note the reduction in bogbudget!
        ;;If unit has already bogged over budget, budget is zeroed!
        bogbudget   (compute-bogbudget u newpolicy)
        newrecord   (cyc/modify-cyclerecord (:currentcycle u) max-bog max-dwell
                                            cycle-length max-mob bogbudget)]
    (assoc u :currentcycle newrecord)))


;;BUG NOTE: We have an issue with computing normalized dwell
;;when the unit's last-update is not consistent with the
;;state of the system (i.e. the current time).  This happens
;;when units are out of sync and actions that depend on
;;accumulated statistics pop up, in particular, ordering
;;units for consideration in filling demand.

;;Since dwell is the primary arbiter, and we can compute
;;dwell as a function of cycletime, we can establish a
;;quick projection of normalized dwell relative to the
;;time elapsed since last-update.  If we add a component
;;that indicates last-look, rather than last-update,
;;we can have a simple means of projecting statistics like
;;cycletime and dwell, without having to update the whole
;;unit.

;;We now have a component, :dt, which tells us if any
;;time has elapsed for the unit entity.  :dt comes to
;;us when we use ces.core/current-entity.

;;TODO# Verify this statistic is accurate....
#_(defn normalized-dwell [u]
 (double  (/ (+ (get u :cycletime) (get u :dt 0))
             (-> u :currentcycle :dwell-expected))))

(defn normalized-dwell [u]
 (core/float-trunc  (/ (+ (get u :cycletime) (get u :dt 0))
                       (-> u :currentcycle :dwell-expected))
                    6))


;;trying to boost speed.
(comment
  (defn normalized-dwell [^clojure.lang.ILookup u]
    (let [ct                         (.valAt u :cycletime)
          ^clojure.lang.ILookup cc   (.valAt u :currentcycle)]
   (/ (double ct)
      (.valAt cc :dwell-expected))))
  
;;maybe faster?
(defn normalized-dwell [u] 
  (/ (* 1.0 (get u :cycletime))
     (-> u :currentcycle :dwell-expected)))
)

;Public Property Get BDR() As Single
;BDR = CurrentCycle.BDR
;End Property

;NOTE -> need to define cycle-bdr
(defn- cycle-bdr [c]  0.0)
(defn get-BDR    [u] (cycle-bdr (:currentcycle u)))

;;TODO remove
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
(declare  ;update
          change-location! 
         re-deploy-unit deploy-unit change-policy) 

;;  220:   Public Sub changePolicy(newpolicy As IRotationPolicy)
;;  221:   If Not (policy Is Nothing) Then policyStack.add newpolicy
;;  222:  
;;  223:   If policyStack.count = 0 Then Err.Raise 101, , "no policy on stack!"
;;  224:  'TOM Change 21 July -> account for passage of time between updates!
;;  225:   If newpolicy.name <> policy.name Then 
;;  232:       ChangeState "PolicyChange", (parent.getTime - parent.lastupdate(name))
;;  234:   End If
;; 236:   End Sub

;;Possibly OBE.
#_(defn change-policy [unit newpolicy entity ctx]
    (let [t (core/get-time ctx)]
      (change-state unit :policy-change-state
                    (- t  (get unit :last-update 0))
                    (max-boggable-time unit) ctx)))

#_(defn change-policy [e next-policy ctx]
  (core/handle-message! ctx e
       (core/->msg (:name e) (:name e)
                   (core/get-time ctx)
                   :change-policy
                   {:policy-change {:next-policy next-policy}})))

;;refactored to use send!! wrapper.

;;Sends the entity a message that queues up handling policy
;;changes.
(defn change-policy [e next-policy ctx]
  (core/send!! e :change-policy
    {:next-policy next-policy} ctx))

(defn change-cycle
  "Update the statistics for the current cyclerecord to record 
   a change."
  [unit t]
  (let [pname (pol/atomic-name (:policy unit))
        c (-> (:currentcycle unit)
              (update :policy-name #(str % "->" pname))
              (update :traversals #(conj % (str t "_Policy Change to " pname))))]
    (assoc unit :currentcycle c)))

 ;;93:   Public Sub ChangeCycle(t As Single)
 ;;94:  
 ;;95:   With CurrentCycle
 ;;96:       .policyname = .policyname & "->" & policy.name
 ;;97:       .Traversals.add t & "_Policy Change to " & policy.name
 ;;98:   End With
 ;;99:       
 ;;100:   End Sub

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
   (core/trigger-event :UnitMoved nm newlocation  msg (short-policy unit) ctx)))

;;CHANGE hiding unit policy 
;;Records the first time a unit moved.
(defn unit-first-moved-event!  [unit newlocation ctx]
  (let [nm  (:name unit) 
        loc (:locationname unit)
        msg (str nm " Started moving from " loc " to " newlocation " on day " (sim/get-time ctx))]
   (core/trigger-event :UnitMoved nm newlocation  msg (short-policy unit) ctx)))

(def ^:dynamic *uic* nil)


    
    
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
;;synchronously.  This presents a functional api around the former OOP method call.
;;The old way used to be to update the state externally, set up the transition, and
;;then invoke the change-state function on the unit.  We'll try to tell the entity
;;what to do more often, and let the behavior be decoupled via messaging.
;;we may no longer need the deltat arg...
(defn change-state [entity newstate deltat duration ctx]
  (binding-when (or *uic* (= newstate :abrupt-withdraw))
    ;;conditional binds for debugging info
    [spork.ai.core/*debug* (or spork.ai.core/*debug*
                               (= (:name entity) *uic*)
                               (= newstate :abruptwithdraw))]
    ;;affect change-state by message passing evaluation.
    (core/handle-message! ctx entity
                          (core/->msg (:name entity) (:name entity)
                                      (core/get-time ctx)
                                      :change-state
                                      {:newstate newstate
                                       :deltat deltat
                                       :duration duration
                                       }))))

;;instructs the entity to execute a logical move; possibly changing
;;the physical location, and changing the state.
(defn move-to [entity location position duration ctx]
  (binding-when *uic*
    [spork.ai.core/*debug* (or spork.ai.core/*debug*
                               (= (:name entity) *uic*))]
    (core/handle-message! ctx entity
      (core/->msg (:name entity) (:name entity)
                  (core/get-time ctx)
                  :move
                  {:next-location location
                   :next-position position
                   :wait-time duration
                   }))))

;;wrapper around our spawning functionality.
(defn spawn [ent ctx]
  (core/handle-message! ctx  ent
                        (core/->msg (:name ent) (:name ent) 0 :spawn)))

;;Think about changing this to use the context.
(defn push-location [unit newlocation]
  (-> unit 
      (assoc :locationname    newlocation)
      (assoc :locationhistory (conj (:locationhistory unit) 
                                    newlocation))))


(defn change-location [unit newlocation]
  (if (= newlocation (:locationname unit))
    unit
    (->> (-> (if (:moved unit)
               (assoc unit :moved true)
               unit)
             (push-location newlocation)))))

;;#TODO Figure out a way more effecient way to express this, 
;;we're going to have lots of location changes.  I have a feeling 
;;the associng is going to kill us when we have a lot of movement.
(defn change-location! [unit newlocation ctx]
  (if (= newlocation (:locationname unit))
    ctx
    (-> ctx
    (store/mergee (:name unit) (change-location unit newlocation))
    (unit-moved-event! unit newlocation))))

;;TODO -> bring this back in.
;        (unit-first-moved-event! unit newlocation ctx)))))


;Predicate to indicate unit U's ability to bog.
(defn bog-remains? [u]
  (pos? (gen/deep-get u [:currentcycle :bogbudget] 0)))

(defn unit-state [u] (-> u :statedata :curstate))

;;(defmacro kw? [x]
;;  `(instance? clojure.lang.Keyword ~x))

(defn has?
  "Dumb aux function to help with state sets/keys.
   If we have an fsm with statedata, we can check 
   whether the state representation has a desired 
   state - or 'is'  the state - even in the 
   presence of multiple conjoined states - ala 
   state sets."
  [ys xs]
  (if (set? xs) 
    (if (set? ys) (when (every? ys xs) xs)
        (and (== (count xs) 1)
             (xs ys)))
    (if (set? ys) 
        (and (== (count ys) 1)
             (ys xs))
        (when (= ys xs) ys))))

(defn has-state? [u s]
  (has? (unit-state u) s))

(defn waiting? [u]  (has-state? u :waiting))

;;added :overlapping...
(defn deployed-state? [s]
  (case s 
    (:bogging :deploying pol/Deployed pol/Bogging :non-bogging :waiting :overlapping) true
    false))

;Consults the unit's state to determine if it's in a Bogging or Overlapping state.
;Note, this implicitly hardcodes deployed states.  We could probably yank this out into
;a data-driven definition that's more dynamic.  TBD.
(defn deployed? [u]
  (let [s (unit-state u)]
    (if-not (set? s)
      (deployed-state? s)
      (some #(deployed-state? %) s))))   

;Indicates whether unit u is eligible for a follow on deployment.
;Units eligible for follow on deployments are units that have "any" followon code.
;The followon code indicates the context of the followon deployment.
(defn can-followon? [u] (followon-code u))

;Determine if a unit falls within the deployable window of a given policy.  If no
;policy is supplied, the unit's associated policy will be consulted.
(defn in-deployable-window? 
  ([u policy] (pol/deployable-by? policy  (:cycletime u)))
  ([u] (in-deployable-window? u (:policy u))))

(defn deployable-window [u] (pol/deployable-window (:policy u)))

;; 'TOM Hack 24 July 2012
;; 'Guard to prevent units in recovery from being eligible to deploy.
;; Public Function isRecovering() As Boolean
;; isRecovering = StateData.currentState = "Recovering"
;; End Function
(defn recovering? [u]
  (has-state? u :recovering)
  #_(= (unit-state u) :recovering))

;; 'TOM Hack 15 Nov 2015 -> Preventing recovery from being nonBog
;; Public Function isDemob() As Boolean
;; isDemob = StateData.currentState = "DeMobilizing"
;; End Function
(defn demobilizing? [u]
    (has-state? u :demobilizing)
    #_(= (unit-state u) :demobilizing))

;Determines if u is capable of deploying, as a function of u's associated policy.
(defn valid-deployer?
  ([u spawning? non-bog? policy]
   (if spawning? 
      (pol/deployable-at? policy (:positionpolicy u))
      (and (bog-remains? u) 
           (not (deployed? u))
           (not (recovering? u))
           (or non-bog?
               (or (can-followon? u)
                   (pol/deployable-at? policy (:positionpolicy u))
                   )))))
  ([u spawning? policy] (valid-deployer? u spawning? nil policy))
  ([u] (valid-deployer? u nil nil (:policy u))))

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





;;NonBOG state predicates.

;; 'TOM hack 17 Jan 2013 -> could've used isdeployed, but this is clear too....
;; Public Function canNonBOG() As Boolean
;; 'TOM Hack 15 Nov 2015 -> Preventing recovery from being nonBog

;; canNonBOG = Not isDeployed() And Not isRecovering() And Not isDemob() _
;;     And PositionPolicy <> "Recovery"
;; End Function
(defn can-non-bog? [u]
  (and (not (deployed? u))
       (not (recovering? u))
       (not (demobilizing? u))
       (not= (:positionpolicy u) :recovery)))


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
  (core/trigger-event :supplyUpdate  nm nm msg nil ctx))


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

;;if we boil down change-state :bogging and a wait time;
;;it's actually a primitive behavior.
;;We probably want to have a keep-bogging-until-depleted behavior...
;;which means, change your state to bog, according to your
;;policy's bog budget.  Wait until the bog budget elapses, or
;;someone interrupts you.


;;This is a choke point for the differences between SRM and
;;the original rotational policies.
;;When we deploy units now, we actually have a hook to determine
;;if they should assume the demand's behavior, duration, etc.
  
(defn keep-bogging-until-depleted
  [u new-location ctx]
  (move-to u new-location "Deployed" (boggable-time u) ctx))

;;designed to account for the non-bogging position, and other
;;similar frankenstates.  Rather than having a specific
;;policy position and corner case, we instead have a state
;;set that we can use.  The legacy implementation of this
;;was a hacky system of "demand effects," where we
;;check to see if the demand has any pre-set effects that
;;do stuff to the deploying unit.  This was really a
;;primitive version of location-based-behavior, where the
;;location determines policy and other things.  So,
;;we can piggy-back on the location-based-behavior
;;to describe our edge-cases like non-bogging and friends.


;;A deployment in which the details of the location tell
;;us how to proceed.  The entity must have special support
;;for this built in.  The cool thing here is that we can
;;implement all this on the behavioral side, and the
;;entity's complexity, or lacktherof, can dictate
;;how we handle things.
(defn location-based-deployment
  [entity location-details ctx]
  (core/handle-message! ctx entity
     (core/->msg (:name entity) (:name entity)
                 (core/get-time ctx)
                 :location-based-move
                 location-details)))

(defn wait-at [u wait-info ctx]
  (core/handle-message! ctx u
       (core/->msg (:name u) (:name u)
                   (core/get-time ctx)
                   :wait-based-move
                   wait-info)))


;;We need to expand this notion...
;;If the demand has a local policy, or other local attributes,
;;we should provide it to the interpreter.

;;THis is kind of obviated....
;'Assumes a unit has not yet bogged, at least not as a follow on
;'Bogs the unit for its remaining bog budget.  Accounts for the passage of time before computing
;'the unit's next update.
;Public Sub wakeAndBogUntilDepleted(unit As TimeStep_UnitData, t As Single, Optional context As TimeStep_SimContext)
;With unit
;    .ChangeState "Bogging", t - SimLib.lastupdate(.name, context), .policy.maxbog - .policy.overlap
;End With
;End Sub
(defn wake-and-bog-until-depleted [u t ctx]
  (throw (Exception. (str [:wake-and-bog-until-depleted :not-in-use])))
  (change-state u :bogging
      (- t  (get u :last-update 0))
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
(defn  re-deploy-unit [unit demand t ctx] 
  (let [c    (:currentcycle unit)
        deps (:deployments c)
        newlocation (:name demand)
        new-unit      (-> unit
                         (increment-followons)
                         (increment-deployments)
                         (assoc :followoncode nil))]
    (->>  (store/add-entity ctx new-unit)
          (keep-bogging-until-depleted new-unit newlocation))))

;;We can probably combine these into a unit behavior.
;;For instance, notice we update the deployments.
(defn  deploy-unit [unit demand t ctx]
  (let [newlocation (:name demand)
        cnt  (core/deployment-count ctx)
        ctx  (core/inc-deployment-count ctx)]
    (-> unit
        (assoc :deployment-index cnt)
        (increment-deployments)
        (keep-bogging-until-depleted newlocation ctx))))

(defn pseudo-deploy [unit info t ctx]
  (let [cnt  (core/deployment-count ctx)
        ctx  (core/inc-deployment-count ctx)]
    (-> unit
        (assoc :deployment-index cnt)
        (increment-deployments)
        (wait-at info ctx))))


;;TBD
;;casting modernization as  location-based deployment.
;;This is based off the SRM style of location-based
;;behavior, where the demand is assumed to provide
;;the metadata we need, keys for
;;[name MissionLength BOG StartState EndState overlap timeinstate]
;;We'll simplify this and define a high-level API for mods.
;;Modernization equates to a location-based deployment,
;;where the StartState is :modernizing, the EndState
;;is :modernized.  These two state map to corresponding
;;behaviors, which (by default) push the unit into
;;an immediate deployable status.  Unsure how to balance
;;this...
;;Not used currently, went with psuedo deployment
;;based on demand-category effects.

#_(defn modernize [unit demand t ctx]
  (let [p   (:policy unit)
        location-info (assoc demand
                             :StartState    :modernizing
                             :EndState      :modernized
                             :MissionLength (get demand :MissionLength 365)
                             :BOG           false
                             :overlap       0
                             :timeinstate   0
                             )]
    (location-based-deployment unit location-info ctx)))

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


