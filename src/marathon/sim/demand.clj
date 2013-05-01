(ns marathon.sim.demand
  (:require [marathon.demand [demanddata :as d]
                             [demandstore :as store]]
            [marathon.sim [supply :as supply]]
            [sim [simcontext :as sim]]
            [util [tags :as tag]]))

;This is the companion module to the TimeStep_ManagerOfDemand class.
;The module contains library functions for the demand simulation in Marathon.  Functions for
;creating, initializing, resetting, and updating state related to the Demand simulation are
;found here.

;The high-level entry point is ManageDemands, which is invoked from the EventStep_Marathon
;method in the TimeStep_Engine class.

Public Sub fromExcel(state As TimeStep_SimState)

Dim factory As TimeStep_EntityFactory
Set factory = state.EntityFactory

factory.DemandsFromTable getTable("DemandRecords"), state.demandstore
;factory.AddDemandFromExcel strtcell.Worksheet, Me

End Sub

Public Function CanSimulate(demandstore As TimeStep_ManagerOfDemand) As Boolean
CanSimulate = demandstore.tags.getSubjects("Enabled").count > 0
End Function

Public Sub NewDemand(name As String, tstart As Single, duration As Single, overlap As Single, _
                            primaryunit As String, quantity As Long, priority As Single, demandstore As TimeStep_ManagerOfDemand, _
                              policystore As TimeStep_ManagerOfSupply, ctx As TimeStep_SimContext, _
                                Optional operation As String, Optional vignette As String, Optional sourcefirst As String)
Dim dem As TimeStep_DemandData

Set dem = createDemand(name, tstart, duration, overlap, primaryunit, quantity, priority, _
                            demandstore.demandmap.count + 1, operation, vignette, sourcefirst)
registerDemand dem, demandstore, policystore, ctx

End Sub

;(defn new-demand [name tstart duration overlap primary-unit quantity priority demandstore 
;                  policystore ctx operation vignette source-first]
;
;>>>>>>>>>>>>>>>>>Missing Functionality>>>>>>>>>>>>>>>>>>>>
(defn schedule-demand [demandstore demand ctx] :blah!)
;<<<<<<<<<<<<<<<<<Missing Functionality<<<<<<<<<<<<<<<<<<<<

(defn register-demand [demand demandstore policystore ctx]
  (let [dname  (:name demand)
        ctx    (sim/trigger-event :added-demand "DemandStore" "DemandStore" 
                                  (str "Added Demand " demand.name) ctx)]
    {:demand-store (tag-demand demand 
                     (schedule-demand demand 
                        (assoc-in demandstore [:demand-map dname] demand) ctx))
     :policy-store (register-location dname policy-store)})) 

(defn add-fillable [fillrule demandstore]
  (assert (not (contains? (-> demandstore :fillables) fillrule))  
          "Tried to add fillrule multiple times")
  (update-in demandstore [:fillables] conj fillrule))

(defn remove-fillable [fillrule demandstore]
  (assert (contains? (-> demandstore :fillables) fillrule)  
          "Tried to remove non-existent fillrule")
  (update-in demandstore [:fillables] disj fillrule))

;Note -> we're just passing around a big fat map, we'll use destructuring in the 
;signatures to pull the args out from it...the signature of each func is 
;state->state

;Perform a prioritized fill of demands, heirarchically filling demands using followon
;supply, then using the rest of the supply.
(defn fill-demands [t state]
  (->> state 
    (fill-follow-ons t)
    (supply/release-max-utilizers)
    (fill-normal-demands t)))

;TOM Change 21 Sep 2011 -> this is the new first pass we engage in the fill process.
;The intent is to ensure that we bifurcate our fill process, forcing the utilization of follow-on
;units.  We split the fill process into 2 phases...
;   Phase 1:  Find out which demands in the UnfilledQ(FillRule) are eligible for Follow-On supply.
;             If a demand is eligible for follow-on supply (it has a corresponding Group in FollowOns in
;             supplymanager.
;             Add the demand to a new priority queue for the fillrule (lexically scoped).
;             Basically, we;re getting a subset of eligible demands (by fillrule) from the existing unfilledQ for
;             the fillrule.
;
;             Pop ALL Demands off the new priorityQ for the fillrule, trying to fill them using supply from the
;             followonbuckets relative to the applicable demand group. (basically a heapsorted traversal)
;             This may result in some demands being filled.
;               If a demand is filled, we update its fill status (mutate the unfilledQ) just as in the normal fill
;                routine.
;               If it;s not filled, we leave it alone.
;                       *Update -> thre problem is, our fill function, if allowed to make ghosts, will
;                                  KEEP trying to fill, and effectively short circuit our other supply.
;                                  we need to prevent the fill function from making ghosts in phase1!
;             The biggest difference is that we DO NOT stop, or short-circuit the fill process if we don;t find
;             follow-on supply.  We give every eligible demand a look.
;             After phase 1 is complete, all demands that were eligible for follow-ons will have recieved follow-on
;             supply.  Any demands completely filled by follow-ons will have been eliminated from further consideration.
;             Demands with requirements remaining are still in the unfilledQ for our normal, ARFORGEN-based fill.
;   Phase 2:  This is the normal, ARFORGEN-based fill routine.  Anything left in the unfilledQ is processed as we
;             did before.  This time, we should only be looking at ARFORGEN supply (since our follow-on supply was
;             utilized in the Phase 1).

(defn fill-follow-ons 
  [t {:keys [fillstore supplystore parameters demandstore policystore ctx] :as state}]

Dim i As Long
;TOM Change 6 Dec 2010 -Added several variables, used for dictionary access
Dim demandname As String
;TODO -> change this.  I don;t think we need it.
Dim DemandCategory As GenericSortedDictionary
Dim Categoryname
Dim canFill As Boolean
Dim demand As TimeStep_DemandData
Dim followonbuckets As Dictionary

Static startfill As Long
Static stopfill As Long

;Check
Set followonbuckets = supplystore.followonbuckets

With demandstore
    ;For each independent set of prioritized demands (remember, we partition based on substitution/SRC keys)
    If followonbuckets.count > 0 Then ;we can try to process follow-on supply...
        For Each Categoryname In .UnfilledQ ;UnfilledQ is a global, want to replace this with a function parameter...
                ;get eligible demands from the unfilled priorityQ....
                ;This is a subset of demands that "may" have eligible supply in the followonBuckets.
                ;Note -> followonBuckets are keyed by {DemandGroup, {FillRule, Supply}}
                ;So if we have eligible demands, we tap into the followonBuckets by the demand;s demandgroup property.
                ;point to the priorityQ of unfilled, homogeneous demands
                Set DemandCategory = _
                    getFollowOnDemands(demandstore, followonbuckets, .UnfilledQ(Categoryname).data)

                ;going to traverse ALL eligible demands to ensure we exhaust our follow-ons.
                ;Note, this is different than our typical supply traversal, where we stop pulling supply after the highest priority
                ;demand goes unfilled.  In here, we ensure that all demands with the potential for
                While DemandCategory.count > 0
                    canFill = False
                    ;try to fill the topmost demand
                    demandname = DemandCategory.removeNext ;values stored in pq are the names of demands
                    If demandname = vbNullString Then Err.Raise 101, , "Demand has no name"
                    Set demand = .demandmap(demandname)

                    ;we CAN short-circuit the follow-on fill if there are no more units for the specified follow-ons in the
                    ;demand group
                    If followonbuckets.exists(demand.demandgroup) Then
                        startfill = demand.unitsAssigned.count
                        ;We want to change the sourcing to use supply from the follow-on buckets....
                        ;we pass the scoped-down set of eligible Supply (from followonbuckets) to the sourcing routine...
                        ;this lets us use the same fillrules, with a different set of supply.

                        ;Decoupling -> need to change this....either passing in Fillmanager as a parameter...or something
                        ;else...
                        ;TODO -> remove the DEF b.s.
                        canFill = MarathonOpFill.sourceDemand(supplystore, parameters, fillstore, ctx, policystore, t, demand, "DEF", followonbuckets(demand.demandgroup), onlyUseFollowons)
                        stopfill = demand.unitsAssigned.count

                        If stopfill <> startfill Then
                            SimLib.triggerEvent DemandFillChanged, demandstore.name, demand.name, "The fill for " & demand.name & " changed.", demand, ctx
                           ;If we fill a demand, we update the unfilledQ.
                            ;UpdateFill demandname, UnfilledQ
                            
                            ;TOM Change 20 Aug 2012...wasn;t recording properly.
                            registerChange demandstore, demand.name
                        End If

                        If canFill Then ;changed sourceDemand to a function returning a boolean, which we use
                            ;Decoupled
                            SimLib.triggerEvent FillDemand, demandstore.name, demandstore.name, _
                                                    "Sourced Demand " & demandname, , ctx
                            ;Decoupled
                            SimLib.triggerEvent CanFillDemand, demandstore.name, demandname, _
                                                    "Filled " & demandname, , ctx
                            UpdateFill demandname, .UnfilledQ, demandstore, ctx
                        Else
                            ;the demand is still on the unfilled queue, we only "tried" to fill it. No state changed .
                            ;Debug.Print "could not fill"
                        End If
                    End If
                Wend ;keep popping off all eligible demands
        Next Categoryname
    End If
End With

End Sub
;TOM Note 13 Mar -> we perform some major effeciency improvements.  The most obvious is representing demands as
;objects.  Rather than having a demand for 5 srcs result in 5 physical entries, we just have one demand and keep
;track of its fill.  Major reduction in overhead (and searching).
;TOM Change 6 Dec 2010
;Going to redirect this sub to incorporate unfilledQ. The idea is to look for the highest priority demand,
;by SRC . Specifically, we will be redirecting the portion of the routine that finds "a demand" to be filled.
;In the previous algorithm, we just traversed thousands of demands until we found one with a status of False,
;then tried to fill it.
;In this new algorithm, instead of a loop over each demand in the demanddata array,
;we consult our bookeeping:
;For each independent set of prioritized demands (remember, we partition based on substitutionjSRC keys)
;We use our UnfilledQ to quickly find unfilled demands.
;UnfilledQ keeps demands in priority order for us, priority is stored in DEFDemand(i) .priority
;Note - we can change priority dynamically, interesting possibilities later ....
;We only fill while we have feasible supply left.
;We check deployables to efficiently find feasible supply.
;Not currently kept in priority order .... could be converted easily enough though.
;If we fill a demand, we take it off the queue.
;If we fail to fill a demand, we have no feasible supply, thus we leave it on the queue, stop filling,
;and proceed to the next independent set.
;After all independent sets have been processed, we;re done.
;TOM Change 3 Mar 2011 renamed from DEFSourcing
Public Sub FillNormalDemands(t As Single, fillstore As TimeStep_ManagerOfFill, supplystore As TimeStep_ManagerOfSupply, _
                                parameters As TimeStep_Parameters, demandstore As TimeStep_ManagerOfDemand, _
                                    policystore As TimeStep_ManagerOfPolicy, ctx As TimeStep_SimContext)

;Dim demand As Long
Dim i As Long
;TOM Change 6 Dec 2010 -Added several variables, used for dictionary access
Dim demandname As String
Dim DemandCategory As GenericSortedDictionary
Dim Categoryname
Dim canFill As Boolean
Dim demand As TimeStep_DemandData
Static startfill As Long
Static stopfill As Long
Dim msg As String

;For each independent set of prioritized demands (remember, we partition based on substitution/SRC keys)
;TOM Change 27 Mar 2011 -> added a mutable filter called fillables, which records demands with known supply.
;This allows the stockwatcher to maintain a list of fillable demands.  We then proceed with the previous
;logic of filling each demand.
;Note -> as of 27 Mar 2011, the fill routine is only putting out a single path (the shortest path).
;This happens in sourcedemand.
With demandstore
    For Each Categoryname In .UnfilledQ ;UnfilledQ
            ;Decoupled
            SimLib.triggerEvent RequestFill, .name, .name, "Trying to Fill Demand Category " & Categoryname, , ctx
            ;We use our UnfilledQ to quickly find unfilled demands.
            Set DemandCategory = .UnfilledQ(Categoryname) ;point to the priorityQ of unfilled, homogeneous demands
            For i = 1 To DemandCategory.count
                canFill = False
                ;try to fill the topmost demand
                demandname = CStr(DemandCategory.nextval) ;values stored in pq are the names of demands
                If demandname = vbNullString Then Err.Raise 101, , "Demand has no name"

                msg = "Highest priority demand in Category " & Categoryname & " is " & demandname & " with priority " _
                        & DemandCategory.HighestPriority
                ;Decoupled
                SimLib.triggerEvent RequestFill, .name, .name, msg, , ctx

                ;We;re going to be passing entire blocks of demand, vs. little atomic components of demand.  This will be
                ;loads more effecient.  That way, the supply manager just fills as much as it can, and replies if the demand
                ;was filled.  IF yes, then we can continue filling this category of demands.
                ;We only fill while we have feasible supply left.
                ;TOM Note -> Source Demand is actually trying to source the demand, although it looks like
                ;a simple boolean check, it;s actually mutating stuff.
                ;If parent.SupplyManager.sourceDemand(day, DemandMap(demandname), "DEF") Then ;changed sourceDemand to a function returning a boolean, which we use
                ;TOM change 22 Mar 2011 -> demandsourcing is now done through fillmanager.
                Set demand = .demandmap(demandname)
                startfill = demand.unitsAssigned.count
                ;Tom Note 27 Mar 2012 - DEF is vestigial....should be excised.
                ;Decoupling again....push fillmanager as fill function.
                canFill = sourceDemand(supplystore, parameters, fillstore, ctx, policystore, t, demand, "DEF", , useEveryone)
                stopfill = demand.unitsAssigned.count
                ;Decoupled
                If stopfill <> startfill Then
                    SimLib.triggerEvent DemandFillChanged, demandstore.name, demand.name, "The fill for " & demand.name & " changed.", demand, ctx
                    ;TOM Change 20 Aug 2012...wasn;t recording properly.
                    registerChange demandstore, demand.name
                End If
                
                If canFill Then ;changed sourceDemand to a function returning a boolean, which we use
                        msg = "Sourced Demand " & demandname
                        ;Decoupled
                        SimLib.triggerEvent FillDemand, demandstore.name, demandstore.name, msg, , ctx
                    ;If we fill a demand, we update the unfilledQ.
                    UpdateFill demandname, .UnfilledQ, demandstore, ctx
                    ;Decouple
                    SimLib.triggerEvent CanFillDemand, demandstore.name, demandname, "Filled " & demandname, , ctx
                Else
                    Exit For ;If we fail to fill a demand, we have no feasible supply, thus we leave it on the queue, stop filling.
                ;Note, the demand is still on the queue, we only "tried" to fill it. No state changed .
                End If
            Next i
            ;Proceed to the next independent set.
        ;End If
    Next Categoryname
End With

;After all categories processed, we;re done. Remaining code is legacy, has been moved, etc.
End Sub



;Return a list of demands that are currently eligible for follow-on
;The returned dictionary is a nested dictionary, nested by Group (FillRule demands)
;This is a subset of the original unfilledQ.

;add the demand if its follow-on group is present.
;note, we don;t know a-priori if the demand will be filled...we;re merely noting that the demand "may" have
;follow-on units, reached through potentially complicated substitution rules, that can be utilized.  we;re just
;scoping the set of demands.
(defn get-followon-demands [demandstore followon-buckets unfilled-demands]
  (->> (map #(get demandstore %) unfilled-demands)
       (filter #(contains? followon-buckets (:demandgroup %))))
       (reduce (fn [m x] (assoc m [(:name x) (:priority x)] (:name x))) {})))   

;follow-on supply is a function of fillrule, demandgroup, followonbuckets

;procedure that allows us to, using the fillgraph, derive a set of tags whose associated demands
;should be disabled.  if removal is true, the demands will be removed from memory as well.
;in cases where there are a lot of demands, this may be preferable.
(defn scope-demand [demandstore disable-tags & {:keys [removal]}]
  (let [tags    (:tags demandstore)
        f       (if removal #(remove-demand %1 %2) (fn [m k] m))]     
    (reduce (fn [store demand-name] 
              let [demands (:demand-map store)]
              (if (contains? demands demand-name)
                (f (disable store demand-name) demand-name) store))
      demandstore (mapcat (partial tag/get-subjects tags) disable-tags))))

(defn remove-demand [demandstore demandname]
  (if (contains? (:demand-map demandstore) demandname)
    (let [{:keys [activations deactivations demand-map]} demandstore
          demand (get demand-map demandname)
          dname  (:name demand)
          tstart  (:startday demand)
          tfinal (+ tstart (:duration demand))]
      (-> demandstore 
        (update-in [:demand-map] dissoc demand-map demand-name)
        (update-in [:activations] update-in activations [tstart] dissoc dname)
        (update-in [:deactivations] update-in deactivations [tfinal] dissoc dname)))
    demandstore)) 

(defn is-enabled [demandstore demandname] 
  (tag/has-tag (:tags demandstore) :enabled demandname))
(defn enable [demandstore demandname]
  (update-in demandstore [:tags] tag/add-tag :enabled demandname))
(defn disable [demandstore demandname]
  (update-in demandstore [:tags] tag/remove-tag :enabled demandname))

;TOM note 27 Mar 2011 ->  I;d like to factor these two methods out into a single function,
;discriminating based on a parameter, rather than having two entire methods.
;Register demand activation for a given day, given demand.
;TOM Change 7 Dec 2010
(defn add-activation [t demandname demandstore ctx]
  (let [actives (get-in demandstore [:activations t] #{})]
    (request-demand-update t demandname ctx) ;WRONG
    (assoc-in demandstore [:activations t] (conj actives demandname))))

;Simple wrapper for demand update requests.
(defn request-demand-update [t demandname ctx]
  (sim/request-update t demandname :demand-update ctx))

(defn add-deactivation [t demandname demandstore ctx
;TOM Change 27 MAr 2011 -> refactored this to a one-liner.
;Register demand deactviation for a given day, given demand.
;TOM Change 7 Dec 2010
Public Sub AddDeActivation(t As Single, demandname As String, demandstore As TimeStep_ManagerOfDemand, ctx As TimeStep_SimContext)
AddActivation t, demandname, demandstore, ctx, demandstore.deactivations ;<- points to a different dictionary

If t > demandstore.tLastDeactivation Then
   demandstore.tLastDeactivation = t
End If

End Sub

Public Sub scheduleDemand(demand As TimeStep_DemandData, demandstore As TimeStep_ManagerOfDemand, ctx As TimeStep_SimContext)
    With demand
        AddActivation .startday, .name, demandstore, ctx ;register activation
        AddDeActivation (.startday + .duration), .name, demandstore, ctx
    End With
End Sub
;
Public Sub rescheduleDemands(demandstore As TimeStep_ManagerOfDemand, ctx As TimeStep_SimContext)
Static demand As TimeStep_DemandData
Static itm
With demandstore
    For Each itm In .demandmap
        Set demand = (.demandmap(itm))
        scheduleDemand demand, demandstore, ctx
        demand.reset
    Next itm
End With

End Sub
;
;TOM Change 6 Dec 2010
;Introducing this sub, to be inserted into the mainmodel loop.
;Its purpose is to maintain a running list of demands (ActiveDemands dictionary), which will help us only fill
;active demands. How do demands get added to the list? Upon initialization, we schedule their activation day
;and deactivation day (start + duration). During the course of the mainmodel loop, we have a listener that
;checks to see if the current day is a day of interest, specifically if it;s an activation day.
;Note that this idea, partitioning our days into "days of interest", while localized to handle demand
;activations and deactivations, could easily be extended to a general "days of interest" manager, with
;subscribed handlers and subroutines to run. In fact, this is exactly what an event step simulation does.
;We;re just copping techniques and modifying them for use in a timestep sim to make it more efficient.
Public Sub ManageDemands(t As Single, state As TimeStep_SimState)
Dim DemandKey
Dim demand As TimeStep_DemandData
Dim ptr As Dictionary
Dim nm
Dim msg As String
Dim demandstore As TimeStep_ManagerOfDemand

Set demandstore = state.demandstore

With demandstore
    If .activations.exists(t) Then
        Set ptr = .activations(t)
        For Each DemandKey In ptr
            Set demand = .demandmap(DemandKey)
            With demand
                If .status = True Then
                    Err.Raise 101, , "Activating an already Active demand, impossible"
                End If
                ;todo -> remove this
                .status = True ;vestigial
                ;If demandtraffic Then
                    msg = "Activating demand " & DemandKey & " on day " & t
                    ;Decoupled
                    SimLib.triggerEvent ActivateDemand, demandstore.name, demand.name, msg, , state.context
                ;End If
                demandstore.activeDemands.add CStr(DemandKey), demand
                ;addEligible demand
                UpdateFill CStr(DemandKey), demandstore.UnfilledQ, demandstore, state.context
                registerChange demandstore, demand.name
            End With
        Next DemandKey
    End If
    ;TOM change 7 Dec 2010 -> added a hook in here to send home deployed/overlapping units, want deactivation to force
    ;them to go to reset.
    If .deactivations.exists(t) Then
        Set ptr = .deactivations(t)
        For Each DemandKey In ptr
            Set demand = .demandmap(DemandKey)
            With demand
                .status = False

                msg = "DeActivating demand " & DemandKey & " on day " & t
                ;Decouple
                 SimLib.triggerEvent DeActivateDemand, demandstore.name, .name, msg, .name, state.context


                If demandstore.activeDemands.exists(CStr(DemandKey)) = False Then
                    Err.Raise 101, , "deactivating something that does not exist ... "
                End If

                demandstore.activeDemands.Remove CStr(DemandKey)
                ;removeEligible demand

                For Each nm In .unitsAssigned ;send all units home.
                    SendHome t, demand, CStr(nm), state.context ;<----- This is the hook
                Next nm
                UpdateFill CStr(DemandKey), demandstore.UnfilledQ, demandstore, state.context ;remove demand from fill consideration
                 ;Tom change 20 aug 2012 -> force the system to sample demands.
                registerChange demandstore, demand.name
            End With
        Next DemandKey
    End If
End With

End Sub
;;TOM Change 21 Sep 2011 -> register a demand as being eligible for follow-on fills
;;Private Sub addEligible(demand As TimeStep_DemandData)
;;
;;Dim grp As String
;;Dim ptr As Dictionary
;;
;;grp = demand.demandgroup
;;
;;If grp <> vbNullString Then
;;    With eligibleDemands
;;        If .exists(grp) Then
;;            Set ptr = .item(grp)
;;        Else
;;            Set ptr = New Dictionary
;;            .add grp, ptr
;;        End If
;;    End With
;;    ptr.add demand.src, demand.name
;;End If
;;
;;End Sub
;;TOM Change 21 Sep 2011 -> unregister a demand as being eligible for follow-on fills
;;Private Sub removeEligible(demand As TimeStep_DemandData)
;;
;;Dim grp As String
;;Dim ptr As Dictionary
;;
;;grp = demand.demandgroup
;;If grp <> vbNullString Then
;;    With eligibleDemands
;;        If .exists(grp) Then
;;            Set ptr = .item(grp)
;;        Else
;;            Err.Raise 101, , "Trying in remove an eligible demand that was never recorded as eligible!"
;;        End If
;;    End With
;;    If ptr.exists(demand.name) Then
;;        ptr.remove demand.src
;;    Else
;;       Err.Raise 101, , "Trying in remove an eligible demand that was never recorded as eligible!"
;;    End If
;;End If
;;
;;End Sub
Public Sub ManageChangedDemands(day As Single, state As TimeStep_SimState)
state.demandstore.changed.RemoveAll
End Sub
Public Sub clearChanges(demandstore As TimeStep_ManagerOfDemand)
demandstore.changed.RemoveAll
End Sub

;TOM Change 7 Dec 2010 -> removed deactivation code from sourcedemand, placed it in a separate sub.
;This sub implements the state changes required to deactivate a demand, namely, to send a unit back to reset.
;The cases it covers are times when a demand is deactivated, and units are not expecting to overlap.
;If units are overlapping at a newly-inactive demand, then they get sent home simultaneously
;For this reason, we take the abs(unit) to get both overlapping and bogging units.
;Alex;s convention for overlapping units was a negative value for unitsdeployed.
;TODO -> Remove convention of negative cycle time = BOG, transition to stateful information contained in
;unit data, or derived from Policy data.
;TOM Change 14 Mar 2011 <- provide the ability to send home all units or one unit...default is all units
Public Sub SendHome(t As Single, demand As TimeStep_DemandData, unitname As String, ctx As TimeStep_SimContext)

Dim unit As TimeStep_UnitData
Dim startloc As String
Dim msg As String

With demand
    If .unitsAssigned.count = 0 Then
        msg = "Demand " & .name & " Deactivated on day " & t & " with nothing deployed "
        ;Decoupled
        SimLib.triggerEvent DeActivateDemand, "DemandStore", .name, msg, , ctx
    ElseIf .status = False And .unitsAssigned.count > 0 Then ;Abs(.unitsDeployed) > 0 Then
        Set unit = .unitsAssigned(unitname)
        With unit
            startloc = .LocationName
            ;Decoupled....need to pass in last update as a parameter, or find a way for demandmanager to derive it.
            .update (t - SimLib.lastupdate(unit.name, ctx))
            SimLib.triggerEvent supplyUpdate, "DemandStore", unit.name, "Send Home Caused SupplyUpdate for " & unit.name, , ctx
            ;stamp the unit with a followon code
            If demand.demandgroup <> "UnGrouped" Then
                .followoncode = demand.demandgroup
                .ChangeState "AbruptWithdraw", 0, , ctx
            ;Tom change 24 Jul 2012
            ElseIf unit.src <> "Ghost" Then
                .ChangeState "AbruptWithdraw", 0, , ctx
            Else
                ;Decoupled
                If unit.src = "Ghost" Then _
                SimLib.triggerEvent GhostReturned, demand.src, unit.name, _
                                        "Ghost for src " & demand.src & " left deployment.", , ctx
                .ChangeState "Reset", 0, , ctx
            End If
            ;.ChangeState "MovingState" ;The unit should figure out, via behavior, what it;s supposed to do next.
            msg = "Disengaging unit" & unit.name & " from de-activated demand" & demand.name
            ;Decoupled
            SimLib.triggerEvent DisengageUnit, name, unit.name, msg, , ctx
        End With
    End If
End With

End Sub

;This is also called independently from Overlapping_State.....
;Remove a unit from the demand.  Have the demand update its fill status.
;move the unit from the assigned units, to overlappingunits.
Public Sub disengage(demandstore As TimeStep_ManagerOfDemand, unit As TimeStep_UnitData, demandname As String, _
                        ctx As TimeStep_SimContext, Optional overlap As Boolean)

Dim demand As TimeStep_DemandData
Dim needsupdate As Boolean
Dim disengagetype As String

Set demand = demandstore.demandmap(demandname)

With demand
    needsupdate = .required = 0
    If overlap Then
        ;Decoupled
        SimLib.triggerEvent OverlappingUnit, demandstore.name, unit.name, "Overlapping unit" & unit.name _
            & " in demand" & .name, unit, ctx
        .SendOverlap unit
    Else
        ;Decoupled
        .SendHome unit
        SimLib.triggerEvent DisengageUnit, demandstore.name, unit.name, "Sending unit" & unit.name _
                & "home from demand" & .name, unit, ctx
    End If



End With

registerChange demandstore, demandname
If needsupdate Then UpdateFill demandname, demandstore.UnfilledQ, demandstore, ctx

End Sub

Public Sub registerChange(demandstore As TimeStep_ManagerOfDemand, demandname As String)

With demandstore
    If Not .changed.exists(demandname) Then .changed.add demandname, 0
End With

End Sub
;Tom change 6 Dec 2010
;Sub to register or deregister Demands from the UnfilledQ
;UnfilledQ partitions the set of demands that are unfilled
;When we go to look for demands that need filling, we traverse the keys linearly.
;Only keys that exist will be filled ...
;Currently, we have no implemented way of prioritizing SRC fills over eachother, but it also should not matter
;due to the independence of the SRC populations.
;Unfilled demands exist in a priority queue, based on the demand;s priority, this is data driven/positional.
;The basic idea is this:
;Upon initialization of the demand, we tag a field in the demand and associate it with priority. This
;will, by default, be the positional location of the demand in the data (i.e. it;s row index). The idea
;is that the user will put data in priority order. Note - with the advent of "priority" associated with
;the demand, we can always just read this in as another parameter in the data. Then position won;t matter,
;only data.
;Basic demand events are Activation, Deactivation, Fill, Unfill.
;Marathon3 currently models these events by filling an array, aDEFDemandData, with a boatload of heterogeneous
;DemandData containers, which contain info on status. Basically, if the demand is active, its status should be
;true. Our goal is to move this information from a polling environment, where we have to traverse the entire
;array in O(N) worst case time, every day, to a pushing environment. In a push environment, we check for the
;existence of unfilled demands very effeciently, 0(1) constant time, and upon discovering the existence, we
;retrieve the highest priority demand in 0(1) time.
;Activation and unfill events manifest in demandnames being added to the unfilledQ.
;Deactivation and filled events manifest in demandnames being removed from the unfilledQ.
;The bottom line is that we want to quickly find out if A. there are unfilled demands, B. what the most important
;unfilled demand is, C. what happens when we fill the demand (take the demand off the q or not?)
;aDEFDemandData is the indicator we use to capture active demands, it;s the only thing that changes state ..
;One more note, this assumes an atomic model of filling demands -> we split demands up into individual units,
;which we fill. Rather than keeping track of the quantity filled at each demand to determine overall fill,
;we only have a binary filled/unfilled in the atomic model. This means we don;t ever check the condition
;that a demand might have gained a unit, via deployment, and still remain unfilled. It;s impossible.
Public Sub UpdateFill(demandname As String, unfilled As Dictionary, _
                        demandstore As TimeStep_ManagerOfDemand, ctx As TimeStep_SimContext)
Dim demandq As GenericSortedDictionary
Dim demand As TimeStep_DemandData
Static required As Long

Set demand = demandstore.demandmap(demandname)
;TOM Change 9 DEC 2010
demand.status = demandstore.activeDemands.exists(demand.name)
If demand.src = vbNullString Then
    Err.Raise 101, , "NO SRC for demand" & demandname
Else
;what should the demand status be?
    With demand
        required = .required
        ;TOM Chang 9 Dec 2010 -> made dependent on active demands
        If .status = False Or (.status = True And required = 0) Then ;demand is filled, remove it
            If unfilled.exists(.src) Then
                Set demandq = unfilled(.src)
                demandq.Remove .name
                If demandq.count = 0 Then
                    unfilled.Remove (.src)
                    Set demandq = Nothing
                    ;If demandtraffic Then
                    ;Decoupled
                    SimLib.triggerEvent FillDemand, demandstore.name, .name, _
                        "Removing demand " & .name & " from the unfilled Q", , ctx
                    ;End If
                End If
            Else ;have a deactivation
                ;Err.Raise 101, ,  "Demand is filled, but was not registered on unfilled q!"
                ;If demandtraffic Then
                    ;Decoupled
                    SimLib.triggerEvent DeActivateDemand, demandstore.name, .name, _
                         "Demand " & .name & " was deactivated unfilled", .name, ctx
                ;End If
            End If
        ElseIf .status = True And required > 0 Then ;demand is unfilled, add it
            If unfilled.exists(.src) Then
                Set demandq = unfilled(.src)
            Else
                Set demandq = New GenericSortedDictionary
                unfilled.add .src, demandq
            End If

            If .name = vbNullString Then Err.Raise 101, , "DemandName is invalid!"
            If Not demandq.data.exists(.name) Then
                demandq.add .name, .name, .priority ;store the kvp as String (demandname,demandname)
                ;If demandtraffic Then
                    ;Decoupled
                    SimLib.triggerEvent RequestFill, demandstore.name, .name, _
                        "Adding demand " & .name & " to the unfilled Q", , ctx
                ;End If
            End If

        End If
    End With
End If

Set demandq = Nothing
End Sub

;;inject appropriate tags into the GenericTags
;decoupled
Public Sub tagDemand(demand As TimeStep_DemandData, demandstore As TimeStep_ManagerOfDemand, Optional extras As Dictionary)

With demand
    demandstore.tags.multiTagDict demand.name, newdict("FILLRULE_" & .primaryunit, 0, _
                                                       "PRIORITY_" & .priority, 0, _
                                                       "Enabled", 0)
End With

If Not (extras Is Nothing) Then demandstore.tags.multiTag demand.name, extras


End Sub

Public Sub tagDemandSink(demandstore As TimeStep_ManagerOfDemand, sink As String)
demandstore.tags.addTag "Sinks", sink
End Sub

Public Function getDemandSinks(demandstore As TimeStep_ManagerOfDemand) As Dictionary
Set getDemandSinks = demandstore.tags.getSubjects("Sinks")
End Function

;;;Demand can request an update at a specified time ....
;;Public Sub RequestUpdate(t As Single, demand As TimeStep_DemandData)
;;;Decouple
;;parent.RequestUpdate t, demand.name, UpdateType.demand
;;End Sub

Public Sub clearDemands(demandstore As TimeStep_ManagerOfDemand)

With demandstore
    .demandmap.RemoveAll ; = Nothing ;KVP mapping of demand names to indices in aDEFdata
    .DemandIndex.RemoveAll
    .tLastDeactivation = -1

    ;infeasibledemands.RemoveAll

    .UnfilledQ.RemoveAll
    .activations.RemoveAll  ; = Nothing
    .deactivations.RemoveAll ; = Nothing
    .activeDemands.RemoveAll  ;= Nothing
    .fillables.RemoveAll  ;= Nothing
    .tags.tags.RemoveAll
    .tags.subjects.RemoveAll ;= Nothing

    .tLastDeactivation = -1
    .tags.addTag "Sinks"
End With

End Sub



