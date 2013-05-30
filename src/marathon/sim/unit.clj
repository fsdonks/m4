(ns marathon.sim.unit)

;TEMPORARILY ADDED for marathon.sim.demand
(declare change-state update can-deploy? change-location! 
         re-deploy-unit deploy-unit) 

;'marathonopunit
;'A module for unit behaviors...
;Option Explicit
;'Wrapper for low level calls to the unit.
;'Assumes a unit is in a followon status.  Imediately puts the unit in a bogging state, with no
;'change in time.  Instantaneous.
;'Bogs the unit for its remaining bog budget.  The unit will ask for an update
;Public Sub keepBoggingUntilDepleted(unit As TimeStep_UnitData, Optional context As TimeStep_SimContext)
;unit.ChangeState "Bogging", 0, unit.CurrentCycle.bogbudget - unit.policy.overlap, context
;End Sub
;'Assumes a unit has not yet bogged, at least not as a follow on
;'Bogs the unit for its remaining bog budget.  Accounts for the passage of time before computing
;'the unit's next update.
;Public Sub wakeAndBogUntilDepleted(unit As TimeStep_UnitData, t As Single, Optional context As TimeStep_SimContext)
;With unit
;    .ChangeState "Bogging", t - SimLib.lastupdate(.name, context), .policy.maxbog - .policy.overlap
;End With
;End Sub
;'Increment the unit's deployment count for the current cycle.
;Public Sub incrementDeployments(unit As TimeStep_UnitData, Optional context As TimeStep_SimContext)
;unit.CurrentCycle.deployments = unit.CurrentCycle.deployments + 1
;End Sub
;'Increment the unit's deployment count for the current cycle.
;Public Sub incrementFollowOns(unit As TimeStep_UnitData, Optional context As TimeStep_SimContext)
;unit.CurrentCycle.followons = unit.CurrentCycle.followons + 1
;End Sub
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
;Public Sub reDeployUnit(unit As TimeStep_UnitData, t As Single, Optional deploymentindex As Long, Optional context As TimeStep_SimContext)
;
;incrementFollowOns unit, context
;keepBoggingUntilDepleted unit, context
;unit.followoncode = vbNullString
;incrementDeployments unit, context
;
;End Sub
;
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
;'Records the first time a unit moved.
;Public Sub unitMovedEvent(unit As TimeStep_UnitData, newlocation As String, context As TimeStep_SimContext)
;With unit
;triggerEvent UnitMoved, .name, newlocation, .name & " moved from " & _
;                            .LocationName & " to " & newlocation & " on day " & SimLib.getTime(context), unit, context
;End With
;End Sub
;
;Public Sub unitFirstMovedEvent(unit As TimeStep_UnitData, newlocation As String, context As TimeStep_SimContext)
;With unit
;triggerEvent UnitMoved, .name, newlocation, .name & " started moving from " & _
;                            .LocationName & " to " & newlocation & " on day " & SimLib.getTime(context), unit, context
;End With
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
;
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
;
;
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
;
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
;
;'TOM change 20 April 2012 - > Note, we were using cycletime here, which is the cycletime associated
;'with the unit state, i.e the empirical cycle time.  Since we've got a separation between the cycle
;'length experienced by the unit, and the nominal policy length that unit is operating under (i.e.
;'it changed rotational policies and is currently under a different policy), we need to change the
;'deployment criteria from the empirical or experienced cycletime (unitdata.cycletime), to the notion
;'of cycletime relative to active rotational policy, which is kept in currentcycle.duration.
;Public Function CanDeploy(u As TimeStep_UnitData, Optional spawning As Boolean, Optional policy As IRotationPolicy) As Boolean
;If policy Is Nothing Then Set policy = u.policy
;CanDeploy = validDeployer(u, spawning, policy)
;End Function
;'Determines if u is capable of deploying, as a function of u's associated policy.
;Public Function validDeployer(u As TimeStep_UnitData, Optional spawning As Boolean, Optional policy As IRotationPolicy) As Boolean
;If policy Is Nothing Then Set policy = u.policy
;If spawning Then
;    validDeployer = policy.Deployable(u.PositionPolicy)
;Else
;    validDeployer = bogRemains(u) And Not isDeployed(u) 'bogbudget > 0
;
;    If validDeployer Then
;        validDeployer = canFollowOn(u) Or inDeployableWindow(u, policy)
;    End If
;End If
;
;End Function
;
;'Consults the unit's state to determine if it's in a Bogging or Overlapping state.
;'Note, this implicitly hardcodes deployed states.  We could probably yank this out into
;'a data-driven definition that's more dynamic.  TBD.
;Public Function isDeployed(u As TimeStep_UnitData) As Boolean
;Dim state As String
;Select Case u.StateData.CurrentState
;    Case Bogging, Overlapping
;        isDeployed = True
;    Case Else
;        isDeployed = False
;End Select
;End Function
;
;'Predicate to indicate unit U's ability to bog.
;Public Function bogRemains(u As TimeStep_UnitData) As Boolean
;bogRemains = u.CurrentCycle.bogbudget > 0
;End Function
;
;'Indicates whether unit u is eligible for a follow on deployment.
;'Units eligible for follow on deployments are units that have "any" followon code.
;'The followon code indicates the context of the followon deployment.
;Public Function canFollowOn(u As TimeStep_UnitData) As Boolean
;canFollowOn = u.followoncode <> vbNullString
;End Function
;
;'Determine if a unit falls within the deployable window of a given policy.  If no
;'policy is supplied, the unit's associated policy will be consulted.
;Public Function inDeployableWindow(u As TimeStep_UnitData, Optional policy As IRotationPolicy) As Boolean
;If policy Is Nothing Then Set policy = u.policy
;inDeployableWindow = u.cycletime >= policy.startdeployable And cycletime < policy.stopdeployable
;End Function
;
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
;
;'Todo -> determine if we need context in this case...
;'Change unit u's current state.  Prompts an update from the unit behavior.
;Public Sub changeUnitState(u As TimeStep_UnitData, newstate As String, deltat As Single, Optional duration As Single, _
;                                Optional context As TimeStep_SimContext, Optional behavior As IUnitBehavior)
;If behavior Is Nothing Then Set behavior = u.behavior
;
;behavior.ChangeState u, newstate, deltat, duration
;End Sub
;
;'TODO -> flesh this out and experiment.
;'Experimental.  Not yet tested.  Allows for behavior changes.
;Public Sub changeUnitBehavior(u As TimeStep_UnitData, newbehavior As IUnitBehavior, Optional oldbehavior As IUnitBehavior)
;If oldbehavior Is Nothing Then Set oldbehavior = u.behavior
;changeUnitState u, "BehaviorChange", 0, 0, , newbehavior
;End Sub
;
;'TOM Change 27 MAr 2011 -> aren't really using day anymore, but it's in the parameter for now.
;'Note -> context isn't necessary, as behavior contains that stuff.
;Public Function updateUnit(u As TimeStep_UnitData, deltat As Single, Optional behavior As IUnitBehavior) As TimeStep_UnitData
;If behavior Is Nothing Then Set behavior = u.behavior
;Set updateUnit = behavior.update(deltat, u)
;End Function
;
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


