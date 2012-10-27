(ns marathon.port.data.unitbehaviorghost)

'unitbehaviorghost
'This is an attempt to describe ghost behaviors...
'True Ghosts follow a simple policy...
'They spawn, are immediately deployable, then wait forever until they changestate.
    'While waiting, they ask for no updates.  They just sit until needed.
    'So they show up as a constant deployable supply.
    'When a new ghost is needed (i.e. the fill manager requests the ghost)

'Since ghost behavior is different, especially in some of the possible states, we just implement the
'interface for the base class (rather than delegating input to a reference of it).  This allows us to
'completely customize the behavior.

Option Explicit
Public name As String
'Decouple
'Public parent As TimeStep_ManagerOfSupply
Public simstate As TimeStep_SimState

Public Enum GhostType
    Transient
    persistentghost
End Enum

Public nextbehavior As IUnitBehavior

Public gtype As GhostType
Implements IUnitBehavior
Private Sub Class_Initialize()
name = "DefaultGhostBehavior"
gtype = GhostType.persistentghost
End Sub
Public Sub init(nm As String, typeofghost As GhostType, state As TimeStep_SimState, Optional nxtbehavior As IUnitBehavior)
name = nm
gtype = typeofghost
'Decouple
'Set parent = par
Set simstate = state

Set nextbehavior = nxtbehavior

End Sub
Public Function update(tElapsed As Single, unit As TimeStep_UnitData) As TimeStep_UnitData

Set update = RollForward(unit, tElapsed)

End Function
'What happens when we change a state?
'Some states are just blips (0 duration).
'Some states are absorbing states (infinite duration).
'Unless otherwise specified (by a duration), duration is assumed infinite.
Public Function ChangeState(unit As TimeStep_UnitData, tostate As String, deltat As Single, Optional duration As Single, _
                                        Optional followingstate As String) As TimeStep_UnitData


With unit
    .StateData.ChangeState tostate, duration, followingstate, True 'allows instant state changes.
    If duration > 0 Then
        'Decoupled*
        If duration <> .StateData.inf Then
            MarathonOpSupply.requestUnitUpdate _
                SimLib.getTime(simstate.context) + duration, unit, simstate.context 'TOM Change 26 Jan 2011-->(.parent.GetTime + duration - 1)
        End If
    End If
    Set ChangeState = UpdateState(unit, 0) 'State Changes are instantaneous.  If the current state is instantaneous, then
End With

End Function

'This function allows us to roll through multiple updates, if delta T is too large.
'We could set deltaT to 1095 and get an entire cycle in theory.
Private Function RollForward(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData

Dim remaining As Single
Dim deltaNext As Single

If deltat <= unit.StateData.remaining Then
    Set RollForward = UpdateState(unit, deltat)
Else
    While unit.StateData.remaining < deltat
        deltaNext = unit.StateData.remaining
        deltat = deltat - deltaNext
        Set RollForward = UpdateState(unit, deltaNext)
    Wend
End If

End Function
'Ghosts handle states in the ghost policy.
'"Spawning", "Spawning", "Deployable", "Deployable", "Waiting", "Nothing", _
                "Deploying", "Deploying", "NotDeployable", "NotDeployable", _
                    deployed, "Bogging", Overlapping, "Overlapping", _
                            "BehaviorChange", "BehaviorChange"
Private Function UpdateState(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData

If Not specialstate(unit.StateData.CurrentState) Then
    Select Case unit.StateData.CurrentState
        Case Is = "Bogging"
            Set UpdateState = Bogging_State(unit, deltat)
        Case Is = "Moving"
            Set UpdateState = Moving_State(unit, deltat)
        Case Is = "Overlapping"
            Set UpdateState = Overlapping_State(unit, deltat)
        Case Is = "Deployable"
            Set UpdateState = unit
        Case Is = "NotDeployable"
            Set UpdateState = unit
        Case Is = "Nothing"
            Set UpdateState = unit
        Case Is = "Deploying"
            Set UpdateState = unit
        Case Is = "StartCycle"
            Set UpdateState = StartCycle_State(unit, deltat)
        Case Is = "EndCycle"
            Set UpdateState = EndCycle_State(unit, deltat)
        Case Is = "Reset"
            Set UpdateState = Reset_State(unit, deltat)
    Case Else
        Err.Raise 101, , "No implementation for unit state " & unit.StateData.CurrentState
    End Select
    Set unit = Global_State(unit, deltat) 'run global tasks common to every state.
Else
    Select Case unit.StateData.CurrentState
        Case Is = "Spawning"
            Set UpdateState = Spawning_State(unit, deltat)
        Case Is = "AbruptWithdraw"
            Set UpdateState = AbruptWithdraw_State(unit, deltat)
        Case Is = "BehaviorChange"
            Set UpdateState = BehaviorChange_State(unit, deltat)
    End Select
End If

End Function
Private Function specialstate(instate As String) As Boolean
specialstate = instate = "Spawning" Or instate = "AbruptWithdraw"
End Function

Private Function FinishCycle(unit As TimeStep_UnitData, fromloc As String, toloc As String) As TimeStep_UnitData
Set FinishCycle = unit

If NewCycle(unit, fromloc, toloc) Then
    If Not JustSpawned(unit) Then
        Set FinishCycle = StartCycle_State(EndCycle_State(unit, 0), 0) 'wrap up the cycle.
    End If
End If

End Function
'TOM Change 20 May 2011
'Private Function getState(unit As TimeStep_UnitData, location As String) As String
'getState = unit.policy.getState(location)
'End Function
Private Function getState(unit As TimeStep_UnitData, position As String) As String
getState = unit.policy.getState(position)
End Function
'TOM Change 20 May 2011
'Private Function getNextLocation(unit As TimeStep_UnitData) As String
'getNextLocation = unit.policy.nextlocation(unit.LocationName)
'End Function
Private Function getNextPosition(unit As TimeStep_UnitData) As String
getNextPosition = unit.policy.nextposition(unit.PositionPolicy)
End Function
Private Function getWaitTime(unit As TimeStep_UnitData, position As String, Optional deltat As Single) As Single
Dim nextposition As String

With unit
    nextposition = .policy.nextposition(position)
    getWaitTime = .policy.TransferTime(position, nextposition)
    getWaitTime = getWaitTime - (deltat - .StateData.remaining) '<- this gets the "next" transfer.
End With

End Function
'TOM Change 6 June 2011 -> same change from behaviorbase....should have these linked better.
Private Sub checkOverlap(unit As TimeStep_UnitData, fromloc As String, nextloc As String)

If nextloc = "Overlapping" Then
    'Decoupled*
    'unit.parent.parent.demandManager.disengage unit, unit.LocationName, True
    MarathonOpDemand.disengage simstate.demandstore, unit, unit.LocationName, simstate.context, True
ElseIf fromloc = "Overlapping" Then
    'Decoupled*
    'unit.parent.parent.demandManager.disengage unit, unit.LocationName, False
    MarathonOpDemand.disengage simstate.demandstore, unit, unit.LocationName, simstate.context, False
End If
End Sub
Private Sub checkDeployable(unit As TimeStep_UnitData, fromloc As String, nextloc As String)
With unit
    'Decoupled*
    If .policy.Deployable(fromloc) <> .policy.Deployable(nextloc) Then 'came home from deployment, "may" back in Reset
        '.parent.UpdateDeployStatus unit
        MarathonOpSupply.UpdateDeployStatus simstate.supplystore, unit, , , simstate.context
    End If
End With

End Sub
'''TODO -> these should be functions of policy, not parameters.
''Private Function exceedsCycle(NewCycle As Long, unit As TimeStep_UnitData) As Boolean
''Dim upperbound As Long
''exceedsCycle = NewCycle > unit.policy.CycleLength
''End Function
Private Function Deployable(location As String, policy As TimeStep_Policy) As Boolean
Deployable = policy.Deployable(location)
End Function
'TODO -> Get this back to working as a function of policy.
'TOM Change 3 Jan 2011 -> when conditions are met, units will start new cycles, update their cycles collection
'This is a boolean filter, based on POLICY, that determines if the unit's state change merits a new cycle
Private Function NewCycle(unit As TimeStep_UnitData, fromloc As String, toloc As String) As Boolean
NewCycle = (unit.policy.startstate = toloc)
End Function

'This is a very general and powerful mechanism to capture state changes in the unit.
Private Function StateExpired(unit As TimeStep_UnitData, deltat As Single) As Boolean
If unit.StateData.remaining <= deltat Then StateExpired = True
End Function
Private Function JustSpawned(unit As TimeStep_UnitData) As Boolean
'Decoupled*
JustSpawned = (unit.spawnTime = SimLib.getTime(simstate.context))
End Function
'State handler for generic updates that occur regardless of the state.
'These are specific to the unit data structure, not any particular state.
Private Function Global_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData
Dim nextposition As String
Dim nextstate As String

Set unit = ageUnit(unit, deltat)
    
If StateExpired(unit, deltat) Then 'time in current state has expired
    nextposition = getNextPosition(unit) 'what is the next location according to policy?
    Set Global_State = Moving_State(unit, 0, nextposition) 'TOM Note 29 Mar 2011 -> this now reflects an instant move.
End If

Set Global_State = unit
End Function
'Tom Change 1 July 2011
Private Function ageUnit(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData
With unit
    If deltat > 0 Then
        .cycletime = .cycletime + deltat 'units will always increase cycletime
        .StateData.timeinstate = .StateData.timeinstate + deltat 'we increase state time here, but we haven't
        'by changing the timeinstate, we've made the delta 0
        deltat = 0 'mutate the deltat variable
        'however, rollforward should be doing this for us by default.
    End If
End With
Set ageUnit = unit
End Function
'State to control how a unit acts when it spawns.
Private Function Spawning_State(unit As TimeStep_UnitData, deltat As Single, Optional toloc As String, Optional cycletime As Single) As TimeStep_UnitData
Dim newduration As Single
Dim offset As Single
Dim timeinstate As Single
Dim timeremaining As Single
Dim tnow As Single

tnow = SimLib.getTime(simstate.context)

With unit
    'Decoupled*
    .spawnTime = tnow
    .cycletime = 0
    'hack...
    timeinstate = 0
    'Tom Change 27 Mar 2012 - added facility for delineating ghosts, to keep bogdwell calcs from screwing up.
    'Decoupled*
    unit.InitCycles tnow, True
    .DateToReset = DateAdd("d", -.cycletime, simstate.parameters.startdate)
End With

Set Spawning_State = ChangeState(unit, "Moving", deltat)

End Function
Private Function Bogging_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData

If deltat > 0 Then
    MarathonOpUnit.AddBOG unit, deltat
End If

Set Bogging_State = unit
End Function
'Function to handle the occurence of an early withdraw from a deployment.
'when a demand deactivates, what happens to the unit?
'The behavior will be guided by (the unit's) policy.
'The default behavior is that a unit will check its policy to see if it CAN deploy.
'If policy says it's okay, the unit will return to the point time of its current lifecycle.
'We can parameterize the penalty it takes to get back into lifecycle from deployment.
'Note, we can also specify if the unit is instantly available to local demands.
Private Function AbruptWithdraw_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData
'----old
'MarathonOpUnit.AddBOG unit, deltat
''Consult policy to determine if entry back into available / ready pool is feasible.
'Set AbruptWithdraw_State = Moving_State(unit, deltat, unit.policy.StartState)
'------------------------------
Static bogremaining As Single

If deltat > 0 Then MarathonOpUnit.AddBOG unit, deltat
'Consult policy to determine if entry back into available / ready pool is feasible.
bogremaining = unit.CurrentCycle.bogbudget - unit.policy.overlap

If bogremaining <= 0 Then 'makes no sense for the unit to continue BOGGING, send it home.
    Set AbruptWithdraw_State = Reset_State(unit, deltat)
Else 'unit has some feasible bogtime left, we can possibly have it followon or extend its bog...
    'A follow-on is when a unit can immediately move to fill an unfilled demand from the same
    'group of demands.  In otherwords, its able to locally fill in.
        'This allows us to refer to forcelists as discrete chunks of data, group them together,
        'and allow forces to flow from one to the next naturally.
    'Decoupled*
    'parent.addFollowOn unit 'register the unit as a possible followOn
    MarathonOpSupply.addFollowOn simstate.supplystore, unit, simstate.context
    Set AbruptWithdraw_State = FollowOn_State(unit, 0) 'this puts us in position to followon.
End If

End Function
'way to get the unit back to reset.
Private Function Reset_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData
unit.followoncode = vbNullString
Set Reset_State = Moving_State(unit, deltat, unit.policy.startstate)
End Function
'Follow-on state is an absorbing state, where the unit waits until a changestate sends it elsewhere.
'The only feasible state transfers are to a reentry state, where the unit re-enters the arforgen pool
'in a dynamically determined position, or the unit goes to another demand compatible with the
'followon code.
Private Function FollowOn_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData

Set FollowOn_State = ageUnit(unit, deltat)

End Function
Private Function Moving_State(unit As TimeStep_UnitData, deltat As Single, Optional nextpos As String, _
                                                                Optional waitDuration As Single) As TimeStep_UnitData
Dim frompos As String
Dim logtime As Single
Dim newstate As String

Set Moving_State = unit
If nextpos = vbNullString Then nextpos = getNextPosition(unit)
With unit
    frompos = .PositionPolicy
    newstate = getState(unit, nextpos) '.policy.GetState(nextloc) 'derive the new state associated with the location
    
    If frompos <> nextpos Then
        'Decoupled*
        'logtime = .parent.getTime
        logtime = SimLib.getTime(simstate.context)
        .PositionPolicy = nextpos 'Move the unit .
        'Tom Change 6 June 2011
        'Decoupled*
        '.parent.LogPosition logtime, frompos, nextpos, unit  'record the move
        MarathonOpSupply.LogPosition logtime, frompos, nextpos, unit, , simstate.context
        .CurrentCycle.addTraversal logtime, frompos, nextpos
        
        checkOverlap unit, frompos, nextpos
        checkDeployable unit, frompos, nextpos
        
        Set unit = FinishCycle(unit, frompos, nextpos)
        If waitDuration = 0 Then waitDuration = getWaitTime(unit, nextpos) '- deltat
        If frompos = "Overlapping" Then
            'Decoupled*
            '.parent.parent.trigger GhostReturned, getDemandSRC(unit), .name, "ghost " & .name & " returned from deployment"
            SimLib.triggerEvent GhostReturned, getDemandSRC(unit), .name, "ghost " & .name & " returned from deployment", , simstate.context
        End If
    End If
    
    
    ''TOM Change 14 July 2011 -> Eliminated logging from ghost behavior, location change was
    'being logged 2x for ghosts...causing problems in sandtrends.
    'TOM Change 6 June 2011 -> HACK!
    If ((newstate = "Waiting") Or (newstate = "Nothing")) And .LocationName <> nextpos Then
        'unit.parent.LogMove logtime, .LocationName, nextpos, unit 'record the move
        MarathonOpUnit.changeLocation unit, nextpos, simstate.context
        'unit.LocationName = nextpos
    End If
    
    
    Set Moving_State = ChangeState(unit, newstate, deltat, waitDuration) 'changestates
End With

End Function
Private Function getDemandSRC(unit As TimeStep_UnitData) As String
'Decoupled*
'getDemandSRC = parent.parent.demandManager.demandmap(unit.LocationName).src
getDemandSRC = simstate.demandstore.demandmap(unit.LocationName).src

End Function

'Units dwelling will accumulate dwell over time.
Private Function Dwelling_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData
If deltat > 0 Then unit.AddDwell deltat
Set Dwelling_State = unit

End Function
'Units starting cycles will go through a series of procedures.
Private Function StartCycle_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData
'Possibly log this as an event?
With unit
    .cycletime = 0 'reset the time in cycle
    'Decoupled*
    .DateToReset = SimLib.getTime(simstate.context) '.parent.getTime 'reset the datetoreset
End With

Set StartCycle_State = unit
End Function
'Units ending cycles will record their last cycle locally.
Private Function EndCycle_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData
'Dim cycledict As Dictionary
With unit
    'Set cycledict = New Dictionary
    'Decoupled*
    .CurrentCycle.tfinal = SimLib.getTime(simstate.context) 'parent.getTime
    'cycledict.add "Cycle", .CurrentCycle
    'TOM Change 27 Mar 2012 -> avoids division by zero, using long
    If .CurrentCycle.dwell = 0 Then .CurrentCycle.dwell = 1
    'Decoupled*
'    .parent.parent.trigger CycleCompleted, .name, .parent.name, "Completed A Cycle", , , .CurrentCycle 'cycledict
    SimLib.triggerEvent CycleCompleted, .name, simstate.supplystore.name, "Completed A Cycle", .CurrentCycle, simstate.context 'cycledict

    .RecordCycle (.CurrentCycle.tfinal), True
End With

'Set cycledict = Nothing

If gtype = persistentghost Then
    Set EndCycle_State = BehaviorChange_State(unit, 0)
Else
    Set EndCycle_State = unit
End If

End Function
Private Function BehaviorChange_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData

If Not (nextbehavior Is Nothing) Then
    Set unit.behavior = nextbehavior
    Set BehaviorChange_State = ChangeState(unit, "StartCycle", deltat)
Else
    Set BehaviorChange_State = unit
End If

End Function

'Units in overlap accumulate dwell. They also may do other things.
Private Function Overlapping_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData

MarathonOpUnit.AddBOG unit, deltat

Set Overlapping_State = unit
End Function


Private Sub Class_Terminate()
'Set parent = Nothing
End Sub

'********Default Implementations for UnitBehavior Interface.*******************************
'********                                                   *******************************
Private Sub IUnitBehavior_init(state As TimeStep_SimState, Optional basebehavior As IUnitBehavior)
'Decouple
'Set parent = supply
Set simstate = state
'we don't utilize base behavior in this class.
End Sub
Private Function IUnitBehavior_ChangeState(unit As TimeStep_UnitData, tostate As String, deltat As Single, Optional duration As Single, Optional followingstate As String) As TimeStep_UnitData
Set IUnitBehavior_ChangeState = ChangeState(unit, tostate, deltat, duration, followingstate)
End Function
Private Property Let IUnitBehavior_name(ByVal RHS As String)
name = RHS
End Property
Private Property Get IUnitBehavior_name() As String
IUnitBehavior_name = name
End Property
Private Property Set IUnitBehavior_simstate(ByVal RHS As TimeStep_SimState)
Set simstate = RHS
End Property

Private Property Get IUnitBehavior_simstate() As TimeStep_SimState
Set IUnitBehavior_simstate = simstate
End Property

Private Function IUnitBehavior_update(tElapsed As Single, unit As TimeStep_UnitData) As TimeStep_UnitData
Set IUnitBehavior_update = update(tElapsed, unit)
End Function



