(ns marathon.port.data.unitbehavior)

'unitbehaviorbase
'Interface defining unit behaviors.
'AC behavior, RC behavior, etc. all implement these things.
'We have multiple options for implementing unique unit behaviors.

'1) The easiest option is to just use the base unit behavior class (this class).

'2) The next option is to embed this class in another class that extends the functionality of the base
'   behavior, while delegating standard functionality to the base class.
'   This is similar to inheritance (but not exactly...) in that we can wrap additional functionality (or
'   override specific functionality) in another class, delegating all other behavior to the base class.
'   By including the implements keyword, we can pass off the extended class as an instance of the base
'   class, so that unique behaviors can be utilized.

'3) The final option is to implement the public members of the unitbehavior base class entirely from scratch.
'   We do this in the case of Ghost Behavior, because it is very different from typical unit behaviors.
'   In this case, we just implement a function for Update and ChangeState.  We can make the new behavior
'   as complex as needed, or very very simple.

Option Explicit
Public name As String
'Decouple
'Public parent As TimeStep_ManagerOfSupply

'going to cheat for now...allow behaviors to have access to core data.
Public simstate As TimeStep_SimState

'We need to pass along a simcontext....to allow triggering of events and such.
'View this is a partially applied function, with a reference to simcontext.  We'll create
'constructors for the base behavior, moving init there...

Implements IUnitBehavior

Private Sub Class_Initialize()
name = "UnitBehaviorBase"
End Sub

'TODO -> Change the signature to use coredata to bind to environment.
Public Sub init(supply As TimeStep_ManagerOfSupply, Optional basicbehavior As IUnitBehavior)
'Decouple
'Set parent = supply
'this is the base class, so we won't do anything with the basebehavior argument.
End Sub
Public Function update(tElapsed As Single, unit As TimeStep_UnitData) As TimeStep_UnitData

Set update = RollForward(unit, tElapsed)

End Function
'What happens when we change a state?
'Some states are just blips (0 duration) .
'Some states are absorbing states (infinite duration) .
'Unless otherwise specified (by a duration), duration is assumed infinite.
Public Function ChangeState(unit As TimeStep_UnitData, tostate As String, deltat As Single, Optional duration As Single, _
                                        Optional followingstate As String) As TimeStep_UnitData

'TOM change 1 July 2011

If deltat > 0 Then Set unit = update(deltat, unit)

With unit
    .StateData.ChangeState tostate, duration, followingstate, True 'allows instant state changes.
    If duration > 0 Then
        'If duration <> .StateData.inf Then
            'Decoupled*
            MarathonOpUnit.requestUnitUpdate SimLib.getTime(simstate.context) + duration, unit, simstate.context
        'End If
    End If
    Set ChangeState = UpdateState(unit, 0) 'State Changes are instantaneous.  If the current state is instantaneous, then
End With

End Function
'Tom change 25 july 2011
'Ability to revert back to previous state, timeinstate, duration, etc.
Private Function RevertState(unit As TimeStep_UnitData) As TimeStep_UnitData

unit.StateData.RevertState
Set RevertState = unit

End Function

'This function allows us to roll through multiple updates, if delta T is too large.
'We could set deltaT to 1095 and get an entire cycle in theory.
Private Function RollForward(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData

Dim remaining As Single
Dim deltaNext As Single

If deltat <= unit.StateData.remaining Then 'will the change cause us to be in the same state?
    Set RollForward = UpdateState(unit, deltat)
Else
    While unit.StateData.remaining < deltat 'if the change means we're exceeding our time remaining...
        deltaNext = unit.StateData.remaining 'this entire logic branch prevents us from overreaching time in state.
        deltat = deltat - deltaNext
        Set RollForward = UpdateState(unit, deltaNext)
    Wend
End If

End Function

Private Function UpdateState(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData

If Not specialstate(unit.StateData.CurrentState) Then
    Select Case unit.StateData.CurrentState
        Case Is = "Reset"
            Set UpdateState = Reset_State(unit, deltat)
        Case Is = "Bogging"
            Set UpdateState = Bogging_State(unit, deltat)
        Case Is = "Moving"
            Set UpdateState = Moving_State(unit, deltat)
        Case Is = "Dwelling"
            Set UpdateState = Dwelling_State(unit, deltat)
        Case Is = "StartCycle"
            Set UpdateState = StartCycle_State(unit, deltat)
        Case Is = "EndCycle"
            Set UpdateState = EndCycle_State(unit, deltat)
        Case Is = "Overlapping"
            Set UpdateState = Overlapping_State(unit, deltat)
        Case Is = "DeMobilizing"
            Set UpdateState = DeMobilizing_State(unit, deltat)
        Case Is = "PolicyChange"
            Set UpdateState = PolicyChange_State(unit, deltat)
        Case Is = "Recovering"
            Set UpdateState = Recovering_State(unit, deltat)
        Case Is = "Recovered"
            Set UpdateState = Recovered_State(unit, deltat)
        Case Is = "Nothing"
            Set UpdateState = unit
    Case Else
        Err.Raise 101, , "No implementation for unit state" & unit.StateData.CurrentState
    End Select
    Set unit = Global_State(unit, deltat) 'run global tasks common to every state.
Else
    Select Case unit.StateData.CurrentState
        Case Is = "Spawning"
            Set UpdateState = Spawning_State(unit, deltat)
        Case Is = "AbruptWithdraw"
            Set UpdateState = AbruptWithdraw_State(unit, deltat)
        
    End Select
End If

End Function
'Executes an instantaneous behavior change.  Behavior changes allow units to interpret policies
'differently...It's bascially changing to another Finite State Machine implementation, where the states
'are interpreted differently.
'The transition between behaviors is accomplished by
Private Function BehaviorChange_State(unit As TimeStep_UnitData, deltat As Single, nextbehavior As IUnitBehavior) As TimeStep_UnitData

If Not (nextbehavior Is Nothing) Then
    Set unit.behavior = nextbehavior
    Set BehaviorChange_State = ChangeState(unit, "StartCycle", deltat)
Else
    Set BehaviorChange_State = unit
End If

End Function
Private Function specialstate(instate As String) As Boolean
specialstate = instate = "Spawning" Or instate = "AbruptWithdraw"
End Function
Private Function FinishCycle(unit As TimeStep_UnitData, frompos As String, topos As String) As TimeStep_UnitData
Set FinishCycle = unit

If NewCycle(unit, frompos, topos) Then
    If Not JustSpawned(unit) Then
        Set FinishCycle = StartCycle_State(EndCycle_State(unit, 0), 0) 'wrap up the cycle.
    End If
End If

End Function
Private Function getState(unit As TimeStep_UnitData, position As String) As String
getState = unit.policy.getState(position)
End Function
'Tom Change 24 May 2011
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
'TODO -> reduce this down to one logical condition man....
'TOM Change 6 June -> pointed disengagement toward demand name, via unit.LocationName
Private Sub checkOverlap(unit As TimeStep_UnitData, frompos As String, nextpos As String)

If nextpos = "Overlapping" Then
    'Decoupled*
    'unit.parent.parent.demandManager.disengage unit, unit.LocationName, True
    MarathonOpDemand.disengage simstate.demandstore, unit, unit.LocationName, simstate.context, True
ElseIf frompos = "Overlapping" Then
    'Decoupled*
    'unit.parent.parent.demandManager.disengage unit, unit.LocationName, False
    MarathonOpDemand.disengage simstate.demandstore, unit, unit.LocationName, simstate.context, False
End If

End Sub
Private Sub checkDeployable(unit As TimeStep_UnitData, frompos As String, nextpos As String)
With unit
    If .policy.Deployable(frompos) <> .policy.Deployable(nextpos) Then 'came home from deployment, "may" back in Reset
        'Decoupled*
        '.parent.UpdateDeployStatus unit
        MarathonOpSupply.UpdateDeployStatus simstate.supplystore, unit, , , simstate.context
    End If
End With

End Sub
'TODO -> these should be functions of policy, not parameters.
Private Function exceedsCycle(NewCycle As Long, unit As TimeStep_UnitData) As Boolean
Dim upperbound As Long
exceedsCycle = NewCycle > unit.policy.cyclelength
End Function
Private Function Deployable(position As String, policy As TimeStep_Policy) As Boolean

Deployable = policy.isDeployable(policy.GetCycleTime(position))

End Function
'TODO -> Get this back to working as a function of policy.
'TOM Change 3 Jan 2011 -> when conditions are met, units will start new cycles, update their cycles collection
'This is a boolean filter, based on POLICY, that determines if the unit's state change merits a new cycle
Private Function NewCycle(unit As TimeStep_UnitData, frompos As String, topos As String) As Boolean
NewCycle = (topos = unit.policy.startstate)
End Function

'This is a very general and powerful mechanism to capture state changes in the unit.
Private Function StateExpired(unit As TimeStep_UnitData, deltat As Single) As Boolean

If unit.StateData.remaining <= deltat Then StateExpired = True

End Function
Private Function JustSpawned(unit As TimeStep_UnitData) As Boolean
'Decoupled*
'JustSpawned = (unit.spawnTime = parent.getTime)
JustSpawned = (unit.spawnTime = SimLib.getTime(simstate.context))
End Function
'State handler for generic updates that occur regardless of the state.
'These are specific to the unit data structure, not any particular state.
Private Function Global_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData
Dim nextposition As String
Dim nextstate As String
Dim change As Boolean

change = False
'bring the unit up to current time
Set unit = ageUnit(unit, deltat)

If StateExpired(unit, deltat) Then 'time in current state has expired
    nextposition = getNextPosition(unit) 'what is the next position according to policy?
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
Private Function Spawning_State(unit As TimeStep_UnitData, deltat As Single, Optional topos As String, Optional cycletime As Single) As TimeStep_UnitData
Dim newduration As Single
Dim offset As Single
Dim timeinstate As Single
Dim timeremaining As Single
Dim nextstate As String
'added for overuse of .gettime
Dim tnow As Single

tnow = SimLib.getTime(simstate.context)

With unit
    If topos = vbNullString And .PositionPolicy = vbNullString Then 'no target position to spawn at, must derive
        .PositionPolicy = .policy.getPosition(.cycletime) '.parent.parent.PolicyManager.DeriveLocationID(.cycletime, . component)
        topos = .PositionPolicy '.parent.parent.policyManager.LocationIndex(.location)
        'Decoupled*
        '.location = .parent.parent.policyManager.locationID(topos)
        .location = MarathonOpPolicy.locationID(topos, simstate.policystore)
    Else 'target position, derive index
        topos = .PositionPolicy
        'Decoupled*
        '.location = .parent.parent.policyManager.locationID(topos)
        .location = MarathonOpPolicy.locationID(topos, simstate.policystore) 'TODO <----this is redundant, see if we can eliminate it.
    End If
    'Decoupled*
    '.spawnTime = .parent.getTime
    .spawnTime = tnow
    'hack...
    If .cycletime >= 0 Then
        timeinstate = .cycletime - .policy.GetCycleTime(.PositionPolicy) 'derived time in state upon spawning.
        'Decoupled*
        'unit.InitCycles parent.getTime
        unit.InitCycles tnow
        unit.AddDwell .cycletime
    Else
        unit.AddBOG timeinstate
        Err.Raise 101, , "Negative cycle times are not handled..."
    End If
    timeremaining = .policy.TransferTime(.PositionPolicy, .policy.nextposition(.PositionPolicy)) 'time remaining in this state
    newduration = timeremaining - timeinstate
    
    If .cycletime > 0 Then
        'Decoupled*
'       .DateToReset = DateAdd("d", -.cycletime, .parent.parent.parameters.startdate)
        .DateToReset = DateAdd("d", -.cycletime, simstate.parameters.startdate)
        nextstate = "Dwelling"
    ElseIf .cycletime = 0 Then
        'TOM note double check this.  make it point to current date.
        'Decoupled*
        '.DateToReset = .parent.parent.parameters.startdate
        .DateToReset = simstate.parameters.startdate
        nextstate = "Dwelling"
    Else 'account for deployed/bogging
        newduration = .cycletime
        nextstate = "Bogging"
    End If

    'initialize cycle from policy
End With

Set Spawning_State = ChangeState(unit, nextstate, 0, newduration)

'TOM Change -> Changed from "Initialized" to "Spawning"
With unit
    'Decoupled*
    '.parent.LogMove .spawnTime, "Spawning", .PositionPolicy, unit, newduration
    MarathonOpSupply.LogMove .spawnTime, "Spawning", .PositionPolicy, unit, newduration, simstate.context
End With

End Function

Private Function Bogging_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData

If deltat > 0 Then
    With unit
        .AddBOG deltat
        '.DeployTimeRemaining = .DeployTimeRemaining - deltat
    End With
End If

Set Bogging_State = unit
End Function
'Function to handle the occurence of an early withdraw from a deployment.
'when a demand deactivates, what happens to the unit?
'The behavior will be guided by (the unit's) policy.
'The default behavior is that a unit will check its policy to see if it CAN deploy.
'If policy says it's okay, the unit will return to the point time of its current lifecycle.
'We can parameterize the penalty it takes to get back into lifecycle from deployment.
    'A usual penalty is a move to "90 days of recovery"
'Note, we can also specify if the unit is instantly available to local demands.
Private Function AbruptWithdraw_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData

Dim bogremaining As Single

If deltat > 0 Then MarathonOpUnit.AddBOG unit, deltat
'Consult policy to determine if entry back into available / ready pool is feasible.
'TOM note 18 july 2012 -> this is erroneous.  We were check overlap....that's not the definition of
'a unit's capacity to re-enter the available pool.
'bogremaining = unit.CurrentCycle.bogbudget - unit.policy.overlap

'Recovery should now be an option by default, not specifically dictated by
'policy.

'On second thought, this is sound.  If the unit is already in overlap, it's in a terminal state..
'For followon eligibility, it means another unit would immediately be overlapping this one anyway,
'and the demand would not be considered filled....It does nothing to alleviate the demand pressure,
'which is the intent of followon deployments.  Conversely, if overlap is 0, as in typical surge
'periods, then units will always followon.  I take back my earlier assessment, this is accurate.
bogremaining = unit.CurrentCycle.bogbudget - unit.policy.overlap

If bogremaining <= 0 Then 'makes no sense for the unit to continue BOGGING, send it home.
    Set AbruptWithdraw_State = Reset_State(unit, deltat)
Else 'unit has some feasible bogtime left, we can possibly have it followon or extend its bog...
    'A follow-on is when a unit can immediately move to fill an unfilled demand from the same
    'group of demands.  In otherwords, its able to locally fill in.
        'This allows us to refer to forcelists as discrete chunks of data, group them together,
        'and allow forces to flow from one to the next naturally.
    
    'Decoupled*
'   parent.addFollowOn unit 'register the unit as a possible followOn
    MarathonOpSupply.addFollowOn simstate.supplystore, unit, simstate.context 'register the unit as a possible followOn
    
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
'A state to handle reentry into the available pool....
Private Function ReEntry_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData

Dim newpolicy As IRotationPolicy
Dim wasDeployable As Boolean
Dim isDeployable As Boolean
Dim newduration As Single
Dim offset As Single
Dim timeinstate As Single
Dim timeremaining As Single
Dim nextstate As String
Dim newcycletime As Single
Dim cycletimeA As Single
Dim policynameA As String
Dim policynameB As String
Dim CycleProportionA As Single
Dim CycleTimeB As Single
Dim PositionA As String
Dim PositionB As String

With unit
    
    'TOM Change 20 April 2012
    'wasDeployable = .policy.isDeployable(.cycletime)
    wasDeployable = .policy.isDeployable(.cycletime)

    'HACK to account for negative cycle times.
    If .cycletime < 0 Then Err.Raise 101, , "Cycle Time Should not be negative!"
    
    If .PositionPolicy <> "Deployed" And .PositionPolicy <> "Overlapping" Then  'we can safely change the unit's policy.
       
        isDeployable = .policy.isDeployable(.cycletime)
        PositionA = .PositionPolicy
        If .PositionPolicy <> "Deployed" Then
            If .PositionPolicy = "Overlapping" Then Err.Raise 101, , "Forgot about overlap.."
            PositionB = .policy.getPosition(.cycletime) 'dwelling units can change positions.
        Else
            Err.Raise 101, , "Cannot be deployed!"
        End If
        
        'position policy is the SAME as positionB
        .PositionPolicy = PositionB
        
        timeremaining = .policy.TransferTime(PositionB, .policy.nextposition(PositionB)) 'time remaining in this state
        
        'How long will the unit have been in this state?
            'Since it's a policy change....do we zero it out?
            'Or do we assume that the unit has been in the state the exact amount of time required?
        'We assume that the unit has been in the state the exact amount of time required.
        'We also assume that the unit is not entering another cycle, merely extending or truncating.
            'Its current cycle is modified.
            'Does not get a cycle completion out of it.
            
        If .cycletime >= 0 Then
            If PositionB <> "Deployed" Then
                timeinstate = .cycletime - .policy.GetCycleTime(PositionB) 'derived time in state upon spawning.
            Else
                Err.Raise 101, , "Error, should not be deployed"
            End If
        End If
        
        newduration = timeremaining - timeinstate
        nextstate = getState(unit, PositionB)
        
        'TOM change 23 july 2011
        'Decoupled*
        'parent.LogPosition parent.getTime, PositionA, PositionB, unit
        MarathonOpSupply.LogPosition SimLib.getTime(simstate.context), PositionA, PositionB, unit, , simstate.context
        
        If nextstate = "Dwelling" Or nextstate = "DeMobilizing" _
                Or nextstate = Recovering Or nextstate = Recovered Then
            'parent.LogMove parent.getTime, .LocationName, .PositionPolicy, unit
            .changeLocation .PositionPolicy, simstate.context
            '.LocationName = .PositionPolicy
        End If
        
        'Decoupled*
        '.parent.UpdateDeployStatus unit
        MarathonOpSupply.UpdateDeployStatus simstate.supplystore, unit, , , simstate.context

        'Decouple*
        'parent.parent.trigger supplyUpdate, .name, .name, "Unit ReEntering with " & .CurrentCycle.bogbudget & " BOGBudget & " & .name
        SimLib.triggerEvent supplyUpdate, .name, .name, "Unit ReEntering with " & .CurrentCycle.bogbudget & " BOGBudget & " & .name, , simstate.context
        Set ReEntry_State = ChangeState(unit, nextstate, 0, newduration)
    Else
        Err.Raise 101, , "Cannot handle during deployment or overlap"
    End If
End With

End Function

'''TOM Change 1 June 2011 -> Renamed moving state to Repositioning_State.  Location changes (moves) cannot happen without a
'''change of position.  This is convention.  Change of position implies a state transition.
''Private Function Repositioning_State(unit As TimeStep_UnitData, deltat As Single, _
''                                Optional nextpos As String, Optional waitDuration As Single) As TimeStep_UnitData
''
''Dim frompos As String
''Dim logtime As Single
''Dim newstate As String
''
''Set Moving_State = unit
''If nextpos = vbnullstring Then nextpos = getNextPosition(unit)
''With unit
''    frompos = .PositionPolicy
''    If frompos <> nextpos Then
''        logtime = .parent.gettime
''        .PositionPolicy = nextpos 'Move the unit .
''        .CurrentCycle.addTraversal logtime, frompos, nextpos
''
''        checkOverlap unit, frompos, nextpos
''        checkDeployable unit, frompos, nextpos
''
''        Set unit = FinishCycle(unit, frompos, nextpos)
''        If waitDuration = 0 Then waitDuration = getWaitTime(unit, nextpos) '- deltat
''        .parent.LogMove logtime, frompos, nextpos, unit, waitDuration  'record the move
''    End If
''
''    newstate = getState(unit, nextpos) 'derive the new state associated with the location
''    Set Moving_State = ChangeState(unit, newstate, waitDuration)  'changestates
''End With
''
''
''End Function

'TOM Change 1 June 2011 -> Added a subordinate routine to deal with changes in the spatial
'location of a unit, i.e. "moves".
'By convention, every call to moving state will also call update_location, to ensure the unit's
'location is kept up to date.
    'Location changes are always precipitated by position updates.  A unit cannot change its non-temporal
    'state without realizing a positional change.
Private Function Moving_State(unit As TimeStep_UnitData, deltat As Single, _
                                Optional nextpos As String, Optional waitDuration As Single) As TimeStep_UnitData
Dim frompos As String
Dim logtime As Single
Dim newstate As String

Set Moving_State = unit
If nextpos = vbNullString Then nextpos = getNextPosition(unit)
With unit
    frompos = .PositionPolicy
    newstate = getState(unit, nextpos) 'derive the new state associated with the location
    
    If frompos <> nextpos Then
        'Decouple
        logtime = SimLib.getTime(simstate.context)
        
        'TOM Change 24 July -> moved down....
        .PositionPolicy = nextpos 'Move the unit .
        .CurrentCycle.addTraversal logtime, frompos, nextpos
        
        'TOM note 6 June 2011 -> this both cause side-effects....
        checkOverlap unit, frompos, nextpos
        checkDeployable unit, frompos, nextpos
        
        'This is where mutation occurs.  PolicyChange could happen here.
        'If there is a policy change, we need to record the intermediate location change as well.
        'We come in from overlapping to reset.  The positionpolicy is updated to reset.
        'We go into finishcycle, with a pending policy change on the policy stack.
        'The policy change is applied, to the current position and cycletime....
            'which are both 0, and don't produce a neglible side effect.
        'The policy change then changes the location from
        Set unit = FinishCycle(unit, frompos, nextpos)
        
        
        'TOM change 21 july 2011
        If waitDuration = 0 And unit.StateData.remaining = 0 Then
            waitDuration = getWaitTime(unit, nextpos) '- deltat
        Else
            waitDuration = unit.StateData.remaining
        End If
        
        'TOM Change 6 June 2011
        If nextpos <> .PositionPolicy Then nextpos = .PositionPolicy
        'Decoupled*
        '.parent.LogPosition logtime, frompos, nextpos, unit, waitDuration  'record the move
        MarathonOpSupply.LogPosition logtime, frompos, nextpos, unit, waitDuration, simstate.context 'record the move
    End If

    'TOM Change 6 June 2011 -> HACK
    'We want overlapping units to remain associated with their locations....
    'Tom Change 19 April 2012
    If newstate = "Dwelling" Or newstate = "DeMobilizing" Or newstate = "Recovering" Then .changeLocation nextpos, simstate.context  '.LocationName = nextpos
    Set Moving_State = ChangeState(unit, newstate, deltat, waitDuration) 'changestates
End With

End Function

'TOM Change 13 Jul 2011
'Needed to implement the transition from one policy to another.  I chose to add a state to handle just this.
'Visual analysis showed that PolicyChange looks a lot like Spawn, in that when a unit changes policies, it must change
'a lot of its internal state to follow the new policy.  The result of the policy change is:
'   1: The unit's cycle time is normalized, and then transformed into the relevant cycletime in the new policy.
'   2: The unit's position "may" change to reflect its position in the new policy.
'   3: The unit's location "may" change to reflect its location in the new policy.
'TOM Change 20 April:
'   4: The unit's BOGBudget "may" change to reflect either increased, or decreased, BOGBudget.
'TOM Change 24 April:
'   5: The unit's BOGBudget and MAXBOG may only change (increase) as the result of a policy change.
'   6: Policy changes can NOT happen during terminal states:
'       [Deployed {Bogging, Overlapping}, Demobilizing]


'If NOT deployed (bogging, overlapping) or in a terminal state (demobilizing), then entities can change policy immediately.
'Otherwise, units change policy upon next reset (change is deferred).

'Assumes that the new policy is already set for the unit (i.e. the unitdata is pointing toward the new policy).
'Ideally, an outside agent will have modified the unit's policy, and subsequently told it to changestates to a policy-change
'state.

'Net effect is that policy changes to the same policy are idempotent.
'State to control how a unit acts when it changes policy.
'Note -> we extract the next policy from the unitdata's policy stack.

'TOM note -> figure out how to change this for the deployed population...they have negative cycle
'times.

'Note -> this assumes we have compatible policies, or at least policies that have a cyclical
'rotational lifecycle.
Function PolicyChange_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData

Dim newpolicy As IRotationPolicy

Dim wasDeployable As Boolean
Dim isDeployable As Boolean

Dim newduration As Single
Dim offset As Single
'tom change
Dim tnow As Single
Dim timeinstate As Single
Dim timeremaining As Single
Dim nextstate As String

Dim newcycletime As Single

Dim cycletimeA As Single
Dim policynameA As String
Dim policynameB As String
Dim CycleProportionA As Single
Dim CycleTimeB As Single
Dim PositionA As String
Dim PositionB As String


tnow = SimLib.getTime(simstate.context)

With unit

    If .policyStack.count = 0 Then Err.Raise 101, , "no new policies on stack!"
               
    'fetch the new policy defined by the IRotationPolicy.
    'Note, for composite policies, the newpolicy defers to the active policy.
    'The active policy should have been modified for us.  That's a weak spot...
    Set newpolicy = .policyStack(.policyStack.count)
    
    If newpolicy.bogbudget = 0 Then Err.Raise 101, , "No bog budget!"
    policynameA = .policy.AtomicName 'points to the active atomic policy
    policynameB = newpolicy.AtomicName 'points to the active atomic policy
    
    'TOM Change 20 April -> We need to separate the unit's experienced
    'cycle length vs the NOMINAL cycle duration, which exists in
    'POLICY SPACE.  In composite rotational policies, the NOMINAL cycle duration
    'changes when Atomic policies change.  Specificallly, we map the unit's position
    'or coordinates in the current atomic policy to coordinates in the new policy.
    'The unit's actual experienced lifecycle, i.e. its cycletime property, is not
    'an accurate mapping between policies.  The implicit assumption is that when
    'mapping from one policy to another, if the policies have differing cycle lengths
    'then there is a discount or exchange rate between the policies, such that time
    'spent in one policy is NOT equal to time spent in another.  However, our
    'unit's cyclelength property is not subject to this, since it technically
    'exists OUTSIDE of the policy view of time.  The cyclelength property reflects the
    'actual time a unit has spent, under ANY policy, until it has reset or started a
    'new cycle.
    
    'Prior to 19 April 2012, The unit's ability to deploy, via the CanDeploy method,
    'depended on it's position in the current policy as a function of the cyclelength property.
    'We should prefer the duration of the current cycle record, which is an accurate reflection
    'of the relative time in the unit's current policy.
    cycletimeA = .cycletime
    
    'TOM Change 20 April 2012
    'cycletimeA = .CurrentCycle.duration

    PositionA = .PositionPolicy
    'TOM Change 20 April 2012
    'wasDeployable = .policy.isDeployable(.cycletime)
    wasDeployable = .policy.isDeployable(.cycletime)

    
    'HACK to account for negative cycle times.
    If .cycletime < 0 Then Err.Raise 101, , "Cycle Time Should not be negative!"
    
    CycleProportionA = .cycletime / .policy.cyclelength
    'TOM change 23 April 2012 -> No longer allow units that are De-mobilizing to enter into available pool.
    
    If CycleProportionA <= 1# And PositionA <> "Deployed" And _
        PositionA <> "Overlapping" And PositionA <> "DeMobilizing" Then   'we can safely change the unit's policy.

        'Tom Change -> fixed to integers.
        CycleTimeB = Fix(CycleProportionA * newpolicy.cyclelength)
        If CycleTimeB > newpolicy.cyclelength Then Err.Raise 101, , "Cyclelength is too long!"
        
        isDeployable = newpolicy.isDeployable(CycleTimeB)
        
        If .PositionPolicy <> "Deployed" Then
            If .PositionPolicy = "Overlapping" Then Err.Raise 101, , "Forgot about overlap.."
            PositionB = newpolicy.getPosition(CycleTimeB) 'dwelling units can change positions.
        Else
            PositionB = .PositionPolicy 'deployed units remain deployed.
        End If
        
        'Tom Note 12 July 2012
        'This actually updates the unit's policy....
        'What are the consequences?
        'We 're setting its IRotation policy to the new policy....
        'We should be setting it to the root policy....
        Set .policy = newpolicy
        'position policy is the SAME as positionB
        .PositionPolicy = PositionB
        
        timeremaining = .policy.TransferTime(PositionB, .policy.nextposition(PositionB)) 'time remaining in this state
        
        'How long will the unit have been in this state?
            'Since it's a policy change....do we zero it out?
            'Or do we assume that the unit has been in the state the exact amount of time required?
        'We assume that the unit has been in the state the exact amount of time required.
        'We also assume that the unit is not entering another cycle, merely extending or truncating.
            'Its current cycle is modified.
            'Does not get a cycle completion out of it.
        .cycletime = CycleTimeB
       
        If .cycletime >= 0 Then
            If PositionB <> "Deployed" Then
                timeinstate = .cycletime - .policy.GetCycleTime(PositionB) 'derived time in state upon spawning.
                
                'Decoupled*
'                unit.ChangeCycle parent.getTime
                unit.ChangeCycle tnow
                unit.ModifyCycle newpolicy
                
            Else 'how long have we been deployed?  Assume it's BOG for now....
                timeinstate = .CurrentCycle.bog  'TOM Note This may change.
            End If
        Else
            unit.AddBOG timeinstate
            Err.Raise 101, , "Negative cycle times are not handled..."
        End If
        
        newduration = timeremaining - timeinstate
        nextstate = getState(unit, PositionB)
        
        'TOM change 23 july 2011
        If PositionA <> PositionB Then
            'Decoupled*
'           parent.LogPosition parent.getTime, PositionA, PositionB, unit
            MarathonOpSupply.LogPosition tnow, PositionA, PositionB, unit, , simstate.context

            If nextstate = "Dwelling" Or nextstate = "DeMobilizing" _
                    Or nextstate = Recovering Or nextstate = Recovered Then
                'parent.LogMove parent.getTime, .LocationName, .PositionPolicy, unit
                .changeLocation .PositionPolicy, simstate.context
                '.LocationName = .PositionPolicy
            End If
        End If
        
        
        'If wasDeployable <> isDeployable Then .parent.UpdateDeployStatus unit
        'Decoupled*
        '.parent.UpdateDeployStatus unit
        MarathonOpSupply.UpdateDeployStatus simstate.supplystore, unit, , , simstate.context
        
        'Adopt Policy B.
        'Policy A ->
        '    Find relative CT = ct/CLengthA
        'Policy B ->
        '    Find relative positionB = pos(RelativeCT * CLengthB)
        'Movingstate from PositionA to relative PositionB.
        'Update with delta0.
        'TOM Change 2 Sep -> moved this north so that we can use the policy stack as a flag in unit's
        'ChangeCycle logic.  Check for sideeffects
        .policyStack.Remove 1
        
        'Decoupled*
'        parent.parent.trigger UnitChangedPolicy, .name, .policy.AtomicName, "Unit " & .name & " changed policies: " & _
'            policynameA & ":" & cycletimeA & "->" & policynameB & ":" & CycleTimeB
        SimLib.triggerEvent UnitChangedPolicy, .name, .policy.AtomicName, "Unit " & .name & " changed policies: " & _
            policynameA & ":" & cycletimeA & "->" & policynameB & ":" & CycleTimeB, , simstate.context

        
        'parent.parent.trigger SupplyUpdate, .name, .name, "Policy Change Caused Supply Update"
        Set PolicyChange_State = ChangeState(unit, nextstate, 0, newduration)
        'Decoupled*
'        parent.parent.trigger supplyUpdate, .name, .name, "Policy Change Caused Supply Update for unit " & .name
        SimLib.triggerEvent supplyUpdate, .name, .name, "Policy Change Caused Supply Update for unit " & .name, , simstate.context

        'NOTE -> I may need to consider changing location here.....
    Else 'The unit's cycle cannot project onto another cycle.  We need to defer policy change until reset.
        'leave the policy on the stack.  Catch it during reset.
        ''TOM change 2 Sep 2011 -> we modify the cyclerecord to reflect changes in expectations...
        'This is not a replacement...
        
        'TOM Change 23 April 2012 -> We do NOT modify the cycle.....screws with expectations?
        '.ModifyCycle newpolicy
        
        'Decoupled*
'        parent.parent.trigger AwaitingPolicyChange, .name, .policy.AtomicName, "Unit " & _
'                .name & " in position " & .PositionPolicy & " is waiting until reset to change policies"
        SimLib.triggerEvent AwaitingPolicyChange, .name, .policy.AtomicName, "Unit " & _
                .name & " in position " & .PositionPolicy & " is waiting until reset to change policies", , simstate.context
        Set unit = RevertState(unit)
        'We updated the unit in the process
        'Decoupled*
        'parent.parent.trigger supplyUpdate, .name, .name, "Policy Change Attempt Caused Supply Update for unit " & .name
        SimLib.triggerEvent supplyUpdate, .name, .name, "Policy Change Attempt Caused Supply Update for unit " & .name, , simstate.context
    End If
End With

End Function
'Units dwelling will accumulate dwell over time.
Private Function Dwelling_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData
If deltat > 0 Then unit.AddDwell deltat
Set Dwelling_State = unit
End Function

'TOM Change 23 April 2012 -> Check to see if we should even try to recover....
'Criteria: if cycletime + recoverytime > cycleduration then 'skip recovery....
'go straight to recovered.  This will push us into whatever the post recovery state was...
Public Function Recovering_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData

Dim nm As String
If canRecover(unit) Then
    If deltat > 0 Then unit.AddDwell deltat
    Set Recovering_State = unit
Else
    nm = unit.name
    'decoupled*
    'parent.parent.trigger supplyUpdate, nm, nm, "Unit " & nm & " Skipping Recovery with " & unit.CurrentCycle.bogbudget & " BOGBudget"
    SimLib.triggerEvent supplyUpdate, nm, nm, "Unit " & nm & " Skipping Recovery with " & unit.CurrentCycle.bogbudget & " BOGBudget", , simstate.context
    'zero out the unit's ability to BOG, preventing recovery and re-Entry.
    unit.CurrentCycle.bogbudget = 0
    Set Recovering_State = Moving_State(unit, deltat) 'advance the unit forward in time.
End If

End Function

'TOM Change 23 April 2012
'Auxillary function to help us determine whether a unit should try to recover....
Private Function canRecover(unit As TimeStep_UnitData) As Boolean
Dim rtime As Single
Dim demobtime As Single 'Tom Change 18 July 2012

canRecover = False

With unit
    'Tom change 18 July 2012 ->
        'Allowing general recovery as a default behavior, although policies can change it...
            'I.e. RC14 remob is an example.
        'If the unit's policy doesn't have a recovery state, we set an abitrary recoverytime.
        'Derived from Parameters("DefaultRecoveryTime")
        
    'Tom Change 23 April 2012
    If .policy.PositionGraph.nodeExists(recovery) Then
        rtime = .policy.TransferTime(recovery, Recovered)
    Else 'Decoupled*
        'rtime = parent.parent.parameters.getKey("DefaultRecoveryTime")
        rtime = simstate.parameters.getKey("DefaultRecoveryTime")
        'Tom note -> I think this is a moot point, because our criteria for recovering is to not
        'exceed the expected duration of a cycle.  If we're already exceeding it, we don't have to
        'check demob.
        'if the policy includes demob, we need to account for demob time as well.
'        If .policy.PositionGraph.nodeExists(demobilization) Then
'            rtime = .policy.TransferTime(demobilization, .policy.nextposition(demobilization))
'        End If
    End If
    
    If .CurrentCycle.bogbudget > 0 And .cycletime + rtime < .CurrentCycle.DurationExpected Then
        canRecover = True
    End If
End With

End Function
'A state in which the unit will attempt to re-enter the available pool, if it has enough BOG time remaining.
Public Function Recovered_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData
Set Recovered_State = unit
Dim entryPosition As String
Dim timeremaining As Single
Dim timeinstate As Single
Dim newduration As Single

With unit
    'Tom Change 23 April 2012
    If .CurrentCycle.bogbudget > 0 Then
        If .cycletime < .CurrentCycle.DurationExpected Then
            Set unit = ReEntry_State(unit, deltat) 'We try to re-enter the available pool.
        End If
    Else
        Set unit = Moving_State(unit, deltat)
    End If
End With
       
End Function
'Units starting cycles will go through a series of procedures.
Private Function StartCycle_State(unit As TimeStep_UnitData, deltat As Single, Optional tnow As Single) As TimeStep_UnitData
'Possibly log this as an event?
If IsMissing(tnow) Then tnow = SimLib.getTime(simstate.context)
With unit
    .cycletime = 0 'reset the time in cycle
    'Decoupled*
'    .DateToReset = .parent.getTime 'reset the datetoreset
    .DateToReset = tnow 'reset the datetoreset
    'TOM change 19 Jul 2011
    'Check to see if we have a pending policychange.
    If .policyStack.count > 0 Then
        Set unit = PolicyChange_State(unit, deltat)
    End If
End With

Set StartCycle_State = unit
End Function
'Units ending cycles will record their last cycle locally.
Private Function EndCycle_State(unit As TimeStep_UnitData, deltat As Single, Optional tnow As Single) As TimeStep_UnitData
If IsMissing(tnow) Then tnow = SimLib.getTime(simstate.context)
With unit
    'Set cycledict = New Dictionary
    'Decouple
    .CurrentCycle.tfinal = tnow
    'cycledict.add "Cycle", .CurrentCycle
    'Decoupled*
    SimLib.triggerEvent CycleCompleted, .name, simstate.supplystore.name, "Completed A Cycle", .CurrentCycle, simstate.context 'cycledict
    .RecordCycle (.CurrentCycle.tfinal)
End With

'Set cycledict = Nothing

Set EndCycle_State = unit
End Function

'Units in overlap accumulate dwell. They also may do other things.
Private Function Overlapping_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData

MarathonOpUnit.AddBOG unit, deltat

Set Overlapping_State = unit
End Function
'Units in overlap accumulate dwell. They also may do other things.
Private Function DeMobilizing_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData

unit.addMOB deltat
Set DeMobilizing_State = unit

End Function

Private Sub Class_Terminate()
'Set parent = Nothing
End Sub

Private Property Let IUnitBehavior_name(ByVal RHS As String)
name = RHS
End Property
Private Property Get IUnitBehavior_name() As String
IUnitBehavior_name = name
End Property
Private Sub IUnitBehavior_init(state As TimeStep_SimState, Optional basicbehavior As IUnitBehavior)
Set simstate = state
End Sub
Private Function IUnitBehavior_ChangeState(unit As TimeStep_UnitData, tostate As String, deltat As Single, Optional duration As Single, Optional followingstate As String) As TimeStep_UnitData
Set IUnitBehavior_ChangeState = ChangeState(unit, tostate, deltat, duration, followingstate)
End Function
Private Property Set IUnitBehavior_simstate(ByVal RHS As TimeStep_SimState)
Set simstate = RHS
End Property

Private Property Get IUnitBehavior_simstate() As TimeStep_SimState
Set IUnitBehavior_simstate = simstate
End Property

Private Function IUnitBehavior_update(tElapsed As Single, unit As TimeStep_UnitData) As TimeStep_UnitData
Set IUnitBehavior_update = update(tElapsed, unit)
End Function





