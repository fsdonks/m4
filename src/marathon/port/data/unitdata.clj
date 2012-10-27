(ns marathon.port.data.unitdata)

'unitdata
'Need to separate an entity's position from its state...
'Currently, the location is equal to the entity's spatial location.
'Some locations and states have the same name (i.e. reset = reset).
'We need to inject some separation between these concepts.
'Inside of policy, we refere to nodes in the policy as states.
'The behavior interpreting the policy will apply state changes to the unit to create movement through
'the coordinate space.
Option Explicit

Public name As String
Public src As String
Public component As String

'Tom Change 12 July 2012 -> provides an index into the rotational policy.
Public policy As IRotationPolicy
Public policyStack As Collection

Public behavior As IUnitBehavior

Public StateData As GenericStateData
Public cycletime As Single
Public DateToReset As Date

'Tom Change 3 august
Public followoncode As String

'TOM Change 24 May 2011

'Current problem with how location is represented...
    'Physical location and policy location are different things.
    'There are physical locations that bear the same name as policy
    'locations.
    'Units thus have at least 2 coordinate systems when we talk about "location"....
        'spatial location, and their policy location.
    'The previous implementation had only a single locationname, which conflated these two items.
    
'TOM Change 19 May 2011 -> Added spatial location to unitdata, to denote unit's location in space.
Public LocationName As String

'TOM Change 19 May 2011 -> Added policy location to unitdata, to denote unit's location in policy.
Public PositionPolicy As String
'Public SpatialLocation As String

Public location As Long
Public CurrentCycle As TimeStep_CycleRecord
Public cycles As Collection
Public DeployTimeRemaining As Single
Public OITitle As String
Public index As Long
Public dwellTimeWhenDeployed As Single
Public extendedBOG As Boolean 'This is vestigial
Public spawnTime As Single
Public LocationHistory As Collection
Public moved As Boolean

'Decouple
'Public parent As TimeStep_ManagerOfSupply

Implements ISerializable
Implements IVolatile


Private Sub Class_Initialize()

Set StateData = New GenericStateData
Set cycles = New Collection
Set CurrentCycle = New TimeStep_CycleRecord
Set LocationHistory = New Collection
Set policyStack = New Collection

spawnTime = -1
End Sub
Public Sub InitCycles(t As Single, Optional ghost As Boolean)

Set CurrentCycle = CurrentCycle.NewCycle(0, policy.maxbog, policy.maxdwell, policy.cyclelength, policy.MaxMOB, ghost, policy.bogbudget)
With CurrentCycle
    .UICname = name
    .policyname = policy.name
    .tstart = t
    .src = src
    .component = component
End With

End Sub

'Account for a name change in the current cycle
'TOM Change 2 Sep 2011 -> also update the cyclerecord state to account for changed expected bog/dwell/mob
Public Sub ChangeCycle(t As Single)

With CurrentCycle
    .policyname = .policyname & "->" & policy.name
    .Traversals.add t & "_Policy Change to " & policy.name
End With
    
End Sub
'TOM Change 24 April 2012 -> When bogbudget is increased, we take into account the unit's bog history
'as part of the projection.  Namely, we reduce! the bogbudget correspondingly.  Negative bogbudgets are
'zeroed, which correctly prevents the unit from bogging any more until reset.
'TOM Change 20 April 2012 -> Incoporated BOGBudget into this guy, so unit's can now grow their bog budget
'as composite policies change over time.
'TOM Change 2 Sep 2011
Public Sub ModifyCycle(plcy As IRotationPolicy)

With CurrentCycle
    'update the record to account for this....when we record the cycle as completed, we want to use expected
    'values from the new policy.
    .modify plcy.maxbog, plcy.maxdwell, plcy.cyclelength, plcy.MaxMOB, _
            maxFloat(plcy.bogbudget - bog, 0)  'Note the reduction in bogbudget!
                                               'If unit has already bogged over budget, budget is zeroed!
End With

End Sub
Public Sub AddBOG(bogtime As Single)
CurrentCycle.bog = CurrentCycle.bog + bogtime

'If CurrentCycle.BOG > CurrentCycle.BOGExpected Then Err.Raise 101, , "Added too much bog somewhere"
CurrentCycle.bogbudget = CurrentCycle.bogbudget - bogtime
addDuration bogtime
End Sub
Public Sub AddDwell(dwelltime As Single, Optional available As Boolean)
If available Then CurrentCycle.availableTime = CurrentCycle.availableTime + dwelltime
CurrentCycle.dwell = CurrentCycle.dwell + dwelltime
addDuration dwelltime
End Sub
Public Sub addMOB(MOBtime As Single)
CurrentCycle.mob = CurrentCycle.mob + MOBtime
addDuration MOBtime
End Sub
Public Sub addDuration(dt As Single)
CurrentCycle.duration = CurrentCycle.duration + dt
End Sub
'Note, this will retain a good deal of data...we're keeping track of the unit's arforgen histories..
Public Sub RecordCycle(day As Single, Optional ghost As Boolean)
cycles.add CurrentCycle
Set CurrentCycle = CurrentCycle.NewCycle(day, policy.maxbog, policy.maxdwell, policy.cyclelength, policy.MaxMOB, ghost)
End Sub
'.ChangeState "Dwelling", timeremaining
Public Sub ChangeState(newstate As String, deltat As Single, Optional duration As Single, Optional context As timestep_SimContext)
'TODO FIX THIS, commented out to get compiling!
'Call behavior.ChangeState(Me, newstate, deltat, duration, , context)
End Sub
'TOM Change 27 MAr 2011 -> aren't really using day anymore, but it's in the parameter for now.
Public Function update(deltat As Single, Optional context As timestep_SimContext) As TimeStep_UnitData
'TODO FIX THIS, commented out to get compiling!
'Set update = behavior.update(deltat, Me, context)
End Function

'These both need to get yanked, as they're not specific to the uic...

'TOM change 20 April 2012 - > Note, we were using cycletime here, which is the cycletime associated
'with the unit state, i.e the empirical cycle time.  Since we've got a separation between the cycle
'length experienced by the unit, and the nominal policy length that unit is operating under (i.e.
'it changed rotational policies and is currently under a different policy), we need to change the
'deployment criteria from the empirical or experienced cycletime (unitdata.cycletime), to the notion
'of cycletime relative to active rotational policy, which is kept in currentcycle.duration.
Public Function CanDeploy(Optional spawning As Boolean) As Boolean


'With policy
'    'TOM Change 20 April 2012
'    CanDeploy = (followoncode <> vbNullString) Or _
'            (cycletime >= .StartDeployable And cycletime < .StopDeployable)
''    CanDeploy = (followoncode <> vbNullString) Or _
''            (CurrentCycle.duration >= .StartDeployable And CurrentCycle.duration < .StopDeployable)
'
'End With
'TOM Change 24 april -> added additional criteria to ensure valid deployability.
CanDeploy = validDeployer(spawning)

End Function
Public Function validDeployer(Optional spawning As Boolean) As Boolean
If spawning Then
    validDeployer = policy.Deployable(PositionPolicy)
Else
    validDeployer = bogRemains() And Not isDeployed() 'bogbudget > 0
    
    If validDeployer Then
        validDeployer = canFollowOn() Or inDeployableWindow()
    End If
End If

End Function
Public Function isDeployed() As Boolean
Dim state As String
Select Case StateData.CurrentState
    Case Bogging, Overlapping
        isDeployed = True
    Case Else
        isDeployed = False
End Select
End Function
Public Function bogRemains() As Boolean
bogRemains = CurrentCycle.bogbudget > 0
End Function
Public Function canFollowOn() As Boolean
canFollowOn = followoncode <> vbNullString
End Function
'note this identical to isDeployable in policy...
Public Function inDeployableWindow() As Boolean
inDeployableWindow = cycletime >= policy.startdeployable And cycletime < policy.stopdeployable
End Function
Public Property Get bog() As Single
bog = CurrentCycle.bog
End Property
Public Property Get dwell() As Single
dwell = CurrentCycle.dwell
End Property
Public Property Get BDR() As Single
BDR = CurrentCycle.BDR
End Property
Public Sub changePolicy(newpolicy As IRotationPolicy, Optional context As timestep_SimContext)
If Not (policy Is Nothing) Then policyStack.add newpolicy

If policyStack.count = 0 Then Err.Raise 101, , "no policy on stack!"
'TOM Change 21 July -> account for passage of time between updates!
If newpolicy.name <> policy.name Then
'    If name = "32_BCT_AC" Then
'        Debug.Print "phenomenon"
'    End If
    
    'TOM Change 25 July 2011
    'Decoupled
    ChangeState "PolicyChange", (SimLib.getTime(context) - SimLib.lastupdate(name, context)), , context
    'ChangeState "PolicyChange", 0
End If

End Sub

Public Function getStats() As String
getStats = "Policy:" & policy.AtomicName & " Cycletime: " & cycletime
End Function

'force the unit to broadcast a unitmoved event if it's the first time it moved.
'Note, we need to ensure that units are cleaned up at the end of day....using end of day
'logic, specifically, set moved = false for every unit that moved.

'TODO -> context is currently required, changed it back to optional after decoupling
'is complete.
Public Sub changeLocation(newlocation As String, context As timestep_SimContext)
If newlocation <> LocationName Then
    If Not moved Then
        moved = True
        'Decoupled
        SimLib.triggerEvent UnitMoved, name, newlocation, name & " started moving from " & _
                    LocationName & " to " & newlocation & " on day " & SimLib.getTime(context), Me, context
    Else
        'Decoupled
        SimLib.triggerEvent UnitMoved, name, newlocation, name & " moved from " & _
                        LocationName & " to " & newlocation & " on day " & SimLib.getTime(context), Me, context
    End If
    'update the location
    LocationName = newlocation
End If
End Sub
Public Sub ChangePolicyPosition(newposition As String)
Err.Raise 101, , "Not Implemented!"
End Sub
'TODO -> remove this.
Public Function hasParent() As Boolean
'Decoupled
hasParent = False 'Not (parent Is Nothing)
End Function


Private Sub Class_Terminate()

Set policy = Nothing
Set policyStack = Nothing

Set behavior = Nothing
Set StateData = Nothing
Set CurrentCycle = Nothing
Set cycles = Nothing
Set LocationHistory = Nothing
'decoupled
'Set parent = Nothing
End Sub

Private Function ISerializable_asString() As String
ISerializable_asString = DictionaryToJSON(newdict("class", "unitdata", "name", name, _
                                                  "SRC", src, "component", component, _
                                                  "followoncode", followoncode))
End Function

Private Sub ISerializable_fromDictionary(indict As Scripting.IDictionary)

End Sub

Private Sub ISerializable_fromFile(path As String)

End Sub

Private Sub ISerializable_FromString(code As String)

End Sub

Private Sub ISerializable_toFile(path As String)

End Sub

Private Sub IVolatile_Reset()

End Sub

Private Sub IVolatile_Terminate()
policy.subscribers.Remove name
Class_Terminate
End Sub


