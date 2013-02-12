(ns marathon.port.data.policycomposite)

Option Explicit

Public name As String
Public subscribers As Dictionary

Public ActivePolicy As TimeStep_Policy
Public activePeriod As String

Public policies As Dictionary

Implements IRotationPolicy
Implements ISerializable

Private Sub Class_Initialize()

Set ActivePolicy = New TimeStep_Policy
ActivePolicy.name = "Uninitialized"
Set policies = New Dictionary
Set subscribers = New Dictionary
End Sub

'TOM Removed 12 July 2012

'units subscribe to policies, so when we change, we can notify only the affected units.
Public Sub Subscribe(unit As TimeStep_UnitData)
If Not subscribers.exists(unit.name) Then
    subscribers.add unit.name, unit
End If

End Sub
'''when a period change occurs, we check to see if there is a policy alotted to the period.
'''If so, we switch to it.
Public Sub periodChange(newperiod As String, Optional t As Single)
Static newpolicy As IRotationPolicy

activePeriod = newperiod
If policies.exists(newperiod) Then
    Set newpolicy = policies(newperiod)
    Set ActivePolicy = newpolicy
    
'    changePolicy newpolicy
End If

End Sub
'''changepolicy is slightly different for the composite policy.
'''We want all subscribers to change to the new policy, then to point themselves back to the composite.
Public Sub changePolicy(newpolicy As IRotationPolicy)

Set ActivePolicy = newpolicy
'Dim unitname
'Static unit As TimeStep_UnitData
'
'If (newpolicy.name <> activePolicy.name) Then
'    For Each unitname In subscribers
'        Set unit = subscribers(unitname)
'        unit.changePolicy newpolicy 'tries to change policies...
'        If unit.policy.name = newpolicy.name Then
'            Set unit.policy = Me 'points the unit back toward composite policy.
'        Else 'ensure that this policy gets switched to if the unit delayed.
'            unit.policyStack.Remove 1 'remove the new policy...
'            Set unit.policy = activePolicy
'            unit.policyStack.add Me
'        End If
'    Next unitname
'    Set activePolicy = newpolicy
'End If

End Sub

Public Sub addPolicy(policy As IRotationPolicy, period As String)

If Not policies.exists(policy.name) Then
    policies.add period, policy
End If

If policies.count = 1 Then Set ActivePolicy = policy

End Sub

Private Sub Class_Terminate()
Set ActivePolicy = Nothing
Set subscribers = Nothing
Set policies = Nothing
End Sub

'Note -> the implementations are essentially read-only, i.e. we don't actually allow the changing
'of any policies in the composite policy.  They are effectively immutable in this container.
'Some properties ARE mutable, specifically the composite policy's name and subscribers.
'This provides a layer of separation between things associated with the composite policy, and
'things associated with the atomic policies.
    'I.e., subscribers to atomic policies are mutually excluded from being subscribers to composite
    'policies.
    'Composite policies are on the same level of atomic policies, but they handle policy changes and
    'other events a bit differently.....

'**********************IRotationPolicy Implementations*******************************************
'************************************************************************************************
Private Function IRotationPolicy_AtomicName() As String
IRotationPolicy_AtomicName = ActivePolicy.name
End Function

Private Property Get IRotationPolicy_BOGBudget() As Long
IRotationPolicy_BOGBudget = ActivePolicy.bogbudget
End Property

Private Function IRotationPolicy_getActivePolicy() As IRotationPolicy
Set IRotationPolicy_getActivePolicy = ActivePolicy
End Function

'TOM Change 12 July 2012
'Turned this into a tree-search.  We're basically traversing an n-way tree from the root,
'using the period as a key, looking for the lowest leaf the corresponds to the period.  This amounts to
'determining the intersection of the root's period, and some atomic policy (leaf) defined over the period in a
'subtree.  WE can turn this into a more flexible search, but I'd like for it to trip errors.  It's not a real
'tree search, which fits the semantics of the composite policy.
Private Function IRotationPolicy_getPolicy(period As String) As IRotationPolicy
Dim childpolicy As IRotationPolicy

If policies.exists(period) Then
    Set childpolicy = policies(period)
    While childpolicy.getPolicyType <> atomic
        If childpolicy.getPolicy(period) Is Nothing Then
            Err.Raise 101, , "Child policy is undefined for period " & period
        Else
            Set childpolicy = childpolicy.getPolicy(period)
        End If
    Wend
Else
    Err.Raise 101, , "Child policy is undefined for period " & period
End If

Set IRotationPolicy_getPolicy = childpolicy
End Function

'TOM Removed 12 July 2012
Private Sub IRotationPolicy_OnPeriodChange(period As String)
periodChange period
End Sub

Private Property Let IRotationPolicy_name(ByVal RHS As String)
name = RHS
End Property

Private Property Get IRotationPolicy_name() As String
IRotationPolicy_name = name
End Property

Private Function IRotationPolicy_nextposition(position As String) As String
IRotationPolicy_nextposition = ActivePolicy.nextposition(position)
End Function

Private Property Let IRotationPolicy_overlap(ByVal RHS As Long)
End Property

Private Property Get IRotationPolicy_overlap() As Long
IRotationPolicy_overlap = ActivePolicy.overlap
End Property

Private Property Set IRotationPolicy_PositionGraph(ByVal RHS As GenericGraph)
End Property

Private Property Get IRotationPolicy_PositionGraph() As GenericGraph
Set IRotationPolicy_PositionGraph = ActivePolicy.PositionGraph
End Property

Private Function IRotationPolicy_previousPosition(position As String) As String
IRotationPolicy_previousPosition = ActivePolicy.previousPosition(position)
End Function

Private Function IRotationPolicy_setDeployable(tstart As Single, tfinal As Single, Optional policy As IRotationPolicy) As IRotationPolicy
Set IRotationPolicy_setDeployable = ActivePolicy
End Function

Private Function IRotationPolicy_setDeployableStart(cycletime As Single, Optional policy As IRotationPolicy) As IRotationPolicy
Set IRotationPolicy_setDeployableStart = ActivePolicy
End Function

Private Function IRotationPolicy_setDeployableStop(cycletime As Single, Optional policy As IRotationPolicy) As IRotationPolicy
Set IRotationPolicy_setDeployableStop = ActivePolicy
End Function

Private Property Let IRotationPolicy_StartDeployable(ByVal RHS As Single)
End Property

Private Property Get IRotationPolicy_StartDeployable() As Single
IRotationPolicy_StartDeployable = ActivePolicy.startdeployable
End Property

Private Property Let IRotationPolicy_StartIndex(ByVal RHS As Long)
End Property

Private Property Get IRotationPolicy_StartIndex() As Long
IRotationPolicy_StartIndex = ActivePolicy.StartIndex
End Property

Private Property Let IRotationPolicy_StartState(ByVal RHS As String)
End Property

Private Property Get IRotationPolicy_StartState() As String
IRotationPolicy_StartState = ActivePolicy.startstate
End Property

Private Property Let IRotationPolicy_StopDeployable(ByVal RHS As Single)
End Property

Private Property Get IRotationPolicy_StopDeployable() As Single
IRotationPolicy_StopDeployable = ActivePolicy.stopdeployable
End Property

Private Sub IRotationPolicy_Subscribe(unit As TimeStep_UnitData)
Subscribe unit
End Sub

Private Property Set IRotationPolicy_subscribers(ByVal RHS As Scripting.IDictionary)
Set subscribers = RHS
End Property

Private Property Get IRotationPolicy_subscribers() As Scripting.IDictionary
Set IRotationPolicy_subscribers = subscribers
End Property

Private Function IRotationPolicy_TransferTime(startPosition As String, endPosition As String) As Single
IRotationPolicy_TransferTime = ActivePolicy.TransferTime(startPosition, endPosition)
End Function

Private Sub IRotationPolicy_AddPosition(PosName As String, PosState As String, ParamArray MoreNodes() As Variant)
End Sub

Private Sub IRotationPolicy_AddRoute(Start As String, destination As String, TransferTime As Single)
End Sub

'Tom change 12 July 2012
''Private Sub IRotationPolicy_ChangePolicy(newpolicy As IRotationPolicy, Optional SpecificUnit As String)
''changePolicy newpolicy
''End Sub

'This may cause errors.....just don't use clone... ?
Private Function IRotationPolicy_clone() As IRotationPolicy
Set IRotationPolicy_clone = Me
End Function

Private Property Let IRotationPolicy_cyclelength(ByVal RHS As Long)
End Property

Private Property Get IRotationPolicy_cyclelength() As Long
IRotationPolicy_cyclelength = ActivePolicy.cyclelength
End Property

Private Function IRotationPolicy_Deployable(position As String) As Boolean
IRotationPolicy_Deployable = ActivePolicy.Deployable(position)
End Function

Private Property Let IRotationPolicy_EndIndex(ByVal RHS As Long)
End Property

Private Property Get IRotationPolicy_EndIndex() As Long
IRotationPolicy_EndIndex = ActivePolicy.EndIndex
End Property

Private Property Let IRotationPolicy_EndState(ByVal RHS As String)
End Property

Private Property Get IRotationPolicy_EndState() As String
IRotationPolicy_EndState = ActivePolicy.endstate
End Property

Private Function IRotationPolicy_GetCycleTime(position As String) As Long
IRotationPolicy_GetCycleTime = ActivePolicy.GetCycleTime(position)
End Function

Private Function IRotationPolicy_getPolicyType() As PolicyType
IRotationPolicy_getPolicyType = composite
End Function

Private Function IRotationPolicy_getPosition(cycletime As Single) As String
IRotationPolicy_getPosition = ActivePolicy.getPosition(cycletime)
End Function

Private Function IRotationPolicy_getState(ByVal position As String) As Variant
IRotationPolicy_getState = ActivePolicy.getState(position)
End Function

Private Function IRotationPolicy_insertModifier(cycletime As Single, policy As IRotationPolicy, modifier As String) As IRotationPolicy
Set IRotationPolicy_insertModifier = Me
End Function

Private Function IRotationPolicy_isDeployable(cycletime As Single) As Boolean
IRotationPolicy_isDeployable = ActivePolicy.isDeployable(cycletime)
End Function

Private Function IRotationPolicy_isDwell(position As String) As Boolean
IRotationPolicy_isDwell = ActivePolicy.isDwell(position)
End Function

Private Property Let IRotationPolicy_MaxBOG(ByVal RHS As Long)
End Property

Private Property Get IRotationPolicy_MaxBOG() As Long
IRotationPolicy_MaxBOG = ActivePolicy.maxbog
End Property

Private Property Let IRotationPolicy_MaxDwell(ByVal RHS As Long)
End Property

Private Property Get IRotationPolicy_MaxDwell() As Long
IRotationPolicy_MaxDwell = ActivePolicy.maxdwell
End Property

Private Property Let IRotationPolicy_MaxMOB(ByVal RHS As Long)
End Property

Private Property Get IRotationPolicy_MaxMOB() As Long
IRotationPolicy_MaxMOB = ActivePolicy.MaxMOB
End Property

Private Property Let IRotationPolicy_MinDwell(ByVal RHS As Long)
End Property

Private Property Get IRotationPolicy_MinDwell() As Long
IRotationPolicy_MinDwell = ActivePolicy.mindwell
End Property

Private Function ISerializable_asString() As String

Dim res As Dictionary
Dim per

Set res = New Dictionary
Dim p As TimeStep_Policy

For Each per In policies
    Set p = policies(per)
    'res.add CStr(per), pickle(p)
    res.add CStr(per), JSONtoDictionary(asSerial(p).asString)
Next per

ISerializable_asString = DictionaryToJSON(newdict("name", name, "policies", res))

End Function
Private Sub ISerializable_FromString(code As String)
Class_Initialize
Dim res As Dictionary
Dim per

Set res = New Dictionary
Dim p As TimeStep_Policy
Dim s As ISerializable

Set res = JSONtoDictionary(code)

Me.name = res("name")
Set res = res("policies")
For Each per In res
    Set p = New TimeStep_Policy
    Set s = p
    s.fromDictionary res(per)
    'Set p = unpickle(p, res(per))
    addPolicy p, CStr(per)
Next per

End Sub
Private Sub ISerializable_fromDictionary(indict As Scripting.IDictionary)
asSerial(Me).FromString (DictionaryToJSON(indict))
End Sub

Private Sub ISerializable_fromFile(path As String)
unpickleFrom Me, path
End Sub
Private Sub ISerializable_toFile(path As String)
pickleTo Me, path
End Sub
