(ns marathon.port.data.unitbehaviorrc)

'unitbehaviorRC
'This is a template for variations on unit behavior specific to the RC.
'Currently, there are no differences, except in the name.  However, we have the infrastructure
'to easily extend behavior by component now (just redirect which updates get sent to the base behavior,
'if any)

Option Explicit
Public name As String
'Decouple
'Public parent As TimeStep_ManagerOfSupply

Private basebehavior As IUnitBehavior
Implements IUnitBehavior

Private Sub Class_Initialize()
name = "DefaultRCBehavior"
'Set parent = Nothing
Set basebehavior = Nothing
End Sub
Public Sub init(basebeh As IUnitBehavior)
Set basebehavior = basebeh
'Decouple
'Set parent = basebehavior.parent
End Sub

''Currently, behavior in the base class is appropriate.  We delegate to it.
'Public Function update(tElapsed As Single, unit As TimeStep_UnitData) As TimeStep_UnitData
'Set update = basebehavior.update(tElapsed, unit)
'End Function
''Currently, behavior in the base state is appropriate.  We delegate to it.
'Public Function ChangeState(unit As TimeStep_UnitData, tostate As String, deltat As Single, Optional duration As Single, _
'                                        Optional followingstate As String) As TimeStep_UnitData
'Set ChangeState = basebehavior.ChangeState(unit, tostate, deltat, duration, followingstate)
'End Function



Private Sub Class_Terminate()
'Set parent = Nothing

Set basebehavior = Nothing ' As IUnitBehavior
End Sub

'********Default Implementations for UnitBehavior Interface.*******************************
'********                                                   *******************************
Private Sub IUnitBehavior_init(state As TimeStep_SimState, Optional basicbehavior As IUnitBehavior)
'Decouple
'Set parent = supply
Set basebehavior = basicbehavior
basebehavior.init state
End Sub
Private Function IUnitBehavior_ChangeState(unit As TimeStep_UnitData, tostate As String, deltat As Single, Optional duration As Single, Optional followingstate As String) As TimeStep_UnitData
Set IUnitBehavior_ChangeState = basebehavior.ChangeState(unit, tostate, deltat, duration, followingstate)
End Function
Private Property Let IUnitBehavior_name(ByVal RHS As String)
name = RHS
End Property
Private Property Get IUnitBehavior_name() As String
IUnitBehavior_name = name
End Property

Private Property Set IUnitBehavior_simstate(ByVal RHS As TimeStep_SimState)
Set basebehavior.simstate = RHS
End Property

Private Property Get IUnitBehavior_simstate() As TimeStep_SimState
Set IUnitBehavior_simstate = basebehavior.simstate
End Property

Private Function IUnitBehavior_update(tElapsed As Single, unit As TimeStep_UnitData) As TimeStep_UnitData
Set IUnitBehavior_update = basebehavior.update(tElapsed, unit)
End Function




