(ns marathon.port.data.unitbehaviorac)

'unitbehaviorac
'object do describe and control unit behaviors.
'NOTE -> This class is implicitly inheritable.  I kept down the number of public methods intentionally, to
'keep from
Option Explicit
Public name As String
'Decouple
'Public parent As TimeStep_ManagerOfSupply
Private basebehavior As IUnitBehavior
Implements IUnitBehavior

Private Sub Class_Initialize()
name = "DefaultACBehavior"
End Sub
Public Sub init(basebeh As IUnitBehavior)
Set basebehavior = basebeh
'Decouple
'Set parent = basebehavior.parent
End Sub

'''Currently, behavior in the base class is appropriate.  We delegate to it.
''Public Function update(tElapsed As Single, unit As TimeStep_UnitData) As TimeStep_UnitData
''Set update = basebehavior.update(tElapsed, unit)
''End Function
'''Currently, behavior in the base state is appropriate.  We delegate to it.
''Public Function ChangeState(unit As TimeStep_UnitData, tostate As String, deltat As Single, Optional duration As Single, _
''                                        Optional followingstate As String) As TimeStep_UnitData
''Set ChangeState = basebehavior.ChangeState(unit, tostate, deltat, duration, followingstate)
''End Function

Private Sub Class_Terminate()
'Set parent = Nothing
Set basebehavior = Nothing
End Sub

'********Default Implementations for UnitBehavior Interface.*******************************
'********                                                   *******************************
Private Sub IUnitBehavior_init(state As TimeStep_SimState, Optional basicbehavior As IUnitBehavior)
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


