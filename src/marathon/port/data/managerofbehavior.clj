(ns marathon.port.data.managerofbehavior)

(defrecord behaviorstore [name behaviors])

'Behavior Factory class ...
'Used for producing unit and demand behaviors ...
Option Explicit
Public name As String
Public parent As TimeStep_Engine
Public behaviors As Dictionary
Private tags As GenericTags
Private behptr As IUnitBehavior
Private basebehavior As IUnitBehavior

Implements IVolatile
Private Sub Class_Initialize()


Dim acbeh As TimeStep_UnitBehaviorAC
Dim rcbeh As TimeStep_UnitBehaviorRC
Dim ghostbeh As TimeStep_UnitBehaviorGhost

name = "Behaviors"
Set behaviors = New Dictionary
Set tags = New GenericTags

Set basebehavior = New TimeStep_UnitBehaviorBase

Set acbeh = New TimeStep_UnitBehaviorAC
acbeh.init basebehavior

Set rcbeh = New TimeStep_UnitBehaviorRC
rcbeh.init basebehavior

Set ghostbeh = New TimeStep_UnitBehaviorGhost

addUnitBehavior basebehavior, "BaseBehavior"
addUnitBehavior acbeh, "DefaultACBehavior"
addUnitBehavior rcbeh, "DefaultRCBehavior"
addUnitBehavior ghostbeh, "DefaultGhostBehavior"

End Sub
Public Sub initBehaviors(supply As TimeStep_ManagerOfSupply)

Dim beh
For Each beh In behaviors
    Set behptr = behaviors(beh)
    behptr.init supply, basebehavior
    'registerSupply supply, behaviors(beh)
Next beh
    
End Sub
Public Sub registerSupply(supply As TimeStep_ManagerOfSupply, behavior As IUnitBehavior)
'Set behavior.parent = supply
End Sub
Public Sub addACUnitBehavior(beh As TimeStep_UnitBehaviorAC, Optional tag As String, Optional extratags As Dictionary)
Dim tg
If Not behaviors.exists(beh.name) Then
    behaviors.add beh.name, beh
    If tag <> vbNullString Xor Not (extratags Is Nothing) Then associate beh.name, tag, extratags
Else
    Err.Raise 101, , "Behavior already exists"
End If

End Sub
Public Sub addRCUnitBehavior(beh As TimeStep_UnitBehaviorRC, Optional tag As String, Optional extratags As Dictionary)
Dim tg
If Not behaviors.exists(beh.name) Then
    behaviors.add beh.name, beh
    If tag <> vbNullString Xor Not (extratags Is Nothing) Then associate beh.name, tag, extratags
Else
    Err.Raise 101, , "Behavior already exists"
End If

End Sub
Public Sub addGhostUnitBehavior(beh As TimeStep_UnitBehaviorGhost, Optional tag As String, Optional extratags As Dictionary)
Dim tg
If Not behaviors.exists(beh.name) Then
    behaviors.add beh.name, beh
    If tag <> vbNullString Xor Not (extratags Is Nothing) Then associate beh.name, tag, extratags
Else
    Err.Raise 101, , "Behavior already exists"
End If

End Sub
Public Sub addUnitBehavior(beh As IUnitBehavior, Optional tag As String, Optional extratags As Dictionary)
Dim tg
'Set beh.parent = getSupply
If Not behaviors.exists(beh.name) Then
    behaviors.add beh.name, beh
    If tag <> vbNullString Xor Not (extratags Is Nothing) Then associate beh.name, tag, extratags
Else
    Err.Raise 101, , "Behavior already exists"
End If

End Sub
'we can categorize behaviors....just like events.
Public Sub associate(behaviorname As String, tag As String, Optional extratags As Dictionary)
tags.addTag tag, behaviorname
tags.multiTagDict behaviorname, extratags
End Sub
Public Function behaviorFromTag(tag As String) As IUnitBehavior
Dim beh As Dictionary
Set beh = tags.getSubjects(tag)

If beh.count = 1 Then
    Set behaviorFromTag = behaviors(beh.keys(0))
ElseIf beh.count = 0 Then
    Err.Raise 101, , "No tags associated with this behavior"
ElseIf beh.count > 1 Then
    Err.Raise 101, , "Multiple behaviors associated with this tag"
End If

End Function
Public Function exists(behname As String) As Boolean
exists = behaviors.exists(behname)
End Function
Private Function getACBehavior(behname As String) As TimeStep_UnitBehaviorAC
If exists(behname) Then
    Set getACBehavior = behaviors(behname)
Else
    Err.Raise 101, , "does not exist"
End If
End Function

Private Function getRCBehavior(behname As String) As TimeStep_UnitBehaviorRC
If exists(behname) Then
    Set getRCBehavior = behaviors(behname)
Else
    Err.Raise 101, , "does not exist"
End If
End Function

Private Function getGhostBehavior(behname As String) As TimeStep_UnitBehaviorGhost
If exists(behname) Then
    Set getGhostBehavior = behaviors(behname)
Else
    Err.Raise 101, , "does not exist"
End If
End Function
Public Function assignBehavior(unit As TimeStep_UnitData, Optional specificbehavior As String) As TimeStep_UnitData

With unit
    If specificbehavior = vbNullString Or specificbehavior = "Auto" Then
        Select Case .component
            Case Is = "AC"
                Set .behavior = defaultACBehavior
            Case Is = "RC"
                Set .behavior = defaultRCBehavior
            Case Is = "NG"
                Set .behavior = defaultRCBehavior
            Case Is = "Ghost"
                Set .behavior = defaultGhostBehavior
        End Select
    ElseIf exists(specificbehavior) Then
        Set .behavior = behaviors(specificbehavior)
    Else
        Err.Raise 101, , "Behavior does not exist in this codebase." & _
                                " Possibly not implemented or not spelled correctly."
    End If
End With

Set assignBehavior = unit

End Function
Public Function getSupply() As TimeStep_ManagerOfSupply
End Function

Public Function defaultACBehavior() As TimeStep_UnitBehaviorAC
Set defaultACBehavior = behaviors("DefaultACBehavior")
End Function
Public Function defaultRCBehavior() As TimeStep_UnitBehaviorRC
Set defaultRCBehavior = behaviors("DefaultRCBehavior")
End Function
Public Function defaultGhostBehavior() As TimeStep_UnitBehaviorGhost
Set defaultGhostBehavior = behaviors("DefaultGhostBehavior")
End Function

Private Sub Class_Terminate()
Set parent = Nothing
Set behaviors = Nothing
Set tags = Nothing
End Sub

Private Sub IVolatile_Reset()
'should be no need to do anything with this guy.
End Sub
Private Sub IVolatile_Terminate()
End Sub
