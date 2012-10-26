(ns marathon.port.data.managerofupdates)

Option Explicit
'All the update manager does is listen for update request traffic.
'It records the time of the update request, who made the request, and the future update.
'It's really just a nice way to decouple and centralize the updating information in the sim.
'This represents a shift from just requesting a time to update the entire system, to a more
'localized method of updating individual pieces of the system as needed.
'It also provides a big hook for our observers....and for debugging purposes.
'The simulation cannot progress without updates, else every bit of state remains static.
'When we go to sample the state, we see no changes without updates.

Public name As String
'Decouple
Public parent As TimeStep_Engine

'TOM Change 7 Jun 2011 -> added policy as a valid update source.
'Currently 3 types of update sources
Public Enum UpdateType
    supply
    demand
    policy
End Enum

Public updates As Dictionary
Public lastupdate As Dictionary
Private localupdates As Dictionary
Private ptr As Dictionary
Private updpacket As TimeStep_UpdatePacket
Private packet As GenericPacket
Private msg As String
Implements ITriggerable
Implements IVolatile

(def default-updates {:supply {} :demand {} :policy {}})
(defrecord updatemanager [name updates lastupdate])
(def empty-updatemanager (updatemanager. "UpdateManager" default-updates {}))
(defn get-updates [mgr utype t]
  (get-in mgr [utype t]))

Private Sub Class_Initialize()
Set updates = New Dictionary
Set ptr = New Dictionary
updates.add supply, ptr

Set ptr = New Dictionary
updates.add demand, ptr

Set ptr = New Dictionary
updates.add policy, ptr


Set lastupdate = New Dictionary
name = "UpdateManager"
Set packet = New GenericPacket
End Sub
'TODO this is really just connecting an event listener for specific events.  I think we can simplify.
Public Sub listenUpdates(updatesource As TimeStep_ManagerOfEvents)

With updatesource
    .AddListener Me.name, Me, TimeStep_Msg.supplyUpdate
    .AddListener Me.name, Me, TimeStep_Msg.demandupdate
End With

End Sub
'return a list of all requested updates...
Public Function getUpdates(UpType As UpdateType, Optional t As Single, Optional ctx As timestep_SimContext) As Dictionary
Set ptr = updates(UpType)
'Decouple
'If t = 0 Then t = parent.CurrentTime
If t = 0 Then t = SimLib.getTime(ctx)


If ptr.exists(t) Then
    Set getUpdates = ptr(t)
Else
    Set getUpdates = New Dictionary
    ptr.add t, getUpdates
End If

End Function

Public Sub requestUpdate(tupdate As Single, requestedby As String, requestType As UpdateType, Optional trequest As Single, Optional ctx As timestep_SimContext)

(defn request-update [mgr requestor request trequest])
(defn update-packet [tupdate requestedby requesttype trequest]
  {:[tupdate requestedby requesttype trequest]
(defn add-update [mgr ]
  
  
  
'Decouple
'If trequest = 0 Then trequest = parent.CurrentTime
If trequest = 0 Then trequest = SimLib.getTime(ctx)
Set updpacket = New TimeStep_UpdatePacket
updpacket.init tupdate, requestedby, requestType, trequest
addUpdate updpacket
msg = "Update Requested by " & requestedby & " on day " & tupdate
'Decouple
'parent.trigger UpdateRequest, requestedby, name, msg
SimLib.triggerEvent UpdateRequest, requestedby, name, msg, , ctx
End Sub

Public Sub addUpdate(packet As TimeStep_UpdatePacket)
Set localupdates = getUpdates(packet.requestType, packet.tupdate)
'TOM note -> got a duplication error earlier, going to bypass it for now, may need to
'verify.  I think it's just units rapidly updating.

If Not localupdates.exists(packet.requestedby) Then
    localupdates.add packet.requestedby, packet
Else
    Set localupdates(packet.requestedby) = packet
End If

End Sub

Public Sub recordUpdate(updated As String, Optional t As Single, Optional ctx As timestep_SimContext)
Dim tlast As Single
'Decouple
'If t = 0 Then t = parent.CurrentTime
If t = 0 Then t = SimLib.getTime(ctx)

If lastupdate.exists(updated) Then
    tlast = lastupdate(updated)
    If t > tlast Then
        lastupdate(updated) = t
    ElseIf t < tlast Then
        Err.Raise 101, , "trying to update from the past...."
    End If
Else
    lastupdate.add updated, t
End If

End Sub

Private Sub Class_Terminate()
Set updates = Nothing
Set ptr = Nothing
Set lastupdate = Nothing

Set parent = Nothing ' As TimeStep_Engine

Set localupdates = Nothing 'As Dictionary
Set ptr = Nothing '  As Dictionary
Set updpacket = Nothing ' As TimeStep_UpdatePacket
Set packet = Nothing '  GenericPacket

End Sub

'TOM Change 30 june 2011
'Private Sub ITriggerable_trigger(msgid As Long, Optional data As Scripting.IDictionary)
Private Sub ITriggerable_trigger(msgid As Long, Optional data As GenericPacket)
'TOM NOTE 29 Mar 2011 -> CAREFUL when using OR statements in conditions, particularly selects,
'if it's a number or a string, it'll try to infer a binary OR on the underlying bits, NOT giving
'the intended result.
Set packet = data
'TOM Change 1 July 2011 -> No need for dictionary.....
If msgid = TimeStep_Msg.supplyUpdate Or msgid = TimeStep_Msg.demandupdate Then
        recordUpdate packet.entityTo, packet.t  'Packet.data.Item("Updated"), Packet.t
End If


End Sub

Private Sub IVolatile_Reset()
Set updates = Nothing
Set updates = New Dictionary

Set ptr = Nothing
Set ptr = New Dictionary
updates.add supply, ptr

Set ptr = New Dictionary
updates.add demand, ptr

Set ptr = New Dictionary
updates.add policy, ptr

Set lastupdate = Nothing
Set lastupdate = New Dictionary

Set ptr = Nothing


End Sub

Private Sub IVolatile_Terminate()
Class_Terminate
End Sub
