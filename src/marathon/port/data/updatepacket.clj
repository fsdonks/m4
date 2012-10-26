(ns marathon.port.data.updatepacket)

Option Explicit
Public tupdate As Single
Public requestedby As String
Public requestType As UpdateType
Public trequest As Single

Public Sub init(tupd As Single, reqBy As String, reqType As UpdateType, treq As Single)

tupdate = tupd
requestedby = reqBy
requestType = reqType
trequest = treq

End Sub

Public Function clone() As TimeStep_UpdatePacket
Set clone = New TimeStep_UpdatePacket
clone.init tupdate, requestedby, requestType, trequest
End Function

Public Function Elapsed(tnow As Single, Optional lastupdate As Single) As Single

If lastupdate = 0 Then lastupdate = trequest
Elapsed = tnow - lastupdate

End Function
