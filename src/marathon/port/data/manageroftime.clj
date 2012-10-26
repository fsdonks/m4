(ns marathon.port.data.manageroftime)

'As the name implies, this object is responsible for managing time in the simulation.  The current
'approach to time management caches a series of future updates into a dictionary.  These are basically
'eventful days.  As the simulation proceeds, events will be requested to happen on specific days, many
'times in the future.  The time manager serves as a centralized clearing-house for these requests, and
'avoids duplication of update requests.  The net effect is that the "system" updates itself only on
'unique days that have been scheduled.  The time manager also provides a central source for determining
'the current time, and for determining a final time.
Option Explicit
Public name As String
'Decouple
Public parent As TimeStep_Engine
Public TimeHeap As Heap_SngInt
Public PlannedUpdates As Dictionary
Public singleUpdatesOnly As Boolean
Public CurrentTime As Single
Public PreviousTime As Single
Public updateCount As Long
Public tfinal As Single
Private qtr As Long 'renders current time into its quarter.
Private nextqtr As Single

(defrecord manageroftime [name timeheap plannedupdates 
                          singleupdatesonly currentime previoustime 
                          updateCount tfinal]) 
Private Sub Class_Initialize()

name = "SimulationClock"
singleUpdatesOnly = True
Set PlannedUpdates = New Dictionary
Set TimeHeap = New Heap_SngInt
tfinal = infinite
qtr = 0
nextqtr = 0


End Sub
(def empty-manageroftime (manageroftime. "SimulationClock" (sorted-map) {} 
                                         true 0 0 
                                         0 :inf))

(defn nextTime [tm] (first (keys (:timeheap tm))))
(defn quarter [tm] (/ (:currenttime tm) 90) + 1)
(defn elapsed [tm] (- (:currenttime tm) (:previoustime tm)))
(defn addTime [tm t]
  (let [{:keys [timeheap currenttime]} tm]
    (merge tm {:timeheap (assoc timeheap [t 
 
'Tom Change 17 April 2012
Public Function quarter() As Long
quarter = (CurrentTime \ 90) + 1
End Function
Public Function inf() As Single
inf = infinite
End Function
'Get next time from the clock!
Public Function NextTime() As Single

If TimeHeap.count > 0 Then
    NextTime = TimeHeap.maxkey
End If

End Function

Public Property Get Elapsed() As Single
Elapsed = CurrentTime - PreviousTime
End Property

Public Sub addTime(t As Single)


If tfinal = infinite Or t < tfinal Then
    If singleUpdatesOnly Then
        If Not HasTime(t) Then
            updateCount = updateCount + 1
            TimeHeap.add t, updateCount 'record the update time in the heap
            PlannedUpdates.add t, updateCount 'record the update time in the dictionary
        Else
            'do nothing
        End If
    Else
        updateCount = updateCount + 1
        TimeHeap.add t, updateCount 'record the update time in the heap
        If Not HasTime(t) Then
            PlannedUpdates.add t, updateCount 'record the update time in the dictionary
        End If
    End If
End If

End Sub
Private Function HasTime(t As Single)
HasTime = PlannedUpdates.exists(t)
End Function
Public Function advanceTime(Optional t As Single) As Single

'right now we do nothing the time parameter.
PreviousTime = CurrentTime
CurrentTime = NextTime 'updates the time, based on the next heaptime
Call TimeHeap.Remove 'ignore the result of remove, we don't care about the value, only the key ...

advanceTime = CurrentTime 'return the currenttime

End Function

Public Function StillTime() As Boolean

If TimeHeap.count > 0 Then
    If NextTime <= tfinal Then
        StillTime = True
    End If
End If

End Function

Private Sub Class_Terminate()
Set parent = Nothing
Set TimeHeap = Nothing
Set PlannedUpdates = Nothing
End Sub

Private Sub IVolatile_Reset()
Set parent = Nothing
Set TimeHeap = Nothing
Set PlannedUpdates = Nothing

Class_Initialize

CurrentTime = 0
PreviousTime = 0
updateCount = 0

End Sub

Private Sub IVolatile_Terminate()
Class_Terminate
End Sub
