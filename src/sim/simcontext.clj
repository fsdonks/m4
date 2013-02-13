;A simulation context is a general purpose structure for bearing discrete event
;simulation information, specifically a scheduler, an updater, event-routing/
;event registration, and the simulation state.

(ns sim.simcontext)

;A generic class for defining simulation contexts.
;We have an event stream, a time manager, and an update manager.
;This helps us factor out a lot of access parameters that were being passed implicitly
;by a parent/child relationship.  It also lets us establish mechanisms for rolling back
;time....and serializing the simulation state, or using different simulation contexts
;to coordinate simulations.  We collate stuff into a simulation context and just pass it around,
;Rather than having individual parameters

Public scheduler As TimeStep_ManagerOfTime
Public updater As TimeStep_ManagerOfUpdates
Public events As TimeStep_ManagerOfEvents
Public state As Dictionary 'Extra chunk of state associated with context.  Common enough in general simulations.

(defrecord simcontext [scheduler updater events state])

(defn current-time [ctx] 
  (
Public Function CurrentTime() As Single
    CurrentTime = scheduler.CurrentTime
End Function

Public Function CurrentQuarter() As Single
    CurrentQuarter = scheduler.quarter
End Function

'this may no longer be as useful since we've moved to local updating.
'keeping it around since it displays the time elapsed from last day.
Public Function Elapsed() As Single
    Elapsed = scheduler.Elapsed
End Function

Public Sub addTime(t As Single)
    scheduler.addTime t
End Sub

'Created a public interface for managing updates to the system.
Public Sub requestUpdate(tupdate As Single, requestedby As String, requestType As UpdateType, Optional trequest As Single)
addTime tupdate
updater.requestUpdate tupdate, requestedby, requestType, trequest
End Sub

'Top-Level method for adding listeners to the event stream
Public Sub AddListener(lname As String, listener As ITriggerable, subscriptions As Dictionary)
Dim subscr
For Each subscr In subscriptions
    events.AddListener lname, listener, CLng(subscr)
Next subscr

End Sub
'Trigger an event.
Public Sub trigger(eventtype As TimeStep_Msg, entityFrom As String, entityTo As String, msg As String, Optional logtarget As LogTargets, Optional alternatetype As String, Optional data As Variant)

events.dispatch CurrentTime, eventtype, entityFrom, entityTo, msg, logtarget, data

End Sub


Private Sub Class_Initialize()
Set state = New Dictionary
End Sub

Private Sub Class_Terminate()
Set scheduler = Nothing
Set updater = Nothing
Set events = Nothing
Set state = Nothing
End Sub
