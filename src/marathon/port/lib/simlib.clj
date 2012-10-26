(ns marathon.port.simlib)

'Provides a wrapper for a several functions that interact with a generic simulation
'context.  The simulation context is a TimeStep_SimContext object (Currently), which
'wraps several components vital to any discrete event simulation:
'   The update manager:
'     An object that tracks requests for updates (akin to wake events for threads or processes).
'   The scheduler:
'     An object that tracks and schedules time events (i.e. an agenda, or abstract clock).
'   The manager of events:
'     An object that propogates event information via an Observable/Observer design pattern.
'     Maintains event registrations, and allows clients to notify observers of certain events.

Option Explicit
Sub niltest()
Dim blah As Collection
pprint nil(blah)
pprint nil(list(1, 2, 3))
pprint nil(newdict(1, 2))
pprint nil("A")

End Sub
Public Function exists(ByRef inval) As Boolean
exists = Not (inval Is Nothing)
End Function
Sub existstest()
Dim blah As Collection
pprint exists(blah)
pprint exists(list(1, 2, 3))
End Sub
Public Function nil(Optional ByRef inval) As Boolean

If IsMissing(inval) Then
    nil = True
Else
    Select Case vartype(inval)
        Case VbVarType.vbEmpty, VbVarType.vbNull
            nil = True
        Case VbVarType.vbObject
            nil = inval Is Nothing
        Case Else
            nil = False
    End Select
End If

End Function

'Pop the next time event off of the simulation context.  Side effecting.
Public Function advanceTime(ctx As timestep_SimContext) As Single
advanceTime = ctx.scheduler.advanceTime
End Function

'get the last, or upper bound on the final time event in the context.
Public Function getFinalTime(ctx As timestep_SimContext) As Single
getFinalTime = ctx.scheduler.tfinal
End Function

'get the next scheduled time event in the simulation context, past the
'current time of the simulation context.
Public Function getNextTime(ctx As timestep_SimContext) As Single
getNextTime = ctx.scheduler.NextTime
End Function

'Add a single time event to the simulation context.
Public Sub addTime(t As Single, ctx As timestep_SimContext)
ctx.scheduler.addTime t
End Sub

'Add multiple time events to the simulation context.
Public Sub addTimes(times As Collection, ctx As timestep_SimContext)
Dim t
For Each t In times
    addTime CSng(t), ctx
Next t
End Sub

'Get the current time of the simulation context.
Public Function getTime(Optional ctx As timestep_SimContext) As Single
If exists(ctx) Then getTime = ctx.CurrentTime Else getTime = 0
End Function

'Set the final time of the simulation context.  Acts as a filter
'to scope the time horizon of the context.
Public Sub setFinalTime(t As Single, ctx As timestep_SimContext)
ctx.scheduler.tfinal = t
End Sub

'Consult the context to determine if there are, in terms of scheduled
'time events and a possible final time, any time events remaining.
Public Function hasTimeRemaining(ctx As timestep_SimContext) As Boolean
hasTimeRemaining = ctx.scheduler.StillTime
End Function

'Triggers a generic event on the context's event thread.
Public Sub triggerEvent(eventtype As TimeStep_Msg, entityFrom As String, entityTo As String, msg As String, _
                            Optional data As Variant, Optional context As timestep_SimContext)
If exists(context) Then _
    context.events.dispatch getTime(context), eventtype, entityFrom, entityTo, msg, , data
End Sub

'interface for adding wake times to the simulation.
'should change this to be more generic....we don't need specific update types.
'If an entity wants an update, as long as the entity has a unique ID, it should be able to....
'Thus, we can organize event routing by entityID.
'Requesting an update, as a demand update, is equivalent to a demand-managing entity ID 02 requesting an
'update...
Public Sub requestUpdate(tupdate As Single, requestedby As String, _
                         requestType As UpdateType, Optional trequest As Single, _
                         Optional context As timestep_SimContext)

Dim tnow As Single
If trequest <> 0 Then
    tnow = trequest
Else
    tnow = context.CurrentTime
End If

If exists(context) Then
    context.scheduler.addTime tupdate
    context.updater.requestUpdate tupdate, requestedby, requestType, tnow
End If

End Sub

'Helper function....will be deprecated.
'Simply sets up a new sim context.  Pass in existing args if you'd like (useful for repetetive simulations.)
'For instance, we probably don't want to keep creating event contexts over and over again.
Public Function makeContext(Optional scheduler As TimeStep_ManagerOfTime, _
                                Optional updater As TimeStep_ManagerOfUpdates, _
                                    Optional events As TimeStep_ManagerOfEvents) As timestep_SimContext

Set makeContext = New timestep_SimContext
With makeContext
    Set .scheduler = createScheduler(scheduler)
    Set .events = createEvents(events)
    Set .updater = createUpdater(.events, updater)
End With

'hook up the update listener to the event context

End Function

'Creates a scheduler (a time manager).
Function createScheduler(Optional x As TimeStep_ManagerOfTime) As TimeStep_ManagerOfTime
If Not exists(x) Then Set x = New TimeStep_ManagerOfTime
Set createScheduler = x
End Function

'Creates an event manager with built-in funcitonality.  This might change in the future.
Function createEvents(Optional x As TimeStep_ManagerOfEvents) As TimeStep_ManagerOfEvents
If Not exists(x) Then Set x = New TimeStep_ManagerOfEvents
Set createEvents = x
End Function

'Creates an update manager.
Function createUpdater(evtsource As TimeStep_ManagerOfEvents, Optional x As TimeStep_ManagerOfUpdates) As TimeStep_ManagerOfUpdates
If Not exists(x) Then Set x = New TimeStep_ManagerOfUpdates
x.listenUpdates evtsource
Set createUpdater = x
End Function

'Fetch the last time the entity was updated.
Public Function lastupdate(entityname As String, context As timestep_SimContext) As Single
'Decoupled
lastupdate = context.updater.lastupdate(entityname)
End Function

'Returns the list of updates, by type, registered for time t.
Public Function getUpdates(update As UpdateType, t As Single, ctx As timestep_SimContext)
Set getUpdates = ctx.updater.getUpdates(update, t)
End Function

'Returns the generic state of the simulation.
'Allows mutation.
Public Function getState(ctx As timestep_SimContext) As Dictionary
Set getState = ctx.state
End Function

'it might be nice to recast this in terms of thread/sleep, thread/wake
'rather than update...
Public Sub threadSleep()

End Sub

Public Function makeDebugContext() As timestep_SimContext
Set makeDebugContext = makeContext()
makeDebugContext.AddListener "Debugger", New TimeStep_ObserverSimpleLog, newdict(0, True)

End Function
