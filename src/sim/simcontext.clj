;A simulation context is a general purpose structure for bearing discrete event
;simulation information, specifically a scheduler, an updater, event-routing/
;event registration, and the simulation state.

;The simulation context defines the dependencies in the simulation, as well as 
;the pending instantaneous changes (event), and the notion of time or an 
;absolute ordering of the computation in the simulation.

;The scheduler is just a schedule value from sim.schedule.  Its purpose is to 
;provide a focus for ordering events.  In the obserer-based, mutation-driven 
;style of old, we would "trigger" events in functions, which would communicate
;the presence of an event along the event stream by broadcasting an
;instantaneous event.  In the newer functional styling, "triggering" an event 
;should amount to pushing an event onto the simulation context.  We can then
;handle instantaneous events by threading an event-handling function in between
;each of the main simulation process functions....thus allowing for an absolute
;ordering of events (even instantaneous events) as they are queued.

;Scheduling is typically used to "wake" entities within the simulation at 
;specified times, or to indicate that a set of simulation processing needs to 
;occur.  

;The updater allows us to encode localized updates or patches to entities, 
;and to keep track of the last "time" an entity was updated.  For temporal 
;simulations, this is valuable as it provides effecient, localized updating 
;of entities, and keeps the system consistent (i.e. prevents the application of
;old time-dependent updates with outdated notions of elapsed time).
;Much of the simulation domain requires time deltas to compute the next step 
;of the simulation, or to define an entity's behavior, so keeping track of 
;this is fairly useful.

;Event routing information encodes a communications network.  For every 
;simulation, there is a vocabulary used to communicate changes in the simulation
;or to trigger side-effects (rendering or logging for instance).
;The event stream describes a network of named events, and for each event, 
;an ordered set of subscribers that are "interested" in the event.
;We typically handle this with an observable/observer setup, which encodes an 
;event as an enumerated type, mapping every event to a list of oberservers who
;need to be notified when an event is observed.  We can simulate this setup 
;in a pure manner, by maintaining the same dependency map, and - rather than 
;broadcasting the event, fold the event and the simulation state through 
;the handlers in series.

(ns sim.simcontext
  (:require [sim [data :as sim] [agenda :as agenda]]
            [sim.pure [net :as simnet]]))

;IEventContext is a simple wrapper for things that have state and events. 
(defprotocol IEventContext 
  (get-state [ec]     "return the state of the context")
  (set-state [ec s]   "return a new context with state as s")
  (get-events [ec]    "return the IEventSeq of the context")
  (set-events [ec es] "return a new context with events as es"))

;Allow pairs of vectors, lists, and {:keys [events state]} to be seen as 
;event contexts.
(extend-protocol IEventContext
  clojure.lang.PersistentVector
  (get-state  [v] (first v))
  (set-state  [v s] [s (fnext v)]) 
  (get-events [v] (fnext v))
  (set-events [v es] [(first v) es])
  clojure.lang.PersistentList
  (get-state  [l] (first l))
  (set-state  [l s] (cons s (rest l)))
  (get-events [l] (fnext l))
  (set-events [l es] (list (first l) es)) 
  clojure.lang.PersistentArrayMap
  (get-state  [m] (:state m))
  (set-state  [m s] (assoc m :state s))
  (get-events [m] (:events m))
  (set-events [m es] (assoc m :events es)))

;Define an all-in-one record that supports operations for both IEventContext 
;and IEventSeq. 
;An eventcontext is simply a nice wrapper for some state, and an IEventSeq, 
;events.
(defrecord eventcontext [state events]
  IEventContext 
  (get-state [ec] state)
  (set-state [ec s] (eventcontext. s events))
  (get-events [ec] events)
  (set-events [ec es] (eventcontext. state es))
  IEventSeq
  (add-event [v e] (eventcontext. state (add-event events e)))
  (drop-event [v] (eventcontext. state (drop-event events)))
  (next-event [v] (next-event events))
  (pending-events [v] (pending-events events)))

(def emptycontext (->eventcontext nil []))

(defn next-type
  "Gets the type of the next event, based off of its :type field."
  [ec] (-> ec
         (get-events)
         (next-event)
         (event-type)))

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

(defrecord simcontext 
  [scheduler ;supported by agenda.  
   updater ;a weak agenda with some special state, tracks previous updates. 
   propogator  ;event propogation, represented by a propogation network. 
   state]) ;the state of the simulation.

(defn current-time
  "Fetches the current time of the context."
  [ctx]  (sim/current-time (:scheduler ctx)))

(defn current-quarter
  "Don't really need this in agenda..Fetches the current quarter."
  [ctx] (agenda/quarter (:scheduler ctx)))

(defn elapsed
  "Computes the time elapsed since the last event in the context."
  [ctx] (agenda/elapsed (:scheduler ctx)))

Public Sub addTime(t As Single)
    scheduler.addTime t
End Sub
(defn add-time
  "Ensures that time t exists on the agenda."
  [ctx t] (assoc ctx :scheduler (agenda/add-time (:scheduler ctx) t)))  

;'Created a public interface for managing updates to the system.
;Public Sub requestUpdate(tupdate As Single, requestedby As String, requestType As UpdateType, Optional trequest As Single)
;addTime tupdate
;updater.requestUpdate tupdate, requestedby, requestType, trequest
;End Sub

(defn request-update
  "Public API for accounting for update requests, which consist of a time 
   to update a specific entity in the simulation, and a form of request.  No
   additional data is passed (although I may change that in future...)"
  [ctx tupdate requested-by request-type]
  (let [t    (current-time ctx)
        req-data  {:update-time tupdate  
                   :requested-by requested-by
                   :update-type request-type 
                   :trequest t}] 
    (->> ctx 
      (add-time t)
      (simnet/handle-event 
        (->simple-event :update-request t req-data)))))    

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

(defn trigger
  "Shorthand convenience function for triggering immediate, synchronous events
   inline.  "
  ([ctx eventtype entityFrom entityTo msg data])
  ([ctx event] (simnet/handle-event event 


Private Sub Class_Initialize()
Set state = New Dictionary
End Sub

Private Sub Class_Terminate()
Set scheduler = Nothing
Set updater = Nothing
Set events = Nothing
Set state = Nothing
End Sub


;  12:   Option Explicit
;  13:  'TOM change 30 Aug 2012
;  14:   Public Function setTimeHorizon(tstart As Single, tfinal As Single, ctx As TimeStep_SimContext) As TimeStep_SimContext
;  15:   Set setTimeHorizon = ctx
;  16:  
;  17:  'Maybe extract this into a block in simlib.
;  18:   addTime tstart, ctx 'Add at least 1 day to the sim ....
;  19:   addTime tfinal, ctx
;  20:   setFinalTime tfinal, ctx 'filters out later eventful days, they just don't get added.
;  21:  
;  22:   End Function
;  23:  'Pop the next time event off of the simulation context.  Side effecting.
;  24:   Public Function advanceTime(ctx As TimeStep_SimContext) As Single
;  25:   advanceTime = ctx.scheduler.advanceTime
;  26:   End Function
;  27:  
;  28:  'get the last, or upper bound on the final time event in the context.
;  29:   Public Function getFinalTime(ctx As TimeStep_SimContext) As Single
;  30:   getFinalTime = ctx.scheduler.tfinal
;  31:   End Function
;  32:  
;  33:  'get the next scheduled time event in the simulation context, past the
;  34:  'current time of the simulation context.
;  35:   Public Function getNextTime(ctx As TimeStep_SimContext) As Single
;  36:   getNextTime = ctx.scheduler.NextTime
;  37:   End Function
;  38:  
;  39:  'Add a single time event to the simulation context.
;  40:   Public Sub addTime(t As Single, ctx As TimeStep_SimContext)
;  41:   ctx.scheduler.addTime t
;  42:   End Sub
;  43:  
;  44:  'Add multiple time events to the simulation context.
;  45:   Public Sub addTimes(times As Collection, ctx As TimeStep_SimContext)
;  46:   Dim t
;  47:   For Each t In times
;  48:       addTime CSng(t), ctx
;  49:   Next t
;  50:   End Sub
;  51:  
;  52:  'Get the current time of the simulation context.
;  53:   Public Function getTime(Optional ctx As TimeStep_SimContext) As Single
;  54:   If exists(ctx) Then getTime = ctx.CurrentTime Else getTime = 0
;  55:   End Function
;  56:  
;  57:  'Set the final time of the simulation context.  Acts as a filter
;  58:  'to scope the time horizon of the context.
;  59:   Public Sub setFinalTime(t As Single, ctx As TimeStep_SimContext)
;  60:   ctx.scheduler.tfinal = t
;  61:   End Sub
;  62:  
;  63:  'Consult the context to determine if there are, in terms of scheduled
;  64:  'time events and a possible final time, any time events remaining.
;  65:   Public Function hasTimeRemaining(ctx As TimeStep_SimContext) As Boolean
;  66:   hasTimeRemaining = ctx.scheduler.StillTime
;  67:   End Function
;  68:  
;  69:  'Triggers a generic event on the context's event thread.
;  70:   Public Sub triggerEvent(eventtype As TimeStep_Msg, entityFrom As String, entityTo As String, msg As String, _
;  71:                               Optional data As Variant, Optional context As TimeStep_SimContext)
;  72:   If exists(context) Then _
;  73:       context.events.dispatch getTime(context), eventtype, entityFrom, entityTo, msg, , data
;  74:   End Sub
;  75:  
;  76:  'interface for adding wake times to the simulation.
;  77:  'should change this to be more generic....we don't need specific update types.
;  78:  'If an entity wants an update, as long as the entity has a unique ID, it should be able to....
;  79:  'Thus, we can organize event routing by entityID.
;  80:  'Requesting an update, as a demand update, is equivalent to a demand-managing entity ID 02 requesting an
;  81:  'update...
;  82:   Public Sub requestUpdate(tupdate As Single, requestedby As String, _
;  83:                            requestType As UpdateType, Optional trequest As Single, _
;  84:                            Optional context As TimeStep_SimContext)
;  85:  
;  86:   Dim tnow As Single
;  87:   If trequest <> 0 Then
;  88:       tnow = trequest
;  89:   Else
;  90:       tnow = context.CurrentTime
;  91:   End If
;  92:  
;  93:   If exists(context) Then
;  94:       context.scheduler.addTime tupdate
;  95:       context.updater.requestUpdate tupdate, requestedby, requestType, tnow
;  96:   End If
;  97:  
;  98:   End Sub
;  99:  
; 100:  'Helper function....will be deprecated.
; 101:  'Simply sets up a new sim context.  Pass in existing args if you'd like (useful for repetetive simulations.)
; 102:  'For instance, we probably don't want to keep creating event contexts over and over again.
; 103:   Public Function makeContext(Optional scheduler As GenericScheduler, _
; 104:                                   Optional updater As TimeStep_ManagerOfUpdates, _
; 105:                                       Optional events As TimeStep_ManagerOfEvents) As TimeStep_SimContext
; 106:  
; 107:   Set makeContext = New TimeStep_SimContext
; 108:   With makeContext
; 109:       Set .scheduler = createScheduler(scheduler)
; 110:       Set .events = createEvents(events)
; 111:      'Tom change 3 Oct 2012
; 112:       Set .updater = createUpdater(.events, .scheduler, updater)
; 113:   End With
; 114:  
; 115:  'hook up the update listener to the event context
; 116:  
; 117:   End Function
; 118:  
; 119:  'Creates a scheduler (a time manager).
; 120:   Function createScheduler(Optional x As GenericScheduler) As GenericScheduler
; 121:   If Not exists(x) Then Set x = New GenericScheduler
; 122:   Set createScheduler = x
; 123:   End Function
; 124:  
; 125:  'Creates an event manager with built-in funcitonality.  This might change in the future.
; 126:   Function createEvents(Optional x As TimeStep_ManagerOfEvents) As TimeStep_ManagerOfEvents
; 127:   If Not exists(x) Then Set x = New TimeStep_ManagerOfEvents
; 128:   x.defaults
; 129:   Set createEvents = x
; 130:   End Function
; 131:  
; 132:  'Creates an update manager.
; 133:   Function createUpdater(evtsource As TimeStep_ManagerOfEvents, scheduler As GenericScheduler, Optional x As TimeStep_ManagerOfUpdates) As TimeStep_ManagerOfUpdates
; 134:   If Not exists(x) Then
; 135:       Set x = New TimeStep_ManagerOfUpdates
; 136:       x.listenUpdates evtsource
; 137:       Set x.schedule = scheduler
; 138:   End If
; 139:      
; 140:   Set createUpdater = x
; 141:   End Function
; 142:  
; 143:  'Fetch the last time the entity was updated.
; 144:   Public Function lastupdate(entityname As String, context As TimeStep_SimContext) As Single
; 145:  'Decoupled
; 146:   lastupdate = context.updater.lastupdate(entityname)
; 147:   End Function
; 148:  
; 149:  'Returns the list of updates, by type, registered for time t.
; 150:   Public Function getUpdates(update As UpdateType, t As Single, ctx As TimeStep_SimContext)
; 151:   Set getUpdates = ctx.updater.getUpdates(update, t)
; 152:   End Function
; 153:  
; 154:  'Returns the generic state of the simulation.
; 155:  'Allows mutation.
; 156:   Public Function getState(ctx As TimeStep_SimContext) As Dictionary
; 157:   Set getState = ctx.state
; 158:   End Function
; 159:  
; 160:  'it might be nice to recast this in terms of thread/sleep, thread/wake
; 161:  'rather than update...
; 162:   Public Sub threadSleep()
; 163:  
; 164:   End Sub
; 165:  
; 166:   Public Function makeDebugContext() As TimeStep_SimContext
; 167:   Set makeDebugContext = makeContext()
; 168:   makeDebugContext.AddListener "Debugger", New TimeStep_ObserverSimpleLog, newdict(0, True)
; 169:  
; 170:   End Function

