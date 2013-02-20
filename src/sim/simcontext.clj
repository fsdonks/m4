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
            [sim.pure [observable :as obs]]))

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
   updater ;an agenda with some special state, keeping track of previous updates. 
   events ;the main agenda for the entire simulation.
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

'Created a public interface for managing updates to the system.
Public Sub requestUpdate(tupdate As Single, requestedby As String, requestType As UpdateType, Optional trequest As Single)
addTime tupdate
updater.requestUpdate tupdate, requestedby, requestType, trequest
End Sub
(defn request-update [ctx t requested-by request-type]
  (
  

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
