(ns sim.pure.observable)

;This is a simple implementation of the oberserver pattern.
;An observer serves as an interface for registering "listeners" that respond to 
;any number of external stimuli by triggering subscribed procedures...
;This is an explicit implementation of native .Net language features.  When we 
;port to .Net or clojure, we "probably" won;t even need this class.

;Basically, the observer keeps an internal list of procedures to call.  We;ll do
;this with object references So, an observer keeps a list of Callables. A 
;callable is just a delegate<;T>, basically it takes an argument and performs 
;some sideffect with it. We will use observers to flexibly automate tasks like 
;logging events, keep track of statistics, recording graphs and trends, etc.

;As a result, the main control flow of the program will populate the event 
;stream, which can be listened to [observed] by any number of registered 
;observers.  The observers will, by convention, perform side effects that 
;DO NOT affect the control flow of the simulation.  They are primarily 
;responsible for recording state, updating the user interface, writing to files, 
;storing statistics for later retrieval. Basically, they accessorize the engine, 
;and provide an easily extendable mechanism to add lots and lots of ad-hoc 
;functionality to the basic simulation without screwing with the dedicated 
;internals of the simulation.

;Note -> we can also use observers to model functional composition, by passing 
;state references through an observer chain, where each oberver partially 
;mutates the referenced state. While useful, this is again discouraged....
;mutation can inhibit reasoning about the program state, and could create funky 
;bugs that are hard to spot.  Still, if necessary, it could be a powerful tool.

;Note -> there is currently no order of execution in notifying the observers 
;(actually, the order they register is the order of execution.  The net effect
;of this is that observations should be cummutative i.e., the effect of an
;observation should be independent of order of execution. This is another 
;argument against using observers as mutators of state, or signal chains, since 
;the designer would need to be extra vigilant about the order of execution.
;Note -> all of these concerns are moot in F# and clojure.
Option Explicit

(defrecord observer-network [name clients subscriptions])

;When you boil it down, the observable is just a collection of routing 
;information.  We add routing information to the observable in the form of 
;subscriptions, where unique clients subscribe, or attach, to one or more 
;events.

;All an observer needs to do is provide and interface that allows interested 
;parties to register themselves with the observer.  We can then flex that
;knowledge later to traverse the clients for a specific event.  In the impure
;world, the traversal would cause I/O via evaluating a valueluess callback 
;function associated with the entity.  In the pure world, our traversal will 
;involve invoking a handler function (passed in as a map of handlers) associated
;with the interested entity.  The handler function will take the current state,
;and the event-data, and will return a new state.  This way, we perform an
;event-driven reduction using A: routing information provided by the observable,
;and B: a set of handler functions, and C: an initial state.  
;That allows us to bridge the gap between the typical impure simulation and the 
;desired pure form! 

;Note -> some handler functions may (and likely will!) have side-effects, for 
;logging, visualization, etc.  In this case, they must still return the input
;state after performing the side-effect.

;Note -> we could probably use a simple tags data structure here...since it's 
;basically the same thing....
(defn- drop-event-client
  "Remove the relation from event-type to client-name.  If no relations 
   remain for event-type, drops the event-type from subscriptions."
  [obs client-name event-type] 
  (let [subscriptions (get obs :subscriptions)
        scripts (get subscriptions event-type {})]
    (if (contains? scripts client-name)
      (if (= (count scripts) 1)        
        (assoc obs :subscriptions (dissoc subscriptions event-type))
        (assoc-in obs [:subscriptions event-type] (dissoc scripts client-name)))      
      (throw (Exception. 
               (str "Client subscription does not exist" client-name)))))) 

(defn- drop-client-event
  "Remove the relation from client-name to event-type.  If no relations remain
   for client, client is dropped from clients."
  [obs client-name event-type]
  (let [clients (get obs :clients)
        events (get clients client-name #{})]
    (if (contains? clients client-name)
      (if (= (count events) 1)        
        (assoc obs :clients (dissoc clients client-name))
        (assoc-in obs [:clients client-name] (dissoc events event-type)))      
      (throw (Exception. 
               (str "Client does not exist" client-name)))))) 

(defn un-register
  "Drop the relation from client-name to event-type, and the subscription from 
   event-type to client-name."
  [obs client-name event-type]
  (-> (drop-event-client obs client-name event-type)
    (drop-client-event client-name event-type))) 

(defn get-event-clients
  "Return a map of clients to handlers, which will be invoked when this event 
   is traversed."
  [obs event-type]
  (get-in obs [:subscriptions event-type]))
  
(defn get-client-events
  "Return an un-ordered set of the events this client is interested in."
  [obs client-name]
  (get-in obs [:clients client-name]))

(defn universal?
  "Determines if the client is subscribed to all events, or only a specific set
   of events.  If the client has :all as its subscription, it will trigger on 
   any event."
  [obs client-name]
  (contains? (client-events obs client-name) :all))

(defn register
  "Adds a bi-directional relation between client-name and event-type, where the 
   abstract traversal cost from event-type to client, is to invoke f.  
   We only want to allow 2 classes of registration: universal and specific.  
   Universal subscribers, tagged by :all, trigger on any event, and require no 
   argument for event-type. Specific subscriptions take an argument for 
   event-type.  Subscribers are either specific (triggering on specific events) 
   or universal, but never both.  If no event-type is supplied, the type will 
   default to :all, or a universal handler."
  ([obs client-name handler event-type]
    (let [{:keys [clients subscriptions]} obs]    
      (let [next-obs  (merge obs 
                             {:clients (assoc clients client-name 
                                (conj (get clients client-name #{}) event-type))
                              :subscriptions (assoc subscriptions event-type 
                                (assoc (get subscriptions event-type {})
                                       client-name handler))})
            evts (get-client-events nextobs)]
        (cond (= event-type :all) ;registered a universal subscription. 
              (reduce (fn [o e-type] (un-register o client-name etype))
                      next-obs (disj evts :all))
              :else  ;registered a specific subscription.
              (if (= evts #{:all}) 
                nextobs
                (reduce (fn [o e-type] (un-register o client-name etype))
                        next-obs (disj evts :all)))))))
  ([obs client-name handler] (register obs client-name handler :all)))

(defn register-routes
  "Register multiple clients associated with multiple event/handler pairs.  
   The routing is passed as a map of maps, where the keys are client-names, and 
   the associated values are maps of event-type -> handler.  Thus, registering 
   a client entity 'Bob with the :shower and :eat events would look like: 
   {'Bob {:shower take-shower :eat eat-sandwich}}"
  [obs client-event-handler-map]
  (reduce (fn [o1 [client handler-map]] 
            (reduce  (fn [o2 [etype handler]]
                       (register o client-name handler etype)) o1 handler-map))
          obs client-event-handler-map))

(defn drop-client
  "Unregisters client from every event, automatically dropping it from clients."
  [obs client-name] 
  (assert (contains? (:clients obs) client-name) 
          (str "Client " client-name " does not exist!"))
  (reduce (fn [o etype] (un-register o client-name etype))
      obs (client-events obs client-name)))      

(defn propogate
  "Instead of the traditional notify, as we have in the observable lib, we 
   define a function called propogate, which acts akin to a reduction.
   Each client with either an :all routing or a subscription to the event-type 
   of event-to-handle, will be traversed.  Every step of the walk is treated as 
   a state transition, in which the handler function and the state are supplied
   to a transition-function - event-transition-func - which determines the 
   resulting value.  The resulting reduction over every transition is the return 
   value of the network." 
  [obs event-type event-data state & {:keys [event-transition-func] 
                                      :or   {event-transition-func 
                                             (fn [state handler e]
                                               (handler e state))}}]
  (let [clients (get-event-clients obs (event-type 
  
)
  


'TOM Change 30 June 2011 -> state is now passed as a genericpacket.
'Public Sub notify(msgid As Long, Optional state As Dictionary)
Public Sub notify(msgid As Long, Optional state As GenericPacket)
Dim clientname
Dim client As ITriggerable
Dim ptr As Dictionary

If clients.count > 0 Then
    'recursively notify indiscriminate listeners....things listening for 0
    If subscriptions.exists(msgid) Then
        Set ptr = subscriptions(msgid)
        For Each clientname In ptr
            Set client = ptr(clientname)
            client.trigger msgid, state
        Next clientname
    End If
    
    'If msgid <> 0 Then notify 0, state 'notify universal listeners....
    If subscriptions.exists(0) Then
        Set ptr = subscriptions(0)
        For Each clientname In ptr
            Set client = ptr(clientname)
            client.trigger msgid, state
        Next clientname
    End If
'Else
    'Debug.Print "no clients registered for "; msgid & " under observer " & name
End If

Set ptr = Nothing

End Sub

Public Function getClients(msgid As Long) As Dictionary

If subscriptions.exists(msgid) Then
    Set getClients = subscriptions(msgid)
Else
    Set getClients = emptydict
End If

End Function

Public Function getSubscriptions(clientname As String) As Dictionary

If clients.exists(clientname) Then
    Set getSubscriptions = clients(clientname)
Else
    Set getSubscriptions = New Dictionary
End If
    
End Function

Private Sub Class_Terminate()

Set clients = Nothing
Set subscriptions = Nothing

'Set ptr = Nothing
Set emptydict = Nothing

End Sub



Private Function IObservable_getRegistered() As Collection
Set IObservable_getRegistered = list(clients, subscriptions)
End Function

'observers can be triggered, so observers can subscribe to eachother.
'their trigger simply points to the notify method.
'TOM Change 30 june 2011
'Private Sub ITriggerable_trigger(msgid As Long, Optional data As Scripting.IDictionary)
Private Sub ITriggerable_trigger(msgid As Long, Optional data As GenericPacket)
notify msgid, data
End Sub

'IObservable implementations mirror earlier ones.
Private Sub IObservable_clearClient(clientname As String)
clearClient clientname
End Sub

Private Function IObservable_getClients(msgid As Long) As Scripting.IDictionary
Set IObservable_getClients = getClients(msgid)
End Function

Private Function IObservable_getSubscriptions(clientname As String) As Scripting.IDictionary
Set IObservable_getSubscriptions = getSubscriptions(clientname)
End Function

Private Sub IObservable_notify(msgid As Long, Optional state As GenericPacket)
notify msgid, state
End Sub

Private Sub IObservable_Register(clientname As String, client As ITriggerable, Optional msgid As Long)
Register clientname, client, msgid
End Sub

Private Sub IObservable_unRegister(clientname As String, Optional msgid As Long)
unRegister clientname, msgid
End Sub

