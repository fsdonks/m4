(ns sim.pure.observable)
;Note -> there is currently no order of execution in notifying the observers 
;(actually, the order they register is the order of execution.  The net effect
;of this is that observations should be cummutative i.e., the effect of an
;observation should be independent of order of execution. This is another 
;argument against using observers as mutators of state, or signal chains, since 
;the designer would need to be extra vigilant about the order of execution.
;Note -> all of these concerns are moot in F# and clojure.

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

(defrecord event-network [name clients subscriptions])

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
  (contains? (get-client-events obs client-name) :all))

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
            evts (get-client-events next-obs client-name)]
        (cond (= event-type :all) ;registered a universal subscription. 
              (reduce (fn [o e-type] (un-register o client-name e-type))
                      next-obs (disj evts :all))
              :else  ;registered a specific subscription.
              (if (= evts #{:all}) 
                next-obs
                (reduce (fn [o e-type] (un-register o client-name e-type))
                        next-obs (clojure.set/difference  
                                   evts #{:all event-type})))))))
  ([obs client-name handler] (register obs client-name handler :all)))

(defn register-routes
  "Register multiple clients associated with multiple event/handler pairs.  
   The routing is passed as a map of maps, where the keys are client-names, and 
   the associated values are maps of event-type -> handler.  Thus, registering 
   a client entity 'Bob with the :shower and :eat events would look like: 
   {'Bob {:shower take-shower :eat eat-sandwich}}"
  [obs client-event-handler-map]
  (reduce (fn [o1 [client-name handler-map]] 
            (reduce  (fn [o2 [etype handler]]
                       (register o2 client-name handler etype)) o1 handler-map))
          obs client-event-handler-map))

(defn drop-client
  "Unregisters client from every event, automatically dropping it from clients."
  [obs client-name] 
  (assert (contains? (:clients obs) client-name) 
          (str "Client " client-name " does not exist!"))
  (reduce (fn [o etype] (un-register o client-name etype))
      obs (get-client-events obs client-name)))      

(def default-transition 
  (fn [state e [client-name handler]]
    (handler e state)))
(def noisy-transition 
  (fn [state e [client-name handler]]
    (do (println (str {:state state :event e :client client-name}))
      (handler e state))))

;This is a pretty straightforward replacement for notify, or trigger from the 
;old mutable observable implementation.
(defn propogate-event
  "Instead of the traditional notify, as we have in the observable lib, we 
   define a function called propogate-event, which acts akin to a reduction.
   Each client with either an :all routing or a subscription to the event-type 
   will be traversed.  Every step of the walk is treated as 
   a state transition, in which the handler function and the state are supplied
   to a transition-function - event-transition-func - which determines the 
   resulting state.  
   event-transition-func:: 
      'a -> sim.data.IEvent -> ('b, (sim.data.IEvent -> 'a -> 'a)) -> 'a        
   The resulting reduction over every transition is the return value of the 
   network.  If no transition function is supplied, we default to simply  
   applying the associated handler to the event-data and the state." 
  [obs event-type event-data state0 
   & {:keys [event-transition-func] 
      :or   {event-transition-func default-transition}}]
  (let [client-handler-map 
        (merge (get-event-clients obs event-type)
               (if (not= event-type :all)
                 (get-event-clients obs :all) {}))]
    (reduce (fn [state [client-name handler]] 
              (event-transition-func state event-data [client-name handler]))
            state0  client-handler-map)))

;these are combinators for defining ways to compose event handlers, where an 
;event handler is a function of the form: 
; (event-type -> event-data -> state -> state)

(defn merge-networks
  "This is a simple merge operation that clooges together one or more networks,
   and returns a new network that is the set-theoretic union of clients and 
   events.  The resulting network has every client that the original did, as 
   well as every event, with subscriptions merged."
  [nets]
  )
  
  

(defn map-network
  "When events are propogated across the network, apply function f to the 
   result of the propogation."
  [origin f]
  (fn [event-type event-data state0]
   (f (propogate-event origin event-type event-data state0))))

(defn singleton [handler-function]
  (register-routes (empty-stream :anonymous) 
     {:base {:all handler-function}})) 

;comp base base2   
;{:base {:all (fn [type data state] 
;               (->> (handle-func1 type data state)
;                    (handle-func2 type data)))}}

;;map f base
;{:base {:all (fn [type data state] (f (handle-func1 type data state)))}}
;;pmap f base 
;{:base {:all (fn [type data state] 
;               (pmap (fn [h] (h type data state)) 
;                     [handle-func1 handle-func2]))}}
;;reduce f [base base2] 
;{:base {:all (fn [type data state] 
;               (reduce (fn [s h] 
;                         (h type data s)) state [base base]



(defn map-handler [handler-func base]  
  (singleton (fn [type data state]
               (handler-func type data 
                 (propogate-event base type data state)))))

(defn filter-handler [f base]
  (singleton (fn [type data state]
               (if (f {:type type :data data :state state})
                 (base type data state)
                 state))))

(defn split-handler [split-map base]
  (let [split (reduce (fn [label] 
  (singleton (fn [type data state]
               
{:left  {:base {:all handle-func1}}}
{:right {:base {:all handle-func2}}}


  

(defn empty-stream [name] 
  (map->event-network  {:name name :clients {} :subscriptions {}})) 

(comment ;testing 
         
(def sample-net 
  (-> (empty-network "Mynetwork")
    (register-routes 
      {:hello-client {:hello-event 
                       (fn [e state] 
                         (do (println "Hello!") state))}})))
(def simple-net 
  (-> (empty-network "OtherNetwork")
    (register-routes 
      {:hello-client {:all 
                      (fn [e state] 
                        (do (println "I always talk!")) state)}})))
(defn prop-hello [] 
  (propogate-event sample-net :hello-event "hello world!" nil))

(defn echo-hello [] 
  (propogate-event sample-net :hello-event "hello world!" nil 
                   :event-transition-func noisy-transaction))
)


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


