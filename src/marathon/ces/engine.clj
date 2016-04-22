;##Overview
;sim.engine contains the higher-level operations that define Marathon.  It acts 
;as a harness around the simulation context, and guides the flow of control 
;throughout the simulation.  At a high level of abstraction, the engine is a 
;process that coordinates one or more concurrent processes to calculate 
;new simulation contexts from prior simulation contexts.  
;From a functional perspecive, the engine defines an ordered composition of 
;simpler transfer functions - or systems - that, when applied to a initial
;context, returns a new simulation context.  

;When performed repeatedly, feeding preceding simulation contexts into
;the transfer function, a recurrence relation between previous contexts and 
;future contexts emerges.  The result is effectively a snapshot of simulation 
;contexts, which are computed via a simple transfer function, __sim-step__,  
;which simulates multiple domains relevant to Marathon:   
;1. The flow of units through a supply  
;2. The presence of demands  
;3. The flow of units between supply and demand via fills.    

;Thus, the engine serves as the causal backbone, and coordinating mechansim, 
;for every bit of logic executed during the course of the simulation. The 
;primary function, __event-step-marathon__, prescribes the order of application 
;;of each logical subsystem. 
(ns marathon.ces.engine
  (:require [marathon.ces.missing] 
            [marathon.ces.core   :as core :refer [now]]           
            [marathon.ces.supply :as supply :refer [manage-supply manage-followons update-all]]
            [marathon.ces.demand :as demand :refer [manage-demands manage-changed-demands]]
            [marathon.ces.fill.demand       :refer [fill-demands]]
            [marathon.ces.policy :as policy :refer [manage-policies]]
            [marathon.data [store :as simstate]]
            [spork.sim     [simcontext :as sim]]))


;#Auxillary functions, and legacy functions

;;Auxillary functions from the old simstate module
;;==================================================
(defn guess-last-day 
  ([ctx lastday]
     (if-let [first-non-zero
              (first (filter #(and (not (nil? %)) (pos? %))
                             [lastday 
                              (-> ctx :state :parameters :last-day)
                              (-> ctx :state :parameters :last-day-default)]))]
       first-non-zero
       0))
  ([ctx] (guess-last-day ctx 0)))

;;Predicate to determine if we continue drawing time from the stream, i . e . simulating.
;;If an endtime is specified, we use that in our conditional logic, else we keep working until
;;no more eventful days are upon us.

;;THis is a pretty weak port....
(defn keep-simulating? [ctx]
  (let [state (:state ctx)]
    (if (-> state :truncat-time)
      (let [tlastdemand (-> state :demandstore :tlastdeactivation)]        
        (if (>= (sim/get-time ctx) tlastdemand)
          (if (neg? tlastdemand) 
            (throw (Error. "No demands scheduled.  Latest deactivation never initialized!"))
            (do (sim/trigger-event :Terminate (:name state) (:name state) 
                                   (str "Time " (sim/current-time ctx) "is past the last deactivation: "
                                        tlastdemand " in a truncated simulation, terminating!") nil ctx)
                false))
          true))
      (sim/has-time-remaining? ctx))))
  
 ;;When we simulate, starting from a GUI, we usually want to tell the user if there are any anomolies in
 ;;the data.  Specifically, if there is no supply or no demand, we inform the user, requiring verification
 ;;before simulating.
(defn get-user-verification [state & [suppress]]
  (do (println "get-user-verification is currently a stub!  We need to add interface hooks here.  Messages will still print.")
      (if (zero? (-> state :supplystore :unitmap   count)) (println "No Supply Loaded.  Continue With Simulation?"))
      (if (zero? (-> state :demandstore :demandmap count)) (println "No Demand Loaded.  Continue With Simulation?"))
      true))

;; Uses the event stream provided by simcontext, to broadcast the availability of each element of the simstate.
;; Allows interested observers, who require access across different domains, to bind to the components, making
;; local processing easier to reason about.

(defn notify-watches [simctx & [ctx simname & _]]
  (let [ctx     (or ctx simctx)
        state   (:state simctx)
        simname (or simname (:name state))]
    (->> ctx 
        (sim/trigger-event :WatchDemand simname simname "" (:demandstore state))
        (sim/trigger-event :WatchSupply simname simname "" (:supplystore state))
        (sim/trigger-event :WatchTime simname simname ""   (:scheduler ctx))
        (sim/trigger-event :WatchPolicy simname simname "" (:policystore state))
        (sim/trigger-event :WatchParameters simname simname "" (:parameters state))
        (sim/trigger-event :WatchFill simname simname "" (:fillstore state)))))
        
;;Notify interested parties of the existence of the GUI.
(defn notifyUI [ui ctx]
  (sim/trigger-event :WatchGUI "" "" "GUI Attached" ui ctx))

;##Simulation Initialization
(defn set-time
  "Initialize the start and stop time of the simulation context, based on 
   last-day."
  [ctx last-day]  
 (sim/set-time-horizon 1 (guess-last-day ctx last-day) ctx))

;This is just a handler that gets added, it was "placed" in the engine object
;in the legacy VBA version.  __TODO__ Relocate or eliminate engine-handler.
(defn engine-handler
  "Forces all entities in supply to be brought up to date."
  [ctx edata]
  (if (= (:type edata) :update-all-units)
    (supply/update-all ctx) ;from marathon.sim.supply
    (throw (Exception. (str "Unknown event type " edata)))))

(defn initialize-control
  "Registers a default handler for updating all units in the supplystore
   after an :update-all-units event."
  [ctx]
  (sim/add-listener :Engine 
      (fn [ctx edata name] (engine-handler ctx edata)) [:update-all-units] ctx))

;#Initialization
;Initialization consists of 3 tasks:    
;1. Prep the system for day 0  
;2. Adjust the context so that it defines a finite time horizon for simulation.
;   The last  day of the simulation is determined either by passing in value, 
;   or by allowing the context to derive its own last day dynamically.  
;3. Tell observers to sync themselves to the simulation state.  Some observers 
;   need access to the specific elements of the state.  This makes it easy 
;   (and indirect) to advertise the state, and to allow the observers to link 
;   to it as needed. Observers/watchers will need to be expanded on, since 
;   watches will involve effects.  

;;Temporary Stubs
;;===============
;;Used to be entityfactory.start-state supplymanager
;;Intent is to apply initial conditions to the simulation state,
;;particularly moving unit entities to where they need to be.
(defn start-state [ctx]
  (do (println "start-state is a stub.  Should be setting entities to their starting states.")
      ctx))

(defn initialize-sim
  "Given an initial - presumably empty state - and an optional upper bound on 
   on the simulation time - lastday - returns a simulation context that is 
   prepared for processing, with default time horizons and any standard 
   preconditions applied."
  [ctx & [lastday]]
    (-> ctx
        (start-state)
        (assoc-in  [:state :time-start] (now))
        (assoc-in  [:state :parameters :work-state] :simulating)
        (set-time lastday)
        (notify-watches) 
        (initialize-control)))

(defn initialize-output 
  "Sets the output path for output streams in the outputstore.
   Currently a stub...We can let an event handler deal with 
   setting up the outputstore.  There are probably other 
   entities interested in knowing about output setup 
   prior to a run."
  [ctx]
  (do (println "initialize-output is currently a stub.")
      (sim/trigger-event :initialize-output :Engine :Engine nil nil ctx)))

;##Simulation Termination Logic
;When we exit the simulation, we typically want to perform some final tasks.
;For instance, any resources (for logging, display, etc.) may need to be freed.
;The default mechanism for this is to propogate some events through the context
;and let interested parties handle themselves appropriately.


;;Look into using ->as here.

;;Note-> I bolted on manage-policies before.  I think we can just move
;;that to a handler...manage-policies enters units into the final period.
(defn finalize
  "Shifts the simulation period into a final period.  Forces sampling and any
   other cleanup actions, like computing final statistics, truncating unit 
   lifecycles, etc. Notify any other listeners that the simulation has 
   terminated.  Such notification is particularly important for observers that 
   may be stewarding resources."
  [t ctx]
  (let [final-ctx  (-> (manage-policies t ctx :final) ;;this just captures
                       ;;the final period in a period-driven update.
                       ;;really triggers an update-all-units action,
                       ;;relative to the final period.
                       (assoc-in [:state :parameters :work-state] :terminating) ;useless?
                       (assoc-in [:state :time-finish] (now)))]
    (sim/trigger-event :Terminate :Engine :Engine "Simulation OVER!" nil final-ctx)))

;##Begin Day Logic
;Prior to starting a new time inteval (currently a day), we typically want to 
;perform some pre-processing or updating. 

(defn day-msg [msg day]  (str "<-------- " msg " Day " day " ---------->"))
(defn check-pause
  "In an interactive simulation, like the legacy sim, this hook lets us check 
   for user intervention each active day.  DEPRECATED."
  [ctx] 
  (if (-> ctx :state :pause)
     (sim/trigger-event :pause-simulation :Engine :Engine 
                  "Simulation Paused" [(sim/get-time ctx) 
                                       (sim/get-next-time ctx)] ctx)
     ctx))

;_Note_: in _begin-day_, check-pause is incidental to the ui, not the repl. 
;The call should be yanked.
(defn begin-day
  "Update Logic for beginning a day.  Broadcasts the beginning of the current 
   day on the simulation context's event stream.  Also, if a GUI is involved, 
   notifies listeners whether a user has paused the simulation."
  [day ctx]
  (->> ctx
    (sim/trigger-event :begin-day :Engine :Engine
                       (day-msg "Begin" day) [day (sim/get-next-time ctx)])
    (check-pause)))  

(defn check-truncation
  "Legacy function that checks to see if the simulation can be truncated, i.e. 
   cut short.  Typically, we take the minimum time of 
   [latest-demand-end-time  simulation-end-time], but there are other phenomena
   that may cause us to advance the end time of the simulation."
  [ctx] 
  (if (and (-> ctx :state :truncate-time ) (-> ctx :state :found-truncation))
    (-> (sim/trigger-event :all :Engine :Engine 
           (str "Truncated the simulation on day " (sim/get-time ctx) ", tfinal is now : " 
                (sim/get-final-time ctx)) nil ctx) 
        (assoc-in [:state :found-truncation] true))
    ctx))

;##End Day Logic
;At the end of each "day" or discrete time step, we typically mark the passage 
;of time with some processing.  This notion could be abstracted into a higher 
;order function to wrap the simulation, along with the aforementioned begin-day
;logic.  The default behavior is to trigger a sampling event, to record daily 
;samples, and to broadcast the end-of-day to interested parties.

(defn end-day
  "Logs the passing of the day, notifies listeners of a need to generate samples
   for the day, and possibly truncates the simulation early if criteria have 
   been met."
  [day ctx]
  (->> ctx 
    (sim/trigger-event :log-status :Engine :Engine 
       (str "Processed day " day " of " (sim/get-final-time ctx) " of Simulation") nil)
    (sim/trigger-event :sample :Engine :Engine "Sampling" nil)
    (check-truncation)
    (sim/trigger-event :end-of-day :Engine :Engine (day-msg "End" day) nil)))

;For dwell stats, we typically report a proxy record for units 
;not utilized during a simulation period.  To do this, we have to sample the 
;entire unit population, which this event handler does.  This is a specific bit
;of functionality that we should probably move out of here at some point.

(defn sample-unit-cycles
  "This is a special event handler where the simulation context is notified of a 
   need to sample all units in the supply.  This typically happens when a period 
   change occurs, or some other sweeping event requires a synchronization of 
   all the units."
  [t ctx quarterly units]    
  (->> (if (and quarterly (zero? (quot (- t 1) 90)))
           (sim/add-time (+ t 90) ctx)
           ctx)
       (supply/update-all)
       (sim/trigger-event :get-cycle-samples :Engine :Engine
                          "Sample all UIC Cycles.  This is a hack." 
                          {:t t :uics units})))

(defn can-simulate? 
  "Simple predicate to ensure we have supply and demand in 
   the simulation state.  If we don't, we currently toss an 
   error."
  [ctx] 
  (let [dem  (core/get-demandstore ctx)
        supp (core/get-supplystore ctx)]
    (and 
     (supply/can-simulate? supp)
     (demand/can-simulate? dem))))

;##Primary Simulation Logic
;We enter the main engine of the Marathon simulation, which implements a 
;single-threaded, discrete event simulation by default.


;The simulation is technically a discrete event simulation, although it's 
;relatively "coarse-grained" in that, rather than having a ton of fine-grained 
;events on the queue, we only really enqueue "eventful" times.  The simulation
;step function is written as a high-level process, that is intended to be 
;invoked on every eventful day (every time on the simulation context's agenda).
;We primarily use the event system to communicate changes, and to notify 
;- potentially effectful - observers that could be logging, updating displays,
;or doing other things.  Internal simulation workings are handled in a fairly 
;bulk or batch manner by the coarse functions that comprise the step function.

;The end result is that control flow is fairly simple: we only really have 
;pending "times" to evaluate the step function against, rather than a slew of 
;smallish events to trace.  In a sense, it is very similar to the "game loop" 
;or simulation loop of time-step simulations, except for the ability to vary the
;time intervals.  Also, we retain the flexibility to push more fine-grained 
;events onto the queue if we need to.  It's the best of both worlds.  

;We want to ensure that there is an enabled supply and /or 
;demand that will actually provide some meaningful simulation output.  If not, 
;we should warn the user about missing data leading to the absence of "stuff"
;to simulate.

;#State Transition Function#
;We simulate each eventful day using a composition of simulation systems in 
;Marathon.  Each system acts in turn, computing updates to pieces of the overall
;simulation state.  Some systems communicate with eachother via events.  
;All systems have access to the entire simulation context, including the event 
;queue, for communication purposes.

;;In one sense, this defines higher-order behavior for a simulation
;;entity....
;;It handles steps by trying to achieve each of the steps in turn,
;;as with a [seq] behavior.  Another, less-structured way of looking
;;at this is that we are trying to get the system into a consistent
;;state by the end of the day.  In order to achieve consistency,
;;we can have no further pending events for the day.  So, distributing
;;the responsibilities by system is perhaps less meaningful if we're using
;;a kind of actor context.  One way we could partition these responsibilities
;;is to have each store implement its own messaging system.  For example, managing
;;the supply means allowing entities under the supply's purvue to process
;;updates for the day.
(defn sim-step
  "Primary state transition function for Marathon.  Threads the next day and
   an initial state through a series of transfer functions that address 
   high-level state transfers for supply, policy, demand, filling, and more. 
   Computes the resulting state - either the final state, or the initial state
   for the next step."
  [day ctx]
  (->> ctx 
    (begin-day day)         ;Trigger beginning-of-day logic and notifications.
    (manage-supply day)     ;Update unit positions and policies.
    (manage-policies day)   ;Apply policy changes, possibly affecting supply.
    (manage-demands day)    ;Activate/DeActiveate demands, handle affected units.      
    (fill-demands day)      ;Try to fill unfilled demands in priority order. 
    (manage-followons day)  ;Resets unused units from follow-on status. 
    (end-day day)           ;End of day logic and notifications.
    (manage-changed-demands day)));Clear set of changed demands in demandstore.

;#Simulation Interface
;sim.engine/event-step-marathon is the entry point for Marathon.
(defn event-step-marathon
  "Higher order simulation handling function.  Given an initial state and an 
   upper bound on simulated time, computes the resulting simulation context via
   a state transfer function, typically sim.engine/sim-step."
  [last-day ctx] 
  (let [init-ctx (initialize-sim (initialize-output ctx) last-day)]
    (assert (can-simulate? init-ctx) 
            "There's nothing to simulate. Check Supply, Demand, and Relations!")
    (loop [day    0
           ctx init-ctx]
      (if (not (keep-simulating? ctx))
          (finalize day ctx) ;base case, return the final state and clean up.
          (let [next-day   (sim/advance-time ctx) ;WRONG
                next-ctx (sim-step next-day ctx)] ;Transition to next state.
            (recur next-day next-ctx))))))



;;testing 
(comment 
(keep-simulating? (sim/add-time  22 emptysim))
)
