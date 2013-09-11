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
(ns marathon.sim.engine
  (:require [marathon.sim.missing] 
            [marathon.sim [core :refer now]           
                          [supply [:refer [manage-supply manage-follow-ons]]]
                          [demand [:refer [manage-demands manage-changed-demands]]]
                          [fill.demand    [:refer [fill-demands]]]
                          [policy [:refer [manage-policies]]]]
            [spork.sim    [simcontext :as sim]]))

;##Simulation Initialization

;#Auxillary functions, and legacy functions

(defn set-time
  "Initialize the start and stop time of the simulation context, based on 
   last-day."
  [ctx last-day]  
  (sim/set-time-horizon 1 (guess-last-day state last-day) ctx))


;This is just a handler that gets added, it was "placed" in the engine object
;in the legacy VBA version.  __TODO__ Relocate or eliminate engine-handler.

(defn engine-handler
  "Forces all entities in supply to be brought up to date."
  [ctx edata]
  (if (= (:type edata) :update-all-units)
    (update-all-supply ctx)
    (throw (Exception. (str "Unknown event type " edata)))))

(defn initialize-control
  "Registers a default handler for updating all units in the supplystore
   after an :update-all-units event."
  [ctx]
  (sim/add-listener :Engine 
      (fn [ctx edata name] (engine-handler ctx edata)) [:update-all-units]))

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

(defn initialize-sim
  "Given an initial - presumably empty state - and an optional upper bound on 
   on the simulation time - lastday - returns a simulation context that is 
   prepared for processing, with default time horizons and any standard 
   preconditions applied."
  [state & [lastday]]
  (-> state 
    (start-state)
    (assoc :time-start (now))
    (assoc-in  [:parameters :work-state] :simulating)
    (update-in [:context]  set-time lastday)
    (notify-watches) 
    (initialize-control)))

;##Simulation Termination Logic
;When we exit the simulation, we typically want to perform some final tasks.
;For instance, any resources (for logging, display, etc.) may need to be freed.
;The default mechanism for this is to propogate some events through the context
;and let interested parties handle themselves appropriately.

(defn finalize
  "Shifts the simulation period into a final period.  Forces sampling and any
   other cleanup actions, like computing final statistics, truncating unit 
   lifecycles, etc. Notify any other listeners that the simulation has 
   terminated.  Such notification is particularly important for observers that 
   may be stewarding resources."
  [t state]
  (let [ctx (:context state)
        s   (-> (manage-policies :final t state)   
              (assoc-in [:parameters :work-state] :outputing)
              (log-status "Processing Output")
              (assoc-in [:parameters :work-state] :terminating)
              (assoc  :time-finish (now)))]
    (sim/trigger-event :terminate :Engine :Engine "Simulation OVER!" s)))

;##Begin Day Logic
;Prior to starting a new time inteval (currently a day), we typically want to 
;perform some pre-processing or updating. 

(defn day-msg [msg day]  (str "<-------- " msg " Day " day " ---------->"))
(defn check-pause
  "In an interactive simulation, like the legacy sim, this hook lets us check 
   for user intervention each active day.  DEPRECATED."
  [state] 
  (if (paused? state)
     (sim/trigger :pause-simulation :Engine :Engine 
                  "Simulation Paused" [day (sim/get-next-time state)]) state)) 

;_Note_: in _begin-day_, check-pause is incidental to the ui, not the repl. 
;The call should be yanked.
(defn begin-day
  "Update Logic for beginning a day.  Broadcasts the beginning of the current 
   day on the simulation context's event stream.  Also, if a GUI is involved, 
   notifies listeners whether a user has paused the simulation."
  [day state]
  (->> state 
    (sim/trigger-event :begin-day :Engine :Engine
                       (day-msg "Begin" day) [day (sim/get-next-time state)])
    (check-pause)))  

(defn check-truncation
  "Legacy function that checks to see if the simulation can be truncated, i.e. 
   cut short.  Typically, we take the minimum time of 
   [latest-demand-end-time  simulation-end-time], but there are other phenomena
   that may cause us to advance the end time of the simulation."
  [state] 
  (if (and (:truncate-time state) (:found-truncation state))
    (-> (sim/trigger :all :Engine :Engine 
           (str "Truncated the simulation on day " day ", tfinal is now : " 
                (sim/get-final-time state)) state) 
      (assoc :found-truncation true))))

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
  [day state last-day]
  (->> state 
    (log-status (str "Processed day " day " of " lastday " of Simulation"))
    (sim/trigger :sample :Engine :Engine "Sampling" nil)
    (check-truncation)
    (sim/trigger :end-of-day :Engine :Engine (day-msg "End" day))))

;For dwell stats, we typically report a proxy record for units 
;not utilized during a simulation period.  To do this, we have to sample the 
;entire unit population, which this event handler does.  This is a specific bit
;of functionality that we should probably move out of here at some point.

(defn sample-unit-cycles
  "This is a special event handler where the simulation context is notified of a 
   need to sample all units in the supply.  This typically happens when a period 
   change occurs, or some other sweeping event requires a synchronization of 
   all the units."
  [t state quarterly units]    
  (->> (if (and quarterly (zero? (quot (- t 1) 90))
           (sim/add-time (inc t 90) state))
        state)
    (update-all-supply state)
    (sim/trigger-event :get-cycle-samples :Engine :Engine
       "Sample all UIC Cycles.  This is a hack." {:t t :uics units})))           

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

(defn sim-step
  "Primary state transition function for Marathon.  Threads the next day and
   an initial state through a series of transfer functions that address 
   high-level state transfers for supply, policy, demand, filling, and more. 
   Computes the resulting state - either the final state, or the initial state
   for the next step."
  [day state]
  (->> state 
    (begin-day day)         ;Trigger beginning-of-day logic and notifications.
    (manage-supply day)     ;Update unit positions and policies.
    (manage-policies day)   ;Apply policy changes, possibly affecting supply.
    (manage-demands day)    ;Activate/DeActiveate demands, handle affected units.      
    (fill-demands day)      ;Try to fill unfilled demands in priority order. 
    (manage-follow-ons day) ;Resets unused units from follow-on status. 
    (end-day day last-day)  ;End of day logic and notifications.
    (manage-changed-demands day)));Clear set of changed demands in demandstore.

;#Simulation Interface
;sim.engine/event-step-marathon is the entry point for Marathon.

(defn event-step-marathon
  "Higher order simulation handling function.  Given an initial state and an 
   upper bound on simulated time, computes the resulting simulation context via
   a state transfer function, typically sim.engine/sim-step."
  [lastday state] 
  (let [init-state (initialize-sim (initialize-output state) last-day)]
    (assert (can-simulate? init-state) 
            "There's nothing to simulate. Check Supply, Demand, and Relations!")
    (loop [day    0
           state init-state]
      (if (not (keep-simulating? state))
          (finalize day state) ;base case, return the final state and clean up.
          (let [next-day   (sim/advance-time (:context state)) ;WRONG
                next-state (sim-step next-day state)] ;Transition to next state.
            (recur next-day next-state))))))

