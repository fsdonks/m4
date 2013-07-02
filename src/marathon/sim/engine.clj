;sim.engine is basically the essence of Marathon.  It acts as a 
;harness around the simulation state, and dictates the flow of control through 
;the simulation.  At a high level of abstraction, the engine is a process that
;manages one or more concurrent threads of execution.  The simulation is 
;effectively a snapshot of simulation states, or contexts, that are computed via
;transfer 
;engine wakes each thread sequentially, to simulate the flow of units through a
;supply, the presence of demands, and the flow of units between supply and
;demand via fills.  As such, the engine serves as the causal backbone for every
;bit of logic executed during the course of the simulation.  The main method,
;EventStep_Marathon, displays the order of execution of each logical subsystem 
;(or thread). Engine's entire purpose is to prosecute a simulation.
(ns marathon.sim.engine
  (:require [sim    [simcontext :as sim]]
            [supply [:refer [manage-supply manage-follow-ons]]]
            [demand [:refer [manage-demands manage-changed-demands]]]
            [fill   [:refer [fill-demands]]]
            [policy [:refer [manage-policies]]]))

(defn now [] (System/currentTimeMillis))

;--------Missing Functionality>>>>>>>>>>>>>>>>>>
(comment  ;stub is somewhat useless at the moment.
	(stub "guess-last-day"
	   (defn guess-last-day [state & [last-day]]))
	;If .interactive Then MarathonEngine.notifyUI DumbUI, .context 
	;let folks know if we have a UI
	(stub "notify-watches, check MarathonEngine"
	   (defn notify-watches [state]))
	(stub "initialize-ouput not implemented, reference MarathonSteup" 
	   (defn initialize-output [state path]))
	(stub "can-simulate? not implemented, check MarathonEngine"
	   (defn can-simulate? [state]))
	(stub "keep-simulating? not implemented, check MarathonEngine"
	   (defn keep-simulating? [state]))
	(stub "log-status not yet implemented"  
	    (defn log-status [msg]))
)
;<<<<<<<<Missing Functionality----------------

;##Simulation Initialization##

;#Auxillary functions, and legacy functions#
(defn set-time
  "Initialize the start and stop time of the simulation context, based on 
   last-day."
  [ctx last-day]  
  (sim/set-time-horizon 1 (guess-last-day state last-day) ctx))


;this is just a handler that gets added, it was "placed" in the engine object
;in the legacy VBA version.
(defn control-io
  "Forces all entities in supply to be brought up to date."
  [ctx edata]
  (if (= (:type ctx) :update-all-units)
    (update-all-supply ctx)
    (throw (Exception. (str "Unknown event type " edata)))))

(defn initialize-control
  "Registers a default handler for updating all units in the supplystore
   after an :update-all-units event."
  [ctx]
  (sim/add-listener :Engine 
      (fn [ctx edata name] (control-io ctx edata)) [:update-all-units]))

;#Initialization#
;Initialization consists of 3 tasks:
;Prep the system for day 0

;Adjust the context so that it defines a finite time horizon for the simulation.
;The last  day of the simulation is determined either by passing in a lastday, 
;or by allowing the context to set its own last day dynamically.

;Tell observers to sync themselves to the simulation state.  Some observers need
;access to the specific elements of the state.  This makes it easy 
;(and indirect) to advertise the state, and to allow the observers to link to it
;as needed. Observers/watchers will need to be expanded on, since watches will 
;involve effects.
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

;##Simulation Termination Logic##
;Shift the simulation period into a final period.  Forces sampling and any other
;cleanup actions, like computing final bog:Dwell ratios, truncating unit 
;lifecycles, etc. Notify any other listeners that the simulation has terminated.
(defn finalize [t state]
  (let [ctx (:context state)
        s  (-> (manage-policies :final t state)   
             (assoc-in [:parameters :work-state] :outputing)
             (log-status "Processing Output")
             (assoc-in [:parameters :work-state] :terminating)
             (assoc  :time-finish (now)))]
    (sim/trigger-event :terminate :Engine :Engine "Simulation OVER!" s)))

;##Begin Day Logic##
;Update Logic for beginning a day.  Broadcasts the beginning of the current day
;on the simulation context's event stream.  Also, if a GUI is involved, notifies
;listeners whether a user has paused the simulation.
(defn day-msg [msg day]  (str "<-------- " msg " Day " day " ---------->"))
(defn check-pause
  "In an interactive simulation, like the legacy sim, this hook lets us check 
   for user intervention each active day.  DEPRECATED."
  [state] 
  (if (paused? state)
     (sim/trigger :pause-simulation :Engine :Engine 
                  "Simulation Paused" [day (sim/get-next-time state)])
     state)) ;PLACE Holder

(defn begin-day [day state]
  (->> state 
    (sim/trigger-event :begin-day :Engine :Engine
                       (day-msg "Begin" day) [day (sim/get-next-time state)])
    (check-pause))) ;check-pause is incidental to the ui, not the repl.  yank.

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
;##End Day Logic##
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

;##Primary Simulation Logic##
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

;If the simulation state is feasible, then we simulate each eventful day using a composition of
;simulation systems in Marathon.  Each system acts in turn, changing pieces of the overall simulation
;state.  Some systems communicate with eachother, and have direct access to eachother for efficiency and
;clarity.

(declare sim-step)
;SHOULD REPLACE THIS WITH A CALL TO EVENT-STEPPER higher order function.

;note -> these are all just partially applied functions.  could probably just 
;reduce...
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
                next-state (sim-step next-day state)] ;Updates the simulation scheduler to the next scheduled eventful time.
            (recur next-day next-state))))))

(comment 
;an alternate version, may be cleaner?
(defn sim-step2 [day state]
  (reduce (fn [s f] (f day s)) state 
    [begin-day ;Trigger beginning-of-day logic and notifications.
     manage-supply  ;update unit positions and policies.
     manage-policies  ;Activate/DeActivate policies, also moves/rotates supply...
     manage-demands  ;Activate/DeActiveate demands.  Send home units that are at DeActivating demands.      
     fill-demands ;Try to fill Active, Unfilled demands in priority order.  Moves units as needed to fill demands.
     manage-follow-ons ;Ensure that any unused units in a follow-on status are allowed to re-enter their policy.
     #(end-day %1 last-day %2) ;End of day logic and notifications.  Triggers sampling, possibly truncates the simulation.
     manage-changed-demands]))   ;Eliminate the set of changed demands in the demandstore.  Used for sampling purposes.
)

