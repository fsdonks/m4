(ns marathon.sim.engine
  (:require [sim [simcontext :as sim]]))

;'The TimeStep_Engine is basically the essence of Marathon.  It acts as a harness around the simulation
;'state, and dictates the flow of control through the simulation.  At a high level of abstraction, the
;'engine is a process that manages one or more threads of execution.  In effecting the simulation, the
;'engine wakes each thread sequentially, to simulate the flow of units through a supply, the presence of
;'demands, and the flow of units between supply and demand via fills.  As such, the engine serves as the
;'causal backbone for every bit of logic executed during the course of the simulation.  The main method,
;'EventStep_Marathon, displays the order of execution of each logical subsystem (or thread).
;
;'TOM change 10 Sep 2012
;'repurposing engine to be a shell...it's just a minimal wrapper around SimState.
;'Engine's entire purpose is to prosecute a simulation, and wrap some parameters.
;'Really, we should push all the parameters into TimeStep_Parameters.

(defn now [] (System/currentTimeMillis))
(defn not-implemented [msg] (throw (Exception. msg)))

;--------Missing Functionality>>>>>>>>>>>>>>>>>>
(defn guess-last-day [state & [last-day]]  (not-implemented "guess-last-day"))
(defn notify-watches [state]
  ;If .interactive Then MarathonEngine.notifyUI DumbUI, .context 
  ;let folks know if we have a UI
  (not-implemented "notify-watches, check MarathonEngine"))
(defn initialize-output [state path]
  (not-implemented "initialize-ouput not implemented, reference MarathonSteup"))
(defn can-simulate? [state] 
  (not-implemented "can-simulate? not implemented, check MarathonEngine"))
(defn keep-simulating? [state]
  (not-implemented "keep-simulating? not implemented, check MarathonEngine"))
(defn log-status [msg] (not-implemented "log-status not yet implemented"))
;<<<<<<<<Missing Functionality----------------

(defn set-time [ctx last-day]  
  (sim/set-time-horizon 1 (guess-last-day state last-day) ctx))

;this is just a handler that gets added, it was "placed" in the engine object
;in the legacy VBA version.
(defn control-io [ctx edata]
  (if (= (:type ctx) :update-all-units)
    (update-all-supply ctx)
    (throw (Exception. (str "Unknown event type " edata)))))

(defn initialize-control [ctx]
  (sim/add-listener :Engine 
      (fn [ctx edata name] (control-io ctx edata)) [:update-all-units]))


;Prep the system for day 0

;Adjust the context so that it defines a finite time horizon for the simulation.
;The last  day of the simulation is determined either by passing in a lastday, or
;by allowing the context to set its own last day dynamically.

;Tell observers to sync themselves to the simulation state.  Some observers need access
;to the specific elements of the state.  This makes it easy (and indirect) to advertise the
;state, and to allow the observers to link to it as needed.
;might need to expand on this, since watches will involve effects.
(defn initialize-sim [state & [lastday]]
  (-> state 
    (start-state)
    (assoc :time-start (now))
    (assoc-in [:parameters :work-state] :simulating)
    (update-in [:context]  set-time lastday)
    (notify-watches) 
    (initialize-control)))

;Simulation termination logic.

;Shift the simulation period into a final period.  Forces sampling and any other cleanup actions, like
;computing final bog:Dwell ratios, truncating unit lifecycles, etc.

;'Notify any other listeners that the simulation has terminated.  Usually used for cleanup operations.
(defn finalize [t state]
  (let [ctx (:context state)
        s  (-> (manage-policies :final t state)   
             (assoc-in [:parameters :work-state] :outputing)
             (log-status "Processing Output")
             (assoc-in [:parameters :work-state] :terminating)
             (assoc  :time-finish (now)))]
    (sim/trigger-event :terminate :Engine :Engine "Simulation OVER!" s)))

;'Update Logic for beginning a day.  Broadcasts the beginning of the current day
;'on the simulation context's event stream.  Also, if a GUI is involved, notifies
;'listeners whether a user has paused the simulation.
(defn day-msg [msg day]  (str "<-------- " msg " Day " day " ---------->"))
(defn check-pause [state] 
  (if (paused? state)
     (sim/trigger :pause-simulation :Engine :Engine 
                  "Simulation Paused" [day (sim/get-next-time state)])
     state)) ;PLACE Holder

(defn begin-day [day state]
  (->> state 
    (sim/trigger-event :begin-day :Engine :Engine
                       (day-msg "Begin" day) [day (sim/get-next-time state)])
    (check-pause))) ;check-pause should get yanked, it's incidental to the ui, not the repl.

(defn check-truncation [state] 
  (if (and (:truncate-time state) (:found-truncation state))
    (-> (sim/trigger :all :Engine :Engine 
           (str "Truncated the simulation on day " day ", tfinal is now : " 
                (sim/get-final-time state)) state) 
      (assoc :found-truncation true))))

;'End of day logic.  Logs the passing of the day, notifies listeners of a need to generate
;'samples for the day, and possibly truncates the simulation early if criteria have been met.
(defn end-day [day state last-day]
  (->> state 
    (log-status (str "Processed day " day " of " lastday " of Simulation"))
    (sim/trigger :sample :Engine :Engine "Sampling" nil)
    (check-truncation)
    (sim/trigger :end-of-day :Engine :Engine (day-msg "End" day))))

;This is a special event handler, where the TimeStep_Engine is notified of a need to sample
;all units in the supply.  This typically happens when a period change occurs, or some other
;sweeping event that requires a synchronization of all the units.  For dwell stats, we typically
;report a proxy record for units not utilized during a simulation period.  To do this, we have
;to sample the entire unit population, which this event handler does.
(defn sample-unit-cycles [t state quarterly units]    
  (->> (if (and quarterly (zero? (quot (- t 1) 90))
           (sim/add-time (inc t 90) state))
        state)
    (update-all-supply state)
    (sim/trigger-event :get-cycle-samples :Engine :Engine
       "Sample all UIC Cycles.  This is a hack." {:t t :uics units})))
           

;'The main engine of the Marathon simulation.  This function comprises a 
;single-threaded, discrete event simulation.

;Decided to decouple the initialization of output from the input simstate.
;This opens up some possibilities for controlling how we handle IO...Right now, it's
;effectively a relocation of the output manager's initialization logic (which used to
;happen as the final step of generating simstate.  The upshot is, we can generate simstate
;without generating output files or causing side-effects related to output.  That allows
;much more pre-processing of the input simstate, which is necessary for splitting a state
;into multiple runs.

;we are stepping through eventful days only .... Moving to push system for time, minimizing updates.
;want to ensure that there is an enabled supply and /or demand that will actually provide some meaningful
;simulation output.  If not, we should warn the user about missing data leading to the absence of "stuff"
;to simulate.

;If the simulation state is feasible, then we simulate each eventful day using a composition of
;simulation systems in Marathon.  Each system acts in turn, changing pieces of the overall simulation
;state.  Some systems communicate with eachother, and have direct access to eachother for efficiency and
;clarity.

(declare sim-step)
;SHOULD REPLACE THIS WITH A CALL TO EVENT-STEPPER higher order function.
(defn event-step-marathon [lastday state] 
  (let [init-state (initialize-sim (initialize-output state) last-day)]
    (assert (can-simulate? init-state) 
            "There's nothing to simulate. Check Supply, Demand, and Relations!")
    (loop [day    0
           state init-state]
      (if (not (keep-simulating? state))
        (finalize day state) ;base case, return the final state and clean up.
        (let [next-day (sim/advance-time (:context state)) ;WRONG
              next-state (sim-step next-day state)] ;Updates the simulation scheduler to the next scheduled eventful time.
          (recur next-day next-state))))))
;note -> these are all just partially applied functions.  could probably just 
;reduce...
(defn sim-step [day state]
  (->> state 
    (begin-day day) ;Trigger beginning-of-day logic and notifications.
    (manage-supply day) ;update unit positions and policies.
    (manage-policies day) ;Activate/DeActivate policies, also moves/rotates supply...
    (manage-demands day) ;Activate/DeActiveate demands.  Send home units that are at DeActivating demands.      
    (fill-demands day) ;Try to fill Active, Unfilled demands in priority order.  Moves units as needed to fill demands.
    (manage-follow-ons day) ;Ensure that any unused units in a follow-on status are allowed to re-enter their policy.
    (end-day day last-day) ;End of day logic and notifications.  Triggers sampling, possibly truncates the simulation.
    (manage-changed-demands day)));Eliminate the set of changed demands in the demandstore.  Used for sampling purposes.
    ;Could be handled elsewhere, maybe as an event handler.

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

