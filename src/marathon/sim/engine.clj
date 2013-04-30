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

(defn guess-last-day [state & [last-day]]  (not-implemented "guess-last-day"))  
(defn set-time [ctx last-day] 
  (sim/set-time-horizon 1 (guess-last-day state last-day) ctx))


;'TOM Change 17 Aug 2012 -> control system to allow the engine to handle system-wide requests
;'for things like updates, etc.
Sub ControlIO(msgid As Long, Optional data As GenericPacket)
If msgid = TimeStep_Msg.updateAllUnits Then
    'TOM Change, relying on a binding to global simState.
    MarathonOpSupply.updateALL data.t, simstate.supplystore, simstate.context
Else
    Err.Raise 101, , "No other events are explicitly handled..."
End If

End Sub

(defn control-io [ctx edata]
  (if (= (:type ctx) :update-all-units)
    (update-all-supply ctx)
    (throw (Exception. (str "Unknown event type " edata)))))

;'TOM Change 18 Aug 2012
;'registers simulation-wide events, like updateAll__ with the engine.
;'Allows the engine to perform system-wide IO operations via its handler.
Sub InitializeControl(ctx As TimeStep_SimContext)
With ctx.events
    If Not .GetObserver.clients.exists(name) Then
        .AddListener name, Me, TimeStep_Msg.updateAllUnits
    End If
End With
End Sub

(defn initialize-control [ctx]
  (sim/add-listener :Engine (fn [ctx edata name] (control-io ctx edata))

                    
                    
                    ;'Timestep_simulation from given parameters.
;'Right now, we read parameters/demand from Excel Host

(defn initialize-sim [state & [last-day]]
  (-> state 
    (start-state)
    (assoc :time-start (now))
    (assoc-in [:parameters :work-state] :simulating)
    (update-in [:context]  set-time last-day)
    (notify-watches) ;might need to expand on this, since watches will involve effects.
    (initialize-control))
    
  )


;'Prep the system for day 0
Private Sub InitializeSim(state As TimeStep_SimState, Optional lastday As Single)

With state
    
;    'TOM Change 30 Aug 2012
;    'EntityFactory.startstate SupplyManager
    .EntityFactory.startstate .supplystore
    
;    'TOM Change 30 Aug -> shift to this next.  SWAP
;    'MarathonOpFactory.startstate .supplystore, .parameters, .context
    .timeStart = TimeValue(Now)
    .parameters.Workstate = Workstates.Simulating
    
;    'Adjust the context so that it defines a finite time horizon for the simulation.
;    'The last  day of the simulation is determined either by passing in a lastday, or
;    'by allowing the context to set its own last day dynamically.
    Set .context = SimLib.setTimeHorizon(1, MarathonEngine.guessLastDay(state, lastday), _
                                            .context)
        
;    'TOM Change 17 Aug 2012
;    'Tell observers to sync themselves to the simulation state.  Some observers need access
;    'to the specific elements of the state.  This makes it easy (and indirect) to advertise the
;    'state, and to allow the observers to link to it as needed.
    MarathonEngine.NotifyWatches state, .context  'propogate information about the simulation....
    If .interactive Then MarathonEngine.notifyUI DumbUI, .context 'let folks know if we have a UI
        
    InitializeControl .context  'establish a link to the engine for system-wide events.
    
;    'Already sampling on day 0.
;    'trigger Sample, name, name, vbnullstring
End With

End Sub
;'TOM Change 26 Oct -> decoupled output creation.
Private Sub initializeOutput(simstate As TimeStep_SimState, Optional path As String)
Call MarathonSetup.initializeOutput(simstate, path)
End Sub

;'The main engine of the Marathon simulation.  This function comprises a single-threaded, discrete event
;'simulation.
Function EventStepMarathon(Optional lastday As Single, _
                                Optional state As TimeStep_SimState)
Dim day As Single
Dim replicationtime As Single 'something to keep time in between days...

If state Is Nothing Then Set state = getSimState()
Set simstate = state 'bind the global state for the engine as well, only used for control IO.

;'TOM Change 26 October 2012
;'Decided to decouple the initialization of output from the input simstate.
;'This opens up some possibilities for controlling how we handle IO...Right now, it's
;'effectively a relocation of the output manager's initialization logic (which used to
;'happen as the final step of generating simstate.  The upshot is, we can generate simstate
;'without generating output files or causing side-effects related to output.  That allows
;'much more pre-processing of the input simstate, which is necessary for splitting a state
;'into multiple runs.
initializeOutput state

;'Derive the last day if we don't know it.
If lastday = 0 Then lastday = MarathonEngine.guessLastDay(state, 0)

;'we are stepping through eventful days only .... Moving to push system for time, minimizing updates.
;'want to ensure that there is an enabled supply and /or demand that will actually provide some meaningful
;'simulation output.  If not, we should warn the user about missing data leading to the absence of "stuff"
;'to simulate.
InitializeSim state, lastday

;'If the simulation state is feasible, then we simulate each eventful day using a composition of
;'simulation systems in Marathon.  Each system acts in turn, changing pieces of the overall simulation
;'state.  Some systems communicate with eachother, and have direct access to eachother for efficiency and
;'clarity.
If MarathonEngine.CanSimulate(state) Then
    While KeepSimulating(state)
;        'Updates the simulation scheduler to the next scheduled eventful time.
        profile "AdvanceTime"
        day = SimLib.advanceTime(state.context)
        profile "AdvanceTime"
    
;        'Trigger beginning-of-day logic and notifications.
        BeginDay day, state
        
;        'Trying to manage the supply with our supply op
;        'Manage the supply of the simulation, updating unit positions where necessary.
        profile "ManageSupply"
;        'state.supplystore.ManageSupply day, state  'Move/Rotate Supply -> changed location.
        MarathonOpSupply.ManageSupply day, state
        profile "ManageSupply"
        
;        'Activate/DeActivate policies, also moves/rotates supply...
;        'TOM change 30 Aug 2012 -> added a shim to allow simcontext.  Policy is handled entirely in module now.
        profile "ManagePolicies"
        MarathonOpPolicy.ManagePolicies day, state
        profile "ManagePolicies"
               
        'Activate/DeActiveate demands.  Send home units that are at DeActivating demands.
        profile "ManageDemand"
;        'state.demandstore.ManageDemands CLng(day), state
        MarathonOpDemand.ManageDemands day, state
        profile "ManageDemand"
        
;        'Try to fill Active, Unfilled demands in priority order.  Moves units as needed to fill demands.
        profile "FillDemands"
        MarathonOpDemand.FillDemands day, state
;        'state.demandstore.FillDemands CLng(day), state
        profile "FillDemands"
        
;        'Ensure that any unused units in a follow-on status are allowed to re-enter their policy.
;        'TOM Change 17 Sep 2012 -> using logic in module vs. method.
;        'state.supplystore.ReleaseFollowOns
        MarathonOpSupply.ManageFollowOns day, state
                
;        'End of day logic and notifications.  Triggers sampling, possibly truncates the simulation.
        endday day, state, lastday 'Record End of Day
        
;        'Eliminate the set of changed demands in the demandstore.  Used for sampling purposes.
;        'Could be handled elsewhere, maybe as an event handler.
;        'state.demandstore.clearChanges <- eliminated old call to have a uniform function signature
        MarathonOpDemand.ManageChangedDemands day, state
    Wend
        
;    'Final simulation logic.  Triggers final sampling of the simulation state, as well as termination
;    'notification.
    finalize day, state
Else
    Err.Raise 101, , "There's nothing to simulate.  Check Supply, Demand, and Relations!"
End If

profile "EventStep"
End Function


;'Simulation termination logic.
Public Sub finalize(t As Single, state As TimeStep_SimState)
;'Shift the simulation period into a final period.  Forces sampling and any other cleanup actions, like
;'computing final bog:Dwell ratios, truncating unit lifecycles, etc.
;'state.policystore.ManagePolicies t, "Final", state

;'TOM note -> It'd be nice for all of these guys to have an identical signature...
MarathonOpPolicy.ManagePolicies t, state, "Final"

;'Not vital, but notifies user we're in an output phase.
state.parameters.Workstate = outputing
logStatus "Processing Output"
state.parameters.Workstate = terminating
EnableScreenUpdates

state.timeFinish = TimeValue(Now)
;'Notify any other listeners that the simulation has terminated.  Usually used for cleanup operations.
SimLib.triggerEvent TimeStep_Msg.Terminate, name, name, "SimulationOVER!", , state.context

;'Excel only....should be independent.
finalizeControlSheet state.timeStart, state.timeFinish
;'Clear the simulation state global.
Set simstate = Nothing
End Sub





;'Update Logic for beginning a day.  Broadcasts the beginning of the current day
;'on the simulation context's event stream.  Also, if a GUI is involved, notifies
;'listeners whether a user has paused the simulation.
Private Sub BeginDay(day As Single, state As TimeStep_SimState)

If state.interactive Then
    DoEvents
End If

trigger TimeStep_Msg.BeginDay, name, name, "<-------- Begin Day " & day & " ---------->", _
        SimulationLog, , list(day, SimLib.getNextTime(state.context)), state.context

If pause = True Then
    trigger TimeStep_Msg.PauseSimulation, name, name, "Simulation Paused", _
        SimulationLog, , list(day, SimLib.getNextTime(state.context)), state.context
End If

End Sub


;'End of day logic.  Logs the passing of the day, notifies listeners of a need to generate
;'samples for the day, and possibly truncates the simulation early if criteria have been met.
Private Sub endday(day As Single, state As TimeStep_SimState, Optional lastday As Single)

logStatus "Processed day " & day & " of " & lastday & " of Simulation"

If interactive Then
    DoEvents
End If
;'trigger EndofDay, name, name, "<-------- End of Day" & day & " ---------->", SimulationLog
trigger sample, name, name, "Sampling"
;'TOM Change 13 August 2012 -> added a default to quarterly sample all unit cycles
;'for bog dwell stats.

If state.sampleCyclesOverTime Then _
    sampleUnitCycles day, state, True
;    'sampleUnitCycles day, SupplyManager.unitmap, True

If state.truncateTime And state.foundTruncation Then
    trigger all, name, name, "Truncated the simulation on day " & day & ", tfinal is now : " & SimLib.getFinalTime(state.context)
    foundTruncation = False
End If

If pause = True Then
    ui.interrupt
End If

trigger TimeStep_Msg.EndofDay, name, name, "<-------- End Day " & day & " ---------->", _
        SimulationLog, , day, state.context

End Sub

;'This is a special event handler, where the TimeStep_Engine is notified of a need to sample
;'all units in the supply.  This typically happens when a period change occurs, or some other
;'sweeping event that requires a synchronization of all the units.  For dwell stats, we typically
;'report a proxy record for units not utilized during a simulation period.  To do this, we have
;'to sample the entire unit population, which this event handler does.
;'TOM Change 13 Aug 2012
Private Sub sampleUnitCycles(t As Single, state As TimeStep_SimState, Optional quarterly As Boolean, Optional units As Dictionary)
Dim samplep As Boolean
If quarterly Then
    If (t - 1) Mod 90 = 0 Then
        samplep = True
        addTime t + 90 'request another sample in 90 days.  This really should be request event.
    End If
Else
    samplep = True
End If
If samplep Then
    'SupplyManager.updateALL t 'synchronize our unit stats
    MarathonOpSupply.updateALL t, state.supplystore, state.context
    trigger GetCycleSamples, name, name, "Sample all UIC Cycles.  This is a hack.", , , _
            newdict("t", t, "uics", units)
End If

End Sub





