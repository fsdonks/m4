(ns marathon.test)

;basic marathon simulation....
;supply...
  ;set of units
  ;force generation function....
;demand...
  ;set of demands
  ;demand activation function
;policy...
  ;set of active force generation policies 
  ;set of active force management (fill) policies

;our job is to integrate
  ;supply (units)
    ;register and deregister units of supply
  ;demand (activities)
    ;activate/deactivate demands
  ;policy
    ;force management (fill policy: suitability of fill, priority of demand)
      ;involves substitution, suitability function...
      ;generally, fill highest priority demands with most suitable units-greedy.
    ;force generation (supply policy; available supply, readiness of supply)

;this integration is Marathon.....some dynamic process that can take initial 
;supply, demand, policy states, and compute a history.

;We maintain a notion of time to order our history, and to control state 
;transition.

  ;Some state transitions may be simultaneous (occur on the same time period).
                                        ;Multiple events happen on the same day.


;;==Brought in from sim.legacy==
;; this is a good demo app for a simple simulation built on spork.

;This is a simple test simulation to illustrate how to use periods in a simulation context.
;Assuming we have some rudimentary system that operates off of change in simulation time,
;and determines if a new period has become active, we broadcast the changing of periods and
;update some state to indicate current period.
;Simultaneously, we have another system that responds to changes in time.  Every time sample,
;if the time is odd, an odd event (defined inline) is triggered.  If an even event is encounted,
;an even event is fired.
;
;Public Sub periodtest()
;
;Dim t 'a simple time index.
;Dim sim As timestep_SimContext
;Dim simstate As Dictionary 'some simple simulation state.
;
;these periods are defined apriori.  A system consults them to trigger events.
;Dim lessthan10 As GenericPeriod 'a period defined for t <  10
;Dim morethan10 As GenericPeriod 'a period defined for t >= 10
;
;these periods are triggered by events
;Dim oddPeriod As GenericPeriodReactive 'a reactive period that is defined over odd time intervals.
;Dim evenPeriod As GenericPeriodReactive 'a reactive period that is defined over even time intervals.
;
;Dim reactiveLogger As TimeStep_ObserverSimpleLog
;Dim periodLogger As TimeStep_ObserverSimpleLog
;
;Define a new simulation context.  This provides a blank
;template for a simulation.
;Set sim = SimLib.makeContext
;
;map two event slots to indicate even flips, odd flips, and a Period Change.
;EventLib.addEvents sim.events.evtstream, eventList(1, "Odd", 2, "Even", 3, "Period Changed")
;
;set up two reactive periods that observe odd and even events:
;
;Odd period measures periods between every odd event on the
;simulation's event stream.
;Set oddPeriod = periodEvery(1, sim.events.evtstream.observer, "OddPeriod")
;
;Even period measures periods between every even event on the
;simulations' event stream.
;Set evenPeriod = periodEvery(2, sim.events.evtstream.observer, "EvenPeriod")
;
;Define an observer that listens for Period change events (defined
;elsewhere).
;Set periodLogger = New TimeStep_ObserverSimpleLog
;sim.AddListener "Schedule", periodLogger, newSet(3)
;
;watch for instances of the period, logging output to console.
;Set reactiveLogger = New TimeStep_ObserverSimpleLog 'a logger that echoes the reactive periods.
;
;observeEvent oddPeriod, reactiveLogger 'logger will log oddPeriod's unique events.
;observeEvent evenPeriod, reactiveLogger 'logger will log evenPeriod's unique events.
;
;define scheduled periods.  These are ignorant of events, they are purely data.
;Set lessthan10 = namedPeriod("t <  10", periodTo(10))
;Set morethan10 = namedPeriod("t >= 10", periodFrom(10))
;
;define a simulation state.
;Set sim.state = newdict("currentperiod", vbNullString, _
;                        "previousperiod", vbNullString)
;
;schedule some times to advance our simulation.
;This system will likely change, we don't have to schedule
;time necessarily, but it's how the earlier system worked.
;Will be overhauled.
;
;For Each t In intList(20)
;    SimLib.addTime CSng(t), sim
;Next t
;
;a simple system for managing periods.
;While SimLib.hasTimeRemaining(sim)
;    t = SimLib.advanceTime(sim)
;    
;    'Manage our scheduled periods, changing state if necessary.
;    With getState(sim) 'examine the periods in the state
;        If PeriodLib.intersectsPeriod(CSng(t), lessthan10) Then
;            .item("currentperiod") = lessthan10.name
;        ElseIf PeriodLib.intersectsPeriod(CSng(t), morethan10) Then
;            .item("currentperiod") = morethan10.name
;        End If
;        
;        If .item("currentperiod") <> .item("previousperiod") Then
;            SimLib.triggerEvent 3, .item("previousperiod"), .item("currentperiod"), _
;                    "changed periods", .item("currentperiod"), sim
;            .item("previousperiod") = .item("currentperiod")
;        End If
;    End With
;
;    If (t Mod 2) > 0 Then
;        SimLib.triggerEvent 1, "flipper", "sim", "flip!", , sim
;    Else
;        SimLib.triggerEvent 2, "flipper", "sim", "flip!", , sim
;    End If
;Wend
;        
;End Sub







