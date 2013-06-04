(ns marathon.data.period)
;A module to capture functions that operate on periodic data.
;Periods are defined as having a start and and end time.
;We can test for intersection over a period.

;->Re: the below discussion on a declarative specification for defining 
;    composable periods....We can simple bifurcate periods into two classes:
;   Temporal and Reactive.
;       Temporal are the current case; generally that periods are known in advance.
;       The beginning and end of the period is known apriori.
;   Reactive
;       Reactive periods happen in response to observed events.
;       The start of a reactive period is an oberserved event.
;       The end of a reactive period is also an observed event.
;       Thus, reactive periods are functions on oberservables.
;   Seems like the simplest way to do this.
;       Develop a little language for describing reactive events.
;       StartOn: entityX spawns.
;       StopOn:  DemandY deactivates.
;   Very similar to reactive GUI stuff I've already ported....
;   Periodicity is implicit.
;   This gives us a simple notion of timelines...
;       Scheduled/Known......
;       Reactive/UnKnown....
;   A timeline then, is a chunk of data, that can be queried via a function
;   to determine which period(s) are active at time t.
;   A timeline can be composed of scheduled and unscheduled periods.
;   A reactive timeline implies some side-effecting notion.
;       An observer listening for events.
;       The observer can also be causing events, effectively I/O
;Using the timeline abstraction, we can compose period-generating functions.
;   A GenericPeriod can be seen as a period generating function, of the known variety.
;   A ReactivePeriod can be seen a a period generating function, of the unknown variety.
;Constructing ReactivePeriods requires some IO....
;   holy shit, allows for User I/O (duh).

(defn ->period [name fromday today & rest]
  {:name name :fromday tstart :today tfinal})
(def empty-period (->period nil nil nil))
;Simple period constructor and modifier.  If p is provided, modifies the period name, else
;returns a new period with the proper name.
(defn named-period [name & [p]] (-> (or p empty-period) (assoc :name name))) 
;General period constructor.  Defines a period [tstart...tfinal], where tstart <= tfinal
(defn period-across [tstart tfinal] 
  (assert (<= tstart tfinal) 
          (str "Periods must have final is >= tstart " [tstart tfinal]))
  (->period nil tstart tfinal))
;Defines a period whose only valid intersection value is tstart, [tstart..tstart]
;Used to define instantaneous phenomena...
(defn period-at [tstart] (period-across tstart tstart))
;Defines a negatively unbounded period over [-inf...tfinal]
(defn period-to [tfinal] (period-across :inf-negative tfinal))
;Defines a positively unbounded period over [tstart...inf]
(defn period-from [tstart] (period-across tstart :inf))
;Defines a period that cannot ever intersect with any value.
(def period-undefined (period-across :inf :inf))
;Defines a period that intersects every value of t [-inf...inf]
(def period-infinite (period-across :inf-negative :inf))
(defn period->list [p] (vals p))
(defn list->period [xs] (apply ->period xs)) 
;Simple 1 dimensional intersection function.  Allows for infinite values in 
;either direction, i.e. [tstart...inf], or [-inf...tfinal] are valid.  
;Returns false if t is inf.
(defn intersect-1d [t tstart tfinal] 
  (cond (= t :inf) false 
        (and (= tstart :inf-negative) (= tfinal :inf)) true
        :else (and (>= t tstart) (<= t tfinal))))            
;Determines if time t intersects the period p
(defn intersects-period? [t p] (intersect-1d (:fromday p) (:today p)))

;simple function for testing in other modules.
;Public Function testPeriods() As Collection
;Set testPeriods = list(namedPeriod("Initialization", periodTo(1)), _
;                       namedPeriod("PreSurge", periodAcross(1, 200)), _
;                       namedPeriod("Surge", periodAcross(200, 500)), _
;                       namedPeriod("PostSurge", periodFrom(500)))
;
;End Function
;Public Function tableToPeriods(tbl As GenericTable) As Collection
;Dim rec As GenericRecord
;
;Set tableToPeriods = New Collection
;
;While Not tbl.EOF
;    Set rec = tbl.getGenericRecord
;    tableToPeriods.add recordToPeriod(rec)
;    tbl.moveNext
;Wend
;
;End Function

;(Type    Name    FromDay ToDay)
(defn record->period [r]
  (->> (named-period (:Name r))
       (period-across (parse-inf (:FromDay r)) (parse-inf (:ToDay r)))))
;----------OBSOLETE--------?
(defn make-temporal-period [& start-day end-day period-name]
  (let [name (or period-name :Default)] (->period name start-day end-day)))

;Public Function toString(p As GenericPeriod) As String
;toString = p.name & "'[" & p.fromday & ":" & p.today & "]'"
;End Function
;

;--------------DEFERRED--------
;---IMPLEMENT LATER-----------
;Defines a period that exists over (potentially) multiple times.....
;Public Function periodWhen(startevent As Long, stopevent As Long, Optional eventsource As GenericObserver, Optional name As String) As GenericPeriodReactive
;Set periodWhen = New GenericPeriodReactive
;
;renames the period from default.
;If name <> vbNullString Then
;    periodWhen.name = name
;ElseIf periodWhen.name <> vbNullString Then
;    name = periodWhen.name
;Else
;    If exists(eventsource) Then name = randName("periodWhen", eventsource.clients)
;End If
;
;periodWhen.startevent = startevent
;periodWhen.stopevent = stopevent
;
;If exists(eventsource) Then
;    eventsource.Register name, periodWhen, startevent
;    If startevent <> stopevent Then eventsource.Register name, periodWhen, stopevent
;End If
;   
;End Function
;Defines a period that exists over every triggering of a single event, a very simple reactive period.
;Public Function periodEvery(startevent As Long, Optional eventsource As GenericObserver, _
;                                Optional name As String) As GenericPeriodReactive
;Set periodEvery = periodWhen(startevent, startevent, eventsource, name)
;End Function
;
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

;-------------OBSOLETE?--------------------------------------------------------
;Public Function makeReactivePeriod(startEvent As Long, stopEvent As Long, observer As GenericObserver, periodName As String) As IPeriod
;
;End Function
;19 May 2011 Tom Note -> This should be refactored.  Too coupled to Excel.
;Public Function sheetToPeriods(periodsheet As Worksheet, p As TimeStep_StorePolicy, _
;                                    context As timestep_SimContext) As Collection
;
;Dim j As Long
;Dim record()
;Dim source As range
;Dim rw As range
;Dim fields As Dictionary
;Dim per As GenericPeriod
;Set fields = New Dictionary
;
;Set source = periodsheet.Cells(1, 1)
;Set source = source.CurrentRegion
;Set rw = source.rows(1)
;record = rw.value
;For j = 1 To UBound(record, 2)
;    fields.add CStr(record(1, j)), j
;Next j
;
;Set periods = New Dictionary
;Set sheetToPeriods = New Collection
;
;Set source = source.offset(1, 0)
;Set source = source.resize(source.rows.count - 1, source.Columns.count)
;For Each rw In source.rows
;    record = rw.value
;    Set per = New GenericPeriod
;    With per
;        .name = record(1, fields("Name"))
;        If record(1, fields("FromDay")) = "inf" Then
;            'Decouple
;            .fromday = inf
;        Else
;            .fromday = record(1, fields("FromDay"))
;        End If
;
;        If record(1, fields("ToDay")) = "inf" Then
;            'Decouple
;            .today = inf
;        Else
;            .today = record(1, fields("ToDay"))
;        End If
;        'Replaced.
;        
;
;    End With
;    sheetToPeriods.add per
;Next rw
;
;End Function
