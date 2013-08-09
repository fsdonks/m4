;A module to capture functions that operate on periodic data.
;Periods are defined as having a start and and end time.
;We can test for intersection over a period.
(ns marathon.data.period)

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

(def inf-error 
  (Exception. "Only numbers, or :inf and :inf-negative are valid keys"))

(defn parse-inf [t] 
  (cond (keyword? t) 
    (case t 
      :inf  java.lang.Long/MAX_VALUE
      :inf-negative java.lang.Long/MIN_VALUE
      inf-error)
    (number? t) t
    :else inf-error))

(defn ->period [name tstart tfinal & rest]
  {:name name :from-day tstart :to-day tfinal})

(def empty-period (->period nil nil nil))
;Simple period constructor and modifier.  If p is provided, modifies the period name, else
;returns a new period with the proper name.
(defn named-period [name & [p]] (-> (or p empty-period) (assoc :name name))) 

;General period constructor.  Defines a period [tstart...tfinal], where tstart <= tfinal
(defn period-across [tstart tfinal]
  (assert (<= (parse-inf tstart) (parse-inf tfinal)) 
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
(defn intersects-period? [t p] (intersect-1d (:from-day p) (:to-day p)))

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
(defn make-temporal-period [& [start-day end-day period-name & rest]]
  (let [name (or period-name :Default)] (->period name start-day end-day)))

;Public Function toString(p As GenericPeriod) As String
;toString = p.name & "'[" & p.from-day & ":" & p.to-day & "]'"
;End Function
;


