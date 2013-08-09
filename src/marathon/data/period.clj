;A module to capture functions that operate on periodic data.
;Periods are defined as having a start and and end time.
;We can test for intersection over a period.
(ns marathon.data.period)

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

