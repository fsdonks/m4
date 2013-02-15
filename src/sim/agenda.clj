(ns sim.agenda
  (:use [sim.schedule]))

;Defined a simple protocol for schedules.  The operations on schedules are 
;similar to Abelson and Sussman's agenda from Structure and Interpretation of 
;computer programs.  The difference is that we allow one to bound the time 
;horizon (in the case of a desire to truncate the processing of events on the 
;schedule).
(defprotocol IAgenda
  (advance-time [a] "Advance the agenda by one step.")
  (previous-time [a] "Return the time of the last step, if any.")
  (final-time [a] "Peek at the final time of the event on the schedule.")
  (set-final-time [a t] "Set an upper bound on the final time of the schedule.")
  (item-count [a] "Return the count of the items on the agenda.")
  (time-segments [a] "Return a map of agenda events, keyed by time segment.")
  (add-times [a ts] "Add a elements of time to the agenda.")
  (get-times [a] "Return a set of all times contained in the agenda.")

(defn feasible-time? [a t]
  (let [tf (final-time a)]
    (cond (or (= tf :inf) (nil? tf)) true
          (<= t tf) true 
          :else false)))
          
(defrecord agenda [tprev tfinal schedule itemcount times]
  IAgenda 
  (final-time [a] tfinal)
  (previous-time [a] tprev)
  (set-final-time [a tf] (agenda. tf schedule))
  (item-count [a] itemcount)
  (time-segements [a] schedule)
  (add-times [a ts] (reduce #(add-event %1 (->simple-event :time %2))  a ts))
  (get-times [a] times)
  IEventSeq 
  (add-event  [a e]  (if (feasible-time? (event-time e))
                       (agenda. tprev 
                                tfinal 
                                (add-event schedule e) 
                                (inc itemcount))
                       (conj times (event-time e))
                       a))
  (drop-event  [a]  (if (> 0 itemcount)
                      (agenda. (first-time schedule) 
                               tfinal 
                               (drop-event schedule) 
                               (dec itemcount))
                      (throw (Exception. "No items left in the agenda!"))))    
  (first-event [a] (first-event  schedule))
  (get-events  [a] (event-seq schedule)))

(defn add-time [a t] (add-times a #{t}))
(defn get-quarter [day] ((comp inc int) (/ day 90)))

(defn quarter [a] (get-quarter (current-time tdata))) 
(defn elapsed [a] (- (current-time a) (:trevious tdata)))

(defn still-time? [a] 
  (and (not= (item-count a) 0)
       (<= (next-time a) (final-time a))))

(defn has-time? [a t] (contains? (get-times a) t))
(defn add-time
  "Add at least one time event to the agenda for time t.  If an entry for t
   already exists, "
  [a t]
  (if (has-time? a t)
    a
    (-> (add-event a (->simple-event :time t))
      (add-times a #{t}))))
(defn advance-time [a] (drop-event a))

