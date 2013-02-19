(ns sim.agenda
  (:use [sim.data]))

;Defined a simple protocol for agendas.  The operations on schedules are 
;similar to Abelson and Sussman's agenda from Structure and Interpretation of 
;computer programs.  The difference is that we allow one to bound the time 
;horizon (in the case of a desire to truncate the processing of events on the 
;schedule).
(defprotocol IAgenda
  (previous-time [a] "Return the time of the last step, if any.")
  (final-time [a] "Peek at the final time of the event on the schedule.")
  (set-final-time [a t] "Set an upper bound on the final time of the schedule.")
  (agenda-count [a] "Return the count of the items on the agenda.")
  (time-segments [a] "Return a map of agenda events, keyed by time segment.")
  (add-times [a ts] "Add a elements of time to the agenda.")
  (get-times [a] "Return a set of all times contained in the agenda."))

(defn feasible-time? [a t]
  (let [tf (final-time a)]
    (cond (or (= tf :inf) (nil? tf)) true
          (<= t tf) true 
          :else false)))
          
(defrecord agenda [tprev tfinal schedule item-count times]
  IAgenda 
  (previous-time [a] tprev)
  (final-time [a] tfinal)
  (set-final-time [a tf]  (agenda. tprev tf schedule item-count times))
  (agenda-count [a] item-count)
  (time-segments [a] schedule)
  (add-times [a ts] (reduce #(add-event %1 (->simple-event :time %2))  a ts))
  (get-times [a] times)
  IEventSeq 
  (add-event  [a e] ;note->allowing the agenda to have events beyond tfinal  
    ;(if (feasible-time? (event-time e))
    (agenda. tprev tfinal (add-event schedule e) (inc item-count)
               (conj times (event-time e))))
               ;a) )
  (drop-event  [a]  
    (if (> item-count 0)  
       (let [tnext (current-time schedule)]
         (agenda. tnext tfinal (drop-event schedule) (dec item-count)
                  (if (not= tnext 
                            (event-time (next-event schedule)))
                    (disj times tnext)
                    times)))
       (throw (Exception. "No items left in the agenda!"))))    
  (first-event [a] (first-event schedule)))

(def empty-agenda (->agenda nil nil nil 0 #{}))

(defn add-time [a t] (add-times a #{t}))
(defn get-quarter [day] ((comp inc int) (/ day 90)))

(defn quarter [a] (get-quarter (current-time a))) 
(defn elapsed [a] (- (current-time a) (previous-time a)))

(defn still-time? [a] 
  (and (not= (agenda-count a) 0)
       (<= (next-time a) (final-time a))))

(defn has-time? [a t] (contains? (get-times a) t))
(defn add-time
  "Add at least one time event to the agenda for time t.  If an entry for t
   already exists, "
  [a t]
  (if (has-time? a t)
    a
    (add-times  a #{t})))
(defn advance-time [a] (drop-event a))

(comment ;testing 
(def simple-agenda (add-time empty-agenda 2))
)

