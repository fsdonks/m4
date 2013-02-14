(ns sim.agenda
  (:use [sim.schedule]))

;Defined a simple protocol for schedules.  The operations on schedules are 
;similar to Abelson and Sussman's agenda from Structure and Interpretation of 
;computer programs.  The difference is that we allow one to bound the time 
;horizon (in the case of a desire to truncate the processing of events on the 
;schedule).
(defprotocol IAgenda 
  (current-time [a] "Return the current time of the schedule.")
  (final-time [a] "Peek at the final time of the event on the schedule.")
  (scheduled-events [a] "Return a map of event queues in temporal order.")
  (push-event [a e] "Push an event onto the schedule, if <= tfinal.")
  (pop-event [a] "Advance the schedule by removing the next event.")
  (set-final-time [a t] "Set an upper bound on the final time of the schedule.")
  (set-current-time [a t] "Advance the schedule to a new time."))

(defrecord agenda   [t tfinal schedule]
  IAgenda 
  (current-time [a] t)
  (final-time [a] tfinal)
  (scheduled-events [a] schedule)
  (push-event [a e] (if (or (nil? tfinal) (<= (event-time e) tfinal)) 
                      (agenda. t tfinal (put-event events e))))
  (pop-event [a] (agenda. (get-time schedule) tfinal (take-event schedule)))
  (set-final-time [a tf] (agenda. t tf schedule))
  (set-current-time [a tc] (agenda. tc tinfal schedule)))


(defn schedule-component 
  ([tcurrent tfinal] [:schedule (->schedule tcurrent tcurrent tfinal 
                                       (sorted-set tcurrent tfinal))])
  ([] (timedata-component 0 0)))

(defn- get-quarter [day] ((comp inc int) (/ day 90)))

(defn next-time [tdata] (first (:tplanned tdata)))
(defn quarter [tdata] (get-quarter (:tcurrent tdata))) 
(defn elapsed [tdata] (- (:tcurrent tdata) (:tprevious tdata)))
(defn add-time [tdata t] (merge tdata {:tplanned (conj (:tplanned tdata) t)}))

(defn advance-time [sched] 
  (let [tnext (next-time sched)
        remaining (disj (:tplanned tdata) tnext)]
    (merge tdata {:tplanned remaining} {:tcurrent tnext} 
                 {:tprevious (:tcurrent tdata)})))


(defn still-time? [tdata] 
  (and (not= (count (:scheduled tdata)) 0)
       (<= (next-time tdata) (:tfinal tdata))))

