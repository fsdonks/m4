;;A simple discrete event simulation framework, with all sorts of goodies...
;;This is intended to be a barebones implementation of the event engine that
;;currently rests under the VBA version....however, we should be able to use it
;;elsewhere, i.e. it's general.
(ns sim.schedule
  (:use [util.datetime]
        [sim.data]
        [sim.events :only [->event]]))

;;A sim engine is quite simple...I will adapt this from the F# version.
;;We basically just need a priority q.
;;Events are queued in priority order according to time t (some single floating
;;point val).
;;This implemention will be similar to the Agenda from SICP.
;;We have multiple events associated with time, however...
;;I want to use sorted-map to keep this straight.  It has logarithmic
;;performance....
;;Another idea is to have a sorted-map based on float keys (time), whose values
;;are queues.  Could use vectors to add to the queue.
;;Allows for insertion order.
;;Should be pretty fast...
;;Dunno.

(def emptyq clojure.lang.PersistentQueue/EMPTY)
(defmethod print-method clojure.lang.PersistentQueue [q,w]
  (print-method (quote <Q|) w) (print-method (seq q) w) 
  (print-method (quote <|EndQ) w))


(defn zip-events [ts packets] 
  "Zip a sequence of times and packets to produce a sequence of events."
  (map #(apply timed-event %)(seq (zipmap ts packets))))

;define a schedule, a sorted-map of event queues keyed by time.
(def empty-schedule (sorted-map 0.0 emptyq))

(defn empty-schedule? [s]
  "If schedule s has one eventqueue, and it's emptyq, the schedule is empty"
  (and
    (= (count (vals s)) 1)
    (= (first (vals s)) emptyq)))

(defn get-events
  "[s] retrieve next queue of events from schedule s
   [s t] retrieve queue of events for time t from schedule s"
  ([s] (second (first s)))
  ([s t] (cond (contains? s t) (get s t)
               :else emptyq)))

(defn active?
  "Determine if the schedule has any events, or if [s t] if events exist for a
   specific time"
  ([s] (not (= (get-events s) emptyq)))
  ([s t] (and (contains? s t) (not (= (get-events s t) emptyq)))))
    

(defn get-time [s]
  "[s] Determine the :time of the pending event in the schedule, if no events
   are pending (an empty schedule) we return nil"
  (first (keys s)))

(defn next-active [s]
  "Clear empty queues.  If our current queue is no longer active (has no events)
   we remove it from consideration iff there are more pending queues."
  (if (or (active? s) (empty-schedule? s))
    s
    (recur (dissoc s (get-time s)))))

(defn next-time [s]
  "[s] Get the next active time"
  (get-time (next-active s)))

(defn next-event
  "[s] retrieve first event in next active queue of events from schedule s
   [s t] retrieve first event in next active queue of events for time t from s"
  ([s] (peek (get-events (next-active s))))
  ([s t] (peek (get-events (next-active s) t)))) 

(defn put-event
  "[s e & es] insert one or more events into a schedule, based on event time.
   We assert that added events must be futures...i.e. their time must be >=
   the current time.  Right now, invalid events just aren't added...
   Note that we're checking each added event, using calls to get-time, this
   could be done once and passed for performance sake."
  ([s e] (let [t       (->> e (:content :t ))
               q (get-events s t)]
           (if (>= t (get-time s))
             (assoc s t (conj q e))
             s)))
  ([s e & es] (if es
                (recur (put-event s e) (first es) (rest es))
                (put-event s e))))


(defn put-events [s es] 
  "Insert multiple events (es) into the schedule."
  (reduce #(put-event %1 %2) s es))


(defn ->schedule 
  "Unified constructor for building schedules.  No args invokes a reference to 
   the empty-schedule.  Providing a sequence of events inserts them into an 
   empty schedule.  Providing a sequence of times and packets zips and inserts 
   them into an empty schedule."
  ([] empty-schedule)
  ([evts] (put-events empty-schedule evts))
  ([ts packets] (put-events empty-schedule (zip-events ts packets)))) 
                            

(defn take-event [s]
  "[s] Remove the next event from schedule s, returning remaining schedule."
  (let [snext (next-active s)]
    (next-active
      (assoc snext (get-time snext) (pop (get-events snext))))))

;;for all active days, return a sequence of events.
;;I am going to intern this function, it's just an auxillary...
(defn- event-streamf [s]
  (if (empty-schedule? s)
    []
    [(next-event s) , (take-event s)]))

(defn event-stream [s]
  "Return a lazy seq of events by unfolding the schedule using 
   event-streamf."
    (let [[evt, snext] (event-streamf s)]
      (if snext
        (cons evt (lazy-seq (event-stream snext))))))

(defn do-schedule 
  ([s f n] (doseq [evt (take n (event-stream s))] (f evt)))
  ([s f] (doseq [evt (event-stream s)] (f evt))))


(defn print-schedule 
  "print the first n items of the schedule, produces a lazy seq...
   Planning to provide a pretty-printer for events."
  ([s] (do-schedule s println)) 
  ([s n] (do-schedule s println n)))  



(comment 
;;Testing.....

;;schedule events
(defn random-schedule [n tmax]
  "Define a simple random schedule, with up to 10 events for n days randomly 
   spread across time [0.0 tmax]"
    (->> 
      (repeatedly n #(rand tmax)) 
      (map #(for 
              [i (map inc (range (rand-int 10)))] 
              (timed-event %1 
                           (make-packet :timestamp i))))
      (flatten)
      (put-events empty-schedule)))

(def sample-schedule 
  (let [wednesdays (take 6 (map date->num (daystream "Wednesday")))
        chores (repeat ["wake-up" "take out trash" "eat cereal" "pet cat" 
                        "work"])]
    (make-schedule (zip-events wednesdays chores))))  



(defn bullet-list [coll] (map #(str "->" % \newline) coll))
)

                    
