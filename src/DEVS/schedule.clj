;;A simple discrete event simulation framework, with all sorts of goodies...
;;This is intended to be a barebones implementation of the event engine that
;;currently rests under the VBA version....however, we should be able to use it
;;elsewhere, i.e. it's general.
(ns DEVS.schedule
  (:use [util.datetime]
        [DEVS.events :only [->event]]))

;;A DEVS engine is quite simple...I will adapt this from the F# version.
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

;Revised 28 May 2012
;Events are covered in DEVS.events 

;;Note -> events are simple kvps of {:t timestamp :type someeventyype :data
;;somedata}
;;probably want to define events explicitly somewhere else..

;(defrecord Event [t packet]) ;I don't think we need this anymore....
;schedule can serve as an interface to event....
;we store time data in the event data.
;schedule satisfies IEventSeq, and orders its events by time. 
;In the case where there is no time in event-data, time is :immediate. 
;we use a custom comparison function to ensure that :immediate events are 
;always ahead of ANY event scheduled with an actual time value.  


;packets are still useful.  they basically wrap extra data for us.
;when we build an event, we append some context to it.

;Packets are just standard information we'd like to have bindings for when 
;handling events.  They're subordinate to generic event structures. 

(defrecord Packet [id eventtype from to data])
(defn make-packet
  "Default constructors for making packets.  Packets are the contextual
   information associated with an event.
   At a minimum, the type of event must be supplied.
   We can also supply an event type and some data (data is any data type).
   Finally, we can append from and to information about the event.
   Note, the id field is never set by the user, but the schedule uses it
   internally and provides it as a return value"
  ([eventtype] (Packet. 0 eventtype nil nil nil))
  ([eventtype data] (Packet. 0 eventtype nil nil data))
  ([eventtype from to data] (Packet. 0 eventtype from to data)))


(defn timed-event
  "Default constructors for building temporal events; really just a nice
   way of representing time-stamped packets.  The contextual information is
   contained in the packet (another record).  We use the time-stamp on events
   to keep them sorted in a schedule.  The time stamp is somewhat arbitrary,
   in that we could use any comparable key for comparison.  However, we tend
   to denote an absolute notion of time or ordering, hence the convention of
   using a float as our :time key."
  ([t] (->event :timed-event (make-packet :generic {:t t})))
  ([t pckt]
    (if (= (class pckt) DEVS.schedule.Packet)
      (->event :timed-event pckt)
      (->event :timed-event 
         (make-packet :generic (assoc-in pckt [:data :t] t))))))

(defn zip-events [ts packets] 
  "Zip a sequence of times and packets to produce a sequence of events."
  (map #(apply timed-event %)(seq (zipmap ts packets))))

;define a schedule, a sorted-map of event queues keyed by time.
(def initial-schedule (sorted-map 0.0 emptyq))

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
  ([s e] (let [t       (->> e (:data :t ))
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


(defn make-schedule 
  "Unified constructor for building schedules.  No args invokes a reference to 
   the empty-schedule.  Providing a sequence of events inserts them into an 
   empty schedule.  Providing a sequence of times and packets zips and inserts 
   them into an empty schedule."
  ([] initial-schedule)
  ([evts] (put-events initial-schedule evts))
  ([ts packets] (put-events initial-schedule (zip-events ts packets)))) 
                            

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
      (put-events initial-schedule)))

(def sample-schedule 
  (let [wednesdays (take 6 (map date->num (daystream "Wednesday")))
        chores (repeat ["wake-up" "take out trash" "eat cereal" "pet cat" 
                        "work"])]
    (make-schedule (zip-events wednesdays chores))))  



(defn bullet-list [coll] (map #(str "->" % \newline) coll))
)

                    

