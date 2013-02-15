(ns sim.data
  (:use [util.datetime]))

(defprotocol IEvent
  (event-type [e] 
   "Given a chunk of data, tries to coerce its event-type.  Keyword values are  
   viewed as an intrinsic event type.  Maps (and records) dispatch off a :type 
   key.  Any other generic sequences dispatch off the first value.")
  (event-data [e]   
  "Extracts event data from e.  Handles maps/records, sequences, and keywords.
   Keywords are assumed to have no data.  Sequences are assumed to have data 
   as the second element of the sequence.")
  (event-id [e] "Return a (usually numeric) ID associated with event e.")
  (event-time [e] "Report the timestamp for event e.")
  (event-from [e] "Return the originating source of event e, usually numeric." )
  (event-to [e] "Return the intended destination for event e, if any."))

(def sequential-event 
  {:event-type (fn [e] (nth e 0 nil))
   :event-data (fn [e] (nth e 1 nil))
   :event-id   (fn [e] (nth e 2 nil))
   :event-time (fn [e] (nth e 3 nil))
   :event-from (fn [e] (nth e 4 nil))
   :event-to   (fn [e] (nth e 5 nil))})

;basic implementations for core structures.
(extend clojure.lang.PersistentVector
  IEvent 
  sequential-event)
(extend clojure.lang.PersistentList
  IEvent
  sequential-event)
(extend-protocol IEvent 
  nil
  (event-type [e] nil)
  (event-data [e] nil)
  (event-id [e] nil)
  (event-time [e] nil)
  (event-from [e] nil)
  (event-to [e] nil)
  clojure.lang.PersistentArrayMap
  (event-type [e] (:type e))
  (event-data [e] (:data e))
  (event-id [e]   (:id e)) 
  (event-time [e] (:time e))
  (event-from [e] (:from e) )
  (event-to [e] (:to e))
  clojure.lang.Keyword ;keywords are events...
  (event-type [e] e)
  (event-data [e] e)
  (event-id [e]   e) 
  (event-time [e] nil)
  (event-from [e] nil)
  (event-to [e] nil))

(defn ->simple-event
  "Constructs a simple map-based event."
  ([type t] {:time t :type type})
  ([type t data] {:time t :type type :data data}))

;An event structure with more complex data, used primarily for scheduling.
(defrecord event [type data id t from to]
  IEvent 
  (event-type [e] type)
  (event-data [e] data)
  (event-id   [e] id)
  (event-time [e] t)
  (event-from [e] from)
  (event-to   [e] to))

(defn time-event
  "Default constructors for building temporal events; really just a nice
   way of representing time-stamped packets.  The contextual information is
   contained in the packet (another record).  We use the time-stamp on events
   to keep them sorted in a schedule.  The time stamp is somewhat arbitrary,
   in that we could use any comparable key for comparison.  However, we tend
   to denote an absolute notion of time or ordering, hence the convention of
   using a float as our :time key."
  ([t] (->simple-event :time t))
  ([t evt]
    (if (= (class evt) event)
      (merge {:t t} evt)
      (->simple-event :generic t evt))))

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
  (print-method (seq q) w))

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
  ([s e] (let [t (or (event-time e) (get-time s))
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

(defn zip-events [ts packets] 
  "Zip a sequence of times and packets to produce a sequence of events."
  (map #(apply time-event %)(seq (zipmap ts packets))))

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

;Don't know if this is needed...

;protocol for operating on abstract event collections.
(defprotocol IEventSeq
  (add-event [ecoll e] "add an event to the collection of events")
  (drop-event [ecoll]  "remove an event from the collection of events")
  (first-event [ecoll] "return the next event in the collection")
  (get-events [ecoll]  "return a collection of pending events"))

(extend-protocol IEventSeq
  clojure.lang.PersistentVector
  (add-event [v e] (conj v e))
  (drop-event [v] (cond (or (= 1 (count v)) 
                            (= v [])) []
                        (> (count v) 1) (subvec v 1)))                       
  (first-event [v] (first v))
  (pending-events [v] v)
  
  clojure.lang.PersistentTreeMap 
  (add-event [m e] (put-event m e)) 
  (drop-event [m] (take-event m))                       
  (first-event [m] (next-event  m))
  (pending-events [m] (event-stream m)))

;;protocol-derived functionality
(defn add-events
  "Add multiple events to the event sequence."
  [ecoll es] 
  (reduce add-event ecoll es)) 
(defn next-time
  "Compute the time of the next event in the sequence."
  [ecoll] (get-time (first-event (drop-event ecoll))))
(defn event-seq [ecoll]
  "Return a lazy seq of events by unfolding the schedule using 
   event-streamf."
   (map first 
        (iterate (fn [[x xs]] (when xs [(next-event xs) (drop-event xs)]))
                 [(first-event ecoll) (drop-event ecoll)]))

(comment 
;;Testing.....

;;schedule events
(defn random-schedule [n tmax]
  "Define a simple random schedule, with up to 10 events for n days randomly 
   spread across time [0.0 tmax]"
    (->> 
      (repeatedly n #(rand-int tmax)) 
      (map #(for [i (map inc (range (rand-int 10)))] 
              (time-event %1  (->simple-event :timestamp i))))
      (flatten)
      (put-events empty-schedule)))

(def sample-schedule 
  (let [wednesdays (take 6 (map date->num (daystream "Wednesday")))
        chores (repeat ["wake-up" "take out trash" "eat cereal" "pet cat" 
                        "work"])]
    (->schedule (zip-events wednesdays chores))))  

(defn bullet-list [coll] (map #(str "->" % \newline) coll))
(def easy-schedule (->schedule (take 10 (repeat :multiply))))
)



