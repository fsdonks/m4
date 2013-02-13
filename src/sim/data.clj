(ns sim.data)

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
(extend IEvent clojure.lang.PersistentVector
  sequential-event)
(extend IEvent clojure.lang.PersistentList
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

;An event structure with more complex data, used primarily for scheduling.
(defrecord packet [id type t from to content]
  IEvent 
  (event-type [e] type)
  (event-data [e] e))

(defn make-packet
  "Default constructor for making event packets.  packets are basic events with
   added contextual information (id, type, from, to, content). At a minimum, the 
   type of event must be supplied."
  ([type] (->packet 0 type nil nil nil))
  ([type content] (->packet 0 type nil nil content))
  ([type from to content] (packet. 0 type from to content)))

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
    (if (= (class pckt) sim.schedule.packet)
      (->event :timed-event pckt)
      (->event :timed-event 
         (make-packet :generic (assoc-in pckt [:contents :t] t))))))


;(defprotocol IEvent 
;  (event-id [e] "Return a (usually numeric) ID associated with event e.")
;  (event-type [e] "Return (usually string) event-type associated with event e.")
;  (event-time [e] "Report the timestamp for event e.")
;  (event-from [e] "Return the originating source of event e, usually numeric." )
;  (event-to [e] "Return the intended destination for event e, if any.")
;  (event-data [e] "Return (usually a map) of data associated with event e."))
;
;             
;(defrecord Event [id type t from to data]
;  IEvent 
;	  (event-id [e] id)
;    (event-type [e] type)
;    (event-time [e] t)
;	  (event-from [e] from)
;	  (event-to [e] to)
;    (event-data [e] data)) 

;;I haven't really used this yet, although for generic observables, I 
;;probably will.  So far, the swing stuff has the protocol extended to it...
;(def empty-event (Event. -1 :none -1 :anonymous :global nil))
;(defn make-event 
;  [id evtype & {:keys [t from to data] 
;                 :or {t 0 from :anonymous to :global data nil} 
;                 :as opts}]
;    (->event id evtype t from to data))