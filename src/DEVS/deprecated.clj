(ns DEVS.deprecated)

(comment 

;Event handlers are simple.  They look at the next event in the context and 
;compute a response.  This is similar to a parser combinator.  The parser can
;choose to parse a character (parsing one or more characters in the process), 
;or it can return nil/nothing (a failed parse).  In our case, we prefer to 
;return the original context, rather than nothing.  The difference here is that 
;we're possibly consuming events, and Adding events for additional parsing or 
;processing.  Really, all the handler has to do is match the nextevent against 
;some predicate.  If predicate, dispatch on (nextevent events) to determine the
;next context.  

  
(defmacro defhandler 
  "defines a function that takes an event context, and the supplied args, 
   evaluating the body inside of an implicit with-context, in which the 
   state and events of the context are bound and accessible inside of body."
  ([nm args body]
  `(defn ~nm 
     ([~@(concat (list 'context) args)] (with-context ~'context  ~body))
     ([] 
       (throw 
         (Exception. "handler needs at least a {:state _ :events _ }")))))
  ([nm body] `(defhandler ~nm [] ~body)))

;(defhandler timescheduler [t] (->eventcontext state (conj events :time)))



(defn remove-event
  "Given a collection of events, returns the event collection with the next
   event removed, as well as the event that was removed."
  [ecoll]
  [(drop-event ecoll) (next-event ecoll)])

(defn advance-event
  "Advance an event-driven process, by returning the result of freeing the
   next event from the event collection. "
  [state ecoll]
 
  [[state (drop-event ecoll)] (next-event ecoll)])

(defn filter-event
  "Determines if the next event meets the criteria defined by predicate."
  [pred es] (pred (next-event es)))

(defn handle-when
  "Handles the next event on the event-stream, by applying f to [state es] when
   the (pred (nextevent es)) is true, else returns the state untouched.
   f::(a,b)->(a,b)"
  [pred f [state es]]
  (if (pred (next-event es))
    (f [state es])
    [state es]))

(defn map-h 
  [f] 
  (fn [ec] (f ec)))

(defn state-h
  "Returns a handler that maps f onto an event context's state."
  [f]
  (map-h (fn [ec] (set-state ec (f (get-state ec)))))) 

(defn event-h 
  "Returns a handler that maps f onto an event context's state."  
  [f] 
  (map-h (fn [ec] (set-events ec (f (get-events ec))))))


)