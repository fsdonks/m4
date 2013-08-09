;;DEPRECATED

(ns marathon.time
"As the name implies, this object is responsible for managing time in the 
simulation.  The current approach to time management caches a series of future 
updates into a dictionary.  These are basically eventful days.  As the 
simulation proceeds, events will be requested to happen on specific days, many
times in the future.  The time manager serves as a centralized clearing-house 
for these requests, and avoids duplication of update requests.  The net effect 
is that the \"system\" updates itself only on unique days that have been 
scheduled.  The time manager also provides a central source for determining the
current time, and for determining a final time."
  (:use [CBA.component]))


(def inf (java.lang.Double/POSITIVE_INFINITY))

(defrecord timedata [tcurrent tprevious tfinal tplanned])
(defn timedata-component 
  ([tcurrent tfinal] [:time (timedata. tcurrent tcurrent tfinal 
                                       (sorted-set tcurrent tfinal))])
  ([] (timedata-component 0 0)))

(defn- get-quarter [day] ((comp inc int) (/ day 90)))

(defn next-time [tdata] (first (:tplanned tdata)))
(defn quarter [tdata] (get-quarter (:tcurrent tdata))) 
(defn elapsed [tdata] (- (:tcurrent tdata) (:tprevious tdata)))
(defn add-time [tdata t] (merge tdata {:tplanned (conj (:tplanned tdata) t)}))

(defn advance-time [tdata] 
  (let [tnext (next-time tdata)
        remaining (disj (:tplanned tdata) tnext)]
    (merge tdata {:tplanned remaining} {:tcurrent tnext} 
                 {:tprevious (:tcurrent tdata)})))


(defn still-time? [tdata] (and (not= (count (:tplanned tdata)) 0)
                               (<= (next-time tdata) (:tfinal tdata))))

(def base-timer (make-entity "SimulationClock" [(timedata-component)]))
(defn make-timer [] base-timer)


