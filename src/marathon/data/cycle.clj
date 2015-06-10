;Provides a container for an invidual unit's cycle information.  Specifically, 
;the policy followed, the unit in the cycle, the component of the unit, the 
;accumulated state during the cycle (bog, mob, dwell, etc.), start and end 
;of the cycle, transitions in the cycle, and more...
(ns marathon.data.cycle
  (:use [spork.util.record :only [defrecord+ with-record]]))

(defrecord+ cyclerecord 
  [uic-name    ;Associated uic
   src        ;The unit-type, or template that identifies the unit's capability.
   component  ;Identifier for the operating component (active,reserve,etc.)  
   policyname ;Policy the unit entity follows
   tstart     ;The start time of the unit's current cycle.
   tfinal     ;The expected end time of the unit's current cycle.
   [duration 0]   ;The time the entity has spent in cycle.   
   [available-time 0]  ;Time the entity spent in available pool
   [deployableTime 0] ;Time the entity spent in a deployable state.
   duration-expected  ;The expected cycle length.
   [bog 0]         ;The cumulative BOG experienced by the entity. (units of time, days)
   [bogbudget 0]   ;The remaining BOG a unit can expend in the current cycle.
   bog-expected ;The expected BOG days for the unit in the current cycle. 
   [dwell 0]       ;The cumulative Dwell days for the current cycle.
   dwell-expected ;The expected Dwell for the current cycle.
   mob-expected  ;mobilization time expected, if any, for the current cycle.
   [mob 0]          ;Accumulated mobilization days.
   [deployments 0]  ;count of deployments in the current cycle.
   [followons   0]  ;count of follow-on deployments for the current cycle.
   bog-to-dwell-expected  ;expected BOG/Dwell ratio for the current cycle.
   traversals]) ;record of state traversal, for the current cycle.

;Note -> this may be a little vestigial, or easily revamped; for instance, 
;we can provide uic, src, and component and we're fine...
(defn ^cyclerecord cycle-NewCycle
  "Creates a new cycle, at time t, defined by the bogtime, dwelltime, etc. 
   characteristics, patterned off of cycle c, which provides the name of the 
   uic, the src, and the component."
  [cyclerec t bogtime dwelltime policyduration & [MOBtime  ghost  bogbudget]]
    (with-record cyclerec
      :bog-expected  bogtime
      :bogbudget (if (zero? bogbudget) bogtime bogbudget)
      :bog-to-dwell-expected  (/ 1 (/ (+ MOBtime bogbudget)  
                                      (-  policyduration (+ bogbudget  MOBtime))))
      :duration-expected  policyduration
      :dwell-expected  dwelltime
      :mob-expected  MOBtime
      :tstart t
      :tfinal (+ t policyduration)))

(defn ^cyclerecord cycle-modify 
  "Modifies oldcycle, assumably in the context of a policy change.  Returns the 
   result of the modification, as a new cycle."
  [cyclerec bogtime dwelltime policyduration & [MOBtime bogbudget]]
  (if (and (> dwelltime 1095)  (zero? MOBtime) (not (= :inf dwelltime))
           (throw (Exception. "Expected dwell time is abnormal...")))     
    (with-record cyclerec
      :bog-expected  bogtime
      :bog-to-dwell-expected (/ 1 (/ (+ bogtime MOBtime) 
                           ( - policyduration (+ bogtime  MOBtime))))
      :duration-expected policyduration
      :dwell-expected  dwelltime
      :mob-expected = MOBtime
      :bogbudget = bogbudget)))

(defn ^cyclerecord cycle-add-traversal [cyclerec t startlocation endlocation]
  (let [trav  [t startlocation endlocation]
        ts (get :traversals cyclerec [])] 
    (with-record cyclerec 
      :traversals (conj ts trav))))

(defn bog-to-dwell
  "Computes the BOG:Dwell ratio for a cycle."
  [bog mob dwell available-time mob-expected bog-expected duration-expected 
   & [conventional]]  
  (let [res 
        (cond (and  (> bog  0) (> dwell 0))
              (/ (+ bog mob)  dwell)
              (and (> dwell 0) 
                   (> available-time 0))
              (/ available-time dwell)
              :else 
              (/ (+ mob-expected  bog-expected) (- duration-expected 
                                                 (+ mob-expected bog-expected))))]
    (if conventional 
      (/ 1 res) 
      res)))

(defn cycle-bog-to-dwell
  "Computes the BOG:Dwell ratio from a cyclerecord."
  [cyclerec & [conventional]]
  (let [{:keys [bog mob dwell available-time 
                mob-expected bog-expected duration-expected]} cyclerec]
    (bog-to-dwell bog mob dwell available-time 
         mob-expected bog-expected duration-expected conventional)))
  
