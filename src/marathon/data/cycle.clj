(ns marathon.data.cycle
  (:use [util.record :only [defrecord+ with-record]]))

;cyclerecord
;Provides a container for an invidual unit's cycle information.  Specifically, 
;the policy followed, the unit in the cycle, the component of the unit, the 
;accumulated state during the cycle (bog, mob, dwell, etc.), start and end 
;of the cycle, transitions in the cycle, and more...
(defrecord+ cyclerecord 
  [UICname    ;As String ;;Associated uic
   src        ;As String
   component  ;As String   
   policyname ;As String ;;Associated policy
   tstart     ;As Single ;cycle start
   tfinal     ;As Single ;expected cycle end
   duration   ;As Single ;actual cycle length   
   availableTime  ;As Single ;time spent in available pool
   deployableTime ;As Single ;time spent deployable.
   DurationExpected  ;As Single ;expected cycle length
   bog         ;As Single ;experienced BOG (units of time, days)
   bogbudget   ;As Single
   BOGExpected ;As Single ;expected BOG (units of time, days)
   dwell       ;As Single ;expected Dwell (units of time, days)
   DwellExpected ;As Single ;experienced dwell
   MOBexpected   ;As Single ;mobilization time expected, if any.
   mob           ;As Single
   deployments   ;As Long ;count of deployments
   followons     ;As Long ;count of follow-on deployments
   BDRExpected   ;As Single ;expected BOG/Dwell ratio
   Traversals]) ;As Collection ;record of state traversal, good for verification

;Note -> this may be a little vestigial, or easily revamped; for instance, 
;we can provide uic, src, and component and we're fine...
(defn ^cyclerecord cycle-NewCycle
  "Creates a new cycle, at time t, defined by the bogtime, dwelltime, etc. 
   characteristics, patterned off of cycle c, which provides the name of the 
   uic, the src, and the component."
  [cyclerec t bogtime dwelltime policyduration & [MOBtime  ghost  bogbudget]]
    (with-record cyclerec
      :BOGExpected  bogtime
      :bogbudget (if (zero? bogbudget) bogtime bogbudget)
      :BDRExpected =  (/ 1 (/ (+ MOBtime bogbudget)  
                              (-  policyduration (+ bogbudget  MOBtime))))
      :DurationExpected  policyduration
      :DwellExpected  dwelltime
      :MOBexpected  MOBtime
      :tstart t
      :tfinal (+ t policyduration)))

(defn ^cyclerecord cycle-modify 
  "Modifies oldcycle, assumably in the context of a policy change.  Returns the 
   result of the modification, as a new cycle."
  [cyclerec bogtime dwelltime policyduration & [MOBtime bogbudget]]
  (if (and (> dwelltime 1095)  (zero? MOBtime) (not (= :inf dwelltime))
           (throw (Exception. "Expected dwell time is abnormal...")))     
    (with-record cyclerec
      :BOGExpected  bogtime
      :BDRExpected (/ 1 (/ (+ bogtime MOBtime) 
                           ( - policyduration (+ bogtime  MOBtime))))
      :DurationExpected policyduration
      :DwellExpected  dwelltime
      :MOBexpected = MOBtime
      :bogbudget = bogbudget)))

(defn ^cyclerecord cycle-add-traversal [cyclerec t startlocation endlocation]
  (let [trav  (str t "_"  startlocation  "_" endlocation)
        ts (get :Traversals cyclerec)] 
    (with-record cyclerec 
      :Traversals (conj trav ts))))

(defn BDR
  "Computes the BOG:Dwell ratio for a cycle."
  [bog mob dwell availableTime MOBexpected BOGExpected DurationExpected 
   & [conventional]]  
  (let [res 
        (cond (and  (> bog  0) (> dwell 0))
              (/ (+ bog mob)  dwell)
              (and (> dwell 0) 
                   (> availableTime 0))
              (/ availableTime dwell)
              :else 
              (/ (+ MOBexpected  BOGExpected) (- DurationExpected 
                                                 (+ MOBexpected BOGExpected))))]
    (if conventional 
      (/ 1 res) 
      res)))

(defn cycle-BDR
  "Computes the BOG:Dwell ratio from a cyclerecord."
  [cyclerec & [conventional]]
  (let [{:keys [bog mob dwell availableTime 
                MOBexpected BOGExpected DurationExpected]} cyclerec]
    (BDR bog mob dwell availableTime 
         MOBexpected BOGExpected DurationExpected conventional)))
  