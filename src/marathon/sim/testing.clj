(ns marathon.sim.testing
  (:require [marathon.sim.missing] 
            [marathon.sim [engine :refer :all]]
            [marathon.sim.fill.demand   :as fill]
            [marathon.sim [core :as core]
                          [supply :as supply]
                          [demand :as demand]
                          [policy :as policy]
                          [policyio :as policyio]
                          [sampledata :as sd]]                        
            [marathon.data [simstate :as simstate]]
            [spork.sim     [simcontext :as sim]]            
            [clojure.test :as test]))

;;Testing for the core Engine Infrastructure
;;==========================================

;;There is no guard against negative times....we may want to enforce 
;;that.
(def ^:constant +end-time+ 1000)

;;note, even the empty sim has time 0 on the clock.  Perhaps we should
;;alter that....
(def primed-sim
  (->> (initialize-sim core/emptysim +end-time+)
       (sim/add-times [44 100 203 55])))

(deftest basic-engine-testing
  (is (keep-simulating?  core/emptysim)
      "We should be able to simulate, since there is time, 
       and thus events on the clock now.")
  (is (not (can-simulate? core/emptysim))     
      "No supply or demand should indicate as false for now.")
  (is (zero?(sim/current-time core/emptysim)) 
      "empty simulations have no time")
  (is (not  (sim/has-time-remaining? (sim/advance-time core/emptysim))) 
      "nothing scheduled, should be no more work.")
)

(deftest primed-engine-testing 
  (is (keep-simulating? primed-sim)
      "We should be able to simulate, since there is time, 
       and thus events on the clock now.")
  (is (= (sim/get-next-time primed-sim) 1)
      "Initialized simulation should have a start time of one....")
  (is (= (sim/get-final-time primed-sim) +end-time+ ) "upper bound should be the final time.")
  (is (sim/has-time-remaining? primed-sim) "we have time scheduled")
)

(defn push-message! [ctx edata name]
  (let [s    (sim/get-state ctx)
        msgs (get-in s [:state :messages] [])]
    (->> (assoc-in s [:state :messages] (conj msgs [name edata]))
        (assoc ctx :state))))

(def listener-ctx (assoc core/emptysim :propogator 
                     (:propogator (sim/make-debug-context :debug-handler push-message!))))

(deftest event-propogation-testing
  (is (= (:messages (sim/get-state (sim/trigger-event :hello :dee :dumb "test!" nil listener-ctx)))
         [[:debugger #spork.sim.simcontext.packet{:t nil, :type :hello, :from :dee, :to :dumb, :msg "test!", :data nil}]])
      "Should have one message logged."))
  

  
      
 


