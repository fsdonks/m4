(ns marathon.sim.testing
  (:require [marathon.sim.missing] 
            [marathon.sim.core  :as core  :refer [now]]
            [marathon.sim [engine :refer :all]]
            [marathon.sim.supply :as supply]
            [marathon.sim.demand :as demand]
            [marathon.sim.fill.demand   :as fill]
            [marathon.sim.policy :as policy ]
            [marathon.data [simstate :as simstate]]
            [spork.sim     [simcontext :as sim]]
            [clojure.test :refer :all]))




;;Testing for the core Engine Infrastructure
;;==========================================

;;There is no guard against negative times....we may want to enforce 
;;that.
(def ^:constant +end-time+ 1000)
(def primed-sim
  (->> (initialize-sim emptysim +end-time+)
       (sim/add-times [44 100 203 55])))

(deftest basic-engine-testing
  (is (not (keep-simulating?  emptysim))
      "We should be able to simulate, since there is time, 
       and thus events on the clock now.")
  (is (keep-simulating? primed-sim)
      "We should be able to simulate, since there is time, 
       and thus events on the clock now.")
  (is (not (can-simulate? emptysim))
      "No supply or demand should indicate as false for now.")
  (is (= (sim/current-time primed-sim) 1)
      "Initialized simulation should have a start time of one....")
  (is (= (sim/current-time primed-sim) 1)
      "Initialized simulation should have a start time of one....")
  (is (nil? (sim/current-time emptysim)) "empty simulations have no time")
  (is (= (sim/get-final-time primed-sim) +end-time+ ) "upper bound should be the final time.")
  (is (sim/has-time-remaining? primed-sim) "we have time scheduled")
  (is (not (sim/has-time-remaining? emptysim)) "nothing scheduled, should be no more work.")
)


(defn push-message! [ctx edata name]
  (let [s    (sim/get-state ctx)
        msgs (get-in s [:state :messages] [])]
    (->> (assoc-in s [:state :messages] (conj msgs [name edata]))
        (assoc ctx :state))))

(def listener-ctx (assoc emptysim :propogator 
                     (:propogator (sim/make-debug-context :debug-handler push-message!))))
(deftest event-propogation-testing
  (is (= (:messages (sim/get-state (sim/trigger-event :hello :dee :dumb "test!" nil listener-ctx)))
         [[:debugger #spork.sim.simcontext.packet{:t nil, :type :hello, :from :dee, :to :dumb, :msg "test!", :data nil}]])
      "Should have one message logged."))
  
  
      
 


