;;A namespace for testing, currently, entity behaviors and such.
(ns marathon.data.testing
  (:require [marathon.sim.testing :as test]
            [marathon.data.behavior :refer :all]
            [marathon.data.fsm :as fsm]
            [marathon.sim [core :as core]
                          [unit :as u]
                          [demand :as d]
                          [supply :as supply]
                          ]            
            [spork.sim.simcontext :as sim]
            [clojure.test :refer :all]))


(def u   (val (first (core/units test/demandctx))))
(def s1  (assoc fsm/blank-data :duration 0)) ;motivate a change.

;;note u and statedata are decoupled....
(def testctx 
  (-> test/demandctx
      (merge-bb {:entity    u 
                 :statedata s1
                })))

(defn b!  [b ctx]  (first  (beval b ctx)))
(defn b!! [b ctx]  (second (beval b ctx)))

(defn get-u [ctx] 
  (get (core/units ctx) (:name u)))

(def test-delta 400)
(def test-run 
  (core/debugging 
   (update u test-delta testctx)))
(def u2 (get-u test-run))
(def summaries (map u/summary [u u2]))

;;Let's excercise and test the default entity behavior (supported by
;;the new behavior tree system).
(deftest entity-update 
  (let [[l r] summaries]
    (is (not= test-run testctx)
        "Should have a change in unit data, and change in hash.")
    (is (not= u u2) 
        "Unit should have been updated quite a bit.")
    (is (= (- (:cycletime r) (:cycletime l)) test-delta)
        "Updated unit should have spent expected time in cycle.")
    (is (= (- (:dwell r) (:dwell l)) test-delta)
        "Updated unit should have spent expected time in cycle.")))


