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
            [spork.sim.simcontext :as sim]))


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

