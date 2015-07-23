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

(defn get-u [ctx]  (get (core/units ctx) (:name u)))

(def test-delta 400)
(def test-run  (core/debugging (update u test-delta testctx)))
(def u2        (get-u test-run))
(def summaries (map u/summary [u u2]))

;;Let's excercise and test the default entity behavior (supported by
;;the new behavior tree system).
(deftest entity-update 
  (let [[l r] summaries]
    (is (not= test-run testctx)
        "Should have a change in unit data, and change in hash.")
    (is (not= l r) 
        "Unit should have been updated quite a bit.")
    (is (= (- (:cycletime r) (:cycletime l)) test-delta)
        "Updated unit should have spent expected time in cycle.")
    (is (= (- (:dwell r) (:dwell l)) test-delta)
        "Updated unit should have spent expected time in cycle.")))

;;Some thoughts on the current process...
;;We can evolve the context over time by updating each entity in turn,
;;and allowing them to have access to the context (ultimately, "write" 
;;access, or at least the opportunity to compute the new context). 
;;We essentially have a plethora of state transition functions
;;composed for each entity....The transition function for the context 
;;is a composition of these functions (currently a high-level
;;sequencing of "managers" that thread their state transitions 
;;through subordinate "entity" functions). 

;;This mechanism is simple and pure, but it has some problems....
;;We have no concept of identity (aside from explicit paths into 
;;the context), so we can't talk about entities in a higher-order 
;;fashion.  So I'd like to "map" a behavior to an entity, filtered 
;;by a time period?  What about "mapping" a behavior to an entity 
;;by event? Interesting questions...

;;It would be convenient, not necessarily more performant, to 
;;provide access to entities by identity.  So, in the context, 
;;if we declare an entity, we automatically get an address to 
;;its place (If we're in an ECS, this is done for us already...) 







