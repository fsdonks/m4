;;this is pretty cool; we can test entity
;;behaviors in isolation here.  very nice.
(ns marathon.ces.behaviortest
  (:require [spork.ai.behavior :refer [beval]]
            [marathon.ces.behavior :as b]
            [marathon.ces.basebehavior :as base]
            [marathon.ces.core :as core]
            [spork.entitysystem.store :as store]
            [marathon.ces.testing :as t]
            [spork.sim.simcontext :as sim]))

(set! *print-level* 5)
(set! *print-length* 100)
;;gives us a base context to work with, for messaging and
;;stuff.
(def ctx marathon.ces.testing/defaultctx)

(def ent 
  {:oi-title "no-description",
   :cycles [],
   :policystack [],
   :statedata
   #marathon.data.fsm.statedata{
      :curstate :spawning,
      :prevstate nil,
      :nextstate nil,
      :timeinstate 0,
      :timeinstateprior 0,
      :duration Infinity,
      :durationprior Infinity,
      :statestart 0,
      :statehistory []},
   :home :default,
   :color nil,
   :locationname "Spawning",
   :interactive true,
   :deployable true,
   :speed 8,
   :cycletime 1399,
   :unit-entity true,
   :name "23_SRC3_NG",
   :position-policy :deployed,
   :deployable-bucket :default,
   :supply true,
   :type "SRC3",
   :behavior
   b/default
   :src "SRC3",
   :state :spawning,
   :icon :blank-icon,
   :positionpolicy ["Ready" :deployable],
   :component "NG",
   :policy
   #marathon.policy.policydata.policy{
    :name "RCOpSus",
    :cyclelength 1825,
    :mindwell 730,
    :maxdwell 1825,
    :maxbog 270,
    :maxMOB 9999999,
    :recovery 90,
    :startdeployable 1216,
    :stopdeployable 1735,
    :positiongraph
    #spork.data.digraph.digraph{:nodes
     {:deployable 7,
      "DeMobilization" 6,
      "Ready" #{:dwelling},
      ["Available" :non-deployable] 10,
      "Train" #{:dwelling},
      "Deployed" #{:bogging},
      ["Ready" :deployable] #{:deployable},
      "Reset" #{:dwelling},
      :non-deployable 9,
      "Available" #{:deployable :dwelling},
      "Overlapping" #{:overlapping}},
     :sources
     {:deployable {"Ready" 486},
      "DeMobilization" {"Overlapping" 90},
      "Ready" {"Train" 365},
      ["Available" :non-deployable] {:non-deployable 0},
      "Train" {"Reset" 365},
      "Deployed" {},
      ["Ready" :deployable] {:deployable 0},
      "Reset" {"DeMobilization" 95, ["Available" :non-deployable] 90},
      :non-deployable {"Available" 275},
      "Available" {["Ready" :deployable] 244},
      "Overlapping" {"Deployed" 180}},
     :sinks
     {:deployable {["Ready" :deployable] 0},
      "DeMobilization" {"Reset" 95},
      "Ready" {:deployable 486},
      ["Available" :non-deployable] {"Reset" 90},
      "Train" {"Ready" 365},
      "Deployed" {"Overlapping" 180},
      ["Ready" :deployable] {"Available" 244},
      "Reset" {"Train" 365},
      :non-deployable {["Available" :non-deployable] 0},
      "Available" {:non-deployable 275},
      "Overlapping" {"DeMobilization" 90}},
     :data nil},
    :startstate "Reset",
    :endstate "Available",
    :overlap 45,
    :bogbudget 270,
    :maxMob 270},
   :dwell-time-when-deployed 1399,
   :oititle "Generated_SRC3",
   :messages nil,
   :currentcycle
   #marathon.data.cycle.cyclerecord{
    :uic-name "23_SRC3_NG",
    :src "SRC3",
    :component "NG",
    :policyname "RCOpSus",
    :tstart 0,
    :tfinal 1825,
    :duration 1399,
    :available-time 270,
    :deployableTime 0,
    :duration-expected 1825,
    :bog 0,
    :bogbudget 270,
    :bog-expected 270,
    :dwell 1399,
    :dwell-expected 1825,
    :mob-expected 9999999,
    :mob 0,
    :deployments 1,
    :followons 0,
    :bog-to-dwell-expected nil,
    :traversals nil},
   :label "23_SRC3_NG",
   :spawntime -1,
   :deployable-cat "SRC3",
   :locationhistory [],
   :physical true,
   :position [0 0],
   :followoncode nil,
   :location :spawning,
   :velocity [0 0]})

;;we want to verify our individual behaviors here...
;;instead of stepping the entity fully, we'll
;;beval and see what happens...

;;let's do a spawning test...
(def e (:name ent))
;;we should the the entity spawn...
(defn changed-unit []
  (let [spawning-ent (assoc ent :behavior b/change-state-beh)]
    (-> ctx
        (base/step-entity! spawning-ent (core/->msg e e 0 :change-state nil ;{:newstate
                                                                            ; :duration
                                                                            ; :followingstate
                                                                            ; :timeinstate}
                                                    ))
        (store/get-entity e))))

(defn aged-unit []
  (let [spawning-ent (assoc ent :behavior b/age-unit)]
    (-> ctx
        (base/step-entity! spawning-ent (core/->msg e e 0 :update nil))
        (store/get-entity e))))

(defn spawn-unit []
  (let [spawning-ent (assoc ent :behavior b/spawning-beh)
        ctx (-> ctx
                (base/step-entity! spawning-ent (core/->msg e e 0 :update nil)))]
        
        (with-meta (store/get-entity ctx e) {:ctx ctx})))

;; (defn updated-unit []
;;     (let [spawning-ent (assoc (spawn-unit) :behavior b/]
;;       (-> ctx
;;           (base/step-entity! e (core/->msg e e 10 :update nil))
;;           (store/get-entity e))))
