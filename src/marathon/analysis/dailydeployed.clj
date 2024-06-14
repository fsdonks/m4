;This namespace is used to compute the daily deployed readiness of units.
(ns marathon.analysis.dailydeployed
  (:require [marathon.analysis :as a]
            [marathon.analysis.util :as util]
            [marathon.ces.core :as c]
            [spork.util.table :as tbl]))

;;check marathon.analysis.vstats or
;;marathon.analysis.tacmm for examples
;; Marathon [t ctx] (stream)
;; Unit entities
;; marathon.ces.core/current-units, interpolates 
;; Filter deployed
;; Of those deployed to location
;; Group-by src and readiness function
;; analysis.util/readiness

(defn lerp-stream
  "Given a marathon stream with potential sparse times, return
  frames for all times to make post processing easy."
  [stream]
  (let [parts (partition 2 1 stream)
        last-frame (last stream)]
    (->  (mapcat (fn [[[t1 ctx1 :as frame1] [t2 ctx2]]]
                   (repeat (- t2 t1) frame1)) parts)
         ;;because the partition doesn't include the last frame as
         ;;frame1         
         (concat [last-frame]))))
                 
(defn unit->location [u ctx]
  (let [location (:locationname u)
        demand-map (:demandmap (c/get-demandstore ctx))
        demand-name (demand-map location)]
    (cond demand-name (:demandgroup demand-name)
          (:deployable u) :deployable
          :else :not-deployable)))
      
(defn unit->record [u ctx t]
  (let [{:keys [src name policy]} u
        policy-name (get-in policy [:activepolicy :name])]
    {:name name
     :src src
     :day t
     :state-or-demand (unit->location u ctx)}))
  
(defn frame->dailydeployed [[t ctx]]
  (let [units (c/units ctx)]
    (map #(unit->record % ctx t) units)))

;;Add a readiness rank, so among the same src and compo, was it the
;;most ready (1) or less ready.
;;group units by src, compo, sort the groups, each unit gets a rank

(defn daily-deployed
  "Returns a table with keys
  [:src :demand-group :day :unit-or-demand :deployed-c-level] where
  :state-or-demand is :deployable, :not-deployable, or a
  demand group.
  :unit-or-demand is always a unit name (index_src_compo).
  :deployed-t-level is always the deployed t-level derived from
  cycletime and from the
  name of the policy with days to T1 from T3 and days to T1 from
  T2. We'll assume T4 were cannibalized."
  [proj]
  (let [stream (lerp-stream (a/marathon-stream proj))]
    (mapcat frame->dailydeployed stream)))

(def test-path "/home/craig/runs/test-run/testdata-v7-bog.xlsx")
(def dailys (daily-deployed test-path))
(def ctx1 (second (second (a/marathon-stream test-path))))
(def unit1 (first (c/units ctx1)))

(defn daily-demand
    "Returns records with keys
  [:src :demand-group :day :unit-or-demand :deployed-t-level] where
  :unit-or-demand is always :demand
  :deployed-t-level is always :demand
  This will allow a post-processed rollup of the daily number of units
    in each c-level and how large the demand is."
  [])

(defn deployed-demand
  "Concat daily-deployed and daily-demand. Filter results for only the
  peak demand between t-start and t-end."
  [proj t-start t-end])

(defn later-demand
   "Takes a project and removes demand records that start before time, t"
  [proj t]
  )



                       
