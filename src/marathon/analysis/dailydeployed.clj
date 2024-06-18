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
      
(defn unit->record [u ctx t rankings]
  (let [{:keys [src name policy]} u
        policy-name (get-in policy [:activepolicy :name])
        position (unit->location u ctx)]
    {:name name
     :src src
     :day t
     :position position
     :rank (rankings name)
     :unit-or-demand :unit
     :demand-group (if (contains? #{:deployable :not-deployable}
                                  position)
                     nil
                     position)}))

(defn start-deployable [u]
  (get-in u [:policy :activepolicy :startdeployable]))
;;Add a readiness rank, so among the same src and compo, was it the
;;most ready (1) or less ready.
;;All conflict policies have an effectively infinite cycle length.
;;Instead of using cycle length, we determine the most ready units by
;;cycletime/startdeployable to determine how close they are to start
;;deployable.  
(defn deployable-portion
  [unit]
  (let [startdeployable (start-deployable unit)
        cycletime (:cycletime unit)]    
    (if (zero? startdeployable)
      ;;If startdeployable=0,
      ;;cycletime goes from 0 to to cyclelength-1
      ;;proportion=100% on day 0, 200% on day 1, so
      ;;proportion=(cycletime+1)/1
      (inc cycletime)
      (/ cycletime startdeployable))))

(defn grouped-rank
  [[group recs]]
  (let [sorted (sort-by deployable-portion recs)]
          (for [i (range (count sorted))]
            [(:name (nth sorted i)) i])))

;;group units by src, compo, sort the groups, each unit gets a rank
(defn rank-units
  "Given a sequence of unit entities, returns a map of the unit's name
  to it's rank among the the units grouped by group-fn and sorted by
  rank-fn."
  [units group-fn rank-fn]
  (let [groups (group-by group-fn units)]
    (into {} (mapcat #(grouped-rank %) groups))))

(defn src-compo
  [unit]
  ((juxt :src :component) unit))

;;Per vstats, after calling current-units to add a dt to the unit,
;;need to update unit cycletime.
(defn frame->dailydeployed [[t ctx]]
  (let [units (map #(update % :cycletime + (% :dt))
                   (c/current-units ctx))
        rankings (rank-units units src-compo deployable-portion)]
    (map #(unit->record % ctx t rankings) units)))

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
(def frame1 (second (a/marathon-stream test-path)))
(def ctx1 (second frame1))
(def unit1 (first (c/units ctx1)))
(def policy1 1)

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



                       
