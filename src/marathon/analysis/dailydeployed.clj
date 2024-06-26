;This namespace is used to compute the daily deployed readiness of units.
(ns marathon.analysis.dailydeployed
  (:require [marathon.analysis :as a]
            [marathon.analysis.util :as util]
            [marathon.ces.core :as c]
            [spork.util.table :as tbl]
            [proc.core :as pcore]))

;;check marathon.analysis.vstats or
;;marathon.analysis.tacmm for examples
;; Marathon [t ctx] (stream)
;; Unit entities
;; marathon.ces.core/current-units, interpolates 
;; Filter deployed
;; Of those deployed to location
;; Group-by src and readiness function
;; analysis.util/readiness

(defn sample
  "Given a sequence with potential sparse times, return
  a sequence with the previous value repeated for all times.
  The new t is assoced into the position, which should be an index or
  key in a map."
  [xs position]
  (let [parts (partition 2 1 xs)
        last-x (last xs)]
    (->  (mapcat (fn [[item-1 item-2]]
                   ;;Need to update time in the item, too.
                   (for [t (range (item-1 position)
                                  (item-2 position))]
                     (assoc item-1 position t)))
                 parts)
         ;;because the partition doesn't include the last item as
         ;;item-1         
         (concat [last-x]))))


(defn sample-stream
  "Given a marathon stream with potential sparse times, return
  frames for all times to make post processing easy."
  [stream]
  (sample stream 0))

(defn sample-trends
  "Given demand trend records with potentiall sparse times, return
  record for all times to make post processing easy."
  [trends]
  (sample trends :t))
                 
(defn unit->location [u ctx]
  (let [location (:locationname u)
        demand-map (:demandmap (c/get-demandstore ctx))
        demand-name (demand-map location)]
    (cond demand-name (:demandgroup demand-name)
          (:deployable u) :deployable
          :else :not-deployable)))

;;----------ranking to t-level stuff that we probably don't need
;;because we currently assume a high level of readiness when deployed.
(defn t-level
  [src rank trainings]
  (let [indices (trainings src)
        [idx limit :as indexed]
        (first (filter #(> (second %) rank) indices))]
    (if indexed
      (inc idx)
      (throw 
       (ex-info "Inventory might be greater than the number of units in
  training-days."
                {:src src :rank rank :indices indices})))))

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
  to it's rank among the the units grouped by src and sorted by
  rank-fn."
  [units rank-fn]
  (let [groups (group-by :src units)]
    (into {} (mapcat #(grouped-rank %) groups))))

(defn training-indices
  [training-days]
  (reduce-kv (fn [acc src train-days]
               (assoc acc src
                      (map-indexed vector
                                   (reductions + train-days))))
               {}
               training-days))

(def training-days
  {"77202K000" [1 2 3 200]
   "01205K000" [20 40 1 200]})

;;-----end of rank to training level stuff

(defn unit->record [u ctx t ;rankings trainings
                    ]
  (let [{:keys [src name policy]} u
        policy-name (get-in policy [:activepolicy :name])
        position (unit->location u ctx)
        ;rank (rankings name)
        ]
    {:name name
     :src src
     :day t
     :position position
     ;:rank rank
     :unit-or-demand :unit
     :demand-group (if (contains? #{:deployable :not-deployable}
                                  position)
                     nil
                     position)
     ;:t-level (t-level src rank trainings)
     }))

(defn frame->dailydeployed [[t ctx] ;training-days
                            ]
  (let [;;Per vstats, after calling current-units to add a dt to the unit,
        ;;need to update unit cycletime.
        ;;units (map #(update % :cycletime + (% :dt))
        ;;         (c/current-units ctx))
        ;;rankings (rank-units units deployable-portion)
        ;;trainings (training-indices training-days)
        ]
    (map #(unit->record % ctx t ;rankings trainings
                        ) (c/current-units ctx))))

(defn daily-deployed
  "Returns a table with keys
  [:src :demand-group :day :unit-or-demand :position] where
  :state-or-demand is :deployable, :not-deployable, or a
  demand group.
  :unit-or-demand is always :unit.
  :demand-group is the demand group if the unit is deployed.
  Otherwise, it is nil.
  :position is either the demand-group, :not-deployable, or
  :deployable.
  :rank is the unit's position in a sequence when grouped by [src
  compo] and sorted by cycletime/startdeployable"
  [proj ;training-days
   ]
  (let [stream (a/marathon-stream proj)]
    (mapcat #(frame->dailydeployed % ;training-days
                                   ) stream)))

(def test-path "/home/craig/runs/test-run/testdata-v7-bog.xlsx")
(def dailys (daily-deployed test-path))
(def frame1 (second (a/marathon-stream test-path)))
(def ctx1 (second frame1))
(def unit1 (first (c/units ctx1)))
(def policy1 1)

(defn project->demandtrends [proj-or-path]
  (->> (a/marathon-stream proj-or-path)
       (a/->demand-trends)
       (pcore/sample-demand-trends-correct)
       ;;keys are demand names and vals are maps of time to demand
       ;;record.      
       (vals)
       ;;get the demand record for each time for each demand name
       (mapcat vals)))

(def dtrends (project->demandtrends test-path))

(defn daily-demand
    "Returns records with keys
  [:src :demand-group :day :unit-or-demand :rank] where
  :unit-or-demand is always :demand
  :rank is always nil
  :demand-group represents the demand group of the demand
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



                       
