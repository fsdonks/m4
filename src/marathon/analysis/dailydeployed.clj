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


(def ^:dynamic *force-t-days* false)
(defn t-level
  "Determine ranking to t-level."
  [unit-name rank trainings]
  (let [src (second (clojure.string/split unit-name #"_"))
        indices (trainings src)
        [idx limit :as indexed]
        (first (filter #(> (second %) rank) indices))]
    (if indexed
      [unit-name (inc idx)]
      (if *force-t-days*
        (throw 
         (ex-info "Inventory might be greater than the number of units in
  training-days."
                  {:unit-name unit-name :rank rank :indices indices}))
        ;;assume t4 by default
        [unit-name 4]))))

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
    (if (< cycletime startdeployable)
      (/ cycletime startdeployable)
      ;;faster policy probably doesn't do anything after
      ;;startdeployable
      (+ 1 (- cycletime startdeployable)))))

(defn grouped-rank
  [[group recs]]
  (let [sorted (sort-by (comp - deployable-portion) recs)]
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

;;-----end of rank to training level stuff

(defn unit->record [u ctx t t-levels training-start]
  (let [{:keys [src name policy]} u
        policy-name (get-in policy [:activepolicy :name])
        position (unit->location u ctx)]
    {:name name
     :src src
     :day t
     :position position
     :unit-or-demand :unit
     :demand-group (if (contains? #{:deployable :not-deployable}
                                  position)
                     nil
                     position)
     :t-level (when (>= t training-start)
                (t-levels name))
     }))

;;Per vstats, after calling current-units to add a dt to the unit,
;;need to update unit cycletime.
(defn updated-units
  [ctx]
  (map #(update % :cycletime + (% :dt))
       (c/current-units ctx)))

(defn frame->dailydeployed [[t ctx] t-levels training-start]
  (let [units (updated-units ctx)]
    (map #(unit->record % ctx t t-levels training-start
                        ) (c/current-units ctx))))

(defn stream->t-levels
  "Returns a map of unit to t-level after marathon readiness is ranked
  and matched to a number of units in each t-level bucket."
  [stream t training-days]
  (let [ctx (a/day-before (inc t) stream)
        units (updated-units ctx)
        rankings (rank-units units deployable-portion)
        trainings (training-indices training-days)]
    (into {} (map (fn [[unit-name rank]]
                    (t-level unit-name rank trainings))
                  rankings))))
     
(defn daily-deployed
  "Given a MARATHON proj, map of src string to a sequence of number
  of units
  in each t-level where [1 4] indicates 1 unit in t1 and 4 units in
  t2.
  An empty map of training-days forces everything to t4.  Units get
  tagged with a t-level based on their readiness ranking on the
  day of training-start.
  Returns a table with keys
  [:src :name :demand-group :day :unit-or-demand :position :t-level] where
  :state-or-demand is :deployable, :not-deployable, or a
  demand group.
  :name is the unit name
  :unit-or-demand is always :unit.
  :demand-group is the demand group if the unit is deployed.
  Otherwise, it is nil.
  :position is either the demand-group, :not-deployable, or
  :deployable.
  :t-level is 1, 2, 3, or 4 based on a ranking of units by
  deployable-portion and then mapped to a number of units in each
  training level, mapping to t1 first, then t2, etc."
  [proj training-days training-start]
  (let [stream (a/marathon-stream proj)
        t-levels (stream->t-levels stream training-start
                                   training-days)]
    (mapcat #(frame->dailydeployed % t-levels training-start
                                   ) stream)))

(defn project->demandtrends [proj-or-path]
  (->> (a/as-stream proj-or-path)
       (a/->demand-trends)
       (pcore/sample-demand-trends-correct)
       ;;keys are demand names and vals are maps of time to demand
       ;;record.      
       (vals)
       ;;get the demand record for each time for each demand name
       (mapcat vals)))

(defn daily-demand
  "Returns records with keys
  [:src :demand-group :day :unit-or-demand :rank :position :name
  :t-level] where
  :unit-or-demand is always :demand
  :position is always :demand
  :rank is always nil
  :name is always nil
  :t-level is always nil
  :demand-group represents the demand group of the demand
  This will allow a post-processed rollup of the daily number of units
    in each c-level and how large the demand is."
  [proj-or-path]
  (let [trends (project->demandtrends proj-or-path)
        groups (group-by (juxt :SRC :DemandGroup :t) trends)]
    (for [[[src demand-group day] d-trends] groups
          trend (range (reduce + (map :TotalRequired d-trends)))]
      {:src src :demand-group demand-group :day day
       :unit-or-demand :demand
       :position :demand
       :name nil
       :t-level :demand}
      )))

(defn deployed-demand
  "Concat daily-deployed and daily-demand"
  [proj-or-path training-days training-start]
  (concat
   (daily-demand proj-or-path)
   (daily-deployed proj-or-path training-days training-start))
  )

(defn daily->file
  [proj-or-path out-path training-days training-start]
  (tbl/records->file
   (deployed-demand proj-or-path training-days training-start)
   out-path))                  

(defn later-demand
   "Takes a project and removes demand records that start before time, t"
  [proj t]
  )

;;testing
;;One of these srcs doesn't exist so I wanted to see if default
;;t-level of 4 would kick in.
;; (def training-days
;;   {"77202K00" [1 2 3 200]
;;    "01205K000" [20 40 1 200]})
;; (def test-path "/home/craig/runs/test-run/testdata-v7-bog.xlsx")
;; (def out-path "/home/craig/runs/test-run/dailys.txt")
;; (def dailys (daily-deployed test-path training-days 1650))
;; (def dtrends (project->demandtrends test-path))
;; (def daily-demands (daily-demand test-path))
;; (daily->file test-path out-path training-days 1650)

                       
