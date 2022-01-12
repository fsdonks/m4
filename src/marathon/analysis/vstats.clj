(ns marathon.analysis.vstats
  (:require [marathon.ces
             [core :as core]
             [unit :as unit]
             [demand :as demand]]
            [marathon.analysis :as a]
            [spork.entitysystem.store :as store]
            [spork.util.general]
            [spork.util [io :as io]]))

;;testing
(def p (io/file-path "~/workspacenew/notional/base-testdata-v7-rearmm-vizbct2.xlsx"))

;;we can keep track of a sparse sequence of changes.

;;for t in frames, we only want to know
;;{t name state location ct normalized-ct}
;;should be able to infer what we need from that...

(defn re-key [m kvs]
  (reduce-kv (fn [acc kold knew]
               (if-let [v (acc kold)]
                 (-> acc (dissoc kold) (assoc knew v))
                 acc))
             m kvs))

(defn unit-entities [ctx]
  (->> (for [u     (core/current-units ctx)]
         (let [v  (if (marathon.ces.unit/deployed? u) 0
                      (/ 1.0 (marathon.ces.unit/get-cyclelength u)))]
               (-> (marathon.ces.unit/summary u)
                   (select-keys [:name :curstate :location :cycletime :src])
                   (assoc  :readiness (marathon.ces.unit/normalized-dwell u)
                           :compo     (u :component)
                           :velocity  v)
                   (re-key {:name :id :curstate :state}))))
           (reduce (fn [acc r] (assoc acc (r :id) r))
                   {})))

(defn compute-moves [ctx]
  (when-let [locs (a/location-changes ctx)]
    (let [ds (core/demand-entities ctx)]
      (->> (for [[id from to] locs]
             (cond (and (not (ds from)) (ds to)) ;;deployment
                   [:deployed id from to]
                   (and (ds from) (not (ds to))) ;;home
                   [:home id from to]
                   ;;c-rating change.
                   :else
                   nil
                   #_[:dwell id from to] #_(throw (ex-info "unknown move!" {:move [id from to]}))))
           (filterv identity)))))

(defn frame->vstats [[t ctx]]
  {:t t
   :period   (core/current-period ctx)
   :entities (unit-entities ctx)
   :moves    (compute-moves ctx)
   :missed   (if-let [xs (seq (marathon.ces.demand/unfilled-demand-count ctx))]
               (reduce + 0 (map :unfilled xs))
               0)})

;;need to group demands by region...

(defn history->vis-state [h]
  (let [[t0 ctx0]     (first h)
        init-entities (unit-entities ctx0)
        regions       ((store/domains ctx0) :region)
        init-demand   (->> ctx0
                           core/active-demands
                           (map :region)
                           frequencies)]
    {:c-day    0
     :tstart   t0
     :tstop    (store/gete ctx0 :parameters :LastDayDefault)
     :entities init-entities
     :regions  ((store/domains ctx0) :region)
     ;;demand and slots don't make sense.
     :demand   init-demand
     :slots    init-demand
     :period   (core/current-period ctx0)
     ;;these aren't currently computed.
     ;;we can compute them on the other side.
     :totals   {:mission     0
                :available   0
                :unavailable 0}
    :frames (vec (map frame->vstats (rest h)))}))


;;we want to build a map of
;; #_
;; {:entities intial set of {:keys [id src compo icon location readiness]}
;;  :c-day 0
;;  :tstart tstart
;;  :tstop  tstop
;;  :stats {:deployed {:C1 0
;;                     :C2 0
;;                     :C3 0
;;                     :C4 0
;;                     :C5 0
;;                     :Missing 0}
;;          :totals  (totals @state)}
;;  :fill-stats {:northcom empty-fill-stats
;;               :eucom    empty-fill-stats
;;               :centcom  empty-fill-stats
;;               :pacom    empty-fill-stats}}

#_
(defn totals [s]
  (->> s :entities vals
       (reduce (fn [acc e]
                 (case (e :location)
                   :home (case (naive-c-rating e)
                           (:C1 :C2) (update acc :available inc)
                           (update acc :unavailable inc))
                   (update acc :mission inc)))
               {:mission 0
                :available 0
                :unavailable 0})))

