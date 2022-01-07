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
(def p (io/file-path "~/workspacenew/notional/base-testdata-v7-rearmm-vizbct.xlsx"))

;;we can keep track of a sparse sequence of changes.

;;for t in frames, we only want to know
;;{t name state location ct normalized-ct}
;;should be able to infer what we need from that...

#_
{:SRC "ABCT"
 :compo "NG"
 :icon "abct.png"
 :location :home
 :readiness 0 ;;0 to 1, 0 == c4, 1 == c1, even partitions.
 }


(defn compute-moves [ctx]
  (when-let [locs (a/location-changes ctx)]
    (let [ds (core/demand-entities ctx)]
      (->> (for [[id from to] locs]
             (do #_(println [id from to])
                 (cond (and (not (ds from)) (ds to)) ;;deployment
                       [:deployed id from to]
                       (and (ds from) (not (ds to))) ;;home
                       [:home id from to]
                       :else nil #_(throw (ex-info "unknown move!" {:move [id from to]})))))
           (filterv identity)))))

(defn frame->vstats [[t ctx]]
  (let [region (spork.util.general/memo-1
                (fn [loc] (store/gete ctx loc :region)))]
    {:t t
     :period   (core/current-period ctx)
     :entities (->> (for [u     (core/current-units ctx)]
                      (-> (marathon.ces.unit/summary u)
                          (select-keys [:name :curstate :location :cycletime :src])
                          (assoc  :readiness (marathon.ces.unit/normalized-dwell u)
                                  :compo (u :component)
                                  :id    (u :name))))
                    (reduce (fn [acc r]
                              (assoc acc (r :id) r))
                            {}))
     :moves    (compute-moves ctx)
     :missed   (if-let [xs (seq (marathon.ces.demand/unfilled-demand-count ctx))]
                 (reduce + 0 (map :unfilled xs))
                 0)}))

(defn history->vis-state [h]
  (let [[t0 ctx0] (first h)
        [tf _]    (last h)
        init-entities (core/unit-entities )]
    {:c-day 0
     :tstart t0
     :tstop  tf
     :entities init-entities
     :demand {:northcom 1
              :pacom    3
              :eucom    3
              :centcom  2}
     :slots {:northcom 1
             :pacom    3
             :eucom    3
             :centcom  2}
     :period "Initialization"
     :totals {:mission 0
              :available 0
              :unavailable 0}
    :frames (rest h)}))


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

