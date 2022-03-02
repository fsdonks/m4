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
         (let [deployed? (marathon.ces.unit/deployed? u)
               clength   (marathon.ces.unit/get-cyclelength u)
               v  (if deployed? 0 (/ 1.0 clength))
               location (if deployed?
                          (store/gete ctx (u :locationname) :region)
                          :home)]
               (-> (marathon.ces.unit/summary u)
                   (select-keys [:name :curstate #_:location :cycletime :src])
                   ;;normalized dwell is not a great indicator here.
                   ;;we can increase dwell while bogging since it's based on
                   ;;cycletime....kind of a misnomer :(
                   (assoc  :readiness (if deployed?
                                        (marathon.ces.unit/normalized-dwell u
                                          (u :cycle-time-when-deployed))
                                        (marathon.ces.unit/normalized-dwell u))
                           :compo     (u :component)
                           :location  location
                           :velocity  v)
                   ;;lame rekey to SRC because reasons...
                   (re-key {:name :id :curstate :state :src :SRC}))))
           (reduce (fn [acc r] (assoc acc (r :id) r)) {})))

(defn compute-moves [ctx]
  (when-let [locs (a/location-changes ctx)]
    (let [ds (core/demand-entities ctx)]
      (->> (for [[id from to] locs]
             (cond (and (not (ds from)) (ds to)) ;;deployment
                   [:deployed id from to  :home (store/gete ctx to :region)]
                   (and (ds from) (not (ds to))) ;;home
                   [:returned id from to (store/gete ctx from :region)  :home]
                   ;;c-rating change.
                   (and (ds from) (ds to)) ;;re-deployed
                   [:re-deployed id from to (store/gete ctx from :region) (store/gete ctx to :region)]
                   :else
                   nil
                   #_[:dwell id from to] #_(throw (ex-info "unknown move!" {:move [id from to]}))))
           (filterv identity)))))

(defn frame->vstats [[t ctx]]
  (let [id->region (fn [r] (store/gete ctx (r :id) :region))
        misses (when-let [xs (seq (marathon.ces.demand/unfilled-demand-count ctx))]
                 (->> (for [[region recs] (group-by id->region xs)
                            [src    ys]   (group-by :src recs)]
                            [region src (reduce + 0 (map :unfilled ys))])
                      (reduce (fn [acc [region src n]]
                                (assoc-in acc [region src] n)) {})))
        total-missed (if (seq misses) (reduce + (mapcat vals (vals misses))) 0)]
  {:t t
   :period   (core/current-period ctx)
   :entities (unit-entities ctx)
   :moves    (compute-moves ctx)
   :total-missed  total-missed
   :misses   misses}))

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
     :profile  (core/demand-profile ctx0)
    :frames (vec (map frame->vstats (rest h)))}))

;;simple API call to dump an edn file.  Should probably migrate to using
;;transit, but meh.  We'll see how long this takes in practice.
(defn dump-stats [from to]
  (println [:spitting :vis-stats :from from])
  (->> (io/file-path from)
       a/marathon-stream
       history->vis-state
       pr-str
       (spit (io/file-path to)))
  (println [:emitted :to to]))
