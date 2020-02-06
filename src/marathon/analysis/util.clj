(ns marathon.analysis.util
  (:require [clojure.core.async :as async]
            [marathon     [analysis :as a]]
            [marathon.ces [core :as c] [query :as query]]
            [spork.entitysystem [store :as store]]
            [spork.util [io :as io] [table :as tbl]]))

(defn pmap!
  ([n f xs]
   (let [output-chan (async/chan)]
     (async/pipeline-blocking n
                          output-chan
                          (map f)
                          (async/to-chan xs))
     (async/<!! (async/into [] output-chan))))
  ([f xs] (pmap! (.availableProcessors (Runtime/getRuntime)) f  xs)))


;;some generic stuff migrated from the tacmm scripting..
;;formerly tacmm-key
(defn state-key [u]
  (let [s (:state u)]
    (cond (s :deployable)   :deployable
          (s :modernizing)  :modernizing
          (s :dwelling)     :not-ready
          (s :bogging)      :deployed
          (s :overlapping)  :deployed
          (s :demobilizing) :not-ready
          (= s :waiting)    :deployed ;;NonBOG corner case.
          :else (throw (ex-info (str "unknown state!" s)
                                {:in (:state u) :name (:name u)})))))

(defn deployables [xs]
  (filter #(= (state-key %) :deployable) xs))

(defn c-rating [s]
  (first (clojure.set/intersection
          #{:c1 :c2 :c3 :c4}
          (if (coll? s) (set s) #{s}))))

(defn readiness [u]
  (let [state     (:state u)
        statedata (:statedata u)
        c         (c-rating state)]
    (cond (or (state :modernizing) (state :waiting))
          (-> statedata :nextstate c-rating)
          c c
          :else
          (or (c-rating (:prevstate statedata))
              (let [p  (:policy u)
                    ;;we had units exceeding cycle times (naturally due to late deployment),
                    ;;need to inject a ceiling...
                    ct (min (-> u :currentcycle :dwell)
                            (marathon.data.protocols/expected-dwell p))]
                (->> (marathon.data.protocols/get-position p ct)
                     (marathon.data.protocols/state-at     p)
                     c-rating))
              (throw (ex-info "can't find c-rating"
                              {:in (select-keys u [:name :state :statedata])}))))))

(defn demand-trends-exhaustive
  ([t ctx]
   (let [qtr     (a/as-quarter t) ;;1-based quarters.
         changes (store/gete ctx :demand-watch :demands) ;;map of {demand-name ..}
         finals  (store/gete ctx :DemandStore  :finals)
         actives (store/gete ctx :DemandStore  :activedemands)
         finals? (finals t)]
     (->> actives
          (keys)
          (map #(store/get-entity ctx %))
          (map  (fn [{:keys [category demandgroup operation vignette Command] :as d}]
                  (let [assigned     (:units-assigned    d)
                        overlapping  (:units-overlapping d)
                        ua           (count              assigned)
                        uo           (count              overlapping)
                          ]
                    (merge
                     {:t             t
                      :Quarter       qtr
                      :SRC           (:src      d)
                      :TotalRequired (:quantity d)
                      :TotalFilled   (+ uo ua)
                      :Overlapping   uo
                      :Deployed      ua
                      :DemandName    (:name d)
                      :Vignette      vignette
                      :DemandGroup   demandgroup
                      :deltaT       (when (and finals? (= t (a/tfinal d))) 1)}
                     (a/compo-fills ctx assigned overlapping) )))))))
  ([ctx] (demand-trends-exhaustive (spork.sim.core/get-time ctx) ctx)))

(defn map-val   [k->v f]
  (fn map-val [k]
    (if-let [v (k->v k)]
      (f v)
      (throw (ex-info "no result for input"
                      {:in k
                       :k->v k->v})))))

(defprotocol IGen
  (next-long [g])
  (next-double [g]))

(defn ->gen [seed]
  (let [^java.util.Random gen (java.util.Random. (long seed))]
    (reify
      clojure.lang.IFn
      (invoke [this]    (.nextDouble gen))
      (invoke [this n]  (* (.nextDouble gen) n))
      IGen
      (next-long   [g] (.nextLong gen))
      (next-double [g] (.nextDouble gen)))))

(def default-gen (->gen 42))
(defn gen-rand-int
  ([gen n] (long (* ^double (next-double gen) ^long n)))
  ([n]     (long (* ^double (next-double default-gen) ^long n))))
