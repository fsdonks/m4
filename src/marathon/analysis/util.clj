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

(defn not-readies [xs]
  (filter #(= (state-key %) :not-ready) xs))

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

(extend-protocol IGen
  java.util.Random
  (next-long   [g] (.nextLong   g))
  (next-double [g] (.nextDouble g)))

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


(defn try-random-run
  "Utility function to help flush out random runs
   to find an input that leads to an exception."
  [get-random-project init-proj]
  (let [rp (get-random-project init-proj)]
    (try (count (a/marathon-stream (a/load-context rp)))
         (catch Exception e (ex-info "bad-value!" {:err e :project rp})))))

(defn find-bad-run
  "Utility function to help seek out inputs that cause errors.
   If we know we have an initial project that generated a
   random project that caused a failure, we can seed this
   process with the project, init-proj, and use the
   same randomization function, get-random-project, to
   do either a finite amount of runs, via. limit, or
   and unbounded amount of random runs.  This is useful
   for searching for poor inputs.  If one is found, an
   ex-info will be returned which the caller and extract
   relevant data from - the stack trace, as well as the
   project - which are in the ex-data, {:keys [err project]}."
  [init-proj & {:keys [get-random-project limit]}]
  (->> (if limit (range limit) (range))
       (map (fn [idx]
              (do (println [:run idx])
                  (try-random-run get-random-project init-proj))))
       (drop-while number?)
       (first)))

(def log-chan (async/chan (async/sliding-buffer  1000)))

(defn log
  "Logs messages asynchronously, prints synchronously.
   This allows logging to occur from multiple writers
   asynchronously, while retaining serialized printing
   for readability."
  [msg]
  (clojure.core.async/put! log-chan msg))

(def logger
  (async/go-loop []
    (when-let [msg (async/<! log-chan)]
        (do (println msg)
            (recur)))))


(defn derive-phases [proj]
  (->> proj
       :tables
       :PeriodRecords
       tbl/table-records
       (filter (complement (comp zero? :ToDay)))
       (sort-by :FromDay)
       (mapv (juxt :Name :FromDay :ToDay))))
