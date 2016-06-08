;;Defines utilities and workflows for
;;performing higher-order analysis
;;of simulations.  Specifically, we
;;define ways to process simulation history
;;and produce dynamic analysis.
(ns marathon.analysis
  (:require [spork.util.table      :as tbl]
            [marathon.ces [core     :as core]
                          [engine :as engine]]
            [clojure.core.reducers :as r]
            [spork.sim.simcontext  :as sim]))

;;#Move these into core...#
(defn ->simreducer [stepf init]  
  (r/take-while identity (r/iterate (fn [ctx]
                                       (when  (engine/keep-simulating? ctx)
                                          (let [init ctx
                                                t  (sim/get-time ctx)
                                                processed  (stepf t  ctx)
                                                nxt        (sim/advance-time processed)]
                                            (with-meta nxt {t {:start init
                                                               :end processed}}))))
                                    init)))

;;A wrapper for an abstract simulation.  Can produce a sequence of
;;simulation states; reducible.
(defn ->simulator [stepf seed]
  (let [simred (->simreducer stepf seed)]
    (reify     
      clojure.lang.Seqable 
      (seq [this]  
        (take-while identity (iterate (fn [ctx] 
                                        (when  (engine/keep-simulating? ctx)
                                          (let [init ctx
                                                t  (sim/get-time ctx)
                                                processed  (stepf t  ctx)
                                                nxt        (sim/advance-time processed)]
                                            (with-meta nxt {t {:start init
                                                               :end processed}}))))
                                      seed)))
      clojure.core.protocols/CollReduce
      (coll-reduce [this f1]   (reduce f1 simred))
      (coll-reduce [_ f1 init] (reduce f1 init simred)))))

(defn ->history [tfinal stepf init-ctx]
  (->> init-ctx
       (->simulator stepf)
       (map (fn [ctx] [(core/get-time ctx) ctx]))
       (take-while #(<= (first %) tfinal))
       (into {})))

(defn ending [h t] (get (meta (get h t) :end)))
(defn start  [h t] (get (meta (get h t) :start)))

(defn ->collect-samples [f h]
  (let [ks    (sort (keys h))
        pairs (partition 2 1 ks)]
    (into (vec (apply concat
                    (->> pairs
                         (mapcat (fn [[l r]]
                                   (let [ctx (get h l)]
                                     ;;sample in between...
                                     (for [t (range l r)]
                                       (f t ctx))))))))
          (f (last ks) (get h (last ks))))))

;;dumb sampler...probably migrate this to
;;use spork.trends sampling.
(defn ->location-samples   [h]  (->collect-samples core/locations   h))
(defn ->deployment-samples [h]  (->collect-samples core/deployments h))
(defn tsv->csv [path]
  (with-open [rdr  (clojure.java.io/reader (str path))
              wrtr (clojure.java.io/writer (str path ".csv"))]
    (doseq [^String ln (line-seq rdr)]
      (.write wrtr (str (clojure.string/replace ln \tab \,) \newline)))))

;;this is basically the api for performing a run....
(defn spit-history! [h path]
  (let [lpath (str path "locsamples.txt")
        dpath (str path "depsamples.txt")]        
    (do (println [:spitting-locations lpath])
        (tbl/records->file (->location-samples h) lpath)
        (println [:spitting-deployments dpath])
        (tbl/records->file (->deployment-samples h) dpath))))



;;we need to create a pipeline that allows us

;;Right now, we're looking
;;Actual output from a Marathon run will include....

;;
(comment 

)
