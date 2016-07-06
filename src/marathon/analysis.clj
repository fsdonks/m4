;;Defines utilities and workflows for
;;performing higher-order analysis
;;of simulations.  Specifically, we
;;define ways to process simulation history
;;and produce dynamic analysis.
(ns marathon.analysis
  (:require [spork.util.table       :as tbl]
            [marathon.ces [core     :as core]
                          [engine   :as engine]
                          [setup    :as setup]
                          ]
            [clojure.core.reducers :as r]
            [marathon.project  :as proj]
            [spork.sim.simcontext  :as sim]
            [marathon.serial :as ser]))

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


(defn ->history-stream [tfinal stepf init-ctx]
  (->> init-ctx
       (->simulator stepf)
       (map (fn [ctx] [(core/get-time ctx) ctx]))
       (take-while #(<= (first %) tfinal))
       ))

;;Now using transducers.
(defn ->history [tfinal stepf init-ctx]
  (into {} (comp (map (fn [ctx] [(core/get-time ctx) ctx]))
                 (take-while #(<= (first %) tfinal)))        
        (->simulator stepf init-ctx)))

(defn ending [h t] (get (meta (get h t) :end  )))
(defn start  [h t] (get (meta (get h t) :start)))

;;We can speed this up by not using for/range..
;;It's not a huge bottleneck at the moment...
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

;;can we spit out demandtrends?
;;Yes....
;;They're basically location-samples...
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

;;spits a log of all the events passing through.
(defn spit-log
  ([h root nm]
   (println [:logging-to (str root nm)])
   (with-open [wrtr (clojure.java.io/writer (str root nm))]
     (binding [*out* wrtr]
       (core/debugging
        (doseq [hd h]
          )
        ))))
  ([h root] (spit-log h root "events.txt")))

;;spits a verbose log of all the events and
;;behavioral updates that are performed...
(defn spit-log!
  ([h root nm]
   (println [:logging-to (str root nm)])
   (with-open [wrtr (clojure.java.io/writer (str root nm))]
     (binding [*out* wrtr]
       (core/debugging!
        (doseq [hd h]
          )
        ))))
  ([h root] (spit-log! h root "events.txt")))

(defn compare-lines [l r]
  (with-open [left  (clojure.java.io/reader l)
              right (clojure.java.io/reader r)]
    (let [ls (line-seq left)
          rs (line-seq right)]
      (->> (map (fn [l r] [l r]) ls rs)
           (map-indexed (fn [i [x y]]
                          {:line i
                           :l x
                           :r y})
                        )
           (filter (fn [{:keys [l r]}]
                     (not= l r)))
           (first)))))

(defn compare-lines! [l r]
  (with-open [left  (clojure.java.io/reader l)
              right (clojure.java.io/reader r)]
    (let [ls (line-seq left)
          rs (line-seq right)]
      (->> (map (fn [l r] [l r]) ls rs)
           (map-indexed (fn [i [x y]]
                          {:line i
                           :l x
                           :r y})
                        )
           (filter (fn [{:keys [l r]}]
                     (not= l r)))
           (vec)
           ))))


;;API Definition
;;==============


;;This is the core of doing a "run"...
(defn load-context
  "Given a viable Marathon Project, p, we derive and initial 
   simulation context, from which we can create a simulation
   history."
  [p]
  (->>  (setup/simstate-from 
         (:tables (proj/load-project p))
         core/debugsim)
        (sim/add-time 1)))

(defn marathon-stream
  "Create a stream of simulation states, indexed by time."
  [& {:keys [path tmax] :or {tmax 5001}}]
  (->> (load-context path)
       (->history-stream tmax engine/sim-step)))

;;The goal here is to easily serialize our entity database...
;;Note...we have some options for how we do this...
;;We could do an initial state + diffs (similar to
;;git...) and save our stuff that way.  For now we
;;have a stream of state snapshots which have internal
;;references via persistent structures....so...
;;we should? be able to persist our stuff efficiently.
(defn freeze-history [h path]
  (ser/freeze-to h path)
  )
  
;;testing
(def ep "C:\\Users\\tspoon\\Documents\\srm\\notionalbase.xlsx")



;;We can compare the event logs too...
;;See where history diverges.
;; (defn divergence [lh rh]
;;   (map (fn [l r]
;;          {:t (sim/get-time l)
;;          (core/locations l)
;;          (core/location  r)
;;           (seq lh) (seq rh)
  
;;we need to create a pipeline that allows us

;;Right now, we're looking
;;Actual output from a Marathon run will include....

;;
(comment 

)
