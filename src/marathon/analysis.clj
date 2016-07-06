;;Defines utilities and workflows for
;;performing higher-order analysis
;;of simulations.  Specifically, we
;;define ways to process simulation history
;;and produce dynamic analysis.
(ns marathon.analysis
  (:require [spork.util.table       :as tbl]
            [spork.entitysystem [store :as store]]
            [marathon.ces [core     :as core]
                          [engine   :as engine]
                          [setup    :as setup]
                          ]
            [clojure.core.reducers :as r]
            [marathon.project  :as proj]
            [marathon.project [linked :as linked]
                              [excel :as xl]]
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

;;serializing all the snapshots is untenable...
;;can we compute diffs?
;;All we really care about, as we traverse forward,
;;is information regarding who changed...
;;So if any entity was touched or updated during the
;;t, the it'll show...
;;In theory, any last-updates to entities
;;will show up....so that limits our diffs
;;to the entities with last-update components..
;;From there, we can just compare them with their previous selves...

;;The goal here is to easily serialize our entity database...
;;Note...we have some options for how we do this...
;;We could do an initial state + diffs (similar to
;;git...) and save our stuff that way.  For now we
;;have a stream of state snapshots which have internal
;;references via persistent structures....so...
;;we should? be able to persist our stuff efficiently.
;;We're going to stream this rather than do it all in
;;memory...we can also add a diff buffer that can
;;be serialized at the end of the day...
;;So, anytime an entity is modified (via gete adde
;;assoce, etc.), the diff buffer (or dirty flag)
;;gets mutated in the db.  Then we compare dirty
;;entities with their previous versions to see
;;what the differences are...seems plausible...
;;the brute-force approach is to just use
;;hashing to compare...assuming we have hash
;;equality, we just hash-compare the stores, and
;;then the components in the stores, and then
;;the entities...
;;probably makes more sense to diff the components...
;;structural diffing is a pretty powerful way to
;;compute deltas...and laid back.  It "would" be
;;nice if we'd cached the values though.
(defn diff-stores [l r]
  (let [lcomps (-> l :state :store :domain-map)
        rcomps (-> r :state :stote :domain-map)]
    ;;many components will be the same..
    ;;man, we can actually save time if the hash hasn't been computed yet...
    (if (identical? lcomps rcomps) nil
        (reduce-kv (fn [acc lk lv]
                     (if-let [rv (get rcomps lk)]
                       (if (not (identical? lv rv))
                         (conj acc lk) acc) (conj acc lk))) [] lcomps))))
    
(defn freeze-history [h path]
  (ser/freeze-to! h path)
  )


;;hmm...


(comment 
;;testing
(def ep "C:\\Users\\tspoon\\Documents\\srm\\notionalbase.xlsx")

(def h (take 2 (marathon-stream :path ep))))
(def l (first  h))
(def r (second h))

;;can we perform an efficient entity-diff in the entity-store
;;library?, or do we have to go the long route?
;;Also...how long is the long route...

;;Also, if we diff this way, how does that play out when
;;we're using mutation?
;;Since it's a store, we really just care about which keys
;;changed in the component/entity mappings.
;;That's what we'll serialize.  Should cut down on our
;;problems significantly.  And, we can cache diffs in the
;;future to make this easier, maybe.
(defn diff-map [ls rs]
    (if (identical? ls rs)
      nil
      (let [lks (set (keys ls))
            rks (set (keys rs))
            ;;if anything is missing from either, we know
            ;;they are different.
            only-ls  (clojure.set/difference   lks rks)
            only-rs  (clojure.set/difference   rks lks)
            shared   (clojure.set/intersection lks rks) ;we only need to scan shared.
            changes  (reduce (fn [acc k]
                               (let [lv (get ls k)
                                     rv (get rs k)]
                                 (if (identical? lv rv) acc
                                     (conj acc [k [lv rv]]))))
                             []
                             shared)]        
          {:dropped only-ls
           :changed changes
           :added   (map (fn [c] [c (get rs c)]) only-rs)})))

(defn entity-diff [lstore rstore]
  (let [{:keys [dropped changed added]} (diff-map (:domain-map lstore) (:domain-map rstore))]
    {:dropped dropped
     :changed   (for [[c [old new]]  changed] ;by component, find changes.  These are basically instructions.
                  [c (diff-map old new)])
     :added added}))

;;since components are maps...we can recursively diff to see which
;;entities changed.

;;If we constrain all access to go through assoce, etc,
;;then we can get away with diffing...
(defn diff-store [l r]
  (let [le (:store (sim/get-state l))
        re (:store (sim/get-state r))]
    (if (identical? (:domain-map le) (:domain-amp re))
      nil
      (diff-map (:domain-map le) (:domain-map re)))))

;;component add -> entity-add..
;;{:assoc-entity     [entity component value]
;; :dissoc-entity    [entity component]}
;;to update our entitystore.
(defn drops->patches [drops]
  (map (fn [d] {:drop-domain d}) drops))
(defn adds->patches [adds]
  (for  [[component es] adds
         [e v] es]
    {:add [e component v]}))

(defn changes->patches [changes]
  (for  [[component diffs] changes
         [e {:keys [dropped changed added]}]  diffs]
    (apply concat
           (map (fn [v] {:drop [e component]}) dropped)
           (map (fn [v] {:add  [e component v]}) added)
           (map (fn [[old new]] {:add [e component new]}) changed))))

(defn entity-diffs->patch [{:keys [dropped changed added]}]
  (apply concat (drops->patches dropped)
                (adds->patches added)
                (changes->patches changed)))

;;apply a patch to an existing store to get the
;;next store.
(defn patch->store [init patches]
  (reduce (fn [acc patch]
            (if-let [addition (:add patch)]
              (let [[e c v] addition]
                (store/assoce acc e c v))
              (store/drop-domain acc (:drop patch))))
          init patches))
        
(defn patch-history [h]
  (for [[[t1 l] [t2 r]] (partition 2 1 h)]
    [t2 (changes->patches (diff-store l r))]))
    

)

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

