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
            [spork.util.reducers]
            [clojure.pprint :refer [pprint]]
            [marathon [project  :as proj]]
            [marathon.project [linked :as linked]
                              [excel  :as xl]]
            [spork.entitysystem
             [diff     :as diff]
             [store :as store]]
            [spork.sim.simcontext     :as sim]
            [marathon
             [schemas   :as schemas]
             [observers :as obs]
             [serial    :as ser]
             [util      :as util]]))

;;ripped from clojure.core.reducers temporarily...
(defn- do-curried
  [name doc meta args body]
  (let [cargs (vec (butlast args))]
    `(defn ~name ~doc ~meta
       (~cargs (fn [x#] (~name ~@cargs x#)))
       (~args ~@body))))

(defmacro  defcurried
  "Builds another arity of the fn that returns a fn awaiting the last
  param"
  [name doc meta args & body]
  (do-curried name doc meta args body))
;;Note: there's a problem with the compile-time trick here...
;;in-ns, used in spork.util.reducers, actually produces
;;Huh...well, we'll have to cop this.
;;we're going to add in iterate, range, and friends
;;Reducers patch for Clojure courtesy of Alan Malloy, CLJ-992, Eclipse Public License
(defcurried r-iterate
  "A reducible collection of [seed, (f seed), (f (f seed)), ...]"
  {:added "1.5"}
  [f seed]
  (reify
    clojure.core.protocols/CollReduce
    (coll-reduce [this f1] (clojure.core.protocols/coll-reduce this f1 (f1)))
    (coll-reduce [this f1 init]
      (loop [ret (f1 init seed), seed seed]
        (if (reduced? ret)
          @ret
          (let [next (f seed)]
            (recur (f1 ret next) next)))))
    
    clojure.lang.Seqable
    (seq [this]
      (seq (clojure.core/iterate f seed)))))


;;#Move these into core...#
(defn ->simreducer [stepf init]  
  (r/take-while identity (r-iterate (fn [ctx]
                                       (when  (engine/keep-simulating? ctx)
                                          (let [init ctx
                                                t  (sim/get-time ctx)
                                                processed  (stepf t  ctx)
                                                nxt        (sim/advance-time processed)]
                                            (with-meta nxt {t {:start init
                                                               :end processed}}))))
                                    init)))

;;I think we want to convert this into a stream with the simulation
;;state.  So, instead of just [t ctx], we get [t ctx :begin|:end]
;;That way, other streams can filter on either begin/end or use both.

;;A wrapper for an abstract simulation.  Can produce a sequence of
;;simulation states; reducible.
(defn ->simulator [stepf seed]
  (let [simred (->simreducer stepf seed)]
    (reify     
      clojure.lang.Seqable 
      (seq [this]  
        (take-while identity (iterate (fn [ctx] 
                                        (when  (engine/keep-simulating? ctx)
                                          (let [init       ctx
                                                t          (sim/get-time     ctx)
                                                processed  (stepf t          ctx)
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
                    
;;most metrics should be collected at the end of the
;;day.  For debugging and verification purposes, we'd
;;like to have the history at the beginning of each day.
;;We technically provide access to both via the history stream.
;;we embed the previous day's sample in the meta.
(defn end-of-day-history [h]
  (->> h
       (map #(first (meta (second %))))
       (filter identity)
       (map (fn [[t {:keys [start end]}]] 
              [t end]))))

(defn expanded-history [h]
  (mapcat (fn [[t ctx]]
            (let [{:keys [start end]} (get (meta ctx) t)]
              [[t start  :start]
               [t end :end]])) h))

;;Note: we probably want to vary the resolution
;;here, currently we're hardwired to sample
;;every day.  Based on the post-processing
;;libs, we don't have to do this, and can use
;;discrete event sampling to compute a minimal
;;set of samples, then down-sample / upsample  based on that.

;;We can speed this up by not using for/range..
;;It's not a huge bottleneck at the moment...
(defn ->map-samples [f h]
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

;;#OPTIMIZATION
;;my gut says we can do this more efficiently, with regards to
;;final, since it holds onto the tail of the stream
;;Also, allow sampling rate to vary...
;;currently we sample every day.
;;Changed to reflect end-of-day sampling.
(defn ->seq-samples [f kvs]
  (let [pairs (partition 2 1 kvs)
        ;final (last kvs) ;no longer necessary, possible memory leak.
        ]
    (apply concat
       (->> pairs
            (mapcat (fn [[ [l ctx] [r nxt] ]]
                      ;;sample in between...
                      (for [t (range l r)]
                        (f t ctx))))))))

(defn ->collect-samples [f h]
  (cond (seq? h) (->seq-samples f h)
        (map? h) (->map-samples f h)
        :eles (throw (Exception. (str "Dunno how to do samples with " (type h))))))

;;Canonical Sampling Functions
;;============================

;;These samplers transform a stream of
;;history into a stream of sampled output.
;;We typically have a set of canonical outputs
;;we expect for post-processing.  This lets us
;;define them using higher-level sequence functions
;;so we can define sampling based on the "history"
;;rather than imperatively logging and myopically
;;processing.

;;It'd be nice to define a channel-based version of this so that
;;we can compute samples state by state...
;;at any given point in time, we have n samples we're collecting.
;;as we transition through the state stream, we run each sampler
;;to see if it computes a sample.

;;Some samples are event-based...
;;like dwell-before deployment stats...

;;Can we just append a deployment record? 
;;dumb sampler...probably migrate this to
;;use spork.trends sampling.
(defn ->location-samples   [h]  (->collect-samples core/locations   h))
;;I think this is similar to demand-trends.
(defn ->deployment-samples [h]  (->collect-samples core/deployments h))
;;compute the deployments table

;;so, we basically just pipe th deployments component
;;to out and concat....

;;derives a stream of deployments across the history.
;;daily deployments are stored in the :deployments component.
;;so we just extract that and boom.
(defn ->deployment-records  [h]
  (->> h 
       (mapcat (fn [[t ctx]]
                 (when-let [deps (store/get-domain ctx :deployments)]
                    (first (vals deps)))))
       (filter identity)
       (map-indexed (fn [idx d] (assoc d :DeploymentID idx)))))


;;note: we may need to replicate the audit trail for completeness
;;sake....this should be fairly easy...it's simple io stuff.

;;we only need to capture this when demands change..
	   

;;If we can define trends as a map
;;or a reduction....
;;this is legacy support...
;;Note: this should work with our 
(defn demand-trends
  ([t ctx]
   (let [qtr (unchecked-inc (quot t 90)) ;;1-based quarters.         
         changes (store/gete ctx :demand-watch :demands)
         actives (store/gete ctx :DemandStore :activedemands)]
     (when (seq changes)
       (->> actives
            (keys)
            (map #(store/get-entity ctx %))
            (map  (fn [{:keys [category demandgroup operation vignette Command] :as d}]
                    (let [assigned     (:units-assigned    d)
                          overlapping  (:units-overlapping d)
                          ua           (count              assigned)
                          uo           (count              overlapping)
                          compo-fills  (->> assigned
                                            (keys)
                                            (map (fn [nm]
                                                   (store/gete ctx nm :component)))
                                            (frequencies))
                        {:strs [AC RC NG Ghost]
                         :or   {AC 0 RC 0 NG 0 Ghost 0}} compo-fills]                      
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
                       :ACFilled      AC
                       :RCFilled      RC
                       :NGFilled      NG
                       :GhostFilled   Ghost
                       :OtherFilled   (- ua (+ AC RC NG Ghost))}
            )))))))
  ([ctx] (demand-trends (sim/get-time ctx) ctx)))

(defn ->demand-trends      [h]  (->collect-samples demand-trends h))

;;creating legacy output from basic data..
;;fills are a join of unit<>demanddata<>deployments

;; (def fillrecord {:Unit      :text
;;                  :category :text
;;                  :DemandGroup :text
;;                  :SRC :text
;;                  :FillType :text
;;                  :FollowOn :boolean
;;                  :name :text
;;                  :Component :text
;;                  :operation :text
;;                  :start :int
;;                  :DeploymentID :int
;;                  :duration :int
;;                  :dwell-plot? :boolean
;;                  :DwellYearsBeforeDeploy :float
;;                  :DeployDate :text
;;                  :FollowOnCount :int
;;                  :AtomicPolicy :text
;;                  :Category :text
;;                  :DeployInterval :int
;;                  :fill-type :text
;;                  :FillPath :text
;;                  :Period :text
;;                  :unitid :int
;;                  :deltat :int
;;                  :Demand :text
;;                  :PathLength :int
;;                  :OITitle :text
;;                  :BogBudget :int
;;                  :CycleTime :int
;;                  :DeploymentCount :int
;;                  :DemandType :text
;;                  :quantity :int
;;                  :end :int
;;                  :FillCount :int
;;                  :Location :text
;;                  :location :text
;;                  :compo :text     
;;                  :DwellBeforeDeploy :int
;;                  :Policy :text
;;                  :sampled :boolean
;;                  })


;;API Definition
;;==============
;;This is the core of doing a "run"...
(defn load-context
  "Given a viable Marathon Project, p, we derive and initial 
   simulation context, from which we can create a simulation
   history.  An optional function table-xform may be supplied 
   to pre-process the project tables, to implement options 
   like filtering, etc."
   [p & {:keys [table-xform] :or {table-xform identity}}]
   (->>  (setup/simstate-from 
          (table-xform (:tables (proj/load-project p)))
          core/debugsim)  
         (sim/add-time 1)
         (sim/register-routes obs/default-routes)))

(defn as-context [x & {:keys [table-xform]
                       :or {table-xform identity}}]
  (cond (string? x) (load-context x :table-xform table-xform)
        (util/context? x) x
        :else (throw (Exception.
                      (str "Invalid MARATHON sim context " x)))))
        
(defn marathon-stream
  "Create a stream of simulation states, indexed by time."
  [path-or-ctx & {:keys [tmax table-xform] :or {tmax 5001 table-xform identity}}]
  (->> (as-context path-or-ctx :table-xform table-xform)
       (->history-stream tmax engine/sim-step)
       (end-of-day-history)))

;;simple-project xforms               
(defn filter-srcs
  "Given a sequence of srcs to keep, pre-processes the project tables 
   to retain only records with an associated :SRC value, where the value 
   is in the set defined by srcs.  Defaults to filtering supply and demand 
   records."
  [srcs & {:keys [tables]
                             :or {tables [:SupplyRecords :DemandRecords]}}]
  (let [srcs        (set srcs)
        tbl-filter #(spork.util.table/filter-records (fn [r]  (srcs (:SRC r))) %)]
      (fn [tbls]
        (reduce (fn [acc t] (update acc t tbl-filter)) tbls tables))))

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
    
;;since components are maps...we can recursively diff to see which
;;entities changed.

;;If we constrain all access to go through assoce, etc,
;;then we can get away with diffing...
(defn diff-store [l r]
  (let [le (:store (sim/get-state l))
        re (:store (sim/get-state r))]
    (if (identical? (:domain-map le) (:domain-amp re))
      nil
      (diff/entity-diff le re))))

;;we might have a memory leak here if we're force the first and traversing the
;;rest of the history...
(defn patch-history [h]
  {:init    (first h)
   :patches (for [[[t1 l] [t2 r]] (partition 2 1 h)]
              [t2 (diff/entity-diffs->patch (diff-store l r))])})

(defn     write-history  [h path]  (ser/freeze-to (patch-history h)  path))
(defn     write-history! [h path]  (ser/freeze-to! (patch-history h) path))

(defmacro with-print [{:keys [level length]} & body]
  `(let [before-level# ~'*print-level*
         before-length# ~'*print-length*
         lvl#    ~level
         length# ~length]
    (do (set! ~'*print-level*  lvl#)
        (set! ~'*print-length* length#)
        ~@body
        (set! ~'*print-level*  before-level#)
        (set! ~'*print-length* before-length#))))

;;textual, printed version
;;if we use pprint, we get killed here.
(defn print-history [h path]
  (with-print {}
    (with-open [writer (clojure.java.io/writer path)]
      (binding [*out* writer]
        (let [{:keys [init patches]} (patch-history h)]
          (println "{:init")
          (pr init)
          (println " :patches")
          (doseq [[t patches] patches]
            (println "[" t)
            (pr patches)
            (println "]"))
          (println "}"))))))

(defn print-patches [h path]
  (with-print {}
    (with-open [writer (clojure.java.io/writer path)]
      (binding [*out* writer]
        (let [{:keys [init patches]} (patch-history h)]
          (println "{:patches ")
          (doseq [[t patches] patches]
            (println "[" t)
            (pr patches)
            (println "]"))
          (println "}"))))))

;;hmmm...can we actually slurp this up?  It's 28 mb...so maybe...
;;ahh...this poorly named...
(defn string->history [path]
  (println [:warning 'read-history "you're using read-string, vs. clojure.edn/read-string"])
  (read-string (slurp path)))
  
(defn read-history! [path]
  (let [{:keys [init patches]} (ser/thaw-from path)
        store  (atom (second init))]
    (into [init]
          (map (fn [[t patch]]
                 (let [prev @store
                       nxt  (diff/patch->store prev patch)
                       _    (reset! store nxt)]
                   [t nxt]))
               patches)
          )))

;;hmm...

;;IO  Routines
;;============

;;can we spit out demandtrends?
;;Yes....
;;They're basically location-samples...

;;note: if we stream, we don't compress, but we do
;;get a differential compression. We might
;;use lz4 to compress after the fact....once the
;;file has been written.  For now, it's about 28 mb for
;;a patch set for 13 years.

;;this is basically the api for performing a run....
(defn spit-history! [h path]
  ;;hackneyed way to munge outputs and spit them to files.
  (let [hpath      (str path "history.lz4"   )
        lpath      (str path "locsamples.txt")
        dpath      (str path "depsamples.txt")
        deploypath (str path "deployments.txt")
        dtrendpath (str path "DemandTrends.txt") ;probably easier (and lighter) to just diff this.
        ]        
    (do (println [:saving-history hpath])
        (println [:fix-memory-leak-when-serializing!])
        ;(write-history h hpath)
        (println [:spitting-locations lpath])
        (tbl/records->file (->location-samples h) lpath )
        (println [:spitting-deployed-samples dpath])
        (tbl/records->file (->deployment-samples h) dpath)
        (println [:spitting-deployments deploypath])
        (tbl/records->file (->deployment-records h) deploypath
          :field-order schemas/deployment-fields)
        (println [:spitting-demandtrends dtrendpath])
        (tbl/records->file (->demand-trends h) dtrendpath
           :field-order schemas/demandtrend-fields))))

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


(comment 
;;testing
(def ep "C:\\Users\\tspoon\\Documents\\srm\\notionalbase.xlsx")
;;local diff.
(def ep "C:\\Users\\1143108763.C\\srm\\notionalbase.xlsx")

(def ctx (load-context ep))

(def h (take 2 (marathon-stream  ep)))
(def l (first  h))
(def r (second h))


    
    
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

;;obe
(comment
  (def demand-trend-schema
  {:t  	           :int
   :Quarter	   :int ;;derived...
   :SRC	           :text
   :TotalRequired  :int
   :TotalFilled	   :int
   :Overlapping	   :int
   :Deployed	   :int
   :DemandName	   :text
   :Vignette 	   :text
   :DemandGroup	   :text
   :ACFilled	   :int
   :RCFilled	   :int
   :NGFilled	   :int
   :GhostFilled	   :int
   :OtherFilled	   :int})

(def dt-fields   [:t  	           
                  :Quarter	   
                  :SRC	           
                  :TotalRequired   
                  :TotalFilled	   
                  :Overlapping	   
                  :Deployed	   
                  :DemandName	   
                  :Vignette 	   
                  :DemandGroup	   
                  :ACFilled	   
                  :RCFilled	   
                  :NGFilled	   
                  :GhostFilled	   
                  :OtherFilled])
)
