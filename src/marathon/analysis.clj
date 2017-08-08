;;Defines utilities and workflows for
;;performing higher-order analysis
;;of simulations.  Specifically, we
;;define ways to process simulation history
;;and produce dynamic analysis.
(ns marathon.analysis
  (:require [spork.util [table       :as tbl]
                        [io :as io]
                        [stream :as stream]
                        [serial :as ser]
                        [metaprogramming :as util]
             ]
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
            [spork.sim [simcontext     :as sim]
                       [history :as history]]
            [marathon
             [schemas   :as schemas]
             [observers :as obs]]))

(comment
;;ripped from clojure.core.reducers temporarily...
(defn- do-curried
  [name doc meta args body]
  (let [cargs (vec (butlast args))]
    `(defn ~name ~doc ~meta
       (~cargs (fn [x#] (~name ~@cargs x#)))
       (~args ~@body))))

(defn try-seq
  "Tries to coerce x to a seq, returning nil if unable.
   Useful wrapper around seq, since some fundamental types
   like String and others cannot implement seq and are 
   handled via hardcoded dispatch."
  [x]
  (try (seq x)
       (catch Exception e nil)))

(defmacro juxt-map [m]
  `(fn [v#]
      (reduce-kv (fn [acc# k# f#]
                   (assoc acc# k# (f# v#)))  {}
                   ~m)))

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

(defn merge-meta [obj m]
  (with-meta obj (merge (get meta obj) m)))

;;#Move these into core...#
(defn ->simreducer [stepf init]
  (r/take-while identity (r-iterate (fn [ctx]
                                       (when  (engine/keep-simulating? ctx)
                                          (let [init ctx
                                                t  (sim/get-time ctx)
                                                processed  (stepf t  ctx)
                                                nxt        (sim/advance-time processed)]
                                            (merge-meta nxt {:start-end {:t t
                                                                        :start init
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
                                            (merge-meta nxt {:start-end {:t t
                                                                        :start init
                                                                        :end processed}}))))
                                      seed)))
      clojure.core.protocols/CollReduce
      (coll-reduce [this f1]   (reduce f1 simred))
      (coll-reduce [_ f1 init] (reduce f1 init simred)))))

(defn ->history-stream [tfinal stepf init-ctx]
  (->> init-ctx
       (->simulator stepf)
       (map (fn [ctx] [(core/get-time ctx) ctx]))
       (take-while
        (fn [^clojure.lang.Indexed v]
          (<= (.nth v 0) tfinal)))
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
       (map #(:start-end (meta (second %))))
       (filter identity)
       (map (fn [{:keys [t start end]}]
              [t end]))))

(defn expanded-history [h]
  (mapcat (fn [[t ctx]]
            (let [{:keys [start end]} (get (meta ctx) :start-end)]
              [[t start  :start]
               [t end :end]])) h))

)

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
            (mapcat (fn [[[l ctx] [r nxt]]]
                      ;;sample in between...
                      (for [t (range l r)]
                        (f t ctx))))))))

(defn ->collect-samples [f h]
  (cond (seq? h) (->seq-samples f h)
        (map? h) (->map-samples f h)
        :eles (throw (Exception. (str "Dunno how to do samples with " (type h))))))


;;Multimethods for Emitting Frames and Saving them to Output
;;==========================================================
;;Given a source of emitted frames, we can
;;record the frames to an output file...
(defmulti init-frame-saver (fn [t path & {:keys [field-order]}] t))

;;Immediate, i.e. frame-by-frame outputs.
;;This is more efficient than our initial hack at
;;traversing the entire history for each output.
;;This way, we support incremental outputs built
;;over time.
(defmulti emit-frame (fn [t frm] t))

;;Implementations follow later...


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

;;Note: these change daily potentially.
(defn frame->location-samples [[t ctx]]
  (core/locations t ctx))
(defn frame->deployment-samples [[t ctx]]
  (core/deployments t ctx))

;;compute the deployments table

;;so, we basically just pipe th deployments component
;;to out and concat....
(defn frame->deployment-records [[t ctx]]
  (when-let [deps (store/get-domain ctx :deployments)]
    (first (vals deps))))
;;derives a stream of deployments across the history.
;;daily deployments are stored in the :deployments component.
;;so we just extract that and boom.
(defn ->deployment-records  [h]
  (->> h
       (mapcat frame->deployment-records)
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
   (let [qtr     (unchecked-inc (quot t 90)) ;;1-based quarters.
         changes (store/gete ctx :demand-watch :demands)
         finals  (store/gete ctx :DemandStore :finals)
         actives (store/gete ctx :DemandStore :activedemands)]
     (when (or (seq changes)
               (finals t))
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

;;this compute the demand-trends frame from the current frame.
;;useful idiom...This is a keyframe fyi.
(defn frame->demand-trends [[t ctx]]
  (demand-trends t ctx))


;;Note: locations are really "policypositions", should rephrase
;;this....otherwise we get :locationname confused.  We actually
;;do want policy-position changes....Another option is
;;for us to reconcile locations with known-locations
;;location is easy, we just track any entities with :location-delta
;;components...

;;Note: Cloned from quilsample.bridge, temporary
(defn delta-seq [s domain]
   (when-let [es (seq (store/get-domain s domain))]
     (for [[id {:keys [init current]}] es]
       [id init current])))

(defn location-changes [s] (delta-seq s :location-delta))
(defn position-changes [s] (delta-seq s :position-delta))
(defn state-changes    [s] (delta-seq s :state-delta))

;;Ideal solution is to port the incubated stuff in quilsample.bridge,
;;but for now, we'll stick with the dumb way.  Just get the
;;table computed....

;;replicating output for locations.txt
;;T, EntityFrom, EntityTo, EventName, Msg
(defrecord location-rec [T EntityFrom EntityTo EventName Msg])
(defn location-trends
  ([t ctx]
   (when-let [xs (location-changes ctx)]
     (for [[id from to] xs]
       (->location-rec t  id to  "Unit Moved"  ""))))
  ([ctx] (location-trends (sim/get-time ctx) ctx)))

(defn frame->location-records [[t ctx]] (location-trends t ctx))
(defn ->location-records [h]
  (mapcat frame->location-records h))


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

(defmacro or-get [m k & else]
  `(if-let [res# (get ~m ~k)]
    res#
    ~@else))

;;API Definition
;;==============
(defn load-project [p & {:keys [tables]}]
  (if tables
    (proj/load-project p :tables tables)
    (proj/load-project p)))

(defn load-requirements-project [p]
  (proj/load-project p :tables xl/marathon-requirements-schema))

;;Should we be running engine/initialize-sim here?
;;This is the core of doing a "run"...
(defn load-context
  "Given a viable Marathon Project, p, we derive and initial
   simulation context, from which we can create a simulation
   history.  An optional function table-xform may be supplied
   to pre-process the project tables, to implement options
   like filtering, etc.  The project may be a path - a string.  
   Additionally, an initial context may be supplied, perhaps 
   with custom observes for logging or graphics.  Otherwise 
   the typical debug simulation context will received any 
   setup and initialization.
   "
  [p & {:keys [table-xform lastday observer-routes init-ctx]
        :or {table-xform identity
             observer-routes obs/default-routes
             init-ctx core/debugsim}}]
   (->  (setup/simstate-from ;;allows us to pass maps in, hackey
         (table-xform (or-get p :tables
                              (:tables (proj/load-project p))))
         init-ctx)
        (engine/initialize-sim :observer-routes observer-routes
                               :lastday lastday)))

;TimeStamp	SRC	Reason
(defn load-audited-context
  "Like analysis/load-context, except we perform some io in the parent folder.
   Generates 'AUDIT_...' files from raw inputs and computed input."
  [path-or-proj & {:keys [root table-xform lastday observer-routes init-ctx events?]
                   :or {table-xform identity
                        observer-routes obs/default-routes
                        }}]
  (let [proj (proj/load-project path-or-proj)
        root (or root (proj/project-path proj))
        event-saver (when events?
                      (init-frame-saver
                       :event-log (str root "eventlog.txt") :strict? true)) 
        ctx  (load-context proj :table-xform table-xform
                                :lastday lastday
                                :observer-routes observer-routes
                                :init-ctx (or init-ctx
                                              (if event-saver
                                                (core/debug-by! core/debugsim
                                                                event-saver)
                                                core/debugsim)))
        scope-table (fn [m]
                      (let [t (System/currentTimeMillis)]
                        (->>  (for [[src reason] (seq m)]
                                {:TimeStamp t :SRC src :Reason reason})
                              (tbl/records->table))))
        computed-tables  (let [params   (core/get-parameters ctx)
                               inscope  (:SRCs-In-Scope      params)
                               outscope (:SRCs-Out-Of-Scope  params)]
                           {:InScope  (scope-table inscope)
                            :OutScope (scope-table outscope)})
        _    (proj/audit-project (proj/add-tables proj computed-tables)
                                 root
                                 :tables (into proj/default-auditing-tables
                                               (keys computed-tables)))]
    (if events?
      (with-meta ctx {:savers {:event-log event-saver}})
      ctx)))

(defn as-context
  "Coerces x to a marathon simulation context.  Optionally,
   will provide and audit-trail of information if x is
   a project and audit? is truthy."
  [x & {:keys [table-xform audit? audit-path events?]
                       :or {table-xform identity}}]
  (cond (string? x) (if audit?
                      (do (io/make-folders! audit-path ["audit.txt"])
                          (load-audited-context x :table-xform table-xform :root audit-path
                                                :events? events?))
                      (load-context x :table-xform table-xform))
        (core/context? x) x
        :else (throw (Exception.
                      (str "Invalid MARATHON sim context " x)))))

#_(defn marathon-stream
  "Create a stream of simulation states, indexed by time.
   Optionally, set the maximum simulation time, define transformations
   on the project tables, like src filters, provide a custom step function,
   and choose to generate auditing information upon initializing the
   stream."
  [path-or-ctx & {:keys [tmax table-xform step-function audit? audit-path events?]
                  :or {tmax 5001
                       table-xform identity
                       step-function engine/sim-step
                       audit? false}}]
  (let [ctx (as-context path-or-ctx :table-xform table-xform :audit? audit?
                   :audit-path audit-path :events? events?)] 
  (-> (->> ctx
           (->history-stream tmax step-function)
           (end-of-day-history))
      (merge-meta (meta ctx)))))

(defn marathon-stream ;;refactored.
  "Create a stream of simulation states, indexed by time.
   Optionally, set the maximum simulation time, define transformations
   on the project tables, like src filters, provide a custom step function,
   and choose to generate auditing information upon initializing the
   stream."
  [path-or-ctx & {:keys [tmax table-xform step-function audit? audit-path events?]
                  :or {tmax 5001
                       table-xform identity
                       step-function engine/sim-step
                       audit? false}}]
  (-> (as-context path-or-ctx :table-xform table-xform :audit? audit?
                        :audit-path audit-path :events? events?)
      (history/state-stream :tmax tmax
                            :step-function step-function
                            :keep-simulating? engine/keep-simulating?)))

(defn as-stream
  "Convenience wrapper around marathon-stream to use with downstream
  consumption of spork.sim.history api, where a stream of [t ctx] is
  expected.  Allows us to use marathon-stream as our stream producer."
  [x]
  (history/as-stream marathon-stream x))

(defn day-before-error
  "Given a sequence of frames, returns "
  [xs]
  (history/time-before-error xs))

(defn day-before
  "Given a sequence of frames, returns the closest context prior  
   to time t."
  [t xs]
  (->> xs
       (as-stream)
       (history/time-before)))

(defn day-of
  "Given a sequence of frames, returns the context prior to 
   evaluating the step-function at t."
  [t xs]
  (->> xs
       (as-stream)
       (history/time-of)))
    
(defn step-1
  "Take one step from the current context.  Useful for interactive 
   debugging or searching."
  [ctx]
  (-> ctx
      (engine/sim-step)
      (sim/advance-time)))

(defn step-prefill
  "Steps the context up to the point of filling demands.
   Essentially provides an updated state prior to selecting
   units for deployment.  Useful for verifying supply-queries
   and other fill-related rules."
  [ctx]
  (engine/parametric-sim-step ctx 
    :fill nil
    :end  nil))

(defn fill-of
  "Like day-of, except we fast forward to the point just before 
   filling.  Convenience function for debugging."
  [day ctx]
  (-> (day-of day ctx)
      (step-prefill)))

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

(defn frame-at
  "Fetch the simulation frame at or nearest (after) time t from a 
   sequence of [t ctx] frames xs."
  [t xs] (some (fn [[tc ctx]] (when (>= tc t) ctx))
               (as-stream xs)))

;;Entity Tracing and Debugging
;;============================
;;Migrated from quilsample.bridge

;;can we take a discrete entity history and create a
;;continuous entity history?  That's what the cycle-lerper
;;is supposed to be doing, but currently is failing a bit at.
(defn discrete-entity-history
  "Given a context and an entity id to follow, returns a 
   map of the discrete values of the entity's history 
   as a function of time.  Ensures that only inflections 
   where the entity history changes are captured. 
   Caller may supply an optional sample? function  
   to determine if frames should be dropped."
  [ctx id & {:keys [sample?] :or {sample?
                                  (fn [x] true)}}]
  (-> (as-stream ctx)
      (history/discrete-entity-history :sample? sample?)))

(def unit-entity-summary
  (juxt :t :locationname :positionpolicy
            :state marathon.supply.unitdata/unit-stats :location-delta))

(defn entity-trace
  "High level function for directing entity event and behavior 
   traces to *out*.  Allows us to walk through the entity's 
   behavior as it changes and see fine-grained event and 
   behavior messages about the entity, as well as its 
   discrete state changes."
  [ctx e & {:keys [debug? sample? trace]
            :or {debug? true sample? (fn [_] true) trace unit-entity-summary}}]
  (-> (as-stream ctx)
      (history/entity-trace  e
          :debug? debug? :sample? sample? :trace trace)))

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

(def all-outputs #{:history
                   :locsamples
                   :locations
                   :depsamples
                   :deployment-records
                   :demandtrends
                   :patches
                   :event-log})

(def legacy-outputs #{:deployment-records
                      :demandtrends
                      :locations
                      :event-log})

;;These are our canonical output funcitons.
(defmulti spit-output (fn [t h path] t))
(defmethod spit-output :history [t h hpath]
  (do  (println [:spitting-history hpath])
       (println [:fix-memory-leak-when-serializing!])
       (history/write-history h hpath)))

(defmethod spit-output :location-samples [t h lpath]
  (do (println [:spitting-location-samples lpath])
      (tbl/records->file (->location-samples h) lpath)))

(defmethod spit-output :locations [t h locspath]
  (do (println [:spitting-locations locspath])
      (tbl/records->file (->location-records h) locspath)))

(defmethod spit-output :deployed-samples [t h dpath]
  (do (println [:spitting-deployed-samples dpath])
      (tbl/records->file (->deployment-samples h) dpath)))

(defmethod spit-output :deployment-records [t h dpath]
  (do (println [:spitting-deployed-samples dpath])
      (tbl/records->file (->deployment-records h) dpath
         :field-order schemas/deployment-fields)))

(defmethod spit-output :demandtrends [t h dtrendpath]
  (do (println [:spitting-demandtrends dtrendpath])
      (tbl/records->file (->demand-trends h) dtrendpath
                         :field-order schemas/demandtrend-fields)))

;;default frame emission does nothing.
(defmethod emit-frame :default [t frm] nil)

;; (defmethod emit-frame :history [t frm]
;;   (do  (println [:spitting-history hpath])
;;        (println [:fix-memory-leak-when-serializing!])
;;        (write-history h hpath)))

;; (defmethod emit-frame :location-samples [t frm]
;;   (do (println [:spitting-location-samples lpath])
;;       (tbl/records->file (->location-samples h) lpath)))

(defmethod emit-frame :locations [t frm]
  (frame->location-records frm))

;; (defmethod emit-frame :deployed-samples [t h dpath]
;;   (do (println [:spitting-deployed-samples dpath])
;;       (tbl/records->file (->deployment-samples h) dpath)))

(defmethod emit-frame :deployment-records [t frm]
  (frame->deployment-records frm))

(defmethod emit-frame :demandtrends [t frm]
  (frame->demand-trends frm))

;;so, now we have the ability to manage multiple
;;record writers via stream/record-writer

;;We thus maintain some state that manages
;;traversing our history.
;;It should contain all the record-writers
;;we need for each type of frame.

(def field-orderings
  {:deployment-records schemas/deployment-fields
   :demandtrends       schemas/demandtrend-fields
   :event-log         [:t :type :from :to :msg] 
   ;;location records?
   })


;;Our default frame-saver is a record writer.  If we have
;;a field-order provided in field-orderings that matches the
;;type t, we'll use it.
(defmethod init-frame-saver :default [t path & {:keys [field-order strict?]}]
  (let [_ (println [:spitting t path])
        field-order (or field-order
                        (get field-orderings t))]
     (if strict?
       (stream/->strict-record-writer  path field-order)
       (stream/->record-writer path :field-order field-order))))

;;Following the idiom of providing "frames", we have frame->blah
;;functions to provide snapshot sampling, providing implementations
;;for emit-frame multimethods (for now).  Rather than traversing
;;the entire (unnecessarily cached) history, we instead build
;;history frame-by-frame.

;;We need targets to emit to...
;;And a specification of how to emit.


;;If we take a channel-based approach, we can leverage abstraction
;;and get the same functionality in an incremental fashion.
;;We end up inverting the flow of control a bit.
;;It'd be nice to take a sequence, like we have, and
;;coerce it to a channel.  This channel then conceptually
;;broadcasts the history sequence to any interested subscribers.

(def ^:dynamic *outputs* legacy-outputs)
(defmacro with-outputs [os & body]
  `(binding [~'marathon.analysis/*outputs* ~os]
     ~@body))
(defmacro with-all-outputs [& body]
  `(with-outputs ~all-outputs
     ~@body))

(def w (atom nil))

(defn spit-history!
  "Spits h - sequence of [t ctx] frames of simulation history -
   to path using establish output criteria via emit-frame,
   and init-frame-saver.  Incrementally writes history
   vs caching and traversing everything in memory.  Caller
   may supply a pre-defined map of savers to output, in which 
   case the saver will be used instead of the default action of 
   creating a new saver at the default output path."
  [h path & {:keys [outputs savers]
             :or   {outputs *outputs*}}]
  (let [paths {:history            (str (io/as-directory path) "history.lz4")
               :location-samples   (str (io/as-directory path) "locsamples.txt")
               :locations          (str (io/as-directory path) "locations.txt")
               :deployed-samples   (str (io/as-directory path) "depsamples.txt")
               :deployment-records (str (io/as-directory path) "AUDIT_Deployments.txt")
               :demandtrends       (str (io/as-directory path) "DemandTrends.txt")
               :event-log          (str (io/as-directory path) "eventlog.txt")}
        known-savers (get (meta h) :savers)
        _ (println known-savers)
        savers (merge savers known-savers )
        ;;collect all of our frame-grabbers into one state.
        frame-state (vec (for [[k path] paths
                                  :when (outputs k)]
                           (let [
                                 saver (or (get savers k)
                                           (init-frame-saver k path))]
                                {:name k
                                 :grab (fn grab [frm]
                                         (when-let [res (emit-frame k frm)]
                                           (if (and (not (map? res))
                                                    (coll? res))
                                               (doseq [r res] (saver r))
                                               (saver res))))
                                 :saver saver})))
        grab-frames!  (fn [frm] (reduce (fn [acc {:keys [name grab]}]
                                          (grab frm))
                                        nil frame-state))
        _ (reset! w (some #(when (= (:name %) :event-log) (:saver %)) frame-state))]
    (with-open [savers (io/->closer (map :saver frame-state))]
      (doseq [frm h]
        (do (println [:day (first frm)])
            (grab-frames! frm))))))

;;note:
;;spit-log! and spit-log moved to spork.sim.history
(util/import-vars 
   [spork.sim.history
    spit-log
    spit-log!])


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
