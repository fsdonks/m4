;;helmet is a tool the parses sample-queries relative to 
;;a set of records, validation rules, and sample-rules, 
;;and then samples from the records according to the sample-rules, 
;;validates the resultant set of sample records, and allows 
;;replications of the process.
(ns marathon.processing.helmet.core
  (:require [spork.cljgui.components [swing :as gui]]
            [spork.util [general :as gen]
                        [table :as tbl]
                        [stats :as stats]
                        [record :as rec]
                        [sampling :as sample]]
            [spork.util.excel [core :as xl]]
            [marathon.processing.helmet [split :as split] 
                                        [collision :as collision]]))

;;Overview
;;========

;;Helmet is a re-write of a stochastic demand sampling.  The legacy version 
;;was an unnamed tool, built in excel, that used a worksheet interface, and 
;;a VBA backend.  The VBA implementation parsed data from a set of tables that
;;encoded sampling rules, primarily mappings of distributions and data 
;;transformations to subsets of a corpus of data records.  The sponsor extended
;;the requirements after the tool had been built, and the original developer 
;;was no longer around.  Unfortunately, the implementation precluded simple 
;;extension to handle the non-trivial cases that the requirements change 
;;introduced.  Additionally, there were previously unknown errors in the 
;;implementation that surfaced during early attempts to capitalize on the 
;;previous effort and extend it.  
;;After roughly 3 weeks of wasted effort, I made the decision to implement the 
;;sampling language in spork.util.sampling to support sampling rules of 
;;arbitrary complexity.  That language now serves as the basis for the 
;;stochastic demand rule engine.  To support legacy users, I wrapped the 
;;legacy interface (with some improvements) that existed in Excel.  The new 
;;application, dubbed Helmet, is a wrapper around the sampling rules and "cases"
;;encoded in the legacy Excel format.  One should note: the sampling is a 
;;general purpose clojure library.  Helmet is merely a wrapper and a specialized
;;Excel data-munging tool to accomodate legacy users.  Callers may use Helmet 
;;from the standard Marathon GUI, or may use the library programatically.  

;;Differences from the Legacy Version
;;===================================

;;Helmet capitalizes on the fact that Clojure has a really powerful reader.  As
;;such, much of the "data" in the casebook (an Excel workbook with some required
;;tables) is actually a native clojure data structure (like a map or a vector).
;;I was able to simplify much of the case definition, and provide the ability 
;;to compose sampling rules to eliminate much of the redundancy in the previous
;;spec.  Legacy users seemed to like the new format.  There is a detailed 
;;description of new data fields and rule expression semantics, available on 
;;request.  Later versions may ignore Excel entirely, preferring simple clojure
;;scripts and database connections for portability.  

;;Typical Process
;;===============
;;When invoked as an application from the main Marathon GUI, the user is 
;;presented with a file selection dialogue asking for a casebook.  The 
;;path of the casebook is fed to the __xlsx->futures__ function, with default
;;arguments.  Assuming the casebook is well-formed, a set of stochastically 
;;generated demand futures, as defined by the demand data, the sampling rules, 
;;and the case data from the casebook, will be generated in the same working 
;;folder.  These futures are standard tables of tab-delimited txt, and can be
;;easily read or modified.  Alternative formats are feasible, but currently not
;;in demand.  

;;If a user wishes to perform the same process from the clojure REPL, one can
;;do so via invoking (xlsx->futures ...) on an appropriate workbook path.  Check
;;the __xlsx->futures__ docstring for more options for output configuration.

;;Required Data
;;=============


;;Cases
;;=====

;;The "Cases" table defines the name and global characteristics of active cases
;;to be sampled from.  Global characteristics include the number of futures to
;;generate, the random number seed to use, and constraints on duration and 
;;end-times.  Each enabled case in the "Cases" table must have a corresponding 
;;table (or worksheet) that defines the sampling rules for the case.  

;;Case Sampling Rules
;;===================
;;Each case has a table of sampling rules that define a sampling context, ala 
;;__spork.util.sampling__.  The rows or records of the case table contribute 
;;a sampling rule to the sampling corpus, so all of the records for a case are
;;parsed into a composite sampling rule for the entire case.  When a case is 
;;sampled according to these rules, the result of each rule - a sequence of 
;;demand records -- are concatenated into a single "future".  Each case will 
;;have n futures, as defined by the information in the Cases table.  Each future
;;will ultimately reside in a unique tab-delimited file (when default processing
;;is used).  If a caller desires to, they can use the library functions directly
;;and keep case information in-memory as spork.util.table structures, rather 
;;than emitting files.  This may be useful for later experimental processes, or 
;;search processes such as __marathon.processing.stoke__.  

;;The sampling rules are encoded in a tabular format, where each rule has a name, 
;;a frequency, a distribution to transform the "start" field of sampled records 
;;by, a distribution to transform the "duration" field of sampled records by, 
;;and a pool of rules to draw from.  Both the start and distribution fields have
;;accompying values of "S1, S2, S3", and "D1, D2, D3" .  These are remnants of 
;;the legacy incoding, and imply the paramters to be sent to the distribution 
;;named in the associated "... Distribution" field.  This encoding only covers
;;the cases needed for Helmet, but is sufficient and conforms to the legacy 
;;design.

;;Encoding Pools of Choices
;;=========================

;;The pool of rules is either a clojure vector or a clojure map.  Clojure 
;;vectors, denoted by [...] imply a random choice with even probability amongst 
;;every rule in the sequence.  Users may enter multiple identical values for a 
;;vector pool, in which case the result is akin to an empirical distribution.  
;;Users may also prepend the vector sequence with the :every keyword,  
;;[:every ...]  to imply that, rather than a uniform choice, every rule in the 
;;pool is to be sampled and concatenated.  

;;Clojure maps, denoted by {rule1 n1, rule2 n2, ...} imply a weighted choice, 
;;with probability denoted by the numerical values associated with each rule.  
;;While a preferred convention, numerical values need not sum to 1.0 - they will
;;be normalized by default.  One may encode an empirical distribution by 
;;weighting the rules in the map with the number of observations.

;;Validation Rules : Dependency Classes and Prioritization
;;========================================================
;;Validation covers two depenendent phenomena: prioritzation of demands, and 
;;the desire to resolve "collisions" between classes of prioritized demands.  
;;The "ValidationRules" table contains a dependency class, a priority, and a 
;;minimum time rule.  Dependency classes, when present, encode a prioritization 
;;between concurrent demands, if and only, the concurrent demands also have 
;;a dependency class.  Concurrent dependent demands, or collisions, are then 
;;resolved based on the rules in __marathon.processing.helmet.collision__ .  In
;;general, demands are either merged or split, depending on priority, to resolve
;;collisions.  Demands with no associated dependency class are left untouched.  

;;Demand Splitting
;;================
;;One orthogonal requirement that emerged was the ability to split a group of 
;;demands according to context-specific rule-sets, where the "splitting" 
;;operation implied bifurcating demand records based on some notion of 
;;cumulative time relative to the group of demand records.  I decided to split
;;this into a final processing step, separate from the sampling rules.  Once 
;;samples are determing, if any splitting needs to be done - to identify "early"
;;demands in a logical group of demands [presumably for special policies] - 
;;then we split the demand records according to information in the DemandSplit 
;;table.  Where demand records map to a DemandSplit rule, the rule will detail
;;when a set of demands should be "split."  Splitting occurs relative to the 
;;earliest demand in the group of demand records sharing the split rule.  If 
;;the split-day, the day since the start of the earlist demand in the group, 
;;occurs during any demands, then the intersecting demands are bifurcated.  
;;Birfurcation creates two records, one defined up to the split, where the 
;;data is identical except for the duration.  Any records occuring after the 
;;split have - currently - "-Rotational" appened to their "SourceFirst" fields,
;;as dictated by the legacy methodology.

;;Demand Records
;;==============
;;The "DemandRecords" table is effectively the sampling corpus for the entire 
;;set of sampling rules, across all cases.  Upon initialization, a sampling 
;;context is defined, where demand records are grouped into sampling rules by 
;;their "Group" field.  These rules are therefore available for use in composing
;;the rules found in the __Case Sampling Rules__ and the pools.  The other field
;;of consequence is the "DemandSplit" field, which is used for the final 
;;processing step.



;;Implementation
;;==============

;;Most of the following implementation concerns reading data from the casebook, 
;;and parsing tabular data into so-called "Cases" which describe the set of 
;;active cases to sample.  The casebook is read into a spork.util.table 
;;structure, acting as a lightweight database simulacrum of the casebook.    

;;utility-functions                        
(defn collapse-fields [fields r]
  (reduce (fn [acc [from-field to-field]]
            (-> (assoc acc to-field (get acc from-field))
                (dissoc from-field))) r fields))

(defn integral-times [r]
  (let [s (get r :start)
        d (get r :duration)]
    (merge r {:start (quot s 1)
              :duration (quot d 1)})))

;legacy definitions

;these are the original fields from our legacy excel-based tables....
(def legacy-rule-fields 
  ["Rule" "Frequency" 
   "StartDistribution" "S1" "S2" "S3" 
   "DurationDistribution" "D1" "D2" "D3" "Pool"])

(def legacy-rule-keys (vec (map keyword legacy-rule-fields)))

(def start-fields    ["StartDistribution" "S1" "S2" "S3"])
(def start-keys (vec (map keyword start-fields)))

(def duration-fields ["DurationDistribution" "D1" "D2" "D3"])
(def duration-keys (vec (map keyword duration-fields)))

(def demand-fields ["Type" "Enabled" "Priority" "Quantity" 
                    "DemandIndex" "StartDay" "Duration" "Overlap" 
                    "SRC" "SourceFirst" "DemandGroup" "Vignette" 
                    "Operation" "Category" "OITitle"])
(def demand-keys (vec (map keyword demand-fields)))

;We need this to bridge a problem with the legacy data set, namely that 
;the arity of distributions was unknown in the data...there are always 
;four arguments associated with a distribution, [name arg1 arg2 arg3], 
;when the actual distribution may only need arg1, or it may need up to arg3...
;This is a quick hack that encodes the arity for us.  Arities are drawn from 
;util.stats 
(def legacy-arity
  {:normal 2
   :gamma  2
   :beta   2
   :triangle  3
   :uniform   2
   :log-normal  2
   :exponential 1
   :log-logistic 2
   :fix 1})

(defn use-data?
  "Predicate to determine if the supplied field value v indicates building a 
   'distribution' that draws its data from a field, rather than stochastically."
  [v]
  (if (keyword? v) 
    (case v (:vignette :from-data :data) true
      nil)                    
    (case (clojure.string/lower-case v)
      ("vignette" "from-data" "data") true 
      nil)))
            
(defn legacy-distribution
  "Fetches an underlying statistical distribution according to the old encoding 
   from the original vba tool."
  [dist-type dist-name args]
  (if (use-data? dist-name)
    nil
    (let [create-dist (stats/get-distribution dist-name)
          n   (or (get legacy-arity (keyword dist-name)) 
                  (throw (Exception.(str "unknown distribution" dist-name))))]
      (apply create-dist (take n args)))))

(defn distribution-args
  "Fetches the arguments from a legacy record, depending on the distribution 
   type, either \"start\" or \"duration\",  from a record.  Returns a vector 
   compatible with the arguments for #'legacy-distribution."
  [distribution-type rec]
  ((juxt (fn [_] distribution-type) first rest) (vals rec)))

(defn- truthy-string? [s]
  (case (clojure.string/lower-case s)
    ("true" "false") true 
    nil))

(defn parse-legacy-field
  "Parses values in legacy record into a normalized representation.  Symbols 
   become keywords, lists are assumed to be expressions that need to be 
   evaluated, vectors are recursively parsed (i.e. symbol->keyword).  Simplifies
   later processing, since we can use keywords synonymously with symbols."
  [v]  
  (cond (symbol? v) (keyword v)
        (list? v)   (eval v)
        (vector? v) (vec (map parse-legacy-field v))
        (map? v)    (let [ks (map parse-legacy-field (keys v))
                          vs (map parse-legacy-field (vals v))]
                      (zipmap ks vs))
        (string? v) (cond (= (first v) \") v
                          (truthy-string? v) (read-string 
                                               (clojure.string/lower-case v))
                          :else (parse-legacy-field (read-string v)))
                      
        :else v)) 

(defn check-fields [r fields]
  (assert (every? (set fields) 
                  (keys r)) 
          (str "record does not conform to expected fields! "
               {:record-fields (keys r)
                :expected-fields fields})))  

(defn parse-legacy-record
  "Converts a raw legacy record into a normalized record that can be processed 
   into a rule or other data structure."
  [r & {:keys [expected-fields]}]
  (do (if expected-fields (check-fields r expected-fields))
    (into {} (for [[k v] r]
               [k (parse-legacy-field v)]))))

(defn get-computed-fields
  "Returns a map that indicates the transformations to apply for start and 
   duration values, if any.  In some cases, start and duration will be 
   unmodified, i.e. derived from the source record, one or both fields may be
   missing from the resulting map."
  [r]
  (let [start (apply legacy-distribution 
               (distribution-args "start" (rec/sub-record r start-keys)))     
        duration (apply legacy-distribution 
                   (distribution-args "duration" 
                        (rec/sub-record r duration-keys)))]
    (->> (if start {:start start} {})
         ((fn [m] (if duration (assoc m :duration duration) m))))))

(defn include-all? [pool] (= :every (first pool)))
(def draws (atom 0))
(defn next-draw-index! []
  (let [v @draws]
    (do (swap! draws inc)
      v)))
  
(defn ->record-draw [nd]
  (sample/->transform 
    (fn [draws]  (let [idx (next-draw-index!)]
                   (map (fn [r] (assoc r :draw-index idx)) draws)))
    nd))

(defn apply-computed-fields [computed-fields original-fields]
    (let [newstart (if (contains? computed-fields :start)
                     (+ (get computed-fields :start) 
                        (get original-fields :start))
                     (get original-fields :start))]
      (merge original-fields (assoc computed-fields :start newstart))))

;;Modified to use new vocabulary.  We now allow forms of pools that
;;have [:no-replace [x1 x2 ....]] to use (->without-replacement ) 
;;instead of (->choice)
(defn no-replace? [pool]
  (case (first pool) 
    (:no-replace 'no-replace :without-replacement 'without-replacement) true
    nil))

(defn legacy-rule-record->sample-rule
  "Converts a raw legacy record into a sample-rule, as defined in util.sampling.
   We build a set of rules from the legacy rule records, forming them into a 
   sampling network, and then apply the ruleset to source data - usually demand 
   records."
  [rule-record]
  (let [parsed (parse-legacy-record rule-record 
                      :expected-fields legacy-rule-keys)
        distributions (get-computed-fields parsed)
        [name freq pool] (rec/get-fields parsed [:Rule :Frequency :Pool])]
    (->> (cond (include-all? pool)
                  (sample/->transform flatten 
                        (sample/->replications 1 (subvec pool 1)))    ;;this looks suspect       
               (no-replace? pool)
                  (let [xs (second pool)]
                    (assert  (vector? xs) (str "invalid pool sample :" pool ", expected a nested vector!"))
                    (sample/->without-replacement xs))
               :else
                 (sample/->choice pool))
         ((fn [nd] (if (empty? distributions) nd 
                     (sample/->transform 
                       (fn [xs] 
                         (let [sampled-fields ((sample/merge-stochastic
                                                distributions) {})
                               f (partial apply-computed-fields sampled-fields)]
                               (map f xs))) nd))))
         ((fn [nd] (if (> freq 1)                              
                     (sample/->replications freq [(->record-draw nd)])
                     (->record-draw nd))))
         (sample/->transform flatten)
         (assoc {} name))))

(defn read-legacy-population
  "Given a table of demand-records, converts the table into a map of records 
   according to legacy processing rules.  Specifically, we add two fields to 
   the table if they don't exist [:start :duration], which are drawn from  
   StartDay and Duration.  records in the new table are are grouped by their 
   Group field, where each group key forms a map of entries.  These form the 
   context for executing rule-based sampling queries."
  [table & {:keys [group-field start-field duration-field] 
            :or {group-field :Group 
                 start-field :StartDay 
                 duration-field :Duration}}]
  (let [get-col (fn [fld] (first (vals (tbl/get-field fld table))))]
    (->> table 
        (tbl/conj-fields {:start    (get-col start-field)
                          :duration (get-col duration-field)})
        (tbl/table-records)
        (group-by (comp keyword group-field)))))

;This is a slight hack, until I get concatenation working in the dsl.
;We just return a sequence of rule nodes, rather than a map of rule-name 
;to rules.  
(defn read-legacy-rules [table]
  (let [recs (tbl/table-records table)]
    (assert (= (count (distinct (map :Rule recs)))
               (count recs)) (str "Detected indentical rules, " 
                                "ensure that Rule field is distinct" 
                                "for each case"))
       (reduce merge (map legacy-rule-record->sample-rule recs))))

(def case-fields ["CaseName" "Enabled" "Futures" "MaxDuration" 
                  "RandomSeed" "Tfinal" "Replacement"])

(def case-keys (vec (map keyword case-fields)))

(defn parse-legacy-case [record] 
  (parse-legacy-record record :expected-fields case-keys))

(defn compound-key [xs]
  (->> (interleave (map #(let [x (str %)] 
                           (if (= (first x) \:) 
                             (apply str (rest x)) 
                             x))   xs)
                   (repeat \-))
       (butlast)
       (apply str)
       (keyword)))

;;Patched to fix a memory leak....Replacement for build-case. We now
;;encode the specified amount of futures in an intermediate sampler
;;specification map, rather than generating the entirety of the
;;samples for all futures at once.  This should scale much better
;;for larger cases, so long as we process the sampled futures lazily,
;;and without retaining the head.
(defn case->sampler
  "Wrapper for build-case.  Added to enforce O(1) memory constraints when 
   sampling from demand futures.  Rather than embedding the (potentially 
   high) future replications in the case rules using a replication node, 
   we treat the case rules as a sampler.  The sampler is responsible 
   for generating a uniquely identified case future, every time it is 
   sampled.  Also, the return is a map that describes a sampling operation
   rather than a pre-cooked set of samples.  In this case, the sampling 
   operation is evaluated as needed, to generate a set of samples - 
   identical in function to the old Helmet sampling rules."
  [case-name case-rules future-count duration-max seed tfinal replacement]
  (let [idx           (atom 0)        
        seeder        (stats/make-random seed)
        next-idx!     (fn [] (let [i @idx]  (swap! idx inc) i))
        next-seed!    (fn [] (stats/draw seeder))]
    {case-name 
     {:case-name case-name
      :samples   future-count 
      :sampler
      (sample/->constrain 
              {:tfinal tfinal :duration-max duration-max  :seed seed}
       (stats/with-seed (next-seed!)  ;hack, we're re-seeding here for
                                      ;each rep, should have been
                                      ;covered in ->constrain !
         (sample/->transform 
          (fn [case-futures] 
            (let [i (next-idx!)]
              (->>  case-futures
                    (map #(let [vig (str (:Vignette %) "_" (:draw-index %))] ;tag
                               (merge % {:case-name  (tbl/field->string case-name)
                                         :case-future i
                                         :Vignette    vig
                                         :Operation   (str vig  "_" (:Operation %))}))))))
          (sample/->concatenate case-rules))))}}))

(defn sampler->stream 
  "Given a sampling environment - typically the population from a helmet case 
   - and a sampler, returns a lazy sequence of stochastic futures.  Intended to 
   be used with doseq, or any other process that does not retain the head of the 
   sequence."
  [env s]
  (when (> (:samples s) 0)
    (lazy-seq 
     (cons (sample/sample-from env (:sampler s))
           (sampler->stream env (assoc s :samples (-> s :samples dec)))))))
   
(defn read-legacy-cases [table] (->> (tbl/table-records table)
                                  (map parse-legacy-case)
                                  (filter :Enabled)))

;;Patched to use case->sampler, for lazy sampling of futures.
(defn compose-cases [case-records case-rules]
  (reduce (fn [acc case-record] 
            (let [{:keys [Futures Tfinal RandomSeed Enabled MaxDuration CaseName 
                          Replacement]} case-record]
              (conj acc 
                    (case->sampler CaseName (get case-rules CaseName) 
                                   Futures MaxDuration RandomSeed 
                                   Tfinal Replacement))))
          {} case-records))

(defn read-casebook [& {:keys [wbpath ignore-dates?]}]
  (let [db (into {} (for [[k table] (xl/xlsx->tables 
                                      (or wbpath (gui/select-file))
                                      :ignore-dates? ignore-dates?)]
                      [k (tbl/keywordize-field-names table)]))
        case-records (read-legacy-cases (get db "Cases"))
        active-cases (map :CaseName case-records)
        population {:Population
                        (read-legacy-population (get db "DemandRecords"))}
        rules   (into {} (for [k  active-cases]
                           (let [rule-table (or (get db k)
                                                (get db (tbl/field->string k))        
                                                (throw (Exception. 
                                         (str "Table " k " does not exist"))))]
                             ;This is a hack at the moment.  We only return the
                             ;rule nodes of the the rule-table.
                             ;[case-name (read-legacy-rules rule-table)])))
                             [k (vals (read-legacy-rules rule-table))])))
        cases       {:Cases (compose-cases case-records rules)} 
        validation  {:ValidationRules (get db "ValidationRules")}
        demandsplit {:DemandSplit (get db "DemandSplit")}]
    (merge cases population validation demandsplit)))

;;Patched.
;;Changed from original, patched to use lazy sequences and to eschew
;;intermediate maps (i.e. forcing results). This should return a 
;;map of lazy sequences of partitions of futures for each case. 
;;That's mouthful.  So, [[case-name case-future] lazy-records]
;;should be the outcome.  From here, we're ready to post process 
;;each set of lazy-records with collisions and splitting, etc.
;;We already have the case-name and case-future embedded in each 
;;record in the demand. 
(defn compile-cases
  "Given a map of tables, process each case, building its associated rule set, 
   drawing from a sample population.  The results from each case are returned 
   via sampler->stream, where the entries in the stream are a seq of records 
   in a future.  Each record will have the case-name and the case-future added as 
   fields.  The table map, or the database, is expected to have at least the 
   following fields [:ValidationRules :DemandRecords :Cases], where each value
   is a table.  Each enabled case will be evaluated, returning a seq of 
   [[case-name case-future] lazy-records]"
  [db & {:keys [field-merges] 
         :or   {field-merges {:start :StartDay 
                                    :duration :Duration}}}]
  (let [case-key   (juxt :case-name :case-future)
        fix-fields (comp (partial collapse-fields field-merges) integral-times)]
    (concat
     (for [[case-name sampler]  (:Cases db)
           records-in-future  (sampler->stream (:Population db) sampler)] ;generate stream of futures 
       (->> records-in-future
            (map      fix-fields)  ;window dressing
            (vector (case-key (first records-in-future))))))))
  
(defn collide-and-split
  "Given a seq of records, a map of split timings and a map of collision 
   classes, processes the sequence of records by handling collisions, then 
   applying the split logic. "
  [splitmap classes xs & {:keys [log?]}]
  (split/split-future splitmap  
    (collision/process-collisions classes xs :log? log?)))

(defn table->lookup [db tbl-name lookup-field]
  (into {} (for [r (tbl/table-records (get db tbl-name))]
             [(get r lookup-field) r])))

;;Patched to support post-process-cases 
(defn process-if [pred f xs] (if pred (f xs) xs))


(defn validate-splitmap [m]
  (assert (not= (keys m) (list nil))
          "Splitting information appears to be invalid, ensure that you 
            have a DemandSplit field in the DemandRecords table, and a 
            DemandSplit field in the DemandSplit table.")
  m)

;;Changed the lookup field to use a column called DemandSplit, with 
;;corresponding column in the population of records.

;;Patched to avoid intermediate hash-map.
(defn post-process-cases
  "Default post processing for each case.  We validate the case records by 
   handling collisions, and split the resulting data according to the 
   rules defined by DemandSplit."
  [db futures & {:keys [log? processes]}]
  (let [splitmap (validate-splitmap 
                   (table->lookup db :DemandSplit     :DemandSplit))
        classes  (table->lookup db   :ValidationRules :DependencyClass)]    
    (for [[case-key case-records] futures]
      [case-key
       (->> case-records 
            (process-if (:collide processes)
                #(collision/process-collisions classes % :log? log?))
            (process-if (:split processes) 
                #(split/split-future splitmap %)))])))

;;Patched to included discrete processes..
(defn xlsx->futures [wbpath & {:keys [ignore-dates? log? processes] 
                               :or {ignore-dates? true
                                    log? true
                                    processes #{:collide :split}}}]
  (let [db (read-casebook :wbpath wbpath :ignore-dates? ignore-dates?)]
    (post-process-cases db (compile-cases db) :log? log? :processes processes)))

(defn futures->tables [futures & 
                       {:keys [field-order] :or
                        {field-order 
                         (into demand-keys [:case-name :case-future])}}]
  (for [[case-name records] futures]
    [case-name  (->> (tbl/records->table records)
                     (tbl/order-fields-by field-order)
                     (tbl/stringify-field-names))])) 
