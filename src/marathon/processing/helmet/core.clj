;helmet is a tool the parses sample-queries relative to 
;a set of records, validation rules, and sample-rules, 
;and then samples from the records accordig to the sample-rules, 
;validates the resultant set of sample records, and allows 
;replications of the process.
(ns marathon.processing.helmet.core
  (:require [util [general :as gen]
                  [table :as tbl]
                  [stats :as stats]
                  [record :as rec]
                  [sampling :as sample]]
            [util.excel [core :as xl]]
            [marathon.processing.helmet [split :as split] 
                                        [collision :as collision]]))

;utility-functions                        
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
    (->> (if (include-all? pool)
           (sample/->transform flatten 
               (sample/->replications 1 (subvec pool 1)))
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
  (->> (map legacy-rule-record->sample-rule (tbl/table-records table))
       (reduce merge)))

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
                   
(defn build-case 
  [case-name case-rules future-count duration-max seed tfinal replacement]
  {case-name 
   (sample/->constrain {:tfinal tfinal :duration-max duration-max  :seed seed}
     (sample/->transform 
       (fn [case-futures] 
         (map-indexed 
           (fn [i rule-reps]               
               (map #(merge % {:case-name  (tbl/field->string case-name)
                               :case-future i
                               :Operation  (str (:Operation %) "_" 
                                                (:draw-index %))}) 
                    (flatten rule-reps)))
           (first case-futures)))                     
       (sample/->replications future-count [case-rules])))})
;       (sample/->replications future-count [case-rules])))})

(defn read-legacy-cases [table] (->> (tbl/table-records table)
                                  (map parse-legacy-case)
                                  (filter :Enabled)))

(defn compose-cases [case-records case-rules]
  (reduce (fn [acc case-record] 
            (let [{:keys [Futures Tfinal RandomSeed Enabled MaxDuration CaseName 
                          Replacement]} case-record]
              (conj acc 
                    (build-case CaseName (get case-rules CaseName) 
                                Futures MaxDuration RandomSeed 
                                Tfinal Replacement))))
          {} case-records))

(defn read-casebook [& {:keys [wbpath ignore-dates?]}]
  (let [db (into {} (for [[k table] (xl/xlsx->tables 
                                      (or wbpath (util.gui/select-file))
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
        cases {:Cases (compose-cases case-records rules)} 
        validation {:ValidationRules (:ValidationRules db)}]
    (merge cases population validation)))
  


(defn compile-cases
  "Given a map of tables, process each case, building its associated rule set, 
   drawing from a sample population.  The results from each case are returned 
   as a map of {case-name [records]}, where records are a list of records from
   futures.  Each record will have the case-name and the case-future added as 
   fields.  The table map, or the database, is expected to have at least the 
   following fields [:ValidationRules :DemandRecords :Cases], where each value
   is a table.  Each enabled case will be evaluated, returning a map of 
   {[case-name case-future] records}"
  [database & {:keys [field-merges] 
               :or   {field-merges {:start :StartDay 
                                    :duration :Duration}}}]
  (let [case-key (juxt :case-name :case-future)
        fix-fields (comp (partial collapse-fields field-merges) integral-times)]
    (into {} (for [[case-name c] (:Cases database)]
               (->> (sample/sample-from (:Population database) c)
                    (map fix-fields) 
                    (group-by case-key)
                    (seq))))))

(defn xlsx->futures [wbpath & {:keys [ignore-dates?] 
                               :or {ignore-dates? true}}]
  (compile-cases (read-casebook :wbpath wbpath
                                :ignore-dates? ignore-dates?)))

(defn futures->tables [future-map & 
                       {:keys [field-order] :or
                        {field-order 
                         (into demand-keys [:case-name :case-future])}}]
  (into {} 
        (for [[case-name records] future-map]
          [case-name  (->> (tbl/records->table records)
                           (tbl/order-fields-by field-order)
                           (tbl/stringify-field-names))]))) 



(comment ;testing
;;our test record fields...
;[Node	Frequency	StartDistribution	S1	S2	S3	
;               DurationDistribution	D1	D2	D3	Pool]
;;and vals...
;[GetHoot	2	uniform	0	1000	nil	
;           from-data	nil	nil	nil	
;           [:A_Dipper :Dollar :Hoot1 :Hoot2 :Hoot3 :Hoot4 :Ipsum_1Dipper 
;            :S-Foo-FootLbs]]

;  (def sample-fields 
;    '[GetHoot	2	uniform	0	1000	nil	from-data	nil	nil	nil	
;                [:A_Dipper :Dollar :Hoot1 :Hoot2 :Hoot3 :Hoot4 
;                 :Ipsum_1Dipper :S-Foo-FootLbs]])
;
;  (def legacy-record 
;    (gen/align-fields-by 
;      legacy-rule-keys
;      (zipmap legacy-rule-keys 
;              sample-fields)))
;  (def parsed-record (parse-legacy-record legacy-record))
;  (def rule (legacy-rule-record->sample-rule parsed-record))

;(def simple-pop 
;  (reduce #(assoc %1 %2 %2) {} 
;    (map keyword (flatten 
;                   '[[A_Dipper Dollar  Hoot1 Hoot2 Hoot3 Hoot4 Ipsum1_Dipper S-Foo-FootLbs]
;                     [A_Dipper Hoot1 Hoot3]
;                     [Dollar Ipsum1_Dipper Some16 Some18 Some21 Some5 Some6]
;                     [Some5]
;                     [Some5 Some16]
;                     [Some5 Some16]]))))

  (require '[marathon.processing.sampledata :as dat])
  
  (def case-records dat/cases)
  (def case-tbl (tbl/records->table case-records))
  (def cases (read-legacy-cases case-tbl))
  
  (def demand-records dat/demand-records)
  (def demand-tbl (tbl/records->table demand-records))
  
  (def p (read-legacy-population demand-tbl))  
  (def rule-records dat/rule-records)  
  (def rule-tbl (tbl/records->table rule-records))
  (def rules (read-legacy-rules rule-tbl))
  (def case-rules {:Case1 (vals rules)})
  
  (def statics (:Static rules))
  (def composed (compose-cases cases case-rules))

  (def demand-tbl (get db "DemandRecords"))  
  (def p (read-legacy-population demand-tbl))  
  (def rule-tbl (get db "Case3"))
  (def rules (read-legacy-rules rule-tbl))
  
  (def statics (:Static rules))

  
  
;want to transform a rule record into this ->
;{:GetHoot {:replicate 2 {:transform [{:start (uniform 0 1000)} 
;                                     {:choice [:A_Dipper :Dollar :Hoot1 :Hoot2 
;                                               :Hoot3 :Hoot4 :Ipsum_1Dipper 
;                                               :S-Foo-FootLbs]}]}}}

)

;This is unnecessary....by convention, we already have the data and are 
;just transforming it by merging values into the record...so affecting a 
;from-data distribution is identical to doing nothing....
;(defn derive-data-distribution
;  "Given a distribution type, in the form a string value, parses the 
;   type to return a function that simply reads a value from a record field
;   to create the distribution."
;  [dist-type & {:keys [type-fields] 
;                :or {type-fields {:start    "StartDay"
;                                  :duration "Duration"}}}]
;  (let [v (clojure.string/lower-case dist-type)]
;    (cond (re-matches #"start.*" v) #(get % (:start type-fields))
;          (re-matches #"duration.*" v) #(get % (:duration type-fields)))))

;not used anywhere
;(defn parse-legacy-fields [xs] 
;  (vec (map parse-legacy-field xs)))
