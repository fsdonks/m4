;helmet is a tool the parses sample-queries relative to 
;a set of records, validation rules, and sample-rules, 
;and then samples from the records accordig to the sample-rules, 
;validates the resultant set of sample records, and allows 
;replications of the process.
(ns marathon.processing.helmet
  (:require [util [general :as gen]
                  [table :as tbl]
                  [stats :as stats]
                  [record :as rec]
                  [sampling :as sample]]))

;these are the original fields from our legacy excel-based tables....
(def legacy-rule-fields 
  ["Node" "Frequency" 
   "StartDistribution" "S1" "S2" "S3" 
   "DurationDistribution" "D1" "D2" "D3" "Pool"])

(def start-fields    ["StartDistribution" "S1" "S2" "S3"])
(def duration-fields ["DurationDistribution" "D1" "D2" "D3"])

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

(defn parse-legacy-field
  "Parses values in legacy record into a normalized representation.  Symbols 
   become keywords, lists are assumed to be expressions that need to be 
   evaluated, vectors are recursively parsed (i.e. symbol->keyword).  Simplifies
   later processing, since we can use keywords synonymously with symbols."
  [v]  
  (cond (symbol? v) (keyword v)
        (list? v)   (eval v)
        (vector? v) (vec (map parse-legacy-field v))
        :else v)) 

(defn parse-legacy-record
  "Converts a raw legacy record into a normalized record that can be processed 
   into a rule or other data structure."
  [r]
  (assert (every? (set legacy-rule-fields) 
                  (keys r)) 
          (str "record does not conform to expected fields! "
               {:record-fields (keys r)
                :expected-fields legacy-rule-fields}))  
  (into {} (for [[k v] r]
             [k (parse-legacy-field v)]))) 
 
(defn get-computed-fields
  "Returns a map that indicates the transformations to apply for start and 
   duration values, if any.  In some cases, start and duration will be 
   unmodified, i.e. derived from the source record, one or both fields may be
   missing from the resulting map."
  [r]
  (let [start (apply legacy-distribution 
               (distribution-args "start" (rec/sub-record r start-fields)))     
        duration (apply legacy-distribution 
                   (distribution-args "duration" 
                        (rec/sub-record r duration-fields)))]
    (->> (if start {:start start} {})
         ((fn [m] (if duration (assoc m :duration duration) m))))))

(defn legacy-rule-record->sample-rule
  "Converts a raw legacy record into a sample-rule, as defined in util.sampling.
   We build a set of rules from the legacy rule records, forming them into a 
   sampling network, and then apply the ruleset to source data - usually demand 
   records."
  [rule-record]
  (let [parsed (parse-legacy-record rule-record)
        distributions (get-computed-fields parsed)
        [name freq pool] (rec/get-fields parsed ["Node"	"Frequency" "Pool"])]
    {name (sample/->replications freq
             (sample/->transform distributions
                 (sample/->choice pool)))}))

(comment ;testing
;;our test record fields...
;[Node	Frequency	StartDistribution	S1	S2	S3	
;               DurationDistribution	D1	D2	D3	Pool]
;;and vals...
;[GetHoot	2	uniform	0	1000	nil	
;           from-data	nil	nil	nil	
;           [:A_Dipper :Dollar :Hoot1 :Hoot2 :Hoot3 :Hoot4 :Ipsum_1Dipper 
;            :S-Foo-FootLbs]]

  (def sample-fields 
    '[GetHoot	2	uniform	0	1000	nil	from-data	nil	nil	nil	
                [:A_Dipper :Dollar :Hoot1 :Hoot2 :Hoot3 :Hoot4 
                 :Ipsum_1Dipper :S-Foo-FootLbs]])

  (def legacy-record 
    (gen/align-fields-by 
      legacy-rule-fields
      (zipmap legacy-rule-fields 
              sample-fields)))
  (def parsed-record (parse-legacy-record legacy-record))
  (def rule (legacy-rule-record->sample-rule parsed-record))

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
