;helmet is a tool the parses sample-queries relative to 
;a set of records, validation rules, and sample-rules, 
;and then samples from the records accordig to the sample-rules, 
;validates the resultant set of sample records, and allows 
;replications of the process.
(ns marathon.processing.helmet
  (:require [util [general :as gen]
                  [table :as tbl]
                  [stats :as stats] 
                  [sampling :as sample]]))

;these are the original fields from our legacy excel-based tables....
(def legacy-rule-fields 
  ["Node" "Frequency" 
   "StartDistribution" "S1" "S2" "S3" 
   "DurationDistribution" "D1" "D2" "D3" "Pool"])

(def start-fields    ["StartDistribution" "S1" "S2" "S3"])
(def duration-fields ["DurationDistribution" "D1" "D2" "D3"])

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

(defn sub-record [r fields]
  (let [fset (set fields)]
    (gen/align-fields-by fields 
       (into {} (for [[k v] r :when (contains? fset k)]
                  [k v])))))

(defn use-data? [v]
  (case (clojure.string/lower-case v)
    ("vignette" "from-data" "data") true 
    nil))
                 
(defn derive-data-distribution
  "Given a distribution type, in the form a string value, parses the 
   type to return a function that simply reads a value from a record field
   to create the distribution."
  [dist-type & {:keys [type-fields] 
                :or {type-fields {:start    "StartDay"
                                  :duration "Duration"}}}]
  (let [v (clojure.string/lower-case dist-type)]
    (cond (re-matches #"start.*" v) #(get % (:start type-fields))
          (re-matches #"duration.*" v) #(get % (:duration type-fields)))))

(defn distribution-args [distribution-type rec]
  ((juxt (fn [_] distribution-type) first rest) (vals rec)))

(defn legacy-distribution
  "Fetches an underlying statistical distribution according to the old encoding 
   from the original vba tool."
  [dist-type dist-name args]
  (if (use-data? dist-name) 
    (derive-data-distribution dist-type dist-name)   
    (let [create-dist (stats/get-distribution dist-name)
          n   (get legacy-arity (keyword dist-name) 
                 (throw (Exception.(str "unknown distribution" dist-name))))]
      (apply create-dist (take n args)))))

(defn parse-legacy-field [v]  
  (cond (symbol? v) (keyword v)
        (list? v)   (eval v)
        (vector? v) (vec (map parse-legacy-field v))
        :else v)) 
          
(defn parse-legacy-fields [xs] 
  (vec (map parse-legacy-field xs)))

(defn parse-legacy-record [r]
  (assert (every? (set legacy-rule-fields) 
                  (keys r)) 
          (str "record does not conform to expected fields! "
               {:record-fields (keys r)
                :expected-fields legacy-rule-fields}))  
  (into {} (for [[k v] r]
             [k (parse-legacy-field v)]))) 

(comment ;testing
  (def sample-fields '[GetHoot	2	uniform	0	1000	nil	from-data	nil	nil	nil	
                [:A_Dipper :Dollar :Hoot1 :Hoot2 :Hoot3 :Hoot4 
                 :Ipsum_1Dipper :S-Foo-FootLbs]])
  (def legacy-record 
    (gen/align-fields-by 
      legacy-rule-fields
      (zipmap legacy-rule-fields 
              sample-fields)))
  (def parsed-record (parse-legacy-record legacy-record))

;want to transform a rule record into this ->
;{:GetHoot {:replicate 2 {:transform [{:start (uniform 0 1000)} 
;                                     {:choice [:A_Dipper :Dollar :Hoot1 :Hoot2 
;                                               :Hoot3 :Hoot4 :Ipsum_1Dipper 
;                                               :S-Foo-FootLbs]}]}}}

)
 
(defn get-computed-fields [r]
  {:start (apply legacy-distribution 
               (distribution-args "start" (sub-record r start-fields)))     
   :duration  (apply legacy-distribution 
                   (distribution-args "duration" 
                      (sub-record r duration-fields)))})

(defn get-fields [r xs]
  (vec (map (fn [x] (get r x)) xs)))

(defn legacy-rule-record->sample-rule 
  [rule-record]
  (let [normalized (parse-legacy-record rule-record)
        distributions (get-computed-fields normalized)
        [name freq pool] (get-fields normalized ["Node"	"Frequency" "Pool"])]
    {name (sample/->replications freq
             (sample/->transform distributions
                 (sample/->choice pool)))}))
       
    

  
;Node	Frequency	StartDistribution	S1	S2	S3	DurationDistribution	D1	D2	D3	Pool
;GetHoot	2	uniform	0	1000	nil	from-data	nil	nil	nil	[:A_Dipper :Dollar :Hoot1 :Hoot2 :Hoot3 :Hoot4 :Ipsum_1Dipper :S-Foo-FootLbs]
