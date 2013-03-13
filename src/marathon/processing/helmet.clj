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

(defn use-data? [v]
  (case (clojure.string/lower-case v)
    ("vignette" "from-data" "data") true 
    nil))

;from stackoverflow...uses Java reflection :/
(defn arg-count [func] 
  (let [m (first (.getDeclaredMethods (class func)))
        p (.getParameterTypes m)]
    (alength p)))

                 
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
    
(defn legacy-distribution
  "Fetches an underlying statistical distribution according to the old encoding 
   from the original vba tool."
  [dist-type dist-name args]
  (if (use-data? dist-name ) 
    (derive-data-distribution dist-type dist-name )   
    (let [create-dist (stats/get-distribution dist-name)]
      (apply create-dist (take (arg-count create-dist) args)))))

(comment ;testing
  (def legacy-record 
    (gen/align-fields-by 
      legacy-rule-fields
      (zipmap legacy-rule-fields 
              '[GetHoot	2	uniform	0	1000	nil	from-data	nil	nil	nil	
                [:A_Dipper :Dollar :Hoot1 :Hoot2 :Hoot3 :Hoot4 
                 :Ipsum_1Dipper :S-Foo-FootLbs]])))

)
  
(defn legacy-rule-record->sample-rule 
  [rule-record]
  (assert (every? (set legacy-rule-fields) 
                  (keys rule-record)) 
          (str "record does not conform to expected fields! "
               {:record-fields (keys rule-record)
                :expected-fields legacy-rule-fields}))
  (let [
  
Node	Frequency	StartDistribution	S1	S2	S3	DurationDistribution	D1	D2	D3	Pool
GetHoot	2	uniform	0	1000	nil	from-data	nil	nil	nil	[:A_Dipper :Dollar :Hoot1 :Hoot2 :Hoot3 :Hoot4 :Ipsum_1Dipper :S-Foo-FootLbs]
