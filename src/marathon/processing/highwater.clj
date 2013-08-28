;This namespace could probably be changed into a more general stream-processing
;library.  It was built as a single-purpose script, namely to process a large 
;time series, sampling by quarter, to produce a series of peaks in the data
;and a much smaller set of data to examine.  There's definitely a general 
;time-series set of reductions hiding in here somewhere...

(ns marathon.processing.highwater
  (:use [spork.util.record 
           :only [defrecord+ with-record merge-from record-headers]]
        [spork.util.general :only [clumps]])
  (:require [spork.util [io :as io] 
                        [table :as tbl]]))
             
; --Highwater is a type of reduction performed on DemandTrends.txt, which is a
; --fairly large log file produced during simulation. The log file is in the
; --tab-delimited format.


;Note -> we're maintaining backward compatibility with the older version
;of trend here.  We didn't always capture the AC/RC/NG/Ghost, etc. fill data.
;Newer versions may have that information.  In order to process older datasets,
;we want to keep these fields optional.
(defrecord+ trend [t 
                   Quarter 
                   SRC 
                   TotalRequired 
                   TotalFilled 
                   Overlapping
                   Deployed 
                   DemandName 
                   Vignette 
                   DemandGroup
                   OITitle
                   [ACFilled 0] 
                   [RCFilled 0] 
                   [NGFilled 0] 
                   [GhostFilled 0] 
                   [OtherFilled 0]])
(def highwater-headers [:t :Quarter :SRC :TotalRequired :TotalFilled :Overlapping 
                        :Deployed :DemandName :Vignette :DemandGroup :OITitle 
                        :ACFilled :RCFilled :NGFilled :GhostFilled :OtherFilled])
(def headers (record-headers trend))
(def fieldkeys (vec (map keyword headers)))

(defn Qtrend
  "A Qtrend is a type alias for a tuple of Quarter, an Int, and a
   list of Trends. This associates a list of trends to a particular
   Quarter."
  [q trs] 
  [q trs])

(defn readTrend
  "convert a list of stringified fields into a trend"
  [coll & {:keys [fieldnames] :or {fieldnames fieldkeys}}]
  (let [vs (vec (map tbl/parse-string-nonscientific 
                        (tbl/split-by-tab coll)))]
    (if (= fieldnames fieldkeys) ;default fields
    (apply ->trend (map tbl/parse-string-nonscientific 
                        (tbl/split-by-tab coll)))
    (apply make-trend (flatten (seq (zipmap fieldnames vs)))))))
  
(defn tabLine [coll] (apply str (interleave coll (repeat \tab))))
(defn trendString [tr] (str (tabLine (vals tr)) \newline))

(defn trendList
  "convert a nested list of trend fields into a list of trends."
  [coll & [fieldnames]]
  (if fieldnames 
    (map (fn [c] (readTrend c :fieldnames fieldnames)) coll) ;if fieldnames provided, use.
    (map readTrend coll))) ;else, use default fieldnames.

(defn asQuarter
  "convert a sample time into a quarter"
  [t] (inc (quot t 90)))

(defn trendQuarter
  "Extract the trend's quarter"
  [tr] (asQuarter (:t tr)))


(defn groupByQuarter
  "Group a list of Trends by quarters, returning a list of Quarterly Trends."
  [trs]
  (->> (clumps trendQuarter trs)
    (map (fn [[q trs]] (Qtrend q trs)))))

(defn sampleQuarter
  "turn a list of trends into a Map of (src,t), v or a SampleMap
   where v is the sum of totalfilled for all trends of src at time t.
   Each quarter, we compute a set of samples keyed by the SRC and the
   sampletime of the trend, that maps to the total number of units filling
   demands for SRC on day t. There are instances where multiple samples for
   an SRC occur on the same day in the data, which we handle by summing."
  [[q ts]]
  (let [sample (fn [acc {:keys [SRC t TotalFilled]}]
                 (if-let [v (get acc [SRC t])]
                   (assoc! acc [SRC t] (+ TotalFilled v))
                   (assoc! acc [SRC t] TotalFilled)))]
    (persistent! (reduce sample (transient {}) ts))))

(defn maxSamples
  "For each SRC, we want to find a time t, where t has the highest number of uni
   filling demands, relative to every other t in the sample set.
   return a map of (SRC -> t) where t is the time the highest totalfilled
   sample for the SRC."
  [samplemap]
  (let [maxf (fn [m [[s t] newval]]
               (if-let [[oldt oldval] (get m s)]
                 (if (or (> newval oldval)
                         (and (= newval oldval) (< t oldt)))
                   (assoc! m s [t newval])
                   m)
                 (assoc! m s [t newval])))]
    (persistent! (reduce maxf (transient {}) samplemap))))

(defn highTrend
  "highTrend is a filter function, that, given a corresponding map, will tell us
   if a Trend is indeed the highest trend in all the land!"
  [srcmap {:keys [SRC t]}]
  (let [[tbest ] (get srcmap SRC)]
    (= tbest t)))

(defn getHighTrends
  "getHighTrends ties everything together for trends in a Quarter.
   The high trends are defined as all trends in the quarter, where the
   trend is identified as a highTrend, in the context of a heightmap.
   heightmap is defined as the maximum sampling, of the samplequarter of
   the quarterly trend."
  [[qtr trends :as qtrend]]
  (let [heightmap ((comp maxSamples sampleQuarter) qtrend)]
    (filter (partial highTrend heightmap) trends)))

(defn highWaterMarks
  "We can represent our final computation as a lazy mapping of getHighTrends
   to a list of QTrends ... "
  [trends]
  (map getHighTrends (concat (groupByQuarter trends))))

(defn readTrends
  "Where xs is a tab delimited line sequence, and the first value of 
   xs is a row of headers, returns a lazy seq of trend records using the 
   headers in the first row."
  [xs]
  (let [fields (vec (map keyword (tbl/split-by-tab (first xs))))]
    (trendList (rest xs) fields)))

(defn addHeaders [trends] (cons (str (tabLine headers) \newline) trends))
(def workdir "C:\\Users\\tom\\Documents\\Marathon_NIPR\\")
(def batchdir 
  "C:\\Users\\thomas.spoon\\Documents\\TAA 15-19\\Unconstrained Runs")
(def testfile (io/relative-path workdir ["bcttrends.txt"]))
(def bigfile (io/relative-path workdir ["dtrendsfull.txt"]))
(def testout (io/relative-path workdir ["hw.txt"]))
(def bigout (io/relative-path workdir ["highwater.txt"]))


(defn intersect-fields
  "Given a lookup-map of {primarykeyval {fieldname expr}}, and a primary key, 
   returns a function that intersects fields from lookup-map and a record.  
   The resulting value should be identical to the record (field-wise), except 
   field values may be changed or computed by the intersection. "
  [lookup-map primarykey]  
  (fn [record] 
    (merge-from record 
      (get lookup-map (get record primarykey)))))

;this is a simple query....
(def testlookup 
  {"SRC1" {:SRC "SRC1", :OITitle "Little SRC", :STR 5}
   "SRC2" {:SRC "SRC2", :OITitle "Medium SRC", :STR 10}
   "SRC3" {:SRC "SRC3", :OITitle "Big SRC",    :STR 20}})


;These operations will be replaced with more abstract SQL-like operations, 
;once I get a library for it built....

(defn src-lookup [srcmap]
  (fn [m] 
    (intersect-fields m srcmap :SRC)))

(defn compute-strengths
  "Adds a few computed fields to our entry."
  [m]
  (let [? (fn [f] (get m f))
        strength (? :STR)]  
    {:ACStr (*  strength (? :ACFilled)) 
     :RCStr (* strength (? :NGFilled))
     :GhostStr (* strength (? :GhostFilled)) 
     :OtherStr (* strength (? :OtherFilled))
     :TotalStr (* strength (? :TotalFilled))
     :RequiredStr (* strength (? :TotalRequired))})) 
                    
(defn append-src-data
  "Produces an entry processor that tacks on strength, OITitle, and computes 
   compo-specific strengths."
  [srcmap]
  (comp compute-strengths (src-lookup srcmap))) 
                                      
(defn file->trends [inpath]
  (with-open [rdr (clojure.java.io/reader inpath)]
    (vec (readTrends (line-seq rdr)))))

(defn trends->highwater-records
  "Given an lazy sequence of trend records, compiles the highwater trends from
   demandtrends."
  [rawtrends & {:keys [entry-processor]}]
  (for [q (highWaterMarks rawtrends)
        trendrec (map (if entry-processor entry-processor identity) q)]
    trendrec))

(defn trends->highwater-table
  "Composition that produces an ITable from a lazy sequence of raw demand 
   trends, where the table is a fully computed set of highwater records."
  [rawtrends & {:keys [entry-processor]}]
  (tbl/records->table
    (trends->highwater-records rawtrends :entry-processor entry-processor)))

(defn file->highwater-table
  "Given a path to a demand trends file, computes highwater trends and returns
   an ITable data structure from util.table "
  [path]
  (trends->highwater-table (file->trends path)))

;this version is -120 seconds, take about 1 gb of ram.
;Look at swapping out non-cached streams with lazy sequences.

;changed to support using 
(defn main
  "Given an input file and an output file, compiles the highwater trends from
   demandtrends.  If lookup-map (a map of {somekey {field1 v1 field2 v2}} is 
   supplied, then the supplied field values will be merged with the entries 
   prior to writing.  We typically use this for passing in things like 
   OITitle and STR (strength) in a simple lookuptable, usually keyed by src.
   This pattern will probably be extracted into a higher order postprocess 
   function or macro...."
  [infile outfile & {:keys [entry-processor]}]
    (with-open [lazyin (clojure.java.io/reader infile)
                lazyout (clojure.java.io/writer (io/make-file! outfile))]
      (binding [*out* lazyout]
        (do (println (tabLine headers))
          (doseq [t (-> (vec (readTrends (line-seq lazyin))) 
                        (trends->highwater-records 
                          :entry-processor entry-processor))]
                (print (trendString t)))))))
(comment 
(defn jog-table [t n]
  "Duplicates table t, creating a sequence of n records."
  (let [tmin (:t (tbl/first-record t))
        tmax (:t (tbl/last-record t))
        step (- tmax tmin)
        steps (+ (quot n step) (min (rem n step) 1))
        modify (fn [i rec]
                 (let [t (+ (:t rec) (* i step))
                       q (inc (quot t 90))]
                 (-> (assoc rec :t t)
                     (assoc :Quarter q))))                        
                     
        stepfn (fn [i] 
                 (map (partial modify i) 
                      (tbl/table-records t)))]
    (->> (mapcat stepfn (range steps))         
         (take n))))

;simple script to generate large test data.
(with-open [wtr (clojure.java.io/writer (io/make-file! targetpath))]
  (binding [*out* wtr]
    (do (let [fields (tbl/get-fields trends-table)
              recvals (fn [rec] (reduce (fn [acc v] (conj acc (get rec v))) []
                                        fields))]
          (println (tabLine (map tbl/field->string fields)))
          (doseq [rec (jog-table trends-table 200000)]
            (println (str (tabLine (recvals rec)))))))))
)           

;(defn main
;  "Given an input file and an output file, compiles the highwater trends from
;   demandtrends.  If lookup-map (a map of {somekey {field1 v1 field2 v2}} is 
;   supplied, then the supplied field values will be merged with the entries 
;   prior to writing.  We typically use this for passing in things like 
;   OITitle and STR (strength) in a simple lookuptable, usually keyed by src.
;   This pattern will probably be extracted into a higher order postprocess 
;   function or macro...."
;  [infile outfile & {:keys [entry-processor]}]
;    (with-open [lazyin (clojure.java.io/reader infile)
;                lazyout (clojure.java.io/writer (io/make-file! outfile))]
;      (binding [*out* lazyout]
;        (do (println (tabLine headers))
;          (doseq [q (->> (line-seq lazyin)
;                      (readTrends)
;                      (highWaterMarks))]
;              (doseq [t (->> (if entry-processor
;                               (map entry-processor q) q)
;                          (map trendString))]
;                (print t)))))))

;This is a process, I'd like to move it to a higher level script....
(defn findDemandTrendPaths
  "Sniff out paths to demand trends files from root."
  [root]
  (map io/fpath (io/find-files root #(= (io/fname %) "DemandTrends.txt"))))

(defn batch
  "Computes high water for for each p in path. dumps a corresponding highwater.
   in the same directory, overwriting."
  [paths & {:keys [entry-processor]}]
  (for [source paths]
    (let [target (io/relative-path 
                   (io/as-directory (io/fdir source)) ["highwater.txt"])]
      (if (io/fexists? (clojure.java.io/file source))
        (do (println (str "Computing HighWater : " source" -> " target))
            (main source target 
                  :entry-processor entry-processor))
        (println (str "Source file: " source" does not exist!"))))))

;(defn batch2
;  "Computes high water for for each p in path. dumps a corresponding highwater.
;   in the same directory, overwriting."
;  [paths & {:keys [entry-processor]}]
;  (for [source paths]
;    (let [target (io/relative-path 
;                   (io/as-directory (io/fdir source)) ["highwater.txt"])]
;      (if (io/fexists? (clojure.java.io/file source))
;        (do (println (str "Computing HighWater : " source" -> " target))
;            (main2 source target 
;                  :entry-processor entry-processor))
;        (println (str "Source file: " source" does not exist!"))))))
 
(defn batch-from
  "Compiles a batch of highwater trends, from demand trends, from all demand 
   trends files in folders or subfolders from root."
  [root & {:keys [entry-processor]}]
  (batch (findDemandTrendPaths root) 
         :entry-processor entry-processor))

;(defn batch-from2
;  "Compiles a batch of highwater trends, from demand trends, from all demand 
;   trends files in folders or subfolders from root."
;  [root & {:keys [entry-processor]}]
;  (batch2 (findDemandTrendPaths root) 
;         :entry-processor entry-processor))

;this version is as fast, but takes 3 GB of ram ....
; (defn main2 [infile outfile]
; (with-open [lazyin (clojure.java.io/reader infile)
; lazyout (clojure.java.io/writer (make-file! outfile))]
; (binding [*out* lazyout]
; (do (println (tabLine headers))
; (->> (line-seq (clojure.java.io/reader infile))
; (readTrends)
; (groupByQuarter)
; (mapcat getHighTrends)
; (map trendString)
; (print) ) ) ) ) )
