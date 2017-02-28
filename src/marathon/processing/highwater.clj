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
 
;;Something for later...
;(def field-parsers {:long  #(java.lang.Long/parseLong %)
;                    :double #(java.lang.Double/parseDouble %)
;                    :bool  #(java.lang.Boolean/parseBoolean %)
;                    :string identity})
;
;(defn record-parser [field-specs]
;  (let [parsers (vec (map #(get field-parsers % tbl/parse-string-nonscientific) 
;                          field-specs))
;  (fn [^string x]
;    (loop [acc []
;           (tbl/split-by-tab x)
    

;;TODO -> turn this into a macro for reading records.
;given a vec of strings, return a trend, fast...
(defn ^trend read-trend [xs]
    (->trend  
      (java.lang.Long/parseLong (nth xs 0)) ;t  
      (java.lang.Long/parseLong (nth xs 1)) ;Quarter
      (nth xs 2) ;SRC 
      (java.lang.Long/parseLong (nth xs 3)) ;TotalRequired 
      (java.lang.Long/parseLong (nth xs 4)) ;TotalFilled 
      (java.lang.Long/parseLong (nth xs 5)) ;Overlapping
      (java.lang.Long/parseLong (nth xs 6)) ;Deployed 
      (nth xs 7) ;DemandName 
      (nth xs 8) ;Vignette 
      (nth xs 9) ;DemandGroup
      (nth xs 10) ;OITitle
      (java.lang.Long/parseLong (nth xs 11)) ;[ACFilled 0] 
      (java.lang.Long/parseLong (nth xs 12)) ;[RCFilled 0] 
      (java.lang.Long/parseLong (nth xs 13)) ;[NGFilled 0] 
      (java.lang.Long/parseLong (nth xs 14)) ;[GhostFilled 0] 
      (java.lang.Long/parseLong (nth xs 15)))) ;[OtherFilled 0]]

;;an experiment.
(defn ^trend read-trend-arr [^String x]
  (let [xs (.split x "\t")]
    (->trend  
      (java.lang.Long/parseLong (aget xs 0)) ;t  
      (java.lang.Long/parseLong (aget xs 1)) ;Quarter
      (aget xs 2) ;SRC 
      (java.lang.Long/parseLong (aget xs 3)) ;TotalRequired 
      (java.lang.Long/parseLong (aget xs 4)) ;TotalFilled 
      (java.lang.Long/parseLong (aget xs 5)) ;Overlapping
      (java.lang.Long/parseLong (aget xs 6)) ;Deployed 
      (aget xs 7) ;DemandName 
      (aget xs 8) ;Vignette 
      (aget xs 9) ;DemandGroup
      (aget xs 10) ;OITitle
      (java.lang.Long/parseLong (aget xs 11)) ;[ACFilled 0] 
      (java.lang.Long/parseLong (aget xs 12)) ;[RCFilled 0] 
      (java.lang.Long/parseLong (aget xs 13)) ;[NGFilled 0] 
      (java.lang.Long/parseLong (aget xs 14)) ;[GhostFilled 0] 
      (java.lang.Long/parseLong (aget xs 15))))) ;[OtherFilled 0]]

(def highwater-headers [:t :Quarter :SRC :TotalRequired :TotalFilled :Overlapping 
                        :Deployed :DemandName :Vignette :DemandGroup :OITitle 
                        :ACFilled :RCFilled :NGFilled :GhostFilled :OtherFilled])
(def headers   (record-headers trend))
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
  ;;probably killing us in performance too...
;    (if (= fieldnames fieldkeys) ;default fields
  (read-trend (tbl/split-by-tab coll)))
;    (read-trend-arr  coll)) 
;    (apply ->trend (map tbl/parse-string-nonscientific 
;                        (tbl/split-by-tab coll)))

;        (let [vs (vec (map tbl/parse-string-nonscientific 
;                   (tbl/split-by-tab coll)))]
;          (apply make-trend (flatten (seq (zipmap fieldnames vs)))))))
  
(defn tabLine     [coll] (clojure.string/join \tab  coll))
;(defn trendString [tr]   (str (tabLine (vals tr)) \newline))
;;Modified to avoid call to str.
(defn trendString [tr]  (tabLine (vals tr)))

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
  [path & {:keys [entry-processor]}]
  (trends->highwater-table (file->trends path) 
                           :entry-processor entry-processor))

;this version is -120 seconds, take about 1 gb of ram.
;Look at swapping out non-cached streams with lazy sequences.

;;Changed trendString implementation to use clojure.string libs
;;which use string builders.  Might switch the entire implementation 
;;use clojure.string/join, I still need to profile this dude.

(defn main
  "Given an input file and an output file, compiles the highwater trends from
   demandtrends.  If lookup-map (a map of {somekey {field1 v1 field2 v2}} is 
   supplied, then the supplied field values will be merged with the entries 
   prior to writing.  We typically use this for passing in things like 
   OITitle and STR (strength) in a simple lookuptable, usually keyed by src.
   This pattern will probably be extracted into a higher order postprocess 
   function or macro...."
  [infile outfile & {:keys [entry-processor]}]
    (with-open [lazyin  (clojure.java.io/reader infile)
                lazyout (clojure.java.io/writer (io/make-file! outfile))]
      (binding [*out* lazyout]
        (do (println (tabLine headers))
          (doseq [t (-> (vec (readTrends (line-seq lazyin))) 
                        (trends->highwater-records 
                          :entry-processor entry-processor))]
                (println (trendString t))))))) ;modified to avoid stringcat.

(defn altmain
  "Uses the table operations to handle i/o, rather than streaming everything.
   Wipes out outfile."
  [infile outfile & {:keys [entry-processor]}]
  (spit (clojure.java.io/writer (io/make-file! outfile))
        (tbl/table->tabdelimited
          (file->highwater-table infile :entry-processor entry-processor)))) 



;This is a process, I'd like to move it to a higher level script....
(defn findDemandTrendPaths
  "Sniff out paths to demand trends files from root."
  [root]
  (map io/fpath (io/find-files root #(= (io/fname %) "DemandTrends.txt"))))

(defn batch
  "Computes high water for for each p in path. dumps a corresponding highwater.
   in the same directory, overwriting."
  [paths & {:keys [entry-processor]}]
  (doseq [source paths]
    (let [target (io/relative-path 
                   (io/as-directory (io/fdir source)) ["highwater.txt"])]
      (if (io/fexists? (clojure.java.io/file source))
        (do (println (str "Computing HighWater : " source" -> " target))
            (main source target 
                  :entry-processor entry-processor))
        (println (str "Source file: " source" does not exist!"))))))

(defn alt-batch
  "Computes high water for for each p in path. dumps a corresponding highwater.
   in the same directory, overwriting."
  [paths & {:keys [entry-processor]}]
  (doseq [source paths]
    (let [target (io/relative-path 
                   (io/as-directory (io/fdir source)) ["highwater.txt"])]
      (if (io/fexists? (clojure.java.io/file source))
        (let [_ (println (str "Computing HighWater : " source" -> " target))]          
            (do (altmain source target 
                  :entry-processor entry-processor)))
        (println (str "Source file: " source" does not exist!"))))))

(defn batch-from
  "Compiles a batch of highwater trends, from demand trends, from all demand 
   trends files in folders or subfolders from root."
  [root & {:keys [entry-processor]}]
  (batch (findDemandTrendPaths root) 
         :entry-processor entry-processor))

(defn alt-batch-from
  "Compiles a batch of highwater trends, from demand trends, from all demand 
   trends files in folders or subfolders from root."
  [root & {:keys [entry-processor]}]
  (alt-batch (findDemandTrendPaths root) 
         :entry-processor entry-processor))

;;testing
(comment 
(defn fake-entries
  "Generates a fake stream of entries"
  [src-count demand-count & {:keys [tstart interval] 
                             :or {tstart 0
                                  interval 10}}]
  (let [fake-demand-name (memoize (fn [src d] (str "Demand" "_" src "_" d)))
        empty-entry      {:TotalRequired 1
                          :TotalFilled 1
                          :Overlapping 0
                          :Deployed 1                          
                          :Vignette    "Sample"
                          :DemandGroup "SampleGroup"
                          :ACFilled 1
                          :RCFilled 0
                          :NGFilled 0
                          :GhostFilled 0 
                          :OtherFilled 0}
        fake-entry       (fn [src d t] 
                           (map->trend 
                             (merge empty-entry 
                                    {:DemandName (fake-demand-name src d)
                                     :SRC     src
                                     :OITitle src
                                     :t t 
                                     :Quarter (inc (quot t 90))})))]
    (->> (iterate #(+ % interval) tstart)
         (mapcat #(for [src (range src-count)
                        d   (range demand-count)]
                    (fake-entry src d %)))
         (concat))))

;;generate a fake table for testing and profiling purposes.
;;Note, it takes about 5 million entries for this guy.  ugh.
(defn fake-table [n]  
  (->> (tbl/records->table (take n (fake-entries 2 10)))
       (tbl/order-fields-by highwater-headers)))

(def workdir "C:\\Users\\tom\\Documents\\Marathon_NIPR\\")
(def batchdir 
  "C:\\Users\\thomas.spoon\\Documents\\TAA 15-19\\Unconstrained Runs")
(def testfile (io/relative-path workdir ["bcttrends.txt"]))
(def bigfile  (io/relative-path workdir ["dtrendsfull.txt"]))
(def testout  (io/relative-path workdir ["hw.txt"]))
(def bigout   (io/relative-path workdir ["highwater.txt"]))

)

