(ns marathon.processing.highwater
  (:use [util.record :only [defrecord+ with-record merge-from record-headers]])
  (:require [util [io :as io] 
                  [table :as tbl]]))
             
; --Highwater is a type of reduction performed on DemandTrends.txt, which is a
; --fairly large log file produced during simulation. The log file is in the
; --tab-delimited format.

(defn keyed-partition-by
  "Aux function. Like clojure.core/partition-by, except it lazily produces
   contiguous chunks from sequence coll, where each member of the coll, when
   keyf is applied, returns a key. When the keys between different regions
   are different, a chunk is emitted, with the key prepended as in group-by."
  [keyf coll]
  (letfn [(chunks [keyf k0 acc coll]
                  (if (seq coll)
                    (let [s (first coll)
                          k (keyf s)]
                      (lazy-seq
                        (if (= k k0)
                          (chunks keyf k0 (conj acc s) (rest coll))
                          (cons [k0 acc] (chunks keyf k [] coll)))))
                    [[k0 acc]]))]
         (when coll
           (chunks keyf (keyf (first coll)) [] coll))))

;(defrecord trend [t Quarter SRC TotalRequired TotalFilled Overlapping
;                  Deployed DemandName Vignette DemandGroup])

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
  
;; trendString :: Trend -> String
;; trendString (Trend t q s req filled lapping dep dem vig group)
; tabLine [(show t), (show q), s, (show req) , (show filled), (show lapping),
(defn tabLine [coll] (apply str (interleave coll (repeat \tab))))
(defn trendString [tr] (str (tabLine (vals tr)) \newline))

; trendList :: [[String]] - > [Trend]
;trendList Is = map readTrend Is
(defn trendList
  "convert a nested list of trend fields into a list of trends."
  [coll & [fieldnames]]
  (if fieldnames 
    (map (fn [c] (readTrend c :fieldnames fieldnames)) coll) ;if fieldnames provided, use.
    (map readTrend coll))) ;else, use default fieldnames.

;asQuarter:: (Integral a)=> a -> a
;asQuarter t = 1 + (quot t 90)
(defn asQuarter
  "convert a sample time into a quarter"
  [t] (inc (quot t 90)))
;trendQuarter :: Trend -> Quarter
;trendQuarter = asQuarter . samplet
(defn trendQuarter
  "Extract the trend's quarter"
  [tr] (asQuarter (:t tr)))

;groupByQuarter :: [Trend] -> [QTrend]
;groupByQuarter [] = []
;groupByQuarter (tr:trs) = (qtrend (quarter tr) (tr:sames)):(groupByQuarter diff
;         where (sames,diffs) = partition (same quarter tr) trs
;This is wrong. GroupBy will not be lazy ..... it traverses the entire sequence.
;YOU really want something like chunk-by, i.e. repeatedly take-while.
(defn groupByQuarter
  "Group a list of Trends by quarters, returning a list of Quarterly Trends."
  [trs]
  (->> (keyed-partition-by trendQuarter trs)
    (map (fn [[q trs]] (Qtrend q trs)))))
; --turn a list of trends into a Map of (src,t), v or a SampleMap
; --where v is the sum of totalfilled for all trends of src at time t.
; --Each quarter, we compute a set of samples keyed by the SRC and the
; --sampletime of the trend, that maps to the total number of units filling
; --demands for SRC on day t. There are instances where multiple samples for
; --an SRC occur on the same day in the data, which we handle by summing.
;sampleQuarter :: QTrend -> SampleMap
;sampleQuarter (q, ts) = foldl (\acc tr -> conj acc tr) emptymap ts
;    where conj acc tr Map.insertWith (+) (src tr, samplet tr) (totalfilled tr)
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

; maxSamples :: SampleMap -> SRCMap
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
; highTrend :: SRCMap -> Trend -> Bool
; highTrend srcmap Trend{src=s, samplet=t} " I Just(t, -) <- (Map.lookup s srcmap) = True
; otherwise = False
(defn highTrend
  "highTrend is a filter function, that, given a corresponding map, will tell us
   if a Trend is indeed the highest trend in all the land!"
  [srcmap {:keys [SRC t]}]
  (let [[tbest ] (get srcmap SRC)]
    (= tbest t)))
;; getHighTrends :: QTrend -> [Trend]
;; getHighTrends qtrend@(q, trs) = filter (highTrend heightmap) trs
;       where heightmap = maxSamples . sampleQuarter $ qtrend
(defn getHighTrends
  "getHighTrends ties everything together for trends in a Quarter.
   The high trends are defined as all trends in the quarter, where the
   trend is identified as a highTrend, in the context of a heightmap.
   heightmap is defined as the maximum sampling, of the samplequarter of
   the quarterly trend."
  [[qtr trends :as qtrend]]
  (let [heightmap ((comp maxSamples sampleQuarter) qtrend)]
    (filter (partial highTrend heightmap) trends)))

; highWaterMarks :: [Trend] -> [Trend]
; highWaterMarks trends = concatMap getHighTrends $ groupByQuarter trends
; I think this is where the original haskell version was boffed.
;Trying to do too much, violating laziness.
(defn highWaterMarks
  "We can represent our final computation as a lazy mapping of getHighTrends
   to a list of QTrends ... "
  [trends]
  (map getHighTrends (concat (groupByQuarter trends))))

; --We don't need the header from our input file.
; readTrends = trendList . drop 1
(defn readTrends
  "Where xs is a tab delimited line sequence, and the first value of 
   xs is a row of headers, returns a lazy seq of trend records using the 
   headers in the first row."
  [xs]
  (let [fields (vec (map keyword (tbl/split-by-tab (first xs))))]
    (trendList (rest xs) fields)))
;    (comp trendList (partial drop 1)))
  
; addHeaders trs = (tabLine headers) :trs
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

;;Want to describe computed fields easily....
;;To be done later...
;(map-fields 
; {:fields  [ACFilled RCFilled NGFilled GhostFilled OtherFilled] <- many fields...
;  :fn (let [strength (get env :STR)] 
;        (fn [fld] (* (get env field) strength)))})
;
;
;(add-fields [m]
; (with-slots [STR TotalRequired] {:STR 10 :TotalRequired 3}
;   (->> (merge m {:RequiredSTR (* STR TotalRequired)
;                  :FilledSTR   (* STR TotalFilled)  }))
;   (map-fields (fn [k v] (* str v))  
;       [:ACFilled :RCFilled :NGFilled :GhostFilled :OtherFilled])))
;
;(query m
;	{:add-fields [:ACStr :RCStr :NGStr :GhostStr :OtherStr]
;	 :by (let [strength (? :STR)
;	           as-Filled (fn [k] (keyword 
;	                               (str (subs (str k) 0 1) "Filled")))] 
;	       (fn [k v] (* strength (? (as-Filled k)))))})
;
;(fn [{:keys [STR TotalRequired] :as m}]
;  (let [? (fn [f] (get m f))]  
;    {:ACFilled (* STR (? :ACFilled)) 
;     :RCFilled (* STR (? :NGFilled))
;     :GhostFilled (* STR (? :GhostFilled)) 
;     :OtherFilled (* STR (? :OtherFilled))}))

;conj-fields 


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
                                      
;this version is -120 seconds, take about 1 gb of ram.
;Look at swapping out non-cached streams with lazy sequences.
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
          (doseq [q (->> (line-seq lazyin)
                      (readTrends)
                      (highWaterMarks))]
              (doseq [t (->> (if entry-processor
                               (map entry-processor q) q)
                          (map trendString))]
                (print t)))))))

;(defn main
;  "Given an input file and an output file, compiles the highwater trends from
;   demandtrends.  If lookup-map (a map of {somekey {field1 v1 field2 v2}} is 
;   supplied, then the supplied field values will be merged with the entries 
;   prior to writing.  We typically use this for passing in things like 
;   OITitle and STR (strength) in a simple lookuptable, usually keyed by src.
;   This pattern will probably be extracted into a higher order postprocess 
;   function or macro...."
;  [infile outfile & {:keys [lookup-map primarykey] 
;                     :or {lookup-map nil primarykey :SRC}}]
;  (with-open [lazyin (clojure.java.io/reader infile)
;              lazyout (clojure.java.io/writer (io/make-file! outfile))]
;    (binding [*out* lazyout]
;      (do (println (tabLine headers))
;          (doseq [q (->> (line-seq lazyin)
;                      (readTrends)
;                      (highWaterMarks))]
;            (let [merged (if lookup-map 
;                           (map 
;                             (fn [x] 
;                               (merge-from x 
;                                   (get lookup-map (get x primarykey))))
;                             q)
;                           q)]
;              (doseq [t  (map trendString merged)]
;                (print t))))))))


;This is a process, I'd like to move it to a higher level script....
(defn findDemandTrendPaths
  "Sniff out paths to demand trends files from root."
  [root]
  (map io/fpath (io/find-files root #(= (io/fname %) "DemandTrends.txt"))))

(defn batchpaths [root]
  (map #(io/relative-path (io/as-directory root) %)
       [["WithSubsSurges" "DemandTrends.txt"]
        ["NoSubsSurges" "DemandTrends. txt"]
        ["WithSubs1418Demand" "DemandTrends. txt"]
        ["NoSubs1418Demand" "DemandTrends.txt"]]))

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

(defn get-entry-processor [root]
  (let [filepath  (io/relative-path 
                       (io/as-directory root) ["SRCdefinitions.txt"])
        srcfile (clojure.java.io/file filepath)]    
    (if (io/fexists? filepath)
      (->> (tbl/tabdelimited->table (slurp srcfile) :parsemode :noscience)
        (tbl/record-seq))
      identity)))


(defn batch-from
  "Compiles a batch of highwater trends, from demand trends, from all demand 
   trends files in folders or subfolders from root."
  [root & {:keys [entry-processor]}]
  (batch (findDemandTrendPaths root) 
         :entry-processor entry-processor))



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
