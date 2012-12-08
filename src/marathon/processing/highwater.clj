(ns marathon.processing.highwater
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

(defrecord trend [t Quarter SRC TotalRequired TotalFilled Overlapping
                  Deployed DemandName Vignette DemandGroup])


(defmacro record-headers
  "Returns a vector of the field names from a record."
  [recname]
  (let [rname (symbol (str "->" recname))]
  `(vec (map str (first (:arglists (meta #'~rname)))))))

(def headers (record-headers trend))

(defn Qtrend
  "A Qtrend is a type alias for a tuple of Quarter, an Int, and a
   list of Trends. This associates a list of trends to a particular
   Quarter."
  [q trs] 
  [q trs])

(defn readTrend
  "convert a list of stringified fields into a trend"
  [coll]
  (apply ->trend (map tbl/parse-string-nonscientific 
                      (tbl/split-by-tab coll))))

;; trendString :: Trend -> String
;; trendString (Trend t q s req filled lapping dep dem vig group)
; tabLine [(show t), (show q), s, (show req) , (show filled), (show lapping),
(defn tabLine [coll] (apply str (interleave coll (repeat \tab))))
(defn trendString [tr] (str (tabLine (vals tr)) \newline))

; trendList :: [[String]] - > [Trend]
;trendList Is = map readTrend Is
(defn trendList
  "convert a nested list of trend fields into a list of trends."
  [coll] (map readTrend coll))

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
(def readTrends (comp trendList (partial drop 1)))
; addHeaders trs = (tabLine headers) :trs
(defn addHeaders [trends] (cons (str (tabLine headers) \newline) trends))
(def workdir "C:\\Users\\thomas.spoon\\Documents\\trendtest\\")
(def batchdir 
  "C:\\Users\\thomas.spoon\\Documents\\TAA 15-19\\Unconstrained Runs")
(def testfile (io/relative-path workdir ["bcttrends.txt"]))
(def bigfile (io/relative-path workdir ["dtrendsfull.txt"]))
(def testout (io/relative-path workdir ["hw.txt"]))
(def bigout (io/relative-path workdir ["highwater.txt"]))

;this version is -120 seconds, take about 1 gb of ram.
(defn main [infile outfile]
  (with-open [lazyin (clojure.java.io/reader infile)
              lazyout (clojure.java.io/writer (make-file! outfile))]
    (binding [*out* lazyout]
      (do (println (tabLine headers))
          (doseq [q (->> (line-seq lazyin)
                      (readTrends)
                      (highWaterMarks))
                  t (map trendString q)]
            (print t))))))


;This is a process, I'd like to move it to a higher level script....
(defn findDemandTrendPaths
  "Sniff out paths to demand trends files from root."
  [root]
  (map fpath (io/find-files root #(= (io/fname %) "DemandTrends.txt"))))

(defn batchpaths [root]
  (map #(io/relative-path (io/as-directory root) %)
       [["WithSubsSurges" "DemandTrends.txt"]
        ["NoSubsSurges" "DemandTrends. txt"]
        ["WithSubs1418Demand" "DemandTrends. txt"]
        ["NoSubs1418Demand" "DemandTrends.txt"]]))

(defn batch
  "Computes high water for for each p in path. dumps a corresponding highwater.
   in the same directory, overwriting."
  [paths]
  (for [source paths]
    (let [target (relative-path 
                   (as-directory (io/fdir source)) ["highwater.txt"])]
      (if (io/fexists? (clojure.java.io/file source))
        (do (println (str "Computing HighWater : " source" -> " target))
            (main source target))
        (println (str "Source file: " source" does not exist!"))))))

(defn batch-from
  "Compiles a batch of highwater trends, from demand trends, from all demand 
   trends files in folders or subfolders from root."
  [root]
  (batch (findDemandTrendPaths root)))

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
