(ns marathon.processing
  (:require [clojure [string :as stringop]] 
            [clojure.java [io :as io]]))

;t	Quarter	SRC	TotalRequired	TotalFilled	Overlapping	Deployed	DemandName	Vignette	DemandGroup
;450	6	SRC_1	15	0	0	0	1_LS Phase1_SRC_1_[450...467]	LS Phase1	Long Surge
;450	6	SRC_10	15	6	0	6	1_LS Phase1_SRC_10_[450...467]	LS Phase1	Long Surge
;450	6	SRC_100	15	6	0	6	1_LS Phase1_SRC_100_[450...467]	LS Phase1	Long Surge
;(def sample-data 

(def localpath 
  (let [home (str (System/getProperty "user.home"))]
    (fn [pth] (str home "\\" pth))))

(def projectpath 
  (localpath "\\Documents\\Marathon_NIPR\\OngoingDevelopment"))
(def demandtrendpath (str projectpath "\\" "DemandTrendsTest.csv"))

(defn- parse-string [value]
  (if (re-matches #"\d+" value)
    (try (Integer/parseInt value)
         (catch NumberFormatException _ value))
    (try (Double/parseDouble value)
         (catch NumberFormatException _ value))))

(defn csv->seq [ln] (map parse-string (stringop/split ln #",")))

(defn csv->map
  "Consume a comma-separated value file at a given location, turning it into 
   a sequence of records.  Creates a map, the keys of which are provided by 
   either the initial data, or a vector of fieldnames supplied by the user."
  ([lines fieldnames] 
    (let [fieldnames (map keyword fieldnames)
          ln->record (fn [ln] (zipmap fieldnames (csv->seq ln)))]
      (map ln->record lines)))
  ([path] (if (.exists (io/as-file path))
            (let [lines (line-seq (io/reader (io/as-file path))) 
                  fieldnames (first lines)]
              (csv->map (rest lines) (stringop/split fieldnames #","))))))

;one common processing task, for demandtrends.csv, is to determine high-water 
;marks for each SRC, based on some function that maps t -> t2.  
;Generally, we use quarter, which maps t->qtr, which allows us to find 
;the highest demand for each src for each quarter.

(defn aggregate
  "Function that will examine a set of temporal trends, and will return 
   a subset of records that represent the 'high water' marks for a given 
   criterion function, binned by a bin function.  The primary use is to 
   aggregate samples into a single representative for each grouping of samples 
   derived by groupfunc."
  [groupfunc aggregator samples]
  (->> samples 
    (group-by groupfunc)
    (map #(fn [k [vs]] [k (aggregator vs)]))))

(defn get-groups
  "Given a splitting path, use group-by to split the samples 
   across each element in group-paths.  Returns a lazy tree of 
   samples according to the structure defined by group-paths.  
   Leaf values are vectors of one or more samples fitting the 
   category defined by the path."
  [group-paths samples]
  
    (if (and (seq samples) 
             (seq group-paths))
      (let [lvl (group-by (first group-paths) samples)
            ks (keys lvl)]
        (for [k ks]
          [k (get-groups (rest group-paths) (get lvl k))]))
      samples))
  
;  
;(defn high-water
;  "Computes the high-water mark for each SRC, by Quarter.  
;   Returns a single representative sample, by quarter, as the 
;   leaves of the tree.  We reduce over the tree, accumulating 
;   the samples.  The result is a subset of the original 
;   samples."
;  [samples]
;  (let [sample-tree  (get-groups [:SRC :Quarter :t] samples)
     
    
            
              









     
  