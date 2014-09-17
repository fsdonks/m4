;;A suite of tools for reading stoke cases from various sources like
;;workbooks, folders, clojure files, etc.
(ns marathon.processing.stoke.io
  (:require [spork.util.io :refer :all]
            [spork.util.table :refer :all]
            [marathon.processing.stoke.core :refer :all]
            [marathon   [schemas :as schemas]]
            [spork.util [temporal :as temporal]]
            [spork.util.excel [core :as xl]]))
;;ADDED
;;=====
;;We need to determine, from a time series, not only what our peak is,
;;but also what our average demand is, as in a time-weighted average 
;;demand.


;;The original paradigm had us reading demands from files. 
;;In other words, we generate a corpus of futures, and re-use those 
;;futures for multiple runs.  The implementation of spork.util.table 
;;may be slowing us down on the parsing/IO front, so this avenue may 
;;be a bit rough for large sample sizes of futures.  We'll see..
(defn demand-case? [path]
  (let [path (clojure.string/upper-case path)]
    (and (.contains path "CASE")
         (.contains path ".TXT")
         (not (.contains path ".PEAK")))))

;Patched a problem not using the file-filter.
(defn demand-file-paths 
  "Rips some stochastic futures from a path."
  ([path file-filter]
     (->> (list-files path)
          (map fpath)
          (filter file-filter)))
  ([path] (demand-file-paths path demand-case?)))

;;This is going to slow us down.  It's IO bound.
(defn demand-file->table [path]
  (tabdelimited->table (slurp path) 
                       :parsemode :no-science
                       :schema (schemas/get-schema :demand-table-schema)))

;;Cloned from stoke.core.....
(defn total-quantity
  "A custom peak function to determine which samples in the demand activity 
   profile are considered highest."
  [{:keys [actives] :as activity-record}]
  (reduce + (map :Quantity actives)))  

(defn table->peaks [xs]
  (temporal/peaks-by :SRC  (if (tabular? xs) (table-records xs) xs)
                     :start-func :StartDay  :duration-func :Duration
                     :peak-function total-quantity))

;; (defn table->averages [xs]   
;;   (partition 2 1 
;;              (temporal/activity-profile (if (tabular? xs) (table-records xs) xs)
;;                                         :start-func :StartDay  :duration-func :Duration))
;;   (reduce (fn [acc pair]  
;;             (let [prev  (first  pair)
;;                   nxt   (second pair)
;;                   delta (- (first nxt) (first prev))]         
  

;;This guy is producing too many records.  Probably pulling too many 
;;peaks, double check!
(defn peaks->records [peaks]
  (->> (for [[k v] peaks] 
         (let [t (:t v)
               peak-key (str k "_" t)]
           (map (fn [r] (-> r (assoc :PeakTime t) (assoc :PeakKey peak-key)))
                (:actives v))))
       (reduce (fn [acc xs] (reduce conj acc xs)))))

(defn peak-name        [path]    (str path ".peak.txt"))
(defn labeled-peak-name        [table]    
  (let [r (get-record table 0)
        case (:case-name r)
        fut  (:case-future r)]
  (str case "_" fut ".peak.txt")))

(defn peak-table->path [path xs] (spit (peak-name path) (table->tabdelimited xs)))

;;should add this later.  for now, just get the scraping done.  leave intermediate 
;;results in a subfolder.
;(defmethod as-demand-stream 

(defn folder->futures 
  "Given a folder path, produces a demand stream of all the futures.  This is the coarsest way to go."
  [path & {:keys [file-filter] :or {file-filter demand-case?}}]
  (->> (demand-file-paths  path  file-filter)
       (map demand-file->table)))

;;this is useful for stoke.
(defn folder->random-demand-tables [path & {:keys [file-filter] :or {file-filter demand-case?}}]
  (->> (folder->futures path :file-filter file-filter)
       (map (comp records->table peaks->records table->peaks))))

(defn src-quantities
  "Returns the quantities, by src, of each record in the table.  Basically a table 
   reduction, by SRC field, aggregating the Quantity field by count.  Seems like we 
   could rephrase this is as a generic table reduce."
  [t]
  (->> (select :fields [:SRC :Quantity] :from t)
       (table-rows)
       (group-by first)              
       (reduce-kv (fn [acc k entries] 
                    (assoc acc k (reduce + (map second entries))))
                  {}))) 

;;__Temporary Patch__
(defn samples->decile-table 
  "This is a variation of marathon.processing.stoke.core/performance-table."
  [src-counts]
  (let [as-percentile (memoize 
                       (fn [n] (keyword (str (* (inc n) 10) "th_Percentile"))))
        ordered-fields (into [:src] (map as-percentile  (range 9)))
        expand-record (fn [[k xs]] 
                        (->> (marathon.processing.stoke.core/fixed-deciles xs)                      
                             (map-indexed (fn [idx v] [(as-percentile idx) v]))
                             (into {:src k})))
        _             (println [:building :table])]
     (->> (map expand-record src-counts)
          (records->table)
          (order-by [:src])
          (select-fields ordered-fields)
          )))
    
(defn src-samples->peaks 
  "Converts a map of {src [peak1 peak2 peak3]}, where peaks are reduced samples 
   from demand records, into a table of {src peak_qty}"
  [samples]  
  (for [[src xs] samples
        x        xs]
    {:src src :peak-quantity x}))

(defn src-samples->peaks-table
  [samples]  
  (records->table (src-samples->peaks samples)))
    
;;__Patched__
;;The "count" function here, used to compute names programmatically, 
;;was forcing the entire sequence of demands to be realized.  Changed
;;to iterate, so it's lazily computed now.
(defn dump-peaks 
  [root peak-tables & {:keys [names] 
                       :or {names (map labeled-peak-name peak-tables)}}]
  (let [abs-path (partial relative-path root)
        samples  (atom {})
        push-sample! (fn [src-counts]              
                      (swap! samples 
                       (fn [current-sample]                        
                         (reduce (fn [acc [src quantity]]
                                   (assoc acc src (conj (get acc src []) quantity)))
                                 current-sample
                                 src-counts))))]                    
    (doseq [[name tbl] (map vector names peak-tables)]
      (push-sample! (seq (src-quantities tbl)))
      (hock (abs-path [name]) (table->tabdelimited tbl)))
    (when @samples
      (hock (abs-path ["deciles.txt"]) 
            (table->tabdelimited (samples->decile-table @samples)))
      (hock (abs-path ["peak-samples.txt"]) 
            (table->tabdelimited (src-samples->peaks-table @samples))))))  

;;tom hack...
(defn dump-peak-stats 
  [root]
  (let [abs-path (partial relative-path root)
        samples  (atom {})
        push-sample! (fn [src-counts]              
                      (swap! samples 
                       (fn [current-sample]                        
                         (reduce (fn [acc [src quantity]]
                                   (assoc acc src (conj (get acc src []) quantity)))
                                 current-sample
                                 src-counts))))]                    
    (doseq [tbl  (folder->random-demand-tables root :file-filter (fn [path] (.contains (clojure.string/upper-case path) "PEAK")))]
      (push-sample! (seq (src-quantities tbl))))
    (when @samples
      (hock (abs-path ["deciles.txt"]) 
            (table->tabdelimited (samples->decile-table @samples)))
      (hock (abs-path ["peak-samples.txt"]) 
            (table->tabdelimited (src-samples->peaks-table @samples)))))) 

(defn compute-peaks [path & [target]]
  (->> (folder->random-demand-tables path)
       (dump-peaks (or target path))))

(defn compute-peak-stats [path & [target]]
  (->> (folder->random-demand-tables path)
       (dump-peak-stats (or target path))))

;;Project management and data parsing stuff.
(defn eval-strings [x] (if (string? x) (read-string x) x))
(defn parse-table [name tbl f]
  [name (into {} (map f (table-records (keywordize-field-names tbl))))])

(defn eval-filter [xs]
  (if (= (first xs) :not)
    (let [forbidden (second xs)]
      (fn [src compo]  (not= compo forbidden)))
    (throw (Exception. (str "Unknown SupplyNode Filter" xs)))))

(defn set-default-filter [filtermap]
  (if (contains? filtermap ":default")
    (assoc filtermap :default (get filtermap ":default"))
    filtermap))

;;Reading From Case Files / Preparing Cases
;;=========================================

;;We define a way to rip apart a database (either a set of tabular files or 
;;an excel workbook or whatever) into a map of resources to projects.
;;Parsers for different tables.
(defmulti  read-stoke (fn [name tbl] name))
(defmethod read-stoke :default [name tbl]
  (if (= 1 (count (table-fields tbl)))
    [name (first (table-columns tbl))]
    [name (table-records tbl)]))
(defmethod read-stoke :DemandPreferences   [name tbl]
  [name (into {} (map (fn [r] [(eval-strings (:Parent r))
                               (eval-strings (:Children r))])
                      (table-records (keywordize-field-names tbl))))])
(defmethod read-stoke :StrengthConstraints [name tbl]
  [name (into {} (map (fn [r] [(eval-strings (:Parent r))
                               (eval-strings (:Children r))])
                      (table-records (keywordize-field-names tbl))))])

(defmethod read-stoke :SupplyNodeFilters [name tbl]
  [name (set-default-filter 
         (reduce (fn [acc r] 
                   (let [xs (conj (get acc :SRC [])
                                  (eval-filter (eval-strings (:Filter r))))]                    
                     (assoc acc (:SRC r) xs))) 
                 {}
                 (table-records (keywordize-field-names tbl))))])
;;Need to add data validation here.  THere were 0 str values coming in
;;and causing divide-by-zero errors.
(defmethod read-stoke :SRCs [name tbl]
 (parse-table name tbl (juxt  :SRC  :Strength)))
(defmethod read-stoke :Components [name tbl]
 [name (set (map eval-strings (first (table-columns tbl))))])
(defmethod read-stoke :Case [name tbl]
 [name (first (table-records (keywordize-field-names tbl)))])
(defmethod read-stoke :InitialSupply [name tbl]
 [name  (table-records (keywordize-field-names tbl))])
(defmethod read-stoke :SupplyGroups [name tbl]
  [name (group-by :Parent
                  (map (fn [r] (-> r 
                                   (assoc :Quantity (eval-strings (:Quantity  r)))
                                   (assoc :Strength (eval-strings (:Strength  r)))))
                      (table-records (keywordize-field-names tbl))))])
(defmethod read-stoke :Fixed [name tbl]
  [name  (table-records (keywordize-field-names tbl))])
(defmethod read-stoke :Bounded [name tbl]
  (parse-table name tbl (juxt  :SRC  :Quantity)))

;;Facilities for reading a stoke case from a workbook, yields a simple 
;;project map.
(defn workbook->stoke-project [path] 
  (into {}
     (for [[name tbl] (xl/wb->tables (xl/as-workbook path))]
       (read-stoke (keyword name) tbl))))
