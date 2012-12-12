;TOM SPOON 9 July 2012 
;A simple library for reading and writing tabular representations of data.
;Uses map as a container.
(ns util.table
  (:require [clojure [string :as strlib]]
            [clojure [set :as setlib]]
            [util [clipboard :as board]]))
			
(defprotocol ITable 
  (table-fields [x] "Get a vector of fields for table x")
  (table-columns [x] "Get a nested vector of the columns for table x"))


(defprotocol IQueryable 
  (select-fields  [x fs] "Select a subset of fields from the queryable.")
  (select-columns [x fs] "Select a subset of the columns from the queryable."))

(defn table-rows
  "Returns a vector of the rows of the table."
  [x]  (map vec (table-columns x)))

(defn row-at [tbl x]
  (vec (map #(get % x) table-columns)))
 
;note -> we should have a transient version of this....
(defn rows->columns [rows]
  (let [colcount (count (first rows))
        tcols 
        (reduce (fn [cols flds] 
                  (reduce (fn [cols [j fld]]  
                            (assoc cols j (conj (get cols j) fld)))
                          cols 
                          (map-indexed (fn [idx fld] [idx fld]) flds)))
                (reduce conj [] (take colcount (repeat [])))
                rows)]
    tcols))

;(defn filter-rows [tbl f]
;  (reduce (fn 
    

;protocol-derived functions 
;(defn select
;  "A small adaptation of Peter Seibel's excellent mini SQL language from 
;   Practical Common Lisp.  This should make life a little easier when dealing 
;   with abstract tables...."  
;  [& {:keys [columns from join-by where unique order-by] 
;      :or {columns true
;           join-by :intersection
;           unique true 
;           where nil
;           order-by nil}}]
;    (->> (select-columns from columns)
;         (if where 
;             (filter 
    


;(defn select-table [store & {:keys [from join-by where order-by] 
;                             :or {from (get-domains store)
;                                  join-by :intersection
;                                  where nil
;                                  order-by nil}}]
;  (->> (select-entities  store 
;         {:from from :join-by join-by :where where :order-by order-by})
;       (add-entities emptystore)))    
    
  

(defrecord column-table [fields columns]
  ITable 
  (table-fields [x]  fields)
  (table-columns [x] columns))

(defn tabular? [x] (satisfies? ITable x))

(defn parse-string 
	"Parses a string, trying various number formats.  Note, scientific numbers,
	 or strings with digits sandwiching an E or an e will be parsed as numbers,
	 possibly as Double/POSITIVE_INFINITY, i.e. Infinity."
	[value]	
  (try (Integer/parseInt value)
	(catch NumberFormatException _
	  (try (Double/parseDouble value)
		(catch NumberFormatException _ value)))))

(def scientific-reg 
	"A regular expression gleefully borrowed from Stack Overflow.  Matches 
	 strings that correspond to scientific numbers."
	#"-?\d*\.?\d+[Ee][+-]?\d+")
	
(defn parse-string-nonscientific 
	"Parses a string, trying various number formats.  Scientific numbers,
	 or strings with digits sandwiching an E or an e will be kept as strings.  
	 Helpful in contexts where there are alphanumeric string values."
	[value]
	(if-let [res (re-find scientific-reg value)]
		value
		(parse-string value)))
	
(defn pair [a b] [a b])
(def re-tab (re-pattern (str \tab)))
(def split-by-tab #(strlib/split % re-tab))
(def empty-table {:fields [] :records []})

(defn tabdelimited->table 
  "Return a map-based table abstraction from reading a string of tabdelimited 
   text.  The default string parser tries to parse an item as a number.  In 
   cases where there is an E in the string, parsing may return a number or 
   infinity.  Set the :parsemode key to any value to anything other than 
   :scientific to avoid parsing scientific numbers."
   [s & {:keys [parsemode keywordize-fields?] 
         :or   {parsemode :scientific
                keywordize-fields? true}}]
  (let [lines (strlib/split-lines s )
        parsef (if (= parsemode :scientific) 
                 parse-string 
                 parse-string-nonscientific)
        parse-rec (comp vec #(map parsef %) split-by-tab)]
    {:fields (vec (map (if keywordize-fields? 
                         keyword
                         identity) (split-by-tab (first lines ))))                 
     :records (persistent! 
                (reduce (fn [rs l] (conj! rs (parse-rec l))) (transient []) 
                      (rest lines )))}))
					  
(defn record-seq 
	"Returns a sequence of records from the underlying table reprsentation.
	 Like a database, all records have identical fieldnames."
	[{:keys [fields records] :as table}]
  (for [r records]
    (into {} (map pair fields r))))

(defn records->table 
	"Takes a sequence of maps (records) and returns a tabular representation 
   of the records.  Infers the field names for the table from the first 
	 record.  Assumes every record has identical fieldnames."
	[recs]
	{:fields (vec (keys (first recs)))
	 :records (vec (map (comp vec vals) recs))})	
  
(defn get-record 
	"Fetches the nth record from a tabular map."
	[{:keys [fields records] :as table} n]
	(into {} (map pair fields (nth records n))))

(defn field->string [f] (cond (string? f) f
                              (keyword? f) (str (subs (str f) 1))
                              :else (str f)))
                               
(defn record-count [t] (count (:records t)))
(defn get-fields [t] (:fields t))
(defn last-record [t] (get-record t (dec (record-count t))))
(defn table->tabdelimited 
  "Render a table into a tab delimited representation."
  [{:keys [fields records]} & {:keys [stringify-fields?]
                               :or {stringify-fields? true}}]
  (reduce 
    (fn [acc rec] (str (apply str acc (interleave rec (repeat \tab))) \newline))
    "" (concat (if stringify-fields? 
                 [(vec (map field->string fields))] 
                 [fields]) records)))

(defmulti as-table
  "Generic function to create abstract tables."
  (fn [t] (class t)) :default :empty)

(defmethod as-table java.lang.String [t] (tabdelimited->table t))
(defmethod as-table clojure.lang.PersistentArrayMap [t] t)

(defn copy-table!
  "Copies a table from the system clipboard, assuming that the clipboard
   contains a string of tab-delimited text."
  [& [parsemode keywordize-fields?]]
  (tabdelimited->table (board/copy!) :parsemode (or parsemode :no-science)
                                     :keywordize-fields? keywordize-fields?))
(defn copy-table-literal! []
  "Copes a table from the system clipboard.  Does not keywordize anything..."
  (copy-table! :no-science false))
  
  

;it'd be nice to have simple sql-like operators....
;we've already got select 
;in SQL, we use
