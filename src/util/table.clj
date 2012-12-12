;TOM SPOON 9 July 2012 
;A simple library for reading and writing tabular representations of data.
;Uses map as a container.
(ns util.table
  (:require [clojure [string :as strlib]]
            [clojure [set :as setlib]]
            [util [clipboard :as board]]))

(defn vec-filter
  "Vector-specific filter operation.  Since we're using vectors for our
   tables, this should be optimized a bit.  Not sure if it'll pay off though."
  [f v]
  (persistent! (reduce conj! (transient []) (filter f v))))  

(defn vec-vals
  "Returns an in-order vector of the values of persistent map m.
   Coercing vals into a vector using "
  [m]
  (vec (reverse (vals m))))

(defn transient-vector? [v]
  (= type v clojure.lang.PersistentVector$TransientVector))


(defprotocol ITable 
  (table-fields [x] "Get a vector of fields for table x")
  (table-columns [x] "Get a nested vector of the columns for table x"))

(defprotocol IQueryable 
  (select-fields  [x fieldspecs] "Select a subset of fields from the queryable."))

(defrecord column-table [fields columns]
  ITable 
  (table-fields [x]  fields)
  (table-columns [x] columns)
  IQueryable
  (select-fields [x fieldspecs] 
                 (let [idxs (reduce (fn [acc [i fld]]
                                        (if (f fld)
                                          (conj acc i)
                                          acc)) [] (map-indexed vector fields))
                       flds (set (map #(get fields %) idxs))]
                   (->column-table (vec-filter flds fields)
                                   (vec (map #(get columns %) idxs))))))

(defn make-table [fields columns]
  (->column-table fields columns))

(defn count-rows [tbl]
  (count (first (table-columns tbl))))

(defn valid-row?
  "Ensures n is within the bounds of tbl."
  [tbl n]  (and (>= n 0) (< n (count-rows tbl))))
  
(defn nth-row 
  [tbl n]
  "Extracts the nth row of data from tbl, returning a vector."
  (assert (valid-row? tbl n) (str "Index " n " Out of Bounds"))
  (vec (map #(get % n) (table-columns tbl))))

(defn table-rows
  "Returns a vector of the rows of the table."
  [tbl]  (vec (map (partial nth-row tbl) (range (count-rows tbl)))))

(defn nth-record
  "Coerces a column-oriented vector representation of tables into 
   a map where keys are field names, and vals are column values at 
   the nth record."
  [tbl n & [flds]]
  (assert (valid-row? tbl n) (str "Index " n " Out of Bounds"))  
  (zipmap (if flds flds (reverse (table-fields tbl))) 
          (reverse (nth-row tbl n))))

(defn table-records
  "Fetches a sequence of n records, where records are maps where keys correspond
   to fields in the table, and values correspond to the values at row (n - 1)."
  [tbl]
  (let [flds (reverse (table-fields tbl))]
    (map (fn [n] (nth-record tbl n flds)) (range (count-rows tbl)))))

(defn conj-row
  "Conjoins a rowvector on a vector of columns."
  [columns rowvector]
  (assert (= (count rowvector) (count columns)))
  (reduce (fn [acc [j x]] (assoc acc j (conj (get acc j) x)))
          columns (map-indexed vector rowvector)))

(defn- transient-columns
  "Returns a transient set of columns"
  [cols]
  (reduce (fn [acc v] (conj! acc (transient v)))
                  (transient []) cols))

(defn- persistent-columns!
  "Persists the columns from a transient state."
  [tcols]
  (reduce (fn [acc tc]  (conj acc (persistent! tc))) [] (persistent! tcols)))


(defn transpose
  "Changes rows to columns.  Faster, using transients."
  [rowvectors]
  (let [colcount (count (first rowvectors))
        res 
        (reduce (fn [tcols row] 
                  (reduce (fn [cols [j fld]]  
                            (assoc! cols j (conj! (get cols j) fld)))
                          tcols 
                          (map-indexed vector row)))
                (reduce conj! (transient []) 
                        (take colcount (repeatedly (fn []  (transient [])))))
                rowvectors)]
    (persistent-columns! res)))

(defn- conj-row! [transientcolumns rowvector]
  (reduce (fn [acc [j x]] (assoc! acc j (conj! (get acc j) x)))
          transientcolumns (map-indexed vector rowvector)))

(defn conj-rows
  "Conjoins multiple rowvectors.  Should be fast, using transients.
   Returns a persistent collection."
  [columns rowvectors]
  (assert (= (count (first rowvectors)) (count columns)))
  (persistent-columns! 
    (reduce conj-row! (transient-columns columns) rowvectors)))  

(defn records->table
  "Interprets a sequence of records as a table, where fields are the keys of 
   the records, and rows are the values."
  [recs]
  (let [flds (vec (keys (first recs)))]
    (make-table flds (conj-rows (vec (map (fn [_] []) flds))
                                (map vals recs)))))

(defn filter-rows
  "Returns a subset of rows where f is true.  f is a function of 
   type f::vector->boolean, since rows are vectors...."
  [f tbl ]
  (vec (filter f (table-rows tbl)))) 

(defn filter-records
  [f tbl]
  "Returns a subtable, where the rows of tbl have been filtered according to 
   function f, where f is of type ::record->boolean, where record is a map 
   where the keys correspond to tbl fields, and the values correspond to row  
   values."
  (records->table (filter f (table-records tbl))))

(defn concat-tables
  "Concatenates two or more tables.  Concatenation assumes that "
  [tbls]
  (let [flds (table-fields (first tbls))]
  (assert (every? (= (table-fields %) flds) tbls) "Table fields "

(defn join? [keyspec]
  (if (atom? keyspec)
    (fn [tbls]
      (filter-
        
(defn inner-join
  "Simple join between two foreign fields.
   key-spec is either an atom (typically a keyword), or 
   a vector of atoms that define fields to be joined on.
   Returns a table that is the union of records from  
   all tables where the specified fields in the key-spec are 
   equal."
  [key-spec tables]

  
  
  

;protocol-derived functions 
(defn select
  "A small adaptation of Peter Seibel's excellent mini SQL language from 
   Practical Common Lisp.  This should make life a little easier when dealing 
   with abstract tables...."  
  [& {:keys [columns from join-by where unique order-by] 
      :or {columns true
           join-by :intersection
           unique true 
           where nil
           order-by nil}}]
  



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
	"Returns a sequence of records from the underlying table representation.
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
