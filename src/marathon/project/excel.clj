;Defines operations for building Marathon projects from legacy Excel
;workbook-based data.
(ns marathon.project.excel
  (:use [marathon.project])
  (:require [marathon.schemas :as schemas]
            [spork.util.excel [core :as xl] [docjure :as docj]]
            [spork.util       [io :as io]
                              [table :as tbl]]))
;;From here on out, we're not worrying about post-processing legacy excel
;;projects.  This is strictly for loading a marathon input project
;;from a workbook, or finding whatever canonical tables we can, and
;;then preparing it for processsing.

;;We should be able to adapt this pretty easily to a project coming from
;;tsv, just use schemas to do it...

;;Note checking for canonical fields....this could
;;be problematic...

;;Given a Marathon workbook, we know that these are the tables we'll care about.
(def marathon-workbook-schema
  {:CompositePolicyRecords "CompositePolicyRecords"
   :DemandRecords          "DemandRecords"
   :SuitabilityRecords     "SuitabilityRecords"
   :PolicyTemplates        "PolicyTemplates"
   :SRCTagRecords          "SRCTagRecords"
   :Parameters             "Parameters"
   :PolicyDefs             "PolicyDefs"
   :PolicyRecords          "PolicyRecords"
   :SupplyRecords          "SupplyRecords"
   :RelationRecords        "RelationRecords"
   ;:demand-table-schema "demand-table-schema"
   :PeriodRecords          "PeriodRecords"})

;;Schemas that support requirements analysis...
(def marathon-requirements-schema
  (merge marathon-workbook-schema
     {:GhostProportionsAggregate "GhostProportionsAggregate"}))

(def input-sheets marathon-workbook-schema)
;;When we parse from excel, sometimes we'll get results,
;;primarily numerics, that get parsed as something
;;other than we'd like (doubles...)
;;(defn coerce [s tbl])
;;We may need to perform coercions later...but for now
;;I think we're okay.

;;Since we know the schema, we can coerce the
;;schema value...
;;or we can just read as string values...
(defn try-long [x] (if (zero? (mod x 1) )
                       (long x)
                       x))

(def ^:const +blank+ "")
(defn blank? [x] (identical? x +blank+))

(defn =word
  "Like `=` for strings, except that the arguments are first
  case-folded before comparison with `=`."
  [s1 s2]  
  (= (clojure.string/lower-case s1) (clojure.string/lower-case s2)))

(defn as-int
  "Try to find an int representation for `x`."
  [x]
  (cond (string? x) (cond (=word "-inf" x) Integer/MIN_VALUE
                          (=word "inf"  x) Integer/MAX_VALUE
                          :else (let [thing (clojure.edn/read-string x)]
                                  (if (number? thing)
                                    (int thing)
                                    (do (println "(Replacing" x "with 0.)")
                                        0))))
        (number? x) (int x)
        :else       0))

(defn as-long
  "Try to find a long representation for `x`."
  [x]
  (cond (string? x) (cond (=word "-inf" x) Long/MIN_VALUE
                          (=word "inf"  x) Long/MAX_VALUE
                          :else (let [thing (clojure.edn/read-string x)]
                                  (if (number? thing)
                                    (long thing)
                                    (do (println "(Replacing" x "with 0.)")
                                        0))))
        (number? x) (long x)
        :else       0))

(def string->boolean (:boolean spork.util.parsing/parse-defaults))
(defn as-boolean [x]
  (if (instance? java.lang.Boolean x) x
      (string->boolean x)))

;;This is a little squirelly, and exists solely to
;;cope with excel's handling of numbers and autoparsing
;;text wierdness.
(defn coerce [s tbl]
  (let [numtypes {:boolean as-boolean
                  :int as-int
                  :int? (fn [x] (when x (int x)))
                  :long as-long
                  :long? (fn [x] (when x (long x)))
                  :text  (fn [x] (if (number? x)
                                   ;;we need to coerce this mofo.
                                   (str (try-long x))
                                   x))
                  :clojure clojure.edn/read-string}]
    (spork.util.table/conj-fields
     (for [[fld col] (tbl/enumerate-fields (tbl/table-fields tbl) (tbl/table-columns tbl))]
       (if-let [f (numtypes (s fld))]
         [fld (mapv (fn [row val]
                      (try (f val)
                           (catch Exception e
                             (do (println [:error-in fld :row row :value val])
                                 (throw e)))))
                    (iterate inc 0)
                    col)]
         [fld col]))
     spork.util.table/empty-table)))

;;Another option is to keep everything in tsv....and
;;edit/update from excel.  There are advantages and
;;disadvantages to this...
(defn marathon-book->marathon-tables
  "Extract a map of canonical tables to a map with the same name.  Caller can
   supply additional tables, or supply the :all keyword to get all
  tables."
  [wbpath & {:keys [tables] :or {tables
                                 marathon-workbook-schema}}]
  ;;Only clean the string with io/file-path if wbpath is a string.
  ;;Alternatively, we might be passing a resource.
  (let [wb (xl/as-workbook (if (string? wbpath)
                             (io/file-path wbpath)
                             wbpath))]
    (into {} (filter identity
                     (for [[nm sheetname] (seq tables)]
                       (do (print "Loading" sheetname ". . . ")
                           (if-let [sht (xl/as-sheet sheetname wb)]
                             (let [tab (spork.util.table/keywordize-field-names
                                        (xl/sheet->table sht))
                                   tab (if-let [s (schemas/get-schema nm)]
                                         (coerce s tab)
                                         tab)]
                               (println "done.")
                               [(keyword nm) tab])
                             (println "(missing)."))))))))

(defn load-workbook
  [path file-extension & {:keys [tables]
                          :or {tables marathon-workbook-schema}}]
  (let [ts    (marathon-book->marathon-tables path :tables tables)
        paths (reduce-kv (fn [acc nm _]
                           (assoc acc nm
                                  [path
                                   (name nm)])) {} ts)]
    (-> (make-project
         ;;Check if path is a string before calling fname in case it's
         ;;a resource.
           (clojure.string/replace (if (string? path)
                                     (io/fname path)
                                     path)
                                   (re-pattern
                                                    file-extension) "")           
           (io/as-directory (io/fdir path)))
          (assoc :tables ts)
          (update :paths merge paths))))

(defmethod load-project "xlsm"
  [path & {:keys [tables]
           :or {tables marathon-workbook-schema}
           :as m}]
  ;;Collect up the args and send them to load-workbook
  (apply load-workbook (apply concat [path ".xlsm"] m)))

(defmethod load-project "xlsx"
  [path & {:keys [tables]
           :or {tables marathon-workbook-schema}
           :as m}]
  (apply load-workbook (apply concat [path ".xlsx"] m)))

;;Loading a clojure.java.io/resource
(defmethod load-project java.net.URL
  [path & {:keys [tables]
           :or {tables marathon-workbook-schema}
           :as m}]
  (let [ts    (marathon-book->marathon-tables path :tables tables)]
    (-> (make-project
         ;;Just name the project with path, because fname will throw
         ;;an exception from within the uberjar.
           (clojure.string/replace path #".xlsm|.xlsx" "")
                                   ;;no paths for a resource
                                   ;;since we'll be using the MARATHON
                                   ;;history directly with no i/o for now.
                                    "" :paths {}) 
          (assoc :tables ts))))

(defn stringify-tables [tables]
  (reduce-kv (fn [tables table-key table]
               (assoc tables table-key
                      (tbl/stringify-field-names
                       table)))
             {} tables))

(ns dk.ative.docjure.spreadsheet)
;;covers clojure datatypes like clojure.lang.Keyword and
;;clojure.lang.Symbol
(defmethod set-cell! :default [^Cell cell val]
  (set-cell! cell (str val)))
(ns marathon.project.excel)

(defmethod save-project "xlsx" [proj path & options]
  (->> proj
       (:tables)
       ;;Need to stringify if we reuse the workbook for another
       ;;marathon run, because the field names in the input workbook
       ;;are expected to be stringified.
       (stringify-tables)
       (xl/tables->xlsx path)))

;;this should probably go in docjure..
(defn copy-sheet! [sheetname wb1 wb2]
  (if-let [from-sheet (docj/select-sheet sheetname wb1)]
     (let [to-sheet   (if-let [s (docj/select-sheet  sheetname wb2)]
                        (do (docj/remove-all-rows! s) s) ;this is probably slow
                        (do (docj/add-sheet!   wb2 sheetname)
                            (docj/select-sheet sheetname wb2)))]
       (->> (xl/contiguous-rows from-sheet)
            (map                xl/row->vec)
            (docj/add-rows!     to-sheet)))
     (throw (Exception. (str "Sheet " sheetname "does not exist")))))

(defn copy-sheets! [sheetnames wb-from wb-to]
  (doseq [sheetname sheetnames]
    (copy-sheet! sheetname wb-from wb-to)))

;;This is to support cloning from legacy projects to new projects.
;;The intent is to make higher order operations like case setups and design
;;of experiments a lot easier to do.
(defmethod derive-project ["xlsm" "xlsm"] [proj destination & {:keys [tables]}])

;;clone tables from one workbook to another.
(defmethod migrate-project ["xlsm" "xlsm"]
  [proj destination & {:keys [sheetnames] :or {sheetnames
                                                (vals input-sheets)}}]
    (let [wb-from  (docj/load-workbook proj)
          wb-to    (if (io/fexists? destination)
                     (docj/load-workbook destination)
                     (docj/create-workbook "MigrationInfo" [["Source"]
                                                            [proj]]))]
      (do (copy-sheets! sheetnames wb-from wb-to)
          (docj/save-workbook! destination wb-to ))))


;;I don't like copy and pasting...
(defmethod migrate-project ["xlsx" "xlsx"]
  [proj destination & {:keys [sheetnames] :or {sheetnames
                                               (vals input-sheets)}}]
      (let [wb-from  (docj/load-workbook proj)
            wb-to    (if (io/fexists? destination)
                     (docj/load-workbook destination)
                     (docj/create-workbook "MigrationInfo" [["Source"]
                                                            [proj]]))]
      (do (copy-sheets! sheetnames wb-from wb-to)
          (docj/save-workbook! wb-to destination))))

;;testing
(comment
;;bad example, corrupeted, parameters doesn't show up.
;(def wbpath   "C:\\Users\\tom\\Documents\\Marathon_NIPR\\MPI_3.760298326.xlsm")
(def wbpath2   "C:\\Users\\tom\\Documents\\Marathon_NIPR\\MPI_3.76029832.xlsm")
(def destpath  "C:\\Users\\tom\\Documents\\Marathon_NIPR\\MigrationTest.xlsm")

(migrate-project wbpath2 destpath)

;; (comment
;; (def input-sheets
;;   (select-keys marathon-workbook-schema
;;                [:supply-records
;;                 :demand-records
;;                 :period-records
;;                 :relation-records
;;                 :src-tag-records
;;                 :parameters]))

;; ;These are canonical outputs from a VBA Marathon run for capacity analysis.
;; (def marathon-text-file-output
;;   {:cycle-records  "cycles.txt"
;;    :event-log      "EventLog.csv"
;;    :demand-trends  "DemandTrends.txt"
;;    :sand-trends    "SandTrends.txt"
;;    :locations      "locations.txt"})
;; )
)
