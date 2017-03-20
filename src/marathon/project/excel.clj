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

(defn maybe-int [x]
  (if (and x (not (blank? x)))
    (int x)
    0))

(defn maybe-long [x]
  (if (and x (not (blank? x)))
    (long x)
    0))

(def string->boolean (:boolean spork.util.parsing/parse-defaults))
(defn as-boolean [x]
  (if (instance? java.lang.Boolean x) x
      (string->boolean x)))

;;This is a little squirelly, and exists solely to
;;cope with excel's handling of numbers and autoparsing
;;text wierdness.
(defn coerce [s tbl]
  (let [numtypes {:boolean as-boolean
                  :int maybe-int #_int
                  :int? (fn [x] (when x (int x)))                  
                  :long maybe-long #_long
                  :long? (fn [x] (when x (long x)))
                  :text  (fn [x] (if (number? x)
                                   ;;we need to coerce this mofo.
                                   (str (try-long x))
                                   x))}]
    (spork.util.table/conj-fields 
     (for [[fld col] (tbl/enumerate-fields (tbl/table-fields tbl) (tbl/table-columns tbl))]
       (if-let [f (numtypes (s fld))]
         (try [fld (mapv f col)]
              (catch Exception e (do (println [:error-in fld])
                                     (throw e))))
         [fld col]))
     spork.util.table/empty-table)))

;;Another option is to keep everything in tsv....and
;;edit/update from excel.  There are advantages and
;;disadvantages to this...
(defn marathon-book->marathon-tables 
  [wbpath & {:keys [tables] :or {tables 
                                 marathon-workbook-schema}}]
  "Extract a map of canonical tables to a map with the same name.  Caller can 
   supply additional tables, or supply the :all keyword to get all tables."
  (let [wb   (xl/as-workbook wbpath)]
    (into {} (filter identity
                     (for [[nm sheetname] (seq tables)]
                       (do (println [:loading nm])
                           (if-let [sht (xl/as-sheet sheetname wb)]
                             (let [tab (spork.util.table/keywordize-field-names
                                        (xl/sheet->table sht))
                                   tab (if-let [s (schemas/get-schema nm)]
                                         (coerce s tab)
                                         tab)]
                               (do (println [:loaded nm])
                                   [(keyword nm)  tab]))
                             (println [:missing nm]))))))))

;;This is all that really matters from marathon.project...   
(defmethod load-project "xlsm"
  [path & {:keys [tables]
           :or {tables marathon-workbook-schema}}]
    (let [ts    (marathon-book->marathon-tables path :tables tables)
        paths (reduce-kv (fn [acc nm _]
                           (assoc acc nm
                                  [path
                                   (name nm)])) {} ts)]    
      (-> (make-project
           (clojure.string/replace (io/fname path) #".xlsm" "")
           (io/as-directory (io/fdir path))) 
          (assoc :tables ts)
          (update :paths merge paths))))

(defmethod load-project "xlsx"
  [path & {:keys [tables]
           :or {tables marathon-workbook-schema}}]
  (let [ts    (marathon-book->marathon-tables path :tables tables)
        paths (reduce-kv (fn [acc nm _]
                           (assoc acc nm
                                  [path
                                   (name nm)])) {} ts)]
  (-> (make-project
        (clojure.string/replace (io/fname path) #".xlsx" "")
        (io/as-directory (io/fdir path))) 
      (assoc :tables ts)
      (update :paths merge paths))))

(defmethod save-project "xlsx" [proj path & options]
  (xl/tables->xlsx path
     (project->tables
        (add-path proj :project-path 
            (io/as-directory (io/fdir path))))))

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
