;Defines operations for building Marathon projects from legacy Excel
;workbook-based data.
(ns marathon.project.excel
  (:use [marathon.project])
  (:require [spork.util.excel [core :as xl] [docjure :as docj]]
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
  {:CompositePolicyRecords "CompositePolicyRecords",
   :DemandRecords          "DemandRecords",
   :SuitabilityRecords     "SuitabilityRecords",
   :PolicyTemplates        "PolicyTemplates",
   :SRCTagRecords          "SRCTagRecords",
   :Parameters             "Parameters",
   :PolicyDefs             "PolicyDefs",
   :PolicyRecords          "PolicyRecords",
   :SupplyRecords          "SupplyRecords",
   :RelationRecords        "RelationRecords",
   ;:demand-table-schema "demand-table-schema",
   :PeriodRecords          "PeriodRecords"})

;;When we parse from excel, sometimes we'll get results,
;;primarily numerics, that get parsed as something
;;other than we'd like (doubles...)
;;(defn coerce [s tbl])
;;We may need to perform coercions later...but for now
;;I think we're okay.

;(defn coerce-nums [s tbl]
;  (let [nums (

              
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
                       (if-let [sht (xl/as-sheet sheetname wb)]
                         (do (println nm)                   
                             [(keyword nm)
                              (spork.util.table/keywordize-field-names
                               (xl/sheet->table sht))])
                         (println [:missing nm])))))))

;;This is all that really matters from marathon.project...   
(defmethod load-project "xlsm" [path & {:keys [tables]}]
    (let [ts    (marathon-book->marathon-tables path)
        paths (reduce-kv (fn [acc nm _]
                           (assoc acc nm
                                  [path
                                   (name nm)])) {} ts)]    
      (-> (make-project
           (clojure.string/replace (io/fname path) #".xlsm" "")
           (io/as-directory (io/fdir path))) 
          (assoc :tables ts)
          (update :paths merge paths))))

(defmethod load-project "xlsx" [path & {:keys [tables]}]
  (let [ts    (marathon-book->marathon-tables path)
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
  (xl/tables->xlsx path (project->tables (add-path proj 
                                           :project-path 
                                           (io/as-directory (io/fdir path))))))

;;this should probably go in docjure..
(defn copy-sheet! [sheetname wb1 wb2]
  (if-let [from-sheet (docj/select-sheet sheetname wb1)]
     (let [to-sheet   (if-let [s (docj/select-sheet  sheetname wb2)]
                        (do (docj/remove-all-rows! s) s) ;this is probably slow
                        (do (docj/add-sheet! wb2 sheetname)
                            (docj/select-sheet sheetname wb2)))]
       (->> (xl/contiguous-rows from-sheet) 
            (map xl/row->vec)
            (docj/add-rows! to-sheet)))
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
