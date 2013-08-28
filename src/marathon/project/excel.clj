;Defines operations for building Marathon projects from legacy Excel
;workbook-based data.
(ns marathon.project.excel
  (:use [marathon.project])
  (:require [spork.util.excel [core :as xl]]
            [spork.util       [io :as io]]))

;Given a Marathon workbook, we know that these are the tables we'll care about
;during auditing.
(def marathon-workbook-schema  
  {:deployments      "Deployments"     ;output
   :in-scope         "InScope"         ;output 
   :out-of-scope     "OutOfScope"      ;output                     
   :supply-records   "SupplyRecords"   ;input
   :demand-records   "DemandRecords"   ;input
   :period-records   "PeriodRecords"   ;input
   :relation-records "RelationRecords" ;input
   :src-tag-records  "SRCTagRecords"   ;input
   :parameters       "Parameters"})    ;input


;These are canonical outputs from a VBA Marathon run for capacity analysis. 
(def marathon-text-file-output 
  {:cycle-records  "cycles.txt" 
   :event-log      "EventLog.csv"
   :demand-trends  "DemandTrends.txt"
   :sand-trends    "SandTrends.txt"
   :locations      "locations.txt"})       

(defn marathon-book->marathon-tables 
  [wbpath & {:keys [tables] :or {tables 
                                 marathon-workbook-schema}}]
  "Extract a map of canonical tables to a map with the same name.  Caller can 
   supply additional tables, or supply the :all keyword to get all tables."
  (let [wb (xl/as-workbook wbpath)]
    (into {} (for [[nm sheetname] (seq tables)]
               [nm (xl/sheet->table (xl/as-sheet sheetname wb))])))) 

(defmethod load-project "xlsm" [path & {:keys [tables]}] 
  (-> (make-project
        (clojure.string/replace (io/fname path) #".xlsx" "")
        (io/as-directory (io/fdir path))) 
      (assoc :tables 
         (marathon-book->marathon-tables path))))

(defmethod load-project "xlsx" [path & {:keys [tables]}] 
  (-> (make-project
        (clojure.string/replace (io/fname path) #".xlsx" "")
        (io/as-directory (io/fdir path))) 
      (assoc :tables 
         (marathon-book->marathon-tables path))))

(defmethod save-project "xlsx" [proj path & options]
  (xl/tables->xlsx path (project->tables (add-path proj 
                                           :project-path 
                                           (io/as-directory (io/fdir path))))))

