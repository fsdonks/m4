;post processing routines for marathon.  Includes a simple one-off command 
;line processor.  Since most Marathon stuff is handled via a root directory, 
;the processor (and any others) should be able to apply post processing to a 
;given path. 
(ns marathon.processing.post
  (:require [util [io :as io] [table :as tbl]]
            [marathon.processing 
             [excel :as xl]
             [highwater :as highwater] 
             [fillrates :as fill]]))


(declare compute-highwater compute-fillrates build-audit-trail)

(defn get-environment
  "Returns a default starting environment.  If an environment is provided, 
   passes the environment through."
  [& [env]] 
  (if-let [e env]
    e
    (merge io/common-paths {:current-dir io/*current-dir*})))

;some defaults for post processing.

(declare compute-fillstats) 
(def default-process [compute-highwater 
                      compute-fillrates 
                      compute-fillstats 
                      build-audit-trail])

(defn marathon-project
  "Defines a marathon project structure.  The default work to be done is to 
   compute summary trends for a typical run."
  [rootdir & {:keys [folderspec processes]
              :or {folderspec {"Output" (io/readme)}
                   processes default-process}}]
  :not-implemented)

;(defn build-audit-trail 
;  "Compiles an audit trail from Marathon output."
;  [project-path all-input-tables demandtrends-path fillstats-path]
;  [(proc-log (str "Building audit trail from project in " project-path))])

;A map of paths to resources, relative to a project-path.
(def default-paths {:deployments ["Deployments.txt"]
                    :demand-trends ["DemandTrends.txt"] 
                    :in-scope ["InScope.txt"]
                    :out-of-scope ["OutOfScope.txt"]
                    :demand-records ["DemandRecords.txt"]
                    :parameters ["Parameters.txt"]
                    :period-records ["PeriodRecords.txt"]
                    :relation-records ["RelationRecords.txt"]
                    :src-tag-records ["SRCTagRecords.txt"]
                    :supply-records ["SupplyRecords.txt"] 
                    :titles ["Titles.txt"]})

;a sample environment we'll use for structuring our project...
(def sample-env 
  {:paths {:project-path "the root path"
           :deployments ["A relative path to deployments"]}
   :tables {:res-name {:fields [:field1] :columns [["tom"]]}}
   :some-resource "blah"
   :resource-2 "blee"})

(defn project-path
  "Fetch the project path, or the root folder where the project is located."
  [prj] (get-in prj [:paths :project-path]))

(defn get-path
  "Fetch an absolute path to a resource, as defined by the relative path 
   associated with resname."
  [prj resname]
  (io/relative-path 
    (project-path prj) (get-in prj [:paths resname])))
 
(defn get-input-paths
  "Given a path to a Marathon Project, acquires paths to input tables.
   Depending on the processing we're doing, we may not need every table, so 
   we only locate the paths, saving on memory in the process."
  [project-path & {:keys [paths] :or {files default-paths}}]
  :not-implemented)

(defn add-table
  "Adds a table to the project."
  [prj table-name table]
  (assert tbl/tabular? table)
  (assoc-in [:tables table-name] table))

(defn add-tables
  "Add a sequence of named tables to the project. 
   Typically, we use a map of {tablename table} here, 
   but any sequence of [tablename table] will work."
  [prj tables] 
  (reduce (fn [p [nm table]]
            (add-table p nm table))
          (seq tables)))

(defn compute-highwater 
  "Computes the highwater statistics from marathon output."
  [prj SRCdefinitions demandtrends-path]
  [(proc-log "computing highwater statistics...")
   (proc (fn [env] 
           (reduce (fn [env p] (do (highwater/batch p)
                                 (assoc-in env [:highwater-outputs p] p))) 
                   prj (get-path prj :highwater-paths))))])

(defn compute-fillstats
  "Computes the fill statistics from Marathon output."
  [prj SRCdefinitions highwater-path]
  [(proc-log "Computing fill statistics...")
   (proc (fn [env] 
           (do (highwater/batch highwater-path)
             (assoc-in env [:fillstats-path prj] prj))))]) 

;a sample of compiling an audit trail from a marathon run.
(comment

;testing...copied from marathon.processing.excel 
(def wbpath   
  "C:\\Users\\thomas.spoon\\Documents\\Marathon_NIPR\\OngoingDevelopment\\MPI_3.76029832.xlsm")
(def outpath "C:\\Users\\thomas.spoon\\Documents\\newWB.xlsx")

(def wb (xl/as-workbook wbpath))

;A map of paths to resources, relative to a project-path.
(def default-paths {:deployments ["Deployments.txt"]
                    :demand-trends ["DemandTrends.txt"] 
                    :in-scope ["InScope.txt"]
                    :out-of-scope ["OutOfScope.txt"]
                    :demand-records ["DemandRecords.txt"]
                    :parameters ["Parameters.txt"]
                    :period-records ["PeriodRecords.txt"]
                    :relation-records ["RelationRecords.txt"]
                    :src-tag-records ["SRCTagRecords.txt"]
                    :supply-records ["SupplyRecords.txt"] 
                    :titles ["Titles.txt"]})

;Trying to solve the immediate problem of wrapping a marathon project that's 
;based in an Excel workbook (and also based in surrounding text files.)

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
   :locations      "locations.txt"
   })       

;Current process flow -> to build an audit trail from a run:
;The run is performed (currently requires user intervention).
;Extract auditable tables from Marathon Workbook: 
;  {:deployments      "Deployments"     ;output
;   :in-scope         "InScope"         ;output 
;   :out-of-scope     "OutOfScope"      ;output                     
;   :supply-records   "SupplyRecords"   ;input
;   :demand-records   "DemandRecords"   ;input
;   :period-records   "PeriodRecords"   ;input
;   :relation-records "RelationRecords" ;input
;   :src-tag-records  "SRCTagRecords"   ;input
;   :parameters       "Parameters"})    ;input

(defn marathon-book->marathon-tables [wbpath]
  "Extract a map of canonical tables to a map with the same name."
  (let [wb (xl/as-workbook wbpath)]
    (into {} (for [[nm sheetname] (seq marathon-workbook-schema)]
               [nm (xl/sheet->table (xl/as-sheet sheetname wb))])))) 
  
;derive SRCDefinition {SRC, OITitle, STR}
;  derive  OITitles from supply records or other table.
;  derive STR from strength table, or supply records? 
;

(defn derive-titles
  "Derive the OITitles from a set of tables.  If a :titles table already exists, 
   we will use it."
  [tables]
  (let [src-lookup (tbl/make-lookup-table "SRC")                      
        valid-table? (fn [t] (and (contains? tables t)
                                  (tbl/has-fields? ["SRC" "OITitle"] 
                                                  (get tables t))))]
    (cond (valid-table? :titles) ;if we already have titles, done.
            (tbl/select-distinct (:titles tables))
          (valid-table? :demand-records)  ;look in demand records 
            (make-lookup (:demand-records tables))
          (valid-table? :supply-records) ;finally supply records.
            (make-lookup (:supply-records tables))
         :else (throw (Exception. "No valid table to derive titles from!")))))

(defn derive-strengths
  "Derive strengths from a set of tables.  STR should reside in SupplyRecords.
   If a :strength table already exists, we will use it."
  [tables]
  (let [make-lookup (fn [tb] (tbl/select :from tb 
                                         :fields ["SRC" "STR"]
                                         :unique true))        
        valid-table? (fn [t] (and (contains? tables t)
                                  (tbl/has-fields? ["SRC" "STR"] 
                                                  (get tables t))))]
    (cond (valid-table? :strength)
             (tbl/select-distinct (:titles tables))
          (valid-table? :supply-records) ;finally supply records.
             (make-lookup (:supply-records tables))
          :else (throw (Exception. "No valid table to derive titles from!")))))

(defn derive-SRCdef
  "derive  OITitles from supply records or other table.  derive STR from 
   strength table, or supply records.  Join OITitles on STR by SRC." 
  [title-table strength-table]
  (tbl/select :fields ["SRC" "OITitle" "Strength"]
              :from (tbl/join-tables ["SRC"] 
                           [title-table strength-table])))

(defn clean-deployments
  "Given a table of SRC definitions, with field {SRC OITitle ...},
   Adds Dwell Years and OITitle information to the deployments table."
  [src-def-lookup deploy-table] 
  (let [dwell-years (map #(/ % 365.0) 
                         (tbl/field-vals (tbl/select-field "DwellYears")))
        titles (map (fn [src] (-> (get src-def-lookup src)
                                (get "OITitle"))) 
                    (tbl/field-vals (tbl/select-field "SRC")))]
    (tbl/conj-fields {"DwellYears" dwell-years
                      "OITitle" titles} deploy-table)))                                           

(defn marathon-tables->clean-tables 
  "Given a map of marathon tables, in the form of ITabular data types, 
   performs cleaning and post processing to generate a set of tables that 
   are ready for auditing."
  [marathon-tables] 
  (let [titles    (derive-titles marathon-tables)
        strength  (derive-strngths marathon-tables)
        src-definitions (derive-SRCdef titles strength)]
     (merge marathon-tables 
          {:titles titles 
           :strength strength 
           :src-definitions src-definitions
           :deployments (clean-deployments 
                          (tbl/make-lookup-table "SRC" src-definitions)
                          (:deployments marathon-tables))})))

(defn marathon-workbook->project
  "Given a path to a Marathon workbook, derives a basic project structure from
   the workbook.  Specifically, we get the path to the original workbook, as 
   well as an automatic name for the project, all of the tables necessary for 
   auditing, and a set of paths to (potentially large) outputs from the 
   simulation."
  [wbpath]
  (let [wbname (last (io/list-path wbpath))]   
    (-> {:project-type #{:workbook :text :capacity}
          :project-name wbname 
          :paths {:project-path (io/as-directory wbpath)
                  :project-workbook wbname}}
      (add-tables (marathon-book->marathon-tables wbpath)))))

(defn build-audit-trail
  "Given a set of cleaned tables, we apply the processes necessary to build an 
   audit-trail from the data.  The audit trail is just a map of tables."
  [clean-tables & {:keys [post-processes] :or 
                   {post-processes [:compute-highwater
                                    :compute-fillstats]}}]
  (compute-highwater trends   titles highpath)
  (compute-fillstats highpath titles fillpath))  




;  (defn compute-trends [rootdir]
;	  (let [readme {"readme.txt" "Insert comments here."}        
;	        folderspec {"Output" readme 
;	                    "Input"  readme}]
;	  (with-dir rootdir
;	    (reading-files [trends (relative-path rootdir ["DemandTrends.txt"]) 
;	                    titles (relative-path rootdir ["TitleDef.txt"])]
;        (with-path (relative-path *dir* ["Output"]) [highpath ["highwater.txt"]
;                                                     fillpath ["fillstats.txt"]]
;          (do         
;	           (compute-highwater trends titles highpath)
;	           (compute-fillstats highpath titles fillpath)))))))
;
; (let [martables (marathon-tables (wb->tables runbook))
;       inputs    (get-inputs martables)
;       outputs   (get-outputs martables)
;       computed  {:highwater (compute-highwater outputs)
;                  :fillrates (compute-fillrates outputs)
;                  :fillstates (compute-fillstats outputs)}]
;   (build-audit-trail inputs (clean-outputs outputs))      
 )




;(defn process-env
;  "Applies post processing to an environment.  A process is a function, where 
;   process :: Env -> Env, where Env is a map of keyvals, typically containing 
;   the path and any other structure accumulated during processing (which may 
;   be useful downstream).  If no args are specified, the default process is 
;   to compute highwater results, fillrates, and build an audit trail."
;  [& {:keys [processes env] :or {processes default-proceses
;                                 env       {:path (io/*current-directory*)}}}]
;  (reduce (fn [e p] (p e)) env processes))
;
;(defn process-path 
;  "Applies post processing to an environment in which the path is bound to 
;   rootpath."
;  [rootpath & {:keys [processes] :or {processes default-process}}]
;  (process-env :processes processes :env {:path rootpath}))
