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

(defprotocol IProcess 
  (do-process [p & env] 
  "Applies post processing to an environment.  A process is a function, where 
   process :: Env -> Env, where Env is a map of keyvals, typically containing 
   the path and any other structure accumulated during processing (which may 
   be useful downstream).  If no args are specified, the default process is 
   to compute highwater results, fillrates, and build an audit trail."))

;This will probably be adapted further, currently not used.  The idea is to 
;define inputs for processes, as well as outputs, to allow for a GPS-type 
;system of goal-programming, in which pre and post-conditions, combined with 
;a map of actions, will allow the processor to figure out how to accomplish 
;a set of goals.
(defprotocol IConditionalProcess
  (pre-conditions [p env] "A set of pre-conditions to execute p.")
  (post-conditions [p env] "A set of post-conditions that p will impart."))

(extend-protocol IProcess
  nil ;a nil process returns the environment, if any.
    (do-process [p & env] (first env))
  clojure.lang.PersistentVector ;vectors are seen as a serial set of processes. 
    (do-process [p & env] 
      (reduce (fn [env proc] (if-let [res (proc env)] res env))           
              (get-environment (first env)) p))
  clojure.lang.PersistentArrayMap ;maps are seen as independent processes.  
    (do-process [p & env]
      (let [ps (seq p)] ;execute processes in parallel
        (->> (pmap (fn [[pname proc]] (fn [env] (proc env))) ps) 
          (reduce (fn [env f] (merge env (f env))) ;returning a merged env
                  (get-environment (first env)))))))

;process combinators.
(defn proc
  "Build a sequential process from one or more items."
  [& itms] (vec itms))

(defn proc-parallel
  "Build a parallel process from one or more items."
  [& itms] (into {} (map-indexed vector itms)))

(defn proc-effect
  "Convert a function of no args, f, into a process by making it into 
   a function that takes an environment argument."
  [thunk] 
  [(fn [env] (do (thunk) env))]) 

(defn proc-map 
  "Map function f to env"
  [f]
  [(fn [env] (f env))])

(defn proc-if
  "Apply process p when pred holds."
  [pred p] 
  [(fn [env] (if (pred env) (p env) env))])  

(defn proc-log
  "Prints a simple message to the console, for logging."
  [msg ]
  (proc-effect (println msg)))

(defn proc-read-file
  [{:keys [resname path]}]
  "Reads a file from path, binding merging the result into the environment."
  (fn [env] (merge env {resname (slurp path)})))    


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

(defn build-audit-trail 
  "Compiles an audit trail from Marathon output."
  [project-path all-input-tables demandtrends-path fillstats-path]
  [(proc-log (str "Building audit trail from project in " project-path))])

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

(defn get-marathon-tables
  "Given a workbook schema, extract each worksheet into a util.table for 
   further processing."
  [wb & {:keys [tables] :or {tables marathon-workbook-schema}}]  
  nil)

(defn get-marathon-inputs [wbpath]
  "Extract a map of canonical tables to a map with the same name."
  (let [wb (xl/as-workbook wbpath)]
    (into {} (for [[nm sheetname] (seq marathon-workbook-schema)]
               [nm (xl/sheet->table (xl/as-sheet sheetname wb))])))) 
  
;derive SRCDefinition {SRC, OITitle, STR}
;  derive  OITitles from supply records or other table.
;  derive STR from strength table, or supply records? 

(defn derive-titles
  "Derive the OITitles from a set of tables."
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
  "Derive strengths from a set of tables.  STR should reside in SupplyRecords"
  [tables]
  (let [make-lookup (fn [tb] (tbl/select :from tb 
                                         :fields ["SRC" "OITitle"]
                                         :unique true))        
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

;with-table [deployments]
;  Conj columns to deployments:  
;    Join ["SRC"] [deployments SRCDefinitions] ;adds OITitle, STR.
;    conj-field "Dwell Years" (fn [record] (assoc record "Dwell Years"
;                                                        (* 365 (? "Dwell"))))
;   

;(defn build-SRC-definition
;  "Build a lookup table of {SRC, OITitle, STR}."
;  [tables]
;  (assert (every? (partial contains? tables) []))  
                            
;(defn add-deployment-fields
;  "Given a deployments table, add a couple of fields for the deployment.
;   Specifically, we want to add a computed field for each deploytable, where 
;   "
;  [deploytable srcdeftable]

  


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
