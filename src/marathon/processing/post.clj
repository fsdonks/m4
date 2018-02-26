;post processing routines for marathon.  Includes a simple one-off command 
;line processor.  Since most Marathon stuff is handled via a root directory, 
;the processor (and any others) should be able to apply post processing to a 
;given path. 
(ns marathon.processing.post
  (:use     [marathon.project])
  (:require [spork.util [io :as io] [table :as tbl]]
            [marathon.project     [excel :as xlproj]]
            [marathon.processing  ;[excel     :as xl]
                                  [highwater :as highwater] 
                                  [fillrates :as fill]]))

(declare compute-highwater compute-fillrates build-audit-trail)

(defn process [pre proc post]
  (fn [env] 
    (->> env  pre  proc  post)))

(defn get-environment
  "Returns a default starting environment.  If an environment is provided, 
   passes the environment through."
  [& [env]] 
  (if-let [e env]
    e
    (merge io/common-paths {:current-dir io/*current-dir*})))


;Trying to solve the immediate problem of wrapping a marathon project that's 
;based in an Excel workbook (and also based in surrounding text files.)

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

  
;derive SRCDefinition {SRC, OITitle, STR}
;  derive  OITitles from supply records or other table.
;  derive STR from strength table, or supply records? 
;

(defn derive-titles
  "Derive the OITitles from a set of tables.  If a :titles table already exists, 
   we will use it."
  [tables]
  (let [valid-table? (fn [t] (and (contains? tables t)
                                  (tbl/has-fields? ["SRC" "OITitle"] 
                                                  (get tables t))))]
    (cond (valid-table? :titles) ;if we already have titles, done.
            (tbl/select-distinct (:titles tables))
          (valid-table? :demand-records)  ;look in demand records 
            (tbl/select :fields ["SRC" "OITitle"] 
                        :from (:demand-records tables))
          (valid-table? :supply-records) ;finally supply records.
            (tbl/select :fields ["SRC" "OITitle"] 
                        :from  (:supply-records tables))
         :else (throw (Exception. "No valid table to derive titles from!")))))

(defn derive-strengths
  "Derive strengths from a set of tables.  STR should reside in SupplyRecords.
   If a :strength table already exists, we will use it."
  [tables]
  (let [valid-table? (fn [t] (and (contains? tables t)
                                  (tbl/has-fields? ["SRC" "STR"] 
                                                  (get tables t))))]
    (cond (valid-table? :strength)
             (tbl/select-distinct (:titles tables))
          (valid-table? :supply-records) ;finally supply records.
             (tbl/select :fields ["SRC" "STR"] :from  (:supply-records tables))
          :else nil)))

(defn derive-SRCdef
  "derive  OITitles from supply records or other table.  derive STR from 
   strength table, or supply records.  Join OITitles on STR by SRC." 
  [title-table strength-table]
  (if strength-table 
    (tbl/select :fields ["SRC" "OITitle" "Strength"]
                :from (tbl/join-tables ["SRC"] 
                                       [title-table strength-table]))
    (tbl/conj-field ["STR" (vec (take (tbl/count-rows title-table) (repeat 0)))] 
                    title-table)))

(defn clean-deployments
  "Given a table of SRC definitions, with field {SRC OITitle ...},
   Adds Dwell Years and OITitle information to the deployments table."
  [src-definitions deploy-table] 
  (let [dwell-years (map #(/ % 365.0) 
                         (tbl/field-vals (tbl/get-field "DwellBeforeDeploy" 
                                                        deploy-table)))
        src-lookup (tbl/make-lookup-table "SRC" src-definitions)
        titles (->> (tbl/map-field "DemandType"
                       (fn [src] (get-in src-lookup [src "OITitle"]))
                       deploy-table)
                   (tbl/get-field "DemandType")
                   (tbl/field-vals))]
    (tbl/conj-fields {"DwellYears" dwell-years
                      "OITitle" titles} deploy-table)))                                           

(defn marathon-tables->clean-tables 
  "Given a map of marathon tables, in the form of ITabular data types, 
   performs cleaning and post processing to generate a set of tables that 
   are ready for auditing."
  [marathon-tables] 
  (let [titles    (derive-titles marathon-tables)
        strength  (derive-strengths marathon-tables)
        src-definitions (derive-SRCdef titles strength)]
     (merge marathon-tables 
          {:titles titles 
           :strength strength 
           :src-definitions src-definitions
           :deployments (clean-deployments 
                          src-definitions
                          (:deployments marathon-tables))})))

;(defn marathon-workbook->project
;  "Given a path to a Marathon workbook, derives a basic project structure from
;   the workbook.  Specifically, we get the path to the original workbook, as 
;   well as an automatic name for the project, all of the tables necessary for 
;   auditing, and a set of paths to (potentially large) outputs from the 
;   simulation."
;  [wbpath]
;  (let [wbname (last (io/list-path wbpath))]   
;    (-> {:project-type #{:workbook :text :capacity}
;         :project-name wbname 
;         :paths {:project-path (io/as-directory wbpath)
;                 :project-workbook wbname}}
;      (add-tables (marathon-book->marathon-tables wbpath)))))

(defn clean-project
  "Performs default cleaning on legacy tables coming from marathon.  This may 
   change in the future.  Currently, we're just adding extra columns of 
   information, namely OITitles and Strength, and computing fields for the 
   deployment table."
  [prj]
  (let [ts (:tables prj)]
    (assoc prj :tables (marathon-tables->clean-tables ts))))

;;note: excised legacy functionality.  we're not doing this currently.
(defn add-highwater 
  "Computes the highwater tables.  Requires a path to demand-trends in the 
   project.  Adds highwater under tables.  The final highwater table 
   should not take too much memory, since it is a reduction of the original
   demand-trends table."
  [prj & {:keys [headers] 
          #_:or #_{headers  (vec (map tbl/field->string 
                                  highwater/highwater-headers))}}]
  #_(let [hw-table (tbl/stringify-field-names 
                   (highwater/file->highwater-table 
                     (get-path prj :demand-trends)))]       
    ;join fields from the SRC definition table....
    (->> (get-table prj  :src-definitions)
         (tbl/join-on    ["SRC"] hw-table)
         (tbl/records->table)
         (tbl/order-by [["t" :ascending]                         
                        ["DemandName" :ascending]])
         (tbl/order-fields-by headers)
         (add-table prj :high-water)))
  prj)

(defn audit-marathon-project
  "Automatically pushes a capacity run through the cleaning/auditing process.
   Further processing can be provided via a keyword argument."
  [path & {:keys [destination processing] 
           :or {destination (when (string? path)
                              (io/relative-path 
                                (io/as-directory 
                                  (io/fdir path))
                                ["AuditTrail.xlsx"]))
                processing nil}}]
  (let [final-func (if destination (fn [prj] (save-project prj destination))
                    identity)]
    (->> (load-project path)
         (clean-project)
         (add-highwater)
         (final-func))))      

;;it'd be nice to have a set of functions to copy a project...
;;One current problem with the legacy stuff is that we have a ton of things to 
;;copy between workbooks.
;;So if we want to migrate projects to and from, it can get a bit hairy...
;(defn modify-marathon-project [path tables]
  

;a sample of compiling an audit trail from a marathon run.
(comment

(def wbpath
"C:\\Users\\tom\\Documents\\Marathon_NIPR\\smallsampling\\MPI_3.76029832.xlsm")


(def savepath 
  "C:\\Users\\tom\\Documents\\Marathon_NIPR\\smallsampling\\project.xlsx")


;(def outpath "C:\\Users\\thomas.spoon\\Documents\\newWB.xlsx")

(def myproject (load-project wbpath))

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

;(defn process-path 
;  "Applies post processing to an environment in which the path is bound to 
;   rootpath."
;  [rootpath & {:keys [processes] :or {processes default-process}}]
;  (process-env :processes processes :env {:path rootpath}))
