;project management module
(ns marathon.project
  (:require  [util [io :as io] 
              [table :as tbl] 
              [clipboard :as board]
              [general :as general]]
             [clojure [string :as strlib]]))

;currently unused.
;(defprotocol IResource
;  (load-resource  [r])
;  (read-resource  [r])
;  (write-resource [r])
;  (resource-type  [r]))

;a list of resources we expect to find in any project.
(def default-resources [:titles
                        :supply-records
                        :demand-records
                        :src-tag-records
                        :period-records 
                        :relation-records 
                        :in-scope 
                        :out-of-scope                        
                        :parameters 
                        :deployments 
                        :demand-trends])

(def default-table-order (conj default-resources
                          :high-water))

;A map of paths to resources, relative to a project-path.
(def default-paths {:titles ["Titles.txt"]
                    :supply-records ["SupplyRecords.txt"]
                    :demand-records ["DemandRecords.txt"]                      
                    :src-tag-records ["SRCTagRecords.txt"]
                    :period-records ["PeriodRecords.txt"]
                    :relation-records ["RelationRecords.txt"]
                    :in-scope ["InScope.txt"]
                    :out-of-scope ["OutOfScope.txt"]
                    :parameters ["Parameters.txt"]
                    :deployments ["Deployments.txt"]                    
                    :demand-trends ["DemandTrends.txt"]})



;(defn load-project [path] (io/folders->map (io/as-directory path)))
;(defn save-project [path] (io/map->folders! (io/as-directory path)))

;projects have names, a version, a set of required resources (dependencies), 
;and a resource map, a map of {resource-name resource}, where resource is 
;anything from a simple parameter, to a path, to an inline data structure, 
;or any number of things.  
(defn make-project [name project-path 
                    & {:keys [version dependencies paths tables] 
                       :or {version 0.1
                            dependencies nil
                            paths (assoc default-paths 
                                         :project-path project-path)
                            tables {}}}]
  {:name name 
   :version version 
   :dependencies dependencies 
   :paths paths
   :tables tables})

(defn project-path
  "Fetch the project path, or the root folder where the project is located."
  [prj] (get-in prj [:paths :project-path]))

(defn project-properties
  "Fetches any root-level key that is not a resource.  Typically simple 
   parameters."
  [proj]
  (reduce (fn [p k] (dissoc p k)) proj [:paths :tables]))

(defn add-path 
  "Conjoins a path to resource, resname, in the project under :paths."
  [prj name path]
  (assoc-in prj [:paths name] path))

(defn add-file 
  "Conjoins a simple filename as a relative path under resname."
  [prj name filename]
  (assoc-in prj [:paths name] [filename]))

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
  (assoc-in prj [:tables table-name] table))

(defn add-tables
  "Add a sequence of named tables to the project. 
   Typically, we use a map of {tablename table} here, 
   but any sequence of [tablename table] will work."
  [prj tables] 
  (reduce (fn [p [nm table]]
            (add-table p nm table))
          prj (seq tables)))

(defn get-table
  "Returns a table local to the project."
  [prj t]
  (get-in prj [:tables t]))

(defn project-dispatch [obj & options] 
  (cond (string? obj) (.toLowerCase (io/fext obj))
        :else (class obj)))

(defmulti load-project 
  "Method for loading a Marathon project from one or more formats.
   If obj is a string, it will be parsed as a path, by file
   extension.  Otherwise, project-dispatches on class." 
  project-dispatch)

(defmulti save-project 
  "Method for saving or persisting a Marathon project to a destination.  
   If obj is a string, it will be parsed as a path, dispatching on 
   the file extension." 
  (fn [proj destination & options]
    (project-dispatch destination)))

(defmethod save-project "clj" [proj destination & {:keys [expanded?] 
                                                   :or {expanded? false}}]
  (let [p (add-path proj :project-path destination)]
    (if expanded? 
      (do (io/map->folders!  p destination)
        (io/hock destination (with-out-str 
                               (doseq [l (map tbl/field->string (keys proj))]
                                 (println l)))))
      (io/hock destination (prn p)))))

(defn keyvals->table [kvps & {:keys [string-fields?]}]
  (let [f (if string-fields? 
            (fn [f] (cond (string? f) f
                          (keyword? f) (str (subs (str f) 1))
                          (coll? f) (str f)
                          :else f))
            identity)]
    (->> tbl/empty-table
      (tbl/conj-fields  
        {"key"  (vec (map f (keys kvps)))
         "val" (vec (map f (vals kvps)))}))))
            
(defn project->tables
  "Method for converting a project representation into a map of ITabular
   objects, either for insertion into a database, for rendering to a folder, 
   or for injecting into a workbook.  Anything that benefits from a tabular 
   representation."
  [proj & {:keys [string-fields? ordering] 
           :or {string-fields? true
                ordering default-table-order}}]
  (->> (merge {:paths (keyvals->table (:paths proj) :string-fields? 
                                               string-fields? )
               :properties (keyvals->table (project-properties proj)
                                        :string-fields? 
                                        string-fields? )}
              (:tables proj))
    (general/align-fields-by ordering)))

;(defn search-project
;  "Search a project's resources for any items in which expr 
;   returns true."
;  [prj expr & {:keys [exhaustive? ]
;  (
;  

(defn test-project
  "Perform a sequence of tests on the project to verify its integrity."
  [prj testcoll]
  (reduce (fn [p tst] (tst p)) testcoll)) 

(derive ::csv        ::table)
(derive ::json       ::map)
(derive ::json-table ::table) 
(derive ::worksheet  ::table)
(derive ::workbook   ::db) 

(defn clear-project!
  "Clear the resources from a project."
  [projectroot]
  (io/clear-folders! projectroot))

