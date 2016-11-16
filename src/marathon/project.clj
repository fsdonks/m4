;;Project management functions.
;;Provides protocols and schemas for manipulating Marathon projects 
;;in a variety of formats.  Current implements include Excel workbooks, 
;;Clojure serialized data, and tab delimited files.  JSON and XML are 
;;trivial to support. More to come.
(ns marathon.project
  (:require  [spork.util [io :as io]            
                         [table :as tbl] 
                         [clipboard :as board]              
                         [general :as general]]
             [clojure [string :as strlib]]))

;;I'm not sure how much of this we really
;;need.

;;The basic requirements for a marathon project
;;are to load a set of canonical tables from
;;somewhere, parse them using the canonical
;;schemas, and if they're valid, recognize
;;them as tables.

;;Since we just maintain a map of tables,
;;which is our project, or db,
;;we should be able to easily modify the
;;project by modifying the map.
;;For instance, patching the project
;;by mergeing new tables on top,
;;or performing slight alterations...

;;These are all preprocessing steps...
;;I vote for the simplest abstraction
;;possible.  Keep using a simple
;;map-based setup as the project
;;definition.  The map stores (typically)
;;tables.

;;To read a project, we simply read the
;;appropriate tables.  Easy.

;;Project serialization should be
;;as easy...we can just dump the serialized
;;project (i.e. the tables and friends)
;;using cryo, or any tsv, etc.

;;a list of resources we expect to find in any project.
;;We'll search for these and see if they 
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
(def default-paths {:titles           ["Titles.txt"]
                    :supply-records   ["SupplyRecords.txt"]
                    :demand-records   ["DemandRecords.txt"]                      
                    :src-tag-records  ["SRCTagRecords.txt"]
                    :period-records   ["PeriodRecords.txt"]
                    :relation-records ["RelationRecords.txt"]
                    :in-scope         ["InScope.txt"]
                    :out-of-scope     ["OutOfScope.txt"]
                    :parameters       ["Parameters.txt"]
                    :deployments      ["Deployments.txt"]                    
                    :demand-trends    ["DemandTrends.txt"]})

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
  {:name         name 
   :version      version 
   :dependencies dependencies 
   :paths        paths
   :tables       tables})

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
        (and  (map? obj) (:tables obj)) :map-project ;already mapproject, effectively identity
        :else (class obj)))

(defmulti load-project 
  "Method for loading a Marathon project from one or more formats.
   If obj is a string, it will be parsed as a path, by file
   extension.  Otherwise, project-dispatches on class." 
  project-dispatch)

(defmethod load-project :map-project [obj & options]
  obj)

(defmulti save-project 
  "Method for saving or persisting a Marathon project to a destination.  
   If obj is a string, it will be parsed as a path, dispatching on 
   the file extension." 
  (fn [proj destination & options]
    (project-dispatch destination)))

(defmulti derive-project 
  "Method for building a new project from an existing project.  At the most 
   basic level, it's a simple cloning process.  Primary impetus is to
   facilitate cloning legacy projects and making data migration easier."
  (fn [proj destination & options]
    [(project-dispatch proj) (project-dispatch destination)]))

(defmulti migrate-project 
  "Method for migrating data in an existing project to another, possibly new 
   project.  At the most basic level, it's a simple cloning process.  Primary 
   impetus is to facilitate cloning legacy projects and making data migration 
   easier."
  (fn [proj destination & options]
    [(project-dispatch proj) (project-dispatch destination)]))

;;might be obe.  probably better to serialize this.
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
         "val"  (vec (map f (vals kvps)))}))))
            
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

(defn test-project
  "Perform a sequence of tests on the project to verify its integrity."
  [prj testcoll]
  (reduce (fn [p tst] (tst p)) testcoll))

;;This could be re-located to marathon.project, but that's still
;;a bit in flux.  I'd like to anneal more before we do.  For now,
;;we'll just do something a bit more generic.
;;We're missing SRCsInScope and SRCsOutOfScope here...
;;We compute scope after creating the context...
;;So at a minimum, we'd maybe need to build up the fill network.
(defn audit-project
  "Creates an audit trail of all the input files for a capacity run, primarily 
   to aid legacy post processing.  Files are stored in the same root path, 
   prefixed by AUDIT_"
  [prj outroot & {:keys [tables] :or
                  {tables [:SupplyRecords
                           :DemandRecords
                           :PeriodRecords
                           :RelationRecords
                           :SRCTagRecords
                           :Parameters]}}]
  (do (println [:auditing-to outroot])
      (let [prj (load-project prj)]
        (doseq [k tables]
          (let [t    (get (:tables prj) k)
                tgt  (str outroot "AUDIT_" (name k) ".txt")
                _    (println [:spitting k :to tgt])]
            (tbl/spit-table tgt t))))))

(derive ::csv        ::table)
(derive ::json       ::map)
(derive ::json-table ::table) 
(derive ::worksheet  ::table)
(derive ::workbook   ::db) 

(defn clear-project!
  "Clear the resources from a project."
  [projectroot]
  (io/clear-folders! projectroot))

