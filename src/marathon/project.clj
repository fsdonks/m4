;project management module
(ns marathon.project
  (:require  [util [io :as io] [table :as tbl] 
             [clipboard :as board]]
             [clojure [string :as strlib]]))

(defprotocol IResource
  (load-resource  [r])
  (read-resource  [r])
  (write-resource [r])
  (resource-type  [r]))

;a list of resources we expect to find in any project.
(def default-resources [:deployments 
                        :demand-trends  
                        :in-scope 
                        :out-of-scope 
                        :demand-records 
                        :parameters 
                        :period-records 
                        :relation-records 
                        :src-tag-records 
                        :supply-records  
                        :titles])

;A map of paths to resources, relative to a project-path.
(def default-paths [["Deployments.txt"]
                    ["DemandTrends.txt"] 
                    ["InScope.txt"]
                    ["OutOfScope.txt"]
                    ["DemandRecords.txt"]
                    ["Parameters.txt"]
                    ["PeriodRecords.txt"]
                    ["RelationRecords.txt"]
                    ["SRCTagRecords.txt"]
                    ["SupplyRecords.txt"] 
                    ["Titles.txt"]])

;canonical resources....
(def resource-core 
  {:paths  (zipmap default-resources default-paths)
   :tables {}})

(defn load-project [path] (io/folders->map (io/as-directory path)))
(defn save-project [path] (io/map->folders! (io/as-directory path)))

;a sample environment we'll use for structuring our project...
(def sample-env 
  {:paths {:project-path "the root path"
           :deployments ["A relative path to deployments"]}
   :tables {:the-table (tbl/make-table {:field1 ["tom"]})}
   :some-resource "blah"
   :resource-2 "blee"})


;projects have names, a version, a set of required resources (dependencies), 
;and a resource map, a map of {resource-name resource}, where resource is 
;anything from a simple parameter, to a path, to an inline data structure, 
;or any number of things.  
(defn make-project [name project-path 
                    & {:keys [version dependencies paths tables] 
                       :or {version 0.1
                            dependencies nil
                            paths (-> (zipmap default-resources 
                                              default-paths)
                                    (assoc :project-path 
                                            project-path))
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

(defn get-path
  "Fetch an absolute path to a resource, as defined by the relative path 
   associated with resname."
  [prj resname]
  (io/relative-path 
    (project-path prj) (get-in prj [:paths resname])))
   
(defn conj-path 
  "Conjoins a path to resource, resname, in the project under :paths."
  [prj name path]
  (assoc-in prj [:paths name] path))

(defn conj-file 
  "Conjoins a simple filename as a relative path under resname."
  [prj name filename]
  (assoc-in prj [:paths name] [filename]))

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

