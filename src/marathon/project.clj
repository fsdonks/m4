;project management module
(ns marathon.project
  (:use [util.io])
  (:require [clojure [string :as strlib]]
            [clojure.java [io :as io]]))

(defprotocol IResource
  (load-resource  [r])
  (read-resource  [r])
  (write-resource [r])
  (resource-type  [r]))

;canonical resources....
(def resource-core 
  (zipmap
	  ["Parameters" 
	   "DefaultPolicyRecords" 
	   "AllSupply"  
	   "AllDemand"  
	   "AllRelations"  
	   "Relations" 
	   "Periods" 
	   "Demand Records" 
	   "Requirement List"]
   (repeat ::table)))

;things we must have - derived from aforementioned core resources.
(def basic-dependencies (into #{} (keys resource-core)))

;projects have names, a version, a set of required resources (dependencies), 
;and a resource map, a map of {resource-name resource}, where resource is 
;anything from a simple parameter, to a path, to an inline data structure, 
;or any number of things.  
(defrecord project [name version dependencies resources])
(defn make-project [name & {:keys [version dependencies resources] 
                             :or {version 0.1
                                  dependencies basic-dependencies
                                  resources {}}}]
  (->project name version dependencies resources))

(defn load-project
  "Load a project.  Ensure that resources cover dependencies 
   (= #{} (diff dependencies resourcekeys))
   Load resources, and return a valid project structure 
   that contains all required structures.  This is a primitive, just-above-raw
   processing state, where we've basically just collected the data and collated
   it.  Static processing comes later."
  [{:keys [dependencies resources] :as prj}]
  (let [unresolved (setlib/difference dependencies (into #{} (keys resources)))]
    (if (seq unresolved) 
      (throw (Exception. (str "Unresolved dependencies! " unresolved)))
      (let [loaded (reduce (fn [m reskey res]
                             (assoc m reskey (load-resource res))) resources)]
        (assoc project :resources loaded)))))        

(defn test-project
  "Perform a sequence of tests on the project to verify its integrity."
  [prj testcoll]
  (reduce (fn [p tst] (tst p)) testcoll)) 

(derive ::csv        ::table)
(derive ::json       ::map)
(derive ::json-table ::table) 
(derive ::worksheet  ::table)
(derive ::workbook   ::db) 

;:clj ;eval'd, could be [:csv :json :json-table :worksheet....etc.]
              

(declare loader)
;a simple container for resources.....name is a unique identifier, path is an 
;optional path to the resource, type is intended as a contextual identifier for
;the pathed resource.  data is either nil, or an eval'd form of some reader 
;that converts path and type into actionable data (something we can consume).
(defrecord resource [name path type data]
  IResource 
  (load-resource [r] (if (not (nil? data)) 
                       r
                       (resource. name path type (loader type path))))
  (read-resource [r] data)
  (write-resource [r] nil)
  (resource-type [r] type))

;concrete stuff.
(defrecord table [fields records])
(derive table ::table)

(defn split-by [delim s]
  (strlib/split  s (re-pattern delim)))

(defn csv-line [s] (map strlib/trim (split-by "," s)))
(defn tab-line [s] (map strlib/trim (split-by (str \tab) s)))

(defn- parse-string [value]
  (if (re-matches #"\d+" value)
    (try (Integer/parseInt value)
         (catch NumberFormatException _ value))
    (try (Double/parseDouble value)
         (catch NumberFormatException _ value))))

(defn parse-records [record->vector records]
	(persistent! 
	  (reduce 
     (fn [acc raw-record]
        (conj! acc 
           (vector-map parse-string (record->vector raw-record)))) 
       (transient []) records)))

(defn delimited->table
  "Rip a delimited table into "
  [linef path]
  (with-open [source (io/reader path)] 
	  (let [lines (line-seq source)
	        fields (zipmap (linef (first lines)) (iterate inc 0))
	        unparsed   (rest lines)]
	    (->table fields (parse-records linef unparsed)))))    

(def csvfile->table #(delimited->table csv-line %))
(def tabfile->table #(delimited->table tab-line %))

(defn jsonfile->table [path]
  (with-open [rdr (io/reader path)]
    (let [obj (json/read-json-from rdr :true :true nil)]
      (when (map? obj)
        (->table (get obj :fields (get obj :column-names))  
                 (parse-records identity 
                    (get obj :records (get obj :rows))))))))

(defn cljfile->table [path] 
  (with-open [rdr (io/reader path)]
    (let [obj (read-string rdr)]
      (when (map? obj)
        (if (= (get obj :type) :table) 
          (->table (get obj :fields) (get obj :records)))))))



(defn project-properties
  "Fetches any root-level key that is not a resource.  Typically simple 
   parameters."
  [proj]
  (dissoc proj :resources))

(defn clear-project!
  "Clear the resources from a project."
  [projectroot]
  (clear-folders! projectroot))

(defn project->file! [proj rootpath]
  (let [f (io/file rootpath)]
    (if (.isDirectory f)
      (hock (relative-path rootpath ["project.clj"]) proj)
      (hock rootpath proj))))

(defn project->folders! 
  [proj rootpath & [readable?]]
  (map->folders! proj rootpath :pretty? readable?))

(defn folders->project [rootpath]
  (let [{:keys [name version dependencies resources]} (folders->map rootpath)]
    (->project name version dependencies resources)))



(comment
  '(the purpose of project is to allow a loose definition of bundled Marathon 
    files.  We want to define a standard template for
      a: Required files
      b: Optional or user-added files
      c: other stuff? 
    project will provide an API for collecting a set of files, or building a 
    new set of files in a pre-determined directory structure. 
    The API will also provide facilities for compressing and decompressing the 
    archive into a single .mpj file.
    I'll probably store this in native clojure, since we're just storing KVPs)
  )

;one of the things I'm trying to reconcile is having a flexible representation 
;of marathon projects.

;one way to handle this is to define a project graph (not unlike a scene graph)
;where the structure of the graph encodes some set of instructions for a project
;manager to evaluate and (proably) side effect.

;I think this representation would be really useful for doing things like 
;derived simulations....

;For instance, we could define a base project, as we often have, and then 
;derive sub-projects that inherit all of the attributes of the parent 
;project....they just refer (or defer) to the parent nodes....
;That's akin to defining transforms in a scene graph...

;I'd also like to define some sort of parametric or sequential transform...
;for instance, to define searches over a domain, or to run marathon n times
;across a discrete combination of inputs (an experiment), we'd want to separate
;the output...

;Scenario 1: 
;  User wants to analyze a notional supply against a notional demand using 
;  built-in or default policies. 
;  Analysis is a capacity analysis.

;A resource, by definition, is either a table[], a filepath (which is 
;interpreted based on its extension, as a table (CSV, XLS, XLSX), an inline 
;data structure (clojure currently....might be open to JSON), or 
;a .clj file that is loaded and read.  Vars defined in the .clj file should be
;available inside an expression that evals to a resource.

