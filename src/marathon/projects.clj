;;TARGET FOR DELETION.  MAY MIGRATE STUFF OUT.

;project management module
(ns DEVS.projects
    (:use [clojure.contrib.repl-utils :exclude [source apropos javadoc]]
          [clojure.contrib.java-utils]))

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


(defn default-project 
  (str (System/getProperty "user.home") "\\marathonprojects"))
  
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

;need a multimethod for processing resources...
;if resource is a XREF, it's a path to a (file/URL) that should be processed.

(defmulti read-table (fn [r] (table-type r)))
(defmethod read-table :default [r] []) 

(defn resource-type [r] 
  (case (type r)
    :

;need processors for Table, JSON, Excel worksheets
(defmulti get-resource (fn [r] (type r)))
(defmethod get-resource :csv [r] (read-table r))
(defmethod get-resource :json [r] (if (jsontable? r) 
                                      (read-table r) 
                                      (read-json r)))
(defmethod get-resource :worksheet [r] (read-table r))


;Any project is a bundle of default project properties....
;Projectname, Version, Resources
(defrecord project [projectname version resources]) 

;Resources are expressions that can be read to produce data.
;We want to be able to refer to a base resource template. 
;  In the case where multiple simulation runs are possible, i.e. experimental 
;  design, search, game-trees, stochastic reps, whatever, we'd like to maintain
;  a referentially transparent base set of data, and spin off of that.

;To facilitate spiral devlopment, we need to support the serialization of any 
;project.  There are 2 forms of serialization: deep-copying, and 
;shallow-copying.  Deep-copying implies that the entire project structure is 
;dumped to a single file/resource path.  This is similar to "mixing down" a 
;signal in audio processing.  The entire project tree is evaluated recursively
;and the project is "rendered."

;On the other hand, shallow-copying implies copying only the references to 
;resources.  This is useful for building templates or dependencies, so that 
;entire batch runs can be tweaked and modified with a few upfront input changes.
;Case in point: we have a project that needs to execute a full factorial 
;design of experiment, in which case there is a single design factor, policy.
;The design factor is drawn from a set of 2 policies #{A,B}, while everything
;else remains static.

;Thus we have a base project description:
;(def baseline 
;  {:projectname "Policy Experimentation" 
;   :version 1.0 
;   :Resources
;     {:Base {"Parameters" ".\Params.json"
;             "DefaultPolicyRecords" :default
;             "All Supply" []
;             "All Demand" ".\Demands.json"
;             "All Relations" []
;             "Relations" []
;             "Periods" ".\Periods.json"
;             "Requirement List" ".\RequirementList.json"}}})

;It'd be nice to say...
;(def experiments [(change-parameter DefaultACPolicy AC12)
;                  (change-parameter DefaultACPolicy AC15)])
;Or
(defn range-parameter [pname values]
  (map (partial change-parameter pname) values))

(def experiments (range-parameter DefaultACPolicy #{AC12 AC15}))

;given a set of parameter changes....we can apply them in either a deep way 
;or a shallow way. 

;Let's assume deep for now....It only matters if we serialize.  

(def experimental-inputs (map #(% baseline) experiments)) 
;this produces a set of experimental input data, in memory.

(def outputloc ".\outputs")

(defn do-runs
  "Apply simf to each run, saving the output in loc.  write-run should create 
   a unique directory for each run."
  [inputcoll simf baseloc writef]
   (for [input inputcoll
         x     (iterate inc 0)]
     (writef (simf (input)) (make-path baseloc "x"))))

(def experimental-runs (do-runs experimental-inputs simulation outputloc)) 
(defn dir->project [path]
    (for [fl (file-seq path)]
      (get-resource f)))

(defn clj->project [path]
  (read-string (slurp path)))

  ;I'd like to have a quick language 
  