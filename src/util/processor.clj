;This is a simple set of utilities for defining post processors for data files.
;The need originated in Marathon scripting...and resulted in a more general 
;library.  Enter the generic processor.

;Processors will typically operate on files, and utilize a set of common data to 
;provide a consistent set of output files with canonical fields.

;Processors must define dependencies, as well as resources they produce...not 
;unlike a build tool.

(ns util.processor
  (:require [clojure [set :as set]]
            [util [io :as io]]))

;A processor is a named task that requires preconditions to hold, and provides 
;a set of post conditions, or effects.  A typical precondition would be the 
;existence of a resource..say a file or an in-memory map or something.

(defprotocol IProcessor 
  (can-process? [p state]
     "Determine if the processor can be run in the given state.")
  (process [p state] "Apply the processor to the process state")
  (process-name [p] "Get the name of the process"))

;The processfunc is a function (ProcessState->ProcessState), where ProcessState
;is a simple map of resource names to resources.  The idea is that we define
;individual sub processes, along with their preconditions (resources required
;prior to running), and their postconditions (resources provided as a result of
;running), then apply the processfunc.
(defrecord processor [name pre post processfunc]
  IProcessor
  (can-process? [p state]
                (or (empty? pre)
                    (every? identity
                            (for [[condition f] pre]
                              (f condition state)))))
  (process [p state] 
           (if (can-process? p state)
             (processfunc state)
             (throw (Exception. (str "Cannot execute process " name)))))
  (process-name [p] name))

(def #^:dynamic *processors* (atom {})) ;a db of processors
(defn add-processor [p]
  (swap! *processors* (assoc (process-name p) p)))
;(defn get-processors [] @*processors*)
;
;(defn add-value
;  "Defines a process that has no preconditions, and adds a resource under name
;   to the process state."
;  [name x]
;  (->processor name nil #{name} (fn [state] (assoc state name x))))
;
;(defn read-file
;  "Defines a process that checks file existence as a precondition, and adds 
;   a resource under name to the process state."
;  [path & {:keys [name] :or {name path}}]
;  (->processor name nil {:file-exists? path}
;               (fn [state] (assoc state name x))))
;(defn read-lines 
;  "Defines a process that checks file existence as a precondition, and adds a 
;   resource under name to the process state."
;  [name x]
;  (->processor name nil {:key-exists? name}))
;
;(defn in-directory [path proc]
;  (->processor :in-directory {:directory-exists? path} nil 
;     (fn [state] (process proc (assoc state :dir path)))))  

(defn build-folders!
  "Builds the structure for a set of folders defined by folderspec, in root 
   directory defined by the path rootdir.  A folderspec is simply a map where 
   nested maps represent subdirectories ala 
   {:output {} :input {}}, which expands to rootdir/output, rootdir/input"
  [rootdir folderspec]
  (io/map->folders! folderspec (io/as-directory rootdir) :condensed? false))  

(def readme {"readme.txt" "Insert comments here."})
 
;a sample of compiling an audit trail from a marathon run.
(comment 
	(defprocess compute-trends [rootdir]
	  (let [readme {"readme.txt" "Insert comments here."}        
	        folderspec {"Output" readme 
	                    "Input"  readme}]
	  (with-dir rootdir
	    (reading-files [trends (relative-path rootdir ["DemandTrends.txt"]) 
	                    titles (relative-path rootdir ["TitleDef.txt"])]
	       (with-path (relative-path *dir* ["Output"]) [highpath ["highwater.txt"]
	                                                    fillpath ["fillstats.txt"]]
	         (do         
	           (compute-highwater trends titles highpath)
	           (compute-fillstats highpath titles fillpath)))))))
)

  
  
  
    

    

  


     
   
