;post processing routines for marathon.  Includes a simple one-off command 
;line processor.  Since most Marathon stuff is handled via a root directory, 
;the processor (and any others) should be able to apply post processing to a 
;given path. 
(ns marathon.processing.post
  (:require [util [io :as io]]
            [marathon.processing 
             [highwater :as highwater] 
             [fillrates :as fill]
             [frontend :as gui]]))

(declare compute-highwater compute-fillrates build-audit-trail)
(def default-processes 
  [compute-highwater compute-fillrates build-audit-trail])

(def compute-highwater highwater/batch) 
(def compute-fillrates fillrates/batch) 
;(def build-audit-trail audit/batch)

(defn process-env
  "Applies post processing to an environment.  A process is a function, where 
   process :: Env -> Env, where Env is a map of keyvals, typically containing 
   the path and any other structure accumulated during processing (which may 
   be useful downstream).  If no args are specified, the default process is 
   to compute highwater results, fillrates, and build an audit trail."
  [& {:keys [processes env] :or {processes default-proceses
                                 env       {:path (io/*current-directory*)}}}]
  (reduce (fn [e p] (p e)) env processes))

(defn process-path 
  "Applies post processing to an environment in which the path is bound to 
   rootpath."
  [rootpath & {:keys [processes] :or {processes default-process}}]
  (process-env :processes processes :env {:path rootpath}))


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
