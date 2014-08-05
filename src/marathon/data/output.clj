;Port of the I/O handler from marathon.  Thinking of a way to handle this...
;we have much better I/O facilities in clojure.  This is just a place holder 
;until the new design emerges.
(ns marathon.data.output)

(defrecord outputstore [name observers filestreams 
                        defaultstreams defaultfiles mypath
                        activestreams])
(def dstreams      
  {"LocationWatch", "xl"
   "GhostWatch", "xl"
    "Ghosts", "xl"
    "DeployWatch", "xl"
    "CycleLog", "csv"
    "SandTrends", "xl"
    "Deployments", "xl"
    "DemandTrends", "csv"
    "EmpiricalSummary", "xl"
    "NominalSummary", "xl"})

(def empty-outputstore 
  (outputstore. 
    "OutputStore" {} {} 
     dstreams {"cycles", "cycles.csv"} (System/getProperty "user.dir"))
     {})
                                   




