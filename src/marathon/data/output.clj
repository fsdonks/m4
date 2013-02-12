(ns marathon.port.data.output)


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
                                   




