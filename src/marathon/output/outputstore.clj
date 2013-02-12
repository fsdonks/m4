(ns marathon.output.outputstore
  (use [util.record :only [defrecord+ with-record]]))

;may be vestigial.
(def dstreams {"LocationWatch", "xl"
               "GhostWatch", "xl"
               "Ghosts", "xl"
               "DeployWatch", "xl"
               "CycleLog", "csv"
               "SandTrends", "xl"
               "Deployments", "xl"
               "DemandTrends", "csv"
               "EmpiricalSummary", "xl"
               "NominalSummary", "xl"})

;Container to store all the data associated with output, particularly in the 
;form of effectful (i.e. non-pure) logging and statistics.  Typically, the 
;outputstore will maintain a set of files, as well as output-specific observers
;that record logs and statistics as the simulation progresses.  This may change
;in the near future.
(defrecord+ outputstore [[name "Outputstore"] 
                         [observers {}] 
                         [filestreams {}] 
                         [defaultstreams dstreams]
                         [defaultfiles {"cycles", "cycles.csv"}] 
                         [mypath (System/getProperty "user.dir")]
                         [activestreams {}]])
(def empty-outputstore (make-outputstore)) 
