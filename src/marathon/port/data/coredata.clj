;This is a dirty port of the decoupled VBA code I busted my ass over 2 weeks 
;to produce.  Unfortunately, VBA has a lovely habit of corrupting itself...
;se here we are, porting to clojure (and parts beyond maybe).
(ns marathon.port.data.coredata)

;structure for tracking demand data, fills, unfilled, queued demands.
(defrecord demandstore [name demandmap infeasible-demands 
                        unfilledq  activations deactivations 
                        eligbledemands changed demandtraffic 
                        tryfill loginfeasibles tags 
                        fillables verbose tlastdeactivation]) 
(def empty-demandstore 
  (demandstore. "DemandStore" {} {} 
                 nil {} {} 
                 {} {} true 
                 false  {"Sinks" {}} {} 
                 false false -1))

;structure for bundling information about fill contexts, specifically rules 
;for mapping supply to demand.
(defrecord fillstore [name fillgraph fillfunction 
                      fills rendergraphs outofscope
                      allgraphs])

(def empty-fillstore (fillstore. "FillStore" nil nil 
                                  {} false {} 
                                  false))


;a policy encodes a finite state machine that determines the order in 
;which unit entities process behavior.
(defrecord policy [name cyclelength mindwell 
                   maxdwell maxbog maxMOB
                   recovery startdeployable stopdeployable
                   positiongraph startstate endstate
                   overlap subscribers]) 

(def empty-policy (policy. "BlankPolicy" :inf 0
                           :inf :inf :inf 
                           90   0    :inf 
                           nil  :spawn :spawn 
                           45   {}))


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


(defrecord composite-policy [name periods subscribers])
(defn make-composite-policy [name period-map]
      (composite-policy. name periods))


;Delay
(defrecord behaviorstore [name behaviors])

(defrecord managerofevents [name userevents evtstream streams])
(def empty-managerofevents
  (managerofevents. "EventManager" {} {} {}))

(def msgbands {:simulation 1 :supply 2 :demand 3 :policy 4 :trending 5})

(def events 
    {:spawnunit "Spawning Unit",
		 :neverDeployed "Never Deployed",
		 :supplyUpdate "Supply Update",
		 :CycleCompleted "Cycle Completed",
		 :AddedPeriod "Added Period",
		 :Terminate "Terminating",
		 :extendedBOG "Extended BOG",
		 :sample "Sampling",
		 :scheduleDemand "Scheduling Demand",
		 :MoveUnit "Moving Unit",
		 :UnitChangedPolicy "Unit Changed Policy",
		 :outofstock "out of Stock",
		 :DemandFillChanged "Demand Fill Changed",
		 :NewFollowOn "New Follow On",
		 :GhostReturned "Ghost Returned",
		 :RequestFill "Requesting Fill",
		 :NewDeployable "New Deployable Stock",
		 :Deployable "Deployable",
		 :AwaitingPolicyChange "Awaiting Policy Change",
		 :LocationSwap "Location Swap",
		 :policychange "Policy Change",
		 :StockRequired "Stock Required",
		 :CannotFillDemand "Cannot Fill Demand",
		 :UpdateRequest "UpdateRequest",
		 :CycleStarted "Cycle Started",
		 :MoreSRCAvailable "More SRC Available",
		 :FollowingOn "Follow On",
		 :deploy "Deploying Unit",
		 :Initialize "Initializing",
		 :OverlappingUnit "Overlapping Unit",
		 :InfeasibleDemand "Infeasible Demand",
		 :LocationDecrement "Location Decrement",
		 :ActivateDemand "Activating Demand",
		 :ScopedSupply "Scoped Supply",
		 :NewSRCAvailable "New SRC Stock",
		 :PositionUnit "Positioned Unit",
		 :EndofDay "End of Day",
		 :demandupdate "DemandUpdate",
		 :NotDeployable "Not Deployable",
		 :UnitMoved "Unit Moved",
		 :GhostDeployed "Ghost Deployed",
		 :periodChange "Period Change",
		 :BeginDay "Begin Day",
		 :AddedUnit "Added Unit",
		 :DeActivateDemand "DeActivating Demand",
		 :AddedDemand "Added Demand",
		 :all "Generic",
		 :FillDemand "Filling Demand",
		 :SpawnGhost "Spawn Ghost",
		 :ScopedDemand "Scoped Demand",
		 :LocationIncrement "Location Increment",
		 :SpawnedTransient "Spawned Transient Unit",
		 :CanFillDemand "Can Fill Demand",
		 :DisengageUnit "Sending Unit Home"})
