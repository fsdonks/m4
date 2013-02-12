;This is a dirty port of the decoupled VBA code I busted my ass over 2 weeks 
;to produce.  Unfortunately, VBA has a lovely habit of corrupting itself...
;se here we are, porting to clojure (and parts beyond maybe).
(ns marathon.port.data.coredata)

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
