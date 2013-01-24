(ns marathon.port.data.managerofevents)

;This class serves as a proxy for encapsulating what appear to be classic
;;;event;; handlers. Basically, I want to standardize the generation of events 
;(mostly log events in our case), but also to begin to meld the notion of 
;event-driven simulation within the time-step model. The goal is to eventually 
;turn this into an actual event-driven sim. We just manually "trigger" events 
;from calling objects, and have this class handle them. The events are actually 
;methods contained in the class. The only difference between this and the 
;event-step framework, is that we're calling handlers from a class directly, 
;rather than queuing events in a priorityQ (lose some robustness, since we can
;have multiple handlers with their own state subscribing to events.


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

(defrecord managerofevents [name userevents evtstream streams])
(def empty-managerofevents (managerofevents. "EventManager" {} {} {}))
 
(defn get-observer [estream]) 

(defn add-listener [estream client-name client ids])  
(defn remove-listener [estream client-name ids])
(defn clear-listener [estream client-name])

(defn csv-string [xs] (apply str (interleave xs (repeat \.))))
(defn dispatch [estream t event-type entity-from entity-to msg & [extra-data]]
  ;str = t & "," & eventtype & "," & entityFrom & "," & entityTo & "," & msg
  (let [msg  (csv-string t event-type entity-from entity-to msg)
        p (make-packet event-type (get events msg) entity-from 
             entity-to msg  extra-data)]                       
    (trigger estream p))
  )

;Public Function MsgToString(msgid As TimeStep_Msg) As String
;MsgToString = evtstream.msgindex(msgid)
;End Function
(defn msg->string [msg] (get events msg))

;Public Sub addLog(logname As String, Optional logtarget As LogTargets, Optional header As Boolean)
;Dim listen
;Dim Key
;Dim id As Long
;Dim tgt As String
;Dim newlog As TimeStep_ObserverLog
;Set newlog = New TimeStep_ObserverLog
;newlog.init logname, Me, logtarget, header
;we;ll bias the log based off of its logtarget for now....
;subscribe the log
;If logtarget = SimulationLog Then
;    tgt = "Simulation"
;ElseIf logtarget = SupplyLog Then
;    tgt = "Supply"
;ElseIf logtarget = DemandLog Then
;    tgt = "Demand"
;ElseIf logtarget = PolicyLog Then
;    tgt = "Policy"
;Else
;    tgt = "Simulation"
;End If

;For Each Key In evtstream.msgcategories(tgt)
;    id = CLng(Key)
;    evtstream.AddListener logname, newlog, id
;Next Key

;End Sub

;Public Function CurrentTime() As Single
;;Decouple
;CurrentTime = parent.CurrentTime
;End Function

