(ns marathon.port.data.managerofevents)

;TOM Note 6 April 2011 ->
    ;I re-factored much of the guts of this class into a GenericEventStream class, which handles
    ;all of the subscribing, cataloging, etc., and wraps an observer to provide communications.
    ;As a result, I will also
;This class serves as a proxy for encapsulating what appear to be classic
;;;event;; handlers. Basically, I want to standardize the generation of events (mostly
;log events in our case), but also to begin to meld the notion of event-driven simulation
;within the time-step model. The goal is to eventually turn this into an actual event-driven
;sim. We just manually "trigger" events from calling objects, and have this class handle them.
;The events are actually methods contained in the class. The only difference between this
;and the event-step framework, is that we;re calling handlers from a class directly, rather
;than queuing events in a priorityQ (lose some robustness, since we can have multiple handlers
;with their own state subscribing to events.
;
;Option Explicit
;Public name As String
;Public parent As TimeStep_Engine
;Public userEvents As Dictionary

  
;Creating a bandwith for message enumerations.  Right now, each message has a bandwith of ~100.
;from the msgbands, by convention, all simulation messages should be between 100 and 199
;supply msgs -> 200 -> 299
;demand msgs -> 300 -> 399
;if we need to increase the bandwith later, we can (total messages are actually vast, 2^32)
;Right now, I arbitrarily allocated 99 messages worth of space for each gross category off messages.
;This aids when we;re trying to categorize messages later, for instance, when adding a bunch of loggers,
;I want to associate a particular logger only with "Simulation" messages/events.
;As a result, I can keep a sub up to date that will filter out the appropriate messages for it to
;subscribe to.

;NOTE -> While events appear to be explicitly enumerated here, their actual
;representation is really just a long integer and a dictionary of data.  All of the association
;of events with names, msgbands, etc. is really local to this object.  The end result is that
;the library of events can be easily extended (even via modules or scripts) to accomodate custom
;events.

;Users can define their own ad-hoc events (and should!) that will get assigned to be
;well beyond the existing bandwidth.
;I will implement a custom-event API for this functionality.  Currently, one could add a custom
;event using the AddEvent method.

;Public Enum msgbands
;    Simulation = 1
;    supply = 2
;    demand = 3
;    policy = 4
;    trending = 5
;End Enum



;Really, this is just a method library. We pull the methods from other classes (particularly
;where the method calls are manipulating state across classes, and logging events) .
;Since we;re working with objects and references, we can do this now.
;Pulled this from the MarathonEnumerations, want them to overlap significantly
;Public Enum TimeStep_Msg ;CodeName, Probable Subscribers
;    ;Interest in any event...primarily used for observation purposes
;    all = 0
;    ;Simulation Events
;    Initialize = 101 ;Simhandler (name of sim,
;    Terminate = 102 ;Simhandler
;    sample = 103 ;Simhandler
;    EndofDay = 104
;    BeginDay = 105
;    UpdateRequest = 106
;    ;Supply Events
;    spawnunit = 201 ;SupplyHandler
;    deploy = 202 ;SupplyHandler
;    MoveUnit = 203 ;SupplyHandler
;    FillDemand = 204 ;SupplyHandler
;    NewDeployable = 205 ;New Deployable SRC
;    NewSRCAvailable = 206 ;New type of SRC available
;    MoreSRCAvailable = 207
;    NotDeployable = 208
;    Deployable = 209
;    outofstock = 210
;    CycleCompleted = 211 ;
;    CycleStarted = 212 ;
;    SpawnGhost = 213 ;
;    supplyUpdate = 214
;    ScopedSupply = 215
;    AddedUnit = 216
;    PositionUnit = 217
;    UnitChangedPolicy = 218
;    AwaitingPolicyChange = 219
;    SpawnedTransient = 220
;    neverDeployed = 221
;    NewFollowOn = 222
;    FollowingOn = 223
;    UnitMoved = 224
;    ;Demand Events
;    scheduleDemand = 301 ;DemandHandler
;    ActivateDemand = 302 ;DemandHandler
;    DeActivateDemand = 303 ;DemandHandler
;    DisengageUnit = 304 ;DemandHandler
;    RequestFill = 305 ;DemandHandler
;    InfeasibleDemand = 306 ;demand cannot "ever" be filled
;    extendedBOG = 307
;    demandupdate = 308
;    ScopedDemand = 309
;    CannotFillDemand = 310
;    CanFillDemand = 311
;    StockRequired = 312
;    AddedDemand = 313
;    DemandFillChanged = 314
;    OverlappingUnit = 315
;    ;Policy Events
;    policychange = 401 ;
;    periodChange = 402
;    AddedPeriod = 403
;    ;Trending Events
;    LocationIncrement = 501
;    LocationDecrement = 502
;    LocationSwap = 503 ;more effecient for trending, trigger 1 even vs 2.
;    GhostDeployed = 504
;    GhostReturned = 505
;End Enum

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

;Public evtstream As GenericEventStream
;Public streams As Dictionary ;collection of event streams.
;Public packet As GenericPacket
;Private state As Dictionary
;Public ptr As Dictionary
;Implements IVolatile
;
;Private Sub Class_Initialize()
;
;Set evtstream = New GenericEventStream
;Set userEvents = New Dictionary
;name = "EventManager"
;SimulationStream
;SupplyStream
;DemandStream
;PolicyStream
;TrendingStream
;Set streams = New Dictionary
;
;End Sub

(defrecord managerofevents [name userevents evtstream streams])
(def empty-managerofevents
  (managerofevents. "EventManager" {} {} {}))
  

;Public Sub AddStream(name As String, evts As Dictionary)
;Dim stream As GenericEventStream
;Dim supplier As GenericObserver
;Dim subscriber As GenericObserver
;Dim evt
;Dim id As Long
;Dim description As String
;
;If Not streams.Exists(name) Then
;    Set stream = New GenericEventStream
;    streams.add name, stream
;    Set subscriber = stream
;    For Each evt In evts
;        id = CLng(evt)
;        if not evtstream.eventExists
;
;
;End Sub

;Public Function GetObserver() As GenericObserver
;Set GetObserver = evtstream.observer
;End Function

;Sub to subscribe a listener object, an ITriggerable, to one or more event ids
;Public Sub AddListener(clientname As String, client As ITriggerable, ParamArray eventIDs())
;Dim id As TimeStep_Msg
;Dim id

;For Each id In eventIDs
;    evtstream.AddListener clientname, client, CLng(id)
;Next id

;End Sub
;Sub to unsubscribe a listener object, an ITriggerable, from one or more event ids
;Public Sub RemoveListener(clientname As String, ParamArray eventIDs())
;Dim id

;For Each id In eventIDs
;    evtstream.RemoveListener clientname, CLng(id)
;Next id

;End Sub
;Public Sub ClearListener(clientname As String)
;evtstream.ClearListener clientname
;End Sub
;;;Tom Change 7 June 2011
;;;Alternate sub to add custom listeners to events.
;;Public Sub AddCustomListener(clientname As String, client As ITriggerable, eventID As Long)
;;evtStream.AddListener clientname, client, eventID
;;End Sub

;Public Function eventExists(msgid As Long) As Boolean
;eventExists = evtstream.eventExists(msgid)
;End Function

;Sub routine to associate event name and category information with a msgid
;Public Sub addEvent(msgid As Long, Optional evtname As String, Optional category As String)
;evtstream.addEvent msgid, evtname, category
;End Sub

;Public Function bandname(msgid As Long)
;Dim idx As Long
;idx = msgid \ 100
;If idx = 1 Then
;    bandname = "Simulation"
;ElseIf idx = 2 Then
;    bandname = "Supply"
;ElseIf idx = 3 Then
;    bandname = "Demand"
;ElseIf idx = 4 Then
;    bandname = "Policy"
;Else
;    bandname = "Other"
;End If

;End Function
;TOM Change
;Public Sub dispatch(t As Single, eventtype As Long, entityFrom As String, entityTo As String, msg As String, Optional logtarget As LogTargets, _
;                                                        Optional extradata As Variant)
;Dim str As String
;str = t & "," & eventtype & "," & entityFrom & "," & entityTo & "," & msg

;TOM Change 30 June 2011.....
;Realized I was wasting a lot of effort with unnecessary dictionary allocations.
;constructs a new packet from the input
;Set Packet = New GenericPacket
;With Packet
;    .t = t
;    .eventtype = eventtype
;    .eventname = MsgToString(eventtype)
;    .entityFrom = entityFrom
;    .entityTo = entityTo
;    .msg = msg
;    If extradata Is Nothing Then Set .data = New Dictionary Else Set .data = extradata
;    .data.add "LogMessage", str
;End With

;Set packet = makePacket(eventtype, MsgToString(eventtype), t, entityFrom, entityTo, msg, extradata)
;Set state = Nothing
;Set state = New Dictionary ;this might kill us in allocations....trying to make it generic.  we;ll see.

;TOM Change 30 June 2011.....
;state.add "Packet", Packet
;evtstream.trigger eventtype, packet ;broadcast the event to all subscribers, sending appropriate state along..
;End Sub
;Public Function MsgToString(msgid As TimeStep_Msg) As String
;MsgToString = evtstream.msgindex(msgid)
;End Function

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
;Public Sub defaults()
;DefaultLogs
;End Sub
;some simple methods that we;ll generally call by default...
;Private Sub DefaultLogs()

;addLog "DemandLog", DemandLog, True ;creates and registers an observer for all demand msgs, with header entry
;addLog "SupplyLog", SupplyLog ;vbnullstring for all supply msgs
;addLog "SimulationLog", SimulationLog ;vbnullstringfor all simulation msgs
;addLog "PolicyLog", PolicyLog

;End Sub

;Public Function CurrentTime() As Single
;;Decouple
;CurrentTime = parent.CurrentTime
;End Function

;Private Sub Class_Terminate()

;Set parent = Nothing
;Set userEvents = Nothing
;Set evtstream = Nothing
;Set streams = Nothing ;collection of event streams.
;Set packet = Nothing
;Set state = Nothing
;Set ptr = Nothing

;End Sub

;Private Sub IVolatile_Reset()
;don;t need to do anything with this guy.

;End Sub

;Private Sub IVolatile_Terminate()
;Class_Terminate
;End Sub
