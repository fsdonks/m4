(ns sim.eventlib)

'A library for defining contextual events, used to work with
'Discrete Event Simulations.  Primarily defines operations on
'GenericEventStream classes.
Option Explicit
'Operation for defining an event specification from a list (an entry).
Public Function eventEntry(eventID As Long, eventName As String, Optional category As String) As Collection
If category = vbNullString Then category = "Universal"
Set eventEntry = list(eventID, eventName, category)
End Function
'given a sequence of arguments, in the order of EventID EventName,
'builds an eventlist of (eventID, EventName) entries.
Public Function eventList(ParamArray keyvals()) As Collection
Dim entry As Collection
Set eventList = getArgPairs(keyvals)
For Each entry In eventList
    entry.add "General"
Next entry

End Function
'Operation for mapping a category across multiple event entries.  Returns a new
'list of event entries.
Public Function eventsOfCategory(category As String, entries As Collection) As Collection
Dim e As Collection
Set eventsOfCategory = New Collection
For Each e In entries
    eventsOfCategory.add list(fst(e), snd(e), category)
Next e
End Function
'Function that takes a dictionary as a meta-specification.
'Each key in the dictionary should be a string that assumably defines
'a category for a sequence of events.  The value associated with the
'category key should be a flat list of lists according to the
'event entry scheme (eventID, eventName,...)
'Categories will be superceded by the category key.
Public Function dictToEventEntries(eventDictionary As Dictionary) As Collection
Dim category
Dim entry As Collection
Set dictToEventEntries = New Collection

For Each category In eventDictionary
    For Each entry In eventsOfCategory(CStr(category), eventDictionary(category))
        dictToEventEntries.add entry
    Next entry
Next category
    
End Function
Public Function makeEventDictionary(categories As Collection, eventLists As Collection)

End Function
'Procedure for defining new event contexts on a GenericEventStream
Public Sub addEvent(estream As GenericEventStream, eventID As Long, eventName As String, category As String)
estream.addEvent eventID, eventName, category
End Sub
'Procedure for defining multiple event entries on an event stream.
Public Sub addEvents(estream As GenericEventStream, eventEntries As Collection)
Dim e As Collection
Dim entries As Collection

If IsObject(eventEntries(1)) Then
    Set entries = eventEntries
Else
    Set entries = eventsOfCategory("Generic", partition(eventEntries, 2))
End If

For Each e In entries
    estream.addEvent fst(e), snd(e), thrd(e)
Next e

End Sub
'Constructor for building event streams. Allows an optional list of event entries to serve as a
'specification.
Public Function makeEventStream(Optional eventEntries As Collection) As GenericEventStream
Set makeEventStream = New GenericEventStream
If exists(eventEntries) Then addEvents makeEventStream, eventEntries
End Function
'Returns a list of (ID, Name) pairs.
Public Function getEvents(stream As GenericEventStream) As Collection
Set getEvents = listKeyVals(stream.msgindex)
End Function
'Creates a new stream that is the result of merging the events from all the GenericEventStreams in streams.
'Input streams to-be-merged, are defined in a dictionary of {category stream} pairs.
'The inputs streams are thus automatically categorized.
'The new stream notifies subscribers of events if any of the streams fire.
Public Function mergeEventStreams(mergedname As String, streams As Dictionary, Optional ignoreCategories As Boolean) As GenericEventStream
Dim category
Dim merged As GenericEventStream
Dim entry As Collection
Dim stream As GenericEventStream
Dim cat As String

Set merged = New GenericEventStream
For Each category In streams
    Set stream = streams(category)
    If ignoreCategories Then
        cat = "MergedEvents"
    Else
        cat = CStr(category)
    End If
    For Each entry In getEvents(stream)
        If Not (merged.eventExists(fst(entry))) Then _
            addEvent merged, fst(entry), snd(entry), cat 'only add events that don't exist.
        stream.AddListener mergedname, merged.observer, CLng(fst(entry))
    Next entry
Next category

Set mergeEventStreams = merged
Set merged = Nothing
    
End Function
'Like merge, except it simplifies calling code.  In this case, in the new
'stream, names are unimportant.  A simple collection of event streams are
'passed in, and events are propogated simply.
Public Function flattenEventStreams(streams As Collection, Optional indexedCategories As Boolean) As GenericEventStream
Dim stream As GenericEventStream
Dim nameset As Dictionary
Dim namelist As Collection
Dim mergename As String

Set nameset = SetLib.emptySet
For Each stream In streams
    Set nameset = SetLib.union(nameset, stream.observer.clients)
Next stream

mergename = genName("merged", nameset)
Set flattenEventStreams = mergeEventStreams(mergename, zipMap(intList(streams.count), streams), Not indexedCategories)
End Function

Public Sub flattentest()
Dim s1 As GenericEventStream, s2 As GenericEventStream, s3 As GenericEventStream, s4 As GenericEventStream
Dim events As Collection
Set s1 = makeEventStream(eventList(all, "Generic", Initialize, "Initializing"))
Set s2 = makeEventStream(eventList(spawnunit, "Spawning Unit", deploy, "Deploying Unit"))

Set s3 = mergeEventStreams("Stream3", newdict("Simulation", s1, "Supply", s2))
Set s4 = flattenEventStreams(list(s1, s2))

End Sub

'Casts o to IObservable.
Public Function asObservable(ByRef o) As IObservable
Set asObservable = o
End Function

'Casts o to IObserver (currently ITriggerable)
Public Function asObserver(ByRef o) As ITriggerable
Set asObserver = o
End Function

'notifies an observable of Data.  Calls its notification method
'with a msgid of 0
Public Sub notifyData(o As IObservable, data)
o.notify 0, makePacket(0, , , , , , data)
End Sub

'notifies an observable to propgate a dataless message.  Calls its notification method
'with a msgid of id, but no data.
Public Sub notifyMsg(o As IObservable, msgid As Long)
o.notify msgid
End Sub
'Attaches a simple, or anonymous observer to the observable event.
'Assumes the event only broadcasts on msgid 0
Public Sub observeEvent(evt As IObservable, observer As ITriggerable, Optional name As String)
Dim clients As Dictionary
Set clients = evt.getClients(0)

If name = vbNullString Xor clients.exists(name) Then
    name = genName("Anonymous", clients)
End If

evt.Register name, observer
Set clients = Nothing
End Sub

''Simulation events
'Private Sub SimulationStream()
'addEvent all, "Generic", "Generic"
'addEvent Initialize, "Initializing", "Simulation"
'addEvent Terminate, "Terminating", "Simulation"
'addEvent Sample, "Sampling", "Simulation"
'addEvent EndofDay, "End of Day", "Simulation"
'addEvent BeginDay, "Begin Day", "Simulation"
'addEvent UpdateRequest, "UpdateRequest", "Simulation"
'
'End Sub
'
''Supply Events
'Private Sub SupplyStream()
'addEvent SpawnUnit, "Spawning Unit", "Supply"
'addEvent deploy, "Deploying Unit", "Supply"
'addEvent MoveUnit, "Moving Unit", "Supply"
'addEvent FillDemand, "Filling Demand", "Supply"
'addEvent NewDeployable, "New Deployable Stock", "Supply"
'addEvent NewSRCAvailable, "New SRC Stock", "Supply"
'addEvent MoreSRCAvailable, "More SRC Available", "Supply"
'addEvent NotDeployable, "Not Deployable", "Supply"
'addEvent Deployable, "Deployable", "Supply"
'addEvent outofstock, "out of Stock", "Supply"
'addEvent CycleCompleted, "Cycle Completed", "Supply"
'addEvent CycleStarted, "Cycle Started", "Supply"
'addEvent SpawnGhost, "Spawn Ghost", "Supply"
'addEvent supplyUpdate, "Supply Update", "Supply"
'addEvent ScopedSupply, "Scoped Supply", "Supply"
'addEvent AddedUnit, "Added Unit", "Supply"
'addEvent PositionUnit, "Positioned Unit", "Supply"
'addEvent UnitChangedPolicy, "Unit Changed Policy", "Supply"
'addEvent AwaitingPolicyChange, "Awaiting Policy Change", "Supply"
'addEvent SpawnedTransient, "Spawned Transient Unit", "Supply"
'addEvent neverDeployed, "Never Deployed", "Supply"
'addEvent NewFollowOn, "New Follow On", "Supply"
'addEvent FollowingOn, "Follow On", "Supply"
'addEvent UnitMoved, "Unit Moved", "Supply"
'
'End Sub
'
''Demand Events
'Private Sub DemandStream()
'addEvent RequestFill, "Requesting Fill", "Demand"
'addEvent scheduleDemand, "Scheduling Demand", "Demand"
'addEvent ActivateDemand, "Activating Demand", "Demand"
'addEvent DeActivateDemand, "DeActivating Demand", "Demand"
'addEvent DisengageUnit, "Sending Unit Home", "Demand"
'addEvent InfeasibleDemand, "Infeasible Demand", "Demand"
'addEvent extendedBOG, "Extended BOG", "Demand"
'addEvent demandupdate, "DemandUpdate", "Demand"
'addEvent ScopedDemand, "Scoped Demand", "Demand"
'addEvent CannotFillDemand, "Cannot Fill Demand", "Demand"
'addEvent CanFillDemand, "Can Fill Demand", "Demand"
'addEvent StockRequired, "Stock Required", "Demand"
'addEvent AddedDemand, "Added Demand", "Demand"
'addEvent DemandFillChanged, "Demand Fill Changed", "Demand"
'addEvent OverlappingUnit, "Overlapping Unit", "Demand"
'End Sub
''Policy Events
'Private Sub PolicyStream()
'addEvent policychange, "Policy Change", "Policy"
'addEvent AddedPeriod, "Added Period", "Policy"
'addEvent periodChange, "Period Change", "Policy"
'
'End Sub
'
''Trending Events
'Private Sub TrendingStream()
'addEvent LocationIncrement, "Location Increment", "Trending"
'addEvent LocationDecrement, "Location Decrement", "Trending"
'addEvent LocationSwap, "Location Swap", "Trending"
'addEvent GhostDeployed, "Ghost Deployed", "Trending"
'addEvent GhostReturned, "Ghost Returned", "Trending"
'End Sub
'
'Public Function GetObserver() As GenericObservable
'Set GetObserver = evtstream.observer
'End Function
''This is no longer necessary....most of it's handled in GenericObservable.
'
''Sub to subscribe a listener object, an ITriggerable, to one or more event ids
'Public Sub AddListener(clientname As String, client As ITriggerable, ParamArray eventIDs())
''Dim id As TimeStep_Msg
'Dim id
'
'For Each id In eventIDs
'    evtstream.AddListener clientname, client, CLng(id)
'Next id
'
'End Sub
''Sub to unsubscribe a listener object, an ITriggerable, from one or more event ids
'Public Sub RemoveListener(clientname As String, ParamArray eventIDs())
'Dim id
'
'For Each id In eventIDs
'    evtstream.RemoveListener clientname, CLng(id)
'Next id
'
'End Sub
'
'Public Function eventExists(msgid As Long, evtstream) As Boolean
'eventExists = evtstream.eventExists(msgid)
'End Function
'
''Sub routine to associate event name and category information with a msgid
'Public Sub addEvent(msgid As Long, Optional evtname As String, Optional category As String)
'evtstream.addEvent msgid, evtname, category
'End Sub
'Public Function bandname(msgid As Long)
'Dim idx As Long
'idx = msgid \ 100
'If idx = 1 Then
'    bandname = "Simulation"
'ElseIf idx = 2 Then
'    bandname = "Supply"
'ElseIf idx = 3 Then
'    bandname = "Demand"
'ElseIf idx = 4 Then
'    bandname = "Policy"
'Else
'    bandname = "Other"
'End If
'
'End Function
''TOM Change
'Public Sub dispatch(t As Single, eventtype As Long, entityFrom As String, entityTo As String, msg As String, Optional logtarget As LogTargets, _
'                                                        Optional extradata As Variant)
'Dim str As String
'str = t & "," & eventtype & "," & entityFrom & "," & entityTo & "," & msg
'
''TOM Change 30 June 2011.....
''Realized I was wasting a lot of effort with unnecessary dictionary allocations.
''constructs a new packet from the input
''Set Packet = New GenericPacket
''With Packet
''    .t = t
''    .eventtype = eventtype
''    .eventname = MsgToString(eventtype)
''    .entityFrom = entityFrom
''    .entityTo = entityTo
''    .msg = msg
''    If extradata Is Nothing Then Set .data = New Dictionary Else Set .data = extradata
''    .data.add "LogMessage", str
''End With
'
'Set packet = makePacket(eventtype, MsgToString(eventtype), t, entityFrom, entityTo, msg, extradata)
''Set state = Nothing
''Set state = New Dictionary 'this might kill us in allocations....trying to make it generic.  we'll see.
'
''TOM Change 30 June 2011.....
''state.add "Packet", Packet
'evtstream.trigger eventtype, packet 'broadcast the event to all subscribers, sending appropriate state along..
'End Sub
'Public Function MsgToString(msgid As TimeStep_Msg) As String
'MsgToString = evtstream.msgindex(msgid)
'End Function
'
'Public Sub addLog(logname As String, Optional logtarget As LogTargets, Optional header As Boolean)
'Dim listen
'Dim key
'Dim id As Long
'Dim tgt As String
'Dim newlog As TimeStep_ObserverLog
'Set newlog = New TimeStep_ObserverLog
'newlog.init logname, Me, logtarget, header
''we'll bias the log based off of its logtarget for now....
''subscribe the log
'If logtarget = SimulationLog Then
'    tgt = "Simulation"
'ElseIf logtarget = SupplyLog Then
'    tgt = "Supply"
'ElseIf logtarget = DemandLog Then
'    tgt = "Demand"
'ElseIf logtarget = PolicyLog Then
'    tgt = "Policy"
'Else
'    tgt = "Simulation"
'End If
'
'For Each key In evtstream.msgcategories(tgt)
'    id = CLng(key)
'    evtstream.AddListener logname, newlog, id
'Next key
'
'End Sub
'Public Sub defaults()
'DefaultLogs
'End Sub
''some simple methods that we'll generally call by default...
'Private Sub DefaultLogs()
'
'addlogStatus "DemandLog", DemandLog, True 'creates and registers an observer for all demand msgs, with header entry
'addlogStatus "SupplyLog", SupplyLog 'vbnullstring for all supply msgs
'addlogStatus "SimulationLog", SimulationLog 'vbnullstringfor all simulation msgs
'addlogStatus "PolicyLog", PolicyLog
'
'End Sub
'Public Function CurrentTime() As Single
''Decouple
'CurrentTime = parent.CurrentTime
'End Function
'
'Private Sub Class_Terminate()
'
'Set parent = Nothing
'Set userEvents = Nothing
'Set evtstream = Nothing
'Set streams = Nothing 'collection of event streams.
'Set packet = Nothing
'Set state = Nothing
'Set ptr = Nothing
'
'End Sub
'
'Private Sub IVolatile_Reset()
''don't need to do anything with this guy.
'
'End Sub
'
'Private Sub IVolatile_Terminate()
'Class_Terminate
'End Sub

