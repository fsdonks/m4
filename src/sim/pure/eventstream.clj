(ns sim.eventstream)

'Integrated 29 Aug 2012

'Tom note 5 April 2012-> This is more or less the basis for my discrete event simulation, in addition
'to some other event-driven functionality (like GUI interaction).  You can define your own event
'vocabulary on top of this class, and hook up any object that implement ITriggerable.  A common
'technique is to develop a simulation, have some class act as the event manager, have it add
'simulation-specific events (usually defined as enumerated types) to the event stream, and act
'as a hub for event traffic (i.e. adding/removing listeners, etc.).  You can get a lot of mileage
'out of this setup, even in VBA.  Note, you can send optional data along with your events, so
'there is a lot of flexibility and the ability to provide event context.


'This class serves as a proxy for encapsulating what appear to be classic
'''event'' handlers. Basically, I want to standardize the generation of events (mostly
'log events in our case), but also to begin to meld the notion of event-driven simulation
'within the time-step model. The goal is to eventually turn this into an actual event-driven
'sim. We just manually "trigger" events from calling objects, and have this class handle them.
'The events are actually methods contained in the class. The only difference between this
'and the event-step framework, is that we're calling handlers from a class directly, rather
'than queuing events in a priorityQ (lose some robustness, since we can have multiple handlers
'with their own state subscribing to events.

Option Explicit
Public msgindex As Dictionary
Public observer As GenericObservable

Public msgcategories As Dictionary
'Private children As Dictionary
'Private memo As Dictionary
'Private routing As Dictionary

Public ptr As Dictionary

Private Sub Class_Initialize()
Set observer = New GenericObservable
observer.name = "EventStream"
Set msgindex = New Dictionary
Set msgcategories = New Dictionary
'Set children = New Dictionary 'child streams
'Set memo = New Dictionary
'Set routing = New Dictionary

End Sub

Public Function eventExists(msgid As Long) As Boolean
eventExists = msgindex.exists(msgid)
End Function

'Sub routine to associate event name and category information with a msgid
Public Sub addEvent(msgid As Long, Optional evtname As String, Optional category As String)

If Not eventExists(msgid) Then
    If evtname = vbNullString Then evtname = "Anonymous"
    msgindex.add msgid, evtname
Else
    Err.Raise 101, , "Event ID " & msgid & " already exists!"
End If
    
If category <> vbNullString Then
    If msgcategories.exists(msgid) Then
        Set ptr = msgcategories(msgid) 'find the parent category
    Else
        Set ptr = New Dictionary
        msgcategories.add msgid, ptr 'categorize the msgid
    End If
    If ptr.exists(category) = False Then
        ptr.add category, msgid
        If msgcategories.exists(category) = False Then
            Set ptr = New Dictionary
            msgcategories.add category, ptr
        Else
            Set ptr = msgcategories(category)
        End If
        ptr.add msgid, category
    End If
End If

End Sub

'Add a subordinate stream, whose events are composed into the overall events processed/known by this
'stream.  These events are routed to the subordinate stream.  This allows a partitioning of the
'event space, flow-control so to speak.  The events dictionary must conform to a couple of things...
    'Must be <Long, String> key-value-pairs, this is consistent with the basic event signature, where
    'the event code is the long value, and the string is the event description.
    'If added events do not exist in the msgindex, they are added to the parent's msgindex, indicating
        'that the parent stream "knows" how to handle said messages.  If the message already exists
        
        'However, registering of listeners is delegated to the stream that actually controls the
        'messaging.  We can also get the stream associated with the message/event directly, which
        'narrows the amount of triggering we do, and allows us to partition the routing much easier.
    'If the event exists in the parent's msgindex, a contradiction occurs and an exception is thrown.

'Public Sub Compose(instream As GenericEventStream)
'
'End Sub

'Sub to subscribe a listener object, an ITriggerable, to an event id
Public Sub AddListener(clientname As String, client As ITriggerable, msgid As Long) 'As TimeStep_Msg)
observer.Register clientname, client, CLng(msgid)
End Sub
Public Sub RemoveListener(clientname As String, Optional msgid As Long)
observer.unRegister clientname, msgid
End Sub
Public Sub ClearListener(clientname As String)
observer.clearClient clientname
End Sub

'TOM Change
Public Sub trigger(eventtype As Long, Optional data As GenericPacket)
observer.notify eventtype, data 'broadcast the event to all subscribers, sending appropriate state along..
End Sub

Public Function MsgToString(msgid As TimeStep_Msg) As String
MsgToString = msgindex(msgid)
End Function



Private Sub Class_Terminate()
Set msgindex = Nothing
Set observer = Nothing

Set msgcategories = Nothing

Set ptr = Nothing
End Sub
