;;A dumping ground for legacy code from other namespaces.
;;Used to clean up the docs and highlight pending porting 
;;efforts.
(ns marathon.sim.legacy)

(comment :marathon.policy.policydata :deferred 
         ;'TOM Change 20 May 2011 -> Policy nodes are NOW POSITIONS, no longer Locations.
;    'Change in nomenclature.
;    'Has significant consquences, in that it separates spatial location from policy position.
;
;'TOM change 3 Jan 2011
;'Class for encapsulating implementations of policies ....
;'This is a datastructure designed to allow us to flexibly define Multiple policies.
;'Policy is = a state transition graph, the sequence of Positions a unit will cycle through
;'and parameters that guide said policy.
;'It is my contention that we can describe a vast number of policy types, including those currently
;'employed, by a parameterized function.
;'The crucial parameters are ....
;    'Name
;    'CycleLength 'duration of a standard cycle, absent interference
;    'Minimum Dwell
;    'Maximum Dwell
;    'Maximum BOG
;    'Lower Deployment Window
;    'Upper Deployment Window
;    'Overlap
;'        (d) 182 (d) 183 (d) 365 (d) 365 (d)
;'        PositionRouting (Reset -> Train -> Ready -> Available -> Reset) 'Note, this could be any number of locs
;'        (b) 365 (b) Overlap Parameter
;'        (Deployed ->Overlap ->Reset)
;'        StartState (Reset) *this is almost a given .... it's our basis for the start of cycles.
;Option Explicit
;
;Public name As String
;Public cyclelength As Long
;Public mindwell As Long 'Minimum required Time spent dwelling prior to deploying.
;Public maxdwell As Long 'Maximum allowed Time spent dwelling, will return to CycleStart.
;Public maxbog As Long 'most BOG a unit can take
;Public MaxMOB As Long 'most mobilization a unit can take.
;Private pbogbudget As Long 'Most BOG a unit can accumulate in a cycle.
;'TOM Change 4 August 2011
;
;Public recovery As Long 'time spent in post-deployment recovery.  default is 0
;
;Public startdeployable As Single 'earliest time we can deploy
;Public stopdeployable As Single 'latest time we can deploy
;Public PositionGraph As GenericGraph 'DIRECTED graph, where edges are Position transitions, weights are transition time,
;'Node Data describes unit's state (bogging/dwelling, etc.)
;Public startstate As String 'Position name of the Start of the Cycle
;Public StartIndex As Long 'Legacy support .... have to index these.
;Public endstate As String 'Position name of the End of the Cycle
;Public EndIndex As Long 'same, have to index for now.
;Public overlap As Long
;Public subscribers As Dictionary
;
;

;Private ptr As Dictionary
;Private memoize As Dictionary
;Implements IRotationPolicy
;Implements ISerializable
;'Tom change 23 April 2012
;Public Property Get bogbudget() As Long
;If pbogbudget = 0 Then
;    bogbudget = maxbog
;Else
;    bogbudget = pbogbudget
;End If
;
;
;End Property
;Public Property Let bogbudget(inval As Long)
;If inval >= 0 Then
;    pbogbudget = inval
;Else
;    Err.Raise 101, , "cannot have negative bog budget"
;End If
;End Property
;
;Private Sub Class_Initialize()
;Set PositionGraph = New GenericGraph
;Set subscribers = New Dictionary
;Set memoize = New Dictionary
;bogbudget = 0
;End Sub
;'units subscribe to policies, so when we change, we can notify only the affected units.
;Public Sub Subscribe(unit As TimeStep_UnitData)
;If Not HasSubscriber(unit.name) Then
;    subscribers.add unit.name, unit
;End If
;
;End Sub
;Public Function HasSubscriber(name As String) As Boolean
;HasSubscriber = subscribers.exists(name)
;End Function
;'procedure to change all subscribers' policies to a new policy
;Public Sub changePolicy(newpolicy As TimeStep_Policy, Optional SpecificUnit As String)
;Dim unitname
;Static unit As TimeStep_UnitData
;If newpolicy.name <> name Then
;    If SpecificUnit <> vbNullString And HasSubscriber(SpecificUnit) Then
;        Set unit = subscribers(SpecificUnit)
;    Else
;        For Each unitname In subscribers
;            Set unit = subscribers(unitname)
;            newpolicy.Subscribe unit
;            subscribers.Remove (unitname)
;            unit.changePolicy newpolicy
;        Next unitname
;    End If
;End If
;
;End Sub
;
;'Nodes are input as a sequence of KeY,Value pairs. NodeName, Data
;'For our purposes, this will be PositionName, State(bog/dwell/bogeployable/dwelldeployable/etc.)
;Public Sub AddPosition(PosName As String, PosState As String, ParamArray MoreNodes())
;Dim count
;Dim nodename As String
;Dim nodeval
;
;'if arguements are not even, we truncate the list of nodes.
;If UBound(MoreNodes, 1) Mod 2 = 0 Then 'note, this is a zero based array
;    Err.Raise 101, , "last node has no data"
;Else
;    PositionGraph.addNode PosName, PosState
;    For count = LBound(MoreNodes, 1) To UBound(MoreNodes, 1)
;        If count Mod 2 = 0 Then
;            nodename = MoreNodes(count)
;        Else
;            nodeval = MoreNodes(count)
;            'record the node
;            PositionGraph.addNode nodename, nodeval
;        End If
;    Next count
;End If
;
;End Sub
;'Routes are edges in the graph. They represent some sequence that a unit following this policy must pass through.
;'Description is used to add more information about the nature of the transfer (is it definite or conditional?)
;'Generally, we should have one definite state transfer, and any number of conditional/other transfers (arcs) leading
;'from a node. Transfers can even be probablistic, carrying information on the chance of an edge being chosen.
;Public Sub AddRoute(Start As String, destination As String, TransferTime As Single)
;PositionGraph.addArc Start, destination, TransferTime
;End Sub
;'Get the state of the current Position (generally bogging,dwelling, etc., but potentially anything).
;'State information is stored at the Position's node
;Public Function getState(ByVal position As String) As Variant
;
;position = PositionToPolicy(position)
;
;If isModifier(position) Then position = previousPosition(position) 'assume the state of the previous loc
;
;If IsObject(PositionGraph.nodes(position)) Then
;    Set getState = PositionGraph.nodes(position)
;Else
;    getState = PositionGraph.nodes(position)
;End If
;
;End Function
;'TOM Note -> Rule of Thumb I instantiated is that if a Position does not exist in the
;'Position nodes, then it must be coming from Deployed (demands are not explicitly registered).
;'Get the next Position as a result of current Position and policy structure.
;'If the Position has no arcs leading out (it's an island), then it is considered an absorbing state
;'and will return itself as the next Position.
;Public Function nextposition(position As String) As String
;Dim Key
;Dim keys
;
;position = PositionToPolicy(position)
;
;If memoize.exists("NextPosition" & position) Then
;    nextposition = memoize("NextPosition" & position)
;Else
;    With PositionGraph
;        Set ptr = .neighbors(position)
;        If ptr.count = 1 Then
;            For Each Key In ptr
;                 nextposition = CStr(Key)
;            Next Key
;        ElseIf ptr.count = 0 Then
;            nextposition = position
;        Else
;            Err.Raise 101, , "Currently only expected one or no edges"
;        End If
;    End With
;    memoize.add "NextPosition" & position, nextposition
;End If
;End Function
;Public Function previousPosition(position As String) As String
;Dim Key
;Dim keys
;
;position = PositionToPolicy(position)
;
;If memoize.exists("previousPosition" & position) Then
;    previousPosition = memoize("previousPosition" & position)
;Else
;    With PositionGraph
;        Set ptr = .sources(position)
;        If ptr.count = 1 Then
;            For Each Key In ptr
;                 previousPosition = CStr(Key)
;            Next Key
;        Else
;            'TOM note 3 April -> check this out.  Get it squared away for startstate and endstate.
;            If ptr.exists("Spawning") Then
;                For Each Key In ptr
;                    If CStr(Key) <> "Spawning" Then
;                        previousPosition = CStr(Key)
;                    End If
;                Next Key
;            Else
;                Err.Raise 101, , "Currently only expected one edge"
;            End If
;        End If
;    End With
;    memoize.add "previousPosition" & position, previousPosition
;End If
;End Function
;'Get the length of the path between two Positions.
;Public Function TransferTime(startPosition As String, endPosition As String) As Single
;
;startPosition = PositionToPolicy(startPosition)
;endPosition = PositionToPolicy(endPosition)
;
;If memoize.exists("TransferTime" & startPosition & endPosition) Then
;    TransferTime = memoize("TransferTime" & startPosition & endPosition)
;Else
;    With PositionGraph
;        TransferTime = getArc(PositionGraph, startPosition, endPosition)
;    End With
;    
;    memoize.add "TransferTime" & startPosition & endPosition, TransferTime
;End If
;End Function
;
;Public Function PositionToPolicy(position As String) As String
;If memoize.exists("PositionToPolicy" & position) Then
;    PositionToPolicy = memoize("PositionToPolicy" & position)
;Else
;    If PositionGraph.nodeExists(position) Then
;        PositionToPolicy = position
;    Else
;        'TOM Note 25 Mar 2011 -> assume that a Position is deploying if it's not explicitly in policy.
;        Err.Raise 101, , "Should never have an unknown position in a node graph"
;        PositionToPolicy = "Deployed"
;    End If
;    memoize.add "PositionToPolicy" & position, PositionToPolicy
;End If
;
;End Function
;'Given a cycletime, what is the resulting Position?
;'Doing a simple linear search, this is OK for small N.
;'Our cycles shouldn't be that big ... will have much less than
;'20, usually 6 Positions tops.
;'Tom Change 20 May 2011 -> Changed name from GetPosition to getPosition
;Public Function getPosition(cycletime As Single) As String
;Dim currpos As String
;Dim nextpos As String
;Dim acc As Single
;'Might rethink this
;If cycletime > cyclelength Then
;    Err.Raise 101, , "Cycletime is out of bounds for this policy .... "
;ElseIf memoize.exists("GetPosition" & cycletime) Then
;    
;    getPosition = memoize("GetPosition" & cycletime)
;Else
;    currpos = startstate
;    nextpos = nextposition(currpos)
;    acc = TransferTime(currpos, nextpos)
;    While cycletime >= acc And nextpos <> startstate 'TOM Note I changed to <= from >=, see if it's buggy
;        currpos = nextpos
;        nextpos = nextposition(currpos)
;        acc = acc + TransferTime(currpos, nextpos)
;    Wend
;    getPosition = currpos
;    memoize.add "GetPosition" & cycletime, getPosition
;End If
;
;End Function
;'Given a cycletime, what is the resulting Position?
;'Doing a simple linear search, this is OK for small N.
;'Our cycles shouldn't be that big ... will have much less than
;'20, usually 6 Positions tops. O(N) search should be ok ...
;Public Function GetCycleTime(position As String) As Long
;Dim currloc As String
;Dim nextloc As String
;Dim acc As Single
;'tom change
;'Dim res As Collection
;Dim res As GenericSearchData
;
;position = PositionToPolicy(position)
;'Might rethink this
;If Not PositionGraph.nodeExists(position) Then
;    Err.Raise 101, , "Position is unknown for this policy .... "
;ElseIf memoize.exists("GetCycleTime" & position) Then
;    GetCycleTime = memoize("GetCycleTime" & position)
;Else
;    If position = startstate Then
;        GetCycleTime = 0
;    Else
;'        If name = "Ghost365_45" Then
;'            Debug.Print "ghost"
;'        End If
;        'Err.Raise 101, , "Fix the line below!  Need to change from SearchDepthFirst"
;        'Set res = PositionGraph.SearchDepthFirst(StartState, position)
;        Set res = DFS(PositionGraph, startstate, position)
;        'GetCycleTime = PositionGraph.GetLength(res)
;        GetCycleTime = res.distance(position)
;        If GetCycleTime < 0 Then
;            Err.Raise 101, , "No path exists to this Position from the startstate, check your policy."
;        End If
;    End If
;    memoize.add "GetCycleTime" & position, GetCycleTime
;End If
;End Function
;'Given a cycletime, we need to determine if it's deployable.
;Public Function isDeployable(cycletime As Single) As Boolean
;'is the cycle within the deployable window?
;'This is fastest.
;isDeployable = (cycletime >= startdeployable And cycletime <= stopdeployable)
;'get the Position for the cycletime.
;End Function
;Public Function isDwell(position As String) As Boolean
;position = PositionToPolicy(position)
;If memoize.exists("isDwell" & position) Then
;    isDwell = memoize("isDwell" & position)
;Else
;    isDwell = (getState(position) = Dwelling)
;    memoize.add "isDwell" & position, isDwell
;End If
;End Function
;'Deployability is no longer just a function of Position....it's also a function of
;'cycle time....this is based on an outdated assumption that a unit's availability would be associated with
;'its Position.  We could validate this assumption by creating more Positions....this is simple enough, although
;'it creates additional complexity.  On the other hand, when a unit changes Positions, we can check to see if
;'it "will become available" during the course of waiting at the next Position.  If so, we have the unit
;'behavior request an update for the unit at the specified avaiable time.
;'TOM Change 21 Mar 2011 -> I am going to adopt the convention that any Position with the substring "_Deployable"
;'is a valid deployment point.  We can use this to perform constant-time lookup to determine if a policy
;'allows a unit to deploy from a Position.
;'Consequently, deployability is driven ENTIRELY by the parameters for min deployable and max deployable.
;'When we assert, via the policy manager, a minimum deployable time and a maximum deployable time, we will
;'alter the structure of the Positiongraph to add additional nodes.
;    'Essentially, we add 2 new nodes -> StartDeployable and StopDeployable
;'We can add ANY Number of these nodes, by subverting existing nodes in the graph.  This is great, because it
;'allows us to potentially have highly variable, and dynamic policies.  Rather than a general window of deployable,
;'we could have multiple "pockets" of deployability.  Who knows.
;'To determine if a Position is deployable, we just do a DFS from the policystart to the currentPosition.
;'We then parse the resulting path (from the last Position), to determine the deployable state.  Simple.
;Public Function Deployable(position As String) As Boolean
;Dim path As Collection
;Dim res As GenericSearchData
;Dim loc As String
;Dim i As Long
;position = PositionToPolicy(position)
;If memoize.exists("Deployable" & position) Then
;    Deployable = memoize("Deployable" & position)
;Else
;    'TOM Note 21 Mar 2011 -> we need to incorporate changes in availability as a fn
;    'of policy here.....
;    'We have a DeployWindow as a new criteria now.....
;    ''deployable = isDwell(Position) And isDeployable(GetCycleTime(Position))
;    Deployable = False 'we assume a Position cannot deploy by default....
;'    Err.Raise 101, , "Fix the line below! Need to change from SearchDepthFirst "
;    'Set path = PositionGraph.getpath(PositionGraph.SearchDepthFirst(StartState, position))
;    If startstate <> position Then
;        Set res = DFS(PositionGraph, startstate, position)
;        Set path = GraphLib.tracePath(res, startstate, position)
;    Else
;        Set path = list(position)
;    End If
;    For i = path.count To 1 Step -1
;        loc = path(i)
;        If isModifier(loc) Then
;            If loc = "Deployable" Then Deployable = True Else Deployable = False
;            Exit For
;        End If
;    Next i
;    memoize.add "Deployable" & position, Deployable
;End If
;
;End Function
;Private Function isModifier(position As String) As Boolean
;'Position = PositionToPolicy(Position)
;isModifier = (position = "Deployable" Or position = "NotDeployable")
;End Function
;'algorithm for transforming a graph into a graph with modified nodes existing after tstart
;'This basically communicates meta-information about some temporally-dependent change in the policy.
;'This allows us to schedule things like deployable windows, etc. formally in the structure of the graph.
;'Makes it visual as well.  Plus, we can add multiple modifications to the policy.  This is pretty useful.
;'Right now, it's primarily used to communicate the existence of the deployable window, but the cases can
;'be drastically expanded.  The operative convention is that Anything delimited by a _ following the
;'initial Position name is a modification.
;'find the intersection of edges affected by the change.
;    'GetPosition at cycletime.
;    'This is the affected Position.
;    'create a new node subverting this Position...
;        'utilize native graph function subert the node with a "Deployable".
;        'new node automatically links to all subsequent nodes (should be 1 edge).
;        'since we're conserving the cost, we update the edge costs for the two nodes...
;Public Function insertModifier(cycletime As Single, policy As TimeStep_Policy, modifier As String) As TimeStep_Policy
;Dim post As Single
;Dim prior As Single
;Dim source As String
;Dim dest As String
;Dim cost As Single
;Dim splicednode As String
;Dim splicedstate As String
;
;If Not isModifier(modifier) Then Err.Raise 101, , "modifier is invalid"
;
;With policy
;    source = .getPosition(cycletime)
;    dest = .nextposition(source)
;    cost = .TransferTime(source, dest)
;    
;    prior = cycletime - .GetCycleTime(source) 'length of the new edge before the deployable
;    post = cost - prior 'length of the edge after the deployable
;End With
;
;With policy.PositionGraph
;    .RemoveArc EncodeArc(source, dest)
;    splicedstate = getState(source)
;    .addNode modifier, modifier 'we can change this.....
;    .addArc source, modifier, prior 'splice in the modifier to inform folks of the change.
;    splicednode = (modify(source, modifier))
;    .addNode splicednode, splicedstate
;    .addArc modifier, splicednode, 0 'attach modifier to resulting modified policy, instantaneous transition
;    .addArc splicednode, dest, post 'splice the modified state back into the regular schedule
;End With
;
;'pathlength between source and destination should be conserved.  We just added more nodes.
;If prior + post <> cost Then Err.Raise 101, , "Pathlength not conserved"
;
;Set insertModifier = policy
;clearmemo 'modifications invalidate our previously cached results, so we have to re-memoize things.
;End Function
;'To modify a Position name is to either add the modifier, or if the modifier is
;'present, remove it from the Position name.  It's mutually exclusive, an XOR
;'property.
;Private Function modify(position As String, modifier As String) As String
;
;position = PositionToPolicy(position)
;If Not hasModifier(position, modifier) Then
;    modify = position & "_" & modifier
;Else
;    modify = Mid(position, 1, InStr(1, position, "_") - 1)
;    modify = modify & "_" & modifier 'Replace(Position, "_" & modifier, vbnullstring)
;End If
;
;End Function
;Private Function hasModifier(position As String, modifier As String) As Boolean
;'hasModifier = InStr(1, Position, modifier) > 0
;hasModifier = InStr(1, position, "_") > 0
;End Function
;'Insert a node that defines the start point in the lifecycle for deployment.  By default, if there is
;'no Deployable node in the policy, then this policy will not deploy units.
;Public Function setDeployableStart(cycletime As Single, Optional policy As TimeStep_Policy) As TimeStep_Policy
;If policy Is Nothing Then Set policy = Me
;Set setDeployableStart = insertModifier(cycletime, policy, "Deployable")
;End Function
;'insert a node that defines the stop point in the lifecycle for deployment.
;Public Function setDeployableStop(cycletime As Single, Optional policy As TimeStep_Policy) As TimeStep_Policy
;If policy Is Nothing Then Set policy = Me
;Set setDeployableStop = insertModifier(cycletime, policy, "NotDeployable")
;End Function
;Public Function setDeployable(tstart As Single, tfinal As Single, _
;                                        Optional policy As TimeStep_Policy) As TimeStep_Policy
;If policy Is Nothing Then Set policy = Me
;Set setDeployable = setDeployableStart(tstart, policy)
;Set setDeployable = setDeployableStop(tfinal, policy)
;clearmemo
;End Function
;
;Private Sub clearmemo()
;If memoize.count > 0 Then
;    Set memoize = New Dictionary
;End If
;
;End Sub
;'does not clone subscribers...
;Public Function clone() As TimeStep_Policy
;Set clone = New TimeStep_Policy
;With clone
;    .name = name
;    .cyclelength = cyclelength
;    .EndIndex = EndIndex
;    .endstate = endstate
;    Set .PositionGraph = cloneGraph(PositionGraph)
;    .maxbog = maxbog
;    .maxdwell = maxdwell
;    .mindwell = mindwell
;    .overlap = overlap
;    .startdeployable = startdeployable
;    .StartIndex = StartIndex
;    .startstate = startstate
;    .stopdeployable = stopdeployable
;    'clone subscribers? no for now....
;End With
;
;End Function
;
;
;
;Private Sub Class_Terminate()
;Set PositionGraph = Nothing 'DIRECTED graph, where edges are Position transitions, weights are transition time,
;'Node Data describes unit's state (bogging/dwelling, etc.)
;Set subscribers = Nothing
;
;Set ptr = Nothing
;Set memoize = Nothing
;End Sub
;
;'**********************IRotationPolicy Implementations*******************************************
;'************************************************************************************************

;Private Function IRotationPolicy_AtomicName() As String
;IRotationPolicy_AtomicName = name
;End Function
;
;Private Property Get IRotationPolicy_BOGBudget() As Long
;If bogbudget < maxbog Then
;    IRotationPolicy_BOGBudget = maxbog
;Else
;    IRotationPolicy_BOGBudget = bogbudget
;End If
;End Property
;
;Private Function IRotationPolicy_getActivePolicy() As IRotationPolicy
;Set IRotationPolicy_getActivePolicy = Me
;End Function
;
;'Tom added 12 July 2012
;'Atomic policies never change.  Easy.
;Private Function IRotationPolicy_getPolicy(period As String) As IRotationPolicy
;Set IRotationPolicy_getPolicy = Me
;End Function
;
;Private Property Let IRotationPolicy_name(ByVal RHS As String)
;name = RHS
;End Property
;
;Private Property Get IRotationPolicy_name() As String
;IRotationPolicy_name = name
;End Property
;
;Private Function IRotationPolicy_nextposition(position As String) As String
;IRotationPolicy_nextposition = nextposition(position)
;End Function
;

  
;''TOM Remove July 12 2012
;Private Sub IRotationPolicy_OnPeriodChange(period As String)
;'do nothing.
;End Sub
;
;Private Property Let IRotationPolicy_overlap(ByVal RHS As Long)
;overlap = RHS
;End Property
;
;Private Property Get IRotationPolicy_overlap() As Long
;IRotationPolicy_overlap = overlap
;End Property
;
;Private Property Set IRotationPolicy_PositionGraph(ByVal RHS As GenericGraph)
;Set IRotationPolicy_PositionGraph = RHS
;End Property
;
;Private Property Get IRotationPolicy_PositionGraph() As GenericGraph
;Set IRotationPolicy_PositionGraph = PositionGraph
;End Property
;
;Private Function IRotationPolicy_previousPosition(position As String) As String
;IRotationPolicy_previousPosition = previousPosition(position)
;End Function
;
;Private Function IRotationPolicy_setDeployable(tstart As Single, tfinal As Single, Optional policy As IRotationPolicy) As IRotationPolicy
;Set IRotationPolicy_setDeployable = setDeployable(tstart, tfinal, policy)
;End Function
;
;Private Function IRotationPolicy_setDeployableStart(cycletime As Single, Optional policy As IRotationPolicy) As IRotationPolicy
;Set IRotationPolicy_setDeployableStart = setDeployableStart(cycletime, policy)
;End Function
;
;Private Function IRotationPolicy_setDeployableStop(cycletime As Single, Optional policy As IRotationPolicy) As IRotationPolicy
;Set IRotationPolicy_setDeployableStop = setDeployableStop(cycletime, policy)
;End Function
  
;Private Property Let IRotationPolicy_StartDeployable(ByVal RHS As Single)
;startdeployable = RHS
;End Property
;
;Private Property Get IRotationPolicy_StartDeployable() As Single
;IRotationPolicy_StartDeployable = startdeployable
;End Property
;
;Private Property Let IRotationPolicy_StartIndex(ByVal RHS As Long)
;StartIndex = RHS
;End Property
;
;Private Property Get IRotationPolicy_StartIndex() As Long
;IRotationPolicy_StartIndex = StartIndex
;End Property
;
;Private Property Let IRotationPolicy_StartState(ByVal RHS As String)
;startstate = RHS
;End Property

;Private Property Get IRotationPolicy_StartState() As String
;IRotationPolicy_StartState = startstate
;End Property
;
;Private Property Let IRotationPolicy_StopDeployable(ByVal RHS As Single)
;stopdeployable = RHS
;End Property
;
;Private Property Get IRotationPolicy_StopDeployable() As Single
;IRotationPolicy_StopDeployable = stopdeployable
;End Property
;
;Private Sub IRotationPolicy_Subscribe(unit As TimeStep_UnitData)
;Subscribe unit
;End Sub
;
;Private Property Set IRotationPolicy_subscribers(ByVal RHS As Scripting.IDictionary)
;Set subscribers = RHS
;End Property
;
;Private Property Get IRotationPolicy_subscribers() As Scripting.IDictionary
;Set IRotationPolicy_subscribers = subscribers
;End Property
;
;Private Function IRotationPolicy_TransferTime(startPosition As String, endPosition As String) As Single
;IRotationPolicy_TransferTime = TransferTime(startPosition, endPosition)
;End Function
;
;Private Sub IRotationPolicy_AddPosition(PosName As String, PosState As String, ParamArray MoreNodes() As Variant)
;AddPosition PosName, PosState, MoreNodes
;End Sub
;
;Private Sub IRotationPolicy_AddRoute(Start As String, destination As String, TransferTime As Single)
;AddRoute Start, destination, TransferTime
;End Sub
;
)

(comment :supply :deferred          
;------------Deferred------------
;Public Function SupplyfromExcel(policystore As TimeStep_ManagerOfPolicy, 
;   parameters As TimeStep_Parameters, behaviors As TimeStep_ManagerOfBehavior,
;      ctx As TimeStep_SimContext, Optional ensureghost As Boolean) 
;        As TimeStep_ManagerOfSupply
;Dim tbl As GenericTable
;Dim gunit As TimeStep_UnitData
;
;Set SupplyfromExcel = New TimeStep_ManagerOfSupply
;'TODO -> turn this into a function.
;UnitsFromSheet "SupplyRecords", SupplyfromExcel, behaviors, parameters, 
;   policystore, ctx
;
;If ensureghost Then
;    If Not SupplyfromExcel.hasGhosts Then
;        Set gunit = createUnit("Auto", "Ghost", "Anything", "Ghost", 0, "Auto",
;          parameters, policystore)
;        Set gunit = associateUnit(gunit, SupplyfromExcel, ctx)
;        registerUnit SupplyfromExcel, behaviors, gunit, True, ctx
;        Debug.Print "Asked to do requirements analysis without a ghost, " & _
;            "added Default ghost unit to unitmap in supplymanager."
;    End If
;End If
;

;Public Sub fromExcel(supplystore As TimeStep_ManagerOfSupply, policystore As 
;        TimeStep_ManagerOfPolicy, parameters As TimeStep_Parameters, 
;           behaviors As TimeStep_ManagerOfBehavior, ctx As TimeStep_SimContext
;              Optional ensureghost As Boolean)
;
;Dim gunit As TimeStep_UnitData
;
;UnitsFromSheet "SupplyRecords", supplystore, behaviors, parameters, 
;   policystore, ctx
;
;If ensureghost Then
;    If Not supplystore.hasGhosts Then
;        Set gunit = createUnit("Auto", "Ghost", "Anything", "Ghost", 0, "Auto",
;                                    parameters, policystore)
;        'Decoupled
;        Set gunit = associateUnit(gunit, supplystore, ctx)
;        'decoupled
;       Set supplystore = registerUnit(supplystore, behaviors, gunit, True, ctx)
;        Debug.Print "Asked to do requirements analysis without a ghost, " & _
;            "added Default ghost unit to unitmap in supplymanager."
;    End If
;End If
;
;End Sub
;Public Sub UnitsFromSheet(sheetname As String, supplystore As 
;      TimeStep_ManagerOfSupply, behaviors As TimeStep_ManagerOfBehavior,
;          parameters As TimeStep_Parameters, policystore As 
;                TimeStep_ManagerOfPolicy, ctx As TimeStep_SimContext)
;Dim tbl As GenericTable
;
;Set tbl = New GenericTable
;tbl.FromSheet Worksheets(sheetname)
;
;MarathonOpFactory.unitsFromTable tbl, supplystore, behaviors, parameters, 
;   policystore, ctx
;
;
;End Sub
;Public Sub UnitsFromDictionary(unitrecords As Dictionary, 
;    parameters As TimeStep_Parameters, behaviors As TimeStep_ManagerOfBehavior,
;       policystore As TimeStep_ManagerOfPolicy, supplystore As 
;           TimeStep_ManagerOfSupply, ctx As TimeStep_SimContext)
;'Decouple
;UnitsFromRecords unitrecords, parameters, behaviors, policystore,
;     supplystore, ctx
;
;End Sub
)

(comment :policyio :deferred
;-----------DEFERRED--------------------

;Returns a policystore object initialized from default tables in Excel.  
;Mostly for compatibility.

;Public Function policyStoreFromExcel() As TimeStep_ManagerOfPolicy
;Set policyStoreFromExcel = tablesToPolicyStore(getTable("RelationRecords"), _
;                             getTable("PeriodRecords"), _
;                             getTable("PolicyRecords"), _
;                             getTable("CompositePolicyRecords"))
;End Function
;Public Function policyStoreFromFiles() As TimeStep_ManagerOfPolicy
;Dim tbl
;Dim tblist As Collection
;Dim p As String
;p = ActiveWorkbook.path & "\"
;
;Set tblist = list("RelationRecords", "PeriodRecords", "PolicyRecords", 
;                  "CompositePolicyRecords")
;For Each tbl In tblist
;    TableLib.saveTable getTable(CStr(tbl)), p & "\" & CStr(tbl) & ".json"
;Next tbl
;                             
;Set policyStoreFromFiles = 
;       tablesToPolicyStore(getTable(p & "RelationRecords" & ".json"), _
;                           getTable(p & "PeriodRecords" & ".json"), _
;                           getTable(p & "PolicyRecords" & ".json"), _
;                           getTable(p & "CompositePolicyRecords" & ".json"))
;
;End Function

;Public Function folderToPolicyStore(folderpath As String) 
;   As TimeStep_ManagerOfPolicy
;Dim tbl
;Dim tblist As Collection
;Dim p As String
;p = folderpath
;
;Set tblist = list("RelationRecords", "PeriodRecords", "PolicyRecords", 
;                                     "CompositePolicyRecords")
;Set folderToPolicyStore = 
;    tablesToPolicyStore(getTable(p & "RelationRecords" & ".json"),
;                        getTable(p & "PeriodRecords" & ".json"), _
;                        getTable(p & "PolicyRecords" & ".json"), _
;                        getTable(p & "CompositePolicyRecords" & ".json"))
;                             
;End Function
)
         
         
    

(comment :marathon.sim.fill 
         
;#Legacy Code#
;Function sourceDemand
; (supplystore As TimeStep_ManagerOfSupply, parameters As TimeStep_Parameters, _
;   fillstore As TimeStep_ManagerOfFill, ctx As TimeStep_SimContext, _
;    policystore As TimeStep_ManagerOfPolicy, t As Single, _
;      demand As TimeStep_DemandData, sourcetype As String, _
;        Optional supplybucket As Dictionary, Optional phase As FillPhase) 
;          As Boolean
;
;Dim rule As String
;Dim fillList As Dictionary
;Dim fill As TimeStep_Fill
;Dim unit As TimeStep_UnitData
;Dim deployer
;
;rule = deriveRule(demand, fillstore)
;
;
;this is new, we now dispatch on the type of fill....
;With fillstore.fillfunction 
;    If .exists(rule) Then 'we can try to fill this demand.
;        .Query rule, demand.demandgroup, demand.name, supplybucket, phase, 
;           supplystore
;        'This will take more....i think context for sure.

;        'try to get the required fills
;        Set fillList = .take(demand.required, phase)  
;        If fillList.count > 0 Then
;            'found units to fill required...
;            If fillList.count = demand.required Then sourceDemand = True 
;            For Each deployer In fillList                     
;                Set fill = fillList(deployer)
;               'this may get to be too big at some point, maybe not.....
;                recordFill fill, fillstore 
;                Set unit = fill.source
;                'Decoupled
;                SimLib.triggerEvent FillDemand, demand.name, unit.name, 
;                   "Filled Demand ", , ctx
                    
;                If unit.src = "Ghost" Then
;                    If unit.followoncode = vbNullString Then
;                        'Decoupled
;                    SimLib.triggerEvent GhostDeployed, demand.src,  demand.src, 
;                       "Filled demand with ghost", "normal", ctx
;                    Else
;                        'Decoupled
;                    SimLib.triggerEvent GhostDeployed, demand.src, demand.src, 
;                        "Ghost followed on to another demand", "followon", ctx
;                    End If
;                End If               
;                'TOM Change 30 Aug 2012
                          
;  MarathonOpSupply.deployUnit supplystore, ctx, parameters, policystore, _
;   unit, t, fill.quality, demand, unit.policy.maxbog, fillstore.Fills.count, _
;         fill, parameters.IntervalToDate(t), unit.followoncode <> vbNullString
;  Next deployer
; End If
;End If
;End With
;
;
;End Function



;;THIS IS MOST LIKELY OBSOLETE.

;TOM Change 27 SEp 2012 -> allow fencing of supply via tags...We pass 
;information to the comparer if the unit is fenced to the relative demand or 
;demand group.
;Public Function isInsideFence(uic As TimeStep_UnitData, demandname As String, 
;        followoncode As String, tags As GenericTags) As Boolean
;
;With tags
;    isInsideFence = .hasTag(uic.name, followoncode) Or 
;                    .hasTag(uic.name, demandname)
;End With
;
;End Function

;TOM Change 27 SEp 2012 -> allow fencing of supply via tags...We pass 
;information to the comparer if the unit is fenced to the relative demand or 
;demand group.  If a unit is fenced to a different demand or group, we return
;false.
;Public Function isOutsideFence(uic As TimeStep_UnitData, demandname As String, 
;   followoncode As String, tags As GenericTags) As Boolean
;
;With tags
;    If .hasTag("fenced", uic.name) Then
;        isOutsideFence = Not isInsideFence(uic, demandname, followoncode, tags)
;    Else
;        'by default, units are included if they have no fencing rules.
;        isOutsideFence = False 
;    End If
;End With
;End Function

)

(comment :marathon.sim.fill :construction

;'Constructor for building fill stores from component pieces.
;'Note, the fill store has a fillgraph, a fill function, and a supply generator.
;Public Function makeFillStore(fillgraph As TimeStep_FillGraph, fillfunction As 
;   TimeStep_FillFunction, generator As TimeStep_SupplyGenerator) 
;         As TimeStep_ManagerOfFill
;
;Set makeFillStore = New TimeStep_ManagerOfFill
;With makeFillStore
;    Set .fillgraph = fillgraph
;    Set .fillfunction = fillfunction
;End With
;End Function
;
;
;Process that encapsulates creating a new fillstore from coredata, appending 
;the fillstore to the core data, and then returning a scoped set of core data, 
;where the supply and demand have been reduced according to the relations
;embodied by the fillgraph.
;Assumes simState has valid supply, demand, and policystore instances 
;(i.e. they've been initialzed, probably from tables). Returns updated simState.
;Public Function simStateToScopedSimState(simstate As TimeStep_SimState, 
;    Optional generator As TimeStep_SupplyGenerator) As TimeStep_ManagerOfFill
;
;Dim ff As TimeStep_FillFunction
;Dim fg As TimeStep_FillGraph
;Dim fs As TimeStep_ManagerOfFill
;
;With simstate
;    Set fg = composeFillGraph(.supplystore, .demandstore, .policystore)
;    If generator Is Nothing Then _
;        Set generator = makeSupplyGenerator(simstate, , , 
;             simstate.parameters.getKey("DefaultSupplyPriority") = "RCPreSurge")
;    Set ff = makeFillFunction("FillFunction", .supplystore, fg, .parameters, 
;                                .context, generator)
;    Set fs = makeFillStore(fg, ff, generator)
;End With
;
;Set simstate.fillstore = fs
;'Scopes the data as a final step, since we have a handle on the fillgraph.
;Set simstate = scopeSimState(fg, simstate)
;Set ff = Nothing
;Set fg = Nothing
;Set fs = Nothing
;
;End Function
;'Performs a large scoping operation on core data, using any fillgraph.
;Public Function scopeSimState(fillgraph As TimeStep_FillGraph, 
;                          simstate As TimeStep_SimState) As TimeStep_SimState
;scope fillgraph.reducedgraph, simstate.fillstore, simstate.parameters, 
;                simstate.supplystore, simstate.demandstore, simstate.context
;Set scopeSimState = simstate
;End Function

;'Composes pre-built stores of supply, demand, and policy into a fillgraph.
;Public Function composeFillGraph(supplystore As TimeStep_ManagerOfSupply, 
;   demandstore As TimeStep_ManagerOfDemand, 
;      policystore As TimeStep_ManagerOfPolicy) As TimeStep_FillGraph
;Set composeFillGraph = BuildFillGraph(New TimeStep_FillGraph, supplystore, 
;                           demandstore, policystore)
;End Function
;'Composes tables defining supply, demand, and relation records into a fillgraph
;Public Function tablesToFillgraph(sourcesTbl As GenericTable, 
;  sinksTbl As GenericTable, relationsTbl As GenericTable) As TimeStep_FillGraph
;Set tablesToFillgraph = FillGraphFromTables(New TimeStep_FillGraph, sourcesTbl,
;                             sinksTbl, relationsTbl)
;End Function
;
;'Produces a new fill function from inputs.
;Public Function makeFillFunction
; (nm As String, supplystore As TimeStep_ManagerOfSupply, _
;    graph As IGraph, parameters As TimeStep_Parameters, _
;       context As TimeStep_SimContext, _
;           generator As TimeStep_SupplyGenerator) As TimeStep_FillFunction
;
;Set makeFillFunction = New TimeStep_FillFunction
;With makeFillFunction
;    .name = nm
;    'Decoupled
;    Set .parent = supplystore
;    'Eh...this is dubious....TODO -> separate further.
;    Set .FillRules = .AddFill(graph)
;    'Decoupled
;    Set .generator = generator
;End With
;
;End Function
;
;'A constructor for supply generators.  Currently, there's only one kind of 
;'generator, based off the legacy supply system.  We'll have to fix this.  
;'Extensibility via new generators is pretty fundamental. Fergusonmode is a 
;'legacy hack that basically sets a flag to change the prioritization scheme for
;'pre surge periods.  It came about when we needed to use a different 
;'prioritization scheme.... Maybe we should have complex priotization schemes, 
;'just like we have composite policies.
;Public Function makeSupplyGenerator _
;    (simstate As TimeStep_SimState, Optional gpolicy As TimeStep_Policy, _
;        Optional gcompo As String, Optional fergusonmode As Boolean,
;           Optional comparer As IComparator) As TimeStep_SupplyGenerator
;
;Set makeSupplyGenerator = New TimeStep_SupplyGenerator
;
;With makeSupplyGenerator
; 'TODO -> this is just a temporary decoupling.  I need to extract out the 
; 'supply generator further.  Parent is just pointing at a supplystore now 
; '(not unlike a partially applied function).Still, the dependencies kinda suck.
;    'Decoupled
;    'Set .parent = supplystore
;    Set .simstate = simstate
;    'Decoupled
;    If gpolicy Is Nothing Then 
;       Set gpolicy = simstate.policystore.policies("Ghost365_45")
;    If gcompo = vbNullString Then gcompo = "Ghost" 'no component
;    Set .ghostpolicy = gpolicy
;    .ghostcompo = gcompo
;    'Decoupled
;    Set .tags = simstate.supplystore.tags
;    'Too coupled.  Need to fix unit comparison to allow general prioritization.
;    Set .comparer = New TimeStep_ComparerUnit 
;    If fergusonmode Then .comparer.RCpresurgePreference = True
;End With
;
;End Function
;'TODO ->  Do a better job separating concerns here... Building a fill graph and
;'viewing the intermediate results are likely orthogonal...
;'Accumulate a fill graph from supplymanager, policymanager and demands...
;Public Function BuildFillGraph
;    (sourcegraph As TimeStep_FillGraph, _
;       supplystore As TimeStep_ManagerOfSupply, _
;          demandstore As TimeStep_ManagerOfDemand, _
;             policystore As TimeStep_ManagerOfPolicy) As TimeStep_FillGraph
;
;Set BuildFillGraph = sourcegraph
;
;With sourcegraph
;    Set BuildFillGraph = .fromsupply(supplystore)
;    Set BuildFillGraph = .fromPolicy(policystore)
;    Set BuildFillGraph = .FromDemand(demandstore)
;End With
;
;'currently we build this into the fillgraph build process.  We want to know
;'where there are islands....
;sourcegraph.decompose

;End Function
;
;'TODO ->  Do a better job separating concerns here... Building a fill graph and
; viewing the intermediate results are likely orthogonal...
;'Accumulate a fill graph from supplymanager, policymanager and demands...
;Public Function FillGraphFromTables
;          (sourcegraph As TimeStep_FillGraph, _
;             supply As GenericTable, _
;                demand As GenericTable, _
;                   policy As GenericTable) As TimeStep_FillGraph
;
;Set FillGraphFromTables = sourcegraph
;
;With sourcegraph
;    Set FillGraphFromTables = .FromSourceTable(supply)
;    Set FillGraphFromTables = .fromRelationTable(policy)
;    Set FillGraphFromTables = .FromSinkTable(demand)
;End With
;
;'Tom note -> these are debug tools.
;'renderGraph "FullGraph", BuildFillGraph.graph
;'currently we build this into the fillgraph build process.  We want to know
;'where there are islands....
;sourcegraph.decompose
;
;End Function
;
;'This is the simplest initializer for building and initializing a fill store.  
;'Closest to the legacy stuff as well.
;Public Function fillStoreFromTables(simstate As TimeStep_SimState, 
;   sources As GenericTable, sinks As GenericTable, relations As GenericTable) 
;      As TimeStep_ManagerOfFill
;Dim fg As TimeStep_FillGraph
;Dim ff As TimeStep_FillFunction
;Dim sg As TimeStep_SupplyGenerator
;
;Set fg = New TimeStep_FillGraph
;Set fg = FillGraphFromTables(fg, sources, sinks, relations)
;
;Set sg = makeSupplyGenerator(simstate)
;Set ff = makeFillFunction("FillFunction", simstate.supplystore, fg.graph, 
;                              simstate.parameters, simstate.context, sg)
;Set fillStoreFromTables = makeFillStore(fg, ff, sg)
;
;Set sg = Nothing
;Set ff = Nothing
;Set fg = Nothing
;
;End Function
;Public Function staticGraph(supply As GenericTable, _
;                              demand As GenericTable, _
;                                policy As GenericTable) As TimeStep_FillGraph
;
;
;Set staticGraph = New TimeStep_FillGraph
;
;With staticGraph
;    Set staticGraph = .FromSourceTable(supply)
;    Set staticGraph = .fromRelationTable(policy)
;    Set staticGraph = .FromSinkTable(demand)
;End With
;
;'Tom note -> these are debug tools.
;'renderGraph "FullGraph", BuildFillGraph.graph
;'currently we build this into the fillgraph build process.  We want to know
;'where there are islands....
;staticGraph.decompose
;
;
;End Function
;'Sets flags on the fillstore to indicate all graphs should be rendered with 
;'GraphViz
;Public Sub renderAllGraphs(fillstore As TimeStep_ManagerOfFill)
;fillstore.rendergraphs = True
;fillstore.allgraphs = True
;End Sub
;'add the keys from outofscope to the outofscope in fillstore.
;Private Sub scopeOut(fs As TimeStep_ManagerOfFill, outofscope As Dictionary)
;Dim k
;Set fs.outofscope = SetLib.union(fs.outofscope, outofscope)
;End Sub
;'TOM change 24 Mar 2011 -> Utilize the reduced fillgraph to determine which 
;'elements of supply and demand should be scoped out of the study, remove these 
;'elements from demand and supply.
;Public Sub scope
;    (reduced As GenericGraph, fillstore As TimeStep_ManagerOfFill, 
;       parameters As TimeStep_Parameters, _
;          supplystore As TimeStep_ManagerOfSupply, 
;             demandstore As TimeStep_ManagerOfDemand, _
;                ctx As TimeStep_SimContext, Optional csv As Boolean)
;Dim island
;Dim islands As Dictionary
;Dim strm As IRecordStream
;Dim scoperec As GenericRecord
;Dim msg As String
;Dim src As String
;Dim isle
;Dim res As Dictionary
;
;'TOM change 26 Ovt 2012
;'Set islands = findIslands(reduced, fillstore)
;Set islands = findIslands(reduced)
;scopeOut fillstore, islands("OutOfScope")
;
;'Decoupled
;With parameters.outofscope
;
;    'todo....check this, seems hard coded.
;    If csv Then
;        Set strm = New Streamer_CSV
;    Else
;        Set strm = New Streamer_xl
;    End If
;    
;    Set scoperec = New GenericRecord
;    scoperec.AddField "TimeStamp", Now()
;    scoperec.AddField "SRC", vbNullString
;    scoperec.AddField "Reason", vbNullString
;
;    'TOM Change 20 April 2012
;    'Application.ScreenUpdating = False
;    DisableScreenUpdates
;
;    If csv Then
;        strm.init scoperec.fieldnames, "OutOfScope.csv"
;    Else
;        strm.init scoperec.fieldnames, "OutOfScope"
;    End If
;
;    For Each isle In islands("Supply")
;        src = replace(CStr(isle), "SOURCE_", vbNullString)
;        .add src, "No Demand"
;        scoperec.UpdateField "SRC", src
;        scoperec.UpdateField "Reason", "No Demand"
;        strm.writeGeneric scoperec
;    Next isle
;    
;    For Each isle In islands("Demand")
;        src = replace(CStr(isle), "FILLRULE_", vbNullString)
;        .add src, "No Supply"
;        scoperec.UpdateField "SRC", src
;        scoperec.UpdateField "Reason", "No Supply"
;        strm.writeGeneric scoperec
;        
;    Next isle
;    
;    strm.Terminate
;
;    If csv Then
;        Set strm = New Streamer_CSV
;        strm.init scoperec.fieldnames, "InScope.csv"
;
;    Else
;        Set strm = New Streamer_xl
;        strm.init scoperec.fieldnames, "InScope"
;    End If
;
;    Set res = islands("InScope")
;    For Each isle In res
;        If res(isle) = "Demand" Then
;            src = replace(CStr(isle), "FILLRULE_", vbNullString)
;            If Not parameters.SRCsInScope.exists(src) Then _
;                parameters.SRCsInScope.add src, "Demand"
;        ElseIf res(isle) = "Supply" Then
;            src = replace(CStr(isle), "SOURCE_", vbNullString)
;            If Not parameters.SRCsInScope.exists(src) Then _
;                parameters.SRCsInScope.add src, "Supply"
;        Else
;            Err.Raise 101, , "unknown characterization of fill validity"
;        End If
;        scoperec.UpdateField "SRC", src
;        scoperec.UpdateField "Reason", res(isle)
;        strm.writeGeneric scoperec
;    Next isle
;
;    strm.Terminate
;End With
;
;'TOM Change 20 April 2012
;EnableScreenUpdates
;'Application.ScreenUpdating = True
;
;Set strm = Nothing
;
;msg = "FillManager found " & islands("Supply").count & " Unused Supply Sources"
;'Decoupled
;SimLib.triggerEvent ScopedSupply, fillstore.name, supplystore.name, msg, , ctx
;'Tom change 16 June; inserted True into scope, removes units from supply.
;'Decoupled
;MarathonOpSupply.scopeSupply supplystore, islands("Supply"), True
;msg = "FillManager found " & islands("Demand").count & 
;          " Unfillable Demand Sinks"
;'Decoupled
;SimLib.triggerEvent ScopedDemand, fillstore.name, demandstore.name, msg, , ctx
;'Tom change; inserted True into scope, removes demands from demand.
;'Decoupled
;MarathonOpDemand.scopeDemand demandstore, islands("Demand"), True
;
;End Sub
;'Discovers islands in a graph, annotating them in the fillstore and in/out of 
;'scope rules.
;Public Function findIslands(source As GenericGraph, 
;                Optional fillstore As TimeStep_ManagerOfFill) As Dictionary
;Dim res As Dictionary
;Dim islands As Dictionary
;Dim outofscope As Dictionary
;Dim dependencies As Dictionary
;Dim nd
;Dim grph
;Dim subgraph As GenericGraph
;
;Set res = New Dictionary
;
;'TOM Change 26 Oct 2012 -> making this more generic.
;'Rather than needing a fillstore as an arg, all we really need is
;'to capture the items that are OutOfScope.  We can do that in the function
;'and return OutOfScope as an entry in the result, which can then be processed
;'externally in a fillstore.
;
;'res.add "Supply", New Dictionary
;'res.add "Demand", New Dictionary
;'res.add "InScope", New Dictionary
;Set outofscope = New Dictionary
;'res.add "OutOfScope", outofscope
;Set dependencies = New Dictionary
;
;Set res = newdict("Supply", New Dictionary, _
;                  "Demand", New Dictionary, _
;                  "InScope", New Dictionary, _
;                  "OutOfScope", outofscope, _
;                  "Dependencies", dependencies)
;
;'TOM Change 26 Oct 2012
;'With fillstore
;    For Each grph In source.subgraphs
;        Set subgraph = source.subgraphs(grph)
;        'add all the SRCs from this equivalence class
;        dependencies.add CStr(grph), getDependencies(subgraph)
;        If subgraph.nodes.count = 1 Then 'it's an island.
;            For Each nd In subgraph.nodes
;                Set islands = res(islandType(CStr(nd)))
;                islands.add CStr(nd), 0
;                'TOM change 26 OCt 2012
;                '.outofscope.add CStr(nd), 0
;                outofscope.add CStr(nd), 0
;                source.RemoveNode CStr(nd) 'eliminate the island
;                source.subgraphs.Remove (grph) 'eliminate the subgraph
;                Set subgraph = Nothing
;            Next nd
;        Else 'the source is in scope...
;            Set islands = res("InScope")
;            For Each nd In subgraph.nodes
;                islands.add CStr(nd), islandType(CStr(nd))
;            Next nd
;        End If
;    Next grph
;'End With
;
;Set findIslands = res
;Set res = Nothing
;
;End Function
;Private Function getDependencies(gr As IGraph) As Dictionary
;Dim nd
;Dim x As String
;
;Set getDependencies = New Dictionary
;For Each nd In GraphLib.getNodes(gr)
;    x = translateRule(CStr(nd))
;    If Not (getDependencies.exists(x)) Then
;        getDependencies.add x, 0
;    End If
;Next nd
;    
;End Function
;Public Function translateRule(ByRef inrule As String) As String
;Dim tmp
;tmp = Split(inrule, "_")
;If UBound(tmp, 1) = 1 Then
;    translateRule = tmp(1)
;Else
;    Err.Raise 101, , 
;      "Irregular rule :" & inrule & " should be delimited by a single _ "
;End If
;End Function
;'Aux function to describe the type of island, whether a source or a sink.
;Private Function islandType(nodename As String) As String
;If InStr(1, nodename, "SOURCE") > 0 Then
;    islandType = "Supply"
;ElseIf InStr(1, nodename, "FILLRULE") > 0 Then
;    islandType = "Demand"
;Else
;    Err.Raise 101, , "Island is neither supply nor demand"
;End If
;
;End Function
;
;'Public Sub fromExcel()
;''Decouple
;'init parent
;'End Sub
;
;'Public Sub FromTables(sources As GenericTable, sinks As GenericTable, 
;                            relations As GenericTable)
;''Decouple
;'init parent, True, sources, sinks, relations
;'End Sub
;'
)
         
         
(comment :marathon.sim.demand :notes
         
;TOM Note 20 May 2013   -> We can probably generalize the explicit process
;denoted below.  There are, arbitrarily, 1..n phases of filling, this is one 
;possible ordering.

;TOM Change 21 Sep 2011 -> this is the first pass we engage in the fill process.
;The intent is to ensure that we bifurcate our fill process, forcing the 
;utilization of follow-on units.  We split the fill process into 2 phases...
;Phase 1:  Find out which demands in the UnfilledQ(FillRule) are eligible for 
;          Follow-On supply. If a demand is eligible for follow-on supply 
;          (it has a corresponding Group in FollowOns in supplymanager.
;          Add the demand to a new priority queue for the fillrule (lexically 
;          scoped).  Basically, we;re getting a subset of eligible demands 
;          (by fillrule) from the existing unfilledQ for the fillrule.
;
;          Pop ALL Demands off the new priorityQ for the fillrule, trying to 
;          fill them using supply from the followonbuckets relative to the 
;          applicable demand group. (basically a heapsorted traversal)
;          This may result in some demands being filled.
;          If a demand is filled, we update its fill status (mutate the 
;          unfilledQ) just as in the normal fill routine.
;               If it;s not filled, we leave it alone.
;                       *Update -> thre problem is, our fill function, if 
;                                  allowed to make ghosts, will KEEP trying to 
;                                  fill, and effectively short circuit our other 
;                                  supply.  categories prevent the fill function
;                                  from making ghosts in phase1.
;             The biggest difference is that we DO NOT stop, or short-circuit
;             the fill process if we don't find follow-on supply.  We give every 
;             eligible demand a look. After phase 1 is complete, all demands 
;             that were eligible for follow-ons will have recieved follow-on
;             supply.  Any demands completely filled by follow-ons will have 
;             been eliminated from further consideration. Demands with 
;             requirements remaining are still in the unfilledQ for our normal, 
;             ARFORGEN-based fill.
;Phase 2:  This is the normal, ARFORGEN-based fill routine.  Anything left in 
;          the unfilledQ is processed as we did before.  This time, we should
;          only be looking at ARFORGEN supply (since our follow-on supply was
;          utilized in the Phase 1)  This phase is also known as hierarchical
;          fill.


  
;Going to redirect this sub to incorporate unfilledQ. The idea is to look for 
;the highest priority demand, by SRC . Specifically, we will be redirecting the 
;portion of the routine that finds "a demand" to be filled.
;In the previous algorithm, we just traversed thousands of demands until we 
;found one with a status of False, then tried to fill it.
;In this new algorithm, instead of a loop over each demand in the demanddata 
;array, we consult our bookeeping:
;For each independent set of prioritized demands (remember, we partition based 
;on substitutionjSRC keys), we use our UnfilledQ to quickly find unfilled 
;demands. UnfilledQ keeps demands in priority order for us, priority is stored 
;in demanddata.

;We only fill while we have feasible supply left.
;We check deployables too efficiently find feasible supply.
;Not currently kept in priority order .... could be converted easily enough
;though.
;If we fill a demand, we take it off the queue.
;If we fail to fill a demand, we have no feasible supply, thus we leave it on 
;the queue, stop filling, and proceed to the next independent set.
;After all independent sets have been processed, we're done.
;Tom Note 20 May 2013 -> Independent means we can do this in parallel.
)

(comment :marathon.sim.demand
;>>>>>>>>>>>>>>>>>>Deferred>>>>>>>>>>>>>>>>>>>>>>
;We'll port this when we  come to it....not sure we need it...

;Public Sub rescheduleDemands(demandstore As TimeStep_ManagerOfDemand, 
;                                ctx As TimeStep_SimContext)
;Static demand As TimeStep_DemandData
;Static itm
;With demandstore
;    For Each itm In .demandmap
;        Set demand = (.demandmap(itm))
;        scheduleDemand demand, demandstore, ctx
;        demand.reset
;    Next itm
;End With
;
;End Sub


;Public Sub fromExcel(state As TimeStep_SimState)
;
;Dim factory As TimeStep_EntityFactory
;Set factory = state.EntityFactory
;
;factory.DemandsFromTable getTable("DemandRecords"), state.demandstore
;;factory.AddDemandFromExcel strtcell.Worksheet, Me

;End Sub


;Public Sub NewDemand(name As String, tstart As Single, duration As Single, 
;  overlap As Single, primaryunit As String, quantity As Long, 
;  priority As Single, demandstore As TimeStep_ManagerOfDemand,
;  policystore As TimeStep_ManagerOfSupply, ctx As TimeStep_SimContext,
;  Optional operation As String, Optional vignette As String, 
;  Optional sourcefirst As String)
;Dim dem As TimeStep_DemandData
;
;Set dem = createDemand(name, tstart, duration, overlap, primaryunit, quantity, 
;   priority, demandstore.demandmap.count + 1, operation, vignette, sourcefirst)
;registerDemand dem, demandstore, policystore, ctx
;
;End Sub
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
)
         


(comment :marathon.sim.policy 
;----------DEFERRED-------
;Policy Store operations.....
;Perform a reduction over a list of default policies....adding each one to the
;policystore.

;Public Sub addDefaults(policystore As TimeStep_ManagerOfPolicy)
;Dim pol As TimeStep_Policy
;'register our default set of policies.
;addPolicies MarathonPolicy.DefaultArforgenPolicies, policystore
;'Additional policies added by Trudy F.
;addPolicies MarathonPolicy.TFPolicies, policystore 
;'Future Force Gen policies.
;addPolicies MarathonPolicy.FFGPolicies, policystore 
;
;For Each pol In policystore.policies
;    policystore.permanents.add CStr(pol.name), 0
;Next pol
;
;End Sub



;'TODO
;'clears subscribers from non-permanent policies. WTF? check what non-permanent 
;'policies means.
; POSSIBLY OBSOLETE 
;Public Sub resetPolicies(policystore As TimeStep_ManagerOfPolicy)
;Dim policy As IRotationPolicy
;Dim pol
;
;With policystore
;    For Each pol In .policies
;        If Not .permanents.exists(CStr(pol)) Then
;            Set policy = .policies(pol)
;            policy.subscribers.RemoveAll
;            .policies.Remove pol
;        End If
;    Next pol
;End With
;
;End Sub

;'reschedules the activation and deactivation events for known periods.
; POSSIBLY OBSOLETE 
;Public Sub resetPeriods(policystore As TimeStep_ManagerOfPolicy,
; ctx As TimeStep_SimContext)
;Dim per
;With policystore
;    For Each per In .periods
;        schedulePeriod .periods(per), ctx
;    Next per
;End With
;
;End Sub

;TODO -> relook this operation.  I think we might want it at some point...as it 
;stands, composite policies are built from atomics. This is an extension to the 
;append operation, which allows us to compose compositions....possibly of 
;compositions!  We define the composition operator 
;Public Function appendCompositeComposite(composite As TimeStep_PolicyComposite,
;  period As String, subPolicy As TimeStep_PolicyComposite) 
;     As TimeStep_PolicyComposite
;'
;'createComposite.addPolicy subPolicy, period
;'
;'
;'
;'Set appendCompositeComposite = composite
;End Function


;---------END DEFERRED-----


;------------------OBSOLETE
;'simple indexing function.  We index starting from 101....although we really 
;'don't have to out of necessity.  I might revisit this.  Vestigial and 
;inherited from old, poor designs.
;Public Function nextlocationID(locationcount As Long) As Long
;nextlocationID = locationcount + 101
;End Function


;Adds a sequential policy to the policystore.  Special, because we keep track 
;of composite policies for special consideration during management of the policy
;ontext.
;Public Sub addPolicySequential(ByRef policy As TimeStep_PolicySequential, 
;   policystore As TimeStep_ManagerOfPolicy)
;
;With policystore
;    If Not .policies.exists(policy.name) Then
;        .policies.add policy.name, policy
;    Else
;        Err.Raise 101, , "Policy already exists"
;    End If
;End With
;
;End Sub

;''TOM NOTE 7 Jun 2011 -> These are both vestigial functions.  We're not even 
;using locationID anymore.  Recommend scrapping them to cut down on the code 
;bloat.
;'Wrapper for getting our locationID
;Public Function locationID(locname As String, policystore As 
;          TimeStep_ManagerOfPolicy) As Long
;locationID = CLng(policystore.LocatiOnMap(locname))
;End Function
;
;'Wrapper for getting our LocationName assocated with a locationID from a 
;'policystore.
;Public Function LocationName(locationID As Long, policystore As 
;                                TimeStep_ManagerOfPolicy) As String
;LocationName = CStr(policystore.LocationIndex(locationID))
;End Function
)

(comment :marathon.data.period :deferred;--------------DEFERRED--------
;---IMPLEMENT LATER-----------
;Defines a period that exists over (potentially) multiple times.....
;Public Function periodWhen(startevent As Long, stopevent As Long, Optional eventsource As GenericObserver, Optional name As String) As GenericPeriodReactive
;Set periodWhen = New GenericPeriodReactive
;
;renames the period from default.
;If name <> vbNullString Then
;    periodWhen.name = name
;ElseIf periodWhen.name <> vbNullString Then
;    name = periodWhen.name
;Else
;    If exists(eventsource) Then name = randName("periodWhen", eventsource.clients)
;End If
;
;periodWhen.startevent = startevent
;periodWhen.stopevent = stopevent
;
;If exists(eventsource) Then
;    eventsource.Register name, periodWhen, startevent
;    If startevent <> stopevent Then eventsource.Register name, periodWhen, stopevent
;End If
;   
;End Function
;Defines a period that exists over every triggering of a single event, a very simple reactive period.
;Public Function periodEvery(startevent As Long, Optional eventsource As GenericObserver, _
;                                Optional name As String) As GenericPeriodReactive
;Set periodEvery = periodWhen(startevent, startevent, eventsource, name)
;End Function
;
;This is a simple test simulation to illustrate how to use periods in a simulation context.
;Assuming we have some rudimentary system that operates off of change in simulation time,
;and determines if a new period has become active, we broadcast the changing of periods and
;update some state to indicate current period.
;Simultaneously, we have another system that responds to changes in time.  Every time sample,
;if the time is odd, an odd event (defined inline) is triggered.  If an even event is encounted,
;an even event is fired.
;
;Public Sub periodtest()
;
;Dim t 'a simple time index.
;Dim sim As timestep_SimContext
;Dim simstate As Dictionary 'some simple simulation state.
;
;these periods are defined apriori.  A system consults them to trigger events.
;Dim lessthan10 As GenericPeriod 'a period defined for t <  10
;Dim morethan10 As GenericPeriod 'a period defined for t >= 10
;
;these periods are triggered by events
;Dim oddPeriod As GenericPeriodReactive 'a reactive period that is defined over odd time intervals.
;Dim evenPeriod As GenericPeriodReactive 'a reactive period that is defined over even time intervals.
;
;Dim reactiveLogger As TimeStep_ObserverSimpleLog
;Dim periodLogger As TimeStep_ObserverSimpleLog
;
;Define a new simulation context.  This provides a blank
;template for a simulation.
;Set sim = SimLib.makeContext
;
;map two event slots to indicate even flips, odd flips, and a Period Change.
;EventLib.addEvents sim.events.evtstream, eventList(1, "Odd", 2, "Even", 3, "Period Changed")
;
;set up two reactive periods that observe odd and even events:
;
;Odd period measures periods between every odd event on the
;simulation's event stream.
;Set oddPeriod = periodEvery(1, sim.events.evtstream.observer, "OddPeriod")
;
;Even period measures periods between every even event on the
;simulations' event stream.
;Set evenPeriod = periodEvery(2, sim.events.evtstream.observer, "EvenPeriod")
;
;Define an observer that listens for Period change events (defined
;elsewhere).
;Set periodLogger = New TimeStep_ObserverSimpleLog
;sim.AddListener "Schedule", periodLogger, newSet(3)
;
;watch for instances of the period, logging output to console.
;Set reactiveLogger = New TimeStep_ObserverSimpleLog 'a logger that echoes the reactive periods.
;
;observeEvent oddPeriod, reactiveLogger 'logger will log oddPeriod's unique events.
;observeEvent evenPeriod, reactiveLogger 'logger will log evenPeriod's unique events.
;
;define scheduled periods.  These are ignorant of events, they are purely data.
;Set lessthan10 = namedPeriod("t <  10", periodTo(10))
;Set morethan10 = namedPeriod("t >= 10", periodFrom(10))
;
;define a simulation state.
;Set sim.state = newdict("currentperiod", vbNullString, _
;                        "previousperiod", vbNullString)
;
;schedule some times to advance our simulation.
;This system will likely change, we don't have to schedule
;time necessarily, but it's how the earlier system worked.
;Will be overhauled.
;
;For Each t In intList(20)
;    SimLib.addTime CSng(t), sim
;Next t
;
;a simple system for managing periods.
;While SimLib.hasTimeRemaining(sim)
;    t = SimLib.advanceTime(sim)
;    
;    'Manage our scheduled periods, changing state if necessary.
;    With getState(sim) 'examine the periods in the state
;        If PeriodLib.intersectsPeriod(CSng(t), lessthan10) Then
;            .item("currentperiod") = lessthan10.name
;        ElseIf PeriodLib.intersectsPeriod(CSng(t), morethan10) Then
;            .item("currentperiod") = morethan10.name
;        End If
;        
;        If .item("currentperiod") <> .item("previousperiod") Then
;            SimLib.triggerEvent 3, .item("previousperiod"), .item("currentperiod"), _
;                    "changed periods", .item("currentperiod"), sim
;            .item("previousperiod") = .item("currentperiod")
;        End If
;    End With
;
;    If (t Mod 2) > 0 Then
;        SimLib.triggerEvent 1, "flipper", "sim", "flip!", , sim
;    Else
;        SimLib.triggerEvent 2, "flipper", "sim", "flip!", , sim
;    End If
;Wend
;        
;End Sub

;-------------OBSOLETE?--------------------------------------------------------
;Public Function makeReactivePeriod(startEvent As Long, stopEvent As Long, observer As GenericObserver, periodName As String) As IPeriod
;
;End Function
;19 May 2011 Tom Note -> This should be refactored.  Too coupled to Excel.
;Public Function sheetToPeriods(periodsheet As Worksheet, p As TimeStep_StorePolicy, _
;                                    context As timestep_SimContext) As Collection
;
;Dim j As Long
;Dim record()
;Dim source As range
;Dim rw As range
;Dim fields As Dictionary
;Dim per As GenericPeriod
;Set fields = New Dictionary
;
;Set source = periodsheet.Cells(1, 1)
;Set source = source.CurrentRegion
;Set rw = source.rows(1)
;record = rw.value
;For j = 1 To UBound(record, 2)
;    fields.add CStr(record(1, j)), j
;Next j
;
;Set periods = New Dictionary
;Set sheetToPeriods = New Collection
;
;Set source = source.offset(1, 0)
;Set source = source.resize(source.rows.count - 1, source.Columns.count)
;For Each rw In source.rows
;    record = rw.value
;    Set per = New GenericPeriod
;    With per
;        .name = record(1, fields("Name"))
;        If record(1, fields("from-day")) = "inf" Then
;            'Decouple
;            .from-day = inf
;        Else
;            .from-day = record(1, fields("from-day"))
;        End If
;
;        If record(1, fields("to-day")) = "inf" Then
;            'Decouple
;            .to-day = inf
;        Else
;            .to-day = record(1, fields("to-day"))
;        End If
;        'Replaced.
;        
;
;    End With
;    sheetToPeriods.add per
;Next rw
;
;End Function
)