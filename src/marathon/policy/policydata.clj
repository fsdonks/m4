(ns marathon.policy.policydata
  (:use [util.record :only [defrecord+]]
        [util.metaprogramming :only [keyvals->constants]]))

;need a protocol for policies...
(defprotocol IRotationPolicy 
  (atomic-name       [p])
  (bog-budget        [p])
  (get-active-policy [p])
  (get-policy        [p period])
  (policy-name       [p])
  (next-position     [p position])
  (on-period-change  [p period])
  (overlap           [p])
  (position-graph    [p]) ;if we have this, we can build the rest... 
  (previous-position [p position])
  (set-deployable    [p tstart tfinal])
  (set-deployable-start [p cycletime])
  (set-deployable-stop  [p cycletime])
  (start-deployable [p])
  (stop-deployable  [p])
  (start-state      [p])
  (subscribe        [p unit]) ;EXCTRICATE 
  (get-subscribers  [p]) ;EXCTRICATE
  (set-subscribers  [p xs]) ;EXCTRICATE
  (transfer-time    [p start-position end-position])
  (add-position     [p name state & more-nodes])
  (add-route        [p start destination transfer-time])
  (cycle-length     [p])
  (deployable?      [p position])
  (end-state        [p])
  (get-cycle-time   [p position])
  (get-policy-type  [p])
  (get-position     [p cycletime])
  (get-state        [p position])
  (deployable?      [p cycletime])
  (dwell?           [p position])
  (max-bog          [p])
  (max-dwell        [p])
  (max-mob          [p])
  (min-dwell        [p])
  (add-policy       [p policy & args]))


;Constants used for policy definition, among other things.  Imported from the 
;original VBA implementation for the policystore. 
;Might re-think this, for now it's a way of porting the existing implementation
(def policyconstants 
  {:Bogging "Bogging"
   :Dwelling "Dwelling"
   :BogDeployable "BoggingDeployable"
   :DwellDeployable "DwellingDeployable"
   :Deployable "Deployable"
   :AC12 "AC12" 
   :AC13 "AC13" 
   :RC14 "RC14" 
   :RC15 "RC15" 
   :AC11 "AC11"
   :RC11 "RC11"
   :RC12 "RC12"
   :GhostPermanent12 "GhostPermanent12"
   :GhostPermanent13 "GhostPermanent13"
   :GhostTransient12 "GhostTransient12"
   :GhostTransient13 "GhostTransient13"   
   :reset "Reset"
   :train "Train"
   :ready "Ready"
   :available "Available"
   :deployed "Deployed"
   :Overlapping "Overlapping"
   :SubSymbol  "{>"
   :EquivSymbol "="})
(keyvals->constants policyconstants) ;make the constants first class symbols.
;inherited from substitution rules, may be vestigial.
(keyvals->constants {:Equivalence :Equivalence :Substitution :Substitution})

;TODO -> extend protocols to policy and policycomposite..
;a structure for unit entity policies. 
(defrecord+ policy [[name "BlankPolicy"]
                    [cyclelength :inf] 
                    [mindwell 0]
                    [maxdwell :inf]
                    [maxbog :inf] 
                    [maxMOB :inf]
                    [recovery  90] 
                    [startdeployable 0] 
                    [stopdeployable :inf]
                    [positiongraph  nil]
                    [startstate :spawn]
                    [endstate :spawn]
                    [overlap 45]
                    [subscribers {}]]) 

(def empty-policy (make-policy))

;policies defined by more than one atomic policy.
(defrecord+ policycomposite [name 
                             subscribers ;probably drop this field....
                             activepolicy 
                             activeperiod
                             [policies {}]])

(def empty-composite-policy (make-policycomposite))
;
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



