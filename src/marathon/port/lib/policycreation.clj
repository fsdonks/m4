(ns marathon.port.policycreation)

'marathonpolicycreation
Option Explicit

'This is part of a reorganization of a lot of the embedded functionality in the early object oriented
'implementation for marathon.  All functions in this module either consume no arguments, to produce
'policies, or modify existing policies in some way.  Most are a port from TimeStep_Policy
Private Type routerec
  source As String
  dest As String
  distance As Single
End Type


Dim Key

Private statecache As Dictionary

Public Function create(positions As Dictionary, routes As Dictionary) As TimeStep_Policy
Set create = New TimeStep_Policy
End Function
Private Function addPositions(positions As Dictionary, targetpolicy As TimeStep_Policy) As TimeStep_Policy
Set addPositions = targetpolicy

With addPositions
    For Each Key In positions.keys
        .AddPosition CStr(Key), CStr(positions(Key))
    Next Key
End With
End Function
Private Function getRoute(routes As Dictionary, Key As String) As routerec
'Dim rt As Collection
'With getRoute

End Function
'expects a collection of triples, ((from to cost)...)
Public Function addRoutes(routes As Collection, targetpolicy As TimeStep_Policy) As TimeStep_Policy
Dim r As Collection
For Each r In routes
    targetpolicy.AddRoute CStr(fst(r)), CStr(snd(r)), CSng(thrd(r))
Next r
End Function
'A function that returns the standard position,state associations we use in policy templates.
'I use a privately memoized cache of states, since this is generally going to be read-only.
'Although, callers could eff that plan...the reference returned is still mutable.
Public Function StandardStates() As Dictionary
If statecache Is Nothing Then
    Set statecache = newdict("Deployable", "Deployable", _
                                 "Waiting", "Nothing", _
                                 "Deploying", "Deploying", _
                                 "NotDeployable", "NotDeployable", _
                                 deployed, "Bogging", _
                                 Overlapping, "Overlapping", _
                                 "BehaviorChange", "BehaviorChange", _
                                 "ReturnToDeployable", "Nothing", _
                                 reset, "Dwelling", _
                                 train, "Dwelling", _
                                 ready, "Dwelling", _
                                 available, "Dwelling", _
                                 "Spawning", "Spawning")
End If
    
Set StandardStates = statecache
                                 
End Function
'Allow callers to add a state to the state cache. The added state can have an identical statetype,
'but must have a unique statename...This allows the sharing of states, and a degree of freedom in
'naming and composing behavior.  The name is intentionally long....to prevent this from being
'used all the time.
Public Sub addNonStandardState(name As String, statetype As String, Optional states As Dictionary)
If states Is Nothing Then Set states = StandardStates()
With states
    If Not .exists(name) Then
        .add name, statetype
    Else
        Err.Raise 101, , "There is already an association for " & name & " in the state cache."
    End If
End With
        
End Sub
'Allows entry of multiple named states, derived from a dictionary, into
'the overall statecache.
Public Sub addNonStandardStates(kvps As Dictionary)
Dim entry As Collection
Dim states As Dictionary

Set states = StandardStates()

For Each entry In listKeyVals(kvps)
    addNonStandardState CStr(fst(entry)), CStr(snd(entry)), states
Next entry

End Sub

'Shows the addition of a new named state....
Public Sub cachetest()
pprint StandardStates()
addNonStandardStates newdict("BlahState", "waiting")
pprint StandardStates()

End Sub

'Policy  Type    Schedule    Path    ExpectedBOG ExpectedDwell   Overlap ExpectedCycleLength TimeInterval
Public Function fromRecord(inrec As GenericRecord) As TimeStep_Policy

End Function
Public Function fromPolicy(inpolicy As TimeStep_Policy) As TimeStep_Policy
Set fromPolicy = inpolicy.clone
'make changes
End Function
Private Function getdelta(position As String, deltas As Dictionary) As Long
getdelta = 0
If Not (deltas Is Nothing) Then
    If deltas.exists(position) Then
        getdelta = deltas(position)
    End If
End If

End Function
'Constructor for building policy instances ...
'We want to flexibly create Marathon policies .....
'A policy:
'defines the structure of a Rotational lifecycle ...
'Location transitions, time spent at locations, etc.
'edges on a graph, where nodes are states (locations) and weights are time
'At least one location must be the starting point for a cycle, one is the end.
'defines parameters for availability
'Deployment windows (Megan's parameter)
'Strict vs. Non-Strict (single day available/lifecycle vs. else)
'defines
Public Function RegisterTemplate(name As String, MaxDwellDays As Long, MinDwellDays As Long, _
    maxbogdays As Long, startdeployable As Long, stopdeployable As Long, Optional overlap As Long, _
        Optional deltas As Dictionary, Optional deployableAlreadySet As Boolean) As TimeStep_Policy

Dim res As TimeStep_Policy

Set res = New TimeStep_Policy
Set RegisterTemplate = FromTemplate(name, overlap, deltas)

'parameterize the policy
With RegisterTemplate
    .maxdwell = MaxDwellDays
    .mindwell = MinDwellDays
    .maxbog = maxbogdays 'TOM NOTE 21 Mar 2011 -> I think this is backwards
    .startdeployable = startdeployable
    .stopdeployable = stopdeployable
'    .StartIndex = LocatiOnMap(.StartState) 'TOM TODO fix this .
'    .EndIndex = LocatiOnMap(.EndState)  'TOM TODO fix this.
    If deployableAlreadySet = False Then
        Set res = .setDeployable(.startdeployable, .stopdeployable) 'mutates the policygraph.
    End If
End With

Set RegisterTemplate = res

End Function
Public Function RegisterGhostTemplate(name As String, maxbogdays As Single, Optional overlap As Single) As TimeStep_Policy

Set RegisterGhostTemplate = GhostTemplate(name, maxbogdays, overlap)

'parameterize the policy
With RegisterGhostTemplate
    .maxdwell = 999999
    .mindwell = 0
    .maxbog = maxbogdays 'TOM NOTE 21 Mar 2011 -> I think this is backwards
    .startdeployable = 0
    .stopdeployable = 9999999
    '.StartIndex = 9999999 'LocatiOnMap(.StartState) 'TOM TODO fix this .
    '.EndIndex = LocatiOnMap(.EndState)  'TOM TODO fix this.
End With

End Function
'template for AC policies, we use the parameters to grow and shrink pools
Public Function AC12Template(name As String, overlap As Long, Optional deltas As Dictionary) As TimeStep_Policy
Set AC12Template = New TimeStep_Policy
With AC12Template
    .overlap = overlap
    '.AlterPositions ("AC")
    .name = name
    .AddPosition reset, "Dwelling", train, "Dwelling", ready, "Dwelling", available, "Dwelling", deployed, "Bogging", Overlapping, "Overlapping"
    .AddRoute reset, train, 182 + getdelta(available, deltas)
    .AddRoute train, ready, 183 + getdelta(train, deltas)
    .AddRoute ready, available, 365 + getdelta(ready, deltas)
    .AddRoute available, reset, 365 + getdelta(available, deltas)
    .AddRoute deployed, Overlapping, 365 - overlap + getdelta(deployed, deltas)
    .AddRoute Overlapping, reset, overlap + getdelta(Overlapping, deltas)
End With
'policyname, from, to, time, state
End Function
Public Function AC12defaults(overlap As Long) As Dictionary
Set AC12defaults = _
    newdict(reset, 182, train, 183, ready, 365, available, 365, _
                deployed, 365 - overlap, Overlapping, overlap)
End Function
Public Function getDeltas(field As String, Optional deltas As Dictionary) As Single
getDeltas = 0
If exists(deltas) Then
    If deltas.exists(field) Then getDeltas = CSng(deltas(field))
End If

End Function
'These are default policy routes for AC entities in arforgen.
'Used as scaffolding for templates.
Public Function ACRoutes(overlap As Long, Optional deltas As Dictionary) As Collection
Set ACRoutes = _
    list(list("Reset", "Train", maxFloat(1, getDeltas("Available", deltas))), _
         list("Train", "Ready", maxFloat(1, getDeltas("Train", deltas))), _
         list("Ready", "Available", maxFloat(1, getDeltas("Ready", deltas))), _
         list("Available", "Reset", maxFloat(1, getDeltas("Available", deltas))), _
         list("Deployed", "Overlapping", maxFloat(1, getDeltas("Deployed", deltas) - overlap)), _
         list("Overlapping", "Reset", maxFloat(1, getDeltas("Overlapping", deltas))))
End Function

'These are default policy routes for RC entities in arforgen.
'Used as scaffolding for templates.
Public Function RCRoutes(overlap As Long, demob As Single, Optional deltas As Dictionary) As Collection
Set RCRoutes = _
   list(list("Reset", "Train", maxFloat(1, getDeltas("Reset", deltas))), _
    list("Train", "Ready", maxFloat(1, getDeltas("Train", deltas))), _
    list("Ready", "Available", maxFloat(1, getDeltas("Ready", deltas))), _
    list("Available", "Reset", maxFloat(1, getDeltas("Available", deltas))), _
    list("Deployed", "Overlapping", maxFloat(1, getDeltas("Deployed", deltas) - overlap)), _
    list("Overlapping", "DeMobilization", maxFloat(1, overlap + getDeltas("Ready", deltas))), _
    list("DeMobilization", "Reset", maxFloat(1, demob + getDeltas("Ready", deltas))))
End Function

'These are default routes for ghost entities. they are significantly different than AC/RC
'Used as scaffolding for templates.
Public Function GhostRoutes(Optional bog As Long, Optional overlap As Long, Optional deltas As Dictionary)

Set GhostRoutes = _
    list(list("Spawning", "Deployable", 0), _
         list("Deployable", "Waiting", 0), _
         list("Waiting", "Deploying", 999999), _
         list("Deploying", "NotDeployable", 0), _
         list("NotDeployable", "Deployed", 0), _
         list("Deployed", Overlapping, bog - overlap), _
         list(Overlapping, "ReturnToDeployable", overlap), _
         list("ReturnToDeployable", "Deployable", 0))
End Function
'Eliminates zero wait time routes, useful in some cases.
Public Function truncateRoutes(routes As Collection) As Collection
End Function
'Parses the routes, for each contiguous route, if the states are identical, eliminates the latter route.
Public Function truncateStates(routes As Collection, Optional states As Dictionary) As Collection
If states Is Nothing Then Set states = StandardStates()

End Function

Public Function AC13Template(name As String, overlap As Long, Optional deltas As Dictionary) As TimeStep_Policy
Set AC13Template = New TimeStep_Policy
With AC13Template
    .overlap = overlap
    '.AlterPositions ("AC")
    .name = name
    .AddPosition reset, "Dwelling", train, "Dwelling", ready, "Dwelling", available, "Dwelling", deployed, "Bogging", Overlapping, "Overlapping"
    .AddRoute reset, train, 182 + getdelta(reset, deltas)
    .AddRoute train, ready, 183 + getdelta(train, deltas)
    .AddRoute ready, available, 460 + getdelta(ready, deltas)
    .AddRoute available, reset, 270 + getdelta(available, deltas)
    .AddRoute deployed, Overlapping, 270 - overlap + getdelta(deployed, deltas)
    .AddRoute Overlapping, reset, overlap + getdelta(Overlapping, deltas)
End With
End Function
Public Function AC13defaults(overlap As Long) As Dictionary
Set AC13defaults = _
    newdict(reset, 182, train, 183, ready, 460, available, 270, _
                deployed, 270 - overlap, Overlapping, overlap)
End Function

Public Function AC11Template(name As String, overlap As Long, Optional deltas As Dictionary) As TimeStep_Policy
Set AC11Template = New TimeStep_Policy
With AC11Template
    .overlap = overlap
    '.AlterPositions ("AC")
    .name = name
    .AddPosition reset, "Dwelling", train, "Dwelling", available, "Dwelling", deployed, "Bogging", Overlapping, "Overlapping"
    .AddRoute reset, train, 182 + getdelta(reset, deltas)
    .AddRoute train, available, 183 + getdelta(train, deltas)
    .AddRoute available, reset, 365 + getdelta(available, deltas)
    .AddRoute deployed, Overlapping, 365 - overlap + getdelta(deployed, deltas)
    .AddRoute Overlapping, reset, overlap + getdelta(Overlapping, deltas)
End With
End Function
Public Function AC11defaults(overlap As Long) As Dictionary
Set AC11defaults = _
    newdict(reset, 182, train, 183, available, 365, _
                deployed, 365 - overlap, Overlapping, overlap)
End Function

Public Function RC14Template(name As String, overlap As Long, Optional deltas As Dictionary) As TimeStep_Policy
Set RC14Template = New TimeStep_Policy
With RC14Template
    .MaxMOB = 95
    .overlap = overlap
    '.AlterPositions ("RC")
    .name = name
    .AddPosition reset, "Dwelling", train, "Dwelling", ready, "Dwelling", available, "Dwelling", _
                 deployed, "Bogging", Overlapping, "Overlapping", demobilization, "DeMobilizing"
    .AddRoute reset, train, 365 + getdelta(reset, deltas)
    .AddRoute train, ready, 365 + getdelta(train, deltas)
    .AddRoute ready, available, 730 + getdelta(ready, deltas)
    .AddRoute available, reset, 365 + getdelta(available, deltas)
    .AddRoute deployed, Overlapping, 270 - overlap + getdelta(deployed, deltas)
    
    'TOM Change 13 July 2011
    '.AddRoute Overlapping, Reset, overlap + getdelta(Overlapping, deltas)
    .AddRoute Overlapping, demobilization, overlap + getdelta(ready, deltas)
    .AddRoute demobilization, reset, 95 + getdelta(ready, deltas)
End With
End Function
Public Function RC14Defaults(overlap As Long) As Dictionary
Set RC14Defaults = _
    newdict(reset, 365, train, 365, ready, 730, available, 365, _
                deployed, 270 - overlap, Overlapping, overlap, demobilization, 95)
End Function
'TOM Note 21 Mar 2011 -> Double check the lengths on these policies...
Public Function RC15Template(name As String, overlap As Long, Optional deltas As Dictionary) As TimeStep_Policy
Set RC15Template = New TimeStep_Policy
With RC15Template
    .MaxMOB = 95
    .overlap = overlap
    '.AlterPositions ("RC")
    .name = name
    .AddPosition reset, "Dwelling", train, "Dwelling", ready, "Dwelling", available, "Dwelling", deployed, "Bogging", Overlapping, "Overlapping", _
        demobilization, "DeMobilizing"
    .AddRoute reset, train, 730 + getdelta(reset, deltas)
    .AddRoute train, ready, 365 + getdelta(train, deltas)
    .AddRoute ready, available, 730 + getdelta(ready, deltas)
    .AddRoute available, reset, 365 + getdelta(ready, deltas)
    .AddRoute deployed, Overlapping, 270 - overlap + getdelta(deployed, deltas)
        'TOM Change 13 July 2011
    '.AddRoute Overlapping, Reset, overlap + getdelta(Overlapping, deltas)
    .AddRoute Overlapping, demobilization, overlap + getdelta(ready, deltas)
    .AddRoute demobilization, reset, 95 + getdelta(ready, deltas)
End With
End Function
Public Function RC15Defaults(overlap As Long) As Dictionary
Set RC15Defaults = _
    newdict(reset, 730, train, 365, ready, 730, available, 365, _
                deployed, 270 - overlap, Overlapping, overlap, demobilization, 95)
End Function
Public Function RC11Template(name As String, overlap As Long, Optional deltas As Dictionary) As TimeStep_Policy
Set RC11Template = New TimeStep_Policy
With RC11Template
    .MaxMOB = 95
    .overlap = overlap
    '.AlterPositions ("RC")
    .name = name
    .AddPosition reset, "Dwelling", train, "Dwelling", available, "Dwelling", deployed, "Bogging", Overlapping, "Overlapping", _
        demobilization, "DeMobilizing"
    .AddRoute reset, train, 182 + getdelta(reset, deltas)
    .AddRoute train, available, 183 + getdelta(train, deltas)
    .AddRoute available, reset, 365 + getdelta(ready, deltas)
    .AddRoute deployed, Overlapping, 270 - overlap + getdelta(deployed, deltas)
        'TOM Change 13 July 2011
    '.AddRoute Overlapping, Reset, overlap + getdelta(Overlapping, deltas)
    .AddRoute Overlapping, demobilization, overlap + getdelta(ready, deltas)
    .AddRoute demobilization, reset, 95 + getdelta(ready, deltas)
End With
End Function
Public Function RC11Defaults(overlap As Long) As Dictionary
Set RC11Defaults = _
    newdict(reset, 182, train, 183, available, 365, _
                deployed, 270 - overlap, Overlapping, overlap, demobilization, 95)
End Function
Public Function RC12Template(name As String, overlap As Long, Optional deltas As Dictionary) As TimeStep_Policy
Set RC12Template = New TimeStep_Policy

With RC12Template
    .MaxMOB = 95
    .overlap = overlap
    '.AlterPositions ("RC")
    .name = name
    .AddPosition reset, "Dwelling", train, "Dwelling", available, "Dwelling", deployed, "Bogging", Overlapping, "Overlapping", _
        demobilization, "DeMobilizing"
    .AddRoute reset, train, 365 + getdelta(reset, deltas)
    .AddRoute train, available, 365 + getdelta(train, deltas)
    .AddRoute available, reset, 365 + getdelta(available, deltas)
    .AddRoute deployed, Overlapping, 270 - overlap + getdelta(deployed, deltas)
    
    'TOM Change 13 July 2011
    '.AddRoute Overlapping, Reset, overlap + getdelta(Overlapping, deltas)
    .AddRoute Overlapping, demobilization, overlap + getdelta(ready, deltas)
    .AddRoute demobilization, reset, 95 + getdelta(ready, deltas)
End With

End Function
Public Function RC12Defaults(overlap As Long) As Dictionary
Set RC12Defaults = _
    newdict(reset, 365, train, 365, available, 365, _
                deployed, 270 - overlap, Overlapping, overlap, demobilization, 95)
End Function
Public Function GhostTemplate(name As String, bog As Single, overlap As Single) As TimeStep_Policy
Set GhostTemplate = New TimeStep_Policy
With GhostTemplate
    .overlap = overlap
    '.AlterPositions ("Ghost")
    .name = name
    .AddPosition "Spawning", "Spawning", "Deployable", "Deployable", "Waiting", "Nothing", _
                "Deploying", "Deploying", "NotDeployable", "NotDeployable", _
                    deployed, "Bogging", Overlapping, "Overlapping", _
                            "BehaviorChange", "BehaviorChange", "ReturnToDeployable", "Nothing"
    .AddRoute "Spawning", "Deployable", 0
    .AddRoute "Deployable", "Waiting", 0
    .AddRoute "Waiting", "Deploying", 999999
    .AddRoute "Deploying", "NotDeployable", 0
    .AddRoute "NotDeployable", deployed, 0
    .AddRoute deployed, Overlapping, bog - overlap
    .AddRoute Overlapping, "ReturnToDeployable", overlap
    .AddRoute "ReturnToDeployable", "Deployable", 0
    .startstate = "Deployable"
    .endstate = "ReturnToDeployable"
    .cyclelength = 99999
End With

End Function
Public Function GhostDefaults(bog As Long, overlap As Long) As Dictionary
Set GhostDefaults = newdict("Spawning", 0, _
                            "Deployable", 0, _
                            "Waiting", 999999, _
                            "Deploying", 0, _
                            "NotDeployable", 0, _
                            deployed, bog - overlap, _
                            Overlapping, overlap, _
                            "ReturnToDeployable", 0)
End Function
Public Function FromTemplate(name As String, Optional overlap As Long, Optional deltas As Dictionary, Optional recoverytime As Long) As TimeStep_Policy
Select Case name
    Case Is = AC12
        Set FromTemplate = AC12Template(AC12, overlap, deltas)
    Case Is = AC13
        Set FromTemplate = AC13Template(AC13, overlap, deltas)
    Case Is = AC11
        Set FromTemplate = AC11Template(AC11, overlap, deltas)
    Case Is = RC14
        Set FromTemplate = RC14Template(RC14, overlap, deltas)
    Case Is = RC15
        Set FromTemplate = RC15Template(RC15, overlap, deltas)
    Case Is = RC11
        Set FromTemplate = RC11Template(RC11, overlap, deltas)
    Case Is = RC12
        Set FromTemplate = RC12Template(RC12, overlap, deltas)
    Case Is = RC14Remob
        Set FromTemplate = RC14ReMobTemplate(RC14Remob, overlap, deltas, recoverytime)
End Select

With FromTemplate
    .startstate = reset
    .endstate = available
    'Err.Raise 101, , "Fix the line below, need to update cyclelength!"
    '.cyclelength = .PositionGraph.pathlength(.PositionGraph.getPath(.PositionGraph.FindCycle(.StartState, .EndState)))
    .cyclelength = cycleSearch(.PositionGraph, makeDepthFringe(), .startstate).distance(.startstate)
    If .cyclelength = 0 Then Err.Raise 101, , "Cycle length is 0, check your rotational policy!"
End With

End Function
'template for RC policies with a remob time, we use the parameters to grow and shrink pools
'Allows 2 deployments.  Recovery time dictates the amount of time spent in between bogs.
Public Function RC14ReMobTemplate(name As String, overlap As Long, Optional deltas As Dictionary, _
                                    Optional recoverytime As Long, Optional bogbudget As Long) As TimeStep_Policy
Set RC14ReMobTemplate = New TimeStep_Policy
If recoverytime = 0 Then recoverytime = 365
If bogbudget = 0 Then bogbudget = 270 * 2 'default to 2 deployments
With RC14ReMobTemplate
    .MaxMOB = 95
    .overlap = overlap
    '.AlterPositions ("RC")
    .name = name
    .AddPosition reset, "Dwelling", train, "Dwelling", ready, "Dwelling", available, "Dwelling", _
                 deployed, "Bogging", Overlapping, "Overlapping", demobilization, "DeMobilizing", _
                    recovery, Recovering, Recovered, Recovered
    .AddRoute reset, train, 365 + getdelta(reset, deltas)
    .AddRoute train, ready, 365 + getdelta(train, deltas)
    .AddRoute ready, available, 730 + getdelta(ready, deltas)
    .AddRoute available, reset, 365 + getdelta(available, deltas)
    .AddRoute deployed, Overlapping, 270 - overlap + getdelta(deployed, deltas)
    
    'TOM Change 13 July 2011
    '.AddRoute Overlapping, Reset, overlap + getdelta(Overlapping, deltas)
    '.AddRoute Overlapping, demobilization, overlap + getdelta(ready, deltas)
'    .AddRoute demobilization, reset, 95 + getdelta(ready, deltas)
    .AddRoute Overlapping, recovery, overlap + getdelta(Overlapping, deltas)
    .AddRoute recovery, Recovered, CSng(recoverytime)
    .AddRoute Recovered, demobilization, 95 + getdelta(ready, deltas)
    .AddRoute demobilization, reset, 0
    .bogbudget = bogbudget
End With
'policyname, from, to, time, state
End Function
Public Function RC14ReMobDefaults(overlap As Long) As Dictionary
Set RC14ReMobDefaults = _
    newdict(reset, 365, train, 365, ready, 730, available, 365, _
                deployed, 270 - overlap, Overlapping, overlap, demobilization, 95)
End Function

'TOM TODO ->
'Need a more declarative way to do this, the numerical values hide what's going on in the function
'TOM NOTE 21 MAr 2011 -> Need to parametrically vary these.  I was screwing up the DeployableStart
'and DeployableStart by winging it.  Basically a data error.
Public Function DefaultArforgenPolicies() As Dictionary

Dim pol
Dim policy As TimeStep_Policy
Dim policies As Dictionary

Set policies = New Dictionary

Set policy = RegisterTemplate(AC12, 365 * 3, 365 * 2, 365, 365 * 2, 365 * 2 + 1, 45)
'RegisterPolicyLocations policy
policy.name = "AC12Strict"
policies.add policy.name, policy

'Changed from +- 90, to +- 180
Set policy = RegisterTemplate(AC12, 365 * 3, 365 * 2, 365, 365 * 2 - 180, 365 * 2 + 180, 45)
'RegisterPolicyLocations policy
policy.name = "AC12"
policies.add policy.name, policy

'units can go until end of available
Set policy = RegisterTemplate(AC12, 365 * 3, 365, 365, 365 * 1, 1095, 45)
policy.name = "AC12Loose"
policies.add policy.name, policy


Set policy = RegisterTemplate(AC13, 365 * 3, 825, 270, 825, 825 + 1, 45)
'RegisterPolicyLocations policy
policy.name = "AC13Strict"
policies.add policy.name, policy

'change +-180
Set policy = RegisterTemplate(AC13, 365 * 3, 825, 270, 825 - 180, 825 + 180, 45)
'RegisterPolicyLocations policy
policy.name = "AC13"
policies.add policy.name, policy

'made this actually loose.
Set policy = RegisterTemplate(AC13, 365 * 3, 365, 270, 365, 365 * 3, 45)
policy.name = "AC13Loose"
policies.add policy.name, policy


Set policy = RegisterTemplate(AC11, 365 * 2, 365, 365, 365, 365 * 2, 0) '0 overlap
'RegisterPolicyLocations policy
policies.add policy.name, policy

Set policy = RegisterTemplate(RC14, 365 * 5, 365 * 2, 270, 365 * 4, 365 * 4 + 1, 45)
'RegisterPolicyLocations policy
policy.name = "RC14Strict"
policies.add policy.name, policy

'+- 180
Set policy = RegisterTemplate(RC14, 365 * 5, 365 * 2, 270, 365 * 4 - 180, 365 * 4 + 180, 45)
'RegisterPolicyLocations policy
policy.name = "RC14"
policies.add policy.name, policy

Set policy = RegisterTemplate(RC14, 365 * 5, 365 * 2, 270, 365 * 2, 365 * 5 - 90, 45)
policy.name = "RC14Loose"
policies.add policy.name, policy


Set policy = RegisterTemplate(RC15, 365 * 6, 365 * 3, 270, 365 * 5, 365 * 5 + 1, 45)
'RegisterPolicyLocations policy
policy.name = "RC15Strict"
policies.add policy.name, policy

'+- 180
Set policy = RegisterTemplate(RC15, 365 * 6, 365 * 3, 270, 365 * 5 - 180, 365 * 5 + 180, 45)
'RegisterPolicyLocations policy
policy.name = "RC15"
policies.add policy.name, policy

Set policy = RegisterTemplate(RC15, 365 * 6, 365 * 3, 270, 365 * 5 - 90, 365 * 5 + 90, 45)
policy.name = "RC15Loose"
policies.add policy.name, policy

'This is the RC surge policy...
'Note -> changed to 0 overlap for surge.
'TOM Change 13 July 2011
Set policy = RegisterTemplate(RC12, 365 * 3, 365, 270, 365 * 2, (365 * 3) - 90, 0)
policy.name = "RC12"
policies.add policy.name, policy

Set policy = policies(AC12).clone
policy.name = GhostPermanent12
policies.add policy.name, policy

Set policy = policies(AC13).clone
policy.name = GhostPermanent13
policies.add policy.name, policy

Set policy = RegisterGhostTemplate("Ghost365_45", 365, 45)
policies.add policy.name, policy

Set policy = RegisterGhostTemplate("Ghost270_45", 270, 45)
policies.add policy.name, policy

Set policy = RegisterGhostTemplate("BOGForever", 999999, 0)
policies.add policy.name, policy



''Enabler policies....i.e. 30 day overlap

Set policy = RegisterTemplate(AC13, 365 * 3, 825, 270, 825 - 180, 825 + 180, 30)
policy.name = "AC13_Enabler"
policies.add policy.name, policy

'units can go until end of available
Set policy = RegisterTemplate(AC12, 365 * 3, 365, 365, 365 * 1, 1095, 30)
policy.name = "AC12Loose_Enabler"
policies.add policy.name, policy

Set policy = RegisterTemplate(RC15, 365 * 6, 365 * 3, 270, 365 * 5 - 180, 365 * 5 + 180, 30)
'RegisterPolicyLocations policy
policy.name = "RC15_Enabler"
policies.add policy.name, policy

Set policy = RegisterTemplate(RC14, 365 * 5, 365 * 2, 270, 365 * 2, 365 * 5 - 90, 30)
policy.name = "RC14Loose_Enabler"
policies.add policy.name, policy

Set policy = RegisterGhostTemplate("Ghost365_30", 365, 30)
policy.name = "Ghost365_30"
policies.add policy.name, policy

Set policy = Nothing

'Set DefaultArforgenPolicies = listVals(policies)
Set DefaultArforgenPolicies = policies
Set policies = Nothing

End Function

Public Function TFPolicies() As Dictionary
Dim policy As TimeStep_Policy
Set TFPolicies = New Dictionary

'This is a special policy adapted for T.F's study.
'RC has an extra year of availability to deploy.  I don't think it will matter.
Set policy = RegisterTemplate(RC14, 365 * 6, 365 * 2, 270, 365 * 2, 365 * 6 - 90, 45, _
                                newdict(available, 365))
policy.name = "RC14Loose_3Year"
TFPolicies.add policy.name, policy

'This is a special policy adapted for T.F.'s study.
Set policy = RegisterTemplate(RC14Remob, 365 * 7, 365 * 2, 270, 365 * 2, 365 * 7 - 90, 45, _
                                newdict(available, 730))
policy.name = RC14Remob
TFPolicies.add policy.name, policy

'This is a special policy adapted for T.F's study.
'RC has an extra year of availability to deploy.  I don't think it will matter.
Set policy = RegisterTemplate(RC14, 365 * 6, 365 * 2, 270, 365 * 2, 365 * 6 - 90, 30, _
                                newdict(available, 365))
policy.name = "RC14Loose_3Year_Enabler"
TFPolicies.add policy.name, policy

'This is a special policy adapted for T.F.'s study.
    Set policy = RegisterTemplate(RC14Remob, 365 * 7, 365 * 2, 270, 365 * 2, 365 * 7 - 90, 30, _
                                newdict(available, 730))
policy.name = RC14Remob & "_Enabler"
TFPolicies.add policy.name, policy

End Function


'create some new policies. Let's see if we can interactively build this badboy
Public Sub tst()
Dim p As TimeStep_Policy
Set p = New TimeStep_Policy

End Sub

'Defines a policy that is either deployed or not deployed.
'This is very similar to the Ghost policy.
Public Function maxUtilizationTemplate(name As String, maxbogdays As Long, Optional minimumDwell As Long, _
                                        Optional overlap As Long) As TimeStep_Policy
Set maxUtilizationTemplate = New TimeStep_Policy
With maxUtilizationTemplate
    .overlap = overlap
    '.AlterPositions ("Ghost")
    .name = name
    .AddPosition "Deployable", "Deployable", "Waiting", "Nothing", _
                 "Deploying", "Deploying", "NotDeployable", "NotDeployable", _
                    deployed, "Bogging", Overlapping, "Overlapping", _
                            "BehaviorChange", "BehaviorChange", "ReturnToDeployable", "Nothing"
    .AddRoute "Deployable", "Waiting", CSng(minimumDwell)
    .AddRoute "Waiting", "Deploying", inf 'no upperbound on cycle length.
    .AddRoute "Deploying", "NotDeployable", 0
    .AddRoute "NotDeployable", deployed, 0
    .AddRoute deployed, Overlapping, CSng(maxbogdays) - CSng(overlap)
    .AddRoute Overlapping, "ReturnToDeployable", CSng(overlap)
    .AddRoute "ReturnToDeployable", "Deployable", 0
    .startstate = "Deployable"
    .endstate = "ReturnToDeployable"
    .cyclelength = inf
End With

End Function
Public Function makePolicyTemplate(name As String, overlap As Single, startstate As String, endstate As String, _
                                    states As Dictionary, routes As Collection)
Set makePolicyTemplate = newdict("name", name, _
                                 "states", states, _
                                 "startstate", startstate, _
                                 "endstate", endstate, _
                                 "routes", routes)
End Function

'{:name name
' :overlap overlap
' :states {Deployable Deployable
'          Waiting  Nothing
'          Deploying Deploying
'          Bogging   Bogging
'          Overlapping Overlapping
'          BehaviorChange BehaviorChange
'          ReturnToDeployable Nothing}
' :startstate Deployable
' :endstate   ReturnToDeployable
'''''''''''' :cyclelength inf
' :routes [[Deployable Waiting MinimumDwell]
'          [Waiting Deployed inf]
'          [Deploying NotDeployable 0]
'          [NotDeployable Deployed 0]
'          [deployed Overlapping (maxbog - overlap)]
'          [overlapping ReturnToDeployable overlap]
'          [ReturnToDeployable Deployable 0]]}


'Gives us a barebones specification for a policy, which is just a dictionary.
Public Function policyspec(startstate As String, endstate As String, routes As Collection) As Dictionary

Set policyspec = newdict("startstate", startstate, "endstate", endstate, "routes", routes)

End Function

Public Function basicPolicySpec(name As String, overlap As Single, startstate As String, endstate As String, _
                                    routes As Collection) As TimeStep_Policy
                                    
Set basicPolicySpec = makePolicyTemplate(name, overlap, startstate, endstate, StandardStates(), routes)

End Function

'Public Function policyFromSpec(name As String, overlap As Single, states As Dictionary, routes As Collection) As TimeStep_Policy
'
'Set policyFromSpec = addRoutes(routes, addPositions(states, New TimeStep_Policy))
'policyFromSpec.name = spec("name")
'policyFromSpec.overlap = overlap
'
'End Function




