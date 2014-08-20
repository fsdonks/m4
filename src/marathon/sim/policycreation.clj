(ns marathon.sim.policycreation)

;;The functions in this module focus on defining and composing new policies and policy templates.  This is
;;a fairly low level library that end users won;;t normally need, because most of the functionality here is
;;called during parsing of the data.  However, developers do have programmatic access to the same functions,
;;so that custom policies and templates can be created in VBA.  Potential use cases might include parametric
;;or random policies that vary over a range of values.
Option Explicit

;;This is part of a reorganization of a lot of the embedded functionality in the early object oriented
;;implementation for marathon.  All functions in this module either consume no arguments, to produce
;;policies, or modify existing policies in some way.  Most are a port from TimeStep_Policy
Private Type routerec
  source As String
  dest As String
  distance As Single
End Type
Dim key
Private statecache As Dictionary

Public Function create(positions As Dictionary, routes As Dictionary) As TimeStep_Policy
Set create = New TimeStep_Policy
End Function
Private Function addPositions(positions As Dictionary, targetpolicy As TimeStep_Policy) As TimeStep_Policy
Set addPositions = targetpolicy

With addPositions
    For Each key In positions.keys
        .AddPosition CStr(key), CStr(positions(key))
    Next key
End With
End Function
;;expects a dictionary with triples as keys
Private Function getRoute(routes As Dictionary, key As String) As routerec
;;Dim rt As Collection
;;With getRoute

End Function

;;expects a collection of triples, ((from to cost)...)
Public Function addRoutes(routes As Collection, targetpolicy As TimeStep_Policy) As TimeStep_Policy
Dim r As Collection
For Each r In routes
    targetpolicy.AddRoute CStr(fst(r)), CStr(snd(r)), CSng(thrd(r))
Next r
End Function

;;Policy  Type    Schedule    Path    ExpectedBOG ExpectedDwell   Overlap ExpectedCycleLength TimeInterval
Public Function fromRecord(inrec As GenericRecord) As TimeStep_Policy

End Function
Public Function fromPolicy(inpolicy As TimeStep_Policy) As TimeStep_Policy
Set fromPolicy = inpolicy.clone
;;make changes
End Function

Private Function getdelta(position As String, deltas As Dictionary) As Long
getdelta = 0
If Not (deltas Is Nothing) Then
    If deltas.exists(position) Then
        getdelta = deltas(position)
    End If
End If

End Function
;;Redundant
Public Function getDeltas(field As String, Optional deltas As Dictionary) As Single
getDeltas = 0
If exists(deltas) Then
    If deltas.exists(field) Then getDeltas = CSng(deltas(field))
End If

End Function

;;create some new policies. Let;;s see if we can interactively build this badboy
Public Sub tst()
Dim p As TimeStep_Policy
Set p = New TimeStep_Policy

End Sub

;;A function that returns the standard position,state associations we use in policy templates.
;;I use a privately memoized cache of states, since this is generally going to be read-only.
;;Although, callers could eff that plan...the reference returned is still mutable.
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
;;Allow callers to add a state to the state cache. The added state can have an identical statetype,
;;but must have a unique statename...This allows the sharing of states, and a degree of freedom in
;;naming and composing behavior.  The name is intentionally long....to prevent this from being
;;used all the time.
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
;;Allows entry of multiple named states, derived from a dictionary, into
;;the overall statecache.
Public Sub addNonStandardStates(kvps As Dictionary)
Dim entry As Collection
Dim states As Dictionary

Set states = StandardStates()

For Each entry In listKeyVals(kvps)
    addNonStandardState CStr(fst(entry)), CStr(snd(entry)), states
Next entry

End Sub

;;Shows the addition of a new named state....
Public Sub cachetest()
pprint StandardStates()
addNonStandardStates newdict("BlahState", "waiting")
pprint StandardStates()

End Sub


;;Constructor for building policy instances ...
;;We want to flexibly create Marathon policies .....
;;A policy:
;;defines the structure of a Rotational lifecycle ...
;;Location transitions, time spent at locations, etc.
;;edges on a graph, where nodes are states (locations) and weights are time
;;At least one location must be the starting point for a cycle, one is the end.
;;defines parameters for availability
;;Deployment windows (Megan;;s parameter)
;;Strict vs. Non-Strict (single day available/lifecycle vs. else)
;;defines
Public Function RegisterTemplate(name As String, MaxDwellDays As Long, MinDwellDays As Long, _
    maxbogdays As Long, startdeployable As Long, stopdeployable As Long, Optional overlap As Long, _
        Optional deltas As Dictionary, Optional deployableAlreadySet As Boolean) As TimeStep_Policy

Dim res As TimeStep_Policy

Set RegisterTemplate = FromTemplate(name, overlap, deltas, , maxbogdays, MinDwellDays)

;;parameterize the policy
With RegisterTemplate
    .maxdwell = MaxDwellDays
    .mindwell = MinDwellDays
    .maxbog = maxbogdays ;;TOM NOTE 21 Mar 2011 -> I think this is backwards
    .startdeployable = startdeployable
    .stopdeployable = stopdeployable
;;    .StartIndex = LocatiOnMap(.StartState) ;;TOM TODO fix this .
;;    .EndIndex = LocatiOnMap(.EndState)  ;;TOM TODO fix this.
    If deployableAlreadySet = False Then
        Set res = .setDeployable(.startdeployable, .stopdeployable) ;;mutates the policygraph.
    Else
        Set res = RegisterTemplate
    End If
End With

Set RegisterTemplate = res

End Function
Public Function RegisterGhostTemplate(name As String, maxbogdays As Single, Optional overlap As Single) As TimeStep_Policy

Set RegisterGhostTemplate = GhostTemplate(name, maxbogdays, overlap)

;;parameterize the policy
With RegisterGhostTemplate
    .maxdwell = inf
    .mindwell = 0
    .maxbog = maxbogdays ;;TOM NOTE 21 Mar 2011 -> I think this is backwards
    .startdeployable = 0
    .stopdeployable = inf
    ;;.StartIndex = 9999999 ;;LocatiOnMap(.StartState) ;;TOM TODO fix this .
    ;;.EndIndex = LocatiOnMap(.EndState)  ;;TOM TODO fix this.
End With

End Function


Public Function FromTemplate(name As String, Optional overlap As Long, Optional deltas As Dictionary, _
                                Optional recoverytime As Long, Optional maxbog As Long, Optional mindwell As Long) As TimeStep_Policy
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
    Case Is = RC14ReMob
        Set FromTemplate = RC14ReMobTemplate(RC14ReMob, overlap, deltas, recoverytime)
        ;;Added 24 July 2012
    Case Is = MarathonEnumsAndConstants.FFGMission ;;Added 10 Sep 2012
        Set FromTemplate = FFGMissionTemplate(name, overlap, deltas)
    Case Is = MarathonEnumsAndConstants.ACFFG
        Set FromTemplate = ACFFGTemplate(name, overlap, deltas)
    Case Is = MarathonEnumsAndConstants.RCFFG
        Set FromTemplate = RCFFGTemplate(name, overlap, deltas)
    Case Is = MarathonEnumsAndConstants.RCOpSus
        Set FromTemplate = RCOpSusTemplate(name, overlap, deltas)
    Case MarathonEnumsAndConstants.MaxUtilization, MarathonEnumsAndConstants.NearMaxUtilization
        Set FromTemplate = maxUtilizationTemplate(name, CSng(maxbog), CSng(overlap), CSng(mindwell))
    Case Else
        Err.Raise 101, , "Template " & name & " does not exist!"
End Select

With FromTemplate
    If .startstate = vbNullString Then .startstate = reset
    If .endstate = vbNullString Then .endstate = available
    
    ;;Err.Raise 101, , "Fix the line below, need to update cyclelength!"
    ;;.cyclelength = .PositionGraph.pathlength(.PositionGraph.getPath(.PositionGraph.FindCycle(.StartState, .EndState)))
    .cyclelength = cycleSearch(.PositionGraph, makeDepthFringe(), .startstate).distance(.startstate)
    If .cyclelength = 0 Then Err.Raise 101, , "Cycle length is 0, check your rotational policy!"
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
;;
;;;;{:name name
;;;; :overlap overlap
;;;; :states {Deployable Deployable
;;;;          Waiting  Nothing
;;;;          Deploying Deploying
;;;;          Bogging   Bogging
;;;;          Overlapping Overlapping
;;;;          BehaviorChange BehaviorChange
;;;;          ReturnToDeployable Nothing}
;;;; :startstate Deployable
;;;; :endstate   ReturnToDeployable
;;;;;;;;;;;;;;;;;;;;;;;;;; :cyclelength inf
;;;; :routes [[Deployable Waiting MinimumDwell]
;;;;          [Waiting Deployed inf]
;;;;          [Deploying NotDeployable 0]
;;;;          [NotDeployable Deployed 0]
;;;;          [deployed Overlapping (maxbog - overlap)]
;;;;          [overlapping ReturnToDeployable overlap]
;;;;          [ReturnToDeployable Deployable 0]]}
;;
;;
;;Gives us a barebones specification for a policy, which is just a dictionary.
Public Function policyspec(startstate As String, endstate As String, routes As Collection) As Dictionary

Set policyspec = newdict("startstate", startstate, "endstate", endstate, "routes", routes)

End Function
;;
Public Function basicPolicySpec(name As String, overlap As Single, startstate As String, endstate As String, _
                                    routes As Collection) As TimeStep_Policy

Set basicPolicySpec = makePolicyTemplate(name, overlap, startstate, endstate, StandardStates(), routes)

End Function
;;
;;;;Public Function policyFromSpec(name As String, overlap As Single, states As Dictionary, routes As Collection) As TimeStep_Policy
;;;;
;;;;Set policyFromSpec = addRoutes(routes, addPositions(states, New TimeStep_Policy))
;;;;policyFromSpec.name = spec("name")
;;;;policyFromSpec.overlap = overlap
;;;;
;;;;End Function
;;
;;
;;
;;
Public Function printTemplate(templatename As String) As String
printTemplate = printstr(FromTemplate(templatename))
End Function

