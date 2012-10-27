(ns marathon.port.policy)

'marathonoppolicy
Option Explicit
'Helper sub to partially fill in fields for the more generic RequestUpdate sub.
Public Sub requestPolicyUpdate(t As Single, context As timestep_SimContext)
SimLib.requestUpdate t, "PolicyManager", UpdateType.policy, , context
End Sub

'TODO -> We need to build in some glue code in the CoreSimulation...
''The previously coupled code has addPeriod adding the period to the policy store AND
''sending out event notifications (scheduling).  We now schedule in a separate function.
''As a consequence, addPeriod doesn't require a simulation context (doesn't need to
''send events)

'Allows the addition of known periods of time....
Public Sub addPeriod(policystore As TimeStep_StorePolicy, per As GenericPeriod)
With policystore
    If Not .periods.exists(per.name) Then
        .periods.add per.name, per
    Else
        Err.Raise 101, , "Period already exists, check for duplicates"
    End If
End With

End Sub

'Import a list of periods into the policystore.
Public Sub addPeriods(periods As Collection, policystore As TimeStep_StorePolicy)
Dim p As GenericPeriod
For Each p In periods
    addPeriod policystore, p
Next p
End Sub

'Notifies interested parties of the beginning and ending of a period.
'Schedules a policy update to coincide with the beginning and ending of the period,
'so that policy management runs when a period changes.
Public Sub schedulePeriod(per As GenericPeriod, ctx As timestep_SimContext)

With per
    SimLib.triggerEvent TimeStep_Msg.AddedPeriod, .name, .name, "Added period " & .toString, , ctx
    requestPolicyUpdate .fromday, ctx
    requestPolicyUpdate .today, ctx
End With
End Sub
    
'Added 14 Jul 2012 -> further decoupling of event notifications.  Adding a period has nothing to do with scheduling a
'set of known periods.  We now schedule the periods in a separate step.
Public Sub schedulePeriods(policystore As TimeStep_StorePolicy, ctx As timestep_SimContext)
Dim per

With policystore
    For Each per In .periods
        schedulePeriod .periods(per), ctx
    Next
End With

End Sub
'Fetch the period(s) that intersect time t.
''TOM Change 4 Jan 2011 -> note the memoization, this saves us calls if we already calculated the period.
Public Function FindPeriod(t As Single, policystore As TimeStep_StorePolicy) As String
'function that returns the period name within the interval that is set in the
'ARFORGEN Parameters worksheet
Dim name As String
Dim i As Long
Dim per
Dim pd As GenericPeriod
Static fixed As Single
fixed = Fix(t)

If policystore.memoized.exists("FindPeriod_" & t) Then
    FindPeriod = policystore.memoized("FindPeriod_" & t)
Else
    For Each per In policystore.periods
        Set pd = policystore.periods(per)
        With pd
            If fixed >= Fix(.fromday) And fixed <= Fix(.today) Then
                FindPeriod = .name
                Exit For
            End If
        End With
    Next per
    If FindPeriod = vbNullString Then
        FindPeriod = "None"
    End If
    policystore.memoized.add "FindPeriod_" & t, FindPeriod
End If

End Function

'simple indexing function.  We index starting from 101....although we really don't
'have to out of necessity.  I might revisit this.  Vestigial and inherited from old, poor designs.
Public Function nextlocationID(locationcount As Long) As Long
nextlocationID = locationcount + 101
End Function

'This sub helps us to keep track of demand and policy locations.
'The need for indexing is debatable, and is likely vestigial.
Public Sub registerLocation(locname As String, policystore As TimeStep_StorePolicy)
Dim nxtid As Long
With policystore
    If .LocatiOnMap.exists(locname) Then
        'Err.Raise 101, ,  "Location already exists"
    Else
        nxtid = nextlocationID(.LocatiOnMap.count)
        .LocatiOnMap.add locname, nxtid
        .LocationIndex.add nxtid, locname
        'TOM Change 7 June 2011
        SetLib.addSet .locations, locname
    End If
End With

End Sub
Public Sub registerLocations(locs As Collection, policystore As TimeStep_StorePolicy)
Dim loc
For Each loc In locs
    registerLocation CStr(loc), policystore
Next loc
End Sub

Private Sub DefaultArforgenLocations(policystore As TimeStep_StorePolicy)
registerLocations list(available, ready, train, reset, deployed, Overlapping), policystore
End Sub

'each location in a policy should be registered in the locations dictionary.
Private Function getPolicyLocations(policy As IRotationPolicy) As Collection
Dim node

Set getPolicyLocations = New Collection

With policy.PositionGraph
    For Each node In .nodes
        getPolicyLocations.add (CStr(node))
    Next node
End With

End Function

'Policy Store operations.....
'Perform a reduction over a list of default policies....adding each one to the policystore.
Public Sub addDefaults(policystore As TimeStep_StorePolicy)
Dim pol As TimeStep_Policy

addPolicies MarathonPolicyCreation.DefaultArforgenPolicies, policystore  'register our default set of policies.
addPolicies MarathonPolicyCreation.TFPolicies, policystore

'RegisterLocations 'Ensure all locations are registered with policy manager.
For Each pol In policystore.policies
    policystore.permanents.add CStr(pol), 0
Next pol

End Sub
'method for adding atomic policies.
Public Sub addPolicy(ByRef policy As TimeStep_Policy, policystore As TimeStep_StorePolicy)

With policystore
    assert policy.name <> vbNullString, True
    If Not .policies.exists(policy.name) Then
        .policies.add policy.name, policy
        registerLocations getPolicyLocations(policy), policystore
    Else
        Err.Raise 101, , "Policy already exists"
    End If
End With

End Sub
Public Sub addPolicyComposite(ByRef policy As TimeStep_PolicyComposite, policystore As TimeStep_StorePolicy)

With policystore
    If Not .policies.exists(policy.name) Then
        .policies.add policy.name, policy
        .composites.add policy.name, policy
        registerLocations getPolicyLocations(policy), policystore
    Else
        Err.Raise 101, , "Policy already exists"
    End If
End With

End Sub
'adds a list of policies, either atomic or composite.
Public Sub addPolicies(policies As Collection, policystore As TimeStep_StorePolicy)
Dim p As TimeStep_Policy
Dim c As TimeStep_PolicyComposite

Dim r As IRotationPolicy

For Each r In policies
    'Set r = p
    If r.getPolicyType = atomic Then
        Set p = r
        addPolicy p, policystore
    Else
        Set c = r
        addPolicyComposite c, policystore
    End If
Next r

Set r = Nothing
End Sub


''TOM NOTE 7 Jun 2011 -> These are both vestigial functions.  We're not even using locationID
'anymore.  Recommend scrapping them to cut down on the code bloat.
'Wrapper for getting our locationID
Public Function locationID(locname As String, policystore As TimeStep_StorePolicy) As Long
locationID = CLng(policystore.LocatiOnMap(locname))
End Function

'Wrapper for getting our LocationName
Public Function LocationName(locationID As Long, policystore As TimeStep_StorePolicy) As String
LocationName = CStr(policystore.LocationIndex(locationID))
End Function

Public Sub periodChangeEvent(fromname As String, toname As String, ctx As timestep_SimContext)
SimLib.triggerEvent periodChange, "PolicyManager", toname, "Period changed from " & fromname & " to " & toname, , ctx
End Sub

'TOM Change 7 Jun 2011 -> use manage policies to prosecute period changes.
'TOM Change 04 Jan 2011 -> This is going to be obsolete. All of our default policies will be coded, but most
'of them will be derived from data.
'TOM Change 03 Jan 2011
'TOM TODO -> Alter the underyling subs here to manifest as actual "policy changes"
Public Sub ManagePolicies(day As Single, policystore As TimeStep_StorePolicy, ctx As timestep_SimContext)
Dim fromname As String
Dim toname As String

With policystore
    fromname = .activePeriod.name
    toname = FindPeriod(day, policystore)
    If .activePeriod.name <> toname Then
        Set .activePeriod = .periods(toname)
        periodChangeEvent fromname, toname, ctx
        'TOM Change disabled temporarily.
        changePolicies fromname, toname, policystore, ctx
    End If
End With

End Sub
'Tom Change 12 July 2011
'tell each policy to have its subscriber change to a new policy.
'Simple algorithm -> fetch the new policy associated with the period.
'Tell each policy to change to the new policy.
Private Sub changePolicies(currentperiod As String, newperiod As String, policystore As TimeStep_StorePolicy, _
                              ctx As timestep_SimContext)

Dim oldpolicy As IRotationPolicy
'Dim subscribers As Dictionary

Dim newname As String
Dim policy

If currentperiod <> "Initialization" Then
    'only composite policies can change.
    For Each oldpolicy In getChangedPolicies(currentperiod, newperiod, policystore.composites)
        oldpolicy.onPeriodChange newperiod 'update the policy. 'TOM Note -> this should advance the active policy of the
                                           'oldpolicy.
        If oldpolicy.subscribers.count > 0 Then 'map a change onto the subscribed units.
            'replace with change subscribers...
'            Set subscribers = oldpolicy.subscribers
            alterUnitPolicies oldpolicy.subscribers, currentperiod, oldpolicy.getPolicy(currentperiod), ctx
        End If
    Next oldpolicy
End If

End Sub
'Tom added 12 July 2012
'Predicate to determine if a Rotation Policy is defined over a period.
Public Function policyDefined(period As String, policy As IRotationPolicy) As Boolean

If policy.getPolicyType = atomic Then
    policyDefined = True 'atomic policies are a-temporal, and exist regardless of period.
Else
    policyDefined = exists(policy.getPolicy(period))
End If

End Function

'Returns a filtered list of all the composite policies that have changed.
'We define change in a composite policy by the existence of both the new period and the old period in the
'composite policy.  Atomic policies are defined across all periods.  So the only ones that should show up here
'are policies that contained both the old and new, or current and new periods.
'This function is a predicate that effectively filters out composite policies that are undefined over both
'periods.
Public Function getChangedPolicies(currentperiod As String, newperiod As String, candidates As Dictionary) As Collection

Dim pname
Dim p As IRotationPolicy
Dim changed As Collection

Set changed = New Collection
'this pattern is tired.  I wish I had a collect/reduce in VBA...alas...
For Each pname In candidates
    Set p = candidates(pname)
    If policyDefined(currentperiod, p) And policyDefined(newperiod, p) Then changed.add p
Next pname

Set getChangedPolicies = changed
Set changed = Nothing

End Function
'Affects a change in policy.  This is currently only caused when periods change in a composite policy.  I'd really like to get more
'reactive behavior involved....
Public Sub alterUnitPolicies(subscribers As Dictionary, period As String, newpolicy As IRotationPolicy, context As timestep_SimContext)
Dim unitname
Dim unit As TimeStep_UnitData
'Dim currentPolicy As IRotationPolicy
                      
For Each unitname In subscribers
    Set unit = subscribers(unitname)
    queuePolicyChange unit, newpolicy, period, context
Next unitname

End Sub
'Queues a unit's status as having a pending policy change.  Right now, this is maintained in unit data.  When the
'unit has an opportunity to change policies, if it can't change immediately, it retains the policy change until the
'opportuntity arises.
'This could probably be in the unit level simulation.
Public Sub queuePolicyChange(unit As TimeStep_UnitData, newpolicy As IRotationPolicy, period As String, context As timestep_SimContext)
Dim atomicPolicy As IRotationPolicy
Dim currentpolicy As IRotationPolicy

Set currentpolicy = unit.policy

Set atomicPolicy = newpolicy.getPolicy(period)
unit.changePolicy atomicPolicy, context

'TOM Change 12 July 2012 -> I think the reference to the policy is unnecessary...TODO Test.
If unit.policy.name = atomicPolicy.name Then 'unit is now pointing at the right atomic policy.
    Set unit.policy = newpolicy 'ensures that the unit is back to following overarching the composite policy.
Else
    unit.policyStack.Remove 1 'remove the new policy...
    Set unit.policy = currentpolicy 'makes sure the unit continues following its current policy (until reset or next available policy change opportunity)
    unit.policyStack.add newpolicy 'queues the new policy for next available update.  If it's a composite policy, the unit will update with the active policy of the composite.
End If

End Sub

'Tom notes->
'Policy changes were encapsulated in the IRotationPolicy implementations.
'This assumed that units would change policies, regardless of event-context.
'That's actually a decent assumption.
'However, when we tell unitdata to change policy, it evokes a change state evaluation.
'Under the decoupled architecture, this requires simulation context.
'
'I'm going to have the policy ops define a function (really just adapted from policymanager),
'the passes the context needed.  This is in-line with other decoupled, functional representations.
'I have to rewire the IRotationPolicy implementation.....specifically taking out the onperiodchange
'event handling.
'Rather, we'll let policy ops take care of changing units' composite policies.  The good news is,
'all the bits are here.  We just need to re-organize the code.

'Pulled from TimeStep_CompositePolicy
'TOM Change 12 July 2012 ->


'TODO
'clears subscribers from non-permanent policies. WTF? check what non-permanent policies means.
Public Sub resetPolicies(policystore As TimeStep_StorePolicy)
Dim policy As IRotationPolicy
Dim pol

With policystore
    For Each pol In .policies
        If Not .permanents.exists(CStr(pol)) Then
            Set policy = .policies(pol)
            policy.subscribers.RemoveAll
            .policies.Remove pol
        End If
    Next pol
End With

End Sub
'reschedules the activation and deactivation events for known periods.
Public Sub resetPeriods(policystore As TimeStep_StorePolicy, ctx As timestep_SimContext)
Dim per
With policystore
    For Each per In .periods
        schedulePeriod .periods(per), ctx
    Next per
End With

End Sub
'Fetches a policy, by name, from the policystore.
Public Function getPolicy(policyname As String, policystore As TimeStep_StorePolicy) As TimeStep_Policy

If Not policystore.policies.exists(policyname) Then Err.Raise 101, , "Policy does not exist"

Set getPolicy = policystore.policies(policyname)

End Function


'get all pending policy updates.
Public Function getPolicyUpdates(t As Single, ctx As timestep_SimContext) As Dictionary
'Decoupled
Set getPolicyUpdates = SimLib.getUpdates(UpdateType.policy, t, ctx)
End Function

'clear all the locations from the policystore.
Public Sub clearLocations(policystore As TimeStep_StorePolicy)
Dim loc
Dim lname As String
With policystore
    For Each loc In .LocatiOnMap
        lname = CStr(loc)
        If isDemandLocation(lname) Then
            .LocationIndex.Remove .LocatiOnMap(loc)
            .locations.Remove loc
            .LocatiOnMap.Remove loc
        End If
    Next loc
End With

End Sub
Private Function isDemandLocation(locname As String) As Boolean
isDemandLocation = InStr(1, locname, "[") > 0
End Function


'Primitive wrapper for appending atomic policies to composite policies.
'TODO -> extend this to incorporate the semantics for generalized rotation policies....specifically, allow composite policies to be defined over composite policies
'as the intersection of periods across the policies.
Public Function appendComposite(composite As TimeStep_PolicyComposite, period As String, atomicPolicy As TimeStep_Policy) As TimeStep_PolicyComposite
Set appendComposite = composite
composite.addPolicy atomicPolicy, period
End Function
'This is the typical append operation.  When we compose an atomic policy with a composite, we simply register the atomic as a sub policy under the key period.
Private Function appendCompositeAtomic(composite As TimeStep_PolicyComposite, period As String, subPolicy As TimeStep_Policy) As TimeStep_PolicyComposite
Set appendCompositeAtomic = composite
composite.addPolicy subPolicy, period
End Function
'TODO -> relook this operation.  I think we might want it at some point...as it stands, composite policies are built from atomics.
'This is an extension to the append operation, which allows us to compose compositions....possibly of compositions!  We define the composition operator
Public Function appendCompositeComposite(composite As TimeStep_PolicyComposite, period As String, subPolicy As TimeStep_PolicyComposite) As TimeStep_PolicyComposite
'
'createComposite.addPolicy subPolicy, period
'
'
'
'Set appendCompositeComposite = composite
End Function
'Primitive constructore for composite policy.  To define a composite, we need at least one period and one atomic policy.
Public Function createComposite(policyname As String, period As String, atomicPolicy As TimeStep_Policy) As TimeStep_PolicyComposite
Set createComposite = New TimeStep_PolicyComposite
createComposite.name = policyname
Set createComposite = appendComposite(createComposite, period, atomicPolicy)
End Function
'creates a composite policy from n policies defined in periodpolicymap (a dictionary).
'periodpolicymap is assumed to be a map, where the keys are the names of periods over which
'the associated policy values are defined.  Using a dictionary/hashmap ensures that only unique
'values for period names are entered..We only need a dictionary<string,string>, or ::string->string
Public Function composePolicies(policyname As String, periodpolicymap As Dictionary, childpolicies As Dictionary) As TimeStep_PolicyComposite
Dim p
Dim childname As String
Dim pol As TimeStep_Policy

Set composePolicies = New TimeStep_PolicyComposite
For Each p In periodpolicymap
    childname = periodpolicymap(p)
    If Not childpolicies.exists(childname) Then Err.Raise 101, , "Policy does not exist, cannot compose a new policy"
    Set pol = childpolicies(childname)
    Set composePolicies = appendCompositeAtomic(composePolicies, CStr(p), pol)
    composePolicies.name = policyname
Next p

End Function

'Given a set of composite policy descriptions, and a set of child TimeStep Policies, produces a list
'of TimeStep_PolicyComposite derived from the compositions.
Public Function compositionsToComposites(compositions As Dictionary, childpolicies As Dictionary) As Collection

Dim cs As Collection
Dim c
Set cs = New Collection

For Each c In compositions
    cs.add composePolicies(CStr(c), compositions(c), childpolicies)
Next c
    
Set compositionsToComposites = cs
Set cs = Nothing

End Function
Private Function permanentRecord(rec As GenericRecord) As Boolean
permanentRecord = rec.fields("Period") = "Permanent"
End Function

'accesor for equivalency relations in a policystore
Public Function getEquivalencies(policystore As TimeStep_StorePolicy) As Dictionary
Set getEquivalencies = policystore.rules("Equivalencies")
End Function

'accessor for substitution relations in a policystore
Public Function getSubs(policystore As TimeStep_StorePolicy) As Dictionary
Set getSubs = policystore.rules("Substitutions")
End Function
'Adds an equivalence relationship to the policystore
Public Sub addEquivalence(recepient As String, donor As String, policystore As TimeStep_StorePolicy)
Dim eq As String
With getEquivalencies(policystore)
    eq = recepient & policystore.ruleDelim & donor
    If Not .exists(eq) Then .add eq, eq
End With

End Sub
'Adds a substitution relationship to the policystore
Public Sub addSubstitution(recepient As String, donor As String, cost As Single, _
                                policystore As TimeStep_StorePolicy)
Dim subst As String
With getSubs(policystore)
    subst = recepient & policystore.ruleDelim & donor
    'assoc
    If Not .exists(subst) Then .add subst, cost
End With

End Sub
'determine if the policystore has a registered rule
Public Function hasRule(rule As String, policystore As TimeStep_StorePolicy) As Boolean
hasRule = getSubs(policystore).exists(rule) Or getEquivalencies(policystore).exists(rule)
End Function
'Function to add relations to a policystore.  dispatches based on relation type.
Public Sub addRelation(policystore As TimeStep_StorePolicy, ByRef Relation As String, ByRef recepient As String, ByRef donor As String, Optional cost As Single)

Select Case Relation
    Case Is = "equivalence"
        addEquivalence recepient, donor, policystore
    Case Is = "sub"
        addSubstitution recepient, donor, cost, policystore
End Select

End Sub
'Assuming a list of (relation, recepient, donor, cost) entries, maps addRelation to each entry
Public Sub addRelations(relations As Collection, policystore As TimeStep_StorePolicy)
Dim entry As Collection
Dim cost As Single
For Each entry In relations
    addRelation policystore, entry(1), entry(2), entry(3), entry(4)
Next entry
End Sub
'Accessor function for the policies in the policystore
Public Function getPolicies(policystore As TimeStep_StorePolicy) As Dictionary
Set getPolicies = policystore.policies
End Function
'Get a list of the names of policies in the policy store.
Public Function policyNames(policystore As TimeStep_StorePolicy) As Collection
Set policyNames = DictionaryLib.listKeys(policystore.policies)
End Function
'Get a policy associated with Pname, relative to the policystore.
Public Function findPolicy(pname As String, policystore As TimeStep_StorePolicy) As IRotationPolicy
Set findPolicy = policystore.policies(pname)
End Function
'Return the set of policy graphs
Public Function getPolicyGraphs(policystore As TimeStep_StorePolicy) As Dictionary
Dim pol As TimeStep_Policy
Dim pols As Dictionary
Dim grph
Set getPolicyGraphs = New Dictionary

For Each pol In listVals(policystore.policies)
    getPolicyGraphs.add pol.name, pol.PositionGraph
Next pol

End Function

'If graphviz is installed, will render each policy as a directed graph, and build an HTML viewer for the graphs.
Public Function renderPolicies(policyDict As Dictionary) As String

'side effect
rendergraphs policyDict, "Policies.html"
'This is sloppy.
renderPolicies = ActiveWorkbook.path & "\" & "Policies.html"

End Function
Private Function compositeToRenderable(c As TimeStep_PolicyComposite) As Dictionary
Dim per
Dim p As TimeStep_Policy
Set compositeToRenderable = New Dictionary
For Each per In c.policies
    Set p = c.policies(per)
    compositeToRenderable.add CStr(per), p.PositionGraph
Next per

End Function
'If graphviz is installed, will render each policy as a directed graph, and build an HTML viewer for the graphs.
Public Function renderStoredPolicies(policystore As TimeStep_StorePolicy) As String
Dim p
Dim roto As IRotationPolicy
Dim c As TimeStep_PolicyComposite
Dim dots As Dictionary
Set dots = New Dictionary

For Each p In policystore.policies
    Set roto = policystore.policies(p)
    Set roto = policystore.policies(p)
    If roto.getPolicyType = composite Then
        Set c = roto
        dots.add CStr(p), nestedDot(CStr(p), compositeToRenderable(c))
        Set c = Nothing
    Else
        dots.add CStr(p), toDot(roto.PositionGraph)
    End If
Next p
renderStoredPolicies = vbNullString
renderDots dots

End Function

'TODO -> get this constructor back online.
'Rewire this....
'What we're really doing is building a policymanager from several sources...
'relations::     list<(relation, recepient, donor, cost)>
'periods::       list<genericperiod>
'atomicpolicies::list<TimeStep_Policy>
Public Function makePolicyStore(relations As Collection, periods As Collection, atomicpolicies As Collection, _
                                        Optional compositePolicies As Collection) As TimeStep_StorePolicy
Set makePolicyStore = New TimeStep_StorePolicy
With makePolicyStore
    addRelations relations, makePolicyStore
    addPeriods periods, makePolicyStore
    addPolicies atomicpolicies, makePolicyStore
    addPolicies compositePolicies, makePolicyStore
End With

End Function
'Reads in all of the default arforgen policies for testing
Public Function sampleAtomicPolicies() As Collection
Set sampleAtomicPolicies = listVals(MarathonPolicyCreation.DefaultArforgenPolicies)
End Function
'Creates a single composite policy, for testing purposes
Public Function sampleCompositePolicies(Optional policies As Dictionary) As Collection
Dim periods As Collection
Dim p As GenericPeriod
Dim policyperiodmap As Dictionary
Dim compositions As Dictionary

If policies Is Nothing Then Set policies = MarathonPolicyCreation.DefaultArforgenPolicies

Set policyperiodmap = newdict("PreSurge", "AC13", _
                              "Surge", "AC11", _
                              "PostSurge", "AC12")
                              
Set compositions = newdict("CompositeTest", policyperiodmap)
                              
Set sampleCompositePolicies = list(composePolicies("CompositeTest", compositions("CompositeTest"), policies))
                                 

End Function
'Generates a sample policy store for testing other functionality.
Public Function samplePolicyStore() As TimeStep_StorePolicy

Dim relations As Collection
Set relations = zip(list("Sub", "Sub", "Sub", "Sub"), _
                    list("A", "B", "C", "D"), _
                    list("D", "C", "B", "A"), _
                    list(2, 2, 2, 2))

Set samplePolicyStore = makePolicyStore(relations, PeriodLib.testPeriods(), _
                                        sampleAtomicPolicies(), sampleCompositePolicies())

End Function

'A simple test harness for the policy store and the policy sim.
'Thanks to the functional decomposition and decoupling...we can actually do this...
'Note the inclusion of a simple test harness, the debugging context.  awesome.
Public Sub testPolicySim()
Dim ps As TimeStep_StorePolicy
Dim ctx As timestep_SimContext
Set ctx = SimLib.makeDebugContext
Set ps = samplePolicyStore

'already added periods in sample.
    'addPeriods PeriodLib.testPeriods(), ps
'emits output....
schedulePeriods ps, ctx
While SimLib.hasTimeRemaining(ctx)
    ManagePolicies SimLib.advanceTime(ctx), ps, ctx
Wend


End Sub

'(Type    Relation    Donor   Recepient   Cost    Enabled)
Public Function recordToRelation(inrec As GenericRecord) As Collection
Set recordToRelation = fieldVals(inrec, "Relation", "Donor", "Recepient", "Cost")
End Function

'TODO -> implement this bad boy!
'(_   _    _  PolicyName  Template    MaxDwell    MinDwell    MaxBOG  StartDeployable StopDeployable  Overlap     Recovery    BOGBudget   Deltas  _)
Public Function recordToPolicy(inrec As GenericRecord) As TimeStep_Policy
Dim rdict As Dictionary
Dim keylist As Collection
Dim startdeployable As Long, stopdeployable As Long, overlap As Long, recovery As Long, bogbudget As Long, deltas As Dictionary

With inrec
    If .fields("Deltas") <> "{}" Then Set deltas = JSONtoDictionary(.fields("Deltas")) 'expand the deltas...
    If .fields("Template") = "Ghost" Then
        Set recordToPolicy = RegisterGhostTemplate(CStr(.fields("PolicyName")), CLng(.fields("MaxBOG")), CSng(.fields("Overlap")))
    Else
        Set recordToPolicy = RegisterTemplate(CStr(.fields("Template")), CLng(.fields("MaxDwell")), CLng(.fields("MinDwell")), _
                                              CLng(.fields("MaxBOG")), CLng(.fields("StartDeployable")), CLng(.fields("StopDeployable")), _
                                              CLng(.fields("Overlap")), deltas)
    End If
    recordToPolicy.name = CStr(.fields("PolicyName"))
End With

End Function
Public Function samplePolicyRecord() As GenericRecord
Set samplePolicyRecord = makeRecord("PolicyName", "AC12Strict", _
                                    "Template", "AC12", _
                                    "MaxDwell", 1095, _
                                    "MinDwell", 730, _
                                    "MaxBOG", 365, _
                                    "StartDeployable", 730, _
                                    "StopDeployable", 731, _
                                    "Overlap", 45, _
                                    "Recovery", 0, _
                                    "BOGBudget", 365, _
                                    "Deltas", "{}")

End Function

'generate a collection of relations from the table records
Public Function tableToRelations(tbl As GenericTable) As Collection
Dim rec As GenericRecord

Set tableToRelations = New Collection

While Not tbl.EOF
    Set rec = tbl.getGenericRecord
    If rec.fields("Enabled") Then tableToRelations.add recordToRelation(rec)
    tbl.moveNext
Wend

End Function
'generate a collection of atomic policies from the table records
Public Function tableToPolicies(tbl As GenericTable) As Collection
Dim rec As GenericRecord

Set tableToPolicies = New Collection

While Not tbl.EOF
    Set rec = tbl.getGenericRecord
    tableToPolicies.add recordToPolicy(rec)
    tbl.moveNext
Wend

End Function
'generate a dictionary of atomic policies from a table, where the keys are
'policy names.  Enforces unique policy names.
Public Function tableToPolicyDict(tbl As GenericTable) As Dictionary
Dim rec As GenericRecord
Dim p As TimeStep_Policy

Set tableToPolicyDict = New Dictionary

While Not tbl.EOF
    Set rec = tbl.getGenericRecord
    If Not tableToPolicyDict.exists(rec.fields("PolicyName")) Then
        Set p = recordToPolicy(rec)
        tableToPolicyDict.add p.name, p
    End If
    tbl.moveNext
Wend

End Function

'Generates a dictionary of composition rules from the table.
'Keys in the dictionary correspond to the name of the rule, and vals correspond to
'a map of period names to policy names/ policies.
Public Function tableToCompositions(tbl As GenericTable) As Dictionary
Dim rec As GenericRecord
Dim rulename As String
Dim entry As Collection
Dim composition As Dictionary

Set tableToCompositions = New Dictionary

While Not tbl.EOF
    Set rec = tbl.getGenericRecord
    rulename = rec.fields("CompositeName")
    If tableToCompositions.exists(rulename) Then
        Set composition = tableToCompositions.item(rulename)
    Else
        Set composition = New Dictionary
        tableToCompositions.add rulename, composition
    End If
    
    composition.add rec.fields("Period"), rec.fields("Policy")
    tbl.moveNext
Wend

End Function

'much more robust, uses the generictable interface to simplify loading.
Public Function tablesToPolicyStore(relationtable As GenericTable, periodtable As GenericTable, atomictable As GenericTable, compositeTable As GenericTable) As TimeStep_StorePolicy
Dim ps As TimeStep_StorePolicy

Set ps = New TimeStep_StorePolicy

addRelations tableToRelations(relationtable), ps
addPeriods PeriodLib.tableToPeriods(periodtable), ps
addDependentPolicies tableToPolicyDict(atomictable), tableToCompositions(compositeTable), ps

Set tablesToPolicyStore = ps
Set ps = Nothing

End Function
'Since composite policy loading is dependent on atomic policy loading, we provide an auxillary function
'to ensure the order is correct, and specify inputs.
'atomics is a map of policyname->Timestep_Policy,
'compositions is a map of policyname->(map of periodname->(either policyname or Timestep_Policy)
Public Sub addDependentPolicies(atomics As Dictionary, compositions As Dictionary, policystore As TimeStep_StorePolicy)
addPolicies listVals(atomics), policystore 'atomic policies must be added first.
addPolicies compositionsToComposites(compositions, atomics), policystore 'composite policeis added second.
End Sub
'Returns a policystore object initialized from default tables in Excel.  Mostly for compatibility.
Public Sub policyStoreFromExcel()
Dim ps As TimeStep_StorePolicy
Set ps = tablesToPolicyStore(getTable("RelationRecords"), _
                             getTable("PeriodRecords"), _
                             getTable("PolicyRecords"), _
                             getTable("CompositePolicyRecords"))
End Sub

Public Sub policyStoreFromFiles()
Dim tbl
Dim tblist As Collection
Dim ps As TimeStep_StorePolicy
Dim p As String
p = ActiveWorkbook.path & "\"

Set tblist = list("RelationRecords", "PeriodRecords", "PolicyRecords", "CompositePolicyRecords")
For Each tbl In tblist
    TableLib.saveTable getTable(CStr(tbl)), p & "\" & CStr(tbl) & ".json"
Next tbl
    
Set ps = tablesToPolicyStore(getTable(p & "RelationRecords" & ".json"), _
                             getTable(p & "PeriodRecords" & ".json"), _
                             getTable(p & "PolicyRecords" & ".json"), _
                             getTable(p & "CompositePolicyRecords" & ".json"))

End Sub
Public Function folderToPolicyStore(folderpath As String) As TimeStep_StorePolicy
Dim tbl
Dim tblist As Collection
Dim p As String
p = folderpath

Set tblist = list("RelationRecords", "PeriodRecords", "PolicyRecords", "CompositePolicyRecords")
Set folderToPolicyStore = tablesToPolicyStore(getTable(p & "RelationRecords" & ".json"), _
                             getTable(p & "PeriodRecords" & ".json"), _
                             getTable(p & "PolicyRecords" & ".json"), _
                             getTable(p & "CompositePolicyRecords" & ".json"))
                             
End Function


''TODO -> revisit this...separate the interface with the table from the creation of composites from the
''addition of composites to the policystore...
'
''We basically just build a nested dictionary defining the schedule.  Assumes we know the periods apriori
''Pull in the PolicySchedule from a generic table.  Default is a worksheet called PolicyScheduleRecords.
'Public Function getCompositePolicies(tbl As GenericTable) As Dictionary
'Dim myrecord As GenericRecord
'Dim policy
'
'Set getCompositePolicies = New Dictionary
'
'tbl.moveFirst
'While Not tbl.EOF
'    Set myrecord = tbl.getGenericRecord 'should contain three fields of interest: PolicySchedule, Policy, Period
'    CompositeFromRecord myrecord, composites
'    tbl.moveNext
'Wend
'
''record the composite policies as official policies, available for use by unit entities.
'For Each policy In composites
'    policies.add CStr(policy), composites(policy)
'Next policy
'
'End Function

''Read policy data from excel.  Currently, this is chugging all the substitution and equivalence data
''from excel.  Basically, we expect there to be a worksheet called Relations.
''fields -> Relation, Recepient, Donor, Cost
'Public Sub GetRelations(relations As Worksheet)
'Dim rng As Range
'Dim rw As Range
'Dim fld As Range
'Dim j As Long
'Dim fields As Dictionary
'Set fields = New Dictionary
'
'Set rng = relations.Cells(1, 1)
'Set rng = rng.CurrentRegion
'Set rw = rng.rows(1)
'For j = 1 To rw.Columns.count
'    fields.add rw.Cells(1, j).value, j
'Next j
'
'Set rng = rng.offset(1, 0)
'If rng.rows.count > 1 Then
'    Set rng = rng.resize(rng.rows.count - 1, rng.Columns.count)
'
'    For Each rw In rng.rows
'        relationFromRow rw, fields
'    Next rw
'End If
'
'Set fields = Nothing
'
'End Sub
''TOM Note 22 MAr 2011 -> this is sloppy.  We should assign fields dynamically.  currently
''hardcoded.  Bad form.
'Private Sub relationFromRow(row As Range, fields As Dictionary)
'Dim tmp()
'tmp = row.value
'If tmp(1, fields("Enabled")) Then
'    Select Case Trim(tmp(1, fields("Relation")))
'        Case Is = "equivalence"
'            addEquivalence CStr(tmp(1, fields("Recepient"))), CStr(tmp(1, fields("Donor")))
'        Case Is = "sub"
'            addSubstitution CStr(tmp(1, fields("Recepient"))), CStr(tmp(1, fields("Donor"))), CSng(tmp(1, fields("Cost")))
'        Case Else
'            Err.Raise 101, , "UnKnown Relation"
'    End Select
'End If
'
'End Sub




'''intermediate function that reads a record of
'''(Type    CompositeName   Period  Policy), and produces a list that defines one relation between a
'''composite policy, a period, and an atomic policy.
''Public Function recordToComposition(inrec As GenericRecord) As Collection
''Set recordToComposition = fieldVals(inrec, "Type", "CompositeName", "Period", "Policy")
''End Function


