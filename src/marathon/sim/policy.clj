(ns marathon.sim.policy)
;----------TEMPORARILY ADDED for marathon.sim.demand!
(declare register-location atomic-name find-period)

;'This is the companion module to the TimeStep_ManagerOfPolicy class.  The primary functions contained
;'herein surround the management of an abstract policy context in the simulation, which is embodied in
;'the TimeStep_ManagerOfPolicy data - or the policystore.  Most of the functions here deal with creating
;'policies, composing policies, registering policies with the policystore, registering simulation periods
;'with the policystore, and managing policies and periods during the simulation.
;
;                                '***What is a policy context?
;'Policy is vital because it determines the criteria for both the eligibility and suitability of supply
;'to fill demand.  A conservative policy may provide few opportunities for units to fill demand, limiting
;'the deployable supply (presumably for some benefit like reduced costs), while a liberal policy may
;'allow units to deploy almost any time, eliminating the previous limitations on supply (likely at
;'some cost).  Marathon generally examines policies across the spectrum, from liberal to conservative.
;'Policy contexts may not be homogenous (applicable to every unit); there may be individual, custom
;'policies for each unique unit entity.  Additionally, policy contexts may change in response to
;'conditions in the simulation;  It may be appropriate to have a liberal policy when demand is low,
;'presumably minimizing the cost to maintain a large available supply, while shifting to a liberal
;'policy during periods of higher demand.  In some cases, there are known strategic events that
;'prompt the change of rotational policy, typically due to increased demand for units, which prompts
;'a shift to a different, less constrained policy.  Conversely, after demand has receded, the
;'simulation typically tries to return to the pre-existing policy context, or an intermediate policy
;'that re-establishes stability in the rotational supply.  All of this behavior is scriptable, and can
;'be modified by changing input data rather than rewiring logic.
;
;'Marathon has historically maintained a notion of an overarching policy context throughout the
;'simulation.  In fact, the simulation timeline is typically viewed as a mapping of policy periods to
;'segments of time.  Periods typically serve to segment the timeline into epochs that are easily
;'categorized, and may trigger changes in the policy context.  For instance, the most common period
;'set is a sequential #{Pre-Surge, Surge, Post-Surge} set, where Pre-Surge covers the simulation time
;'in which demand is "low", Surge is a particularly high demand that occurs over a short time, and
;'Post-Surge is the remainder of the time, categorized as a lower demand but more intense than the
;'Pre-Surge period.  Technically, a timeline in Marathon can have any different number of periods,
;'not just the common Pre/Surge/Post periods.  The ability to specify periods, and then dictate
;'policy changes in reponse to active periods, is a powerful mechanism for parameterizing the
;'simulation and managing varied policy contexts.  Marathon is currently structured with one default
;'timeline, which is composed of N contiguous periods.  As the periods change, any policies that
;'depend upon a period being active are engaged, and units subscribing to these policies alter their
;'behavior accordingly.  This is how Marathon manages a policy context that can either be simple
;'and uniform (i.e. one policy, one period), or very unique (N different policies, K periods).
;
;                            '***What are policies?
;'Policies describe a set of states and durations that -usually- conform to a rotational policy.
;'Policies are very important, as they serve as the instruction set for unit entity behavior.
;
;'Unit entities interpret policies with their unit behavior, in which the meaning of the states
;'in the policy is interpreted and acted upon.  Additionally, unit behaviors consult a policy to
;'figure out what the next state should be, how long the unit will stay in said state, etc.
;'In the language of Finite State Machines, policies describe the states that a machine
;'can be in, as well as the state transitions that occur. Typical states include Bogging, Dwelling,
;'Waiting, Moving, StartCycle, EndCycle, Spawning, etc.  All the policy has to do is specify which
;'state a unit is in at a particular policy position, specify a starting and stopping position, and
;'indicate the transition time between state changes.  Policies can have at most one cycle, and are
;'typically represented as a Directed Graph.
;
;'Note -> unit behaviors are typically implemented statically, as actual classes in VBA, where
;'policies are much more flexible and can be specified as data.  More information on unit behaviors
;'can be found in the MarathonOpUnit module, and the TimeStep_UnitBehavior[x] classes.
;
;'Next to supply and demand, policy is the most variable higher-order data.  As such, we
;'need a robust, flexible way to specify different policies easily, so that rather than
;'changing code, an end-user can simply modify the data that describes the policy (not unlike a script)
;'and effect a change in unit entity behavior easily and transparently.
;
;'One way to provide flexibility is to use a template system, where users can derive a policy from
;'an existing template, and then apply a set of parameters to transform or "mold" the policy into
;'a desired form.  Marathon currently has several built-in templates, and a language for specifying
;'policy templates is very near on the horizon.  Existing policy templates are located in the
;'MarathonPolicyCreation and MarathonPolicy modules.  The PolicyCreation module serves as a central
;'dispatch for invoking policy templates, applying modifications, and deriving new policies.  The
;'MarathonPolicy module contains the actual structure for each policy template, as well as routines
;'that provide default policies for multiple contexts (rather than reading data).
;
;'Technically, anything that implements the IRotationPolicy interface can serve as a policy, so
;'developers can extend policy responsibilities to many different implementations.
;'Marathon provides two implementations of IRotationPolicy, which work in tandem to fulfill a composite
;'design pattern.  The current implementation, TimeStep_Policy, and TimeStep_PolicyComposite, provide a
;'flexible mechanism for defining policies and composing multiple policies together.  They both
;'implement the IRotationPolicy interface, albeit differently.  The key to their flexibility is that
;'composite policies are defined in terms of Atomic policies.  So users can, without changing any
;'logic, add new policy definitions by supplying data that describes how to combine pre-existing
;'policies.
;
;                            '***Atomic Policies
;'Normal TimeStep_Policy objects are effectively Atomic policies, in that they represent a single set
;'of instructions that describe a rotational policy.  They do not change.  Therefore, if a unit entity
;'subscribes to such an atomic policy, it will always follow the same set of instructions.
;
;                            '***Composite Policies
;'Composite policies represent an association of one or more Atomic policies.  This allows us to
;'capitalize on the atomic policies, and express new policies as simple compositions of
;'N atomic policies.
;
;'The current implementation of composite policies was built under the notion of a simple labeling
;'or association of a set of sub policies, where a period label serves as an index to an associated
;'atomic policy.  Technically, periods are just strings or text and can be any valid value.  By
;'convention, periods refer to a particular epoch in the simulation, and serve as a way of labeling
;'the simulation timeline.  As a consequence, we can define composite policies as a composition of
;'atomic policies, labelled by period, which will then automatically change the governing policy
;'as the simulation period changes.  Note, if the periods that the composite policy are labelled by
;'never occur in the simulation, then only the periods that intersect the simulation will ever be used.
;'This is useful for describing possible behaviors, where the periods correspond to corner-cases that
;'may be triggered by external events.
;
;Option Explicit
;'Helper sub to partially fill in fields for the more generic RequestUpdate sub.
;Public Sub requestPolicyUpdate(t As Single, context As TimeStep_SimContext)
;SimLib.requestUpdate t, "PolicyManager", UpdateType.policy, , context
;End Sub
;
;'TODO -> We need to build in some glue code in the CoreSimulation...
;''The previously coupled code has addPeriod adding the period to the policy store AND
;''sending out event notifications (scheduling).  We now schedule in a separate function.
;''As a consequence, addPeriod doesn't require a simulation context (doesn't need to
;''send events)
;
;'Allows the addition of known periods of time....
;Public Sub addPeriod(policystore As TimeStep_ManagerOfPolicy, per As GenericPeriod)
;With policystore
;    If Not .periods.exists(per.name) Then
;        .periods.add per.name, per
;    Else
;        Err.Raise 101, , "Period already exists, check for duplicates"
;    End If
;End With
;
;End Sub
;
;'Import a list of periods into the policystore, where each period is a GenericPeriod
;'object.
;Public Sub addPeriods(periods As Collection, policystore As TimeStep_ManagerOfPolicy)
;Dim p As GenericPeriod
;For Each p In periods
;    addPeriod policystore, p
;Next p
;End Sub
;
;'Notifies interested parties of the beginning and ending of a period.
;'Schedules a policy update to coincide with the beginning and ending of the period,
;'so that policy management runs when a period changes.
;Public Sub schedulePeriod(per As GenericPeriod, ctx As TimeStep_SimContext)
;
;With per
;    SimLib.triggerEvent TimeStep_Msg.AddedPeriod, .name, .name, "Added period " & .toString, , ctx
;    requestPolicyUpdate .fromday, ctx
;    requestPolicyUpdate .today, ctx
;End With
;End Sub
;    
;'Added 14 Jul 2012 -> further decoupling of event notifications.  Adding a period has nothing to do with scheduling a
;'set of known periods.  We now schedule the periods in a separate step.
;Public Sub schedulePeriods(policystore As TimeStep_ManagerOfPolicy, ctx As TimeStep_SimContext)
;Dim per
;
;With policystore
;    For Each per In .periods
;        schedulePeriod .periods(per), ctx
;    Next
;End With
;
;End Sub
;'Fetch the period(s) that intersect time t.
;''TOM Change 4 Jan 2011 -> note the memoization, this saves us calls if we already calculated the period.
;Public Function FindPeriod(t As Single, policystore As TimeStep_ManagerOfPolicy) As String
;'function that returns the period name within the interval that is set in the
;'ARFORGEN Parameters worksheet
;Dim name As String
;Dim i As Long
;Dim per
;Dim pd As GenericPeriod
;Static fixed As Single
;fixed = Fix(t)
;
;If policystore.memoized.exists("FindPeriod_" & t) Then
;    FindPeriod = policystore.memoized("FindPeriod_" & t)
;Else
;    For Each per In policystore.periods
;        Set pd = policystore.periods(per)
;        With pd
;            If fixed >= Fix(.fromday) And fixed <= Fix(.today) Then
;                FindPeriod = .name
;                Exit For
;            End If
;        End With
;    Next per
;    If FindPeriod = vbNullString Then
;        FindPeriod = "None"
;    End If
;    policystore.memoized.add "FindPeriod_" & t, FindPeriod
;End If
;
;End Function
;
;'Returns the the period currently active in the policy store.  This may change when I introduce
;'multiple timelines....
;Public Function getActivePeriod(policystore As TimeStep_ManagerOfPolicy) As GenericPeriod
;Set getActivePeriod = policystore.activePeriod
;End Function
;
;'simple indexing function.  We index starting from 101....although we really don't
;'have to out of necessity.  I might revisit this.  Vestigial and inherited from old, poor designs.
;Public Function nextlocationID(locationcount As Long) As Long
;nextlocationID = locationcount + 101
;End Function
;
;'This sub helps us to keep track of demand and policy locations.
;'The need for indexing is debatable, and is likely vestigial.
;Public Sub registerLocation(locname As String, policystore As TimeStep_ManagerOfPolicy)
;Dim nxtid As Long
;With policystore
;    If .LocatiOnMap.exists(locname) Then
;        'Err.Raise 101, ,  "Location already exists"
;    Else
;        nxtid = nextlocationID(.LocatiOnMap.count)
;        .LocatiOnMap.add locname, nxtid
;        .LocationIndex.add nxtid, locname
;        'TOM Change 7 June 2011
;        SetLib.addSet .locations, locname
;    End If
;End With
;
;End Sub
;'Register multiple locations in the locs collection with the policystore.
;Public Sub registerLocations(locs As Collection, policystore As TimeStep_ManagerOfPolicy)
;Dim loc
;For Each loc In locs
;    registerLocation CStr(loc), policystore
;Next loc
;End Sub
;'Register canonical ARFORGEN locations with the policystore.
;Private Sub DefaultArforgenLocations(policystore As TimeStep_ManagerOfPolicy)
;registerLocations list(available, ready, train, reset, deployed, Overlapping), policystore
;End Sub
;'Derives locations from the policy.
;'each location in a policy should be registered in the locations dictionary.
;Private Function getPolicyLocations(policy As IRotationPolicy) As Collection
;Dim node
;
;Set getPolicyLocations = New Collection
;
;With policy.PositionGraph
;    For Each node In .nodes
;        getPolicyLocations.add (CStr(node))
;    Next node
;End With
;
;End Function
;
;'Policy Store operations.....
;'Perform a reduction over a list of default policies....adding each one to the policystore.
;Public Sub addDefaults(policystore As TimeStep_ManagerOfPolicy)
;Dim pol As TimeStep_Policy
;
;addPolicies MarathonPolicy.DefaultArforgenPolicies, policystore  'register our default set of policies.
;addPolicies MarathonPolicy.TFPolicies, policystore 'Additional policies added by Trudy F.
;addPolicies MarathonPolicy.FFGPolicies, policystore 'Future Force Gen policies.
;
;'RegisterLocations 'Ensure all locations are registered with policy manager.
;For Each pol In policystore.policies
;    policystore.permanents.add CStr(pol.name), 0
;Next pol
;
;End Sub
;'method for adding atomic policies to the policystore.
;Public Sub addPolicy(ByRef policy As TimeStep_Policy, policystore As TimeStep_ManagerOfPolicy)
;
;With policystore
;    assert policy.name <> vbNullString, True
;    If Not .policies.exists(policy.name) Then
;        .policies.add policy.name, policy
;        registerLocations getPolicyLocations(policy), policystore
;    Else
;        Err.Raise 101, , "Policy already exists"
;    End If
;End With
;
;End Sub
;'Adds a composite policy to the policystore.  Special, because we keep track of composite policies for
;'special consideration during management of the policy context.
;Public Sub addPolicyComposite(ByRef policy As TimeStep_PolicyComposite, policystore As TimeStep_ManagerOfPolicy)
;
;With policystore
;    If Not .policies.exists(policy.name) Then
;        .policies.add policy.name, policy
;        .composites.add policy.name, policy
;        registerLocations getPolicyLocations(policy), policystore
;    Else
;        Err.Raise 101, , "Policy already exists"
;    End If
;End With
;
;End Sub
;'Adds a sequential policy to the policystore.  Special, because we keep track of composite policies for
;'special consideration during management of the policy context.
;Public Sub addPolicySequential(ByRef policy As TimeStep_PolicySequential, policystore As TimeStep_ManagerOfPolicy)
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
;'adds a list of either atomic or composite policies to a policystore.
;Public Sub addPolicies(policies As Collection, policystore As TimeStep_ManagerOfPolicy)
;Dim p As TimeStep_Policy
;Dim c As TimeStep_PolicyComposite
;Dim s As TimeStep_PolicySequential
;
;Dim r As IRotationPolicy
;
;For Each r In policies
;    Select Case r.getPolicyType
;        Case atomic
;            Set p = r
;            addPolicy p, policystore
;        Case composite
;            Set c = r
;            addPolicyComposite c, policystore
;        Case sequential
;            Set s = r
;            addPolicySequential s, policystore
;        Case Else
;            Err.Raise 101, , "Unknown policy type!"
;    End Select
;Next r
;
;Set r = Nothing
;Set s = Nothing
;Set c = Nothing
;Set p = Nothing
;
;End Sub
;
;''TOM NOTE 7 Jun 2011 -> These are both vestigial functions.  We're not even using locationID
;'anymore.  Recommend scrapping them to cut down on the code bloat.
;'Wrapper for getting our locationID
;Public Function locationID(locname As String, policystore As TimeStep_ManagerOfPolicy) As Long
;locationID = CLng(policystore.LocatiOnMap(locname))
;End Function
;
;'Wrapper for getting our LocationName assocated with a locationID from a policystore.
;Public Function LocationName(locationID As Long, policystore As TimeStep_ManagerOfPolicy) As String
;LocationName = CStr(policystore.LocationIndex(locationID))
;End Function
;
;'Shorthand for triggering a period change on the event stream of a simulation context.
;Public Sub periodChangeEvent(fromname As String, toname As String, ctx As TimeStep_SimContext)
;SimLib.triggerEvent periodChange, "PolicyManager", toname, "Period changed from " & fromname & " to " & toname, , ctx
;End Sub
;
;'This is the primary routine to manage policies for a policy store.
;'TOM Change 7 Jun 2011 -> use manage policies to prosecute period changes.
;'TOM Change 04 Jan 2011 -> This is going to be obsolete. All of our default policies will be coded, but most
;'of them will be derived from data.
;'TOM Change 03 Jan 2011
;'TOM TODO -> Alter the underyling subs here to manifest as actual "policy changes"
;'TOM Change 9 Sep 2012, added var for toname
;Public Sub ManagePolicies(day As Single, state As TimeStep_SimState, Optional toname As String)
;Dim fromname As String
;Dim policystore As TimeStep_ManagerOfPolicy, ctx As TimeStep_SimContext
;
;Set policystore = state.policystore
;Set ctx = state.context
;
;With policystore
;    fromname = .activePeriod.name
;    If toname = vbNullString Then toname = FindPeriod(day, policystore)
;    If .activePeriod.name <> toname Then
;        SimLib.triggerEvent TimeStep_Msg.updateAllUnits, .name, .name, _
;            "Period change from " & fromname & " to " & toname & " caused all units to update.", toname, ctx
;        If toname = "Final" Then
;            Set .activePeriod = New GenericPeriod
;            .activePeriod.name = "Final"
;            .activePeriod.fromday = day
;            .activePeriod.today = day
;        Else
;            Set .activePeriod = .periods(toname)
;        End If
;        periodChangeEvent fromname, toname, ctx
;        'TOM Change disabled temporarily.
;        changePolicies fromname, toname, policystore, ctx
;    End If
;End With
;
;End Sub
;
;'This routine informs subscribers of the need to try to change their policies at the next available
;'opportunity.  Only happens for composite policies, when a new, valid period has been engaged.
;
;'Tom Change 12 July 2011
;'tell each policy to have its subscriber change to a new policy.
;'Simple algorithm -> fetch the new policy associated with the period.
;'Tell each policy to change to the new policy.
;Private Sub changePolicies(currentperiod As String, newperiod As String, policystore As TimeStep_ManagerOfPolicy, _
;                              ctx As TimeStep_SimContext)
;
;Dim oldpolicy As IRotationPolicy
;'Dim subscribers As Dictionary
;
;Dim newname As String
;Dim policy
;
;If currentperiod <> "Initialization" Then
;    'only composite policies can change.
;    For Each oldpolicy In getChangedPolicies(currentperiod, newperiod, policystore.composites)
;            'replace with change subscribers...
;'            Set subscribers = oldpolicy.subscribers
;'            alterUnitPolicies oldpolicy.subscribers, currentperiod, oldpolicy.getPolicy(currentperiod), ctx
;            'TOM Change 27 Sep 2012 -> also changed to reflect period indexing.
;            'alterUnitPolicies oldpolicy.subscribers, currentperiod, oldpolicy, ctx
;            alterUnitPolicies oldpolicy.subscribers, newperiod, oldpolicy, ctx
;            'reversed the order here....we don't change the active policy in the oldpolicy until after all units have
;            'had a chance to change, or queue to a new change.
;            oldpolicy.onPeriodChange newperiod 'update the policy. 'TOM Note -> this should advance the active policy of the
;                                   'oldpolicy.
;
;            'there's basically a context here...
;            'oldpolicy(currentperiod) -> oldpolicy(newperiod)
;            'should be current atomic -> should be next atomic
;
;    Next oldpolicy
;End If
;
;End Sub
;'Tom added 12 July 2012
;'Predicate to determine if a Rotation Policy is defined over a period.
;Public Function policyDefined(period As String, policy As IRotationPolicy) As Boolean
;
;If policy.getPolicyType = atomic Then
;    policyDefined = True 'atomic policies are a-temporal, and exist regardless of period.
;Else
;    policyDefined = exists(policy.getPolicy(period))
;End If
;
;End Function
;
;'Returns a filtered list of all the composite policies that have changed.
;'We define change in a composite policy by the existence of both the new period and the old period in the
;'composite policy.  Atomic policies are defined across all periods.  So the only ones that should show up here
;'are policies that contained both the old and new, or current and new periods.
;'This function is a predicate that effectively filters out composite policies that are undefined over both
;'periods.
;Public Function getChangedPolicies(currentperiod As String, newperiod As String, candidates As Dictionary) As Collection
;
;Dim pname
;Dim p As IRotationPolicy
;Dim changed As Collection
;
;'tom change 30 aug 2012
;If currentperiod <> "Initialization" Then
;    Set changed = New Collection
;    'this pattern is tired.  I wish I had a collect/reduce in VBA...alas...
;    For Each pname In candidates
;        Set p = candidates(pname)
;        If p.subscribers.count > 0 Then
;            If policyDefined(currentperiod, p) And policyDefined(newperiod, p) Then
;                changed.add p
;                pprint p.AtomicName
;            End If
;        End If
;    Next pname
;End If
;
;Set getChangedPolicies = changed
;Set changed = Nothing
;End Function
;'Affects a change in policy.  This is currently only caused when periods change in a composite policy.  I'd really like to get more
;'reactive behavior involved....
;Public Sub alterUnitPolicies(subscribers As Dictionary, period As String, newpolicy As IRotationPolicy, context As TimeStep_SimContext)
;Dim unitname
;Dim unit As TimeStep_UnitData
;'Dim currentPolicy As IRotationPolicy
;                      
;For Each unitname In subscribers
;    Set unit = subscribers(unitname)
;    queuePolicyChange unit, newpolicy, period, context
;Next unitname
;
;End Sub
;'Queues a unit's status as having a pending policy change.  Right now, this is maintained in unit data.  When the
;'unit has an opportunity to change policies, if it can't change immediately, it retains the policy change until the
;'opportuntity arises.
;'This could probably be in the unit level simulation.
;Public Sub queuePolicyChange(unit As TimeStep_UnitData, newpolicy As IRotationPolicy, period As String, context As TimeStep_SimContext)
;Dim atomicPolicy As IRotationPolicy
;Dim currentpolicy As IRotationPolicy
;
;Set currentpolicy = unit.policy.getActivePolicy
;
;Set atomicPolicy = newpolicy.getPolicy(period)
;unit.changePolicy atomicPolicy, context
;
;'TOM Change 12 July 2012 -> I think the reference to the policy is unnecessary...TODO Test.
;If unit.policy.name = atomicPolicy.name Then 'unit is now pointing at the right atomic policy.
;    Set unit.policy = newpolicy 'ensures that the unit is back to following overarching the composite policy.
;Else
;    unit.policyQueue.Remove 1 'remove the new policy...
;    Set unit.policy = currentpolicy 'makes sure the unit continues following its current policy (until reset or next available policy change opportunity)
;    unit.policyQueue.add newpolicy 'queues the new policy for next available update.  If it's a composite policy, the unit will update with the active policy of the composite.
;End If
;
;End Sub
;
;'Tom notes->
;'Policy changes were encapsulated in the IRotationPolicy implementations.
;'This assumed that units would change policies, regardless of event-context.
;'That's actually a decent assumption.
;'However, when we tell unitdata to change policy, it evokes a change state evaluation.
;'Under the decoupled architecture, this requires simulation context.
;'
;'I'm going to have the policy ops define a function (really just adapted from policymanager),
;'the passes the context needed.  This is in-line with other decoupled, functional representations.
;'I have to rewire the IRotationPolicy implementation.....specifically taking out the onperiodchange
;'event handling.
;'Rather, we'll let policy ops take care of changing units' composite policies.  The good news is,
;'all the bits are here.  We just need to re-organize the code.
;
;'Pulled from TimeStep_CompositePolicy
;'TOM Change 12 July 2012 ->
;
;
;'TODO
;'clears subscribers from non-permanent policies. WTF? check what non-permanent policies means.
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
;Public Sub resetPeriods(policystore As TimeStep_ManagerOfPolicy, ctx As TimeStep_SimContext)
;Dim per
;With policystore
;    For Each per In .periods
;        schedulePeriod .periods(per), ctx
;    Next per
;End With
;
;End Sub
;'Fetches a policy, by name, from the policystore.
;Public Function getPolicy(policyname As String, policystore As TimeStep_ManagerOfPolicy) As TimeStep_Policy
;
;If Not policystore.policies.exists(policyname) Then Err.Raise 101, , "Policy does not exist"
;
;Set getPolicy = policystore.policies(policyname)
;
;End Function
;
;'get all pending policy updates for time t.
;Public Function getPolicyUpdates(t As Single, ctx As TimeStep_SimContext) As Dictionary
;'Decoupled
;Set getPolicyUpdates = SimLib.getUpdates(UpdateType.policy, t, ctx)
;End Function
;
;'clear all the locations from the policystore.
;Public Sub clearLocations(policystore As TimeStep_ManagerOfPolicy)
;Dim loc
;Dim lname As String
;With policystore
;    For Each loc In .LocatiOnMap
;        lname = CStr(loc)
;        If isDemandLocation(lname) Then
;            .LocationIndex.Remove .LocatiOnMap(loc)
;            .locations.Remove loc
;            .LocatiOnMap.Remove loc
;        End If
;    Next loc
;End With
;
;End Sub
;'Simple predicate to determine if a location is actually a demand.
;'Demands, upon creation, have their start and duration encoded as
;'a textual range: [1..blah], so a predicate only needs to check for
;'a left bracket.  This is somewhat brittle, and dependent upon the
;'demand representation never changing, but it works for now and
;'is relatively easy to patch.
;Private Function isDemandLocation(locname As String) As Boolean
;isDemandLocation = InStr(1, locname, "[") > 0
;End Function
;
;'Primitive wrapper for appending atomic policies to composite policies.
;'TODO -> extend this to incorporate the semantics for generalized rotation policies....specifically, allow composite policies to be defined over composite policies
;'as the intersection of periods across the policies.
;Public Function appendComposite(composite As TimeStep_PolicyComposite, period As String, atomicPolicy As TimeStep_Policy) As TimeStep_PolicyComposite
;Set appendComposite = composite
;composite.addPolicy atomicPolicy, period
;End Function
;'This is the typical append operation.  When we compose an atomic policy with a composite, we simply register the atomic as a sub policy under the key period.
;Private Function appendCompositeAtomic(composite As TimeStep_PolicyComposite, period As String, subPolicy As TimeStep_Policy) As TimeStep_PolicyComposite
;Set appendCompositeAtomic = composite
;composite.addPolicy subPolicy, period
;End Function
;
;'Currently, composite policies are built purely from atomic policies.  In theory, one could compose a new
;'policy from atomic OR composite policies.  This is currently not implemented or tested.
;
;'TODO -> relook this operation.  I think we might want it at some point...as it stands, composite policies are built from atomics.
;'This is an extension to the append operation, which allows us to compose compositions....possibly of compositions!  We define the composition operator
;Public Function appendCompositeComposite(composite As TimeStep_PolicyComposite, period As String, subPolicy As TimeStep_PolicyComposite) As TimeStep_PolicyComposite
;'
;'createComposite.addPolicy subPolicy, period
;'
;'
;'
;'Set appendCompositeComposite = composite
;End Function
;'Primitive constructore for composite policy.  To define a composite, we need at least one period and one atomic policy.
;Public Function createComposite(policyname As String, period As String, atomicPolicy As TimeStep_Policy) As TimeStep_PolicyComposite
;Set createComposite = New TimeStep_PolicyComposite
;createComposite.name = policyname
;Set createComposite = appendComposite(createComposite, period, atomicPolicy)
;End Function
;'creates a composite policy from n policies defined in periodpolicymap (a dictionary).
;'periodpolicymap is assumed to be a map, where the keys are the names of periods over which
;'the associated policy values are defined.  Using a dictionary/hashmap ensures that only unique
;'values for period names are entered..We only need a dictionary<string,string>, or ::string->string
;Public Function composePolicies(policyname As String, periodpolicymap As Dictionary, childpolicies As Dictionary) As TimeStep_PolicyComposite
;Dim p
;Dim childname As String
;Dim pol As TimeStep_Policy
;
;Set composePolicies = New TimeStep_PolicyComposite
;For Each p In periodpolicymap
;    childname = periodpolicymap(p)
;    If Not childpolicies.exists(childname) Then Err.Raise 101, , "Policy does not exist, cannot compose a new policy"
;    Set pol = childpolicies(childname)
;    Set composePolicies = appendCompositeAtomic(composePolicies, CStr(p), pol)
;    composePolicies.name = policyname
;Next p
;
;End Function
;'A constructor for building a sequential policy from a collection of policies.  The sequential policy will
;'represent an ordered list of policies, drawn from child policies.  At this point, only atomic policies may
;'be members of a sequence.
;Public Function sequencePolicies(policyname As String, policysequence As Collection, childpolicies As Dictionary) As TimeStep_PolicySequential
;Dim p
;Dim childname As String
;Dim pol As TimeStep_Policy
;
;Set sequencePolicies = New TimeStep_PolicySequential
;sequencePolicies.name = policyname
;For Each p In policysequence
;    childname = CStr(p)
;    If Not childpolicies.exists(childname) Then Err.Raise 101, , "Policy does not exist, cannot compose a new policy"
;    Set pol = childpolicies(childname)
;    sequencePolicies.addPolicy pol
;Next p
;
;End Function
;
;'Given a set of composite policy descriptions, and a set of child TimeStep Policies, produces a list
;'of TimeStep_PolicyComposite derived from the compositions.
;Public Function compositionsToComposites(compositions As Dictionary, childpolicies As Dictionary) As Collection
;
;Dim cs As Collection
;Dim c
;Set cs = New Collection
;
;For Each c In compositions
;    If isSequential(compositions(c)) Then
;        cs.add sequencePolicies(CStr(c), compositions(c), childpolicies)
;    Else
;        cs.add composePolicies(CStr(c), compositions(c), childpolicies)
;    End If
;Next c
;    
;Set compositionsToComposites = cs
;Set cs = Nothing
;
;End Function
;Private Function isSequential(inval As Variant) As Boolean
;isSequential = TypeName(inval) = "Collection"
;End Function
;Private Function permanentRecord(rec As GenericRecord) As Boolean
;permanentRecord = rec.fields("Period") = "Permanent"
;End Function
;
;'accesor for equivalency relations in a policystore
;Public Function getEquivalencies(policystore As TimeStep_ManagerOfPolicy) As Dictionary
;Set getEquivalencies = policystore.rules("Equivalencies")
;End Function
;
;'accessor for substitution relations in a policystore
;Public Function getSubs(policystore As TimeStep_ManagerOfPolicy) As Dictionary
;Set getSubs = policystore.rules("Substitutions")
;End Function
;'Adds an equivalence relationship to the policystore
;Public Sub addEquivalence(recepient As String, donor As String, policystore As TimeStep_ManagerOfPolicy)
;Dim eq As String
;With getEquivalencies(policystore)
;    eq = recepient & policystore.ruleDelim & donor
;    If Not .exists(eq) Then .add eq, eq
;End With
;
;End Sub
;'Adds a substitution relationship to the policystore
;Public Sub addSubstitution(recepient As String, donor As String, cost As Single, _
;                                policystore As TimeStep_ManagerOfPolicy)
;Dim subst As String
;With getSubs(policystore)
;    subst = recepient & policystore.ruleDelim & donor
;    'assoc
;    If Not .exists(subst) Then .add subst, cost
;End With
;
;End Sub
;'determine if the policystore has a registered rule
;Public Function hasRule(rule As String, policystore As TimeStep_ManagerOfPolicy) As Boolean
;hasRule = getSubs(policystore).exists(rule) Or getEquivalencies(policystore).exists(rule)
;End Function
;'Function to add relations to a policystore.  dispatches based on relation type.
;Public Sub addRelation(policystore As TimeStep_ManagerOfPolicy, ByRef Relation As String, ByRef recepient As String, ByRef donor As String, Optional cost As Single)
;
;Select Case Relation
;    Case Is = "equivalence"
;        addEquivalence recepient, donor, policystore
;    Case Is = "sub"
;        addSubstitution recepient, donor, cost, policystore
;End Select
;
;End Sub
;'Assuming a list of (relation, recepient, donor, cost) entries, maps addRelation to each entry
;Public Sub addRelations(relations As Collection, policystore As TimeStep_ManagerOfPolicy)
;Dim entry As Collection
;Dim cost As Single
;For Each entry In relations
;    addRelation policystore, entry(1), entry(2), entry(3), entry(4)
;Next entry
;End Sub
;'Accessor function for the policies in the policystore
;Public Function getPolicies(policystore As TimeStep_ManagerOfPolicy) As Dictionary
;Set getPolicies = policystore.policies
;End Function
;'Get a list of the names of policies in the policy store.
;Public Function policynames(policystore As TimeStep_ManagerOfPolicy) As Collection
;Set policynames = DictionaryLib.listKeys(policystore.policies)
;End Function
;'Get a policy associated with Pname, relative to the policystore.
;Public Function findPolicy(pname As String, policystore As TimeStep_ManagerOfPolicy) As IRotationPolicy
;Set findPolicy = policystore.policies(pname)
;End Function
;'Return the set of policy graphs
;Public Function getPolicyGraphs(policystore As TimeStep_ManagerOfPolicy) As Dictionary
;Dim pol As TimeStep_Policy
;Dim pols As Dictionary
;Dim grph
;Set getPolicyGraphs = New Dictionary
;
;For Each pol In listVals(policystore.policies)
;    getPolicyGraphs.add pol.name, pol.PositionGraph
;Next pol
;
;End Function
;
;'TODO -> get this constructor back online.
;'Rewire this....
;'What we're really doing is building a policymanager from several sources...
;'relations::     list<(relation, recepient, donor, cost)>
;'periods::       list<genericperiod>
;'atomicpolicies::list<TimeStep_Policy>
;Public Function makePolicyStore(relations As Collection, periods As Collection, atomicpolicies As Collection, _
;                                        Optional compositePolicies As Collection, Optional store As TimeStep_ManagerOfPolicy) As TimeStep_ManagerOfPolicy
;
;If store Is Nothing Then Set store = New TimeStep_ManagerOfPolicy
;With store
;    addRelations relations, store
;    addPeriods periods, store
;    addPolicies atomicpolicies, store
;    addPolicies compositePolicies, store
;End With
;
;Set makePolicyStore = store
;
;End Function
;
;'TOM Change 10 SEP 2012
;Public Sub initializePolicyStore(policystore As TimeStep_ManagerOfPolicy, context As TimeStep_SimContext)
;'schedule periods
;schedulePeriods policystore, context
;End Sub
;
;'tom Change 10 Sep 2012
;Public Function getDictionary(instring As String) As Dictionary
;
;If Mid(instring, 1, 5) = "#JSON" Then
;    Set getDictionary = JSONtoDictionary(Mid(instring, 6))
;Else
;    Set getDictionary = reval(instring)
;End If
;
;End Function
;
;'Since composite policy loading is dependent on atomic policy loading, we provide an auxillary function
;'to ensure the order is correct, and specify inputs.
;'atomics is a map of policyname->Timestep_Policy,
;'compositions is a map of policyname->(map of periodname->(either policyname or Timestep_Policy)
;Public Sub addDependentPolicies(atomics As Dictionary, compositions As Dictionary, policystore As TimeStep_ManagerOfPolicy)
;addPolicies listVals(atomics), policystore 'atomic policies must be added first.
;addPolicies compositionsToComposites(compositions, atomics), policystore 'composite policeis added second.
;End Sub
;
;''TODO -> revisit this...separate the interface with the table from the creation of composites from the
;''addition of composites to the policystore...
;'
;''We basically just build a nested dictionary defining the schedule.  Assumes we know the periods apriori
;''Pull in the PolicySchedule from a generic table.  Default is a worksheet called PolicyScheduleRecords.
;'Public Function getCompositePolicies(tbl As GenericTable) As Dictionary
;'Dim myrecord As GenericRecord
;'Dim policy
;'
;'Set getCompositePolicies = New Dictionary
;'
;'tbl.moveFirst
;'While Not tbl.EOF
;'    Set myrecord = tbl.getGenericRecord 'should contain three fields of interest: PolicySchedule, Policy, Period
;'    CompositeFromRecord myrecord, composites
;'    tbl.moveNext
;'Wend
;'
;''record the composite policies as official policies, available for use by unit entities.
;'For Each policy In composites
;'    policies.add CStr(policy), composites(policy)
;'Next policy
;'
;'End Function
;
;''Read policy data from excel.  Currently, this is chugging all the substitution and equivalence data
;''from excel.  Basically, we expect there to be a worksheet called Relations.
;''fields -> Relation, Recepient, Donor, Cost
;'Public Sub GetRelations(relations As Worksheet)
;'Dim rng As Range
;'Dim rw As Range
;'Dim fld As Range
;'Dim j As Long
;'Dim fields As Dictionary
;'Set fields = New Dictionary
;'
;'Set rng = relations.Cells(1, 1)
;'Set rng = rng.CurrentRegion
;'Set rw = rng.rows(1)
;'For j = 1 To rw.Columns.count
;'    fields.add rw.Cells(1, j).value, j
;'Next j
;'
;'Set rng = rng.offset(1, 0)
;'If rng.rows.count > 1 Then
;'    Set rng = rng.resize(rng.rows.count - 1, rng.Columns.count)
;'
;'    For Each rw In rng.rows
;'        relationFromRow rw, fields
;'    Next rw
;'End If
;'
;'Set fields = Nothing
;'
;'End Sub
;''TOM Note 22 MAr 2011 -> this is sloppy.  We should assign fields dynamically.  currently
;''hardcoded.  Bad form.
;'Private Sub relationFromRow(row As Range, fields As Dictionary)
;'Dim tmp()
;'tmp = row.value
;'If tmp(1, fields("Enabled")) Then
;'    Select Case Trim(tmp(1, fields("Relation")))
;'        Case Is = "equivalence"
;'            addEquivalence CStr(tmp(1, fields("Recepient"))), CStr(tmp(1, fields("Donor")))
;'        Case Is = "sub"
;'            addSubstitution CStr(tmp(1, fields("Recepient"))), CStr(tmp(1, fields("Donor"))), CSng(tmp(1, fields("Cost")))
;'        Case Else
;'            Err.Raise 101, , "UnKnown Relation"
;'    End Select
;'End If
;'
;'End Sub
;
;
;
;
;'''intermediate function that reads a record of
;'''(Type    CompositeName   Period  Policy), and produces a list that defines one relation between a
;'''composite policy, a period, and an atomic policy.
;''Public Function recordToComposition(inrec As GenericRecord) As Collection
;''Set recordToComposition = fieldVals(inrec, "Type", "CompositeName", "Period", "Policy")
;''End Function


