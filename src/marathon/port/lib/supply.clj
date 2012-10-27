(ns marathon.port.supply)

'marathonopsupply
'11 July 2012 -> recasting of supply management.
'We define a supply simulation as a set of operations on supply simulation state.
'It's basically a decoupling of the earlier object hierarchy.
'Instead of encapsulating everything in the supply manager class, we're pulling out as much of
'the methods as possible, and providing a functional interface to modify supply managers.

'The end result is a lower-order supply manager that handles little to no internal functions, and
'manages some state that we need.  All the operations for pushing supply, in the context of a simulation
'are maintained here.

Option Explicit
'TOM Change->
'   This is serving as a template for reorganizing the simulation.
'   The desire is to separate operations from data.
'   We have multiple levels of operations....
'   This library groups several levels of operations along the Supply domain.
'   The primary function is the ManageSupply function....
'   Manage supply eats core data....

'TOM Change 24 April 2012 -> decoupled the getUpdates....now we pass in a list of updates from
'outside (usually via the engine), rather than having supplymanager need visibility on it.
Public Sub ManageSupply(supply As TimeStep_StoreSupply, day As Single, context As timestep_SimContext)
Dim update
Dim unit As TimeStep_UnitData
Dim startloc As String
Dim finloc As String
Dim packet As TimeStep_UpdatePacket

With supply
    'find pending supply updates for today
    Set .todayupdates = getSupplyUpdates(day, context)
    'for each update packet
    For Each update In .todayupdates
        Set packet = .todayupdates(update)
        
        'Decoupled
        'TODO -> see if we need to pass in tags explicitly.
        If isEnabled(.tags, packet.requestedby) Then 'filters out inactive (not being simulated) units.
            Set unit = .unitmap(packet.requestedby) 'grab the requested unit
            'update the unit relative to the time of request
            
            startloc = unit.LocationName
            'Decouple -> might need some decoupling here....
            'TODO replace unit.update with something cleaner from unitsim
            'Decoupled for now.
            Set unit = unit.update(packet.Elapsed(day, SimLib.lastupdate(unit.name, context)), context)
            'msg = "Updated Unit " & unit.name
            'TOM CHANGE 1 july 2011 -> looooots of dictionary allocations here, changed!
            'parent.trigger SupplyUpdate, name, unit.name, msg, , , supplyPacket(unit.name)
            'Decouple
            If supply.Verbose Then
                requestUnitUpdate day, unit, context
            Else
    '            triggerEvent supplyUpdate, name, unit.name, msg & " " & unit.getStats, , context 'supplyPacket(unit.name)
                supplyUpdateEvent supply, unit, "Updated Unit " & unit.name & " " & unit.getStats, context
            End If
                    
        End If
    Next update
End With

End Sub

Public Sub requestSupplyUpdate(t As Single, unit As TimeStep_UnitData, context As timestep_SimContext)
SimLib.requestUpdate t, "SupplyManager", UpdateType.supply, , context
End Sub

Public Sub spawnUnitEvent(unit As TimeStep_UnitData, context As timestep_SimContext)
SimLib.triggerEvent TimeStep_Msg.spawnunit, unit.name, unit.name, "Spawned Unit " & unit.name, , context
End Sub

Public Sub spawnGhostEvent(unit As TimeStep_UnitData, context As timestep_SimContext)
SimLib.triggerEvent TimeStep_Msg.SpawnGhost, unit.name, unit.name, "Spawned a ghost", , context
End Sub
Public Function SupplyfromExcel(policystore As TimeStep_StorePolicy, parameters As TimeStep_Parameters, behaviors As TimeStep_ManagerOfBehavior, _
                                    ctx As timestep_SimContext, Optional ensureGhost As Boolean) As TimeStep_StoreSupply
Dim tbl As GenericTable
Dim gunit As TimeStep_UnitData

Set SupplyfromExcel = New TimeStep_StoreSupply
'TODO -> turn this into a function.
UnitsFromSheet "SupplyRecords", SupplyfromExcel, behaviors, parameters, policystore, ctx

If ensureGhost Then
    If Not SupplyfromExcel.hasGhosts Then
        Set gunit = createUnit("Auto", "Ghost", "Anything", "Ghost", 0, "Auto", parameters, policystore)
        Set gunit = associateUnit(gunit, SupplyfromExcel, ctx)
        registerUnit SupplyfromExcel, behaviors, gunit, True, ctx
        Debug.Print "Asked to do requirements analysis without a ghost, " & _
            "added Default ghost unit to unitmap in supplymanager."
    End If
End If

End Function
Public Function NewUnit(supply As TimeStep_StoreSupply, parameters As TimeStep_Parameters, policystore As TimeStep_StorePolicy, behaviors As TimeStep_ManagerOfBehavior, name As String, src As String, OITitle As String, component As String, _
                            cycletime As Single, policy As String, Optional behavior As IUnitBehavior)
Dim unit As TimeStep_UnitData
Set unit = createUnit(name, src, OITitle, component, cycletime, policy, parameters, policystore, behavior)
Set supply = registerUnit(supply, behaviors, unit)

End Function

'This is decoupled fairly well.
Public Function registerUnit(supply As TimeStep_StoreSupply, behaviors As TimeStep_ManagerOfBehavior, unit As TimeStep_UnitData, Optional ghost As Boolean, Optional context As timestep_SimContext) As TimeStep_StoreSupply

With supply
    .unitindex.add .unitindex.count + 1, unit.name
    .unitmap.add unit.name, unit
    If unit.behavior Is Nothing Then Set unit = assignBehavior(behaviors, unit, vbNullString) 'unit's behavior can be set ahead of time,else
    'will resort to default behaviors by component. (currently they're identical).
    tagUnit supply, unit 'add default tags for this unit
    
    If ghost = False Then
        'Decouple
        'triggerEvent TimeStep_Msg.SpawnUnit, unit.name, name, "Spawned Unit " & unit.name, , context
        spawnUnitEvent unit, context
        UpdateDeployStatus supply, unit, , , context
    Else
        'Decouple
        'triggerEvent SpawnGhost, name, name, "Spawned a ghost", , context
        spawnGhostEvent unit, context
        .hasGhosts = True
    End If
    
    addSRC supply, unit.src
End With

Set registerUnit = supply
End Function

'Encapsulate.
'this might be suitable to keep in the supplymanager...
Public Sub addSRC(supply As TimeStep_StoreSupply, src As String)
With supply
    If Not .SRCsinscope.exists(src) Then .SRCsinscope.add src, .SRCsinscope.count
End With
End Sub
'Encapsulate.
'this might be suitable to keep in the supplymanager...
Public Sub removeSRC(supply As TimeStep_StoreSupply, src As String)
With supply
    If .SRCsinscope.exists(src) Then .SRCsinscope.Remove (src)
End With

End Sub
'Encapsulate.
'this might be suitable to keep in the supplymanager...
Public Sub UpdateDeployStatus(supply As TimeStep_StoreSupply, uic As TimeStep_UnitData, _
                                Optional followon As Boolean, Optional spawning As Boolean, _
                                    Optional context As timestep_SimContext)

If followon = False Then
    UpdateDeployability uic, supply.DeployableBuckets, followon, spawning, context
Else
    UpdateDeployability uic, getFollowonBucket(supply.followonbuckets, uic.followoncode), followon, spawning, context
End If

End Sub
'Encapsulate? -> nah, it's already independent.
Private Function getFollowonBucket(followonbuckets As Dictionary, followoncode As String) As Dictionary

If followoncode = vbNullString Then
    Err.Raise 101, , "No followon code! Units elligble for followon should have a code!"
Else
    With followonbuckets
        If .exists(followoncode) Then
            Set getFollowonBucket = .item(followoncode)
        Else
            Set getFollowonBucket = New Dictionary
            .add followoncode, getFollowonBucket
        End If
    End With
End If

End Function
'TOM Change
'Sub operates on uics to register them as deployable with a dictionary of dictionaries
'Dictionary<Rule, <UICName,Unitdata>>
'UICs are tracked by a unique string name now, changed the unitdata structure to reflect this.
'This sub is only called when necessary, updates the available set, and limits the search effort required
'to find an available uic.
'UpdateDeployability also enables/disables nodes in the ruleset of the policymanager.
'Basically, if the deployable bucket is emptied (i.e. we update a unit's deployability, removing it
'from any number of buckets,
Private Sub UpdateDeployability(uic As TimeStep_UnitData, buckets As Dictionary, Optional followon As Boolean, _
                                    Optional spawning As Boolean, Optional context As timestep_SimContext)

Dim stock As Dictionary
Dim packet As Dictionary

'TODO -> rip out all the stuff from unitdata, particularly CanDeploy and friends...it's a step above
'a rat's nest.
With uic
        If .PositionPolicy = vbNullString Then 'Tom Change 24 May
            Err.Raise 101, , "invalid position!"  'Tom Change 24 May
        'TOM Change 24 April 2012 -> using a single notion of deployable now, derived from the unit.
        'ElseIf .policy.Deployable(.PositionPolicy) = True Or followon Then 'Tom Change 24 May
        'Note how the old check just used the position in the policy to determine deployability.
        'Well, we want to incorporate a stricter notion of deployable, in that we also account for
        'bogbudget and whatnot....The problem is that during spawning, we don't initialize cycles
        'off the bat, so one of the new criteria fails (bogbudget is 0).
        
        'CHECK -> maybe yank .canDeploy out of the uic's methods, make it more functional
        ElseIf .CanDeploy(spawning) Or followon Then
            If Not followon Then
                'Decoupled
                 SimLib.triggerEvent NewDeployable, .name, .name, _
                                "Unit " & .name & " at position " & .PositionPolicy & " is deployable", , context
            Else
                'Decoupled
                 SimLib.triggerEvent NewFollowOn, .name, .name, _
                    "Unit " & .name & " able to followon for demandgroup " & .followoncode, , context
            End If


            If buckets.exists(.src) Then
                'Decoupled
                 SimLib.triggerEvent MoreSRCAvailable, .name, .src, _
                    "Unit " & .name & " at position " & .PositionPolicy & _
                        " has just been added to deployables for SRC " & .src, , context

                Set stock = buckets.item(.src)
                'TOM change 21 july 2011
                If Not stock.exists(.name) Then stock.add .name, uic
            Else
                'Decoupled
                 SimLib.triggerEvent NewSRCAvailable, .name, .src, _
                    "A new category of SRC now has deployable supply " & .src, , context

                'TOM note this may be allocating alot.
                Set stock = New Dictionary
                stock.add .name, uic
                buckets.add .src, stock
                'Tom Change 24 May
                'Decoupled
                 SimLib.triggerEvent NewDeployable, .name, .name, _
                    "Unit " & .name & " at position " & .PositionPolicy & _
                        " has just been added to deployables for SRC " & .src, , context
            End If
        Else 'unit is not deployable

            'Decoupled
            SimLib.triggerEvent NotDeployable, .name, .name, _
                "Unit " & .name & " at position " & .PositionPolicy & " is no longer deployable", , context

            If buckets.exists(.src) = True Then
                Set stock = buckets.item(.src)
                If stock.exists(.name) Then stock.Remove (.name)
                If stock.count = 0 Then
                    'Decoupled
                     SimLib.triggerEvent outofstock, .name, .src, _
                        "SRC " & .src & " has 0 deployable supply", "SOURCE_" & .src, context
                    Set stock = Nothing
                    buckets.Remove (.src) 'mutation!
                End If
            End If
        End If
End With


End Sub

Private Function CanDeploy(unit As TimeStep_UnitData) As Boolean

CanDeploy = unit.policy.isDeployable(unit.cycletime)

End Function
Sub DeployUnit(supply As TimeStep_StoreSupply, unit As TimeStep_UnitData, t As Single, sourcetype As String, _
                demand As TimeStep_DemandData, bog As Long, fillcount As Long, fill As TimeStep_Fill, deploydate As Date, _
                    Optional isFollowon As Boolean, Optional context As timestep_SimContext, Optional location As Long)
''TOM Note 23 Mar 2011 -> we'll have to revisit this, big time.
''if a unit is found, this sub deploys the unit
Dim FromLocation As String
Dim ToLocation As String
Dim FromPosition As String
Dim ToPosition As String

With unit
    'TODO -> .validdeployer and friends are an abomination.  Need to rip that stuff out of unitdata....
    'TOM Change 3 Jan 2011 -> ported this to a string name convention, we need this for plotting values.
    If .validDeployer Then
        FromLocation = .LocationName 'Tom Note <- this is wrong, not being updated.
        FromPosition = .PositionPolicy 'Tom Change 24 May

        ToLocation = demand.name
        'TOM Change June 6 2011 -> Temporary bypass....
        ToPosition = "Deployed" '.policy.deployed 'Tom Change 24 May

        'demand.qualityDeployed = sourceType 'not certain about this......can we calculate this?
        'demand.unitsDeployed = unit.Index
        'mutation!
        demand.Assign unit 'Tom Change 14 Mar 2011, new method in demand class.

        'TOM Change 7 Jun 2011 -> modified this to reflect movement in location space.
        'TOM note 24 May -> Need to bifurcate the move logs into 2 as well, Position Change, Spatial Change.
        'LogMove t, FromLocation, tolocation, unit, unit.policy.MaxBOG

        'TOM note 24 May -> Determine where we want to record the deployment happening from....
        'Decoupled
        LogDeployment t, FromLocation, demand, unit, fillcount, fill, deploydate, context

        'TOM Note -this may be an error...switch cycletime to dwell.  I think i did this already.
        .dwellTimeWhenDeployed = .cycletime
        'TOM Change 24 April 2012 -> included extra criteria that bogbudget > 0


        'set location array to the demand Key value
        'Decoupled
        '.changeLocation demand.name, context 'Mutation!
        MarathonOpUnit.changeLocation unit, demand.name, context
        
        '.LocationName = demand.name
        'Decoupled
        'TODO consider pushing this into unitsim.changelocation...does it need to be here?
        .location = location 'parent.policymanager.locationID(.LocationName)

        'TOM change 6 June 2011 -> this was causing a problem with deployability....one of the casualties of
        'the split between location and policy position.
        'TODO consider pushing this into unitsim.changelocation...does it need to be here?
        .PositionPolicy = ToPosition

        If isFollowon Then
            'Decoupled
            recordFollowon supply, unit, demand, context
            MarathonOpUnit.keepBoggingUntilDepleted unit, context
            resetFollowOn unit

        Else
            'TOM Note 6 June 2011 -> this implies a side effect, where ChangeState requests a unitupdate.
            'Update the unit's State to reflect it's now bogging .
            'TOM Note 24 May 2011 -> changestate to bogging should change the policy position for us.
'            'Redundant.
'            .CurrentCycle.deployments = .CurrentCycle.deployments + 1
            'Decouple
            ''.ChangeState "Bogging", t - SimLib.lastupdate(.name), .policy.MaxBOG - .policy.overlap
            MarathonOpUnit.wakeAndBogUntilDepleted unit, t, context
        End If
        
        'factored out redundancy.
        'More expressive.
        '.CurrentCycle.deployments = .CurrentCycle.deployments + 1
        MarathonOpUnit.incrementDeployments unit
        'Decoupled
        UpdateDeployability unit, supply.DeployableBuckets, followon, False, context

        'TOM Change 14 July 2011

''        triggerEvent supplyUpdate, supply.name, unit.name, msg, , context
        'Decoupled
        'Higher order
        supplyUpdateEvent supply, unit, , context
        'TOM Change 6 DEC 2010
        'TOM Change 25 Mar 2011 -> we no longer need to do this for each deployed unit.  Fill is
        'done in batches now.
        'parent.demandmanager.UpdateFill day, demand.name, parent.demandmanager.UnfilledQ
    'TOM Change 24 april -> Ensures that units are valid, according the criteria established 24 april
    Else
        Err.Raise 101, , "Unit is not a valid deployer! Must have bogbudget > 0, cycletime in " & _
                            "deployable window, or be eligible or a followon deployment"
    End If
End With

End Sub
'Notifies the context of a supply update.
Public Function supplyUpdateEvent(supply As TimeStep_StoreSupply, unit As TimeStep_UnitData, Optional msg As String, Optional context As timestep_SimContext)
triggerEvent supplyUpdate, supply.name, unit.name, msg, , context
End Function
'TODO
'Might put this in unitsim...
'Marks the unit as ineligible for follow ons, until a new code is marked.
Public Sub resetFollowOn(unit As TimeStep_UnitData)
unit.followoncode = vbNullString
End Sub

'process the unused follow-on units, changing their policy to complete cycles.
Public Sub ReleaseFollowOns(supply As TimeStep_StoreSupply, Optional context As timestep_SimContext)
Dim nm
Dim unitptr As TimeStep_UnitData

For Each nm In supply.followOns
    Set unitptr = supply.followOns(nm)
    removeFollowOn supply, unitptr
    'Tom change 18 July 2012
    'We allow the units to pass through a reentry state, to see if they can recover and re-enter
    'the available pool, rather than pushing them straight to a Reset state.
    'unitptr.ChangeState "Reset", 0
    'change the unit's position in its policy to enable possible reentry
    '  even if the policy does not have an explicit reentry state.
    unitptr.PositionPolicy = "ReEntry"
    unitptr.ChangeState "ReEntry", 0
    UpdateDeployStatus supply, unitptr, False, False, context
Next nm

End Sub
Private Sub removeFollowOn(supply As TimeStep_StoreSupply, unit As TimeStep_UnitData)
Dim ptr As Dictionary
Dim removal As Boolean
Dim fcode As String

fcode = unit.followoncode

supply.followOns.Remove unit.name
With getFollowonBucket(supply.followonbuckets, fcode)
    Set ptr = .item(unit.src)
    ptr.Remove unit.name
    If ptr.count = 0 Then .Remove (unit.src)
    If .count = 0 Then removal = True
End With

'mutating the follow on buckets, this is okay...
If removal Then supply.followonbuckets.Remove fcode

'unit.followoncode = vbNullString
resetFollowOn unit
End Sub
'announce that the unit is in fact following on, remove it from the followons list.
Private Sub recordFollowon(supply As TimeStep_StoreSupply, unit As TimeStep_UnitData, demand As TimeStep_DemandData, Optional context As timestep_SimContext)
removeFollowOn supply, unit
'Decoupled
'triggerEvent FollowingOn, unit.name, demand.name, "Unit " & unit.name & " is following on to demand " & demand.name, , context
unitFollowOnEvent unit, demand, context
End Sub
'When a unit engages in a followon deployment, we notify the event context.
'Simple declarative event description for wrapping low level followon event notification.
Public Sub unitFollowOnEvent(unit As TimeStep_UnitData, demand As TimeStep_DemandData, Optional context As timestep_SimContext)
triggerEvent FollowingOn, unit.name, demand.name, "Unit " & unit.name & " is following on to demand " & demand.name, , context
End Sub
'TODO -> formalize dependencies and pre-compilation checks....
'already decoupled.
Public Function CanSimulate(supply As TimeStep_StoreSupply) As Boolean
CanSimulate = supply.tags.getSubjects("Enabled").count > 0
End Function


'TOM Change -> removed due to slowdown
''Public Function supplyPacket(unitname As String) As Dictionary
''Set supplyPacket = New Dictionary
''supplyPacket.add "Updated", unitname
''End Function

'''get all pending supply updates.
'TOM Change 24 April 2012
'get all pending supply updates.
Public Function getSupplyUpdates(t As Single, context As timestep_SimContext) As Dictionary
'Decoupled
Set getSupplyUpdates = context.updater.getUpdates(UpdateType.supply, t)
End Function


'Decoupled.

'procedure that allows us to, using the fillgraph, derive a set of tags whose associated units
'should be deactivated.  if removal is true, the units will be removed from memory as well.
'in cases where there are a lot of units, this may be preferable
Public Sub scopeSupply(supply As TimeStep_StoreSupply, disableTags As Dictionary, Optional removal As Boolean)
Dim subjects As Dictionary
Dim subject As String
Dim subj
Dim tg

With supply
    For Each tg In disableTags
        Set subjects = .tags.getSubjects(CStr(tg))
        For Each subj In subjects
            subject = CStr(subj)
            If .unitmap.exists(subject) Then
                disable .tags, subject
                If removal Then removeUnit supply, subject
            End If
        Next subj
    Next tg
End With

End Sub
'TOM Note 27 Mar 2011 -> we might have to clean up other tables too...
'Decoupled.
Public Sub removeUnit(supply As TimeStep_StoreSupply, unitname As String)

Static unitptr As IVolatile

With supply.unitmap
    If .exists(unitname) Then
        Set unitptr = .item(unitname)
        removeSRC supply, .item(unitname).src
        .Remove (unitname)
        unitptr.Terminate
    End If
End With

End Sub
'TOM Change 3 Jan 2011
'aux function for logging/recording the fact that a unit changed locations
'TODO -> it'd be nice to figure out how to unify this reporting, right now LogMove gets to
'reach directly into the tables of outputmanager and manipulate. This is fast and simple, but it's not
'pure ....
'One current problem is -> we have to transform fromloc/toloc into something palatable for trending ...
Public Sub LogMove(t As Single, fromloc As String, toloc As String, unit As TimeStep_UnitData, _
                        Optional duration As Single, Optional context As timestep_SimContext)

SimLib.triggerEvent UnitMoved, unit.name, toloc, "", unit, context
'TODO -> check to see if this is equivalent.  I think it is.  If so, prefer.
'MarathonOpUnit.unitMovedEvent unit, toloc, context

End Sub

'TODO -> This should be renamed like positionEvent or something.
'Main dependencies are in the unit Behaviors.
'Unit behaviors currently use parent to refer to a supply manager.
'We can probably do better than this.
'Actually, unit behaviors aren't maintaining any state....
'So we can probably just plug them in as modules....they're all pure functions.

'TOM Change 6 June 2011 -> Added logging for unit positioning specifically..
Public Sub LogPosition(t As Single, frompos As String, topos As String, unit As TimeStep_UnitData, Optional duration As Single, Optional context As timestep_SimContext)


'If SupplyTraffic Then
    'Decouple
    triggerEvent PositionUnit, "SupplyManager", unit.name, _
        "UIC " & unit.name & " has repositioned from " & frompos & " to " & topos, , context  'record the move
'End If

End Sub

'TOM change 3 Jan 2011
'aux function for logging/recording the fact that a unit deployed
'transfer 1 item
Public Sub LogDeployment(t As Single, fromname As String, demand As TimeStep_DemandData, unit As TimeStep_UnitData, _
                            fillcount As Long, fill As TimeStep_Fill, deploydate As Date, Optional context As timestep_SimContext)

Dim newrec As GenericRecord
Dim trendtarget As String
'TOM Change 4 Jan 2011

Dim toname As String
'TOM Change 4 Jan 2011
toname = demand.name  'TOM note -> this points to individual instances of the demand. We could produce an aggregated

'TOM Change
'msg =

'Decoupled
triggerEvent deploy, "SupplyManager", unit.name, "Deployed unit " & unit.name & " from " & fromname & " to demand " & toname, _
             newdict("Unit", unit, "Demand", demand, _
                     "Fill", fill, "FillCount", fillcount, _
                     "Period", parent.policyManager.FindPeriod(t), _
                     "t", t, "DeployDate", deploydate), context

End Sub
Public Function isGhost(tags As GenericTags, unit As TimeStep_UnitData) As Boolean
isGhost = tags.hasTag("Ghost", unit.name)
End Function
Public Function assignBehavior(behaviors As TimeStep_ManagerOfBehavior, unit As TimeStep_UnitData, behaviorname As String) As TimeStep_UnitData
Set assignBehavior = behaviors.assignBehavior(unit, behaviorname)
End Function

'TODO -> replace this call to a direct call for getTime, from the simlib module.
Public Function getTime(context As timestep_SimContext) As Single
'Decouple
'getTime = parent.CurrentTime
getTime = SimLib.getTime(context)
End Function
'Unit can request an update at a specified time ....
Public Sub requestUnitUpdate(t As Single, unit As TimeStep_UnitData, Optional context As timestep_SimContext)
'Decoupled
SimLib.requestUpdate t, unit.name, UpdateType.supply, , context
End Sub
'inject appropriate tags into the GenericTags
Public Sub tagUnit(supply As TimeStep_StoreSupply, unit As TimeStep_UnitData, Optional extras As Dictionary)
'Set tmptags = New Dictionary
Dim sourcename As String

With unit
    sourcename = "SOURCE_" & .src
    supply.tags.multiTag .name, "COMPO_" & .component, "BEHAVIOR_" & .behavior.name, _
        "TITLE_" & .OITitle, "POLICY_" & .policy.name, sourcename, "Enabled"
End With

tagSource supply.tags, sourcename

End Sub
Public Function getSources(supply As TimeStep_StoreSupply) As Dictionary
Set getSources = supply.tags.getSubjects("Sources")
End Function
Public Sub tagSource(tags As GenericTags, source As String)
tags.addTag "Sources", source
End Sub
Public Function isEnabled(supplytags As GenericTags, unitname As String) As Boolean
isEnabled = supplytags.hasTag("Enabled", unitname)
End Function
Public Sub enable(supplytags As GenericTags, unitname As String)
supplytags.addTag "Enabled", unitname
End Sub
Public Sub disable(supplytags As GenericTags, unitname As String)
supplytags.removeTag "Enabled", unitname
End Sub
Public Sub addBucket(supply As TimeStep_StoreSupply, bucket As String)
Dim ptr As Dictionary
With supply
    If Not .DeployableBuckets.exists(bucket) Then
        Set ptr = New Dictionary
        .DeployableBuckets.add bucket, ptr
    Else
        Set ptr = .DeployableBuckets(bucket)
    End If
End With

End Sub
'TODO -> update calls to this guy in behaviors.  Should fail, currently.
'sub to register a set of units that need to be utilized, or sent to reset.
Public Sub addFollowOn(supply As TimeStep_StoreSupply, unit As TimeStep_UnitData, _
                        context As timestep_SimContext)

supply.followOns.add unit.name, unit
UpdateDeployStatus supply, unit, True, , context

End Sub
Public Function lastupdate(unitname As String, ctx As timestep_SimContext) As Single
'Decoupled
lastupdate = SimLib.lastupdate(unitname, ctx)
End Function

Public Function multipleGhosts(supplytags As GenericTags) As Boolean
multipleGhosts = supplytags.getSubjects("SOURCE_Ghost").count > 1
End Function

Public Sub fromExcel(supplystore As TimeStep_StoreSupply, policystore As TimeStep_StorePolicy, _
                        parameters As TimeStep_Parameters, behaviors As TimeStep_ManagerOfBehavior, _
                            ctx As timestep_SimContext, Optional ensureGhost As Boolean)

Dim gunit As TimeStep_UnitData

UnitsFromSheet "SupplyRecords", supplystore, behaviors, parameters, policystore, ctx

If ensureGhost Then
    If Not supplystore.hasGhosts Then
        Set gunit = createUnit("Auto", "Ghost", "Anything", "Ghost", 0, "Auto", parameters, policystore)
        'Decoupled
        Set gunit = associateUnit(gunit, supplystore, ctx)
        'decoupled
        Set supplystore = registerUnit(supplystore, behaviors, gunit, True, ctx)
        Debug.Print "Asked to do requirements analysis without a ghost, " & _
            "added Default ghost unit to unitmap in supplymanager."
    End If
End If

End Sub
Public Sub UnitsFromSheet(sheetname As String, supplystore As TimeStep_StoreSupply, behaviors As TimeStep_ManagerOfBehavior, _
                            parameters As TimeStep_Parameters, policystore As TimeStep_StorePolicy, _
                                ctx As timestep_SimContext)
Dim tbl As GenericTable

Set tbl = New GenericTable
tbl.FromSheet Worksheets(sheetname)

unitsFromTable tbl, supplystore, behaviors, parameters, policystore, ctx

End Sub
Public Sub UnitsFromDictionary(unitrecords As Dictionary, parameters As TimeStep_Parameters, behaviors As TimeStep_ManagerOfBehavior, _
                                policystore As TimeStep_StorePolicy, supplystore As TimeStep_StoreSupply, ctx As timestep_SimContext)
'Decouple
UnitsFromRecords unitrecords, parameters, behaviors, policystore, supplystore, ctx

End Sub



