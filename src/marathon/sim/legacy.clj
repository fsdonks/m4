;;A dumping ground for legacy code from other namespaces.
;;Used to clean up the docs and highlight pending porting 
;;efforts.
(ns marathon.sim.legacy)

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