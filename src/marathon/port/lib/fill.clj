(ns marathon.port.fill)

'marathonopfill
'TODO -> port this over...
Option Explicit
'
''TOM Change 21 Sep 2011 -> Ran into the unique circumstance of not using follow-ons unexpectedly...
'    'Arises when we have a higher-priority (either by priority value or by position in the q) demand
'    'to fill, but we don't have any eligible supply to fill it.  By default, we short-circuit the fills,
'    'because we obey the law that lower-priority fills cannot be filled before higher, thus higher
'    'priority fills will always get the supply.
'    'This law no longer holds in the context of follow-on supply.
'        'Immediately, we see that iff supply.followons.count = 0, this law holds.  We process as normal.
'        'Iff supply.followns.count >0 then
'            'We need to find out which demands have eligible follow-on supply (if ANY)
'            'and fill them first.
'    'Demands with eligible follow-on supply will have the same SRC and demandgroup as the follow-ons.
'
'TOM Change 14 Mar 2011 -> Re-writing this from scratch.  We no longer use baked-in rules.
'Assuming we have a fillfunction, given a demand, we try to fill the demand.  Should be a simple
'call to the fillfunction object, mostly.
'''TOM Change 7 Dec 2010 - Removed parameter for priority scheme, since it DOES NOTHING.
'''Change from Sub to function, sourceDemand returns a boolean to indicate success or failure.
'''Excised the functionality for handling deactivated demands with units supplied, basically the
'''sub to send units home upon demand deactivation, into a separate sub called SendHome.
Function sourceDemand(t As Single, demand As TimeStep_DemandData, sourcetype As String, fillstore As TimeStep_StoreFill, _
                                     supplystore As TimeStep_StoreSupply, parameters As TimeStep_Parameters, ctx As timestep_SimContext, _
                                        Optional supplybucket As Dictionary, Optional phase As FillPhase) As Boolean

Dim rule As String
Dim fillList As Dictionary
Dim fill As TimeStep_Fill
Dim unit As TimeStep_UnitData
Dim deployer
'Decoupled
'If supplystore Is Nothing Then Set supplystore = supplyManager

rule = deriveRule(demand, fillstore)

With fillstore.fillfunction '<---this is new, we now dispatch on the type of fill....
    If .exists(rule) Then 'we can try to fill this demand.
        .Query rule, supplystore, demand.demandgroup, demand.name, supplybucket, phase
        'This will take more....i think context for sure.
        Set fillList = .take(demand.required, phase)  'try to get the required fills
        If fillList.count > 0 Then
            If fillList.count = demand.required Then sourceDemand = True 'found units to fill required...
            For Each deployer In fillList
                Set fill = fillList(deployer)
                recordFill fill, fillstore 'this may get to be too big at some point, maybe not.....
                Set unit = fill.source
                'Decoupled
                SimLib.triggerEvent FillDemand, demand.name, unit.name, "Filled Demand ", , ctx
                'Tom Change 18 Aug 2011
                If unit.src = "Ghost" Then
                    If unit.followoncode = vbNullString Then
                        'Decoupled
                        SimLib.triggerEvent GhostDeployed, demand.src, demand.src, "Filled demand with ghost", "normal", ctx
                    Else
                        'Decoupled
                        SimLib.triggerEvent GhostDeployed, demand.src, demand.src, "Ghost followed on to another demand", "followon", ctx
                    End If
                End If
                
                MarathonOpSupply.DeployUnit supplystore, unit, t, fill.quality, demand, unit.policy.maxbog, fillstore.Fills.count, _
                                    fill, parameters.IntervalToDate(t), unit.followoncode <> vbNullString, ctx
            Next deployer
        End If
    End If
End With


End Function
Public Function deriveRule(demand As TimeStep_DemandData, fillstore As TimeStep_StoreFill) As String
deriveRule = fillstore.fillgraph.sinklabel(demand.primaryunit)
End Function
'Simple function to record fills (deployment records)
Public Sub recordFill(fill As TimeStep_Fill, fillstore As TimeStep_StoreFill)
fillstore.Fills.add fillstore.Fills.count + 1, fill
End Sub

'Constructor for building fill stores from component pieces.
'Note, the fill store has
Public Function makeFillStore(fillgraph As TimeStep_FillGraph, fillfunction As TimeStep_FillFunction, generator As TimeStep_SupplyGenerator) As TimeStep_StoreFill

Set makeFillStore = New TimeStep_StoreFill
With makeFillStore
    Set .fillgraph = fillgraph
    Set .fillfunction = fillfunction
End With
End Function

'Process that encapsulates creating a new fillstore from coredata, appending the fillstore to the core data, and then
'returning a scoped set of core data, where the supply and demand have been reduced according to the relations
'embodied by the fillgraph.

'Assumes data has valid supply, demand, and policystore instances (i.e. they've been initialzed,
'probably from tables).  Returns updated core data.
Public Function simStateToScopedSimState(simstate As TimeStep_SimState, Optional generator As TimeStep_SupplyGenerator) As TimeStep_StoreFill

Dim ff As TimeStep_FillFunction
Dim fg As TimeStep_FillGraph
Dim fs As TimeStep_StoreFill

With simstate
    Set fg = composeFillGraph(.supplystore, .demandstore, .policystore)
    If generator Is Nothing Then _
        Set generator = makeSupplyGenerator(simstate, , , simstate.parameters.getKey("DefaultSupplyPriority") = "RCPreSurge")
    Set ff = makeFillFunction("FillFunction", .supplystore, fg, .parameters, .context, generator)
    Set fs = makeFillStore(fg, ff, generator)
End With

Set simstate.fillstore = fs
'Scopes the data as a final step, since we have handle on the fillgraph already.
Set simstate = scopeSimState(fg, simstate)
Set ff = Nothing
Set fg = Nothing
Set fs = Nothing

End Function
'Performs a large scoping operation on core data, using any fillgraph.
Public Function scopeSimState(fillgraph As TimeStep_FillGraph, simstate As TimeStep_SimState) As TimeStep_SimState
scope fillgraph.reducedgraph, simstate.fillstore, simstate.parameters, simstate.supplystore, simstate.demandstore, simstate.context
Set scopeSimState = simstate
End Function

'Given pre-built stores of supply, demand, and policy, composes them into a fillgraph.
Public Function composeFillGraph(supplystore As TimeStep_StoreSupply, demandstore As TimeStep_StoreDemand, policystore As TimeStep_StorePolicy) As TimeStep_FillGraph
Set composeFillGraph = BuildFillGraph(New TimeStep_FillGraph, supplystore, demandstore, policystore)
End Function
Public Function tablesToFillgraph(sourcesTbl As GenericTable, sinksTbl As GenericTable, relationsTbl As GenericTable) As TimeStep_FillGraph
Set tablesToFillgraph = FillGraphFromTables(New TimeStep_FillGraph, sourcesTbl, sinksTbl, relationsTbl)
End Function

'Produces a new fill function from inputs.
Public Function makeFillFunction(nm As String, supplystore As TimeStep_StoreSupply, _
                                    graph As IGraph, parameters As TimeStep_Parameters, _
                                        context As timestep_SimContext, _
                                            generator As TimeStep_SupplyGenerator) As TimeStep_FillFunction

Set makeFillFunction = New TimeStep_FillFunction
With makeFillFunction
    .name = nm
    'Decoupled
    'Set parent = supply
    'Eh...this is dubious....TODO -> separate further.
    Set .FillRules = .AddFill(graph)
    'Decoupled
    Set .generator = generator
End With

End Function

'A constructor for supply generators.  Currently, there's only one kind of generator, based off the legacy supply
'system.  We'll have to fix this.  Extensibility via new generators is pretty fundamental.
Public Function makeSupplyGenerator _
    (simstate As TimeStep_SimState, Optional gpolicy As TimeStep_Policy, _
        Optional gcompo As String, Optional fergusonmode As Boolean, Optional comparer As IComparator) As TimeStep_SupplyGenerator

Set makeSupplyGenerator = New TimeStep_SupplyGenerator

With makeSupplyGenerator
    'TODO -> this is just a temporary decoupling.  I need to extract out the supply generator further.  Parent is
    'just pointing at a supplystore now (now unlike a partially applied function).  Still, the dependencies kinda suck.
    'Decoupled
    'Set .parent = supplystore
    Set .simstate = simstate
    'Decoupled
    If gpolicy Is Nothing Then Set gpolicy = simstate.policystore.policies("Ghost365_45")
    If gcompo = vbNullString Then gcompo = "Ghost" 'no component
    Set .ghostpolicy = gpolicy
    .ghostcompo = gcompo
    'Decoupled
    Set .tags = simstate.supplystore.tags
    Set .comparer = New TimeStep_ComparerUnit 'Too coupled.  Need to fix unit comparison to allow general prioritization.
    If fergusonmode Then .comparer.RCpresurgePreference = True
End With

End Function

'TODO ->  Do a better job separating concerns here... Building a fill graph and viewing the
'intermediate results are likely orthogonal...
'Accumulate a fill graph from supplymanager, policymanager and demands...
Public Function BuildFillGraph(sourcegraph As TimeStep_FillGraph, _
                                supplystore As TimeStep_StoreSupply, _
                                        demandstore As TimeStep_StoreDemand, _
                                            policystore As TimeStep_StorePolicy) As TimeStep_FillGraph

Set BuildFillGraph = sourcegraph

With sourcegraph
    Set BuildFillGraph = .fromsupply(supplystore)
'    If rendergraphs And allgraphs Then renderGraph "SourcesGraph", BuildFillGraph.graph, , True
    Set BuildFillGraph = .fromPolicy(policystore)
'    If rendergraphs And allgraphs Then renderGraph "Sources_RelationsGraph", BuildFillGraph.graph, , True
    Set BuildFillGraph = .FromDemand(demandstore)
'    If rendergraphs And allgraphs Then renderGraph "Sources_Relations_SinksGraph", BuildFillGraph.graph, , True
End With

'Tom note -> these are debug tools.
'renderGraph "FullGraph", BuildFillGraph.graph
'currently we build this into the fillgraph build process.  We want to know
'where there are islands....
sourcegraph.decompose

'If rendergraphs And allgraphs Then renderGraph "Sources_Relations_Sinks_Decomposed", BuildFillGraph.graph, , True
'If rendergraphs Then renderGraph "Source_SinksReducedGraph", BuildFillGraph.reducedgraph, , True

'scope BuildFillGraph.reducedgraph, parameters, supplystore, demandstore, ctx
End Function

'TODO ->  Do a better job separating concerns here... Building a fill graph and viewing the
'intermediate results are likely orthogonal...
'Accumulate a fill graph from supplymanager, policymanager and demands...
Public Function FillGraphFromTables(sourcegraph As TimeStep_FillGraph, _
                                supply As GenericTable, _
                                        demand As GenericTable, _
                                            policy As GenericTable) As TimeStep_FillGraph

Set FillGraphFromTables = sourcegraph

With sourcegraph
    Set FillGraphFromTables = .FromSourceTable(supply)
'    If rendergraphs And allgraphs Then renderGraph "SourcesGraph", FillGraphFromTables.graph, , True
    Set FillGraphFromTables = .fromRelationTable(policy)
'    If rendergraphs And allgraphs Then renderGraph "Sources_RelationsGraph", FillGraphFromTables.graph, , True
    Set FillGraphFromTables = .FromSinkTable(demand)
'    If rendergraphs And allgraphs Then renderGraph "Sources_Relations_SinksGraph", FillGraphFromTables.graph, , True
End With

'Tom note -> these are debug tools.
'renderGraph "FullGraph", BuildFillGraph.graph
'currently we build this into the fillgraph build process.  We want to know
'where there are islands....
sourcegraph.decompose

'If rendergraphs And allgraphs Then renderGraph "Sources_Relations_Sinks_Decomposed", FillGraphFromTables.graph, , True
'If rendergraphs Then renderGraph "Source_SinksReducedGraph", FillGraphFromTables.reducedgraph, , True

End Function
Public Function staticGraph(supply As GenericTable, _
                              demand As GenericTable, _
                                policy As GenericTable) As TimeStep_FillGraph


Set staticGraph = New TimeStep_FillGraph

With staticGraph
    Set staticGraph = .FromSourceTable(supply)
'    If rendergraphs And allgraphs Then renderGraph "SourcesGraph", staticGraph.graph, , True
    Set staticGraph = .fromRelationTable(policy)
'    If rendergraphs And allgraphs Then renderGraph "Sources_RelationsGraph", staticGraph.graph, , True
    Set staticGraph = .FromSinkTable(demand)
'    If rendergraphs And allgraphs Then renderGraph "Sources_Relations_SinksGraph", staticGraph.graph, , True
End With

'Tom note -> these are debug tools.
'renderGraph "FullGraph", BuildFillGraph.graph
'currently we build this into the fillgraph build process.  We want to know
'where there are islands....
staticGraph.decompose

'If rendergraphs And allgraphs Then renderGraph "Sources_Relations_Sinks_Decomposed", staticGraph.graph, , True
'If rendergraphs Then renderGraph "Source_SinksReducedGraph", staticGraph.reducedgraph, , True


End Function
Public Sub renderAllGraphs(fillstore As TimeStep_StoreFill)
fillstore.rendergraphs = True
fillstore.allgraphs = True
End Sub
'TOM change 24 Mar 2011 -> Utilize the reduced fillgraph to determine which elements of supply and demand
'should be scoped out of the study, remove these elements from demand and supply.
Public Sub scope(reduced As GenericGraph, fillstore As TimeStep_StoreFill, parameters As TimeStep_Parameters, _
                    supplystore As TimeStep_StoreSupply, demandstore As TimeStep_StoreDemand, _
                        ctx As timestep_SimContext, Optional csv As Boolean)
Dim island
Dim islands As Dictionary
Dim strm As IRecordStream
Dim scoperec As GenericRecord
Dim msg As String
Dim src As String
Dim isle
Dim res As Dictionary

Set islands = findIslands(reduced, fillstore)

'Decoupled
With parameters.OutOfScope

    'todo....check this, seems hard coded.
    If csv Then
        Set strm = New Streamer_CSV
    Else
        Set strm = New Streamer_xl
    End If
    
    Set scoperec = New GenericRecord
    scoperec.AddField "TimeStamp", Now()
    scoperec.AddField "SRC", vbNullString
    scoperec.AddField "Reason", vbNullString


    'TOM Change 20 April 2012
    'Application.ScreenUpdating = False
    DisableScreenUpdates

    If csv Then
        strm.init scoperec.fieldnames, "OutOfScope.csv"
    Else
        strm.init scoperec.fieldnames, "OutOfScope"
    End If


    For Each isle In islands("Supply")
        src = "'" & replace(CStr(isle), "SOURCE_", vbNullString)
        scoperec.UpdateField "SRC", src
        scoperec.UpdateField "Reason", "No Demand"
        strm.writeGeneric scoperec
        .add src, "No Demand"
    Next isle
    For Each isle In islands("Demand")
        src = "'" & replace(CStr(isle), "FILLRULE_", vbNullString)
        scoperec.UpdateField "SRC", src
        scoperec.UpdateField "Reason", "No Supply"
        strm.writeGeneric scoperec
        .add src, "No Supply"
    Next isle
    strm.Terminate


    If csv Then
        Set strm = New Streamer_CSV
        strm.init scoperec.fieldnames, "InScope.csv"

    Else
        Set strm = New Streamer_xl
        strm.init scoperec.fieldnames, "InScope"
    End If

    Set res = islands("InScope")
    For Each isle In res
        If res(isle) = "Demand" Then
            src = "'" & replace(CStr(isle), "FILLRULE_", vbNullString)
        ElseIf res(isle) = "Supply" Then
            src = "'" & replace(CStr(isle), "SOURCE_", vbNullString)
        Else
            Err.Raise 101, , "unknown characterization of fill validity"
        End If
        scoperec.UpdateField "SRC", src
        scoperec.UpdateField "Reason", res(isle)

        strm.writeGeneric scoperec
    Next isle

    strm.Terminate
End With

'TOM Change 20 April 2012
EnableScreenUpdates
'Application.ScreenUpdating = True

Set strm = Nothing

msg = "FillManager found " & islands("Supply").count & " Unused Supply Sources"
'Decoupled
SimLib.triggerEvent ScopedSupply, fillstore.name, supplystore.name, msg, , ctx
'Tom change 16 June 2011 -> inserted True into scope, removes units from supply.
'Decoupled
MarathonOpSupply.scopeSupply supplystore, islands("Supply"), True
msg = "FillManager found " & islands("Demand").count & " Unfillable Demand Sinks"
'Decoupled
SimLib.triggerEvent ScopedDemand, fillstore.name, demandstore.name, msg, , ctx
'Tom change 16 June 2011 -> inserted True into scope, removes demands from demand.
'Decoupled
MarathonOpDemand.scopeDemand demandstore, islands("Demand"), True

End Sub
Public Function findIslands(source As GenericGraph, fillstore As TimeStep_StoreFill) As Dictionary
Dim res As Dictionary
Dim islands As Dictionary
Dim nd
Dim grph
Dim subgraph As GenericGraph

Set res = New Dictionary

Set islands = New Dictionary
res.add "Supply", islands
Set islands = New Dictionary
res.add "Demand", islands
res.add "InScope", New Dictionary

With fillstore
    For Each grph In source.subgraphs
        Set subgraph = source.subgraphs(grph)
        If subgraph.nodes.count = 1 Then 'it's an island.
            For Each nd In subgraph.nodes
                Set islands = res(islandType(CStr(nd)))
                islands.add CStr(nd), 0
                .OutOfScope.add CStr(nd), 0
                source.RemoveNode CStr(nd) 'eliminate the island
                source.subgraphs.Remove (grph) 'eliminate the subgraph
                Set subgraph = Nothing
            Next nd
        Else 'the source is in scope...
            Set islands = res("InScope")
            For Each nd In subgraph.nodes
                islands.add CStr(nd), islandType(CStr(nd))
            Next nd
        End If
    Next grph
End With

Set findIslands = res
Set res = Nothing

End Function
Private Function islandType(nodename As String) As String
If InStr(1, nodename, "SOURCE") > 0 Then
    islandType = "Supply"
ElseIf InStr(1, nodename, "FILLRULE") > 0 Then
    islandType = "Demand"
Else
    Err.Raise 101, , "Island is neither supply nor demand"
End If

End Function

'Public Sub fromExcel()
''Decouple
'init parent
'End Sub

'Public Sub FromTables(sources As GenericTable, sinks As GenericTable, relations As GenericTable)
''Decouple
'init parent, True, sources, sinks, relations
'End Sub
'



