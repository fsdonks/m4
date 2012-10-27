(ns marathon.port.setup)

Option Explicit
'Returns a list of the corresponding field values, for each key in fields, looked up
'from the record inrec.
Public Function fieldVals(inrec As GenericRecord, ParamArray fields()) As Collection
Set fieldVals = New Collection
Dim itm
For Each itm In fields
    If Not inrec.fields.exists(CStr(itm)) Then Err.Raise 101, , _
            "Field '" & itm & "' does not exist."
    fieldVals.add inrec.fields(CStr(itm))
Next itm

End Function

'TODO -> Port all this stuff to the new coredata/sim paradigm.

''Initialize the Timestep_simulation from given parameters.
''Right now, we read parameters/demand from Excel Host
'Public Function Initialize_Engine_FromExcel(data As TimeStep_CoreData, Optional forRequirements As Boolean, _
'                                                Optional sim As TimeStep_CoreSimulation) As TimeStep_CoreSimulation
'
'If sim Is Nothing Then Set sim = New TimeStep_CoreSimulation
'
'With data
'    .parameters.Workstate = Initializing
'    'Tom Change
'    log "Entering Marathon"
'
'    .parameters.fromExcel
'
'
'    'Allow requirements analysis to shorten sim time to only active demands.
'    If forRequirements Then
'        sim.truncateTime = True
'    End If
'
'    If sim.earlyscoping Then
'        'Note -> we can scope this earlier by moving it up in front of Supply and Demand setup,
'        'to quickly establish a set of unreachable SRCs.  Experimental.
'        .fillManager.FromTables getTable("SupplyRecords"), _
'                                 getTable("DemandRecords"), _
'                                  getTable("RelationRecords")
'    End If
'    .policyManager.fromExcel
'    .supplyManager.fromExcel Worksheets("SupplyAggregate").Range("SupplyStart"), forRequirements
'    .demandManager.fromExcel Worksheets("DemandRecords").Range("DemandStart")
'
'    If Not sim.earlyscoping Then
'        'Note -> we can scope this earlier by moving it up in front of Supply and Demand setup,
'        'to quickly establish a set of unreachable SRCs.
'        .fillManager.fromExcel
'    End If
'
'    'TOM Change 7 Jun 2011 -> moved to end to ensure locations are known prior to initialization.
'    .outputManager.init ActiveWorkbook.path & "\", , policyManager.locations, forRequirements
'
'    If forRequirements Then
'        sim.NoSupplyWarning = True
'        'SupplyManager.UnitsFromDictionary unitrecords
'    End If
'End With
'
'Set sim.data = data
'
'End Function
'
''try to keep pre-calculated policy, parameters, etc. in the system.  we just want to flush out the
''dynamic stuff and pull in more information.
''If requirements is true, we will look for supply in a GeneratedSupply worksheet.
'Public Sub Reset_Engine_FromExcel(data As TimeStep_CoreData, sim As TimeStep_CoreSimulation, Optional Requirements As Boolean, _
'                                    Optional unitrecords As Dictionary)
'
'sim.truncateTime = Requirements
'
'
'reset 'this will reschedule demands
'
'parameters.Workstate = Initializing
''Tom Change
'log "Resetting Marathon"
'
'If Requirements Then
'    NoSupplyWarning = True
'    If Not (unitrecords Is Nothing) Then
'        supplyManager.UnitsFromDictionary unitrecords
'    Else
'        'Tom Change 16 April 2012
'        'SupplyManager.UnitsFromSheet "GeneratedSupply"
'        supplyManager.UnitsFromDictionary getTable(ActiveWorkbook.path & "\GeneratedSupply.csv").toGenericRecords
'    End If
'Else
'    supplyManager.fromExcel Worksheets("SupplyAggregate").Range("SupplyStart")
'End If
'
'fillManager.fromExcel
'
'End Sub
'
''TOM change 25 April 2012
'
'Public Sub Clear_Engine(Optional forRequirements As Boolean)
'parameters.Workstate = Initializing
''Tom Change
'log "Entering Marathon"
'
''keep parameters from last run
'
''profile "ParameterSetup"
''parameters.fromExcel
''profile "ParameterSetup"
'
''Allow requirements analysis to shorten sim time to only active demands.
'If forRequirements Then
'    truncateTime = True
'End If
'
'asVolatile(TimeManager).reset
'asVolatile(eventmanager).reset
'asVolatile(policyManager).reset
'policyManager.clearLocations
'asVolatile(supplyManager).reset
'
'demandManager.clearDemands
'
'asVolatile(fillManager).reset
'asVolatile(behaviorManager).reset
'asVolatile(entityfactory).reset
'
'
'If earlyscoping Then
'    'Note -> we can scope this earlier by moving it up in front of Supply and Demand setup,
'    'to quickly establish a set of unreachable SRCs.  Experimental.
'    profile "FillSetup"
'    fillManager.FromTables getTable("SupplyRecords"), _
'                            getTable("DemandRecords"), _
'                             getTable("RelationRecords")
'    profile "FillSetup"
'End If
'
''we're loading new supply...
'supplyManager.fromExcel Worksheets("SupplyAggregate").Range("SupplyStart"), forRequirements
''we're actually loading new demands
'demandManager.fromExcel Worksheets("DemandRecords").Range("DemandStart")
'
'asVolatile(outputManager).reset
'
'If Not earlyscoping Then
'    'Note -> we can scope this earlier by moving it up in front of Supply and Demand setup,
'    'to quickly establish a set of unreachable SRCs.
'    profile "FillSetup"
'    fillManager.fromExcel
'    profile "FillSetup"
'End If
'
'
'If forRequirements Then
'    NoSupplyWarning = True
'    'SupplyManager.UnitsFromDictionary unitrecords
'End If
'
'End Sub
'
''''For right now, we're just going to load csv's into excel tables....
''''We assume that the project points us to the paths needed to get at our csvs.
''''From there, we load the files into the active tables.
''''Then call initialize engine from excel.
''''Should be easy enough...
'''
'''Public Sub Initialize_Engine_From_Project(Project As TimeStep_Project, _
'''                                            Optional forRequirements As Boolean)
'''
'''Project.load 'should pull in all the csv's we need.
'''
'''parameters.Workstate = Initializing
''''Tom Change
'''log "Entering Marathon"
'''
'''parameters.fromExcel
'''policyManager.fromExcel
'''supplyManager.fromExcel Worksheets("SupplyAggregate").Range("SupplyStart"), forRequirements
'''demandManager.fromExcel Worksheets("DemandRecords").Range("DemandStart")
'''fillManager.fromExcel
'''
''''TOM Change 7 Jun 2011 -> moved to end to ensure locations are known prior to initialization.
''''TOM 12 july -> this indicates a dependency....therfore, we should make a function.
'''outputManager.init ActiveWorkbook.path & "\", , policyManager.locations
'''
'''If forRequirements Then
'''    NoSupplyWarning = True
'''    'SupplyManager.UnitsFromDictionary unitrecords
'''End If
'''
'''End Sub
'
