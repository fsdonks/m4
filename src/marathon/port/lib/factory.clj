(ns marathon.port.factory)

'marathonopfactory
Option Explicit

'Port of factory functions from a class.


'This is a factory object, based on the familiar design pattern from the Gang of Four book.
'Basically, it's job is to encapsulate all the methods necessary to produce entities in the
'simulation.  We're pulling stuff from supply/demand manager into here.  The idea is that
'we go to the factory to build objects from a source (either randomly, from a file, or from
'something else (i.e. excel worksheet)).
Public name As String

Private msg As String
Private record As GenericRecord


Private Function inScope(src As String, params As TimeStep_Parameters) As Boolean
'Decoupled
inScope = params.inScope(src)
End Function

Public Sub AddDemandFromExcel(sheet As Worksheet, parameters As TimeStep_Parameters, demandstore As TimeStep_StoreDemand, _
                                policystore As TimeStep_StorePolicy, ctx As timestep_SimContext)
Dim tbl As GenericTable

Set tbl = New GenericTable
tbl.FromSheet sheet
DemandsFromTable tbl, parameters, demandstore, policystore, ctx

End Sub

'Tom Change 23 June 2011 -> Completed the separation between worksheet and records...
'By the time it gets here, the data is in pure record form, obtained from a data provider

'TOM Change 26 Mar 2011 -> ability to read unit definitions (for single units or entire
'classes of units) from a record-like description, rather than the strictly vector
'format from before.  Records have a quantity field.  If this field is = 1, then the
'parser creates a unique unit record.  If it's > 1, then the parser calls addunits just
'like the vector parser.  The goal is to eventually go entirely to this basis, so that we
'can pop this stuff into a database or
Public Sub UnitsFromRecords(records As Dictionary, parameters As TimeStep_Parameters, behaviors As TimeStep_ManagerOfBehavior, _
                                policystore As TimeStep_StorePolicy, supplystore As TimeStep_StoreSupply, ctx As timestep_SimContext)

Dim rec
Dim unit As TimeStep_UnitData
Dim defbehavior As IUnitBehavior
Dim count As Long, quantity As Long
Dim policy As IRotationPolicy

For Each rec In records
    Set record = records(rec)
    With record
        If .fields("Enabled") = True And inScope(.fields("SRC"), parameters) Then
            quantity = Fix(.fields("Quantity"))
            'Decoupled
            Set policy = choosepolicy(.fields("Policy"), .fields("Component"), parameters, policystore, .fields("SRC"))
            If quantity > 1 Then
                AddUnits quantity, .fields("SRC"), .fields("OITitle"), _
                            .fields("Component"), policy, supplystore, policystore, behaviors, ctx
            ElseIf quantity = 1 Then 'unique unit record
                
                Set unit = associateUnit(recordToUnit(record, parameters, policystore), supplystore, ctx)
                Set supplystore = registerUnit(supplystore, behaviors, unit, unit.src = "Ghost", ctx)  'Tom change 1 April, this takes care of all the registration, refactored.
                Set unit = Nothing
            End If
        End If
    End With
Next rec
End Sub
'TOM Change 4 August -> similar performance enhancement.  This will allow us to more rapidly read and create units.
Public Sub unitsFromTable(table As GenericTable, supplystore As TimeStep_StoreSupply, behaviors As TimeStep_ManagerOfBehavior, _
                            parameters As TimeStep_Parameters, policystore As TimeStep_StorePolicy, ctx As timestep_SimContext)

Dim rec
Dim unit As TimeStep_UnitData
Dim defbehavior As IUnitBehavior
Dim count As Long, quantity As Long
Dim policy As IRotationPolicy

table.moveFirst
While Not table.EOF
    Set record = table.getGenericRecord
    With record
        If .fields("Enabled") = True And inScope(.fields("SRC"), parameters) Then
            quantity = Fix(.fields("Quantity"))
            'Decoupled
            Set policy = choosepolicy(.fields("Policy"), .fields("Component"), parameters, policystore, .fields("SRC"))
            If quantity > 1 Then
                'decoupled
                AddUnits quantity, .fields("SRC"), .fields("OITitle"), _
                            .fields("Component"), policy, supplystore, policystore, behaviors, ctx
            ElseIf quantity = 1 Then 'unique unit record
                'Tom change 19 April 2012
                'decoupled
                Set unit = associateUnit(recordToUnit(record, parameters, policystore), supplystore, ctx)
                'decoupled
                Set supplystore = registerUnit(supplystore, behaviors, unit, unit.src = "Ghost", ctx)  'Tom change 1 April, this takes care of all the registration, refactored.
                Set unit = Nothing
            End If
        End If
    End With
    
    table.moveNext
Wend
End Sub

'Tom Change 17 Jun 2011 -> Intermediate function to extract fields from a record to initialize a unitdata object.
Public Function recordToUnit(inrec As GenericRecord, parameters As TimeStep_Parameters, policystore As TimeStep_StorePolicy) As TimeStep_UnitData

With inrec
    Set recordToUnit = createUnit(.fields("Name"), .fields("SRC"), .fields("OITitle"), _
                                  .fields("Component"), .fields("CycleTime"), .fields("Policy"), parameters, policystore)
End With

End Function

'DECOUPLE

'TOM Change 17 June 2011 -> createunit provides a baseline, unattached unit derived from a set of data.
'The unit is considered unattached because it is not registered with a supply "yet".  Thus, its parent is
'nothing.

'parametrically create a new unit.
Public Function createUnit(name As String, src As String, OITitle As String, component As String, _
                            cycletime As Single, policy As String, parameters As TimeStep_Parameters, _
                               policystore As TimeStep_StorePolicy, _
                                Optional behavior As IUnitBehavior, Optional policyobj As IRotationPolicy) As TimeStep_UnitData
Set createUnit = New TimeStep_UnitData

With createUnit
    If Not (behavior Is Nothing) Then Set .behavior = behavior
    .src = src
    .OITitle = OITitle
    .component = component
    .name = name
    .cycletime = cycletime
    'Decoupled
    If policyobj Is Nothing Then Set policyobj = choosepolicy(policy, .component, parameters, policystore, src)
    Set .policy = policyobj
End With

End Function
'Associate or attach a new unit to a particular supply.  If the new unit's name is not unique, it will be
'changed to accomodate uniqueness requirement.
Public Function associateUnit(unit As TimeStep_UnitData, supplystore As TimeStep_StoreSupply, ctx As timestep_SimContext, Optional StrictName As Boolean) As TimeStep_UnitData

Dim count As Long
count = supplystore.unitmap.count + 1


With unit
    'Decoupled
    'Set .parent = supply
    If .name = "Auto" Then
        .name = count & "_" & .src & "_" & .component
    ElseIf supplystore.unitmap.exists(.name) Then
        If StrictName Then Err.Raise 101, , "A unit already exists with the name " _
                            & .name & " in SupplyManager " & supplystore.name & "unit names must be unique"
        .name = .name & "_" & count
    End If
    .index = count
    'decoupled
    initialize_cycle unit, .policy, ctx, .src = "Ghost"
End With

Set associateUnit = unit
End Function
Public Function choosepolicy(policyname As String, component As String, parameters As TimeStep_Parameters, _
                                policystore As TimeStep_StorePolicy, Optional src As String) As IRotationPolicy

Static nm As String

Dim specialPolicy As Boolean

If src = vbNullString Then
    specialPolicy = False
Else
    'Decoupled
    specialPolicy = parameters.SRCHasTag(src, "Special")
End If

    
With policystore
    If .policies.exists(policyname) Then
        Set choosepolicy = .policies(policyname)
    Else
        If Not specialPolicy Then
            If component = "Ghost" Then
                'Decoupled
                nm = parameters.getKey("DefaultGhostPolicy")
            ElseIf component = "AC" Then
                'Decoupled
                nm = parameters.getKey("DefaultACPolicy")
            Else
                'Decoupled
                nm = parameters.getKey("DefaultRCPolicy")
            End If
        Else
            If component = "Ghost" Then
                'Decoupled
                nm = parameters.getKey("SpecialGhostPolicy")
            ElseIf component = "AC" Then
                'Decoupled
                nm = parameters.getKey("SpecialACPolicy")
            Else
                'Decoupled
                nm = parameters.getKey("SpecialRCPolicy")
            End If
        End If
        
        If .policies.exists(nm) Then
            Set choosepolicy = .policies(nm)
        Else
            Err.Raise 101, , "Default Policy is set at " & policyname & " which does not exist!"
        End If
    End If
End With

End Function
Public Sub startstate(supplystore As TimeStep_StoreSupply, parameters As TimeStep_Parameters, ctx As timestep_SimContext, Optional StartingLocations As Dictionary)
Dim unitname
Dim unit As TimeStep_UnitData
Dim loc As String

With supplystore
    'populate day 1 initial lifecycle times
    For Each unitname In .unitmap
        Set unit = .unitmap(unitname)
        'decoupled
        unit.ChangeState "Spawning", 0, , ctx
    Next unitname
    'TOM Change 9 DEC 2010
    'Decoupled
    parameters.SetKey "TotalUnits", .unitmap.count
End With

End Sub
'TOM Change 17 June 2011 -> don't need supply manager involved in this, removed.
'TOM Change 3 Jan 2011
'Given a unit, initialize it's currentcycle based on policy information
'Private Sub initialize_cycle(unit As TimeStep_UnitData, policy As TimeStep_Policy, supply As TimeStep_ManagerOfSupply, Optional ghost As Boolean)
Private Sub initialize_cycle(unit As TimeStep_UnitData, policy As IRotationPolicy, ctx As timestep_SimContext, Optional ghost As Boolean)
Set unit.policy = policy
policy.Subscribe unit

With unit
    If ghost = False Then
        .PositionPolicy = policy.getPosition(.cycletime)  'TOM Change 20 May
        .LocationName = "Spawning"
        'decoupled
        .changeLocation .PositionPolicy, ctx
    Else
        .PositionPolicy = "Spawning" 'TOM Change 20 May
        .LocationName = "Spawning"
        'decoupled
        .changeLocation .PositionPolicy, ctx  'TOM Change 20 May
        'Tom Change 17 June 2011 -> removed, vestigial, don't need supply manager anymore.
        '.location = supply.parent.policymanager.locationID(.LocationName)
    End If
End With

End Sub
'take the unit map and distribute the units evenly. Pass in a collection of unit names, as well as
'the appropriate counts, and the cycles are uniformly distributed (using integer division)
Private Sub distributeCycleTimeLocations(unitset As Dictionary, policy As IRotationPolicy, supply As TimeStep_StoreSupply, ctx As timestep_SimContext)
Dim UniformInterval As Long 'RCinterval As Long
Dim unitname
Dim existingunit As TimeStep_UnitData
Dim LastInterval As Long
Dim interval As Long

UniformInterval = policy.cyclelength \ unitset.count
LastInterval = -1 * UniformInterval 'allows cycles to start at 0

For Each unitname In unitset
    Set existingunit = unitset(unitname)
    With existingunit
        .cycletime = Round(LastInterval + UniformInterval, 0)
        LastInterval = .cycletime
        'Tom Change 17 June 2011 -> changed to reflect new function signature.
        'initialize_cycle existingunit, policy, supply, False
        initialize_cycle existingunit, policy, ctx, False
        If .LocationName = vbNullString Then
            .PositionPolicy = .policy.getPosition(.cycletime) 'Tom Change 20 May 2011
            .changeLocation .PositionPolicy, ctx
        End If
        If .cycletime < 0 Then Err.Raise 101, , "Negative cycletime"

    End With
Next unitname

End Sub
'TOM Note 11 April 2011 ->  This needs to be unified with createunit.
'Auxillary function to parameterize adding units of different types.
'Provides another hooksite to imprint categories on units
'side effect
Public Function AddUnits(amount As Long, src As String, OITitle As String, _
                            compo As String, policy As IRotationPolicy, _
                                    supplystore As TimeStep_StoreSupply, _
                                        policystore As TimeStep_StorePolicy, behaviors As TimeStep_ManagerOfBehavior, _
                                            ctx As timestep_SimContext, _
                                            Optional extratags As Dictionary, _
                                                Optional ghost As Boolean) As Dictionary
Dim i As Long
Dim NewUnit As TimeStep_UnitData
Dim generated As Dictionary
Dim defbehavior As IUnitBehavior
Dim DeployableBuckets As Dictionary
Dim count As Long
Dim nm

If Not ghost Then
    Set defbehavior = behaviors.defaultACBehavior
Else
    Set defbehavior = behaviors.defaultGhostBehavior
End If

Set DeployableBuckets = supplystore.DeployableBuckets

Set AddUnits = supplystore.unitmap 'Note this is in here for legacy reasons.  Return value is used.

If amount > 0 Then
    Set generated = New Dictionary
    count = supplystore.unitmap.count + 1
    For i = 1 To Fix(amount)
        Set NewUnit = New TimeStep_UnitData
        With NewUnit
            Set .behavior = defbehavior
            'Decouple
            'Set .parent = supply
            .src = src
            .OITitle = OITitle
            .component = compo
            .name = count & "_" & .src & "_" & .component
            .index = count
            'decoupled
            initialize_cycle NewUnit, policy, ctx, ghost
            If ghost Then
                .PositionPolicy = .policy.getPosition(.cycletime) 'Tom Change 20 May 2011
                'decoulped
                .changeLocation .PositionPolicy, ctx
                'Decoupled
                .location = locationID(.LocationName, policystore)
            End If
        End With
                
        generated.add NewUnit.name, NewUnit

        Set NewUnit = Nothing
        count = count + 1

    Next i
    
    If Not ghost Then distributeCycleTimeLocations generated, policy, supplystore, ctx
    
    For Each nm In generated
        Set supplystore = registerUnit(supplystore, behaviors, generated(nm), , ctx) 'this handles all the registration items, refactoring.
    Next nm
End If

End Function
'TOM Note 11 April 2011 -> this is a method to generate a set of ghosts manually.  This should be reconciled
'with the SpawnGhosts method above....there's confusion in the naming convention that needs to be eliminated.
'For now, this function is called by the SupplyGenerator when ghosts are to be created on demand.
Public Function SpawnGhost(src As String, OITitle As String, compo As String, _
                            policy As TimeStep_Policy, behaviors As TimeStep_ManagerOfBehavior, _
                                parameters As TimeStep_Parameters, policystore As TimeStep_StorePolicy, _
                                  supplystore As TimeStep_StoreSupply, ctx As timestep_SimContext) As TimeStep_UnitData

'ghosttags.add "Ghost", 0
Set SpawnGhost = associateUnit(createUnit("Auto", src, OITitle, compo, 0, vbNullString, parameters, policystore, , policy), supplystore, ctx)
registerUnit supplystore, behaviors, SpawnGhost, True, ctx 'Tom change 1 April, this takes care of all the registration, refactored.
SpawnGhost.ChangeState "Spawning", 0, , ctx

End Function

'TOM NOTE 21 Mar 2011 -> Need to seriously re-jigger this.  We've got a very effecient, vector-based format now.
'No longer creating individual atomic demands, but demands with magnitude > 1.  Legacy infrastructure was somewhat
'wasteful in this regard.

'Tom Change -> this is a big change, which should be more effecient.  We process a generic table instead of a
'dictionary of records (which forces a lot of allocations on us...)
Private Sub DemandsFromTable(table As GenericTable, parameters As TimeStep_Parameters, demandstore As TimeStep_StoreDemand, _
                                 policystore As TimeStep_StorePolicy, ctx As timestep_SimContext)

Dim demand As TimeStep_DemandData
Dim dupes As Dictionary
Dim dup
Dim demandmap As Dictionary
Dim vig As String
Dim nm As String
Dim op As String
Dim pri As Long

Dim rec
Set demandmap = demandstore.demandmap


Set dupes = New Dictionary
'Decouple
With parameters
    table.moveFirst
     While Not table.EOF
        Set record = table.getGenericRecord
        With record
            If .fields("Enabled") = True And inScope(.fields("SRC"), parameters) Then
                vig = .fields("Vignette")
                op = .fields("Operation")
                pri = .fields("Priority")
                nm = op & "_" & vig & "_" & pri 'demands have unique names
                If demandmap.exists(nm) Then
                    If dupes.exists(nm) Then
                        dupes(nm) = dupes(nm) + 1
                    Else
                        dupes.add nm, 1
                    End If
                Else 'register the demand.
                    Set demand = associateDemand(recordToDemand(record), parameters, demandstore, policystore, ctx)
                    msg = "Demand" & demand.name & " initialized"
                    'Decoupled
                    SimLib.triggerEvent Initialize, demandstore.name, demandstore.name, msg, DemandLog, ctx   'log demand initialization
                End If
            Else
                msg = "Demand at row" & CLng(rec) & " disabled."
                'Decoupled
                SimLib.triggerEvent Initialize, demandstore.name, demandstore.name, msg, DemandLog, ctx  'log demand initialization
            End If
        End With
        table.moveNext
    Wend
End With

'notify of data errors, specifically duplicate demands.
For Each dup In dupes
    msg = "Demand" & CStr(dup) & " had " & dupes(dup) & " duplicates in source data."
    'Decouple
    SimLib.triggerEvent Initialize, demandstore.name, demandstore.name, msg, DemandLog, ctx   'log demand initialization
Next dup

End Sub
'Tom Change 17 Jun 2011 -> Intermediate function to extract fields from a record to initialize a demanddata object.
Public Function recordToDemand(inrec As GenericRecord) As TimeStep_DemandData
Dim nm As String

With inrec
    nm = DemandKey(inrec)    '.fields("Operation") & "_" & .fields("Vignette") & "_" & .fields("Priority")
    Set recordToDemand = _
        createDemand(nm, .fields("StartDay"), .fields("Duration"), .fields("Overlap"), _
                    .fields("SRC"), .fields("Quantity"), .fields("Priority"), _
                    0, .fields("Operation"), .fields("Vignette"), .fields("SourceFirst"), _
                    .fields("DemandGroup"))
End With

End Function
'generate a unique key for this demand
Private Function DemandKey(rec As GenericRecord) As String
Dim startday As Single
Dim dur As Single

With rec
    DemandKey = .fields("Priority") & "_" & .fields("Vignette") & "_" & .fields("SRC") & "_"
    startday = CSng(.fields("StartDay"))
    dur = CSng(.fields("Duration"))
    DemandKey = DemandKey & "[" & startday & "..." & (startday + dur) & "]"
End With
    
End Function
Public Function associateDemand(demand As TimeStep_DemandData, parameters As TimeStep_Parameters, demandstore As TimeStep_StoreDemand, policystore As TimeStep_StorePolicy, ctx As timestep_SimContext) As TimeStep_DemandData
With demandstore
    'Decoupled
    demand.index = parameters.demandstart + .demandmap.count + 1
    
    If .demandmap.exists(demand.name) Then
        demand.name = demand.name & "_" & .demandmap.count + 1
    End If
    
    'decoupled
    registerDemand demand, demandstore, policystore, ctx 'refactored....
    
    Set associateDemand = demand
End With
End Function

Public Function createDemand(name As String, tstart As Single, duration As Single, overlap As Single, _
                            primaryunit As String, quantity As Long, priority As Single, index As Long, _
                                Optional operation As String, Optional vignette As String, _
                                    Optional sourcefirst As String, Optional demandgroup As String) As TimeStep_DemandData
Set createDemand = New TimeStep_DemandData
'TOM Change 6 Dec 2010 - formatting change, for readability
With createDemand
    .name = name
    .startday = tstart
    .duration = duration
    .overlap = overlap
    .primaryunit = primaryunit
    .src = .primaryunit
    If quantity > 0 Then
        .quantity = quantity
    Else
        .quantity = 1
    End If
    .priority = priority
    .index = index
    If operation = vbNullString Then operation = "Random_" & index
    .operation = operation
    If sourcefirst = vbNullString Then sourcefirst = "AC"
    .sourcefirst = sourcefirst
    If vignette = vbNullString Then vignette = "Random_" & index
    .vignette = vignette
    If demandgroup = vbNullString Then
        .demandgroup = UnGrouped
    Else
        .demandgroup = demandgroup
    End If
End With

End Function




