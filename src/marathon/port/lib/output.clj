(ns marathon.port.output)

'marathonopoutput
Option Explicit
Public Function makeOutputStore(outpath As String, policystore As TimeStep_StorePolicy, demandstore As TimeStep_StoreDemand, parameters As TimeStep_Parameters, context As timestep_SimContext, Optional outfiles As Dictionary, _
                                    Optional onlyExternal As Boolean, Optional moderateio As Boolean) As TimeStep_StoreOutput

Dim os As TimeStep_StoreOutput
Set os = New TimeStep_StoreOutput

With os
    If outfiles Is Nothing Then
        If onlyExternal Then
            Set .defaultfiles = csvStreams()
        Else
            Set outfiles = .defaultfiles
        End If
    Else
        Set outfiles = .defaultfiles
    End If
    .mypath = outpath

    addCycleLog .mypath & "cycles.csv", getStream(os, .mypath & "cycles.csv"), os, context
    addDeployWatch "Deployments", os, context
    addGhostWatch "Ghosts", os, context
    addSummary "NominalSummary", os, context, True
    addSummary "EmpiricalSummary", os, context
    'we tend to use demandtrends for a lot of stuff...
    addDemandTracker getStream(os, .mypath & "DemandTrends.csv"), os, context, demandstore  'getStream("DemandTrends")
    
    'these are high IO, and can slow down stuff, so they're optional.
    'Decoupled
    If moderateio = False Then
        addLocationWatch os, policystore.locations, parameters.startdate, context, "SandTrends"
        addMotionTracker .mypath & "locations.csv", getStream(os, .mypath & "locations.csv"), os, context
    End If

End With


Set makeOutputStore = os
Set os = Nothing
End Function

'get a centrally-managed stream for an observer. assumes the observer's name is name.
Private Function getStream(outputStore As TimeStep_StoreOutput, name As String, _
                                Optional path As String, Optional noio As Boolean) As IRecordStream
Static xlstream As Streamer_xl
Static csvstream As Streamer_CSV

With outputStore

    If path = vbNullString Then path = .mypath
    'Decoupled
    If noio Then
        Set getStream = New Streamer_Null
    Else
        
        If .defaultStreams.exists(name) Then
            Select Case .defaultStreams(name)
                Case Is = "xl"
                    Set xlstream = New Streamer_xl
                    xlstream.StreamTo name
                    Set getStream = xlstream
                    Set xlstream = Nothing
                    
                Case Is = "csv"
                    Set csvstream = New Streamer_CSV
                    If path = vbNullString Then path = ActiveWorkbook.path & "\"
                    If InStr(1, name, ".csv") = 0 Then
                        path = path & name & ".csv"
                    Else
                        path = path & name
                    End If
                    
                    csvstream.StreamTo path
                    Set getStream = csvstream
                    Set csvstream = Nothing
                    
                Case Else
                    Set getStream = New Streamer_Null
            End Select
        ElseIf InStr(1, name, ".csv") > 0 Then
            Set csvstream = New Streamer_CSV
            csvstream.StreamTo name
            Set getStream = csvstream
            Set csvstream = Nothing
        Else
            Set getStream = New Streamer_Null
        End If
    End If
    
    .activeStreams.add name, getStream
End With

End Function
Public Function csvStreams() As Dictionary
Set csvStreams = newdict("LocationWatch", "csv", _
                          "GhostWatch", "csv", "Ghosts", "csv", _
                          "DeployWatch", "csv", _
                          "CycleLog", "csv", _
                          "SandTrends", "csv", _
                          "Deployments", "csv", _
                          "DemandTrends", "csv", _
                          "EmpiricalSummary", "csv", _
                          "NominalSummary", "csv")

End Function
Public Sub closeStream(outputStore As TimeStep_StoreOutput, name As String)
Static stream As IRecordStream

With outputStore
    If .activeStreams.exists(name) Then
        Set stream = .activeStreams(name)
        stream.Terminate
        .activeStreams.Remove name
    End If
End With

End Sub
Public Sub killStreams(outputStore As TimeStep_StoreOutput)
Dim nm
For Each nm In outputStore.activeStreams
    closeStream outputStore, CStr(nm)
Next nm

End Sub
'Tom Change 7 June 2011 -> Location watch will account for ALL locations.....
'This means we need to have the entire set of locations calculated prior to
'adding the location watch.
Public Sub addLocationWatch(outputStore As TimeStep_StoreOutput, locations As Dictionary, startdate As Date, ctx As timestep_SimContext, _
                                Optional nm As String, Optional noio As Boolean)
Dim newrecord As GenericRecord
Dim watcher As TimeStep_ObserverLocationWatch
Dim stream As Streamer_xl
Dim loc
Dim res As VbMsgBoxResult

Set newrecord = New GenericRecord
newrecord.AddField "Day", 0
'TOM Change 15 Jun 2011
'Decouple
newrecord.AddField "Nominal Date", startdate
newrecord.AddField "Quarter", 0

For Each loc In locations
    newrecord.AddField CStr(loc), 0
Next loc

Set watcher = New TimeStep_ObserverLocationWatch
'Decoupled
watcher.init nm, getStream(outputStore, nm, , noio), newrecord, outputStore, locations, ctx, True
outputStore.observers.add watcher.name, watcher

End Sub
Public Sub addDeployWatch(nm As String, outputStore As TimeStep_StoreOutput, ctx As timestep_SimContext, Optional noio As Boolean)
Dim watcher As TimeStep_ObserverDeployWatch


Set watcher = New TimeStep_ObserverDeployWatch

'Decouple
watcher.init nm, getStream(outputStore, nm, , noio), DeployRecord, ctx, True
outputStore.observers.add watcher.name, watcher

End Sub
'add a ghost watcher observer.
Public Sub addGhostWatch(nm As String, outputStore As TimeStep_StoreOutput, ctx As timestep_SimContext, Optional noio As Boolean)
Dim watcher As TimeStep_ObserverGhostWatch

Set watcher = New TimeStep_ObserverGhostWatch
'Decouple
watcher.init nm, getStream(outputStore, nm, , noio), ctx, True
outputStore.observers.add watcher.name, watcher
End Sub
'Tom Change 14 Sep 2011
Public Sub addSummary(nm As String, outputStore As TimeStep_StoreOutput, ctx As timestep_SimContext, _
                        Optional nominal As Boolean, Optional noio As Boolean)
Dim watcher As TimeStep_OutputSummary, stream As Streamer_xl
Dim strm As String

Set watcher = New TimeStep_OutputSummary
watcher.nominal = nominal

If nominal Then strm = "NominalSummary" Else strm = "EmpiricalSummary"

'Decouple
watcher.init nm, getStream(outputStore, strm, , noio), ctx
outputStore.observers.add watcher.name, watcher

End Sub
Public Sub addDemandTracker(stream As IRecordStream, outputStore As TimeStep_StoreOutput, ctx As timestep_SimContext, demandstore As TimeStep_StoreDemand, Optional noio As Boolean)
Dim dtracker As TimeStep_ObserverDemandTracker
Set dtracker = New TimeStep_ObserverDemandTracker

'Decouple
dtracker.init "DemandTracker", stream, demandstore.activedemands, demandstore.changed, ctx
outputStore.observers.add dtracker.name, dtracker

Set dtracker = Nothing

End Sub
Public Sub addCycleLog(path As String, stream As IRecordStream, outputStore As TimeStep_StoreOutput, ctx As timestep_SimContext)
Dim cyclelogger As TimeStep_ObserverCycleLog
Set cyclelogger = New TimeStep_ObserverCycleLog

'Decouple
cyclelogger.init path, ctx, stream, True
outputStore.observers.add cyclelogger.name, cyclelogger
Set cyclelogger = Nothing

End Sub
Public Sub addMotionTracker(path As String, stream As IRecordStream, outputStore As TimeStep_StoreOutput, ctx As timestep_SimContext)
Dim tracker As TimeStep_ObserverMotionTracker

Set tracker = New TimeStep_ObserverMotionTracker
'Decouple
tracker.init "MotionTracker", ctx, stream, outputStore
outputStore.observers.add tracker.name, tracker
End Sub


Public Static Function DeployRecord() As GenericRecord

Set DeployRecord = New GenericRecord

With DeployRecord
    .AddField "Location", vbNullString
    .AddField "Demand", vbNullString
    .AddField "DwellBeforeDeploy", 0
    .AddField "BogBudget", 0
    .AddField "CycleTime", 0
    .AddField "DeployInterval", 0
    .AddField "DeployDate", #1/1/1900#
    .AddField "FillType", vbNullString
    .AddField "FillCount", 0
    .AddField "UnitType", vbNullString
    .AddField "DemandType", vbNullString
    .AddField "DemandGroup", vbNullString
    .AddField "Unit", 0
    .AddField "Policy", ""
    .AddField "AtomicPolicy", ""
    .AddField "Component", vbNullString
    .AddField "Period", vbNullString
    .AddField "FillPath", vbNullString
    .AddField "PathLength", 0
    .AddField "FollowOn", False
End With

End Function
Public Sub clearObservers(outputStore As TimeStep_StoreOutput, ctx As timestep_SimContext)
Dim itm

For Each itm In outputStore.observers
    'Decoupled
    'parent.eventmanager.ClearListener CStr(itm)
    ctx.events.ClearListener CStr(itm)
    If outputStore.activeStreams.exists(CStr(itm)) Then closeStream outputStore, CStr(itm)
    outputStore.observers.Remove itm
Next itm

End Sub

'Private Sub IVolatile_Reset()
'clearObservers 'should close down observations, files, etc.
'
'killStreams
''Decouple
'init mypath, , parent.policyManager.locations
'
'End Sub
'
'Private Sub IVolatile_Terminate()
'killStreams
'Class_Terminate
'End Sub




