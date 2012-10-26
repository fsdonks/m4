(ns marathon.port.simulation)

Option Explicit
Public Sub logged_simulation()
'Dim marathon As TimeStep_Engine
'
'Set marathon = New TimeStep_Engine
'marathon.InteractiveSimulation
Worksheets("control").Select

End Sub
Public Sub logged_simulation_lite()
Dim marathon As TimeStep_Engine
'
'Set marathon = New TimeStep_Engine
'marathon.InteractiveSimulation

DumbUI.disable_logs

Worksheets("control").Select
End Sub
Public Sub simple()

DisableScreenUpdates

commandline_simulation , True

EnableScreenUpdates


End Sub

Public Sub commandline_simulation(Optional logevents As Boolean, Optional moderateio As Boolean)
'Dim marathon As TimeStep_Engine
'Dim tmr As Single
'Dim logger As TimeStep_ObserverLogFile
'
'Worksheets("control").Select
'
'tmr = timer()
'Set marathon = New TimeStep_Engine
'marathon.moderateIO = moderateIO
'marathon.Initialize_Engine_FromExcel
'
'If logevents Then
'    Set logger = New TimeStep_ObserverLogFile
'    logger.init "events", marathon.eventmanager.evtstream
'End If
'
'
'marathon.EventStepMarathon
'
'Debug.Print "simulated in " & timer() - tmr
'
'Set marathon = Nothing
'memclr


End Sub
Public Sub simulation_profile(Optional logevents As Boolean, Optional noio As Boolean)
'Dim marathon As TimeStep_Engine
'Dim tmr As Single
'Dim logger As TimeStep_ObserverLogFile
'Dim results As Dictionary
'
'Worksheets("control").Select
'
'tmr = timer()
'Set marathon = New TimeStep_Engine
'marathon.noio = noio
'marathon.profiledRun = True
'
'marathon.Initialize_Engine_FromExcel
'
'If logevents Then
'    Set logger = New TimeStep_ObserverLogFile
'    logger.init "events", marathon.eventmanager.evtstream
'End If
'
'
'marathon.EventStepMarathon
'Set results = marathon.profiling.report
'
'
'Debug.Print "simulated in " & timer() - tmr & vbCrLf
'Debug.Print marathon.profiling.reportS
'
'memclr
End Sub

Sub memclr()
End
End Sub

Sub pathtest()
Dim graph As GenericGraph
Dim res As Dictionary
Set graph = New GenericGraph

With graph
    .addArc "AC", "AC13"
    .addArc "AC13", "Presurge"
    .addArc "AC", "AC11"
    .addArc "AC11", "Surge"
    .addArc "AC", "AC12"
    .addArc "AC12", "Postsurge"
End With

Set res = (graph.pathAny("AC", "Presurge").item("Paths"))

End Sub

Public Sub dotplot()
With New TimeStep_VisualDotPlots
    .newchart
End With
End Sub
'This function will wipe everything in the current book that is not a protected record.
Public Sub CleanUp(Optional noWarn As Boolean)

Dim tbl As GenericTable
Dim protected As Dictionary
Dim sht As Worksheet
Dim chrt As Chart
Dim shts As Collection
Dim chts As Collection


If noWarn Then Application.DisplayAlerts = False

Set tbl = New GenericTable
tbl.FromSheet Worksheets("ProtectedRecords")
Set protected = New Dictionary
While Not tbl.EOF
    protected.add tbl.getField("Path"), 0
    tbl.moveNext
Wend

Set shts = New Collection

For Each sht In ActiveWorkbook.Worksheets

    If protected.count = 0 Then
        Exit For
    Else
        
        If Not protected.exists(sht.name) Then
            shts.add sht
        End If
    End If
Next sht

While shts.count > 0
    Set sht = shts(1)
    Debug.Print "deleting " & sht.name
    shts.Remove (1)
    sht.Delete
Wend

Set shts = Nothing
Set chts = New Collection

For Each chrt In ActiveWorkbook.charts
    If protected.count = 0 Then
        Exit For
    Else
        If Not protected.exists(chrt.name) Then

            chts.add chrt
        End If
    End If
Next chrt

While chts.count > 0
    Set chrt = chts(1)
    Debug.Print "deleting " & chrt.name
    chts.Remove (1)
    chrt.Delete
Wend

If noWarn Then Application.DisplayAlerts = True
    

End Sub

Public Sub BuildCharts(ParamArray charts() As Variant)
Dim chrt
For Each chrt In charts
    Select Case CLng(chrt)
        Case Is = StandardCharts.DeploymentDotPlot
            With New TimeStep_VisualDotPlots
                .newchart
            End With
        Case Is = StandardCharts.SandChart
            sandtest
    End Select
Next chrt
End Sub

Public Sub BuildAllCharts()
BuildCharts StandardCharts.DeploymentDotPlot, StandardCharts.SandChart
End Sub

'Our goal is to take the data in the GhostProportions table...
'Dice it into a set of SRCs....
    'Create (and clear out) a subfolder called Requirements
        'With Run1....RunN, FinalSupply
            'Each run folder will house the files necessary for "that" run, namely,
                'Ghost Proportions.csv
                'DemandRecords.csv
                'SupplyRecords.csv
                'RelationRecords.csv
                '

Public Sub BuildRequirementsRuns()
Dim fs As FileSystemObject
Dim streams As Dictionary

Dim tbl As GenericTable





End Sub
