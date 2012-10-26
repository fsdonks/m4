(ns marathon.port.requirementsanalysis)

'This used to be the refuge of all bitter, last-minute hacks ..... .
'Now, it is a module for storing scripts.
Option Explicit
'In an attempt to get the requirements analysis down as fast as possible, we really need to
'do an analysis to convergence for EACH src.
'This keeps the simulation size as small as possible, with the only overhead being the
'repeat loading from excel for the sim. In any case, it's still much faster than waiting
'for convergence from a HUGE simulation .... the order of growth is enourmous.
'Basically, we're going to set up the big, massive databanks under supplyrecords, etc.
'we'll have some supplementary sheets as well to help things out ....
'AS requirements scripter goes through its jig, is sits on top of a generic table.
'the generic table gets the current requirement (the SRC) , and "disables" all relevant
'tables with SRC information.
'The table pushes through each SRC, looking at a couple of other tables.
'These tables are AllDemands, Allproportions, AllRelations
'GetDemands(src) will copy the demands from AllDemands for the current SRC into the
'DemandRecords worksheet.
'Getproportions(src) will copy the proportion record in (this is easy).
'TOM Change 28 Mar 2012 -> dissecting this into something that is useful for "users."  It was formerly
'a bitter hack that turned into something more official, now I need to clean it, separate it, and hook it up to
'a GUI.  To ensure that things are loosely coupled, I am going to use observers for Requirements events, and let
'whatever - GUI, etc, hook onto the event stream contained in this module.  There might be some utility in
'pushing all of this into a class, but I think the module is okay for now.  Basically, we'll generate
'requirements events along the way, and pipe them to a custom event stream.  Anything can listen in on us and
'handle the events -> i.e. log them, display, etc. -> in any manner they see fit. This will allow the
'RequirementsUI (or anything else) to hook up to our event stream and see what's going on during the
'requirements analysis.

'GetRelations(src) will copy the relation records into the relationrecords worksheet.
'when everything is set, the scripter calls a fast requirements analysis.
'when the sub returns, scripter appends the output from Generatedsupply onto
'AllRequirements.
'I think this will run pretty quick, actually ..... Definitely faster than manually copying
'and pasting, etc.
Public demtbl As GenericTable
Public reltbl As GenericTable
Public proptbl As GenericTable
Public resultstbl As Streamer_CSV
Public errlog As Streamer_CSV
Public logstream As GenericEventStream
Public relationrecord As GenericRecord

Private Function getLogStream(Optional eventstream As GenericEventStream) As GenericEventStream

If eventstream Is Nothing Then
    Set getLogStream = New GenericEventStream
Else
    Set getLogStream = eventstream
End If

With getLogStream
    .addEvent Requirements_Msg.Standard, "Standard"
    .addEvent Requirements_Msg.Problem, "Problem"
    .addEvent Requirements_Msg.SRCCompleted, "SRCCompleted"
    .addEvent Requirements_Msg.SRCStarted, "SRCStarted"
    .addEvent Requirements_Msg.IterationStarted, "IterationStarted"
End With

End Function

'our tables must be sorted by SRC, in ascending order.
Public Sub sortTables(tbls As Dictionary)
Dim itm
Dim sheetname As String
Dim inrng As range
Dim sort As New GenericSort


For Each itm In tbls
    sheetname = CStr(itm)
    Set inrng = Worksheets(sheetname).Cells(1, 1)
    Set inrng = inrng.CurrentRegion
    sort.SortTableRange inrng, tbls(itm), True
Next itm

Set sort = Nothing
Set inrng = Nothing

End Sub
'disjoint SRCs is the set of SRCs that are not found in all tables....
Public Function DisjointSRCs(tbls As Dictionary, Optional errlog As IRecordStream) As Dictionary
Dim itm
Dim sheetname As String
Dim tbl As GenericTable
Dim src As String
Dim inrng As range
Dim Key
Dim haslog As Boolean

Dim tags As GenericTags

Set DisjointSRCs = New Dictionary
Set tags = New GenericTags 'create a tag database.

For Each itm In tbls
    sheetname = CStr(itm)
    Set tbl = New GenericTable
    tbl.FromSheet Worksheets(sheetname)
    
    While Not tbl.EOF
        src = tbl.getField(tbls(itm))
        tags.addTag src, sheetname
        tbl.moveNext
    Wend
    
    Set tbl = Nothing
Next itm

haslog = errlog Is Nothing

For Each Key In tags.tags
    src = CStr(Key)
    If tags.getSubjects(src).count <> tbls.count Then
        If haslog Then errlog.writeString "SRC " & src & " is not shared amongst all tables, and will be excluded."
        DisjointSRCs.add src, 0
    End If
Next Key

Set tags = Nothing
   
End Function

Public Sub requirementScripter(Optional eventstream As GenericEventStream)

Dim srcs As GenericTable
Dim src As String
Dim excludedSRCs As Dictionary
Dim lastdemand As range
Dim lastRelation As range
Dim lastproportion As range
Dim initial As Boolean
Dim count As Long
Dim completed As Boolean
Dim excluded As Dictionary


Dim req As Dynamic_Requirements

Set logstream = getLogStream(eventstream)

Dim tbls As Dictionary

Set tbls = newdict("RequirementList", "SRC", "AllDemands", "SRC", "AllRelations", "Recepient", "AllProportions", "SRC")
sortTables tbls

Set srcs = New GenericTable
srcs.FromSheet Worksheets("RequirementList")
Set demtbl = New GenericTable
demtbl.FromSheet Worksheets("AllDemands")
Set reltbl = New GenericTable
reltbl.FromSheet Worksheets("AllRelations")
Set proptbl = New GenericTable
proptbl.FromSheet Worksheets("AllProportions")


Set relationrecord = New GenericRecord
With relationrecord
    .AddField "Type", "RelationRecord"
    .AddField "Relation", "sub"
    .AddField "Donor", "Ghostable"
    .AddField "Recepient", ""
    .AddField "Cost", 0
    .AddField "Enabled", True
End With
Set resultstbl = New Streamer_CSV

resultstbl.StreamTo ActiveWorkbook.path & "\AllRequirements.csv"
Set errlog = New Streamer_CSV
errlog.StreamTo ActiveWorkbook.path & "\RequirementsErrors.csv"
errlog.StreamWrite "SRC, Status, Reason"
Set lastdemand = Worksheets("AllDemands").Cells(2, 9)
Set lastRelation = Worksheets("AllRelations").Cells(1, 4)
Set lastproportion = Worksheets("Allproportions").Cells(2, 3)
count = 0

Set excludedSRCs = DisjointSRCs(tbls, errlog)

While Not srcs.EOF And count <= 160
    'If srcs.getField("completed") = False Then
    src = srcs.getField("SRC")
    If Not excludedSRCs.exists(src) Then
        loadSRC src, lastdemand, lastRelation, lastproportion 'load src-specific data into sheets
        Set req = New Dynamic_Requirements 'batch run marathon with loaded data
        req.FastConvergence 'I Took this out by accident, ugh!
        'Requirements.fastRequirementsTest
        completed = appendresults(Worksheets("GeneratedSupply"), resultstbl, count) 'accumulate results
        If completed Then
            errlog.StreamWrite src & "," & "Complete" & ", Successful"
        Else
            errlog.StreamWrite src & "," & "Incomplete" & ", Unknown Error"
        End If
        count = count + 1
        srcs.moveNext
        DoEvents
        Debug.Print src & " completed.  " & count & " SRCs completed!"
        Set req = Nothing 'cleanup
    Else
        Debug.Print "SRC " & src & " excluded"
        errlog.StreamWrite src & "," & "Incomplete" & ", SRCExcluded"
        srcs.moveNext
    End If
Wend


Set errlog = Nothing
Set resultstbl = Nothing

If excludedSRCs.count > 0 Then MsgBox "There were " & excludedSRCs.count & " excluded SRCs.  Check the errorlog....", vbCritical

memclr

End Sub
Private Sub loadSRC(src As String, demptr As range, relptr As range, propptr As range)
loaddemand src, demptr
loadrelations src, relptr
loadproportions src, propptr
End Sub
Private Sub clearTable(sheetname As String)

Static ptr As range
Static ws As Worksheet
Set ws = Worksheets(sheetname)
Set ptr = ws.Cells(1, 1)
Set ptr = ptr.CurrentRegion
Set ptr = ptr.offset(1, 0)
If sheetname = "RelationRecords" Then Set ptr = ptr.offset(1, 0) 'keep the ghost around.
ptr.Clear
Set ptr = Nothing

End Sub
Private Sub loaddemand(src As String, demptr As range)
Dim stream As Streamer_xl
Set stream = New Streamer_xl
clearTable "DemandRecords"
stream.StreamTo "DemandRecords", Worksheets("DemandRecords").Cells(2, 1)
'clear the demand
While demptr.value <> src
    Set demptr = demptr.offset(1, 0)
    demtbl.moveNext
Wend
While demptr.value = src
    stream.StreamWrite demtbl.getRecord
    Set demptr = demptr.offset(1, 0)
    demtbl.moveNext
Wend

Set stream = Nothing
End Sub
Private Sub loadrelations(src As String, relptr As range)
Dim stream As Streamer_xl
clearTable "RelationRecords"
Set stream = New Streamer_xl
stream.StreamTo "RelationRecords", Worksheets("RelationRecords").Cells(3, 1)
'clear the demand
'while relptr.value <> src
', Set relptr = relptr.offset(l, 0)
','w end
'while relptr.value = src
', stream.Streamwrite reltbl.getRecord

Set relptr = relptr.offset(1, 0)
reltbl.moveNext

With relationrecord
    .UpdateField "Recepient", src
End With

stream.StreamWrite relationrecord.FieldValues
Set stream = Nothing


End Sub
Private Sub loadproportions(src As String, propptr As range)
Dim stream As Streamer_xl

clearTable "GhostproportionsAggregate"
Set stream = New Streamer_xl

stream.StreamTo "GhostproportionsAggregate", Worksheets("GhostproportionsAggregate").Cells(2, 1)
'clear the demand
While propptr.value <> src
    Set propptr = propptr.offset(1, 0)
    proptbl.moveNext
Wend
While propptr.value = src
    stream.StreamWrite proptbl.getRecord
    Set propptr = propptr.offset(1, 0)
    proptbl.moveNext
Wend
Set stream = Nothing

End Sub
Private Function appendresults(sheet As Worksheet, stream As IRecordStream, count As Long) As Boolean
Dim tbl As GenericTable
Set tbl = New GenericTable
tbl.FromSheet sheet
If count = 0 Then stream.writerecord tbl.fieldnames

While Not tbl.EOF
    If tbl.getField("SRC") <> "Ghost" Then
        stream.writerecord tbl.getRecord
        appendresults = True
    End If
    tbl.moveNext
Wend

End Function

'Test the generated supply using a capacity analysis....
'Basically, import TheRequiredForce
Public Sub CapacityTest()

End Sub
