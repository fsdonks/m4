(ns marathon.port.graphio)

Option Explicit
'Renders a nested sequence of graphs, where graphs is a dictionary of [graphname->subgraph], or [groupname->group]
'Implicitly renders child graphs in a group as a cluster in a graphviz dot file.
'Makes it easier to compose representations of graphs...particularly for composite policies.
Public Function nestedDot(rootname As String, graphs As Dictionary, Optional child As Boolean, Optional clusters As Dictionary, Optional indices As Dictionary) As String
'establish rootname as a node in the dot file, with arcs to each key in graphs.
Dim g
Dim gr As IGraph
Dim subg As GenericGraph
Dim ndot As String
Dim dummy As String

If Not child Then
    ndot = "digraph " & "Root" & " {" & vbCrLf & "compound=true;" & vbCrLf
    Set clusters = New Dictionary
    ndot = ndot & "subgraph " & "cluster_0" & " {"
    'ndot = ndot & "cluster_0_dummy [label=" & literal(rootname) & "] " & vbCrLf
    'ndot = ndot & "cluster_0_dummy [shape = point style = invis] " & vbCrLf
    ndot = ndot & "label = " & literal(rootname) & ";" & vbCrLf
    ndot = ndot & "fontsize =30;"
    clusters.add rootname, "cluster_0"
    Set indices = New Dictionary
    indices.add indices.count, "cluster_0_dummy"
Else
    clusters.add rootname, "cluster_" & clusters.count
End If

For Each g In reverse(listKeys(graphs))
    If TypeName(graphs(g)) = "Dictionary" Then
        clusters.add CStr(g), "cluster_" & clusters.count
        ndot = ndot & nestedDot(CStr(g), graphs(g), True, clusters, indices)
    Else
        Set gr = graphs(g)
        clusters.add g, "cluster_" & clusters.count
        ndot = ndot & toDot(gr, clusters(g), True, , False, indices, CStr(g)) & vbCrLf
        Set gr = Nothing
    End If
    'dummy = clusterArc(clusters(rootname), clusters(g), dummyNodeName(clusters(rootname)), dummyNodeName(CStr(clusters(g))))
    indices.add indices.count, dummy
    ndot = ndot & dummy & vbCrLf
Next g
    
    

    
nestedDot = ndot & "}"
If Not child Then nestedDot = ndot & "}" & vbCrLf & "}"


End Function
Private Function clustername(Optional clusters As Collection) As String
If exists(clusters) Then
    clustername = "cluster_" & clusters.count + 1
Else
    clustername = "cluster_0"
End If

End Function
Public Function dummyNodeName(root As String) As String
dummyNodeName = root & "_dummy"
End Function
Public Function dummyNode(root As String) As String
dummyNode = literal(dummyNodeName(root)) & " [shape = point style = invis];"
End Function

''simple  function to publish a dot file from our graph, so we can visualize it in graphviz.
Public Function toDot(g As IGraph, Optional externalname As String, _
                        Optional subgraph As Boolean, Optional name As String, Optional rendersubs As Boolean, Optional indices As Dictionary, Optional label As String) As String
                                            
Dim arc
Dim arcs As Dictionary
Dim myname As String
Dim graphtype As String
Dim nd
Dim islands As Dictionary
Dim grph
Dim graphptr As IGraph
Dim compound As String
Dim count As Long
Dim subgraphs As Dictionary
Dim localindex As Dictionary

If externalname <> vbNullString Then
    myname = externalname
Else
    If name = vbNullString Then myname = "SomeGraph" Else myname = name
End If

If subgraph Then
    graphtype = "subgraph "
    compound = "compound=true;" & vbCrLf
Else
    If g.isDirected Then
        graphtype = "digraph "
    Else
        graphtype = "graph "
    End If
    'compound = "clusterrank=none;" & vbCrLf
End If

toDot = graphtype & myname & " {" & vbCrLf & compound
If label <> vbNullString Then
    toDot = toDot & "label = " & literal(label) & vbCrLf
End If

If indices Is Nothing Then Set indices = New Dictionary

Set localindex = New Dictionary

If rendersubs Then
    Set subgraphs = calculateSubGraphs(g)
    For Each grph In subgraphs
        Set graphptr = subgraphs(grph)
        toDot = toDot & dotSubGraph(graphptr, "cluster" & count, indices)
        count = count + 1
    Next grph
Else
    'TOM change 23 Mar 2011
    'Set islands = New Dictionary
    'render nodes first
    'Add a dummy node....if this is a subgraph
    If subgraph Then toDot = toDot & dummyNode(myname) & vbCrLf
    indices.add indices.count, dummyNode(myname)
    For Each nd In getNodes(g)
'        Set ptr = getNeighbors(CStr(nd))
'        If ptr.count = 0 Then islands.add nd, 0
        localindex.add CStr(nd), indices.count
        indices.add indices.count, CStr(nd)
        toDot = toDot & dotNode(CStr(nd), indices.count)
    Next nd
    Set arcs = getArcs(g)
    If g.isDirected Then
        For Each arc In arcs
            toDot = toDot & dotArc(CStr(arc), arcs(arc), , localindex) & vbCrLf
        Next arc
    Else
        For Each arc In arcs
            toDot = toDot & dotArc(replace(CStr(arc), "->", "--"), arcs(arc), "--") & vbCrLf
        Next arc
    End If
    
'    For Each nd In islands
'        toDot = toDot & dotNode(CStr(nd))
'    Next nd
End If

toDot = toDot & "}"

Set islands = Nothing
Set graphptr = Nothing

End Function
Public Function dotSubGraph(graph As IGraph, Optional externalname As String, Optional indices As Dictionary) As String
'If graph Is Nothing Then Set graph = Me
dotSubGraph = toDot(graph, externalname, True, , , indices)
End Function
Public Function dotNode(ByRef node As String, nodecount As Long) As String
'23 Mar 2011 TOM Change
dotNode = "node_" & nodecount & " [fontsize = " & 24 & ", label=" & literal(node) & "]" & ";" & vbCrLf
End Function
Public Function literal(source As String) As String
literal = Chr(34) & source & Chr(34)
End Function
Public Function dotArc(arc As String, weight As Single, Optional delim As String, Optional localindex As Dictionary) As String

Dim startnode As String
Dim endnode As String
Dim lbl As String
Dim tmp

If delim = vbNullString Then delim = "->"
If weight <> 0 Then
    lbl = " [label = " & weight & ", fontsize = " & 24 & "]"
Else
    lbl = vbNullString
End If

tmp = Split(arc, delim)
startnode = tmp(0)
endnode = tmp(1)

If exists(localindex) Then
    startnode = "node_" & (localindex(startnode) + 1)
    endnode = "node_" & (localindex(endnode) + 1)
    dotArc = startnode & " " & delim & " " & endnode
Else
    dotArc = literal(startnode) & " " & delim & " " & literal(endnode)
End If


dotArc = dotArc & lbl & ";"

End Function
Public Function dotArcNodes(startnode As String, endnode As String, weight As Single, Optional delim As String) As String

Dim lbl As String
Dim tmp

If delim = vbNullString Then delim = "->"
If weight <> 0 Then
    lbl = " [label = " & weight & ", fontsize = " & 24 & "]"
Else
    lbl = vbNullString
End If

dotArcNodes = literal(startnode) & " " & delim & " " & literal(endnode)

dotArcNodes = dotArcNodes & lbl & ";"
End Function
Public Function compoundGraph(dot As String) As String
compoundGraph = "compound = true;" & vbCrLf & dot
End Function
Public Function clusterArc(startcluster As String, stopcluster As String, startnode As String, endnode As String) As String

Dim lbl As String
Dim tmp

lbl = " [lhead = " & startcluster & ", ltail = " & stopcluster & "]"

clusterArc = literal(startnode) & " -> " & literal(endnode) & lbl & ";"

End Function


Public Function dotdelim() As String

'If directed Then
'    dotdelim = parsedelim(dotformatDiGraph)
'Else
'    dotdelim = parsedelim(dotformatGraph)
'End If
dotdelim = "->"
End Function
Private Function xmlNode(ByRef node As String) As String
xmlNode = "<node>" & node & "</node>"
End Function
Private Function xmlArc(arc As String, weight As Single) As String
Dim tmp
Dim body As String
xmlArc = "<arc>[BODY]</arc>"
tmp = Split(arc, "->")
body = xmlNode(CStr(tmp(0))) & vbCrLf & xmlNode(CStr(tmp(1)))
body = body & vbCrLf & "<weight>" & weight & "</weight>"
xmlArc = replace(xmlArc, "[BODY]", body)
End Function
Private Function xmlDiGraph(g As IGraph, name As String) As String
Dim body As String
Dim dot As String
Dim nd
Dim arc
Dim spaces As Long
Dim arcs As Dictionary

xmlDiGraph = "<digraph>[BODY]</digraph>"

spaces = 1
body = indent("<name>" & name & "</name>" & vbCrLf, spaces)
body = body & indent("<currentdelimiter>" & "->" & "</currentdelimiter>" & _
                vbCrLf, spaces)
Set arcs = getArcs(g)
For Each arc In arcs
    body = body & indent(xmlArc(CStr(arc), arcs(arc)) & vbCrLf, spaces + 2)
Next arc
body = body & indent("<dot>[BODY]</dot>", spaces)
dot = "<![CDATA[" & toDot(g) & "]]>"
body = replace(body, "[BODY]", dot)

xmlDiGraph = replace(xmlDiGraph, "[BODY]", body)

End Function
Public Function toXml(g As IGraph, Optional name As String) As String
If name = vbNullString Then name = "SomeGraph"
toXml = xmlDiGraph(g, name)
End Function
Private Function indent(source As String, amount As Long) As String
indent = Space(amount) & source
End Function
'build a tall table of the arcs, edge weights
Public Function toTable(g As IGraph) As Variant()
Dim tmp()
Dim arc
Dim arcs As Dictionary
Dim record
Dim row As Long
If getArcs(g).count > 0 Then
    ReDim tmp(1 To getArcs(g).count + 1, 1 To 6)
    tmp(1, 1) = "Name"
    tmp(1, 2) = "Source"
    tmp(1, 3) = "Destination"
    tmp(1, 4) = "Weight"
    tmp(1, 5) = "Information"
    tmp(1, 6) = "Delimiter"
    
    row = 1
    Set arcs = getArcs(g)
    For Each arc In arcs
        row = row + 1
        record = Split(CStr(arc), "->")
        tmp(row, 1) = arc
        tmp(row, 2) = record(0) 'node1
        tmp(row, 3) = record(1) 'node2
        tmp(row, 4) = CSng(arcs(arc)) 'weight
        tmp(row, 5) = "None" 'ArcInformation(arc)
        tmp(row, 6) = "->"
    Next arc
Else
    Err.Raise 101, , "No arcs!"
End If

toTable = tmp
End Function
'read a nx3 range into a graph structure
'4th column, if <> vbnullstring is read as text, no data associated with nodes
Public Function Read_Xl(arcrng As range, Optional noderange As range) As GenericGraph
Dim row As range
Dim tmp()
Set Read_Xl = New GenericGraph

If arcrng.Columns.count <> 3 Then Err.Raise 101, , "3 columns for each arc"
    For Each row In arcrng.rows
        tmp = row
        Set Read_Xl = addArc(Read_Xl, CStr(tmp(1, 1)), CStr(tmp(1, 2)), CSng(tmp(1, 3)))
    Next row
End Function
Public Sub Plot_Xl(rng As range, g As IGraph)
Dim tbl()

tbl = toTable(g)
Set rng = rng.resize(UBound(tbl, 1), UBound(tbl, 2))
rng = tbl

End Sub

Public Sub renderDots(dots As Dictionary, Optional htmlname As String)
Dim mygraph As GenericGraph
Dim grph
Dim ts As TextStream
Dim fs As FileSystemObject
Dim path As String
Dim grapher As GraphVizHandler
Dim simplehtm As String
Dim headers As Dictionary
Dim gheader As String

Set fs = New FileSystemObject
path = ActiveWorkbook.path & "\"
If htmlname = vbNullString Then htmlname = "Default.html"
htmlname = path & htmlname
Set grapher = New GraphVizHandler 'assumes graphviz is installed in documents\graphviz\bin.
'should seek it out under xp and vista
'grapher.init "C:\Users\thomas.spoon\Documents\graphviz\bin\", path
grapher.outpath = path
If grapher.hasPath Then
    simplehtm = "<html>" & vbCrLf & "<body>" & vbCrLf
    grapher.saveas = png 'better resolution
    
    For Each grph In dots
        grapher.render , dots(grph), CStr(grph), True
        simplehtm = simplehtm & graphhtml(CStr(grph), CStr(grph) & grapher.imagesuffix)
    Next grph
    
    simplehtm = simplehtm & "</body>" & vbCrLf & "</html>" & vbCrLf
    Set ts = fs.CreateTextFile(htmlname, True)
    ts.Write simplehtm
    ts.Close
    Set ts = Nothing
    Set fs = Nothing
Else
    Err.Raise 101, , "Can't find graphviz, assumes bins are in documents\graphviz\bin"
End If
End Sub


Sub test()
Dim sample As String
sample = "digraph Root {compound=true; subgraph Composite { " & _
            "Composite_Dummy [shape = point style = invis]; subgraph PreSurge {" & _
            "compound=true; PreSurge_dummy [shape = point style = invis]; Reset [fontsize = 24];" & _
            "Train [fontsize = 24];" & _
            "Ready [fontsize = 24];" & _
            "Available [fontsize = 24];" & _
            "Deployed [fontsize = 24];" & _
            "Overlapping [fontsize = 24];" & _
            "Reset -> Train [label = 182, fontsize = 24];" & _
            "Train -> Ready [label = 183, fontsize = 24];" & _
            "Ready -> Available [label = 460, fontsize = 24];" & _
            "Available -> Reset [label = 270, fontsize = 24];" & _
            "Deployed -> Overlapping [label = 270, fontsize = 24];" & _
            "Overlapping -> Reset;}" & _
        "Composite_dummy -> PreSurge_dummy [lhead = Composite, ltail = PreSurge];}}"

renderDots newdict("sample", sample)
End Sub

