(ns marathon.port.graphlib)


'A library for operating on the GenericGraph class.
'12 April 2012
'This is an adaption of my Clojure library cljgraph, which was a more mature implementation of my
'original GenericGraph library (from VBA).  I've chosen to port the clojure implementation, which
'takes inspiration from Robert Sedgewick's C libraries, and from the Java Universal Network and Graph
'library (at least partially).  The intent is to provide a fairly consistent, functional API between
'multiple platforms.
'-Tom Spoon
'''9-12-2011
'''Note : Greg Heck passed away today.  He was a good man, and his death was untimely.  Thanks for all
'''the mentoring, and your good nature.  We were blessed to have had you in our lives.

Public Enum fringefunctions
    sinks = 0
    sources = 1
End Enum
Public Enum filtertype
    Collect = 0
    exclude = 1
End Enum
Option Explicit
';;default constructor for IGraphs is a mapgraph (since it's implemented first!)
'(defn make-graph
'  ([] (cljgraph.data.mapgraph/make-mapgraph)))
';Protocol-derived functionality.  We define higher order graph operations on
';top of protocol functions.  This allows us to change the implementation
';to get performance benefits.
Public Function makeGraph() As IGraph
Set makeGraph = New GenericGraph
End Function
Public Function EncodeArc(ByRef node1 As String, ByRef node2 As String, Optional delimiter As String) As String
If delimiter = vbNullString Then delimiter = "->"
EncodeArc = node1 & delimiter & node2
End Function
Public Function getArc(g As IGraph, ByRef nodeFrom As String, ByRef nodeTo As String) As Single
getArc = g.getArc(nodeFrom, nodeTo)
End Function
Public Function getArcLabel(g As IGraph, ByRef arclabel As String) As Single
getArcLabel = g.getArcs(arclabel)
End Function
Public Function hasNode(g As IGraph, ByRef node As String) As Boolean
hasNode = g.hasNode(node)
End Function
Public Function isPathPossible(g As IGraph, ByRef startnode As String, ByRef endnode As String) As Boolean
isPathPossible = hasNode(g, startnode) And hasNode(g, endnode)
End Function
Public Function getSinks(g As IGraph, ByRef node As String) As Dictionary
Set getSinks = g.getSinks(node)
End Function
Public Function getSources(g As IGraph, ByRef node As String) As Dictionary
Set getSources = g.getSources(node)
End Function
Public Function getNeighbors(g As IGraph, ByRef node As String) As Dictionary
Set getNeighbors = g.getNeighbors(node)
End Function
Public Function getNodes(g As IGraph) As Dictionary
Set getNodes = g.getNodes
End Function
Public Function getArcs(g As IGraph) As Dictionary
Set getArcs = g.getArcs
End Function
Public Function hasArc(g As IGraph, ByRef fromnode As String, ByRef tonode As String) As Boolean
hasArc = g.hasArc(fromnode, tonode)
End Function
Public Function hasArcLabel(g As IGraph, ByRef arclabel As String) As Boolean
hasArcLabel = g.getArcs.exists(arclabel)
End Function
Public Function addArc(g As IGraph, ByRef nodeFrom As String, ByRef nodeTo As String, Optional weight As Single, Optional commutative As Boolean) As IGraph
If commutative Then 'intercept the default error handler
    If g.hasArc(nodeFrom, nodeTo) Then
        'do nothing
    Else
        g.addArc nodeFrom, nodeTo, weight
    End If
Else
    g.addArc nodeFrom, nodeTo, weight
End If

Set addArc = g
End Function
Public Function asGraph(ByRef g As Variant) As IGraph
Set asGraph = g
End Function
Public Function addNode(g As IGraph, ByRef node As String) As IGraph
g.addNode node
Set addNode = g
End Function
Public Function complement(ByRef arc As String) As String
Dim tmp
tmp = Split(arc, "->")
complement = EncodeArc(CStr(tmp(1)), CStr(tmp(0)))
End Function
'Right now, we're assuming no disabled arcs or nodes.  This is a complete cloning.
Public Function cloneGraph(g As IGraph, Optional asUndirected As Boolean) As IGraph
Dim itm
Dim arcs As Dictionary
Dim clone As GenericGraph
Dim tmp

'If Disabled.count > 0 Then Err.Raise 101, , "Clone doesn't work with disabled arcs at the moment"

Set cloneGraph = New GenericGraph

If asUndirected = True Then
    cloneGraph.setUnDirected
Else
    cloneGraph.setDirected
End If
'most of this is automated, so we should only have to add arcs.
For Each itm In getNodes(g)
    Set cloneGraph = addNode(cloneGraph, CStr(itm))
Next itm

Set arcs = getArcs(g)
For Each itm In arcs
     tmp = Split(CStr(itm), "->")
     Set cloneGraph = addArc(cloneGraph, CStr(tmp(0)), CStr(tmp(1)), arcs(itm))
Next itm

End Function
'Right now, we're assuming no disabled arcs or nodes.  We do allow filtering of arcs, so when
'the new clone is created, it ignores filtered nodes, their incident arcs, and explicitly filtered
'arcs.
Public Function cloneFiltered(g As IGraph, Optional nodefilter As Dictionary, Optional arcfilter As Dictionary, _
                                    Optional ftype As filtertype, Optional asUndirected As Boolean) As IGraph
Dim node1 As String, node2 As String, arcname As String
Dim tmp
Dim itm
Dim neighbors As Dictionary
Dim neighbor
Dim arc As String
Dim arcs As Dictionary
Dim nfilter As Boolean, afilter As Boolean
Dim comp As String

nfilter = Not (nodefilter Is Nothing)
afilter = Not (arcfilter Is Nothing)


'If Disabled.count > 0 Then Err.Raise 101, , "Clone doesn't work with disabled arcs at the moment"

Set cloneFiltered = New GenericGraph

If asUndirected = True Then
    cloneFiltered.setUnDirected
Else
    cloneFiltered.setDirected
End If

With cloneFiltered

    If Not nfilter Then Set nodefilter = New Dictionary
    If Not afilter Then Set arcfilter = New Dictionary
    
    If ftype = exclude Then
        For Each itm In getNodes(g)
            If Not nodefilter.exists(CStr(itm)) Then
                .addNode CStr(itm)
                Set neighbors = getNeighbors(g, CStr(itm))
                For Each neighbor In neighbors
                    arc = neighbors(neighbor)
                    If Not arcfilter.exists(arc) Then arcfilter.add arc, 0
                    If g.isDirected And asUndirected Then  'if we're copying from a directed source to an undirected...
                        'exclude any neighbor arcs that are complements, they're unecessary.
                        arcfilter.add complement(CStr(arc)), 0
                    End If
                Next neighbor
            End If
                
        Next itm
        Set arcs = getArcs(g)
        For Each itm In arcs
            If Not arcfilter.exists(CStr(itm)) Then
                tmp = Split(CStr(itm))
                node1 = tmp(0)
                node2 = tmp(1)
                .addArc node1, node2, arcs(itm)
            End If
        Next itm
    ElseIf ftype = Collect Then
        For Each itm In getNodes(g)
            If nodefilter.exists(CStr(itm)) Then
                .addNode CStr(itm)
                Set neighbors = getNeighbors(g, CStr(itm))
                For Each neighbor In neighbors
                    arc = neighbors(neighbor)
                    If Not (g.isDirected And asUndirected) Then
                        If Not arcfilter.exists(arc) Then arcfilter.add arc, 0 'make sure the neighbor arcs are collected
                    Else
                        comp = complement(CStr(arc))
                        'the only time we don't include an arc is if it's complement has already been added.
                        'we can check the inclusive arcfilter to see if the complement is in there (previously added).
                        If Not arcfilter.exists(comp) Then arcfilter.add arc, 0
                    End If
                Next neighbor
            End If
                
        Next itm
        Set arcs = getArcs(g)
        For Each itm In arcs
            If arcfilter.exists(CStr(itm)) Then
                tmp = Split(CStr(itm), "->")
                node1 = tmp(0)
                node2 = tmp(1)
                .addArc node1, node2, arcs(itm)
            End If
        Next itm
    Else
        Set cloneFiltered = cloneGraph(g, asUndirected)
    End If
End With

Set neighbors = Nothing

End Function
Public Function getUnDirected(g As IGraph, Optional Verbose As Boolean) As IGraph
Dim arc
Dim ugraph As GenericGraph

Set ugraph = cloneGraph(g, True)

If g.isDirected And Verbose Then
    With ugraph
        For Each arc In .arcs
            .addComplement (CStr(arc))
        Next arc
    End With
End If

Set getUnDirected = ugraph
Set ugraph = Nothing

End Function


'(defn u-arcbound [nodecount]
'  (/ (* nodecount (dec nodecount)) 2))
'
Public Function upperArcbound(nodecount As Long) As Long
upperArcbound = ((nodecount - 1) * nodecount) / 2
End Function
'(defn d-arcbound [nodecount]
'  (* nodecount (dec nodecount)))
'
Public Function lowerArcbound(nodecount As Long) As Long
lowerArcbound = nodecount * (nodecount - 1)
End Function

'(def digraph? (partial flip has-attr? ::digraph))
Public Function isDigraph(g As IGraph) As Boolean
isDigraph = g.isDirected
End Function
'(def dag? (partial flip has-attr? ::dag))
Public Function isDAG(g As IGraph) As Boolean
End Function
'(def ugraph? (partial flip has-attr? ::ugraph))
Public Function isUndirected(g As IGraph) As Boolean
isUndirected = g.isUndirected
End Function
'(defn init-graph
'  "Initialize an IGraph (constructed somewhere else) with basic attributes.
'   Assumes all graphs are directed by nature."
'  ([g] (add-attrs g [::digraph]))
'  ([g attrs] (add-attrs g attrs)))


';Protocol-derived functionality.  We define higher order graph operations on
';top of protocol functions.  This allows us to change the implementation
';to get performance benefits.
'
'(defn add-nodes
'  "Record-specific implementation for adding multiple nodes to g."
'  [g nodes]
'  (reduce add-node g nodes))
Public Function addNodes(g As IGraph, nodes As Collection) As IGraph
Set addNodes = g
Dim nd
For Each nd In nodes
    g.addNode CStr(nd)
Next nd

End Function
'(defn add-arcs
'  "Return new graph representing addition of arcs to g"
'  [g arcs] (reduce add-arc g arcs))

Public Function addArcs(g As IGraph, arcs As Collection) As IGraph
'Dim arc As GenericArc
'Set addArcs = g
'For Each arc In arcs
'    g.addArcGeneric arc
'Next arc

End Function
'(def missing-node? (comp not has-node?))
Public Function missingNode(g As IGraph, ByRef node As String) As Boolean
missingNode = Not hasNode(g, node)
End Function

'(defn terminal-node?
'  "Is node n devoid of outbound arcs?"
'  [g n] (not (seq (get-sinks g n))))
Public Function isTerminalNode(g As IGraph, ByRef node As String) As Boolean
If hasNode(g, node) Then
    isTerminalNode = g.getSources(node).count = 0
Else
    Err.Raise 101, , "Node " & node & " does not exist in graph!"
End If
End Function

'(defn source-node?
'  "Is node n devoid of inbound arcs?"
'  [g n] (not (seq (get-sources g n))))
Public Function isSourceNode(g As IGraph, ByRef node As String) As Boolean
If hasNode(g, node) Then
    isSourceNode = getSources(g, node).count > 0
Else
    Err.Raise 101, , "Node " & node & " does not exist in graph!"
End If
End Function

'(defn island?
'  "Does node n have any neighbors?"
'  [g n] (and (terminal-node? g n) (source-node? g n)))
Public Function isIsland(g As IGraph, ByRef node As String) As Boolean
If hasNode(g, node) Then
    isIsland = (getSources(g, node).count > 0) And (getSinks(g, node).count > 0)
Else
    Err.Raise 101, , "Node " & node & " does not exist in graph!"
End If
End Function
'(defn drop-nodes [g coll] (reduce drop-node g coll))
Public Function dropNodes(g As IGraph, nodes As Collection) As IGraph
Dim nd
Set dropNodes = g
For Each nd In nodes
    g.dropNode CStr(nd)
Next nd

End Function
'(defn- union-find
'  [g nd fringef]
'  (loop [fringe (fringef g nd) ;<----makes a set!
'         found fringe]
'    (if (seq fringe)
'      (let [[knext frnext]
'                (reduce
'                 (fn [[known xs] n]
'                   (let [ys (set-theory/difference (fringef g n) known)
'                          k (set-theory/union known ys)]
'                     [k (set-theory/union xs ys)])) [found #{}] fringe)]
'        (recur frnext knext))
'      found)))
'need to implement this a bit differently, using DFS
Public Function unionFind(g As IGraph, ByRef node As String, Optional fringef As fringefunctions) As Dictionary
Dim currnd As String
Dim fringe As GenericFringe
Dim found As Dictionary
Dim chld

Set found = New Dictionary   'initialize our found nodes, emptyset
Set fringe = makeDepthFringe() 'newdict(node, 1) 'push startnode onto the fringe

fringe.addNode node
found.add node, found.count + 1
Select Case fringef
    Case fringefunctions.sinks
        While fringe.count > 0
            currnd = CStr(fringe.peekNode)
            fringe.popNode
            For Each chld In getSinks(g, currnd) 'mutate!
                If Not found.exists(CStr(chld)) Then
                    fringe.addNode CStr(chld)
                    found.add CStr(chld), found.count + 1
                End If
            Next chld
        Wend
        
    Case fringefunctions.sources
        While fringe.count > 0
            currnd = CStr(fringe.peekNode)
            fringe.popNode
            For Each chld In getSources(g, currnd) 'mutate!
                If Not found.exists(CStr(chld)) Then
                    fringe.addNode CStr(chld)
                    found.add CStr(chld), found.count + 1
                End If
            Next chld
        Wend
End Select

Set unionFind = found

End Function


'(defn get-indegree
'  "What is the in-degree of the node?  For directed graphs, how many incident
'   arcs does this node serve as the sink for?  Self-loops count twice."
'  [g nd]
'  (get-degree g nd get-sources))
Public Function getInDegree(g As IGraph, ByRef node As String)
getInDegree = getSources(g, node).count
End Function

'(defn get-outdegree
'  "What is the in-degree of the node?  For directed graphs, how many incident
'   arcs does this node serve as the sink for?  Self-loops count twice."
'  [g nd]
'  (get-degree g nd get-sinks))

Public Function getOutDegree(g As IGraph, ByRef node As String)
getOutDegree = getSinks(g, node).count
End Function

'
'(defn get-predecessors
'  "Find all nodes that are predecessors to nd.  Node A precedes B if there is
'   any path connecting A to B."
'  [g nd]
'  (union-find g nd get-sources))
Public Function getPredecessors(g As IGraph, ByRef node As String) As Dictionary
Set getPredecessors = unionFind(g, node, sources)
End Function
'(defn get-successors
'  "Find all nodes that are successors to nd.  Node B succeeds A if there is
'   any path of outgoing edges from A to B."
'  [g nd]
'  (union-find g nd get-sinks))
Public Function getSuccessors(g As IGraph, ByRef node As String) As Dictionary
Set getSuccessors = unionFind(g, node, sinks)
End Function
'
'
'(load-relative ["cljgraph.data.mapgraph"
'                "cljgraph.algorithms.topsort"
'                "cljgraph.io"])
'
''Shortest path container functions
Public Function emptyPath() As GenericSearchData
Set emptyPath = New GenericSearchData
End Function
'(defn- new-path
'  "When we discover a new path via from source to sink, we add sink to the
'   shortest path tree, register the distance, and add source to the fringe."
'  [source sink w shortest distance fringe]
'    (djikstats. (assoc shortest sink source)
'                (assoc distance sink w)
'                (fr/conj-fringe fringe sink w)))
Public Function newPath(searchstats As GenericSearchData, ByRef source As String, sink As String, wnew As Single) As GenericSearchData
With searchstats
    .shortest.add sink, source
    .distance.add sink, wnew
    .fringe.addNode sink, wnew
End With
Set newPath = searchstats
End Function
'(defn- shorter-path
'  "When a shorter path is found to a node already on the fringe, we update the
'   SPT, distance, and re-weight the fringe based on the new path."
'  ([source sink wnew wpast shortest distance fringe]
'    (djikstats. (assoc shortest sink source) ;new spt
'                (assoc distance sink wnew)  ;shorter distance
'                (fr/re-weigh fringe sink wpast wnew))))
'
Public Function shorterPath(searchstats As GenericSearchData, ByRef source As String, ByRef sink As String, wnew As Single, wpast As Single) As GenericSearchData
With searchstats
    .shortest.item(sink) = source
    .distance.item(sink) = wnew
    .fringe.reWeigh sink, wpast, wnew
End With
Set shorterPath = searchstats
End Function
'(defn- equal-path [source sink shortest distance fringe]
'  "When we discover equivalent paths, we conj them onto the shortest path tree.
'   Note, if a better path is found, the other paths will be eliminated."
'  (let [current (get shortest sink)
'        context (if (vector? current) current [current])
'        newspt (assoc shortest sink (conj context source))]
'       (djikstats. newspt distance fringe)))
Public Function equalPath(searchstats As GenericSearchData, ByRef source As String, ByRef sink As String) As GenericSearchData
Dim paths As Collection
With searchstats
    If IsObject(.shortest(sink)) Then
        Set paths = .shortest(sink)
        paths.add sink
    Else
        Set paths = New Collection
        paths.add .shortest(sink)
        paths.add source
        .shortest.Remove (sink)
        .shortest.add sink, paths
    End If
End With
Set equalPath = searchstats
End Function
'(defn- relax*
'  "Given a shortest path map, a distance map, a source node, sink node,
'   and weight(source,sink) = w, update the search state.
'
'   The implication of a relaxation on sink, relative to source, is that
'   source no longer exists in the fringe (it's permanently labeled).
'   So a relaxation can mean one of three things:
'   1: sink is a newly discovered-node (as a consequence of visiting source);
'   2: sink was visited earlier (from a different source), but this visit exposes
'      a shorter path to sink, so it should be elevated in consideration in
'      the search fringe.
'   3: sink is a node of equal length to the currently shortest-known path from
'      an unnamed startnode.  We want to record this equivalence, which means
'      that we may ultimately end up with multiple shortest* paths."
'
'  [source sink w {:keys [shortest distance fringe] :as state}]
'    (let [relaxed (+ (get distance source) w)]
'      (if-let [known (distance sink)]
'          (cond
'            (< relaxed known) (shorter-path source sink relaxed known state)
'            (= relaxed known) (equal-path source sink state)
'            :else state)
'       ;if sink doesn't exist in distance, sink is new...
'       (new-path* source sink relaxed state))))
Public Function relax(ByRef source As String, ByRef sink As String, w As Single, searchstats As GenericSearchData) As GenericSearchData
Dim relaxed As Single
Dim known As Single

relaxed = searchstats.distance(source) + w
If searchstats.distance.exists(sink) Then
    known = searchstats.distance(sink)
    Select Case relaxed
        Case Is < known
            Set relax = shorterPath(searchstats, source, sink, relaxed, known)
        Case Is = known
            Set relax = equalPath(searchstats, source, sink)
        Case Else
            Set relax = searchstats ' no change
    End Select
Else
    Set relax = newPath(searchstats, source, sink, relaxed)
End If
searchstats.discovered.add sink
End Function
'(defn graph - walk
'  "Generic fn to walk a graph.  The type of walk can vary by changing the
'   fringe of the searchstate, the halting criteria, the weight-generating
'   function, or criteria for filtering candidates.  Returns a searchstate
'   of the walk, which contains the shortest path trees, distances, etc. for
'   multiple kinds of walks, depending on the searchstate's fringe structure."
'  [g startnode targetnode state  & {:keys [halt? weightf neighborf]
'                                    :or {halt? default-halt?
'                                         weightf get-weight
'                                         neighborf default-neighborf} }]
'    (let [walker
'          (fn walker [g targetnode {:keys [fringe] :as searchstate}]
'                      (if-let [candidate (fr/next-fringe fringe)]
'                        (let [w (:weight candidate) ;perceived weight from start
'                              nd (:node candidate)]
'                            (if-not (halt? searchstate targetnode nd w)
'                                (let [sinkweights (for [sink (neighborf g nd searchstate)]
'                                      [sink (weightf g nd sink)])
'                       relaxation (fn [state [sink w]] (relax* nd sink w state))
'                       nextstate (reduce relaxation
'                                 (fr/pop-fringe searchstate)
'                                 sinkweights)]
'                               (recur g targetnode nextstate))
'                           searchstate))
'              searchstate))]
'        (walker g targetnode (fr/conj-fringe state startnode 0))))
'generic graph search, ala sedgewick.
Public Function genericSearch(g As IGraph, fringe As GenericFringe, ByRef startnode As String, Optional endnode As String, Optional revisit As Boolean) As GenericSearchData
Dim searchstats As GenericSearchData
Dim w As Single
Dim nd As String
Dim candidate As String
Dim itm
Dim halt As Boolean

Set searchstats = New GenericSearchData
searchstats.startnode = startnode
searchstats.targetNode = endnode
Set searchstats.fringe = fringe

fringe.addNode startnode, 0 'initialize the fringe
searchstats.distance.add startnode, 0
While fringe.count > 0 And halt = False
    nd = fringe.peekNode
    w = fringe.peekWeight
    fringe.popNode
    searchstats.completed.add nd
    If nd <> endnode Then
        If revisit = True Then
            For Each itm In getSinks(g, nd)
                candidate = CStr(itm)
                Set searchstats = relax(nd, candidate, getArc(g, nd, candidate), searchstats)
            Next itm
        ElseIf revisit = False Then
            For Each itm In getSinks(g, nd)
                candidate = CStr(itm)
                If Not (searchstats.distance.exists(candidate)) Then _
                    Set searchstats = relax(nd, candidate, getArc(g, nd, candidate), searchstats)
            Next itm
        End If
    Else
        halt = True
    End If
Wend
Set genericSearch = searchstats
Set searchstats = Nothing
End Function

Public Function cycleSearch(g As IGraph, fringe As GenericFringe, ByRef startnode As String) As GenericSearchData
Dim searchstats As GenericSearchData
Dim w As Single
Dim nd As String
Dim candidate As String
Dim itm
Dim halt As Boolean

Set searchstats = New GenericSearchData
searchstats.startnode = startnode
searchstats.targetNode = startnode
Set searchstats.fringe = fringe

fringe.addNode startnode, 0 'initialize the fringe
searchstats.distance.add startnode, 0
While fringe.count > 0 And halt = False
    nd = fringe.peekNode
    w = fringe.peekWeight
    fringe.popNode
    searchstats.completed.add nd
    'If nd <> startnode Then
        For Each itm In getSinks(g, nd)
            candidate = CStr(itm)
            If candidate = startnode Then
                searchstats.shortest.add startnode, nd
                halt = True
            ElseIf Not (searchstats.distance.exists(candidate)) Then
                Set searchstats = relax(nd, candidate, getArc(g, nd, candidate), searchstats)
            End If
        Next itm
    'Else
    '    halt = True
    'End If
Wend
If halt Then 'success
    nd = searchstats.shortest(startnode)
    w = searchstats.distance(nd)
    w = w + getArc(g, searchstats.shortest(startnode), startnode)
    searchstats.distance.item(startnode) = w
End If
        
Set cycleSearch = searchstats
Set searchstats = Nothing
End Function

'recover the first path from the search results.  Returns a list of nodes that form the path.
'Note, this will only enumerate one path, even if there are equal paths.  We can recover
'all paths using another function, which will provide a list of lists, each nested list
'corresponding to a path.  Note - another option is to just concat strings, since collections are
'using variants under the hood, and require some allocation.  I'm not sure of the performance
'penalties as of yet, but I think we'll be fine for the intended use case.  If we're churning out
'millions of long paths, there might be something to consider, but I think using lists is a'okay.
Public Function tracePath(searchstats As GenericSearchData, ByRef source As String, _
                            ByRef target As String, Optional enumerate As Boolean) As Collection
Dim curr As String
Dim pred As String
Dim predcoll As Collection
Dim cyc As Boolean
Set tracePath = New Collection
cyc = False
With searchstats
    If .distance.exists(target) Then 'we have a path
        If .distance.exists(source) Then
            cyc = .distance(source) <> 0
        End If
        
        If Not cyc Then
            curr = target 'we're looking for a normal path
        Else
            curr = .shortest(target)
        End If
        Set tracePath = prepend(tracePath, curr)
        While curr <> source
            If IsObject(.shortest(curr)) Then
                Set predcoll = .shortest(curr)
                pred = predcoll(1)
            Else
                pred = .shortest(curr)
            End If
            Set tracePath = prepend(tracePath, pred)
            curr = pred
        Wend
    End If
End With

End Function
Public Function pathTostring(path As Collection) As String
Dim itm
Dim i As Long

i = 1
pathTostring = path(1)
For i = 2 To path.count
    pathTostring = pathTostring & "->" & CStr(path(i))
Next i

End Function
Public Function pathString(searchstats As GenericSearchData, ByRef source As String, _
                            ByRef target As String, Optional enumerate As Boolean)
pathString = pathTostring(tracePath(searchstats, source, target, enumerate))
End Function
Public Function searchReport(stats As GenericSearchData, title As String, ByRef fromnode As String, ByRef tonode As String) As Dictionary
Set searchReport = newdict(title, newdict("Path", pathString(stats, fromnode, tonode), _
                                         "Length", stats.distance(tonode), _
                                         "Discovered", stats.discovered, _
                                         "Completed", stats.completed))
End Function
Public Function makeDepthFringe() As GenericFringe
Set makeDepthFringe = New GenericFringe
makeDepthFringe.setFringe depth
End Function

Public Function makeBreadthFringe() As GenericFringe
Set makeBreadthFringe = New GenericFringe
makeBreadthFringe.setFringe breadth
End Function

Public Function makePriorityFringe() As GenericFringe
Set makePriorityFringe = New GenericFringe
makePriorityFringe.setFringe priority
End Function

'13 Sep 2011

'Entry-point for iterative (stack-based) depth-first search.
Public Function DFS(g As IGraph, startnode As String, Optional endnode As String) As GenericSearchData
Set DFS = genericSearch(g, makeDepthFringe(), startnode, endnode, True)
End Function
'Execute a breadth-first search on graph g, from startnode.
Public Function BFS(g As IGraph, startnode As String, Optional endnode As String) As GenericSearchData
Set BFS = genericSearch(g, makeBreadthFringe(), startnode, endnode, True)
End Function
'Execute a priority first search.  This is an alias for Dijkstra's algorithm, and it uses a binary heap to
'keep performance effecient.
Public Function PFS(g As IGraph, startnode As String, Optional endnode As String) As GenericSearchData
Set PFS = genericSearch(g, makePriorityFringe(), startnode, endnode, True)
End Function
'Traverse graph g, starting at startnode, possibly ending early if endnode is provided.  The difference here is
'that the function call to genericsearch does NOT include a boolean flag to enable revisiting nodes, so the shortest
'path(s) will NOT be found, i.e. "this is not a real search", but a walk.  All reachable nodes will be
'visited in depth first order.
Public Function DepthWalk(g As IGraph, startnode As String, Optional endnode As String) As GenericSearchData
Set DepthWalk = genericSearch(g, makeDepthFringe(), startnode, endnode)
End Function
'Traverse graph g, starting at startnode, possibly ending early if endnode is provided.  The difference here is
'that the function call to genericsearch does NOT include a boolean flag to enable revisiting nodes, so the shortest
'path(s) will NOT be found, i.e. "this is not a real search", but a walk.  All reachable nodes will be
'visited in breadth first order.
Public Function BreadthWalk(g As IGraph, startnode As String, Optional endnode As String) As GenericSearchData
Set BreadthWalk = genericSearch(g, makeBreadthFringe(), startnode, endnode)
End Function
'Traverse graph g, starting at startnode, possibly ending early if endnode is provided.  The difference here is
'that the function call to genericsearch does NOT include a boolean flag to enable revisiting nodes, so the shortest
'path(s) will NOT be found, i.e. "this is not a real search", but a walk.  All reachable nodes will be
'visited in priority first order.
Public Function GreedyWalk(g As IGraph, startnode As String, Optional endnode As String) As GenericSearchData
Set GreedyWalk = genericSearch(g, makePriorityFringe(), startnode, endnode)
End Function
'Traverse graph g, starting at startnode, possibly ending early if endnode is provided.  The difference here is
'that the function call to genericsearch does NOT include a boolean flag to enable revisiting nodes, so the shortest
'path(s) will NOT be found, i.e. "this is not a real search", but a walk.  All reachable nodes will be
'visited in random order.
Public Function RandomWalk(g As IGraph, startnode As String, Optional endnode As String) As GenericSearchData

End Function
'obtain the topological ordering of the nodes (or a subset of them) in the graph.
'the first key/node in the topology is the first terminal node found...
'as such, least dependent nodes are first, the most dependent nodes are found last
Public Function topsort(g As IGraph, Optional nodeSet As Dictionary) As Dictionary
Dim nd
Dim node As String
Dim sink
Dim source
Dim sinkcount As Long
Dim roots As Dictionary
Dim remaining As Dictionary
Dim rootcount As Long
Dim ptr As Dictionary

Set roots = New Dictionary
Set remaining = New Dictionary
Set topsort = New Dictionary

If nodeSet Is Nothing Then
    Set nodeSet = getNodes(g)
End If

'initialize the nodes...
For Each nd In nodeSet
    node = CStr(nd)
    Set ptr = getSinks(g, node)
    sinkcount = ptr.count
    If sinkcount = 0 Then
        rootcount = rootcount + 1
        roots.add node, rootcount
    Else
        remaining.add node, sinkcount
    End If
Next nd

While roots.count > 0 'Note....this could end badly, we need to cover the case of cycles
    For Each nd In roots
        'pop the root off to our solution
        node = CStr(nd)
        topsort.add nd, roots(nd)
        'update the sinkcounts of the children....
        Set ptr = getSources(g, node)
        roots.Remove (nd) 'mark the root node as visited.
        For Each source In ptr
            If nodeSet.exists(source) Then 'Tom change 16 April! Forgot to exclude!
                remaining(source) = remaining(source) - 1 'decrement the running sinkcount
                If remaining(source) = 0 Then 'remaining node is now a root, append it.
                    rootcount = rootcount + 1
                    roots.add source, rootcount + 1
                    remaining.Remove (source) 'remove from remaining consideration.
                End If
            End If
        Next source 'all affected source nodes have been updated/decremented
    Next nd
Wend

If remaining.count > 0 Then 'we have a Cyclic graph, not a DAG, thus there is no topological order.
    Set topsort = New Dictionary 'empty dictionary indicates failure.
End If

Set remaining = Nothing
Set roots = Nothing

End Function

'Tarjan's algorithm for strongly connected components, ported from Wikipedia pseudocode.
Public Function scc(g As IGraph) As Dictionary
Dim index As Long
Dim s As GenericStack
Dim indices As Dictionary
Dim lowlinks As Dictionary
Dim active As Dictionary

Set scc = New Dictionary
Dim v
Set indices = New Dictionary
Set lowlinks = New Dictionary

For Each v In getNodes(g)
    If Not indices.exists(v) Then
        strongconnect g, index, s, CStr(v), indices, lowlinks, scc
    End If
Next v

End Function
'aux function for Tarjan's strongly connected components algo.
Private Sub strongconnect(g As IGraph, ByRef idx As Long, s As GenericStack, v As String, indices As Dictionary, links As Dictionary, sccs As Dictionary, Optional active As Dictionary)
Dim snk
Dim w As String
Dim ptr As Dictionary
If active Is Nothing Then Set active = New Dictionary

indices.add v, idx
links.add v, idx
idx = idx + 1
s.add v
active.add v, 1

For Each snk In getSinks(g, v)
    w = CStr(snk)
    If Not indices.exists(w) Then
        strongconnect g, idx, s, w, indices, links, sccs, active
        links.add v, minLong(links(v), links(w))
    ElseIf active.exists(w) Then
        links(v) = minLong(links(v), links(w))
    End If
Next snk

If links(v) = indices(v) Then
    Set ptr = New Dictionary
    sccs.add sccs.count + 1, ptr
    While v <> w
        w = s.peek
        s.pop
        ptr.add w, sccs.count
    Wend
End If
        
End Sub






