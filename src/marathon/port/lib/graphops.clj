(ns marathon.port.graphops)

Option Explicit

'Assumes graph is DAG, will probably break otherwise.
'The point of the shortest path is to find the shortest path, via precalculated edgeweights
'from a start node to its targetnode.
'dist (node) = min (dist (previous connecting node) + pl_node) for all connecting nodes.
'pl node = edgeweight between nodes.
'so, dist(nodeJ) = min(dist nodei + pl(i,j)) for all i within neighbors (j)
'return a dictionary of shortest paths....multiple paths will all be of equal length.

Public Function ShortestPathBellmanFord(g As IGraph, startnode As String, targetNode As String, _
                                          Optional enumerate As Boolean, Optional topology As Dictionary) As Dictionary
Dim Vfunc As Dictionary
Dim currnode As String
Dim destnode As String
Dim paths As Dictionary
Dim nextnodes As Dictionary
Dim nodetoi As Dictionary
Dim nd
Dim node As String
Dim iTonode As Dictionary
Dim js As Dictionary
Dim i As Long
Dim t As Long
Dim ptr As Dictionary

'plain english..
'vfunc(i) = min j (Cij + V(j)) <--implies we know V(j) for all j within i's neighbors.

Set Vfunc = New Dictionary 'value function, updated as needed
Set ptr = New Dictionary
Vfunc.add "BestFrom", ptr 'we want a place to record our best paths from each node.
Set ptr = New Dictionary
Vfunc.add "Paths", ptr
Set ptr = New Dictionary
Vfunc.add "Pathlengths", ptr
Vfunc.add "StartNode", startnode
Vfunc.add "TargetNode", targetNode
    
If hasNode(g, startnode) And hasNode(g, targetNode) Then 'make sure the nodes are in the graph.
    'Set ShortestPathDP = paths
    
    'we need to have a sorted graph before proceeding...
    'If Not sorted Then sort
    If topology Is Nothing Then
        Set topology = GraphLib.topsort(g, GraphLib.unionFind(g, startnode, fringefunctions.sinks)) 'note, we only want to examine nodes that are
        'reachable via startnode using outbound arcs, this is a -useful - optimization for a DAG structure i'm typically using.
    End If
    If topology.count = 0 Then Err.Raise 101, , "Graph is not acyclic!!!" 'must be a DAG to work.
    
    'what is the minimal set of topologically ordered nodes we have to check?
    'This minimal set of ordered nodes is, essentially, the stages of our decision process.
    'Stages in DP are represented as t, usually.
    'INVERTED the order of the args here. TOM 14 SEP 2011
    Set iTonode = dependencies(g, startnode, targetNode, topology) 'this trims the set of nodes further.
    Set nodetoi = nodemap(iTonode)
    'with a topologically sorted graph that reflects dependencies, we can traverse our topology, using
    'solutions to subproblems as components for dependent problems later in the graph.
    'This basically ensures that DP can always work....
    '***Vfunc.add 1, 0 'initial distance is 0
    currnode = iTonode(1)
    Vfunc.add currnode, 0 'initial distance is 0
    Set Vfunc = setbest(currnode, Vfunc, Nothing)
    'Note that, in the case of our shortest pairs algorithm, Stage t is = to node i.
    'Typically in DP, we break out subproblems by stages, t, States(i,j,etc.), and Value(t,state)
    'In our shortest path problem, the nodes are indexed using i and j, both are aliases of the set of all nodes.
    'When we did the topological sort of the graph, we essentially sorted the graph nodes into "decision stages".
    'The node at stage 1 is the end node, which also happens to be i = 1.  This is the node that has no dependents (sinks).
    'As decision stages increase (we traverse our topological ordering), the current state (i) happens to be equivalent
    'to the current stage, for bookkeeping purposes.  It may seem redundant, but in other DP applications, this is useful.
    'Thus, the value of node(i) is really V(i,t), which is really (V(i,i)) since i and t are equivalent.
    For t = 2 To iTonode.count 'for non-starting nodes...
        currnode = iTonode(t)
        'initialize distance...
        Set nextnodes = getSinks(g, currnode) 'get nodes that this one can flow to (J's)
        Set js = New Dictionary
        For Each nd In nextnodes
            If nodetoi.exists(nd) Then 'exclude anything not in the topologically necessary set!
                destnode = CStr(nd)
                If getArcLabel(g, (EncodeArc(currnode, destnode))) <> inf Then
                    js.add destnode, nextnodes(nd) 'calculate the ÿÿÿÿ
                End If
            End If
        Next nd
        If Not enumerate Then
            Set Vfunc = minV(currnode, js, Vfunc) 'updates our value function to incorporate value for i
        Else 'update using no-cost for feasible paths.  for enumerating all paths.
            Set Vfunc = uniformV(currnode, js, Vfunc)
        End If
    Next t
    ''TOM note -> finish here.  We should have a dictionary of value...now we need to unroll it to
    ''determine the shortest path(s)
    If Vfunc(startnode) <> inf Then
        Set paths = Vfunc("Paths")
        Set paths = tracefrom(g, iTonode(iTonode.count), iTonode(iTonode.count), paths, Vfunc)
    End If
End If

Set ShortestPathBellmanFord = Vfunc 'returns a pointer to the entire state of search information.

Set iTonode = Nothing
Set paths = Nothing
Set nextnodes = Nothing
Set nodetoi = Nothing

End Function


'I think I can replace this with a better function, using generic graph search.

'trace is the process of stepping through our DPShortestPath values we just calculated...
'we look for one or more shortest paths through the graph by starting at the last node we solved for.
'the last solved node is i.  values has a field called BestFrom, which is a dictionary of dictionaries.
'BestFrom keys off of i values, and points to the best path (nodes) to be taken if i is a source node.
'In this case, best = path choices which have values equal to values(i), which is the minimum path value
'for i.
'How do we record paths?  What is the best representation?
'One way is to simply keep a collection, which is the "current" path.
'We do a depth first traversal, appending nodes to the path until we run out of nodes.
'This works fine if there is only one shortest path, but what happens when there are many?
'A naive way is to implement the possibility of branching.
    'We could do this recursively....treating the currentpath as a folded object.
    'When we finish the current path, we add it to values?.
    'We then recurse back into the next path where we branched from.
        'As long as the depth isn't explosive, this works fine.
        'I'll give it a shot...if we start blowing the stack, I can alter the design to avoid recursion.
        'There really shouldn't be too many shortest paths to branch, I think.
        'Then again, it's totally dependent upon the data....so it's a corner case that could explode later.
'This is a recursive function that mutates the paths dictionary, but returns the mutated result (all the paths) as
'its ultimate result.
'Note....we are going to mutate values here.  this is terrible....
'The general idea is to fold over the paths dictionary with subsequent function calls.
'We're using mutable state in a functional manner, though, since we return the updated paths
Private Function tracefrom(g As IGraph, i As String, path As String, paths As Dictionary, values As Dictionary) As Dictionary
Dim nd
Dim best As Dictionary
Dim keys
Set best = getbest(i, values)

If best.count = 0 Then  'terminal node....
    If i = values("TargetNode") Then
        Set paths = addPath(g, paths, appendpath(path, i))
    End If
    Set tracefrom = paths
    Exit Function
Else
    While best.count = 1
        keys = best.keys
        i = keys(0)
        If best(i) <> inf Or best(i) <> -inf Then 'have a feasible path to follow.
            path = appendpath(path, i)
            Set best = getbest(i, values)
        Else 'infinite cost implies no path...
            Set tracefrom = paths
            Exit Function
        End If
    Wend
    If best.count = 0 Then
        If i = values("TargetNode") Then
            Set paths = addPath(g, paths, path)
        End If
        Set tracefrom = paths            'which are topologically sorted nodes....
        Exit Function
    Else
        For Each nd In best
            Set tracefrom = tracefrom(g, CStr(nd), appendpath(path, CStr(nd)), paths, values)
        Next nd
    End If
End If

Set best = Nothing

'it 's broken right now.
Set tracefrom = paths

End Function
Private Function appendpath(path As String, node As String) As String
appendpath = path & "->" & node
End Function

Private Function addPath(g As IGraph, paths As Dictionary, path As String) As Dictionary
Set addPath = paths
paths.add path, length(g, path)
End Function
Public Function length(g As IGraph, path As String) As Single
Dim tmp
Dim node1 As String
Dim node2 As String
Dim arc As String
Dim i As Long

tmp = Split(path, "->")
node1 = tmp(LBound(tmp, 1))

For i = (LBound(tmp, 1) + 1) To UBound(tmp, 1)
    node2 = tmp(i)
    arc = EncodeArc(node1, node2)
    If Not hasArcLabel(g, arc) Then Err.Raise 101, , "bad length"
    length = length + getArcLabel(g, arc)
    node1 = node2
Next i

End Function
'TOM Note 29 Mar 2011-> may be vestigial
Public Function getPaths(DPresults As Dictionary) As Dictionary

If DPresults.exists("Paths") = False Then
    Err.Raise 101, , "Results dictionary should have a subdictionary called 'Paths'"
Else
    Set getPaths = DPresults("Paths")
End If

End Function

Private Function firstnode(path As String) As String
firstnode = Mid(path, 1, InStr(1, path, "_") - 1)
End Function

'V(node i) = for each node j from node i, where i < j, Min(V(j) + C(i,j))
'Note, I am destructively updating the js dictionary, since it is being initialized again
'in the next iteration.
Private Function minV(i As String, js As Dictionary, values As Dictionary) As Dictionary
Dim res As Single
Dim cost As Single
Dim j

For Each j In js 'find the minimum path leading to the current node...
'value of a path

    cost = js(j) 'get edgeweight...Cost(source,destination) = Cij, encoded in the dictionary kvp
    res = cost + values(j)   '<---assumes we already solved j (j is to the "right").
    js(j) = res 'update the weight to reflect the total cost of the choice
    If values.exists(i) Then
        If res < values(i) Then
            values(i) = res
        End If
    Else
        values.add i, res
    End If
Next j

For Each j In js
    If js(j) > values(i) Then
        js.Remove (j) 'get rid of any js that are not possible paths (= to min value, values(i))
    End If
Next j
        
Set minV = setbest(i, values, js) 'record the best node(s) for paths from this location....

End Function
Private Function uniformV(i As String, js As Dictionary, values As Dictionary) As Dictionary
Dim res As Single
Dim cost As Single
Dim j

For Each j In js 'find the minimum path leading to the current node...
'value of a path
    cost = 0 'js(j) 'get edgeweight...Cost(source,destination) = Cij, encoded in the dictionary kvp
    res = cost + values(j)   '<---assumes we already solved j (j is to the "right").
    
    js(j) = res 'update the weight to reflect the total cost of the choice
    If values.exists(i) Then
        If res < values(i) Then
            values(i) = res
        End If
    Else
        values.add i, res
    End If
Next j

For Each j In js
    If js(j) > values(i) Then
        js.Remove (j) 'get rid of any js that are not possible paths (= to min value, values(i))
    End If
Next j
        
Set uniformV = setbest(i, values, js) 'record the best node(s) for paths from this location....

End Function

'this function will aid us in recovering our optimal paths.
Private Function setbest(i As String, values As Dictionary, Optional js As Dictionary) As Dictionary
Dim j
Dim best As Dictionary
Dim ptr As Dictionary
'Set ptr = New Dictionary
Set best = values("BestFrom")
best.add i, New Dictionary   'add a new entry to the bestfrom dictionary
Set ptr = best(i)

If Not (js Is Nothing) Then
    For Each j In js
        ptr.add j, js(j) 'record the best path and the pathlength.
    Next j
End If
Set setbest = values

Set ptr = Nothing
Set best = Nothing

End Function
Private Function getbest(i As String, values As Dictionary) As Dictionary
Dim res As Dictionary
Set res = values("BestFrom")
If res.exists(i) Then
    Set getbest = res.item(i)
Else
    Set getbest = New Dictionary
End If

Set res = Nothing

End Function
'Tom Change 16 April 2012
'   NOTE -> SSPDjikstra is deprecated.  Really, it's implemented in PFS (priority first search), which
'           is a form of generic graph search (which is all Dijkstra is!)
'TOM change 14 Sep 2011
'Find the single-source shortest path (if one exists) using Djikstra's algorithm.  If no
'endnode is supplied, the algorithm will return a full shortest-path-tree, and will not terminate
'early.  Djikstra rests on the critical assumption that edgeweights are positive, and that as a
'consequence, when we consider new vertices, we will either not change the existing SPT, or we will
'find new, optimally short paths to unvisited nodes.  There can be no negative edgeweights, and
'also no negative cycles.
Public Function SSPDjikstra(g As IGraph, ByRef startnode As String, Optional ByRef endnode As String) As Dictionary

Dim fringe As Heap_SngStr 'priority queue
Dim dist As Dictionary 'visited vertices
Dim SPT As Dictionary 'shortest path tree, similar to pred in DFS
Dim nodetoi As Dictionary
Dim snk
Dim nextNode As String
Dim nextd As Single
Dim wt As Single
Dim u As String, v As String 'node vars
Dim newlength As Single

If hasNode(g, startnode) And ((endnode = vbNullString) Xor (hasNode(g, endnode))) Then
    'initialize the search....
    Set fringe = New Heap_SngStr 'current search fringe.  we keep our candidate nodes, in min distance
                                 'order here.
    fringe.minheap
    
    Set dist = New Dictionary 'shortest known distance of all candidate nodes to startnode
                              'if candidate node does not exist in dist, distance is assumed infinite.
    Set SPT = New Dictionary 'shortest path tree.
    fringe.add 0, startnode
    dist.add startnode, 0
    SPT.add startnode, SPT.count
    
    While fringe.count > 0
        nextd = fringe.maxkey 'get next shortest distance
        u = fringe.Remove 'and the corresponding node, labelled u
        If u = endnode And endnode <> vbNullString Then 'we already discovered the shortest path to a valid destination node.
            fringe.Clear
        Else
            For Each snk In getSinks(g, u) 'for each neighbor v of u

                v = CStr(snk)
                wt = getArc(g, u, v)  'distance from u to v
                If wt < 0 Then Err.Raise 101, , "Negative Weights are infeasible in Djikstra's Algorithm"
                newlength = wt + dist(u) 'total distance from u to v = shortest known distance from startnode to v

                If Not dist.exists(v) Then 'found a new node to add to spt
                    dist.add v, newlength 'record distance
                    SPT.add v, u 'record spt relation, shortest route to v came from u
                    fringe.add newlength, v 'add v to the fringe, with its current known distance.
                ElseIf newlength < dist(v) Then 'iff v alreadt existed, and we found a new shorter route to v through u
                    fringe.TryUpdate dist(v), newlength, v  'update the fringe to note the now shorter path.
                    dist(v) = newlength 'update our minium length to get to v
                    SPT(v) = u 'update relation indicating spt leads from v to u
                End If
                
            Next snk
        End If
    Wend
End If

Set SSPDjikstra = New Dictionary 'return a dictionary of search results....
With SSPDjikstra
    .add "SPT", SPT
    .add "Distance", dist 'can get distance to any node from this....
    If endnode <> vbNullString Then _
        .add "Path", derivepath(startnode, endnode, SPT) 'derive a string path from start to end.
        .add "PathLength", dist(endnode)
End With

End Function
'Convert a dependency graph or a shortest path tree into a path.
Public Function derivepath(ByRef startnode As String, ByRef endnode As String, SPT As Dictionary) As String
Dim path As String
Dim currnode As String, nextNode As String
currnode = endnode
path = currnode
While currnode <> startnode
    nextNode = SPT(currnode)
    path = nextNode & "->" & path
    currnode = nextNode
Wend

derivepath = path
End Function

'TOM Change 13 Sep 2011 -> modified to use DFS to dynamically derive smallest subset of the dependent topology
'via reachability queries.  This should cut down hugely on the traversals we're doing....before we were pulling in
'a ton of extra vertices every time a path was calculated, drastically slowing down the search as V grew.
Public Function dependencies(g As IGraph, ByRef startnode As String, ByRef destnode As String, topology As Dictionary) As Dictionary
Dim i As Long
Dim nd
Dim node As String
Dim record As Boolean

Set dependencies = New Dictionary

If destnode = vbNullString Then destnode = topology.keys(topology.count - 1) 'set last node as destination.

'Tom note -> this is a source of ineffeciency.  We top-sorted "all" the nodes, rather than the nodes we
'cared about...maybe another way to determine this is to dynamically top-sort the nodes...
'as we progress through the sets, we assign i's, so that we get a sequence of nodes.
'Really, the sequence gets popped onto the fringe.
'nodes we have all the dependencies for, we can calculate.  Nodes with dependencies that are missing,
'we must calculate later.

For Each nd In topology 'topsort(GraphLib.unionFind(g, startnode, sinks))
    node = CStr(nd)
    If node = startnode Then
        i = i + 1
        dependencies.add i, node
        Exit Function
    ElseIf node = destnode Then
        record = True
        i = i + 1
        dependencies.add i, node
    ElseIf record Then
        i = i + 1
        dependencies.add i, node
    End If
Next nd

End Function

''given a topology, return the list of nodes that must be visited to determine the shortest path
'Public Function dependencies(startnode As String, Optional destnode As String) As Dictionary
'Dim i As Long
'Dim nd
'Dim node As String
'Dim record As Boolean
'
'Set dependencies = New Dictionary
'If destnode = vbNullString Then destnode = topology.keys(topology.count - 1) 'set last node as destination.
'
''Tom note -> this is a source of ineffeciency.  We top-sorted "all" the nodes, rather than the nodes we
''cared about...maybe another way to determine this is to dynamically top-sort the nodes...
''as we progress through the sets, we assign i's, so that we get a sequence of nodes.
''Really, the sequence gets popped onto the fringe.
''nodes we have all the dependencies for, we can calculate.  Nodes with dependencies that are missing,
''we must calculate later.
'
'For Each nd In topology
'    node = CStr(nd)
'    If node = destnode Then
'        i = i + 1
'        dependencies.add i, node
'        Exit Function
'    ElseIf node = startnode Then
'        record = True
'        i = i + 1
'        dependencies.add i, node
'    ElseIf record Then
'        i = i + 1
'        dependencies.add i, node
'    End If
'Next nd
'
'End Function

Private Function nodemap(iset As Dictionary) As Dictionary
Dim id
Set nodemap = New Dictionary
For Each id In iset
    nodemap.add iset(id), CLng(id)
Next id
End Function

'calculating equivalence classes amounts to extracting an undirected graph from the current graph.
'We then walk the nodes to determine which other nodes this node can reach (and their nodes, etc.).
'The end result is a set of nodes that are "related".  These nodes form an equivalence class, and can
'be extracted from the existing graph.  We then start with a remaining node and find its equivalence
'classmates.  Each set of nodes found in an equivalence search is captured in the dictionary.
'Ideally, we want a graph with limited connectedness, and small number of nodes, to perform this on.
'Initial intent was to use this as a pre-processing tool.  Great for sorting out dependencies...
Public Function EquivalenceClasses(sourcegraph As IGraph) As Dictionary

Dim closedlist As Dictionary
Dim newlyclosed As Dictionary
Dim openlist As Dictionary
Dim nextNode As String
Dim cls As String
Dim nd

Dim res As Dictionary
Dim h As IGraph
'
'If isDirected(sourcegraph) Then
'    Set h = sourcegraph
'Else
Set h = getUnDirected(sourcegraph)
'End If

Set openlist = New Dictionary
Set EquivalenceClasses = New Dictionary

For Each nd In getNodes(h)
    openlist.add CStr(nd), 0
Next nd

While openlist.count > 0
    Set newlyclosed = GraphLib.unionFind(h, (popNode(openlist)))
    For Each nd In newlyclosed
        openlist.Remove CStr(nd)
    Next nd
    'keying off number of nodes allows us to rank them, find islands fast.
    cls = EquivalenceClasses.count + 1
    EquivalenceClasses.add cls & "_" & newlyclosed.count, newlyclosed
Wend
    
Set openlist = Nothing
Set res = Nothing
Set newlyclosed = Nothing

End Function
Private Function popNode(openlist As Dictionary) As String
Dim itm

For Each itm In openlist
    popNode = CStr(itm)
    Exit For
Next itm

End Function
'use equivalence classes to compose the graph into 1 or more subgraphs.
Public Function decompose(sourcegraph As IGraph) As Dictionary
Dim classes As Dictionary
Dim classname
Dim classkey As String
Dim Class As Dictionary
'Dim islands As Dictionary
Dim newgraph As GenericGraph
Dim nd

Set decompose = New Dictionary

Set classes = EquivalenceClasses(getUnDirected(sourcegraph))

For Each classname In classes
    Set Class = classes(classname)
    'create a new graph from the sourcegraph, using an inclusive filter based off the nodes
    Set newgraph = cloneFiltered(sourcegraph, Class, , Collect)
    classkey = "Class_" & decompose.count + 1
    newgraph.name = classkey
    decompose.add classkey, newgraph
    'If Class.count = 1 Then islands.add classkey, 1
Next classname

Set classes = Nothing
Set Class = Nothing

End Function
Public Function calculateSubGraphs(source As IGraph) As Dictionary

Set calculateSubGraphs = decompose(source)

End Function
Public Function FindCycle(g As IGraph, startnode As String, endnode As String) As Collection
Dim hascycle As Boolean
Dim path As Collection
'
''cycle can only exist if path exists.
'If hasArc(g, endnode, startNode) Then 'cycle may exist
'    Set FindCycle = SearchDepthFirst(startNode, endnode)
'    hascycle = FindCycle.item(1) 'cycle exists only if path exists.
'End If
'If hascycle Then
'    Set path = FindCycle.item(3) 'stored path value
'    path.add startNode  'add start to the end, to complete the cycle.
'    Set FindCycle = New Collection
'    FindCycle.add True
'    FindCycle.add pathlength(path)
'    FindCycle.add path
'Else 'no cycle
'    Set path = New Collection
'    path.add startNode
'    Set FindCycle = New Collection
'    FindCycle.add False
'    FindCycle.add 0
'    FindCycle.add path
'End If
Err.Raise 101, , "Currently disabled!"
Set path = Nothing
End Function

