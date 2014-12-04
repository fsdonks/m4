;;Fillgraph is a special case of the generic graph that enforces some structural constraints.
;;it is designed to provide a simple, consistent interface for building fillgraphs by adding supply,
;;demand, and substitutions.
(ns marathon.sim.fill.fillgraph
  (:require [marathon.sim.core :as core]
            [spork.cljgraph.core :as graph]
            [clojure.core.reducers :as r]))

;;1.  The structure of the graph should be similar to the following...
;;   Filled -> Some Supply -> Some Demand
;;   If any supply exists, it will automatically be related to the Filled node.  We derive in O(1) if
;;   any supply exists by checking for a fillnode.
;;
;;2.  We can add sinks (demands) or sources(supply), which should automatically generate all necessary
;;    linkages.
;;3.  Sources are ALWAYS Immediately prior to the Fill Node (per #1).
;;4.  Sinks are ALWAYS terminal nodes (no outbound arcs).
;;    a.  Actually, sinks CAN have outbound arcs....we change the definition of sinks to be all nodes
;;        that do not have "Fill" as an outbound arc.
;;        Sources are all nodes that DO have "Fill" as an outbound arc.
;;        This implies that sinks <> sources.
;;        Sources can ONLY be related by 1 or more sinks.
;;        Transitive sources (i.e. related sink nodes) can be related by a series of connected sinks.
;;        This allows us to choose "any" sink as a starting point on the shortest path calculation.
;;5.  Fill paths relate sources to sinks via partial rules, i.e. a network of substitutions or relations.
;;6.  Supply automatically generats fill rules based on SRC, always has fill as a source.
;;7.  Demand automatically generates fill rules based on SRC.
;;8.  Finding supply for demand is a single shortest path from a demand node to the Fill node.
;;9.  Fill Graph can be used to track dependencies between supply/demand.


;;#todo implement this guy if necessary....
;;(defmacro ignoring-filled ....)

(defn donation-label [recepient donor] [:sub recepient donor])
(defn eqlabel [sink source]  [:eq sink source])

;;Equivalencies are associated with the = operator.  They contain any number of zero-cost outbound edges to nodes that are
;;equivalent.
;; Ex. "[:eq SRC1 Source1]"  would indicate that the node "SRC1" has a zero-cost outbound arc to the Source node "Source1".

(defn add-equivalence [g sink source]
  (let [intermediate (eqlabel sink source)]
    (-> g 
        (graph/conj-arc sink intermediate 0)
        (graph/conj-arc intermediate source 0))))


(defn inherit-node [g parent child exclude]
  (assert (not (graph/has-node? g child)) (str "Trying to inherit to a node that already exists: " child))
  (->> (graph/sink-map g parent)
       (reduce-kv  (fn [acc snk w]
                     (if (exclude snk) acc
                         (graph/conj-arc acc child snk w))) g)))

;;This is similar to inheritNode, except we remove the connecting arcs from the original node...
;;We insert a new connecting arc from the sourcenode to the new node, who now has all of the
;;outbound arcs from the sourcenode. This is like...inserting...into a n-ary tree.
(defn subvert-node [g parent child newcost]
  (assert (not (graph/has-node? g child)) (str "Trying to inherit to a node that already exists: " child))
  (let [newg (inherit-node g parent child)]
    (reduce-kv (fn [acc snk w]
                 (-> acc 
                     (graph/disj-arc parent snk)
                     (graph/conj-arc child snk))) 
               (graph/conj-arc  newg parent child)
               (graph/sink-map newg parent))))

;;Substitutions are associated with the :sub operator.  Substitutions contain positive-cost edges that relate a
;;Donor node to a Recipient node that can receive substitutions via an intermediate donation node
;;labeled "[:sub [Recepient] [Donor]]".  The recipient contains a single outbound arc of cost [cost]
;;to the donation node.  The donation node contains outbound edges to all of the Donor's connected outbound nodes,
;;with costs identical to donor's outbound edge costs.
;;  Ex.  "|> (SRC2, SRC1, 2) " indicates that the node "SRC2" has a outbound edge of cost 2 to a
;;  donation node "[:sub SRC2 SRC1]".  The donation node has outbound edges with destination nodes and costs
;;  identical to node "SRC1"'s outbound edges.
;;Tom NOTE 21 Mar 2011 -> this is similar to a subversion....but we don't eliminate the arcs from
;;the donor node.
(defn add-substitution 
  ([g recepient donor cost] 
     (let [nodename (donation-label recepient donor)
           complement (donation-label donor recepient)]
       (-> g
           (inherit-node donor nodename #{complement}) ;inherit the outbound edges of the donor.
           ;now connect the recepient to the new node, nodename
           (graph/conj-arc recepient nodename cost)))
           ;net result is a new path for the recepient through the new substitution node, which contains outbound
           ;edges inherited from the donor.
     )
  ([g recepient donor] (add-substitution 0)))

;;Create a dependency between the sink and the source
;;Note, we can use partial dependencies to create transitive dependencies (paths) between a sink and a
;;source.  The default relation is a simple sink -> source hookup.  If relationtype is not entered, then
;;this will happen.  Optional relations are equivalences and
;;substitutions, #{:equivalence :substitution}.
;--------------------------------------------------------------------------------
(defn relate [g sink source {:keys [weight relation] :or {weight 0}}]
  (if (nil? relation) 
    (graph/conj-arc g sink source weight)
    (case relation 
      :equivalence  (add-equivalence g sink source)
      :substitution (add-substitution g sink source weight)
      (throw (Exception. (str "unknown relation : " relation))))))

;;Node labeling conventions.  We used to use strings, may go back to
;;that if the compound keys end up being rough.
(defn source-label [nm] [:source nm])
(defn source-root  [nm] (second nm))

(defn sink-label [sink] [:fillrule sink])
(defn sink-root  [sink] (second sink))

(defn add-source [g source]  
  (let [nd (source-label source)]
    (-> g
        (relate  nd :filled nil)
        (graph/conj-arc source nd 0))))

(defn add-sink [g sink]
  (let [lbl (sink-label sink)]
    (if (graph/has-node? g lbl)
      g
      (-> g
          (graph/conj-arc  :unfilled  lbl 0)
          (graph/conj-arc  lbl sink 0)))))

(defn conj-arc-info [g arc-info]
  (case (first arc-info)
    :source (add-source g (second arc-info))
    :sink   (add-sink   g (second arc-info))
    :rel    (throw (Exception. (str "not implemented : parse-arc :rel")))))

(defn supplystore->arc-info [supplystore]
  (->> (:unitmap supplystore)
       (map (fn [[k u]] (:src u)))
       (distinct)
       (map (fn [src] 
              [:source src]))))

;;was fromsourcetable
(defn append-sourcetable [g tbl]
  (reduce (fn [acc {:keys [SRC]}]
            (add-source acc SRC))
          g 
          (r/filter :Enabled tbl)))

;;this is fromDemandrecords kinda
(defn append-sinktable [g tbl]
  (reduce (fn [acc {:keys [SRC]}]
              (add-sink acc SRC))
          g 
          (r/filter :Enabled tbl)))

;;was fromRelationTable 
(defn append-relationtable [g tbl]
  (reduce (fn [acc {:keys [Recepient Donor Cost Relation]}]
            (case (clojure.string/lower-case (clojure.string/trim  Relation))
              "sub"        (relate acc Recepient Donor {:weight Cost :relation :substitution})
              "equivalence" (relate acc Recepient Donor {:weight 0 :relation :equivalence})
              (throw (Exception. (str "unrecognized relation: " Relation)))))
          g (r/filter :Enabled tbl)))

(defn record->sink-arc   [{:keys [SRC] :as rec}] [(sink-label SRC) 0])
    

;;From a fully built fillgraph, we can leverage the graph topology to quickly decompose it into
;;n subgraphs of 1 or more nodes.  Subgraphs with only 1 node are islands, i.e. they have no logical
;;connections in the simulation.  They cannot be utilized(sources) or filled(sinks).


;;-------


;'Composes tables defining supply, demand, and relation records into a fillgraph
(defn tables->fillgraph 
  ([g supply demand relations]
     (-> g 
         (append-sourcetable supply)
         (append-relationtable relations)
         (append-sinktable demand)))
  ([supply demand relations] (tables->fillgraph graph/empty-graph supply demand relations))) 


;;Use SSP path finding to find all paths from each node in sourcenodes to filled.
;;convert these paths into weighted edges in an undirected graph.
;;Ultimately, when we prune this out, we don't necessarily need to know all the intermediate nodes..
;;unless we're allowing the fillgraph to be dynamically changed.  Assume we aren't for now.
;;We perform a reduction step on the "potentially" complex fillgraph.  Essentially, we calculate all
;;the shortest paths from sinks to filled.  For each of these paths, we extract the node prior to
;;filled, which by virtue of the fillgraph properties "must" be a sourcenode.  This forms a set
;;of sink, source pairs.  We want to build a directed graph from these pairs, but we want to retain
;;all information from the higher-order graph.  The reduced directed graph will then be a set of
;;valid sink nodes pointing to valid source nodes.  The complex path information from the
;;higher order graph, contained in the shortest path results, will be cataloged in a dictionary...
;;where each sink, source pair
;; 
;;TOM change 15 Sep 2011 -> The old way of doing this was ineffecient.  Now, we use Djikstra's algorithm to
;;calculate the shortest path tree from a fillrule to its supply source (we simply let Djikstra loose from the sink
;;node to "filled", and it'll tell us all the paths (plus distances) to supply nodes).
;;We then condense these down into a simple 1-arc-weighted path.
;;We also get reduction information from this...
;; 

(comment 

(defn get-reduced-fillgraph [g]
  (let []  ))

)


;;use ssp to derive the all pairs shortest path (although floyd
;;warshall is way better).
(defn naive-all-pairs [g fromnodes tonodes]
  (for [from fromnodes 
        to   tonodes]
    (let [res (graph/dijkstra g from to)]
      (when-let [d (get-in res [:distance to])]
        [(graph/first-path res) d]))))
  
(defn reduced-arcs [g] 
  (for [[path w] (filter identity 
                         (naive-all-pairs g (graph/sinks g :unfilled) 
                                          (graph/sources g :filled)))]
    [(last path)  (first path) w]))

;;If the fillgraph is a dag, which it should be by construction
;;(absent any funky data), then we can reduce it by finding the paths
;;between each source node and each sink node, and collapsing the
;;paths into a single arc between the source and sink with the weight
;;of the path.  Since we were careful to codify substitutions and
;;equivalences as specific extra nodes, and defined operations on the
;;fill graph that enforced structure changes (rather than just adding
;;arcs willy nilly), we can get away with this kind of reduction.  The
;;net result should be much faster pathfinding when we go to query the
;;fillgraph.
(defn reduced-graph [g]
  (let [cycs (graph/directed-cycles g)]
    (assert (empty? cycs) (str "Your dependency graph should be a DAG, cycles are present along " cycs))
    (let [sources   (atom (transient #{}))
          sinks     (atom (transient #{}))]
      (as-> (reduce (fn [acc [src snk w]]
                      (do (swap! sources conj! src)
                          (swap! sinks conj!   snk)
                          (graph/conj-arc acc snk src w)))
                    graph/empty-graph (reduced-arcs g)) 
            basegraph
            (as-> (reduce (fn [acc src]
                            (graph/conj-arc acc src :filled 0))
                          basegraph
                          (persistent! @sources))
                  sinkgraph
                  (reduce (fn [acc snk]
                            (graph/conj-arc acc :unfilled snk 0)) sinkgraph (persistent! @sinks)))))))

;;testing 
(comment 
(require '[marathon.sim.sampledata :as sd])
(require '[spork.cljgraph.jungapi :as jung])

(defn visualize [g & {:keys [layout] :or {layout jung/fr}}]  (jung/view-graph g jung/fr))
(def fg (tables->fillgraph (get sd/sample-tables :SupplyRecords)
                           (get sd/sample-tables :DemandRecords)
                           (get sd/sample-tables :RelationRecords)))

(def decomps (graph/decompose fg))


)

;----------------------------------------
;;#Deferred


;'Composes pre-built stores of supply, demand, and policy into a fillgraph.
;Public Function composeFillGraph(supplystore As TimeStep_ManagerOfSupply, 
;   demandstore As TimeStep_ManagerOfDemand, 
;      policystore As TimeStep_ManagerOfPolicy) As TimeStep_FillGraph
;Set composeFillGraph = BuildFillGraph(New TimeStep_FillGraph, supplystore, 
;                           demandstore, policystore)
;End Function

;; 'Methods to enable dynamic changes to the topology of the network.

;;  Public Sub enableSource(source As String)
;;  graph.EnableNode (sourceLabel(source))
;;  End Sub


;; 'Methods to enable dynamic changes to the topology of the network.

;--------------------------------------------------------------------------------

;;  Public Sub disableSource(source As String)
;;  graph.DisableNode (sourceLabel(source))
;;  End Sub
;; 'Methods to enable dynamic changes to the topology of the network.

;--------------------------------------------------------------------------------


;;  Public Sub enableRelation(sink As String, source As String)
;;  graph.EnableArc EncodeArc(sink, source)
;;  End Sub
;; 'Methods to enable dynamic changes to the topology of the network.

;--------------------------------------------------------------------------------

;;  Public Sub disableRelation(sink As String, source As String)
;;  graph.DisableArc EncodeArc(sink, source)
;;  End Sub
;; 'Utility method to generate default fill rules from supply en-masse
;; 'We do assume that all supply has been tagged upon processing, thus supply.tags has sources in it.
;; 'Our goal then, is to traverse the tags, looking for sources.
;; 'When we find new sources, we register them with the fillgraph.  This is just the naive sourcing.  We'll
;; 'assign buckets after we get substitutions in place.  Then we'll replace the original sources with
;; 'buckets.  We need to tag each uic with its bucket.....maybe using a supplytag.





;;#Maybe later....


;; 'Read in substitution rules and equivalencies from the policy manager.

;--------------------------------------------------------------------------------

;;  Public Function fromPolicy(policy As TimeStep_ManagerOfPolicy, _
;;                                  Optional source As TimeStep_FillGraph) As TimeStep_FillGraph
;;  Dim subs As Dictionary
;;  Dim equivs As Dictionary
;;  Dim rule
;;  Dim tmp
;;  Dim recepient As String, donor As String
;;  Dim delim As String
;; 
;; 
;;  If source Is Nothing Then Set source = Me
;;  Set fromPolicy = source
;; 
;;  delim = policy.ruleDelim
;; 
;;  Set subs = policy.getSubs
;;  Set equivs = policy.getEquivalencies
;; 
;;  For Each rule In subs
;;      tmp = Split(rule, delim)
;;      recepient = tmp(0)
;;      donor = tmp(1)
;;      relate recepient, donor, subs(rule), Substitution
;;  Next rule
;; 'TOM Note 27 MAr 2011 -> double check equivalencies, might need to exclude
;; 'some nodes, not sure...
;;  For Each rule In equivs
;;      tmp = Split(rule, delim)
;;      recepient = tmp(0)
;;      donor = tmp(1)
;;      relate recepient, donor, , Equivalence
;;  Next rule
;; 
;;  End Function



;; (defn append-policystore [g pstore]
;;   (reduce (fn [acc {:keys [Recepient Donor Cost Relation]}]
;;             (case (clojure.string/lower-case (clojure.string/trim  Relation))
;;               "sub"        (relate acc recepient donor cost :substitution)
;;               "equivalence" (relate acc recepient donor 0 :equivalence)
;;               (throw (Exception. (str "unrecgonized relation: " Relation)))))
;;           g (r/filter :Enabled tbl)))


;; 'Utility method to generate default fill rules from demand en-masse
;; 'Demand nodes (sinks) are the last to be processed.  They are input after supply and substitutions have a go.
;; 'Basically, supply and substitutions are calculated, then processed into buckets.  The buckets then become
;; 'sources.

;--------------------------------------------------------------------------------

;;  Public Function FromDemand(demandsource As TimeStep_ManagerOfDemand, _
;;                                  Optional source As TimeStep_FillGraph) As TimeStep_FillGraph
;;  Dim nm
;;  Dim src As String
;;  Dim demand As TimeStep_DemandData
;;  Dim demands As Dictionary
;;  Dim newsink As String
;; 
;;  If source Is Nothing Then Set source = Me
;;  Set FromDemand = source
;;   
;;  Set demands = demandsource.demandmap
;; 
;;  For Each nm In demands
;;      Set demand = demands(nm)
;;      src = demand.src
;;      newsink = sinklabel(src)
;;      If Not sinknodes.exists(newsink) Then
;;         'we generate source nodes from units automatically
;;         'attributes that guide source nodes -> SRC, others?  note, we could have uic-specific fill rules...nice.
;;          addSink src'for now, only overtly reading in srcs.
;;          relate newsink, src
;;      End If
;;  Next nm
;; 
;;  End Function



;;#Tom Note -> I think we can do this much much easier using cljgraph.
;--------------------------------------------------------------------------------

;;  Private Function GetReducedGraph(Optional keepislands As Boolean, Optional sourcegraph As GenericGraph) As GenericGraph
;; 
;;  Dim source As String
;;  Dim snk
;;  Dim src
;;  Dim sink As String
;;  Dim pth
;;  Dim path As String
;;  Dim SPT As Dictionary
;;  Dim dist As Dictionary
;; 
;;  Dim sources As Dictionary
;;  Dim sinks As Dictionary
;; 
;;  Dim paths As Dictionary
;; 
;;  If sourcegraph Is Nothing Then Set sourcegraph = graph
;; 
;;  Set GetReducedGraph = reducedgraph
;; 
;;  Set sources = New Dictionary
;;  Set sinks = New Dictionary
;; 
;;  For Each src In sourcenodes
;;      sources.add CStr(src), 0
;;  Next src
;; 
;;  For Each snk In sinknodes
;;      sinks.add CStr(snk), snk
;;  Next snk
;; 
;;  With sourcegraph
;;     'use djikstra to find a path from snk to source....
;; '    For Each snk In sinknodes
;; '        sink = CStr(snk)
;; '        Set paths = .pathAny(sink, "Filled")
;; '        Set paths = paths("Paths")
;; '        If paths.count > 0 Then 'there exists a path from this sinknode to a sourcenode to filled.
;; '            sinks.remove sink
;; '            For Each pth In paths
;; '                path = CStr(pth)
;; '                source = .secondtolast(path) 'get the actual sourcenode used in the path.
;; '                setReducedPath sink, source, path, paths(pth)
;; '                If sources.exists(source) Then sources.remove source 'get rid of the source node
;; '            Next pth
;; '        End If
;; '    Next snk
;;      For Each snk In sinknodes
;;          sink = CStr(snk)
;;          Set paths = SSPDjikstra(sourcegraph, sink)
;;          Set dist = paths("Distance")
;;          Set SPT = paths("SPT")
;;          If SPT.exists("Filled") Then'used the sink
;;              For Each src In SPT
;;                  If InStr(1, src, "SOURCE", vbBinaryCompare) > 0 Then
;;                      reducedgraph.addArc sink, CStr(src), dist(src)
;;                      If sources.exists(CStr(src)) Then sources.Remove CStr(src)
;;                  End If
;;              Next src
;;              sinks.Remove snk
;;          End If
;;      Next snk
;;      
;;      If keepislands Then
;;         'add ununsed sinks and sources to the graph as nodes.  they will show up as islands in the graph.
;;          For Each snk In sinks
;;              GetReducedGraph.addNode CStr(snk)
;;          Next snk
;;          For Each src In sources
;;              GetReducedGraph.addNode CStr(src)
;;          Next src
;;      End If
;;  End With
;; 
;;  Set GetReducedGraph.subgraphs = calculateSubGraphs(GetReducedGraph)
;;  End Function
;; 'retrieve the full, complex path calculated from the higher-order graph.

;--------------------------------------------------------------------------------

;;  Public Function getFullPath(sink As String, source As String) As String
;;  Dim arc As String
;;  arc = EncodeArc(sink, source)
;;  If reducedpaths.exists(arc) Then
;;      getFullPath = reducedpaths(arc)
;;  Else
;;      Err.Raise 101, , "Path does not exist, or was lost during reduction"
;;  End If
;; 
;;  End Function
;; 'procedure to store the reduced path efficiently.

;--------------------------------------------------------------------------------

;;  Private Sub setReducedPath(sink As String, source As String, path As String, _
;;                                  Optional weight As Single, Optional targetgraph As GenericGraph)
;;  Dim arc As String
;;  If targetgraph Is Nothing Then Set targetgraph = reducedgraph
;; 
;;  arc = EncodeArc(sink, source)
;;  Call addArc(targetgraph, sink, source, weight) 'store the reduction in our graph
;;  If Not (reducedpaths.exists(arc)) Then
;;      reducedpaths.add arc, path'store the reduction in our dictionary
;;  Else
;;      Err.Raise 101, , "Arc already reduced"
;;  End If
;; 
;;  End Sub



;------------------------------------------------------------------------------
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
