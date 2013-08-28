(ns marathon.port.data.supplygenerator
  (use [spork.util.record :only [defrecord+ with-record]]))
(declare spawn-ghost)

;An implementation of a supply generator.  Note the relative few public methods 
;here.
;This class is designed to abstract away a few mechanisms, particularly in the 
;fill phase.
;    When the fill function queries its rules for a feasible supply, it'll 
;    eventually settle on one or more buckets (a list of deployable units of 
;    supply).  It will then draw supply from these buckets, in order, until 
;    there is no more feasible supply or the requesting demand has been filled.
;    When a bucket is selected, we still have some work to do.  Namely, we 
;    "may" have to sort said units inside the bucket in some priority order.
;    In fact, this becomes very important, since there are a variety of 
;    prioritization schemes that developers could implement or request.  
;    This is a very dynamic area open to interpretation.  The original 
;    fillfunction just encapsulated all of this and provided a single 
;    suitability function (a way to priotize supply), which just sorted the
;    supply in the bucket in ascending order by cycletime.
;        
;    The fill function really doesn't care about any of this sorting 
;    nonsense.  All it does is execute the effecient selection of a source 
;    of supply relative to a ruleset.  Then it draws from that supply until
;    empty.  Then selects the next best source, etc.  until all sources have 
;    been consumed.
;        
;    So, the business of implementing the actual ordering of supply within a 
;    bucket is delegated entirely to this class.  It allows us to generate other 
;    classes (or just implement different methods) that provide entirely 
;    different functionality in the form of sorting mechanisms.  Or, more 
;    importantly, entire different functionality in the way UNITS are generated.
;    In the end, this class is just a generating function that the fillfunction 
;    is calling.  I would like the fill function to not care about whether a 
;    ghost is generated, or a real unit is.  We get to handle the ghost vs. 
;    real, or the specific characteristics of a unit that make it more suitable
;    than another, etc.  at a lower level of detail (this class).  Fill 
;    functions just pull on an appropriate supply generator to get the next 
;    supply.

(defprotocol ISupplyGenerator
  (load-supply [g frombucket supplyset & [followoncode demandname phase]])
  (capacity [g]) ;analagous to count
  (next-entity [g])
  (take-next [g]))

(defn empty-generator? [g] (zero? (capacity g)))
(def  supply-remaining? empty-generator?) ;alias

(defrecord+ generator [currentbucket 
                       currentsupply 
                       comparer
                       mode
                       scheme ;MAY BE OBSOLETE
                       supplyheap ;MAY BE OBSOLETE                       
                       ghostproportions ;MAY BE OBSOLETE 
                       ghostcompo ;MAY BE OBSOLETE
                       tags ;MAY BE OBSOLETE
                       ])  

;in re-thinking this guy, what we really want is a chunk of data that serves 
;as a query interface for a segment of supply.

;When we go to "fill" a demand, what we're asking is "given the specifics of 
;this fill context, i.e. the demand, the supply, and any other special info, 
;what is total ordering of all supply that can successfully fill the demand?"

;In the VBA version, we threaded the querying-of and generation-of units
;together, so that, for instance, if we could spawn new entities for fill a 
;demand, the very act of drawing from a supply generator would - using side 
;effects - automatically enforce the necessary beuracratic changes (like 
;spawning a ghost, registering it with the supply, to make it "exist" just in 
;time).  This is categorically different from the process for querying existing
;supply, which effectively boils down to providing a lazy sequence of units 
;in sorted order of decreasing suitability.

;Given the pure functional context, what we SHOULD do is refactor the design so
;that it's trivial to define generating functions that.  The difference is that 
;we should NOT weave in the side effects from before, and that there SHOULD be 
;a concrete data type (or supplementary data) that communicates the result of 
;the query. From here, we can make the filling function explicitly handle the 
;query result, which puts the burden on the consumer of the query result to 
;enact the apprioriate updates.  For instance, we could have two simple types 
;of results -> existing-entity | new-entity. We define functions that 
;consume these types, the simplest being for existing entities (most of the 
;infrastructure is already there), and new-entities being instructions for 
;creating a new entity.  So the returns are really simple tokens...

;Public Sub init(state As TimeStep_SimState, Optional gpolicy As TimeStep_Policy,
;                   Optional gcompo As String, Optional fergusonmode As Boolean)
;TODO -> this is just a temporary decoupling.  I need to extract out the supply
;generator further.  Parent is just pointing at a supplystore now (now unlike a 
;partially applied function).  Still, the dependencies kinda suck.

;Decoupled
;Set parent = supply
;Set simstate = state

;Decoupled
;If gpolicy Is Nothing Then Set gpolicy = simstate.policystore.policies("Ghost365_45")
;If gcompo = vbNullString Then gcompo = "Ghost" 'no component
;
;Set ghostpolicy = gpolicy
;ghostcompo = gcompo
;Decoupled
;Set factory = supply.parent.entityfactory
;Set tags = simstate.supplystore.tags
;Set comparer = New TimeStep_ComparerUnit
;If fergusonmode Then comparer.RCpresurgePreference = True
;End Sub
(defn init [state & [gpolicy gcompo fergusonmode]]
  )

;This is where we implement the various sorting schemes and options for the 
;supply. We generally sort on cycle time, but there can be various schemes.
;For ghosts, we don't care.
;Basic functionality is to load a supply from a bucket (a set of units).
;
;We add followonCode to determine if supply for this fill may utilize units that
;are tagged as follow-ons for a class of Vignette.
;
;If follow-ons exist, they are immediately prioritized over all others.
;  Basically, we increase their value an order of magnitude over all the others.
;  Conversely, if a follow-on exists, but of the wrong follow-on, we exclude it 
;  from prioritization....it cannot be used outside of its vignette class...

;Public Sub LoadSupply(frombucket As String, supplyset As Dictionary, _
;      Optional followoncode As String, Optional demandname As String, 
;         Optional phase As FillPhase)

;currentbucket = frombucket
;Set CurrentSupply = supplyset
;
;If Not isGhostBucket(frombucket) Then
;        LoadLocalSupply supplyset, realSupply, followoncode, demandname
;Else
;    'If phase = useEveryone Then
;        LoadLocalSupply supplyset, GhostSupply, followoncode, demandname
;    'TOM Change 16 April, leftover supply heap was mucking with ghosts.  Now we don't look!
;    'ElseIf CurrentSupply.count = 0 Then
;    '    SupplyHeap.Clear
;   ' End If
;End If

;End Sub

;TODO -> turn this into a multimethod...
(defn load-supply [g from-bucket supply-set & [followon-code demandname phase]]
 )

(defn load-local-supply [g & [supply-mode followon-code demandname]]
)

;TODO-> Should we memoize this? 
(defn ghost-bucket? [x] (re-find #"Ghost" x))

;TODO -> explore the consequences herein....maybe we want to return generated 
;supply explicitly, and, in order to actually fill a demand with a generated 
;supply, we enact the spawning, registering, etc. In that case, a generator 
;could be seen as a function that returns a sequence of tokens that indiciate 
;either entities that exist, or actions to take to create entities.
;Spawn a new ghost in response to filling a demand.
(defn new-ghost [gen compo policy ctx]
  (let [state (:state ctx)
        {:keys [supplystore policystore parameters behaviormanager]} state]
    (spawn-ghost (:currentbucket gen) :ghost compo policy behaviormanager 
       parameters policystore supplystore ctx)))

;Get next pulls from all available supply, across multiple paths, as needed.
;Note -> the shortest path may generate multiple paths.  When we traverse, we 
;find this out and remember the other paths.  Since they're all equal in one
;dimension....This MAY cause a problem when we're filling based on buckets....
;Ideally, we want to partition our search space into very small segments so that
;sorting is relatively quick, easy, and efficient.  Yet, we lose visibility on
;"other" potentially better paths by imposing walls via the buckets of supply.  
;On one end of the spectrum, we get have one LARGE bucket, but a sophisticated
;network of paths from which to fill using our shortest path algorithms.  On the
;other hand, we have many smaller buckets, which partition our search space and
;allow for a heuristic approach to filling.
(defn get-next [g phase] (first (:supplyheap g)))

;----------------DEFERRED
;TODO
;Convert this into another type of generator, rather than implicitly switching
;to ghost supply after supply runs out...
;Public Function SupplyRemaining(phase As FillPhase) As Boolean
;SupplyRemaining = count(phase) > 0
;If SupplyRemaining = False Then
;    'TOM Change 17 April 2012
;    If phase = useEveryone Then mode = GhostSupply
;End If
;
;End Function


;Private Sub Class_Initialize()
;mode = realSupply
;scheme = cycletime
;Intention is to have a proportional distribution of supply across components.  
;Maybe I'll do this later.
;Set GhostProportions = New Dictionary
;With GhostProportions
;    .add "AC", 0.5
;    .add "RC", 0.5
;End With
;
;End Sub
;

;an enumeration of the types of generation we can use
;Public Enum GeneratorMode
;    realSupply
;    GhostSupply
;End Enum


;Public Enum PriorityScheme
;    cycletime
;    variable
;End Enum

;Public Function count(Optional phase As FillPhase) As Long
;If mode = realSupply Or phase = FillPhase.onlyUseFollowons Then
;    count = SupplyHeap.count
;Else
;    count = infLong 'infinite ghost generation capability....unless we want to 
;                    'curtail this in the future.
;End If
;
;End Function
;
;Public Function takeNext(phase As FillPhase) As TimeStep_UnitData
;
;Select Case mode
;    Case Is = realSupply
;        Set takeNext = getNext(phase)
;    Case Is = GhostSupply
;        If SupplyHeap.count > 0 Then
;            Set takeNext = getNext(phase)
;        Else
;            Set takeNext = newGhost(ghostcompo, ghostpolicy)
;        End If
;End Select
;
;End Function

;Private Sub LoadLocalSupply(supplyset As Dictionary, Optional supplymode As GeneratorMode, _
;                                Optional followoncode As String, Optional demandname As String)
;Dim name
;Dim uic As TimeStep_UnitData
;
;mode = supplymode
;
;TOM Change 17 April 2012!
;If SupplyHeap Is Nothing Then
;    Set SupplyHeap = New Heap_LngStr
;Else
;    SupplyHeap.Clear
;End If
;
;If supplyset.count > 0 Then
;
;TODO replace with generic sort?  Might require less allocations, end up faster....
;SupplyHeap.maxheap
;    'Decouple
;    With parent
;        For Each name In CurrentSupply
;            Set uic = .unitmap(CStr(name))
;            'TOM Change 17 April 2012!
;            SupplyHeap.add comparer.getSortKey(uic, demandname, followoncode, simstate.policystore.activePeriod.name), uic.name
;        Next name
;    End With
;End If
;
;End Sub

