;;Requirements Analysis implementation.
(ns marathon.analysis.requirements
  (:require [spork.util [record :as r]]))
;;Requirements analysis is the process of calculating some required additional supply in the face of a given
;;demand signal.  Technically, requirements analysis is indifferent to the existence of supply.  We can
;;calculate a requirement regardless of pre-existing supply.  In fact, we can grow a requirement.
;;Traditional requirements analysis consisted of a fixed-point function:
    ;;RequiredSupply(SupplyInitial) = SupplyInitial + Generated(SupplyInitial, Demand)
            ;;Do until SupplyNext = SupplyInitial
                ;;SupplyInitial <- SupplyNext
                ;;SupplyNext <- Distribute(RequiredSupply(SupplyInitial), SupplyInitial)
                
;;We will use DemandAnalysis and Supply Analysis to help the convergence go much faster.
;;As a pre-process step, we utilize Static Demand Analysis to determine the peak demands and the
;;maximum deployment acceleration experienced during the run.
    ;;This provides definite constraints on our minimum required supply.
        ;;ANY supply must have a theoretical capacity >= the peak demand.

;;We then utilize Static Supply Analysis to determine the theoretical rotational capacity for the initial
;;supply.  (If we have no supply, capacity is nil)
;;Prior to starting the convergence algorithm, we distribute this intial gap in supply.

;;Then run until convergence.
    ;;Should take less time.



;;Note -> Requirements analysis does not utilize substitutions.  This is a pure 1:1 demand to supply.
;;This makes the problem essentially much easier (and faster).
;;It also makes calculating requirements even faster and more effecient, because we can parallelize the
;;entire analysis into a set of n independent simulations, which only considers the supply and demand of a
;;single type.

;;Given a set of demand data, the theoretical proportion of supply by component, and policy schedules,
;;calculate the supply required by SRC.

;;Requirements analysis is the process of calculating some required additional supply in the face of a given
;;demand signal.  Technically, requirements analysis is indifferent to the existence of supply.  We can
;;calculate a requirement regardless of pre-existing supply.  In fact, we can grow a requirement.
;;Traditional requirements analysis consisted of a fixed-point function:
    ;;RequiredSupply(SupplyInitial) = SupplyInitial + Generated(SupplyInitial, Demand)
            ;;Do until SupplyNext = SupplyInitial
                ;;SupplyInitial <- SupplyNext
                ;;SupplyNext <- Distribute(RequiredSupply(SupplyInitial), SupplyInitial)
                
;;We will use DemandAnalysis and Supply Analysis to help the convergence go much faster.
;;As a pre-process step, we utilize Static Demand Analysis to determine the peak demands and the
;;maximum deployment acceleration experienced during the run.
    ;;This provides definite constraints on our minimum required supply.
        ;;ANY supply must have a theoretical capacity >= the peak demand.

;;We then utilize Static Supply Analysis to determine the theoretical rotational capacity for the initial
;;supply.  (If we have no supply, capacity is nil)
;;Prior to starting the convergence algorithm, we distribute this intial gap in supply.

;;Then run until convergence.
    ;;Should take less time.
    
;;Note -> Requirements analysis does not utilize substitutions.  This is a pure 1:1 demand to supply.
;;This makes the problem essentially much easier (and faster).
;;It also makes calculating requirements even faster and more effecient, because we can parallelize the
;;entire analysis into a set of n independent simulations, which only considers the supply and demand of a
;;single type.

;;Given a set of demand data, the theoretical proportion of supply by component, and policy schedules,
;;calculate the supply required by SRC.

;;loosely ported from vba....
;;What we'll do instead of having file i/o driving this, is we'll
;;derive any necessary i/o from the sequence we compute as we converge.
;;Basically, using reductions, we'll derive the supply table each iteration.

;;There's a whole mess here about distributors too...
;;I think we'll just 
(defn compute-requirements [ctx init-supply-table proportions]
  (let [supply-table init-supply-table
        distributions {}]
    ))

;;this isn't a huge deal; multiplying by ratios returns bigints...
(defn distribute-rationally [n xs] (mapv (fn [r] (* n r)) xs))
(defn sums-to-one?   [xs] (== 1.0 (double (reduce + xs))))



;;if we wanted to backsolve for requirements to grow accurately,
;;getting MFS, how would we do it?
;;naively....our previous history would form a set of constraints on
;;the position of the supply we're adding...
;;Tradeoff could be, adding supply is more expensive, but we're
;;accurate....

;;If we grow supply - in the context of the initial state of things -
;;we have a dependent "chain" of supply.  Each time we add supply, we
;;have to account for initial conditions....

;;What's the minimal information necessary to determine accurate initial
;;conditions?
;;We would have to create a policy trajectory where - for some reason -
;;the unit was not available to be utilized - ever - and was
;;available just-in-time to fill the missing demand....

;;"If" we could do this, then we regain consistency in our
;;RA, so that we effectively have some notion of an additive
;;history, or additive growth, that is, every time we
;;add supply, we are guaranteed to fill more demand than
;;in the previous step.

;;Another way to do this is to grow supply additively...
;;Kind of like Barry did...
;;We only EVER advance if we can assure that we have enough supply...
;;Rather than run the entire future...basically do an iterative
;;depth first search.

;;At any given step, we have a state:
;;{supply, t, tmax}
;;Our goal is to get to tmax, and return
;;the supply.
;;The canonical way to do this is to run
;;the scenario with an initial supply,
;;get a capacity analysis,
;;and use some representation of missed demand
;;(typically through ghost-units)
;;When you think about it, ghosts are just placeholders,
;;and they follow their own policy.
;;We "can" use max-utilization as a
;;lower-bound / initial supply.
;;This is like having an army of ghosts.

;;distributors need to be implemented....
;;default is binned, provides deterministic
;;round-robin growth.
;;Note: We can also have deterministic growth
;;using a seeded prng...



;;oh, we need to be able to generate supply...
;;Well, we can delegate that to RA....
;;Instead of creating "supply generators"
;;that are complicated, we can add additional
;;supply after-the-fact, and then fill again with
;;the next context.  So, basically, add-ghosts if
;;there are missed demands.
  
;;If we use records, we get ordered fields automatically.  Better strategy.
;;generic supply record.  Should probably tie these to marathon.schemas
(defrecord srecord [Type Enabled Quantity SRC Component OITitle
                    Name Behavior CycleTime Policy Tags Spawntime Location Position Original])
;;outstreaam/mystream is output....canonically RequirementsGeneratedSupply.csv
;;may not use this guy...we'll see.
(r/defrecord+ outrecord [[Iteration 0]
                        [SRC ""]
                        [Component ""]
                        [Quantity 0]])

;;generate a new supplyrecord.
(defn supply-record
  ([src component count]
   (->srecord "SupplyRecord" true count src component (str "Generated_"  src)
              "Auto" "Auto" 0 "Auto" "Auto" 0 "Auto" "Auto" false))
  ([src component] (srecord src component 0)))
  

;;import the original provided supply records (currently from excel) into the supplytable
(defn import-supply-records [ctx]) ;;may be uncessary.
;;we add a ghost record though
(defn make-ghost []
  (supply-record  "SupplyRecord"  true 1 "Ghost" "Ghost"
                  "Anything" "Auto"  "Ghost365_45" ;default behavior...
                  0 "Ghost365_45" "Auto"  0 "Auto" "Auto" false))
;;note:
;;the act of "creating supply" from a supply record
;;transforms a serialized form into a sequence of instructions
;;that create entities and schedule initial behavior, like spawning.
;;So, one way to view it is that the record maps to a sequence of
;;instructions, which yield entities.

;;We're basically altering the initial supply via supply records....
;;The reason we do so is because the system is setup to initialize
;;the context from raw tables, so changing the data creates
;;new sim contexts automagically.

;;As we look at how to vary supply programmatically, rather than
;;"just" varying the table, we could look into varying the
;;context directly....

;;reads distributions from a table of [src ac rc ng]
;;probably a better way to do this...legacy implementation
(defn import-aggregate-distributions [tbls & {:keys [distribution-table dtype]
                                              :or {distribution-table "GhostProportionsAggregate"
                                                   dtype :bin}}]
  (reduce (fn [acc r]
            (let [{:keys [SRC AC RC NG] r}]
              (assoc acc src {"AC" AC "RC" RC "NG" NG}}))
          {}
          (get tbls distribution-table)))

;;kind of lame at the moment
;;should return a map of [compo val]
(defprotocol IDistributor
  (distribute- [obj n]))

(defn distribute-by [f n]
  (let [tf (type f)]
    (cond (extends? IDistributor tf)
            (distribute- f n)
          (extends? clojure.core.protocols/IKVReduce tf)
            (reduce-kv (fn [acc k f]
                         (assoc acc k (f n))) {} f)
          (fn? f) (f n)
          :else
          (throw (Exception. (str "unknown distributor!"))))))

;;fill this in...
;;probably need some state.
;;TOM Change 19 April 2012
;;For an src, use our local distributor to compute how x ghosts should be converted into
;;[X,Y,Z..] units of supply, by component
;;Note: renamed 'ns to 'steps.  Embeding stuff in a requirements state
;;map.
(defn increment-supply [reqstate src compo n]
  (throw (Exception. (str "Not implemented!"))))

(defn compute-amounts [reqstate src n] (distribute-by (get reqstate src) n))
;;Replacement method for an earlier hack.  We now separate the process of calculating and applying
;;distributions.  Given a set of distributions, by component, apply them (whatever that means)
;;to the src.
(defn apply-amounts [reqstate src amounts]
  (reduce (fn [acc [compo n]]
            (if n
              (let [adjusted-count (double n)]
                (if (pos? adjusted-count)
                  (increment-supply acc src compo n)
                  acc))
              acc)) reqstate amounts))
;;So, each time we add supply, we conceptually take a growth step.
(defn distribute      [reqstate src count]
  (let [amounts (compute-amounts reqstate src count)
        steps   (or (:steps reqstate) [])
        total   (if (empty? steps) 0
                    (:total-ghosts (last steps)))]
    (-> reqstate
        (apply-amounts src amounts) 
        (assoc  :steps ;;record the step we took.
                (conj steps {:src src
                             :count count
                             :total-ghosts (+ total count)
                             :added amounts
                             :total  (throw (Exception. "copysupply"))})))))

;;I think we'll have this as part of the reqstate...
;;rather than a separate output.  Redo this...
(defn write-record [reqstate iteration src component quantity]
  (update-in reqstate
             [:iterations src] conj (->outrecord iteration src component quantity)))

;;This is an auxillary function to handle each run of the requirements analysis.
;;Given a simulation that is already primed and loaded, and possibly an initial supply of units, calculate the
;;units needed (the requirement), as represented by the amount of ghosts created by SRC.  The requirement is then
;;applied to the distributor, which transforms the homogeneous supply of ghosts into a set of units that are to be added
;;to the final result. The return is a dictionary of (SRC|Component, count) pairs.  This allows us to trivially update
;;the supplytable, by incrementing.

;;The idea here is to just layer on another fill step,
;;if there are unmet demands, we allow ghosts to be created and
;;used to fill.  This is different than the scheme in vba, where
;;we had "supply generators" that were a little squirrely.  We'll
;;just move to a multipass fill, and use a ghost-fill function as
;;the last stage.  Easy peasy.
(defn unconstrained-ghost-step [ctx]
  (throw (Exception. (str "placeholder for stepping with ghost-fills.")))
  )

;;probably want to stick this in marathon.analysis...
;;Given a history, compute the maximum amount of ghosts
;;(high-water mark) over time.  We should be able to
;;determine this easily by selecting entities with a
;;"Ghost" component at the end of the simulation.
(defn history->ghosts [h]
  (throw (Exception. (str "placeholder for computing ghosts"))))

;;I think we'll prefer to work with the history, so probably using a marathon-stream
;;instead of this approach...
;;TODO: Replace event-step-marathon with the appropriate simreducer or whatnot.
;;Returns the next requirement state, if we actually have a requirement.
;;Otherwise nil.
(defn calculate-requirement [reqstate ctx ns & {:keys [step distance]
                                                :or   {step unconstrained-ghost-step
                                                       distance history->ghosts}}]
  (let [dist (-> ctx (step-function) (distance))]
    (when (pos? distance)
      (do (println "Generated ghosts on iteration")
          (distribute reqstate src dist)))))

;;do we actually need a ghost watcher?
;;We have ghost-specific events, like ghost spawn and the
;;like.  May not need to scrape them..
;;returns a map of {src src-max}, typically
;;looks for a ghost-watcher observer...
(defn get-ghost-counts [ctx] )

;;The iterative convergence function is a fixed-point function that implements the algorithm described in the declarations section.
;;During iterative convergence, we don;;t care about intermediate results, only the final fixed-point calculation.
;;After we determine the fixed-point, we can perform a final dynamic analysis (capacity analysis) on the output.
;;The concrete implementation follows:
    ;;Assuming we have a simulation object, we can call its FromExcel method to load all demand and supply from Excel.
    ;;Future iterations will avoid re-parsing demand and supply, but for now, we;;ll just reload the whole thing everytime.
        ;;Possibly use a "limited-reload? or hot-load" method.
    ;;Each time the simulation runs, it will load supply from the SupplyRecords input (which is a worksheet).
    ;;Our goal is to effectively generate supply records.
    ;;In the extreme case, we start with no supply, thus no supply records.
    ;;We must have ghost relation rules in effect (i.e. ghostables for all the SRCs), and a set of demands to simulate.
    ;;Prior to the first iteration, we attach a special observer to the simulation;;s event pump.
        ;;This observer, the GhostWatcher, will maintain statistics for all the ghosts spawned, by SRC, etc. during the simulation.
        ;;The GhostWatcher will serve as the GhostRecord generator that we will need in our distribution function.
    
    ;;On the first iteration, the simulation is initialized "fromexcel", which pulls in all supply, demand, policy, etc.
        ;;Note, again, there may be no supply.
    
    ;;We then pass the primed simulation, a valid distribution function, and the ghostwatcher, into the CalculateRequirement function.
        ;;The simulation is run in a non-interactive mode, with no supply, which triggers the generation of ghosts.
            ;;We want each run to be as fast as possible, thus we run the sim in its most effecient state, avoiding log files
            ;;and other detritus.
            
            ;;The externally-attached Ghost Watcher observes all of these ghost spawning events, noting the SRC count of ghosts
            ;;generated during the run.
            
        ;;CalculateRequirement;;s return value is simply a function of the application of the distributor to the observerd quantities of ghosts, by SRC,
        ;;in the ghostwatcher.  This should be a number of units, by SRC, by component.
    
    ;;Given a set of new units, we simply update the supply records data (worksheet), possibly recording the amount of supply added during each
        ;;iteration.
    
    ;;Given a set of no new units (i.e. zero ghosts generated), iterative convergence returns the reported supply.  The data is already on-hand for
    ;;additional analysis (namely capacity analyis), if desired.

;;Probably a good idea to break this apart.
(defn ->requirement-state [ctx supply]
  {:steps []
   :ctx   nil
   :supply supply
   }
  )


;;create an efficient requirements analysis context from
;;a set of initial tables.
(defn requirements-context [tables]
  (throw (Exception. (str "not implemented"))))

;;Think about the inputs you'd need...
;;We typically call this from the outside, supplying our
;;tables....
(defn interative-convergence [tables]
  (let [ctx0 (requirements-ctx tables)
        rs   (->requirement-state ctx (initial-supply ctx))
  )

Public Sub IterativeConvergence(Optional logevents As Boolean, Optional addcapacity As Boolean, Optional squeeze As Boolean)

Dim tstrt As Single
Dim logger As TimeStep_ObserverLogFile
Dim ghosts As TimeStep_ObserverGhostWatch
'tom change 25 April 2012
Dim ns As Collection

Set ns = New Collection
Iteration = 0
importSupplyRecords
importAggregateDistributions , dtype

Err.Raise 101, , "Needs updating!"
If sim Is Nothing Then
    Set sim = New TimeStep_Engine
    sim.noio = noio
    sim.Initialize_Engine_FromExcel New TimeStep_SimState, True  'this will cause overhead....
Else
    sim.noio = noio
    sim.Reset_Engine_FromExcel
End If
     

If logevents Then
    Set logger = New TimeStep_ObserverLogFile
    logger.init "ReqEvents" & Iteration, sim.EventManager.evtstream
End If
     
    
     
tstrt = Timer()

Do
    Set ghosts = sim.outputmanager.observers("Ghosts")
    Iteration = Iteration + 1
    
    If Not CalculateRequirement(sim, ghosts, , ns) Then
        Debug.Print "No More ghosts to generate!"
        Exit Do
    Else
        tstrt = Timer() - tstrt
        
        If noio Then 'don't bother writing to the sheet
            sim.Reset_Engine_FromExcel True, supplyTable 'this will reset marathon, using the GeneratedSupply Worksheet to pull in initial supply.
        Else
            updateGeneratedSupply
            sim.Reset_Engine_FromExcel True
        End If
        
        If logevents Then
            Set logger = Nothing
            Set logger = New TimeStep_ObserverLogFile
            logger.init "ReqEvents" & Iteration, sim.EventManager.evtstream
        End If
    End If
Loop


If squeeze Then 'Make sure we've found the optimal using bisection
    If ns.count > 1 Then 'need to handle this corner case.
        Bisect sim, ns, ns(ns.count - 1), ns(ns.count), Iteration
    ElseIf ns.count = 1 Then
        Bisect sim, ns, zeroSupply(ns), ns(ns.count), Iteration
    End If
End If

If noio Then finalIO sim

updateGeneratedSupply

Set logger = Nothing
Set sim = Nothing

End Sub
Private Function zeroSupply(ns As Collection) As Dictionary
Set zeroSupply = copyDict(ns(1))
End Function

'TOM Change 3 August -> implemented a bracketing algorithm not unlike binary search.
'This is meant to be performed on a single SRC, i.e. a single independent requirement.
'Bisection requires an src as the arguement.
Public Sub Bisect(sim As TimeStep_Engine, ns As Collection, left As Dictionary, right As Dictionary, Iteration As Long)

Dim searchstate As Collection
Dim middle As Long
Dim lower As Long
Dim upper As Long
Dim src As String
Dim lowest As Long
Dim uppermoved As Boolean
Dim idx As Long, bin As Long
Dim binstate As Dictionary

Dim ghosts As TimeStep_ObserverGhostWatch

Set searchstate = New Collection

lower = left.item("totalghosts")
lowest = lower
upper = right.item("totalghosts")
middle = lower + (upper - lower) \ 2

src = left("src")
   
'determine what the next step should be

While upper - lower > 1
    Iteration = Iteration + 1
    Debug.Print "Iteration " & Iteration & ", Bracketing solution between n = [" & lower & ", " & upper & "] ghosts."

    Set supplyTable = copysupply(left("total")) 'starting from our last step
    Distribute src, middle - lowest, , searchstate  'add our ghosts.
    
    If noio Then 'don't bother writing to the sheet
        sim.Reset_Engine_FromExcel True, supplyTable 'this will reset marathon, using the GeneratedSupply Worksheet to pull in initial supply.
    Else
        updateGeneratedSupply
        sim.Reset_Engine_FromExcel True
    End If

    Set ghosts = sim.outputmanager.observers("Ghosts")


    'test sufficiency with new supply
    If Not CalculateRequirement(sim, ghosts, , searchstate) Then
        'did not generate ghosts.....
        'this means our middle value is now our right, upperbound.
        upper = middle 'move the bracket <<<<<<-
        middle = lower + (upper - lower) \ 2
        'no need to redistribute.
        uppermoved = True
    Else
        'we added ghosts, which means middle is insufficient.
        lower = middle 'move the bracket ->>>>>>>
        middle = lower + (upper - lower) \ 2
        'Set left = searchstate(searchstate.count)
        uppermoved = False
    End If
Wend

If upper - lower = 1 Then 'ubound is the answer
    lower = upper
    Set supplyTable = copysupply(left("total")) 'starting from our last step
    Distribute src, upper - lowest, , searchstate  'add our ghosts.
ElseIf upper - lower = 2 Then 'middle is the answer
     If uppermoved Then
        lower = lower + 1
     Else
        lower = upper
     End If
     Distribute src, upper - lowest, , searchstate
Else
    Err.Raise 101, , "convergence is off"
End If

Debug.Print "No More ghosts to generate.  Binary search converged on " & lower & " Ghosts for src " & src

If noio Then finalIO sim

updateGeneratedSupply

Set sim = Nothing

End Sub


;;We use this...
Public Sub FastConvergence(Optional logevents As Boolean, Optional Binary As Boolean)
noio = True
IterativeConvergence logevents, , Binary
End Sub


;; Public Sub BinaryConvergence()
;; noio = True
;; BisectionConvergence False
;; End Sub




;; Private Sub makeghost()

;; Dim grecord As GenericRecord
;; Set grecord = New GenericRecord

;; With grecord
;;     .AddField "Type", "SupplyRecord"
;;     .AddField "Enabled", True
;;     .AddField "Quantity", 1
;;     .AddField "SRC", "Ghost"
;;     .AddField "Component", "Ghost"
;;     .AddField "OITitle", "Anything"
;;     .AddField "Name", "Auto"
;;     .AddField "Behavior", "Ghost365_45"
;;     .AddField "CycleTime", 0
;;     .AddField "Policy", "Ghost365_45"
;;     .AddField "Tags", "Auto"
;;     .AddField "SpawnTime", 0
;;     .AddField "Location", "Auto"
;;     .AddField "Position", "Auto"
;;     .AddField "Original", False
;; End With

;; supplyTable.add "Ghost", grecord

;; Debug.Print "Asked to do requirements analysis without a ghost," & _
;;             " added Default ghost record to generated supply table."
;; End Sub



;; 'Write the supplytable to xl.
;; Public Sub updateGeneratedSupply()
;; Static rcrdstream As IRecordStream
;; Dim rec
;; Dim tmp
;; Static myrecord As GenericRecord


;; 'TOM change 16 April 2012
;; 'Set rcrdstream = New Streamer_xl
;; Set rcrdstream = New Streamer_CSV


;; For Each rec In supplyTable
;;     Set myrecord = supplyTable(rec)
;;     rcrdstream.init myrecord.fieldnames, ActiveWorkbook.path & "\GeneratedSupply.csv"
;;     Exit For
;; Next rec

;; For Each rec In supplyTable
;;     Set myrecord = supplyTable(rec) 'these are float values.
;;     Set myrecord = myrecord.clone
;;     myrecord.UpdateField "Quantity", myrecord.fields("Quantity")
;;     rcrdstream.writeGeneric myrecord
;; Next rec

;; End Sub



;;Don't think we really need these...

;; ;;increment the count in the record.  Based on src and component, calculate a key to fetch the record from
;; ;;the supplytable.  Then increment the count of the record.
;; Public Sub incRecord(src As String, component As String, increment As Single)
;; Static rec As GenericRecord

;; Static val As Single
;; If hasrecord(src, component) Then
;;     Set rec = supplyTable(Encode(src, component))
;;     val = rec.fields("Quantity")
;;     rec.UpdateField ("Quantity"), val + increment
;; Else
;;     supplyTable.add (Encode(src, component)), srecord(src, component, CLng(increment))
;; End If

;; End Sub

;; ;;increment the count in the record.  Based on src and component, calculate a key to fetch the record from
;; ;;the supplytable. Then increment the count of the record.
;; Public Sub setRecord(src As String, component As String, amount As Single)
;; Static rec As GenericRecord

;; 'Static val As Single
;; If hasrecord(src, component) Then
;;     Set rec = supplyTable(Encode(src, component))
;;     'val = rec.fields("Quantity")
;;     rec.UpdateField ("Quantity"), CLng(amount)
;; Else
;;     supplyTable.add (Encode(src, component)), srecord(src, component, CLng(amount))
;; End If
;; End Sub

;; Private Function hasrecord(src As String, component As String) As Boolean
;; hasrecord = supplyTable.exists(Encode(src, component))
;; End Function



;; Private Function copysupply(Optional supplytbl As Dictionary) As Dictionary
;; Dim tbl
;; Dim ptr As GenericRecord
;; Set copysupply = New Dictionary
;; If supplytbl Is Nothing Then Set supplytbl = supplyTable
;; For Each tbl In supplytbl
;;     Set ptr = New GenericRecord
;;     Set ptr = supplytbl.item(tbl).clone
;;     copysupply.add tbl, ptr
;; Next tbl

;; End Function


;;Distributors
;;============
;;I think we're going to skip this and just define a
;;distribute function..
  
;; Private Function addDistributor(src As String, compoDistributions As Dictionary, Optional dtype As DistributorType)
;; Dim distributor As Dynamic_Distributors
;; Dim compo

;; For Each compo In compoDistributions
;;     If compoDistributions(compo) = 0 Then
;;         compoDistributions.Remove (compo)
;;     End If
;; Next compo

;; Set distributor = New Dynamic_Distributors
;; Select Case dtype
;;     Case binned
;;         distributor.initBinned compoDistributions, src
;;         SRCdistributors.add src, distributor
;;     Case continuous1418
;;         distributor.initContinuous1418 compoDistributions
;;         SRCdistributors.add src, distributor
;;     Case rounding1418
;;         distributor.initRounding1418 compoDistributions
;;         SRCdistributors.add src, distributor
;; End Select

;; End Function

;; ''TOM Change 3 August -> implemented a bracketing algorithm not unlike binary search.
;; ''This is meant to be performed on a single SRC, i.e. a single independent requirement.
;; ''Bisection requires an src as the arguement.
;; Public Sub BisectionConvergence(Optional logevents As Boolean, Optional addcapacity As Boolean)

;; Dim tstrt As Single
;; Dim logger As TimeStep_ObserverLogFile
;; Dim ghosts As TimeStep_ObserverGhostWatch
;; Dim max As Long, min As Long
;; Dim nextX As Long
;; Dim generatedGhosts As Boolean
;; Dim fX As Long
;; Dim bracketed As Boolean
;; Dim src As String
;; Dim ky

;; Iteration = 0
;; importSupplyRecords
;; importAggregateDistributions
;; Err.Raise 101, , "Needs updating!"

;; If sim Is Nothing Then
;;     Set sim = New TimeStep_Engine
;;     sim.noio = noio
;;     sim.Initialize_Engine_FromExcel New TimeStep_SimState, True 'this will cause overhead....
;; Else
;;     sim.noio = noio
;;     sim.Reset_Engine_FromExcel
;; End If


;; If logevents Then
;;     Set logger = New TimeStep_ObserverLogFile
;;     logger.init "ReqEvents" & Iteration, sim.EventManager.evtstream
;; End If

;; For Each ky In sim.DemandManager.demandmap
;;     src = sim.DemandManager.demandmap.item(ky).src
;;     If src <> "Ghost" Then Exit For
;; Next ky

;; If src = "Ghost" Then Err.Raise 101, , "Only registered ghost srcs"


;; tstrt = Timer()

;; max = 10
;; min = 0
;; fX = max
;; nextX = max
;; bracketed = False

;; While max <> min
;;     Set ghosts = sim.outputmanager.observers("Ghosts")
;;     Iteration = Iteration + 1
;;     'determine what the next step should be
;;     Debug.Print "Searching for solution using " & nextX & " ghosts."
;;     fX = search(src, nextX, sim, ghosts)

;;     If fX = 0 Then
;;         bracketed = True
;;         max = nextX
;;         nextX = (max - min) \ 2 + min
;;     ElseIf fX > 0 Then
;;         If bracketed Then
;;             min = max
;;             max = 2 * max
;;             nextX = (max - min) \ 2
;;         Else
;;             If fX > max Then max = max + fX
;;             'ElseIf fX < max Then
;;                 'max = fX
;;             'End If
;;             min = max
;;             max = 2 * max
;;             nextX = max
;;         End If
;;     End If

;;     tstrt = Timer() - tstrt

;;     If noio Then 'don't bother writing to the sheet
;;         sim.Reset_Engine_FromExcel True, supplyTable 'this will reset marathon, using the GeneratedSupply Worksheet to pull in initial supply.
;;     Else
;;         updateGeneratedSupply
;;         sim.Reset_Engine_FromExcel True
;;     End If

;;     If logevents Then
;;         Set logger = Nothing
;;         Set logger = New TimeStep_ObserverLogFile
;;         logger.init "ReqEvents" & Iteration, sim.EventManager.evtstream
;;     End If
;; Wend

;; Debug.Print "No More ghosts to generate.  Binary search converged on " & max & " Ghosts for src " & src

;; If noio Then finalIO sim

;; updateGeneratedSupply

;; Set logger = Nothing
;; Set sim = Nothing

;; End Sub

;; 'write out the final, summary report, specifically the final supply, the number of ghosts, etc.
;; Private Sub finalIO(sim As TimeStep_Engine, Optional addcapacity As Boolean)

;; updateGeneratedSupply
;; If addcapacity Then
;;     sim.noio = False
;;     sim.Reset_Engine_FromExcel True
;;     sim.EventStepMarathon
;; End If


;; End Sub

;; Public Function search(src As String, ghostcount As Long, sim As TimeStep_Engine, ghostwatcher As TimeStep_ObserverGhostWatch) As Long
;; Static counts As Dictionary
;; search = 0
;; Distribute src, ghostcount, True 'update the ghost solution
;; sim.EventStepMarathon 'execute the simulation, may produce ghosts.
;; Set counts = getGhostCounts(ghostwatcher)
;; If counts.count > 0 Then
;;     Debug.Print "Generated ghosts on iteration " & Iteration
;;     search = counts(src)
;; End If
;; End Function
