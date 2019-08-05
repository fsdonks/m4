;;temporary namespace for requirments analysis to
;;live in during refactoring and migration to
;;parallel stuff.
(ns requirements.testing
  (:require  [clojure.test :as t]))

(comment ;testing
;;   #_(def root (hpath "\\Documents\\srm\\tst\\notionalv2\\reqbase.xlsx"))
;;   (def ags "Type	Enabled	SRC	AC	NG	RC	Note
;; Blah	TRUE	43429R000	0.188405797	0.202898551	0.608695652	This produces a huge requirement lol.  Great pathological case.")
;;   (def agg-table
;;     #spork.util.table.column-table{:fields [:Type :Enabled :SRC :AC :NG :RC :Note],
;;                                    :columns [["Blah"] [True] ["43429R000"] [13/69] [42/69] [14/69]
;;                                              ["This produces a huge requirement lol.  Great pathological case."]]})
;;   (def agg-table
;;     #spork.util.table.column-table{:fields [:Type :Enabled :SRC :AC :NG :RC :Note],
;;                                    :columns [["Blah"] [True] ["10560RN00"] [1] [0] [0]
;;                                              ["This produces a huge requirement lol.  Great pathological case."]]})
  (def root (hpath "\\Documents\\marv\\vnv\\m4v6\\testdata-v6.xlsx"))
  (require '[marathon.analysis [dummydata :as data]])
  (def dummy-table
    (apply-schema (marathon.schemas/get-schema :SupplyRecords)
                  (tbl/keywordize-field-names (tbl/records->table  data/dummy-supply-records))))
  (def tbls (a/load-requirements-project root))
  
  ;;derive a requirements-state...
  (def icres (requirements->table
              (tables->requirements (:tables tbls) :search iterative-convergence)))
  (def bsres (requirements->table
              (tables->requirements (:tables tbls) :search bisecting-convergence)))
  (def bsresa (requirements->table
              (tables->requirements-async (:tables tbls) :search bisecting-convergence)))

  ;;Much better...This ends up testing a huge case.
  (def bigres (requirements->table
              (tables->requirements (assoc (:tables tbls) :GhostProportionsAggregate agg-table) :search bisecting-convergence)))
  (def s1 {"AC" 1696969696969697/4000000000000000
           "RC" 0N
           "NG" 5757575757575757/10000000000000000})
  (def rootbig "C:/Users/tspoon/Documents/srm/tst/notionalv2/reqbasebig.xlsx")
  (def tbls  (a/load-requirements-project rootbig))
  (def icres (requirements->table
              (tables->requirements (:tables tbls) :search iterative-convergence)))
  (def bsres (requirements->table
              (tables->requirements (:tables tbls) :search bisecting-convergence)))
  (def icsres  (requirements->table
                (tables->requirements (:tables tbls) :search iterative-convergence-shared)))

  (def pks (demands->src-peaks (tbl/table-records (:DemandRecords (:tables tbls)))))
  ;;These are massive src requirements...
  (def massives
    [{:SRC "42529RE00", :Required 1479, :Peak 201} ;;~5x
     {:SRC "41750R100", :Required 1240, :Peak 102}
     {:SRC "12567RE00", :Required 1152, :Peak 160}
     {:SRC "27523RC00", :Required 916,  :Peak 127}
     {:SRC "19539RB00", :Required 743,  :Peak 188}
     {:SRC "09537RB00", :Required 675,  :Peak 227}
     {:SRC "14527RB00", :Required 649,  :Peak 128}
     {:SRC "19473K000", :Required 563,  :Peak 262}
     {:SRC "10527RC00", :Required 503,  :Peak 115}])


  (require '[incanter [core :as i] [charts :as c]])
  (def root (hpath "\\Documents\\marv\\vnv\\m4v6\\testdata-v6.xlsx"))
  (def tbls (a/load-requirements-project root))
  (def razero (load-src (:tables tbls) "42529RE00" {"AC" 0 "NG" 0.125 "RC" 0.875}))     


  (def bisections ;;from a previous run
     [[ 3216 0]
      [  2412  0]
      [  2010  0]
      [  1809  0]
      [  1709  0]
      [  1659  9]
      [  1684  4]
      [  1696  0]
      [  1690  1]
      [  1693  0]
      [  1691  0]])
  ;;computing data on requirement holes..  
  (def emps
    (if false ;(io/exists? "emps.edn")
      (clojure.edn/read-string (slurp "emps.edn"))      
      (->> (range 1 1690)
           (pmap! 2 (fn [i] (do (println i) [i (calculate-requirement (distribute razero (:src razero) i) default-distance)])) )
           (into [] ))))

  (def emps (into emps bisections))

  (def bound-steps [[201 1]
                    [402 1]
                    [804  1]
                    [1608 1]
                    [3216 0]])

  
  (i/view (c/scatter-plot (map first emps) (map (comp (fn [n] (or n 0)) second) emps)
                          :title "Earliest Missed Demand x Growth Step" :x-label "Step" :y-label "Quantity Earliest Unfilled Demands"))
  (i/view (c/scatter-plot (map first emps) (map (comp #(if (pos? %) 1.0 0)(fn [n] (or n 0)) second) emps)
                          :title "Missed Demand? by growth Step" :x-label "Step" :y-label "Missed Demand? [1 => True 0 => False]"))

  (def distros {"AC" 0.0, "RC" 0.875, "NG" 0.125})
  (def clength 2190)
  (defn inits [n] (for [[c q] (distribute-by distros n) :when (pos? q)] (marathon.ces.entityfactory/intervals q clength)))
  (defn init-points [n] (apply concat (map-indexed (fn [i xs] (let [x (+ (* i 0.1) i)] (map vector (repeat x) (rest xs)))) (inits n))))
  (defn init-chart [n] (let [xs (init-points n)] (i/view (c/scatter-plot (map first xs) (map second xs)))))
  
  )

(comment ;debugging a wierd case with new policies.
  (def p (io/hpath  "Documents\\m4sampledata\\leereqs\\m4\\testdata-v6-leebug.xlsx"))
(defn requirements-run
  "Primary function to compute  requirements analysis.  Reads requirements 
   project from inpath, computes requirement, and spits results to a tsv 
   table in the same root folder as inpath, requirements.txt"
  [inpath]
  (let [inpath (clojure.string/replace inpath #"\\" "/")
        base (->> (clojure.string/split inpath #"/")
                  (butlast)
                  (clojure.string/join "/"))
        outpath (str base "/requirements.txt")]
    (do (println ["Analyzing requirements for" inpath])        
        (->> (-> (a/load-requirements-project inpath)
                 (:tables)
                 (tables->requirements :search bisecting-convergence)
                 (requirements->table)
                 (tbl/table->tabdelimited))
             (spit outpath))
        (println ["Spit requirements to " outpath]))))
  
;;Performing a requirements run manually to replicate the error..
  
;; marathon.analysis.requirements> (requirements-run p)
;; [Analyzing requirements for C:/Users/thomas.l.spoon/Documents/m4test/testdata-v6-leebug.xlsx]
;; Loading CompositePolicyRecords . . . done.
;; Loading DemandRecords . . . done.
;; Loading SuitabilityRecords . . . (missing).
;; Loading PolicyTemplates . . . (missing).
;; Loading SRCTagRecords . . . done.
;; Loading Parameters . . . done.
;; Loading PolicyDefs . . . (missing).
;; Loading PolicyRecords . . . done.
;; Loading SupplyRecords . . . done.
;; Loading RelationRecords . . . done.
;; Loading GhostProportionsAggregate . . . done.
;; Loading PeriodRecords . . . done.
;; [:computing-requirements 10527RF00 :remaining 0]
;; [:computing-initial-supply]
;; [:growing-by :proportional :from {AC 0, RC 15}]
;; [:guessing-bounds [0 16] :at 16 :got 1]
;; [:guessing-bounds [17 32] :at 32 :got 1]
  
;; Exception [:unit "19_10527RF00_AC" :invalid-deployer "Must have
;; bogbudget > 0, \n cycletime in deployable window, or be eligible or
;; a followon deployment"] marathon.ces.deployment/deploy-unit
;; (deployment.clj:92)

  ;;Synopsis
  ;;========
  ;;The requirements analysis was trying to peform a 2-stage
  ;;search, and failed during the first stage due to exception thrown
  ;;trying to deploy entity 19.  During the first stage, bounding, the
  ;;algorithm is just trying to find an upper and lower bound by
  ;;multiple runs, doubling the interval each time.  That sets us of
  ;;for bection search.  Each run is a capacity analysis run (for
  ;;regardless of phase), where we are only running a single SRC,
  ;;and have a parametric supply, where for each compo:
  ;;current-supply(compo,src,step) =
  ;;    initial-supply(compo,src) + GhostProportionsAggregate(compo,src) * step
  ;;We basically replace the corresponding supply records, then run a
  ;;capacity analysis trying to find missed demand.  During the simulation
  ;;run, as soon as we find any missed demand, we stop and report the misses
  ;;(fail early), so we don't waste time on a failed supply.

  ;;In this case, the run errored out before we failed or completed....
  ;;Now the question is why?  
  
  ;;we learn that the unit is this guy from our convenient error message.
  (def bad-unit "4_10527RF00_AC" #_"19_10527RF00_AC")  
  (def bad-src "10527RF00")

  ;;Recreating the initial conditions for interactive forensic analysis.
  ;;====================================================================
  
  ;;let's alter the supply to do a capacity analysis with the same supply
  ;;we errored on, then use our analysis functions to get the state of
  ;;play when our problem happened.  We'll interactively dissect things
  ;;rather than sift through event logs hoping for history (we can still
  ;;do that too though).
  
  ;;move to marathon.analysis
  (defn alter-supply [src-compo->quantity]
    (fn [tbls]
      (->> (:SupplyRecords  tbls)
           (tbl/table-records)
           (map (fn [{:keys [SRC Component] :as r}]
                  (if-let [q (src-compo->quantity [SRC Component])]
                    (assoc r :Quantity q :Enabled true)
                    r)))
           (tbl/records->table)           
           (assoc tbls :SupplyRecords))))
  ;;we know the error occurs during requirements analysis search at n=64,
  ;;so, let's define a function that gets our context, and overrides the
  ;;input supply table to have said record (currently a Quantity = 1)
  ;;have a quantity of 64 (and be enabled).  This should replicate
  ;;our capacity run that bombed.  We can pass that function in via
  ;;the :table-xform optional keyword argument and it will be run
  ;;as a "pre-process" step on our input tables prior to initializing
  ;;the simulation context from the resulting output.
  (defn error-ctx []
    (a/load-context
      "C:/Users/thomas.l.spoon/Documents/m4test/testdata-v6-leebug.xlsx"
      #_:table-xform
      #_(alter-supply {[bad-src "AC"] 6 #_32
                     [bad-src "RC"] 90
                     })))
  ;;we can get the context the day before the error...
  (def prior (a/day-before-error (a/marathon-stream (error-ctx))))
  ;;[:error-occurs-in-next-frame]
  ;;In this context, a frame is a time-indexed pair of simulation contexts,
  ;;which sequentially form a history...
  ;;[t ctx]
  (def t (first prior))
  (println [:day-before t])
  ;;Note: we typically don't want to print the context, since it's typically
  ;;really verbose (especially with lots of entities).  We have several
  ;;means of visualizing and querying it though. In short: avoid evaluating
  ;;or otherwise print the ctx at the repl, it might occupy your repl for a
  ;;bit.
  (def ctx (second prior))
  ;;We are effectively, at the beginning of the day prior to the error
  ;;occurring.  Let's see what unit 19 is doing now...
  ;;Using the function from marathon.ces.core/current-entity, we can
  ;;get a map representing the entity at the current point in time.
  ;;This is useful, because in a discrete event context, the entity's
  ;;statistics won't technically change until it next update...the entity
  ;;is effectively frozen, but we know it's in a state that implies
  ;;time-varying metrics, and we'd like a view of the entity as of 1822...
  ;;current-entity lets us get a view of the entity as it exists "now",
  ;;namely with linearly interpolated stats for it's various time-dependent
  ;;metrics, like cycle duration, time in cycle, bog, dwell, etc.
  (def e (core/current-entity ctx bad-unit))
  ;; marathon.analysis.requirements> (keys e)
  ;; (:oi-title :cycles :policystack :statedata :home :deployment-index
  ;; :locationname :interactive :deployable :speed :cycletime
  ;; :unit-entity :name :dt :unit-index :deployable-bucket :supply :type
  ;; :behavior :src :state :icon :positionpolicy :component :policy
  ;; :dwell-time-when-deployed :oititle :default-recovery :last-update
  ;; :date-to-reset :currentcycle :label :spawntime :deployable-cat
  ;; :locationhistory :physical :position :location :velocity)
  
  ;;we can print out the entity to the repl by evaluating it....
  ;;however, the policy data structure is typically pretty verbose...
  ;;let's elide it...

  ;;marathon.ces.unit/summary provides a nice short description of the
  ;;typical essentials..
  
  ;; marathon.analysis.requirements> (marathon.ces.unit/summary e)
  ;; {:bog 0, :nextstate #{:c2 :dwelling},
  ;;  :location-history ["Train"
  ;;                     "Ready"
  ;;                     ["Ready" :deployable] "Available"
  ;;                     "9424_Shiloh_10527RF00_[761...769]"
  ;;                     "9425_Shiloh_10527RF00_[769...809]"
  ;;                     "9426_Shiloh_10527RF00_[809...841]"
  ;;                     "9427_Shiloh_10527RF00_[841...1995]" "Reset"
  ;;                     ["Reset" :deployable]
  ;;                     "Train" "Ready"],
  ;;  :cycletime 674, :name "19_10527RF00_AC",
  ;;  :positionstate #{:c2 :dwelling},
  ;;  :deployable? nil,
  ;;  :duration 56,
  ;;  :src   "10527RF00",
  ;;  :dwell 675,
  ;;  :positionpolicy "Ready",
  ;;  :policy   "TAA_2024_Requirements_AC",
  ;;  :statestart 0,
  ;;  :statehistory [],
  ;;  :timeinstate 0,
  ;;  :curstate #{:c2 :dwelling},
  ;;  :location "Ready"}

  ;;We can see the entity - at time 1822 - is dwelling.  The location history
  ;;provides a view of where it's been, to include policy locations and deployments.
  (def pol (marathon.data.protocols/get-active-policy (:policy e)))
  ;;marathon.analysis.requirements> (:name pol)
  ;;"TAA19-23_AC_1:2"
  
  ;;Looks like he's transitioned his policy.  
  ;;Waiting in Ready (not yet deployable according to policy)...
  

)

  
;; 'TOM Change 3 August -> implemented a bracketing algorithm not unlike binary search.
;; 'This is meant to be performed on a single SRC, i.e. a single independent requirement.
;; 'Bisection requires an src as the arguement.
;; Public Sub Bisect(sim As TimeStep_Engine, ns As Collection, left As Dictionary, right As Dictionary, Iteration As Long)

;; Dim searchstate As Collection
;; Dim middle As Long
;; Dim lower As Long
;; Dim upper As Long
;; Dim src As String
;; Dim lowest As Long
;; Dim uppermoved As Boolean
;; Dim idx As Long, bin As Long
;; Dim binstate As Dictionary

;; Dim ghosts As TimeStep_ObserverGhostWatch

;; Set searchstate = New Collection

;; lower = left.item("totalghosts")
;; lowest = lower
;; upper = right.item("totalghosts")
;; middle = lower + (upper - lower) \ 2

;; src = left("src")
   
;; 'determine what the next step should be

;; While upper - lower > 1
;;     Iteration = Iteration + 1
;;     Debug.Print "Iteration " & Iteration & ", Bracketing solution between n = [" & lower & ", " & upper & "] ghosts."

;;     Set supplyTable = copysupply(left("total")) 'starting from our last step
;;     Distribute src, middle - lowest, , searchstate  'add our ghosts.
    
;;     If noio Then 'don't bother writing to the sheet
;;         sim.Reset_Engine_FromExcel True, supplyTable 'this will reset marathon, using the GeneratedSupply Worksheet to pull in initial supply.
;;     Else
;;         updateGeneratedSupply
;;         sim.Reset_Engine_FromExcel True
;;     End If

;;     Set ghosts = sim.outputmanager.observers("Ghosts")


;;     'test sufficiency with new supply
;;     If Not CalculateRequirement(sim, ghosts, , searchstate) Then
;;         'did not generate ghosts.....
;;         'this means our middle value is now our right, upperbound.
;;         upper = middle 'move the bracket <<<<<<-
;;         middle = lower + (upper - lower) \ 2
;;         'no need to redistribute.
;;         uppermoved = True
;;     Else
;;         'we added ghosts, which means middle is insufficient.
;;         lower = middle 'move the bracket ->>>>>>>
;;         middle = lower + (upper - lower) \ 2
;;         'Set left = searchstate(searchstate.count)
;;         uppermoved = False
;;     End If
;; Wend

;; If upper - lower = 1 Then 'ubound is the answer
;;     lower = upper
;;     Set supplyTable = copysupply(left("total")) 'starting from our last step
;;     Distribute src, upper - lowest, , searchstate  'add our ghosts.
;; ElseIf upper - lower = 2 Then 'middle is the answer
;;      If uppermoved Then
;;         lower = lower + 1
;;      Else
;;         lower = upper
;;      End If
;;      Distribute src, upper - lowest, , searchstate
;; Else
;;     Err.Raise 101, , "convergence is off"
;; End If

;; Debug.Print "No More ghosts to generate.  Binary search converged on " & lower & " Ghosts for src " & src
;; If noio Then finalIO sim
;; updateGeneratedSupply
;; Set sim = Nothing
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


;;GeneratedSupply.csv  is out default output, apparently.
;;Don't think we really need these...

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

;;note: we need to filter only positive compodistriutions.

;;multimethod for constructing different distributors.
(defmulti distributor identity)
(defmethod distributor :binned [n] nil)
(defmethod distributor :continuous1418 [kw] nil)
(defmethod distributor :rounding1418 [kw] nil)



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





(comment ;;possibly obe
  ;;we add a ghost record though
  (defn make-ghost []
    (supply-record  "SupplyRecord"  true 1 "Ghost" "Ghost"
                    "Anything" "Auto"  "Ghost365_45" ;default behavior...
                    0 "Ghost365_45" "Auto"  0 "Auto" "Auto" false))

  ;;import the original provided supply records (currently from excel) into the supplytable
  (defn import-supply-records [ctx]) ;;may be uncessary.
  (defn clear-supply
    "Eliminate all the unit entities from the context."
    [ctx]
    )

  #_(defn reload-supply [ctx]
      (default-supply))

  ;;allows a nice handle on 
  #_(defn load-variable-supply-context [tbls]
      (fn [supply-records]))
)


;;Possibly OBE...

;;Compute a sequence of "empty" supply records
;;from the proportions indicated 
#_(defn proportion-record->supply-records [r]
  (let [src    (:SRC r)
        compos [:AC :RC :NG]]
    (for [c compos
          :let [n (get r c)]
          :when (pos? n)]
      (->supply-record src c n))))


;;Note: Currently not in use, OBE?

;;I think we'll have this as part of the reqstate...
;;rather than a separate output. Redo this...
#_(defn write-record
    [reqstate iteration src component quantity]
    (update-in reqstate  [:iterations src] conj
               (->outrecord iteration src component quantity)))
