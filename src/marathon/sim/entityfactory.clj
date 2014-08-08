
;;The entity factory is a port of legacy functions used to read 
;;entity information from tabular sets of data according to 
;;the marathon data schemas.  The factory's primary function is 
;;to project records onto entities (i.e. units and demands) 
;;as well as serve as a hub for registering entities and 
;;scheduling things like initial starting conditions.  
;;The entityfactory could probably be broken apart, 
;;but it's legacy functionality would have to go somewhere. 
;;For now, we'll keep it in a similar namespace, but as 
;;opportunities arise, we may be able to abastract out 
;;functionality.
(ns marathon.sim.entityfactory
  (:require [marathon        [schemas :as s]]
            [marathon.demand [demanddata :as d]]
            [marathon.supply [unitdata :as u]]
            [marathon.sim.engine :as engine]
            [marathon.sim.core :as core]
            [spork.util      [table :as tbl]]
            [clojure.core    [reducers :as r]]))


;;*Entity Orders
;;The entity factory used to be an object that maintained 
;;a reference to a parent simulation context.  This allowed 
;;it to interpret data, like  a set of demand 
;;records, into the act of creating demand entities, 
;;notifying the simulation context that the a new demand had been
;;registered, and registering the demand with the demandstore.
;;Most of this was accomplished via side effects, so creating, 
;;and loading entities, was fairly imperative and stateful.

;;Going forward, we're going to break up the distiction between
;;these responsibilities, and communicate the results via 
;;EntityOrders.  An EntityOrder is merely a record that 
;;the entity factory can intepret, given a context, to yield 
;;a new context.  In this case, the context is our stock 
;;simcontext.  Seen as an interpreter, our entityfactory 
;;evalutates orders relative to the simulation context, returning 
;;a new simulation context.  We define new kinds of EntityOrders 
;;to allow for different types of entities to be built.

;;In the scheme of our IO processes, this means we care about 
;;a lower layer of functions that read raw data - typically 
;;in the form of tab delimited text, and convert that data 
;;into some kind of EntityOrder that our factory can recognize.

;;Ideally, we end up with a simple interface for "Creating" entities
;;in the simulation, where entity creation may involve an arbitrarily 
;;complex sequence of updates.


;;*Basic Functions
;;Many of our functions are concerned with IO, that is, reading 
;;tab-delimited text records, and creating entity orders from them.



;;For now, we're using dynamic scope...
(def ^:dynamic *ctx* engine/emptysim)

;;I think we may want to make the entityfactory functions operate 
;;on a dynamic var.  That should make the calling code clearer...
;;There are events where we trigger notifications and such, for 
;;logging purposes.  Maybe we we only check if the context is 
;;actually bound, and then do something.


;;The original procedure actually mutated the context 
;;as demand records were parsed.  We can decouple this step by 
;;creating a sequence of entity creation actions, i.e. entity 
;;orders, that can be processed into state changes.

;;For instance: 

(defn partition-dupes 
  "Returns a pair of [uniques, dupes] based on the key function"
  [keyfn xs]
  (let [known (atom #{})]
    (reduce (fn [[uniques dupes] x]
              (let [k (keyfn x)]
                (if (contains? @known k)
                  [uniques (assoc dupes k (conj (get k dupes []) x))]
                  (do (swap! known conj k)
                      [(conj uniques x) dupes]))))
            [[] {}]
            xs)))

(defn valid-record? 
  ([r params] 
     (and (:Enabled r) 
          (core/in-scope? (:SRC r) params)))
  ([r] (valid-record? r (core/get-parameters *ctx*))))

(defn demand-key [{:keys [SRC Vignette Operation Priority StartDay Duration]}]
  (str Priority "_"  Vignette "_" SRC "["  StartDay "..."  (+ StartDay Duration) "]"))
  
(defn record->demand 
  "Basic io function for converting raw records to demanddata."
  [{:keys [DemandKey SRC  Priority StartDay Duration Overlap Category 
           SourceFirst Quantity  OITitle Vignette Operation  DemandGroup ] :as rec}]
  (d/->demanddata 
     (or DemandKey (demand-key rec)) ;unique name associated with the demand entity.
     SRC ;demand-type, or other identifier of the capability demanded.
     Priority ;numerical value representing the relative fill priority.
     StartDay ;the day upon which the demand is activated and requiring fill.
     Duration ;the total time the demand is activated.
     Overlap  ;the demand-specific overlap requirement, if any
     Category ;descriptor for deployed unit behavior over-rides.
     SourceFirst  ;descriptor for supply preference. 
     Quantity  ;the total amount of entities required to fill the demand.
     OITitle   ;formerly OITitle.  Long-form description of the src capability.
     Vignette  ;Descriptor of the force list that generated this demand entity.
     Operation ;Fine-grained, unique description of the demand.
     DemandGroup ;Ket that associates a demand with other linked demands.  
     {} ;an ordered collection of all the fills recieved over time.                   
     {} ;map of the units currently associated with the demand.
     {} ;map of the units currently associated with this demand,
        ;that are not actively contributing toward filling the
        ;demand, due to a relief-in-place state.
     ))

(comment ;testing
  (def demand-ctx (assoc-in *ctx* [:state :parameters :SRCs-In-Scope] {"SRC1" true "SRC2" true "SRC3" true}))
)

;;Returns a set of updates to the context, including 
;;the addition of new demands, and 
(defn demands-from-records [recs]  
  (let [params (core/get-parameters *ctx*)]
        (let [[uniques dupes]  (->> recs 
                                    (r/filter valid-record?)
                                    (r/map #(assoc % :DemandKey (demand-key %)))                                    
                                    (partition-dupes :DemandKey)                                    
                                    )]
          {:register-demands uniques
           :report-duplicates dupes})))

(defn load-demand
                        

;;  574:                   If demandmap.exists(nm) Then
;;  575:                       If dupes.exists(nm) Then
;;  576:                           dupes(nm) = dupes(nm) + 1
;;  577:                       Else
;;  578:                           dupes.add nm, 1
;;  579:                       End If
;;  580:                   Else'register the demand.
;;  581:                       Set demand = associateDemand(recordToDemand(record), DemandManager)
;;  582:                       msg = "Demand" & demand.name & " initialized"
;;  583:                      'Decouple
;;  584:                       SimLib.triggerEvent Initialize, DemandManager.name, DemandManager.name, msg, , state.context'log demand initialization
;;  585:                   End If

;; 48:   Public Sub DemandsFromRecords(records As Dictionary, DemandManager As TimeStep_ManagerOfDemand)
;;  549:  
;;  550:   Dim demand As TimeStep_DemandData
;;  551:   Dim dupes As Dictionary
;;  552:   Dim dup
;;  553:   Dim demandmap As Dictionary
;;  554:   Dim vig As String
;;  555:   Dim nm As String
;;  556:   Dim op As String
;;  557:   Dim pri As Long
;;  558:  
;;  559:   Dim rec
;;  560:   Set demandmap = DemandManager.demandmap
;;  561:  
;;  562:  
;;  563:   Set dupes = New Dictionary
;;  564:  'Decouple
;;  565:   With state.parameters
;;  566:        For Each rec In records
;;  567:           Set record = records(rec)
;;  568:           With record


;;  569:               If .fields("Enabled") = True And inScope(.fields("SRC")) Then
;;  570:                   vig = .fields("Vignette")
;;  571:                   op = .fields("Operation")
;;  572:                   pri = .fields("Priority")
;;  573:                   nm = op & "_" & vig & "_" & pri'demands have unique names
;;  574:                   If demandmap.exists(nm) Then
;;  575:                       If dupes.exists(nm) Then
;;  576:                           dupes(nm) = dupes(nm) + 1
;;  577:                       Else
;;  578:                           dupes.add nm, 1
;;  579:                       End If
;;  580:                   Else'register the demand.
;;  581:                       Set demand = associateDemand(recordToDemand(record), DemandManager)
;;  582:                       msg = "Demand" & demand.name & " initialized"
;;  583:                      'Decouple
;;  584:                       SimLib.triggerEvent Initialize, DemandManager.name, DemandManager.name, msg, , state.context'log demand initialization
;;  585:                   End If
;;  586:               Else
;;  587:                   msg = "Demand at row" & CLng(rec) & " disabled."
;;  588:                  'Decouple
;;  589:                   SimLib.triggerEvent Initialize, DemandManager.name, DemandManager.name, msg, , state.context'log demand initialization
;;  590:               End If
;;  591:           End With
;;  592:       Next rec
;;  593:   End With
;;  594:  
;;  595:  'notify of data errors, specifically duplicate demands.
;;  596:   For Each dup In dupes
;;  597:       msg = "Demand" & CStr(dup) & " had " & dupes(dup) & " duplicates in source data."
;;  598:      'Decouple
;;  599:       SimLib.triggerEvent Initialize, DemandManager.name, DemandManager.name, msg, , state.context'log demand initialization
;;  600:   Next dup
;;  601:  
;;  602:   End Sub


 ;; 609:   Public Sub DemandsFromTable(table As GenericTable, DemandManager As TimeStep_ManagerOfDemand)
 ;; 610:  
 ;; 611:   Dim demand As TimeStep_DemandData
 ;; 612:   Dim dupes As Dictionary
 ;; 613:   Dim dup
 ;; 614:   Dim demandmap As Dictionary
 ;; 615:   Dim vig As String
 ;; 616:   Dim nm As String
 ;; 617:   Dim op As String
 ;; 618:   Dim pri As Long
 ;; 619:  
 ;; 620:   Dim rec
 ;; 621:   Set demandmap = DemandManager.demandmap
 ;; 622:  
 ;; 623:  
 ;; 624:   Set dupes = New Dictionary
 ;; 625:  'Decouple
 ;; 626:   With state.parameters
 ;; 627:       table.moveFirst
 ;; 628:        While Not table.EOF
 ;; 629:           Set record = table.getGenericRecord
 ;; 630:           With record
 ;; 631:               If .fields("Enabled") = True And inScope(.fields("SRC")) Then
 ;; 632:                   vig = .fields("Vignette")
 ;; 633:                   op = .fields("Operation")
 ;; 634:                   pri = .fields("Priority")
 ;; 635:                   nm = op & "_" & vig & "_" & pri'demands have unique names
 ;; 636:                   If demandmap.exists(nm) Then
 ;; 637:                       If dupes.exists(nm) Then
 ;; 638:                           dupes(nm) = dupes(nm) + 1
 ;; 639:                       Else
 ;; 640:                           dupes.add nm, 1
 ;; 641:                       End If
 ;; 642:                   Else'register the demand.
 ;; 643:                       Set demand = associateDemand(recordToDemand(record), DemandManager)
 ;; 644:                       msg = "Demand" & demand.name & " initialized"
 ;; 645:                      'Decouple
 ;; 646:                       SimLib.triggerEvent Initialize, DemandManager.name, DemandManager.name, msg, , state.context'log demand initialization
 ;; 647:                   End If
 ;; 648:               Else
 ;; 649:                   msg = "Demand at row" & CLng(rec) & " disabled."
 ;; 650:                  'Decouple
 ;; 651:                   SimLib.triggerEvent Initialize, DemandManager.name, DemandManager.name, msg, , state.context'log demand initialization
 ;; 652:               End If
 ;; 653:           End With
 ;; 654:           table.moveNext
 ;; 655:       Wend
 ;; 656:   End With
 ;; 657:  
 ;; 658:  'notify of data errors, specifically duplicate demands.
 ;; 659:   For Each dup In dupes
 ;; 660:       msg = "Demand" & CStr(dup) & " had " & dupes(dup) & " duplicates in source data."
 ;; 661:      'Decouple
 ;; 662:       SimLib.triggerEvent Initialize, DemandManager.name, DemandManager.name, msg, , state.context'log demand initialization
 ;; 663:   Next dup
 ;; 664:  
 ;; 665:   End Sub


 ;; 667:   Public Function recordToDemand(inrec As GenericRecord) As TimeStep_DemandData
 ;; 668:   Dim nm As String
 ;; 669:  
 ;; 670:   With inrec
 ;; 671:       nm = DemandKey(inrec)   '.fields("Operation") & "_" & .fields("Vignette") & "_" & .fields("Priority")
 ;; 672:       Set recordToDemand = _
 ;; 673:           createDemand(nm, .fields("StartDay"), .fields("Duration"), .fields("Overlap"), _
 ;; 674:                       .fields("SRC"), .fields("Quantity"), .fields("Priority"), _
 ;; 675:                       0, .fields("Operation"), .fields("Vignette"), .fields("SourceFirst"), _
 ;; 676:                       .fields("DemandGroup"))
 ;; 677:   End With
 ;; 678:  
 ;; 679:   End Function


 ;; 681:   Private Function DemandKey(rec As GenericRecord) As String
 ;; 682:   Dim startday As Single
 ;; 683:   Dim dur As Single
 ;; 684:  
 ;; 685:   With rec
 ;; 686:       DemandKey = .fields("Priority") & "_" & .fields("Vignette") & "_" & .fields("SRC") & "_"
 ;; 687:       startday = CSng(.fields("StartDay"))
 ;; 688:       dur = CSng(.fields("Duration"))
 ;; 689:       DemandKey = DemandKey & "[" & startday & "..." & (startday + dur) & "]"
 ;; 690:   End With
 ;; 691:       
 ;; 692:   End Function


 ;; 693:   Public Function associateDemand(demand As TimeStep_DemandData, demandmgr As TimeStep_ManagerOfDemand) As TimeStep_DemandData
 ;; 694:  'Decouple
 ;; 695:   demand.index = state.parameters.demandstart + demandmgr.demandmap.count + 1
 ;; 696:  
 ;; 697:   If demandmgr.demandmap.exists(demand.name) Then
 ;; 698:       demand.name = demand.name & "_" & demandmgr.demandmap.count + 1
 ;; 699:   End If
 ;; 700:  
 ;; 701:  'Decouple from demandmanager
 ;; 702:   MarathonOpDemand.registerDemand demand, demandmgr, state.policystore, state.context
 ;; 703:  'demandmgr.registerDemand demand 'refactored....
 ;; 704:  
 ;; 705:  
 ;; 706:   Set associateDemand = demand
 ;; 707:   End Function


 ;; 710:   Public Function createDemand(ByRef name As String, tstart As Single, duration As Single, overlap As Single, _
 ;; 711:                               primaryunit As String, quantity As Long, priority As Single, index As Long, _
 ;; 712:                                   Optional ByRef operation As String, Optional ByRef vignette As String, _
 ;; 713:                                       Optional ByRef sourcefirst As String, Optional ByRef demandgroup As String) As TimeStep_DemandData
 ;; 714:   Set createDemand = New TimeStep_DemandData
 ;; 715:  'TOM Change 6 Dec 2010 - formatting change, for readability
 ;; 716:   With createDemand
 ;; 717:       .name = name
 ;; 718:       .startday = tstart
 ;; 719:       .duration = duration
 ;; 720:       .overlap = overlap
 ;; 721:       .primaryunit = primaryunit
 ;; 722:       .src = .primaryunit
 ;; 723:       If quantity > 0 Then
 ;; 724:           .quantity = quantity
 ;; 725:       Else
 ;; 726:           .quantity = 1
 ;; 727:       End If
 ;; 728:       .priority = priority
 ;; 729:       .index = index
 ;; 730:       If operation = vbNullString Then operation = "Random_" & index
 ;; 731:       .operation = operation
 ;; 732:       If sourcefirst = vbNullString Then sourcefirst = "AC"
 ;; 733:       .sourcefirst = sourcefirst
 ;; 734:       If vignette = vbNullString Then vignette = "Random_" & index
 ;; 735:       .vignette = vignette
 ;; 736:      'Tom change 17 Aug 2012
 ;; 737:       If isUngrouped(demandgroup) Then
 ;; 738:           .demandgroup = UnGrouped
 ;; 739:       Else
 ;; 740:           .demandgroup = demandgroup
 ;; 741:       End If
 ;; 742:   End With
 ;; 743:  
 ;; 744:   End Function


 ;; 746:   Public Function isUngrouped(Optional grp As String) As Boolean
 ;; 747:   isUngrouped = grp = vbNullString
 ;; 748:   If Not isUngrouped Then _
 ;; 749:       isUngrouped = UCase(grp) = UCase(UnGrouped)
 ;; 750:  
 ;; 751:   End Function



 ;; 126:   Public Function recordToUnit(inrec As GenericRecord) As TimeStep_UnitData
 ;; 127:  
 ;; 128:   With inrec
 ;; 129:       Set recordToUnit = createUnit(.fields("Name"), .fields("SRC"), .fields("OITitle"), _
 ;; 130:                                     .fields("Component"), .fields("CycleTime"), .fields("Policy"))
 ;; 131:   End With
 ;; 132:  
 ;; 133:   End Function
 


 667:   Public Function recordToDemand(inrec As GenericRecord) As TimeStep_DemandData
 668:   Dim nm As String
 669:  
 670:   With inrec
 671:       nm = DemandKey(inrec)   '.fields("Operation") & "_" & .fields("Vignette") & "_" & .fields("Priority")
 672:       Set recordToDemand = _
 673:           createDemand(nm, .fields("StartDay"), .fields("Duration"), .fields("Overlap"), _
 674:                       .fields("SRC"), .fields("Quantity"), .fields("Priority"), _
 675:                       0, .fields("Operation"), .fields("Vignette"), .fields("SourceFirst"), _
 676:                       .fields("DemandGroup"))
 677:   End With
 678:  
 679:   End Function




(defn record->unitdata [])
