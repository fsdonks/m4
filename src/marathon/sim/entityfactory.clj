
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
            [marathon.sim.demand :as demand]
            [marathon.supply [unitdata :as u]]
            [marathon.sim.engine :as engine]
            [spork.sim.simcontext :as sim]
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
  (let [known (java.util.HashSet.)
        res  (reduce (fn [acc x]              
                       (let [uniques (first acc)
                             dupes   (second acc)
                             k (keyfn x)]
                         (if (.contains known k)
                           [uniques (assoc! dupes k (cons x (get dupes k nil)))]
                           (do (.add known k)
                               [(conj uniques x) dupes]))))
                     [[]  (transient {})]
                     xs)]
    [(first res) (persistent! (second res))]))

(defn valid-record? 
  ([r params] 
     (and (:Enabled r) 
          (core/in-scope? params (:SRC r))))
  ([r] (valid-record? r (core/get-parameters *ctx*))))

(defn demand-key 
  ([{:keys [SRC Vignette Operation Priority StartDay Duration]}]
     (demand-key SRC Vignette Operation Priority StartDay Duration))
  ([SRC Vignette Operation Priority StartDay Duration] 
     (clojure.string/join "" [Priority "_"  Vignette "_" SRC "["  StartDay "..."  (+ StartDay Duration) "]"])))

;;Could inline for speed, may be unnecessary...
(defn create-demand   
  "Produces a validated demand from the inputs.  We enforce invariants about 
   demanddata here to ensure that invalid values are caught and excepted."
  [DemandKey SRC  Priority StartDay Duration Overlap Category 
   SourceFirst Quantity  OITitle Vignette Operation  DemandGroup]  
  (let [empty-op  (core/empty-string? Operation)
        empty-vig (core/empty-string? Vignette)
        idx (if (or empty-op empty-vig) (core/next-idx) 0)]

    (d/->demanddata    ;unique name associated with the demand entity.
     (or DemandKey (demand-key SRC Vignette Operation Priority StartDay Duration)) 
     SRC ;demand-type, or other identifier of the capability demanded.
     Priority ;numerical value representing the relative fill priority.
     StartDay ;the day upon which the demand is activated and requiring fill.
     Duration ;the total time the demand is activated.
     Overlap  ;the demand-specific overlap requirement, if any
     Category ;descriptor for deployed unit behavior over-rides.
     SourceFirst  ;descriptor for supply preference. 
     Quantity  ;the total amount of entities required to fill the demand.
     OITitle   ;formerly OITitle.  Long-form description of the src capability.
     (if empty-vig (str "Anonymous" idx) Vignette)  ;Descriptor of the force list that generated this demand entity.
     (if empty-op  (str "Anonymous" idx) Operation) ;Fine-grained, unique description of the demand.
     DemandGroup ;Ket that associates a demand with other linked demands.  
     {} ;an ordered collection of all the fills recieved over time.                   
     {} ;map of the units currently associated with the demand.
     {} ;map of the units currently associated with this demand,
                                        ;that are not actively contributing toward filling the
                                        ;demand, due to a relief-in-place state.
      )))

(defn record->demand 
  "Basic io function for converting raw records to demanddata."
  [{:keys [DemandKey SRC  Priority StartDay Duration Overlap Category 
           SourceFirst Quantity  OITitle Vignette Operation  DemandGroup ] :as rec}]
  (create-demand DemandKey SRC  Priority StartDay Duration Overlap Category 
                 SourceFirst (if (pos? Quantity) Quantity 1) OITitle Vignette Operation  DemandGroup))

(comment ;testing
  (def demand-ctx (assoc-in *ctx* [:state :parameters :SRCs-In-Scope] {"SRC1" true "SRC2" true "SRC3" true}))
)


;;Returns a set of demand data, derived from recs, with 
;;duplicate records attached as meta data.
(defn demands-from-records [recs]  
  (let [params (core/get-parameters *ctx*)]
        (let [[uniques dupes]  (->> recs 
                                    (r/filter valid-record?)
                                    (partition-dupes demand)                                    
                                    )]
          (with-meta (mapv record->demand uniques)
            {:duplicates dupes}))))

;;Broadcast the fact that we have duplicates.
(defn notify-duplicate-demands! [dupes ctx]
  (let [name (:name (core/get-demandstore *ctx*))]
    (reduce (fn [ctx dup]
              (sim/trigger-event :Initialize name name 
                     (str "Demand " (:DemandKey dup) " had duplicates in source data.") nil ctx))
            ctx
            dupes)))

;broadcast that a demand with initialized.
(defn initialized-demand! [ctx d]
  (let [msg (str (str "Demand" (:name d)) "Initialized")]
    (sim/trigger-event :Intialize :DemandStore :DemandStore msg nil ctx)))
  

;;Updates the demand index according to the contents of 
;;demandmanager, providing unique names for duplicate 
;;demands.  Threads the demand through the context 
;;and returns the resulting context.
(defn associate-demand [ctx demand]
  (core/with-simstate [[parameters demandstore policystore] ctx]    
    (let [demands (:demandmap demandstore)
          demand-count (count demands)
          new-idx (+ (:demandstart parameters) demand-count)]
      (-> (if (contains? demands (:name demand))
            (assoc demand :name 
                   (str (:name demand) "_" (inc demand-count)))
            demand)                     
          (assoc :index new-idx)
          (demand/register-demand demandstore policystore ctx)))))

;;Do we need the vestigial demandstore args, or can we assume it's 
;;all in context?
(defn demands-from-table 
  "Read in multiple demand records from anything implementing a generic 
   table protocol in spork.util.table"
  [demand-table demandstore]
  (let [rs (demands-from-records (tbl/record-seq demand-table))
        dupes (get (meta rs) :duplicates)]
    (binding [*ctx* (assoc-in *ctx* [:state :demandstore] demandstore)]
      (->> (reduce (fn [ctx d] 
                     (initialized-demand! (associate-demand *ctx* d) d))
                   *ctx*
                   rs)
           (notify-duplicate-demands! dupes)))))


(defn ungrouped? [grp] 
  (when grp 
      (or (core/empty-string? grp) 
          (= (clojure.string/upper-case grp) "UNGROUPED"))))

;;create-unit provides a baseline, unattached unit derived from a set of data.
;;The unit is considered unattached because it is not registered with a supply "yet".  Thus, its parent is
;;nothing. parametrically create a new unit.

(defn create-unit [name src oititle component cycletime policy behavior policyobj]
  (u/->unitdata
   name ;unit entity's unique name. corresponds to a UIC 
   src ;unit entity's type, or capability it can supply.
   component ;unit entity's membership in supply.
   policy  ;the policy the entity is currently following.
   [] ;a stack of any pending policy changes.
   behavior ;the behavior the unit uses to interpret policy and messages.
   nil ;generic state data for the unit's finite state machine.
   cycletime ;the unit's current coordinate in lifecycle space.
   nil       ;description of the categories this unit serve as a followon to.
   :spawning ;the current physical location of the unit.
   :spawning ;the current position of the unit in its policy space.
    ;the current cycle data structure for the unit.
   [] ;an ordered collection of the cycles that the unit has completed.
   -1 ;the time in which the unit spawned.
   oititle ;the description of the unit.
   [] ;list of all the locations visited.
   0  ;dwell time before deployment
   ))

;;Temporary stub.
(defn choose-policy [pol compo policystore src]
  :default)

(defn assign-policy [unit policystore]
  (assoc unit :policy 
     (choose-policy (:policy unit) (:component unit) policystore (:src unit))))

;;Derives a default unit from a record that describes unitdata.
;;Vestigial policy objects and behavior fields are not defined.  We
;;may allow different behaviors in the future, but for now they are
;;determined at runtime via the legacy processes (by component).
(defn record->unitdata [{:keys [Name SRC OITitle Component CycleTime Policy]}]
  (create-unit Name SRC OITitle Component CycleTime Policy :default nil))

 ;;  92:   Public Sub unitsFromTable(table As GenericTable, supply As TimeStep_ManagerOfSupply)
 ;;  93:  
 ;;  94:   Dim rec
 ;;  95:   Dim unit As TimeStep_UnitData
 ;;  96:   Dim defbehavior As IUnitBehavior
 ;;  97:   Dim count As Long, quantity As Long
 ;;  98:   Dim policy As IRotationPolicy
 ;;  99:  
 ;; 100:   table.moveFirst
 ;; 101:   While Not table.EOF
 ;; 102:       Set record = table.getGenericRecord
 ;; 103:       With record
 ;; 104:           If .fields("Enabled") = True And inScope(.fields("SRC")) Then
 ;; 105:               quantity = Fix(.fields("Quantity"))
 ;; 106:              'Decouple
 ;; 107:               Set policy = choosepolicy(.fields("Policy"), .fields("Component"), state.policystore, .fields("SRC"))
 ;; 108:               If quantity > 1 Then
 ;; 109:                   AddUnits quantity, .fields("SRC"), .fields("OITitle"), _
 ;; 110:                               .fields("Component"), policy, supply
 ;; 111:               ElseIf quantity = 1 Then'unique unit record
 ;; 112:                  'Tom change 19 April 2012
 ;; 113:  
 ;; 114:                   Set unit = associateUnit(recordToUnit(record), supply)
 ;; 115:                   supply.registerUnit unit, unit.src = "Ghost"'Tom change 1 April, this takes care of all the registration, refactored.
 ;; 116:                   Set unit = Nothing
 ;; 117:               End If
 ;; 118:           End If
 ;; 119:       End With
 ;; 120:       
 ;; 121:       table.moveNext
 ;; 122:   Wend
 ;; 123:   End Sub

;;Need to add filters to ensure integrality constraints on supply.
;;Also need to add 
(defn units-from-records [recs supply]
  (let [params (core/get-parameters *ctx*)]
    (->> recs 
         (r/filter valid-record?)    
         (r/map record->unitdata)
         (into []))))

(definline generate-name 
  "Generates a conventional name for a unit, given an index."
  [idx unit]
  `(str ~idx "_" (:SRC ~unit) "_" (:component ~unit)))

(defn check-name 
  "Ensures the unit is uniquely named, unless non-strictness rules are 
   applied."
  [name supply strictname]  
    (if (supply/get-unit name supply)
      (if strictname 
        (throw (Error. (str  "A unit already exists with the name " 
                             name " in SupplyManager " (:name supply)  
                             ", unit names must be unique")))
        (str name (count (:unit-map supply))))
      name))
        
;;This does not actually attach the unit inside the supplystore, only 
;;preps the unit's name.  It is associated, in the legacy sense that
;;the unit would gain a "pointer" to the parent supply, as well as
;;having its name preconditioned to ensure compatibility or catch 
;;errors.  In the newer version, we may consider composing this with 
;;marathon.sim.supply/register-unit , which actually attaches the 
;;unit to the supply store.  Note: we could handle parent-child 
;;assocation there as well...The unique functionality here is that 
;;we are either generating or altering the name of the unit to ensure 
;;it is unique in the supply.
(defn associate-unit   
  "Associate or attach a new unit to a particular supply. If the new unit's name is 
  not unique, it will be changed to accomodate uniqueness requirement."
  [unit supply strictname]
  (let [unit-count (count (:unitmap supply))
        nm         (if (= (clojure.string/upper-case nm) "AUTO")
                     (generate-name unit-count unit)
                     (check-name nm supply strictname))]
    (-> unit (assoc :name nm) 
        (assoc :index unit-count) 
        (initialize-cycle (:policy unit) 
                          (core/ghost? unit)))))

;;Note -  register-unit is the other primary thing here.  It currently 
;;resides in marathon.sim.supply/register-unit 



;;THis is just a stub.  We currently hardwire behaviors, 
;;but will allow more extension in the future.  The legacy 
;;behavior system required an inherited object model implementation, 
;;but now we don't have to worry about that.  We'll probably just 
;;have a map of functions, or types that can fulfill the unit behavior
;;protocol.
(defn get-default-behavior [supply] :default)
;;Adds multiple units according to a template.
(defn add-units [amount src oititle compo policy supply extratags ghost]
  
  

 ;; 435:   Public Function AddUnits(amount As Long, src As String, OITitle As String, _
 ;; 436:                               compo As String, policy As IRotationPolicy, _
 ;; 437:                                       supply As TimeStep_ManagerOfSupply, _
 ;; 438:                                               Optional extratags As Dictionary, _
 ;; 439:                                                   Optional ghost As Boolean) As Dictionary
 ;; 440:   Dim i As Long
 ;; 441:   Dim NewUnit As TimeStep_UnitData
 ;; 442:   Dim generated As Dictionary
 ;; 443:   Dim defbehavior As IUnitBehavior
 ;; 444:   Dim DeployableBuckets As Dictionary
 ;; 445:   Dim count As Long
 ;; 446:   Dim nm
 ;; 447:  
 ;; 448:  'TOM Note -> these need to be decoupled, they're dependent on supply...
 ;; 449:   If Not ghost Then
 ;; 450:       Set defbehavior = supply.behaviors.defaultACBehavior
 ;; 451:   Else
 ;; 452:       Set defbehavior = supply.behaviors.defaultGhostBehavior
 ;; 453:   End If
 ;; 454:  
 ;; 455:   Set DeployableBuckets = supply.DeployableBuckets
 ;; 456:  
 ;; 457:   Set AddUnits = supply.unitmap'Note this is in here for legacy reasons.  Return value is used.
 ;; 458:  
 ;; 459:   If amount > 0 Then
 ;; 460:       Set generated = New Dictionary
 ;; 461:       count = supply.unitmap.count + 1
 ;; 462:       For i = 1 To Fix(amount)
 ;; 463:           Set NewUnit = New TimeStep_UnitData
 ;; 464:           With NewUnit
 ;; 465:               Set .behavior = defbehavior
 ;; 466:              'Decouple
 ;; 467:               Set .parent = supply
 ;; 468:               .src = src
 ;; 469:               .OITitle = OITitle
 ;; 470:               .component = compo
 ;; 471:               .name = count & "_" & .src & "_" & .component
 ;; 472:               .index = count
 ;; 473:              'Tom Change 17 June 2011 -> reflects new function signature.
 ;; 474:              'initialize_cycle NewUnit, policy, supply, ghost
 ;; 475:               
 ;; 476:              'I don't think we need this.
 ;; 477:              'TOM Change 18 Sep 2012
 ;; 478:              'initialize_cycle NewUnit, policy, ghost
 ;; 479:               If ghost Then
 ;; 480:                   .PositionPolicy = .policy.getPosition(.cycletime)'Tom Change 20 May 2011
 ;; 481:                   .changeLocation .PositionPolicy, state.context
 ;; 482:                  '.LocationName = .PositionPolicy 'Tom Change 20 May 2011
 ;; 483:                  'Decouple
 ;; 484:                   .location = state.policystore.locationID(.LocationName)
 ;; 485:               End If
 ;; 486:           End With
 ;; 487:           
 ;; 488:           
 ;; 489:           generated.add NewUnit.name, NewUnit
 ;; 490:  
 ;; 491:           Set NewUnit = Nothing
 ;; 492:           count = count + 1
 ;; 493:  
 ;; 494:       Next i
 ;; 495:       
 ;; 496:       If Not ghost Then distributeCycleTimeLocations generated, policy, supply
 ;; 497:       
 ;; 498:       For Each nm In generated
 ;; 499:  '        supply.registerUnit generated(nm)  'this handles all the registration items, refactoring.
 ;; 500:           MarathonOpSupply.registerUnit state.supplystore, state.behaviormanager, generated(nm), ghost, state.context
 ;; 501:       Next nm
 ;; 502:   End If
 ;; 503:  
 ;; 504:   End Function


;Given a unit, initialize it's currentcycle based on policy information

 ;; 336:   Private Sub initialize_cycle(unit As TimeStep_UnitData, policy As IRotationPolicy, Optional ghost As Boolean)
 ;; 337:   Set unit.policy = policy
 ;; 338:   policy.subscribe unit
 ;; 339:  
 ;; 340:   With unit
 ;; 341:       If ghost = False Then
 ;; 342:           .PositionPolicy = policy.getPosition(.cycletime) 'TOM Change 20 May
 ;; 343:           .LocationName = "Spawning"
 ;; 344:           .changeLocation .PositionPolicy, state.context
 ;; 345:          '.LocationName = .PositionPolicy 'TOM Change 20 May
 ;; 346:          'Tom Change 17 June 2011 -> removed, vestigial, don't need supply manager anymore.
 ;; 347:          '.location = supply.parent.policymanager.locationID(.LocationName)
 ;; 348:       Else
 ;; 349:           .PositionPolicy = "Spawning"'TOM Change 20 May
 ;; 350:           .LocationName = "Spawning"
 ;; 351:           .changeLocation .PositionPolicy, state.context'TOM Change 20 May
 ;; 352:          'Tom Change 17 June 2011 -> removed, vestigial, don't need supply manager anymore.
 ;; 353:          '.location = supply.parent.policymanager.locationID(.LocationName)
 ;; 354:       End If
 ;; 355:   End With
 ;; 356:  
 ;; 357:   End Sub

;;consider changing these to keywords.
;;We can probably push this off to a policy default table.
(def ^:constant +policy-defaults+ 
  {"Ghost" {:special "SpecialGhostPolicy"
            :default "DefaultGhostPolicy"}
   "AC"    {:special "SpecialACPolicy"
            :default "DefaultACPolicy"}          
   "RC"    {:special "SpecialRCPolicy"
            :default "DefaultRCPolicy"}})

(defn choose-policy 
  "Tries to fetch an associated policy, and upon failing, selects a 
   default policy based on the component/src and whether the SRC is 
   tagged as being special."
  [policyname component policystore src]
  (core/with-simstate [[parameters] *ctx*]
    (let [policy-type  (if (or (core/empty-string? src) 
                               (not (core/special-src? (:tags parameters) src)))
                          :default
                          :special)]
      (if-let [p (get-in policystore [:policies policyname])]
        p 
        (if-let [res (get-in +policy-defaults+ [component policy-type])]
           res
           (throw (Exception. (str "Default Policy is set at "  
                                   policyname  " which does not exist!"))))))))

 ;; 181:   Public Function choosepolicy(ByRef policyname As String, ByRef component As String, _
 ;; 182:                                   policymanager As TimeStep_ManagerOfPolicy, Optional ByRef src As String) As IRotationPolicy
 ;; 183:  
 ;; 184:   Static nm As String
 ;; 185:  
 ;; 186:   Dim specialPolicy As Boolean
 ;; 187:  
 ;; 188:   If src = vbNullString Then
 ;; 189:       specialPolicy = False
 ;; 190:   Else
 ;; 191:      'Decouple
 ;; 192:       specialPolicy = state.parameters.SRCHasTag(src, "Special")
 ;; 193:   End If
 ;; 194:  
 ;; 195:       
 ;; 196:   With policymanager
 ;; 197:       If .policies.exists(policyname) Then
 ;; 198:           Set choosepolicy = .policies(policyname)
 ;; 199:       Else
 ;; 200:           If Not specialPolicy Then
 ;; 201:               If component = "Ghost" Then
 ;; 202:                  'Decouple
 ;; 203:                   nm = state.parameters.getKey("DefaultGhostPolicy")
 ;; 204:               ElseIf component = "AC" Then
 ;; 205:                  'Decouple
 ;; 206:                   nm = state.parameters.getKey("DefaultACPolicy")
 ;; 207:               Else
 ;; 208:                  'Decouple
 ;; 209:                   If state.parameters.getKey("TAA1519MaxUtilizationHack") Then
 ;; 210:                       nm = "MaxUtilization_Enabler"
 ;; 211:                   Else
 ;; 212:                       nm = state.parameters.getKey("DefaultRCPolicy")
 ;; 213:                   End If
 ;; 214:               End If



 ;; 215:           Else
 ;; 216:               If component = "Ghost" Then
 ;; 217:                  'Decouple
 ;; 218:                   nm = state.parameters.getKey("SpecialGhostPolicy")
 ;; 219:               ElseIf component = "AC" Then
 ;; 220:                  'Decouple
 ;; 221:                   nm = state.parameters.getKey("SpecialACPolicy")
 ;; 222:               Else
 ;; 223:                  'Decouple
 ;; 224:                   If state.parameters.getKey("TAA1519MaxUtilizationHack") Then
 ;; 225:                       nm = "MaxUtilization"
 ;; 226:                   Else
 ;; 227:                       nm = state.parameters.getKey("SpecialRCPolicy")
 ;; 228:                   End If
 ;; 229:               End If
 ;; 230:           End If


 ;; 232:           If .policies.exists(nm) Then
 ;; 233:               Set choosepolicy = .policies(nm)
 ;; 234:           Else
 ;; 235:               Err.Raise 101, , "Default Policy is set at " & policyname & " which does not exist!"
 ;; 236:           End If
 ;; 237:       End If
 ;; 238:   End With
 ;; 239:  
 ;; 240:   End Function
