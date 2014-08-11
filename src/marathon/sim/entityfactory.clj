
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

(defn record->unitdata [])

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

(defn choose-policy [pol compo policystore src]
  :default)

(defn assign-policy [unit policystore]
  (assoc unit :policy 
     (choose-policy (:policy unit) (:component unit) policystore (:src unit))))


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


 ;; 126:   Public Function recordToUnit(inrec As GenericRecord) As TimeStep_UnitData
 ;; 127:  
 ;; 128:   With inrec
 ;; 129:       Set recordToUnit = createUnit(.fields("Name"), .fields("SRC"), .fields("OITitle"), _
 ;; 130:                                     .fields("Component"), .fields("CycleTime"), .fields("Policy"))
 ;; 131:   End With
 ;; 132:  
 ;; 133:   End Function


 ;; 159:   Public Function associateUnit(unit As TimeStep_UnitData, supply As TimeStep_ManagerOfSupply, Optional StrictName As Boolean) As TimeStep_UnitData
 ;; 160:  
 ;; 161:   Dim count As Long
 ;; 162:   count = supply.unitmap.count + 1
 ;; 163:  
 ;; 164:  
 ;; 165:   With unit
 ;; 166:      'Decouple
 ;; 167:       Set .parent = supply
 ;; 168:       If .name = "Auto" Then
 ;; 169:           .name = count & "_" & .src & "_" & .component
 ;; 170:       ElseIf supply.unitmap.exists(.name) Then
 ;; 171:           If StrictName Then Err.Raise 101, , "A unit already exists with the name " _
 ;; 172:                               & .name & " in SupplyManager " & supply.name & "unit names must be unique"
 ;; 173:           .name = .name & "_" & count
 ;; 174:       End If
 ;; 175:       .index = count
 ;; 176:       initialize_cycle unit, .policy, .src = "Ghost"
 ;; 177:   End With
 ;; 178:  
 ;; 179:   Set associateUnit = unit
 ;; 180:   End Function
