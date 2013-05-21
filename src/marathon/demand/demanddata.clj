;A container and associated functions necessary for managing demand entities.
(ns marathon.demand.demanddata
  (:use [util.record]))

;demanddata is the basic information required to represent demand entities.
(defrecord+ demanddata 
  [name ;unique name associated with the demand entity.
   src ;demand-type, or other identifier of the capability demanded.
   priority ;numerical value representing the relative fill priority.
   startday ;the day upon which the demand is activated and requiring fill.
   duration ;the total time the demand is activated.
   overlap  ;the demand-specific overlap requirement, if any
   [category :rotational] ;descriptor for deployed unit behavior over-rides.
   [source-first :uniform]  ;descriptor for supply preference. 
   [quantity 0] ;the total amount of entities required to fill the demand.
   title    ;formerly OITitle.  Long-form description of the src capability.
   vignette  ;Descriptor of the force list that generated this demand entity.
   operation ;Fine-grained, unique description of the demand.
   demandgroup ;Ket that associates a demand with other linked demands.  
   [fills {}] ;an ordered collection of all the fills recieved over time.                   
   [units-assigned {}] ;map of the units currently associated with the demand.
   [units-overlapping {}]]);map of the units currently associated with this demand, 
                           ;that are not actively contributing toward filling the 
                           ;demand, due to a relief-in-place state.
                       
(def empty-demand (make-demanddata))

;'How many units do we still need here?
;Public Function required() As Long
;required = quantity - units-assigned.count
;End Function

(defn required [d] (- (:quantity d) (-> d :units-assigned count)))

;'TOM Change 14 Mar 2011
;'propose storing the quality of the fill using the fill path.  That way, we can dissect the fill path
;'to determine anything else we want....use the fillgraph to get pathlength, if length meets a certain
;'threshold, then it's a sub, etc.
;Public Sub Assign(unit As TimeStep_UnitData, Optional quality As String)

;If units-assigned.exists(unit.name) = False Then
;    units-assigned.add unit.name, unit
;Else
;    Err.Raise 101, , "unit already assigned"
;End If
;End Sub

(defn assign [d u] (merge d {:units-assigned 
                             (assoc (:units-assigned d) (:name u) u)}))
(defn assign-many [d us] (reduce assign d us)) 

(defn assigned? [d u]
  (contains? (:units-assigned d) (:name u)))

(defn overlapping? [d u]
  (contains? (:units-overlapping d) (:name u)))

(defn has-unit? [d u]
  (or (assigned? d u) (overlapping? d u)))

(defn list-units [d] 
  (keys (:units-assigned d)))

;'TOM Change 14 Mar 2011
;Public Sub SendHome(unit As TimeStep_UnitData)

;If units-assigned.exists(unit.name) = False And units-overlapping.exists(unit.name) = False Then
;    Err.Raise 101, , "unit not assigned"
;Else
;    If units-assigned.exists(unit.name) Then units-assigned.remove unit.name
;    If units-overlapping.exists(unit.name) Then units-overlapping.remove unit.name
;End If

;End Sub

(defn send-home [d u]
  (if (has-unit? d u)
    (let [flds [:units-assigned :units-overlapping]
          ms (map #(hash-map % (dissoc (get d %) (:name u))) flds)]
       (apply merge d ms))
    d))
          

;Public Sub SendOverlap(unit As TimeStep_UnitData)
;
;If units-overlapping.exists(unit.name) = True Then
;    Err.Raise 101, , "unit already overlapping"
;Else
;    units-assigned.remove unit.name
;    units-overlapping.add unit.name, unit.name
;End If
;
;End Sub

(defn send-overlap [d u]
  (if (has-unit? d u)
    (transfer (:units-assigned d) (:name u) (:units-overlapping d))
    d))

;Public Function unitCount() As Long
;unitCount = units-overlapping.count + units-assigned.count
;End Function
(defn unit-count [d] (reduce + ((juxt :units-assigned :units-overlapping) d)))

;Public Sub fillWith(fill As TimeStep_Fill)
;Assign fill.source, fill.quality
;Fills.add fill.source.name, fill
;End Sub

(defn fill-with [d fill]
  (assoc d :fills 
     (conj (:fills d) 
           (name (:fill-source fill)))))  
  
