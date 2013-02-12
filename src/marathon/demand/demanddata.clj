(ns marathon.demand.demanddata
  (:use [util.record]
        [DEVS.Entity]))


(defrecord demanddata [name src priority startday duration overlap primaryunit 
                       sourcefirst quantity OITitle vignette operation
                       demandgroup index fills unitsAssigned unitsOverlapping])
                       
(def emptydemand
  (-> (empty-record Marathon.demand.demanddata)
  (assoc-many 0 [:priority :startday :duration :overlap :quantity])
  (merge {:fills {} :unitsOverlapping {} :unitsAssigned {}})))

;'How many units do we still need here?
;Public Function required() As Long
;required = quantity - unitsAssigned.count
;End Function

(defn required [d] (- (:quantity d) (-> d :unitsAssigned count)))

;'TOM Change 14 Mar 2011
;'propose storing the quality of the fill using the fill path.  That way, we can dissect the fill path
;'to determine anything else we want....use the fillgraph to get pathlength, if length meets a certain
;'threshold, then it's a sub, etc.
;Public Sub Assign(unit As TimeStep_UnitData, Optional quality As String)

;If unitsAssigned.exists(unit.name) = False Then
;    unitsAssigned.add unit.name, unit
;Else
;    Err.Raise 101, , "unit already assigned"
;End If
;End Sub

(defn assign [d u] (merge d {:unitsAssigned 
                             (assoc (:unitsAssigned d) (:name u) u)}))
(defn assign-many [d us] (reduce assign d us)) 

(defn assigned? [d u]
  (contains? (:unitsAssigned d) (:name u)))

(defn overlapping? [d u]
  (contains? (:unitsOverlapping d) (:name u)))

(defn has-unit? [d u]
  (or (assigned? d u) (overlapping? d u)))

(defn list-units [d] 
  (keys (:unitsAssigned d)))

;'TOM Change 14 Mar 2011
;Public Sub SendHome(unit As TimeStep_UnitData)

;If unitsAssigned.exists(unit.name) = False And unitsOverlapping.exists(unit.name) = False Then
;    Err.Raise 101, , "unit not assigned"
;Else
;    If unitsAssigned.exists(unit.name) Then unitsAssigned.remove unit.name
;    If unitsOverlapping.exists(unit.name) Then unitsOverlapping.remove unit.name
;End If

;End Sub

(defn send-home [d u]
  (if (has-unit? d u)
    (let [flds [:unitsAssigned :unitsOverlapping]
          ms (map #(hash-map % (dissoc (get d %) (:name u))) flds)]
       (apply merge d ms))
    d))
          

;Public Sub SendOverlap(unit As TimeStep_UnitData)
;
;If unitsOverlapping.exists(unit.name) = True Then
;    Err.Raise 101, , "unit already overlapping"
;Else
;    unitsAssigned.remove unit.name
;    unitsOverlapping.add unit.name, unit.name
;End If
;
;End Sub

(defn send-overlap [d u]
  (if (has-unit? d u)
    (transfer (:unitsAssigned d) (:name u) (:unitsOverlapping d))
    d))

;Public Function unitCount() As Long
;unitCount = unitsOverlapping.count + unitsAssigned.count
;End Function
(defn unit-count [d] (reduce + ((juxt :unitsAssigned :unitsOverlapping) d)))

;Public Sub fillWith(fill As TimeStep_Fill)
;Assign fill.source, fill.quality
;Fills.add fill.source.name, fill
;End Sub

(defn fill-with [d fill]
  (assoc d :fills 
     (conj (:fills d) 
           (name (fill-source fill)))))  
  
