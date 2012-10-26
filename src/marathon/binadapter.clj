(ns marathon.binadapter
  (:require [marathon [importing :as importing]]))

;these are the source bins that Megan's data has. Assumably, they're not 
;going to change that much.
(def sourcebins #{"Mission" "Progressive" "Op Sust" "Rotational"}) 

;a template for the binning data - Megan's supply data with bin metadata.
;we can use her stuff directly.  We're just remapping fields and adding 
;new fields. 

(def supply-fields ["SRC" "AC" "RC" "STR" "AC STR" "RC STR" "Pool"])

;we need to pre-process Megan's stuff...since she broke out the Component...
;Component = {AC, ARNG, USAR} 
;similarly STR = [AC STR | RC STR]
(defn BAFrec->supplyrecs 
  "Re-map one of Megan's existing supply records to a new record format.
   STR is dropped, AC and RC Are combined into a Component Field, and 
   AC STR and RC STR are combined into a STR field.  As a consequence, 
   we get 2 records for every input."
  [inrec]
  (let [ACFields ["AC" "AC STR"]
        RCFields ["RC" "RC STR"]
        compofield "Component"
        baserec (reduce #(dissoc %1 %2) inrec (concat ACFields RCFields))]
    (map #(into baserec %) 
         [[[compofield "AC"] 
           ["STR" (get inrec "AC STR")]]
          [[compofield "RC"] 
           ["STR" (get inrec "RC STR")]]])))

(defn BAFsupply->supply
  "Squeeze the AC/RC specific raw supply records into a new table with 
   Component and STR."
  [inrecs] 
  (mapcat BAFrec->supplyrecs inrecs))
 
;compose all this stuff into a common bridge...
(importing/defbridge BAF-supply-bridge  
  [["SRC"] 
   ["STR"]
   ["Component"]
   ["Pool"]])

;(defbridge BAF-supply-bridge
;  [["SRC"]
;   [:group ["AC" "RC"] :as "Component"]
;   [:group ["AC STR" "RC STR"] :as "STR" :by :sum]
;   ["Pool"]])
 

;define a means for bridging demand.
;Demand is easier, since it's flat (like any good table....).
(importing/defbridge BAF-demand-bridge 
  [["QTY" "Quantity" 0]
   ["Start" "StartDay" 0]
   ["Duration" "Duration" 0]
   ["TAA SRC"  "SRC"]
   ["Mission" "DemandGroup"]
   ["Operation" "Vignette"]
   ["COA" "Category"]
   ["Category" "Bin" "Rotational"]])

(comment 

;Supply Testing
(defn make-supplyrec
  "A simple testing function for building Megan data."
  [src qty str split]
  (let [ac (* split qty) 
        rc (* (- 1 split) qty)]
  {"SRC" src 
   "AC" ac
   "RC" rc
   "STR" (* qty str)
   "AC STR" (* str ac)
   "RC STR" (* str rc)}))

;a proxy for megan's data.
(def samplesupply 
  {:fields supply-fields 
   :records (->> [["BILLSHATNER" 10 2 0.75]
                  ["SOMESRC" 20 5 0.5]
                  ["ATHIRDSRC" 55 10 1]]
              (map #(apply make-supplyrec %))
              (map vals))})

(def sampletable
  (let [fs (:fields samplesupply)]
	  (for [r (:records samplesupply)]
	       (into {} (map vector fs r)))))

;process the sampletable (a Megan conformant proxy), returning 
;an collapsed table with 2x the records.
(def supply (BAFsupply->supply sampletable))

;Demand Testing 
(defn make-demandrec 
  "A simple testing function for building Megan data."
  [qty start duration src mission coa category]
  {"QTY" qty 
   "Start" start 
   "Duration" duration
   "TAA SRC" src 
   "Mission" mission 
   "COA" coa
   "Category" category})
  
(def demand (map #(apply make-demandrec %)
                  [[10 2 3 "BILLSHATNER" "Treking"  "COA1" "Rotational"]
                   [2 10 3 "SOMESRC"     "Resting"  "COA2" "Op Sust"]
                   [3 2 10 "ATHIRDSRC"   "Spinning" "COA3" "Mission"]]))




;bridge the supply into something marathon can eat natively. 
(def bridged-supply 
  ;(let [bridge (importing/make-bridge supply-template)]
  (map (partial importing/translate-map BAF-supply-bridge) supply))

(def bridged-demand 
  (map (partial importing/translate-map BAF-demand-bridge) demand))
)




     