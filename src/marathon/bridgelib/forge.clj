(ns marathon.bridgelib.forge
  (:require [util [bridging :as bridging]]))

(def forgetemplate 
  [["Type" "Type" "DemandRecord"]
   ["Enabled" "Enabled" "True"]
   ["Priority" "Priority" 0]
   [:quantity "Quantity"]
   ["DemandIndex" "DemandIndex" 0]
   [:start "StartDay"]
   [:duration "Duration"]
   ["Overlap" "Overlap" 45]
   ["SRC"]
   ["SourceFirst" "SourceFirst" "Uniform"]
   [:group "DemandGroup"]
   [:vignette "Vignette"]
   [:operation "Operation"]
   ["ARFORGEN" "Category"]])

(def sample (clojure.core/merge 
              {"SRC" "SRCBLAH"
               "ARFORGEN" "Roto"}
              
              {:phase "A" 
	             :start 10 
	             :duration 10
               :vignette "V1"
               :operation "O1"
               :group "G1"}
              
	            {:quantity 200}))
              