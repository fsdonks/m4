;;Collection of common schemas for Marathon data.
(ns marathon.schemas
  (:require [spork.util [parsing :as p]]))

(defn schema [name field-defs]
  {(keyword name)
   (reduce (fn [acc f]
               (if (not (coll? f))
                 (-> acc (assoc f :text))
                 (-> acc (assoc (first f) (second f)))))
             {}
             field-defs)})
(defn schemas [& xs]
  (reduce merge (for [[n fields] xs] 
                  (schema n fields))))
            

(def marathon-schemas    
   {:PolicyTemplates 
    [[:Behavior  :keyword]
     :Name      
     [:Quantity  :int]
     :Component 
     :SRC 
     :Type 
     [:CycleTime :int]
     :Location 
     :OITitle 
     :Tags  
     :Policy 
     [:Enabled :boolean]
     :Position 
     [:SpawnTime :int]
     [:Original :boolean]]
   :PolicyRecords  
    [:TimeStamp
     :Type
     :TimeInterval
     :PolicyName
     :Template
     [:MaxDwell :int]
     [:MinDwell :int]
     [:MaxBOG   :int]
     [:StartDeployable :int]
     [:StopDeployable :int]
     [:Overlap :int]
     [:Recovery :int]
     [:BOGBudget :int]
     :Deltas
     :Remarks]
   :CompositePolicyRecords 
    [:Type 
     :CompositeName 
     :CompositionType 
     :Period 
     :Policy]
   :PolicyDefs  
    [:CompositeName	
     [:Composition :clojure]]
   :SupplyRecords 
    [:Type
     :Enabled
     [:Quantity :int]
     :SRC
     :Component
     :OITitle
     :Name
     :Behavior
     [:CycleTime :int]
     :Policy
     [:Tags :clojure]
     [:SpawnTime int]
     :Location
     :Position
     [:Original :boolean]]
   :SRCTagRecords  
    [:Type :SRC :Tag]
   :DemandRecords  
    [:Type
     [:Enabled :boolean]
     [:Priority :int]
     [:Quantity :int]
     [:DemandIndex :int]
     [:StartDay :int]
     [:Duration :int]
     [:Overlap :int]
     :SRC
     :SourceFirst
     :DemandGroup
     :Vignette
     :Operation
     :Category]
   :PeriodRecords
    [:Type 
     :Name 
     [:FromDay :int]
     [:ToDay :int]]
   :RelationRecords 
    [:Type 
     :Relation 
     :Donor 
     :Recepient 
     [:Cost :float]
     [:Enabled :boolean]]
   :Parameters 
    [:ParameterName 
     :Value]})

(def known-schemas 
  (reduce-kv (fn [acc name fields]
               (merge acc  (schema name fields)))
             {}
             marathon-schemas))
             
    
