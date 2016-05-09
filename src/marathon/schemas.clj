;;Collection of common schemas for Marathon data.
;;Also a place to document the canonical fields and
;;convey the semantics of the data.
(ns marathon.schemas
  (:require [spork.util [parsing :as p]
                        [table :as tbl]
                        [general :as g]]))

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
            
;;Various schemas for representing marathon related input data.
;;We'll add output here as well.
;;Where field types are not annotated, the inferred type is :text .

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
     :BOGBudget
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
     [:Enabled :boolean]
     [:Quantity :int]
     :SRC
     :Component
     :OITitle
     :Name
     :Behavior
     [:CycleTime :int]
     :Policy
     [:Tags :clojure]
     [:SpawnTime :int]
     :Location
     :Position ;;Starting state for policy (typically SRM-specific....)
     [:Original :boolean]
      ;;Added 4 new fields to accomodate requirements for SRM
     :Command ;;command relationship, if any...
     :Origin     ;;supply relationship, if any...
     [:Duration :int?] ;;Duration remaining in StartState...
     ]
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
     :Category
     [:Priority :int]
     ;;Added for SRM ;;need to allow these to parse loosely...
     :Command
     :Location
     :DemandType
     :Theater
     [:BOG :boolean]
     :StartState
     :EndState
     [:MissionLength :int?]]
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
     :Value]
   :SuitabilityRecords
    [:Suitability 
     :Definition]
   :demand-table-schema ;;added for legacy purposes.
   [:Type
    :Enabled
    [:Priority :float]
    [:Quantity :float]
    [:DemandIndex :float]
    [:StartDay :float]
    [:Duration :float]
    [:Overlap  :float]
    :SRC
    :SourceFirst
    :DemandGroup
    :Vignette
    :Operation
    :Category
    :OITitle
    :case-name
    :case-future
    :DependencyClass
    :DemandSplit
    :Include
    :draw-index
    :Group
    :DemandType
    "Title 10_32"]})

(def known-schemas 
  (reduce-kv (fn [acc name fields]
               (merge acc  (schema name fields)))
             {}
             marathon-schemas))

(defn get-schema [nm] (get known-schemas nm))
;;look for like-named columns, if not found, returns nil
;;for columns in the schema.  Basically pad empty values...

;;Allows us to quickly read tab-delimited txt files 
;;using a named schema.
(defn read-schema  
  "Given a known schema, looks up the schema and uses it 
   to read txt.  Currently reads tab-delimited text files, 
   returning a table parsed via the appropriate schema."
  [name txt]
  (if-let [schm (get-schema name)]
    (tbl/tabdelimited->table txt :schema schm)
    (throw (Exception. (str "Unknown schema " name)))))
    
