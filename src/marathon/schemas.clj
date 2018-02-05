;Collection of common schemas for Marathon data.
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

;;we need a way to define legacy schemas, possibly
;;versioned schemas.
;;That is, we try to match a schema based on the fields
;;available.  Perhaps we specify the schema class, and
;;derive the version?

;;We could use clojure.spec for our schemas.  Future adaptation.
;;Another option is using clojure records.

;;Right now, we have two different versions of supply
;;and demand records.  So we can conceivably define
;;a simple function to choose from one or more schemas.
;;Perhaps we have a base schema.
;;  If the base is satisfied, we see if any of the children
;;  are.
;;  Child schemas extend the base.

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
     [:Enabled     :boolean]
     [:Priority    :int]
     [:Quantity    :int]
     [:DemandIndex :int]
     [:StartDay    :int]
     [:Duration    :int]
     [:Overlap     :int]
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
    #_"Title 10_32"]
    :GhostProportionsAggregate
    [:Type
     [:Enabled :boolean]
     [:SRC :float]
     [:AC  :float]
     [:NG  :float]
     [:RC  :float]
     ]
    })

;;optional helper function.
;;Allows us to print the schemas in a format readable
;;by beamer and other presntation stuff.
(defn print-schema
  ([schema field-spec]
    (str "- " (name schema) \newline
         (clojure.string/join \newline
             (for [fld field-spec]
                (let [fname (if (vector? fld) (name (first fld)) (name fld))
                      ftype (if (vector? fld) (second fld) :text)
                      fdoc  (if (vector? fld) (get fld 2 "") "")]
                  (format "  - %s %s %s" fname ftype fdoc))))         
         ))
  ([v] (print-schema (first v) (second v))))

(def known-schemas 
  (reduce-kv (fn [acc name fields]
               (merge acc  (schema name fields)))
             {}
             marathon-schemas))

(def flds
  #{:Compo
    :Location
    :End
    :Quantity
    :Unitid
    :Fill-type
    :Duration
    :Start
    :Operation
    :SRC
    :Category ;?
    :category ;?
    :Unit
    :DemandGroup
    :FillType
    :FollowOn
    :Component
    :DeploymentID
    :DwellYearsBeforeDeploy
    :DeployDate
    :FollowonCount
    :AtomicPolicy
    :DeployInterval
    :Fill-path
    :Period
    :Demand
    :Pathlength
    :Oititle
    :BogBudget
    :Cycletime
    :Deploymentcount
    :Demandtype
    :Fillcount

   ; :Location
    :Dwellbeforedeploy
   ; :Policy
    :Sampled
    :Dwell-plot?
    :Deltat})

;;According to Craig, probably not used
(def non-proc
  #{:Policy
    :Fillcount
    :Pathlength
    :Fillpath
    :Name ;;unit_SRC_Compo ....
    :AtomicPolicy
    :FollowonCount
    :Deploydate
    :deploymentID
    :category
    :Category
    :Cycletime
    :BogBudget
    :DeploymentCount
    :FillCount
    ;:Policy
    :Location})

(def fillrecord {:Unit      :text
                 :category :text
                 :DemandGroup :text
                 :SRC :text
                 :FillType :text
                 :FollowOn :boolean
                 :name :text
                 :Component :text
                 :operation :text
                 :start :int
                 :DeploymentID :int
                 :duration :int
                 :dwell-plot? :boolean
                 :DwellYearsBeforeDeploy :float
                 :DeployDate :text
                 :FollowOnCount :int
                 :AtomicPolicy :text
                 :Category :text
                 :DeployInterval :int
                 :fill-type :text
                 :FillPath :text
                 :Period :text
                 :unitid :int
                 :deltat :int
                 :Demand :text
                 :PathLength :int
                 :OITitle :text
                 :BogBudget :int
                 :CycleTime :int
                 :DeploymentCount :int
                 :DemandType :text
                 :quantity :int
                 :end :int
                 :FillCount :int
                 :Location :text
                 :location :text
                 :compo :text     
                 :DwellBeforeDeploy :int
                 :Policy :text
                 :sampled :boolean
                 })


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
    

;;Field Order
;;===========
;;some useful stuff for writing ordered records (i.e. output)
(def deployment-fields
  [:DeploymentID ;;INCONSISTENT: This data doesn't exist in unitdata at the moment.
   :Location
   :Demand
   :DwellBeforeDeploy
   :BogBudget
   :CycleTime
   :DeployInterval
   :DeployDate
   :FillType
   :FillCount
   :UnitType
   :DemandType
   :DemandGroup
   :Unit
   :Policy
   :AtomicPolicy
   :Component
   :Period
   :FillPath
   :PathLength
   :FollowOn
   :FollowOnCount
   :DeploymentCount
   ;;Added to account for improper ordering.
   :Category
   :DwellYearsBeforeDeploy
   ])

(def demandtrend-fields
  [:t  	        
   :Quarter	
   :SRC	        
   :TotalRequired	
   :TotalFilled	
   :Overlapping	
   :Deployed	
   :DemandName	
   :Vignette	
   :DemandGroup	
   :ACFilled	
   :RCFilled	
   :NGFilled	
   :GhostFilled	
   :OtherFilled
   :ACOverlap
   :RCOverlap
   :NGOverlap
   :GhostOverlap
   :OtherOverlap])

(def demandtrend
  {:t  	        :int
   :Quarter	:int
   :SRC	        :text
   :TotalRequired	:int
   :TotalFilled	:int
   :Overlapping	:int
   :Deployed	        :int
   :DemandName	:text
   :Vignette	        :text
   :DemandGroup	:text
   :ACFilled	        :int
   :RCFilled	        :int
   :NGFilled	        :int
   :GhostFilled	:int
   :OtherFilled	:int
   :ACOverlap  :int
   :RCOverlap  :int
   :NGOverlap  :int
   :GhostOverlap :int
   :OtherOverlap  :int
   :deltaT :int
   })
