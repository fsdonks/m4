;;Collection of observation functions and
;;samplers that capture events...
;;From the legacy implementation, we
;;have these as logging deployments and
;;things like that.
(ns marathon.observers
  (:require [marathon.data      [protocols :as generic]]
            [spork.entitysystem [store :as store]]
            [spork.sim.pure [network :as simnet]]))

;;identical to proc....copied from there...
;;deployment records
(def deprecordschema
  {:ID	                :int
   :DeploymentID	:int
   :Location	        :text
   :Demand	        :text
   :DwellBeforeDeploy   :int
   :BogBudget	        :int
   :CycleTime	        :int
   :DeployInterval	:int
   :DeployDate	        :text
   :FillType	        :text
   :FillCount	        :int
   :UnitType	        :text
   :DemandType	        :text
   :DemandGroup	        :text
   :Unit	        :text
   :Policy	        :text
   :AtomicPolicy	:text
   :Component	        :text
   :Period	        :text
   :FillPath	        :text
   :PathLength	        :int
   :FollowOn	        :boolean
   :FollowOnCount	:int
   :DeploymentCount	:int
   :Category	         :text
   :DwellYearsBeforeDeploy :text
   :OITitle	             :text})


;;location changes...I don't think this matters for us anymore,
;;although if we can synthesize it from history, that's great.
;;LEGACY
(def locschema {:T :int 
                :EntityFrom :text
                :EntityTo :text
                :EventName :text
                :Msg :text})

;;DemandRecords  ;;DEPRECATED , new SRM stuff trumps this.
(def drecordschema {"Type"         :text
                    "Enabled"      :boolean
                    "Priority"	   :int
                    "Quantity"	   :int
                    "DemandIndex"  :int	
                    "StartDay"	   :int
                    "Duration"	   :int
                    "Overlap"	   :int
                    "SRC"          :text
                    "SourceFirst"  :text	
                    "DemandGroup"  :text	
                    "Vignette"	   :text
                    "Operation"	   :text
                    "Category" 	   :text
                    "Title 10_32"  :text	
                    "OITitle"      :text})

;;DemandTrends...
(def demand-trend-schema
  {:t  	        :int
   :Quarter	        :int
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
   :OtherFilled	:int})

;;CycleRecords...
(def cycleschema 
  {"tstart"	      :int
   "tfinal"	      :int
   "Deployments"      :int
   "BOG"	      :int
   "MOB"	      :int
   "Dwell"            :int	
   "Duration"         :int 	
   "BDR"	      :float
   "BDR 1:X"	      :float
   "BOGExpected"      :int
   "DwellExpected"    :int 
   "DurationExpected" :int})

;;FillRecords, which are produced....

;Tom gave craig this schema on 7/8/15:
 (def fillrecord {:Unit        :text
                  :category    :text
                  :DemandGroup :text
                  :SRC         :text
                  :FillType    :text
                  :FollowOn    :boolean
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


(def deprecordchema
  {:Unit         :text
   :DemandGroup  :text
   :FillType     :text
   :FollowOn     :text
   :UnitType     :text
   :Component    :text
   :DeploymentID :text
   :DeployDate    :text
   :FollowOnCount :text
   :AtomicPolicy  :text
   :DeployInterval :text
   :FillPath      :text
   :Period        :text
   :Demand        :text
   :PathLength    :int
   :BogBudget     :int
   :CycleTime     :int
   :DeploymentCount :int
   :DemandType      :text
   :FillCount       :int
   :Location        :text
   :DwellBeforeDeploy :int
   :Policy            :text})

;;should match the deprecord schema.
(defn new-deployment
  ([{:keys [unitfrom locationfrom demandto fill fillcount t deploydate period]}]
    (new-deployment unitfrom locationfrom demandto fill fillcount t deploydate period))
  ([unitfrom locationfrom demandto fill fillcount t deploydate period]
   (let [{:keys [bogbudget followons deployments dwell]} (:CurrentCycle unitfrom)
         pol (:policy unitfrom)]
     {:Unit          (:name unitfrom)
      :DemandGroup   (or (:demandgroup demandto) "UnGrouped")
      :FillType      (:quality  fill)
      :FollowOn      (:followon fill)
      :UnitType      (:src unitfrom)
      :Component     (:component unitfrom)
      :DeploymentID  (:deploymentindex unitfrom)
      :DeployDate    deploydate
      :FollowOnCount followons
      :AtomicPolicy  (generic/atomic-name pol)
      :DeployInterval t
      :FillPath      (:fillpath fill)
      :Period        period
      :Demand        (:name       demandto)
      :PathLength    (:pathlength fill)
      :BogBudget     bogbudget
      :CycleTime     (:cycletime unitfrom)
      :DeploymentCount deployments
      :DemandType    (:primaryunit demandto)
      :FillCount     fillcount
      :Location      locationfrom
      :DwellBeforeDeploy dwell
      :Policy        (generic/policy-name pol)}))
  )


;;handlers are functions of the form
;;(ctx  -> edata -> name -> ctx)

;;right now, on update, we're just firing off this
;;event...so we get event-based processing with
;;the pure functional approach...
(defn add-deployment [ctx evt]

;;deployments push records onto a transient vector inside of
;;an atom at :deployment-watch/:new-deployments 
(defn deployment-handler [ctx edata name]
  (->>  (:data edata)
        (new-deployment)
        (store/conj-ephemeral ctx :deployment-watch :new-deployments)))


;;by default, we watch all this crap...
(def default-routes
  {:deployment-watch {:deploy  deployment-handler}
;   :location-watch   
   })

(defn register-default-observers [ctx]
  (simnet/register-routes  default-routes ctx))



