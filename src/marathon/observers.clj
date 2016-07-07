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

;;Deployments
;;===========
;;Recording deployments with benevolent side-effects.
;;You know, we could just attach a deployment component
;;to each entity that had a deployment on said day....and
;;then wipe the deployments afterward.  That's an idea...
;;handle "events" by attaching components.  Keeps everything
;;in data.  The other approach is maintaing ephemeral
;;logs like these...

;;I like the approach of tagging entities with eventful
;;data...
;;Like, if we fire an event at an entity, we attach
;;the event to the entity.  That way, if we
;;look at the entity's history at said point in time,
;;it'll have a an event component with all of the
;;events for said day, i.e. the instantaneous events.

;;right now, on update, we're just firing off this
;;event...so we get event-based processing with
;;the pure functional approach...


;;maybe there's a deployments entity with unit components...
;;we conj onto it...
;;I like the data-driven approach a bit better...
;;inside of log-deployment, we can conj into the deployments
;;component for the unit.


;;deployments push records onto a transient vector inside of
;;an atom at :deployment-watch/:new-deployments 
(defn record-deployment [ctx edata name]
  (->>  (:data edata)
        (new-deployment)
        (store/conj-ephemeral ctx :deployment-watch :new-deployments)))

(defn commit-deployments [ctx edata  _]
  (if-let [deployments (core/some-ephmeral ctcx :deployment-watch :new-deployments)]
    (let [t (core/get-time ctx)]
      (-> ctx
          (store/updatee :state :deployments #(conj (persistent! @deployments)))
          (core/reset-ephemeral :deployment-watch :new-deployments (transient []))))
    ctx))

;;a cell for holding ephemeral values.
;;We tend to only care about the initial state and the
;;final state of an identity over time.  For this reason,
;;we can use cells to capture the end points, and provide
;;a simple structure that can be updated pretty quickly with
;;new right-ends.
(defrecord Cell [init current])
(defn ->cell
  ([init] (Cell.  init  init))
  ([init current] (Cell. init current)))
(defn push-cell [^Cell cl v] (Cell. (.init cl) v))

;;if we have known policy trajectories...then we can alter position, velocity, etc.
;;we'd like to ensure that entities who move are recorded.
;;In our case, it's enough to tag a movement component onto the entities.
;;We make sure to drop the movement domain when we begin a new day..
(defn telemetry [ctx edata name]
  (case (sim/event-type edata)
    :positionUnit
          (let [[name frompos topos] (:data edata)
                [ctx storage] (core/get-ephemeral ctx :position-delta name nil)]
            (do (if @storage
                  (swap! storage  #(push-cell topos))            
                  (reset! storage (->cell frompos topos)))
                ctx))                
     :unitMoved
     (let [[name fromloc toloc] (:data edata)
           [ctx storage]        (core/get-ephemeral ctx :movement-delta name nil)]
       (do (if @storage
             (swap! storage  #(push-cell toloc))            
             (reset! storage (->cell fromloc toloc)))
           ctx))          
    (throw (Exception. (str [:unregistered-event (sim/event-type edata)])))
    )
  )

;;these are basically event-driven systems...
;;still works nicely in the ECS paradigm.

;;by default, we watch all this crap...
;;This is almost identical to the pub/sub setup...
;;We have multiple observer-functions processing
;;events (serially...)
(def default-routes
  {:deployment-watch {:deploy       record-deployment
                      :end-day      commit-deployments}
   :movement-watch   {:positionUnit movement-watch
                      :unitMoved    movement-watch
                      :end-day      commit-movement
                                }
                      })   
   ;;We can derive locations pretty easily...
   ;;also demand trends....
   ;;looks like deployments are the current odd datum we're not tracking.
;   :location-watch   

(defn register-default-observers [ctx]
  (simnet/register-routes  default-routes ctx))

;;We can map these into the requisite notifications for the animation/visualzation
;;stuff.  that way, we get our deltas necessary for animation, and it ends up
;;very easy to compare past to previous by diffing.

;;we want to track policy changes.



;;Is there a higher-level concept here?
;;Can we define entity-level events....
;;and then apply our higher-order constructs for mapping and
;;stuff?

;;At the implementation level, the event is a component that's added at
;;an instant in time.
;;Multiple events overwrite (or do we buffer?)

;;We get an event history that way.
;;Also, possibly retain a shitload of events and event messages (may not
;;want to do that).

;;I'd like to say "as the unit moves, record all pairwise
;;movements..."
;;If we go with channels/transducers, we get this kind of stuff.
;;So, if we have an initial channel, we can transduce over it to
;;get the event history..
;;Channels are just buffers in the end. 
;;"ala (->> (:unitMoved) (partition 2 1) 


;;interested in positioning of units...we have an event for that.
;;policy changes...
:PositionUnit


;;unit movement events...location changes..
:unitMoved

