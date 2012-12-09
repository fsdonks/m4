;This is a -currently- centralized set of data definitions used by 
;marathon.  The latest version in 
(ns marathon.data
  (use [util.record]))

(defrecord+ demandstore [[name "DemandStore"] 
                         [demandmap  {}]
                         [infeasible-demands {}] 
                         unfilledq 
                         [activations {}]
                         [deactivations {}]
                         [eligbledemands {}]
                         [changed true]
                         demandtraffic  
                         tryfill  
                         loginfeasibles 
                         [tags {"Sinks" {}}]
                         fillables 
                         verbose 
                         tlastdeactivation])

(def empty-demandstore (make-demandstore)) 

(defrecord+ fillstore [[name "FillStore"] 
                       fillgraph 
                       fillfunction 
                       [fills {}]
                       rendergraphs  
                       [outofscope {}]
                       allgraphs])
(def empty-fillstore (make-fillstore))

(def dstreams {"LocationWatch", "xl"
               "GhostWatch", "xl"
               "Ghosts", "xl"
               "DeployWatch", "xl"
               "CycleLog", "csv"
               "SandTrends", "xl"
               "Deployments", "xl"
               "DemandTrends", "csv"
               "EmpiricalSummary", "xl"
               "NominalSummary", "xl"})

(defrecord+ outputstore [[name "Outputstore"] 
                         [observers {}] 
                         [filestreams {}] 
                         [defaultstreams dstreams]
                         [defaultfiles {"cycles", "cycles.csv"}] 
                         [mypath (System/getProperty "user.dir")]
                         [activestreams {}]])
(def empty-outputstore (make-outputstore)) 

;a structure for unit entity policies. 
(defrecord+ policy [[name "BlankPolicy"]
                    [cyclelength :inf] 
                    [mindwell 0]
                    [maxdwell :inf]
                    [maxbog :inf] 
                    [maxMOB :inf]
                    [recovery  90] 
                    [startdeployable 0] 
                    [stopdeployable :inf]
                    [positiongraph  nil]
                    [startstate :spawn]
                    [endstate :spawn]
                    [overlap 45]
                    [subscribers {}]]) 

(def empty-policy (make-policy))

;policies defined by more than one atomic policy.
(defrecord+ policycomposite [name 
                             subscribers 
                             activepolicy 
                             activeperiod
                             policies])
;Policy store.
;This is for centralizing control over rotational policy, as well as substition policy.
;We maintain all the data structures necessary for managing this stuff.
;    Also manage all feasible locations through this object.

(defrecord+ policystore   [[name "PolicyStore"] 
                          [locationmap {}] 
                          [positions {}] 
                          [locations {}] 
                          [locationindex {}] 
                          [policies {}] 
                          [periods {}]
                          [highest {}] 
                          policytraffic  
                          rules  
                          rulegraph 
                          ruledelim 
                          activeperiod 
                          [periodchanges {}]
                          schedules 
                          [composites {}] 
                          [permanents {}] 
                          canghost])

;Substitutions are going to be managed as a special object...
;These are general states used to describe information about the location.
;We can actually store all kinds of details about the location in the actual
;Graph node's data. Since the structure contains variants, we could have more
;useful, descriptive
;I opted to change these to string constants, since they're more declarative and require no conversion
;These are our unit's states.
;We can add more, or even parse custom states as needed.

;--PORT--Replace all const usage with keywords.... :Bogging :Dwelling, etc. 

(defmacro defmany
  "Define multiple definitions inline.  Takes a collection of bindings, and 
   converts them into def expressions."
  [bindings]
    `(do ~@(->> (partition 2 bindings)
             (map (fn [b] `(def ~(first b) ~(second b)))))))
(defn keyvals->constants
  "Given a map of {symbol val} or {:symbol val}, 
   creates def bindings for each symbol."
  [m]
  (let [as-symbol (fn [k] 
                    (cond (symbol? k) k
                          (keyword? k) (symbol (subs (str k) 1))
                          :else (throw (Exception. "unknown symbol type"))))]
    (eval `(defmany ~(flatten (for [[k v] m] [(as-symbol k) v]))))))


;Might re-think this, for now it's a way of porting the existing implementation
(def policyconstants 
  {:Bogging "Bogging"
   :Dwelling "Dwelling"
   :BogDeployable "BoggingDeployable"
   :DwellDeployable "DwellingDeployable"
   :Deployable "Deployable"
   :AC12 "AC12" 
   :AC13 "AC13" 
   ;not a good thing. I need to fix this with
   :RC14 "RC14" 
   :RC15 "RC15" 
   :AC11 "AC11"
   :RC11 "RC11"
   :RC12 "RC12"
   :GhostPermanent12 "GhostPermanent12"
   :GhostPermanent13 "GhostPermanent13"
   :GhostTransient12 "GhostTransient12"
   :GhostTransient13 "GhostTransient13"   
   :reset "Reset"
   :train "Train"
   :ready "Ready"
   :available "Available"
   :deployed "Deployed"
   :Overlapping "Overlapping"
   :SubSymbol  "{>"
   :EquivSymbol "="})
(keyvals->constants policyconstants)
           
(def Equivalance :Equivalance)
(def Substitution :Substitution) 

;TOM Note 6 April 2011 ->
    ;I re-factored much of the guts of this class into a GenericEventStream 
    ;class, which handles all of the subscribing, cataloging, etc., and wraps 
    ;an observer to provide communications.
;This class serves as a proxy for encapsulating what appear to be classic
;;;event;; handlers. Basically, I want to standardize the generation of events 
;(mostly log events in our case), but also to begin to meld the notion of 
;event-driven simulation within the time-step model. The goal is to eventually 
;turn this into an actual event-driven sim. We just manually "trigger" events 
;from calling objects, and have this class handle them. The events are actually 
;methods contained in the class. The only difference between this and the 
;event-step framework, is that we;re calling handlers from a class directly, 
;rather than queuing events in a priorityQ (lose some robustness, since we can 
;have multiple handlers with their own state subscribing to events.

  
;Creating a bandwith for message enumerations.  Right now, each message has a 
;bandwith of ~100. from the msgbands, by convention, all simulation messages 
;should be between 100 and 199
;supply msgs -> 200 -> 299
;demand msgs -> 300 -> 399
;if we need to increase the bandwith later, we can (total messages are actually 
;vast, 2^32)  Right now, I arbitrarily allocated 99 messages worth of space for 
;each gross category off messages.  This aids when we;re trying to categorize 
;messages later, for instance, when adding a bunch of loggers, I want to 
;associate a particular logger only with "Simulation" messages/events.
;As a result, I can keep a sub up to date that will filter out the appropriate 
;messages for it to subscribe to.

;NOTE -> While events appear to be explicitly enumerated here, their actual
;representation is really just a long integer and a dictionary of data.  All of 
;the association of events with names, msgbands, etc. is really local to this 
;object.  The end result is that the library of events can be easily extended 
;(even via modules or scripts) to accomodate custom events.

;Users can define their own ad-hoc events (and should!) that will get assigned 
;to be well beyond the existing bandwidth.
;I will implement a custom-event API for this functionality.  Currently, one 
;could add a custom event using the AddEvent method.

;Really, this is just a method library. We pull the methods from other classes
;(particularly where the method calls are manipulating state across classes, 
;and logging events) . Since we;re working with objects and references, we can 
;do this now.

(def msgbands {:simulation 1 :supply 2 :demand 3 :policy 4 :trending 5})
;Basic events ported from Marathon.
(def events 
    {:spawnunit "Spawning Unit",
		 :neverDeployed "Never Deployed",
		 :supplyUpdate "Supply Update",
		 :CycleCompleted "Cycle Completed",
		 :AddedPeriod "Added Period",
		 :Terminate "Terminating",
		 :extendedBOG "Extended BOG",
		 :sample "Sampling",
		 :scheduleDemand "Scheduling Demand",
		 :MoveUnit "Moving Unit",
		 :UnitChangedPolicy "Unit Changed Policy",
		 :outofstock "out of Stock",
		 :DemandFillChanged "Demand Fill Changed",
		 :NewFollowOn "New Follow On",
		 :GhostReturned "Ghost Returned",
		 :RequestFill "Requesting Fill",
		 :NewDeployable "New Deployable Stock",
		 :Deployable "Deployable",
		 :AwaitingPolicyChange "Awaiting Policy Change",
		 :LocationSwap "Location Swap",
		 :policychange "Policy Change",
		 :StockRequired "Stock Required",
		 :CannotFillDemand "Cannot Fill Demand",
		 :UpdateRequest "UpdateRequest",
		 :CycleStarted "Cycle Started",
		 :MoreSRCAvailable "More SRC Available",
		 :FollowingOn "Follow On",
		 :deploy "Deploying Unit",
		 :Initialize "Initializing",
		 :OverlappingUnit "Overlapping Unit",
		 :InfeasibleDemand "Infeasible Demand",
		 :LocationDecrement "Location Decrement",
		 :ActivateDemand "Activating Demand",
		 :ScopedSupply "Scoped Supply",
		 :NewSRCAvailable "New SRC Stock",
		 :PositionUnit "Positioned Unit",
		 :EndofDay "End of Day",
		 :demandupdate "DemandUpdate",
		 :NotDeployable "Not Deployable",
		 :UnitMoved "Unit Moved",
		 :GhostDeployed "Ghost Deployed",
		 :periodChange "Period Change",
		 :BeginDay "Begin Day",
		 :AddedUnit "Added Unit",
		 :DeActivateDemand "DeActivating Demand",
		 :AddedDemand "Added Demand",
		 :all "Generic",
		 :FillDemand "Filling Demand",
		 :SpawnGhost "Spawn Ghost",
		 :ScopedDemand "Scoped Demand",
		 :LocationIncrement "Location Increment",
		 :SpawnedTransient "Spawned Transient Unit",
		 :CanFillDemand "Can Fill Demand",
		 :DisengageUnit "Sending Unit Home"})

(defn addEvent [name description grouping eventmap]
  (assoc eventmap (keyword name) description))
(defn add-events [tgt specs]
  (reduce #(apply addEvent %) tgt specs)) 
(def system-events 
  (add-events {}
     ['all, "Generic", "Generic"]
     ['Initialize, "Initializing", "Simulation"]
     ['Terminate, "Terminating", "Simulation"]
     ['sample, "Sampling", "Simulation"]
     ['EndofDay, "End of Day", "Simulation"]
     ['BeginDay, "Begin Day", "Simulation"]
     ['UpdateRequest, "UpdateRequest", "Simulation"]    
     ['WatchSupply, "WatchSupply", "Simulation"]
     ['WatchDemand, "WatchDemand", "Simulation"]
     ['WatchPolicy, "WatchPolicy", "Simulation"]
     ['WatchTime, "WatchTime", "Simulation"]
     ['WatchParameters, "WatchParameters", "Simulation"]
     ['WatchFill, "WatchFill", "Simulation"]
     ['PauseSimulation, "PauseSimulation", "Simulation"]
     ['WatchGUI, "WatchGUI", "Simulation"]))

(def supply-events 
  (add-events {}
     [[ 'spawnunit, "Spawning Unit", "Supply"]
      [ 'deploy, "Deploying Unit", "Supply"]
      [ 'MoveUnit, "Moving Unit", "Supply"]
      [ 'FillDemand, "Filling Demand", "Supply"]
      [ 'NewDeployable, "New Deployable Stock", "Supply"]
      [ 'NewSRCAvailable, "New SRC Stock", "Supply"]
      [ 'MoreSRCAvailable, "More SRC Available", "Supply"]
      [ 'NotDeployable, "Not Deployable", "Supply"]
      [ 'Deployable, "Deployable", "Supply"]
      [ 'outofstock, "out of Stock", "Supply"]
      [ 'CycleCompleted, "Cycle Completed", "Supply"]
      [ 'CycleStarted, "Cycle Started", "Supply"]
      [ 'SpawnGhost, "Spawn Ghost", "Supply"]
      [ 'supplyUpdate, "Supply Update", "Supply"]
      [ 'ScopedSupply, "Scoped Supply", "Supply"]
      [ 'AddedUnit, "Added Unit", "Supply"]
      [ 'PositionUnit, "Positioned Unit", "Supply"]
      [ 'UnitChangedPolicy, "Unit Changed Policy", "Supply"]
      [ 'AwaitingPolicyChange, "Awaiting Policy Change", "Supply"]
      [ 'SpawnedTransient, "Spawned Transient Unit", "Supply"]
      [ 'neverDeployed, "Never Deployed", "Supply"]
      [ 'NewFollowOn, "New Follow On", "Supply"]
      [ 'FollowingOn, "Follow On", "Supply"]
      [ 'UnitMoved, "Unit Moved", "Supply"]
      [ 'firstDeployment, "First Deployment", "Supply"]
      [ 'updateAllUnits, "Update All Units", "Supply"]
      [ 'UnitPromoted, "Unit Promoted", "Supply"]]))

(def demand-events 
  (add-events {}
    [['RequestFill, "Requesting Fill", "Demand"]
     ['scheduleDemand, "Scheduling Demand", "Demand"]
     ['ActivateDemand, "Activating Demand", "Demand"]
     ['DeActivateDemand, "DeActivating Demand", "Demand"]
     ['DisengageUnit, "Sending Unit Home", "Demand"]
     ['InfeasibleDemand, "Infeasible Demand", "Demand"]
     ['extendedBOG, "Extended BOG", "Demand"]
     ['demandupdate, "DemandUpdate", "Demand"]
     ['ScopedDemand, "Scoped Demand", "Demand"]
     ['CannotFillDemand, "Cannot Fill Demand", "Demand"]
     ['CanFillDemand, "Can Fill Demand", "Demand"]
     ['StockRequired, "Stock Required", "Demand"]
     ['AddedDemand, "Added Demand", "Demand"]
     ['DemandFillChanged, "Demand Fill Changed", "Demand"]
     ['OverlappingUnit, "Overlapping Unit", "Demand"]]))

(def policy-events 
  (add-events {}
    [['policychange, "Policy Change", "Policy"]
     ['AddedPeriod, "Added Period", "Policy"]
     ['periodChange, "Period Change", "Policy"]]))
 
(def trending-events 
  (add-events {}
    [['LocationIncrement, "Location Increment", "Trending"]
     ['LocationDecrement, "Location Decrement", "Trending"]
     ['LocationSwap, "Location Swap", "Trending"]
     ['GhostDeployed, "Ghost Deployed", "Trending"]
     ['GhostReturned, "Ghost Returned", "Trending"]
     ['GetCycleSamples, "Getting Cycle Samples", "Trending"]
     ['NewHighWaterMark, "New High Water Mark", "Trending"]
     ['UnitNotUtilized, "Unit Not Utilized", "Trending"]]))


(defrecord+ managerofevents [[name "EventManager"]
                            [userevents events] 
                            evtstream 
                            streams])
  


