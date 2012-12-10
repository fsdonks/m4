;This is a -currently- centralized set of data definitions used by 
;marathon.  The initial port is from Marathon v 3.7602983. 
(ns marathon.data
  (use [util.record :only [defrecord+ with-record]]
       [util.metaprogramming :only [defmany keyvals->constants]]))

;Container to store all the data associated with demand entities. 
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

;Container to store all the data associated with matching supply to demand, 
;namely substitution rules, scoping (both in and out of scope) for the current
;run, and any other associated data.
(defrecord+ fillstore [[name "FillStore"] 
                       fillgraph 
                       fillfunction 
                       [fills {}]
                       rendergraphs  
                       [outofscope {}]
                       allgraphs])
(def empty-fillstore (make-fillstore))



;may be vestigial.
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

;Container to store all the data associated with output, particularly in the 
;form of effectful (i.e. non-pure) logging and statistics.  Typically, the 
;outputstore will maintain a set of files, as well as output-specific observers
;that record logs and statistics as the simulation progresses.  This may change
;in the near future.
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
                             subscribers ;probably drop this field....
                             activepolicy 
                             activeperiod
                             policies])

(defrecord+ policyatomic [name 
                          subscribers ;probably drop this field....
                          activepolicy 
                          activeperiod
                          policies])

;This is for centralizing control over rotational policy, as well as substition 
;policy. 
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
;I opted to change these to string constants, since they're more declarative 
;and require no conversion


;Constants used for policy definition, among other things.  Imported from the 
;original VBA implementation for the policystore. 
;Might re-think this, for now it's a way of porting the existing implementation
(def policyconstants 
  {:Bogging "Bogging"
   :Dwelling "Dwelling"
   :BogDeployable "BoggingDeployable"
   :DwellDeployable "DwellingDeployable"
   :Deployable "Deployable"
   :AC12 "AC12" 
   :AC13 "AC13" 
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
(keyvals->constants policyconstants) ;make the constants first class symbols.
;inherited from substitution rules, may be vestigial.
(keyvals->constants {:Equivalence :Equivalence :Substitution :Substitution})

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

;May be vestigial.
(def msgbands {:simulation 1 :supply 2 :demand 3 :policy 4 :trending 5})

;ported from VBA (no...I did not do it by hand...not entirely :) )
(def default-events
  {:Simulation
   {:Terminate "Terminating",
    :WatchPolicy "WatchPolicy",
    :sample "Sampling",
    :WatchParameters "WatchParameters",
    :WatchGUI "WatchGUI",
    :UpdateRequest "UpdateRequest",
    :Initialize "Initializing",
    :WatchDemand "WatchDemand",
    :EndofDay "End of Day",
    :WatchTime "WatchTime",
    :BeginDay "Begin Day",
    :WatchFill "WatchFill",
    :PauseSimulation "PauseSimulation",
    :WatchSupply "WatchSupply"}
   :Generic {:all "Generic"}
   :Supply
   {:spawnunit "Spawning Unit",
    :neverDeployed "Never Deployed",
    :supplyUpdate "Supply Update",
    :CycleCompleted "Cycle Completed",
    :MoveUnit "Moving Unit",
    :firstDeployment "First Deployment",
    :UnitChangedPolicy "Unit Changed Policy",
    :outofstock "out of Stock",
    :NewFollowOn "New Follow On",
    :NewDeployable "New Deployable Stock",
    :Deployable "Deployable",
    :updateAllUnits "Update All Units",
    :AwaitingPolicyChange "Awaiting Policy Change",
    :CycleStarted "Cycle Started",
    :MoreSRCAvailable "More SRC Available",
    :FollowingOn "Follow On",
    :deploy "Deploying Unit",
    :ScopedSupply "Scoped Supply",
    :NewSRCAvailable "New SRC Stock",
    :PositionUnit "Positioned Unit",
    :NotDeployable "Not Deployable",
    :UnitPromoted "Unit Promoted",
    :UnitMoved "Unit Moved",
    :AddedUnit "Added Unit",
    :FillDemand "Filling Demand",
    :SpawnGhost "Spawn Ghost",
    :SpawnedTransient "Spawned Transient Unit"}
   :Demand
   {:extendedBOG "Extended BOG",
    :scheduleDemand "Scheduling Demand",
    :DemandFillChanged "Demand Fill Changed",
    :RequestFill "Requesting Fill",
    :StockRequired "Stock Required",
    :CannotFillDemand "Cannot Fill Demand",
    :OverlappingUnit "Overlapping Unit",
    :InfeasibleDemand "Infeasible Demand",
    :ActivateDemand "Activating Demand",
    :demandupdate "DemandUpdate",
    :DeActivateDemand "DeActivating Demand",
    :AddedDemand "Added Demand",
    :ScopedDemand "Scoped Demand",
    :CanFillDemand "Can Fill Demand",
    :DisengageUnit "Sending Unit Home"}
   :Policy
   {:periodChange "Period Change",
    :AddedPeriod "Added Period",
    :policychange "Policy Change"}
   :Trending
   {:UnitNotUtilized "Unit Not Utilized",
    :NewHighWaterMark "New High Water Mark",
    :GetCycleSamples "Getting Cycle Samples",
    :GhostReturned "Ghost Returned",
    :GhostDeployed "Ghost Deployed",
    :LocationSwap "Location Swap",
    :LocationDecrement "Location Decrement",
    :LocationIncrement "Location Increment"}})

(defrecord+ managerofevents [[name "EventManager"]
                             [userevents default-events] 
                             evtstream 
                             streams])  

;cyclerecord
;Provides a container for an invidual unit's cycle information.  Specifically, 
;the policy followed, the unit in the cycle, the component of the unit, the 
;accumulated state during the cycle (bog, mob, dwell, etc.), start and end 
;of the cycle, transitions in the cycle, and more...
(defrecord+ cyclerecord 
  [UICname    ;As String ;;Associated uic
   src        ;As String
   component  ;As String   
   policyname ;As String ;;Associated policy
   tstart     ;As Single ;cycle start
   tfinal     ;As Single ;expected cycle end
   duration   ;As Single ;actual cycle length   
   availableTime  ;As Single ;time spent in available pool
   deployableTime ;As Single ;time spent deployable.
   DurationExpected  ;As Single ;expected cycle length
   bog         ;As Single ;experienced BOG (units of time, days)
   bogbudget   ;As Single
   BOGExpected ;As Single ;expected BOG (units of time, days)
   dwell       ;As Single ;expected Dwell (units of time, days)
   DwellExpected ;As Single ;experienced dwell
   MOBexpected   ;As Single ;mobilization time expected, if any.
   mob           ;As Single
   deployments   ;As Long ;count of deployments
   followons     ;As Long ;count of follow-on deployments
   BDRExpected   ;As Single ;expected BOG/Dwell ratio
   Traversals]) ;As Collection ;record of state traversal, good for verification

;Note -> this may be a little vestigial, or easily revamped; for instance, 
;we can provide uic, src, and component and we're fine...
(defn ^cyclerecord cycle-NewCycle
  "Creates a new cycle, at time t, defined by the bogtime, dwelltime, etc. 
   characteristics, patterned off of cycle c, which provides the name of the 
   uic, the src, and the component."
  [cyclerec t bogtime dwelltime policyduration & [MOBtime  ghost  bogbudget]]
    (with-record cyclerec
      :BOGExpected  bogtime
      :bogbudget (if (zero? bogbudget) bogtime bogbudget)
      :BDRExpected =  (/ 1 (/ (+ MOBtime bogbudget)  
                              (-  policyduration (+ bogbudget  MOBtime))))
      :DurationExpected  policyduration
      :DwellExpected  dwelltime
      :MOBexpected  MOBtime
      :tstart t
      :tfinal (+ t policyduration)))

(defn ^cyclerecord cycle-modify 
  "Modifies oldcycle, assumably in the context of a policy change.  Returns the 
   result of the modification, as a new cycle."
  [cyclerec bogtime dwelltime policyduration & [MOBtime bogbudget]]
  (if (and (> dwelltime 1095)  (zero? MOBtime) (not (= :inf dwelltime))
           (throw (Exception. "Expected dwell time is abnormal...")))     
    (with-record cyclerec
      :BOGExpected  bogtime
      :BDRExpected (/ 1 (/ (+ bogtime MOBtime) 
                           ( - policyduration (+ bogtime  MOBtime))))
      :DurationExpected policyduration
      :DwellExpected  dwelltime
      :MOBexpected = MOBtime
      :bogbudget = bogbudget)))

(defn ^cyclerecord cycle-add-traversal [cyclerec t startlocation endlocation]
  (let [trav  (str t "_"  startlocation  "_" endlocation)
        ts (get :Traversals cyclerec)] 
    (with-record cyclerec 
      :Traversals (conj trav ts))))

(defn BDR
  "Computes the BOG:Dwell ratio for a cycle."
  [bog mob dwell availableTime MOBexpected BOGExpected DurationExpected 
   & [conventional]]  
  (let [res 
        (cond (and  (> bog  0) (> dwell 0))
              (/ (+ bog mob)  dwell)
              (and (> dwell 0) 
                   (> availableTime 0))
              (/ availableTime dwell)
              :else 
              (/ (+ MOBexpected  BOGExpected) (- DurationExpected 
                                                 (+ MOBexpected BOGExpected))))]
    (if conventional 
      (/ 1 res) 
      res)))

(defn cycle-BDR
  "Computes the BOG:Dwell ratio from a cyclerecord."
  [cyclerec & [conventional]]
  (let [{:keys [bog mob dwell availableTime 
                MOBexpected BOGExpected DurationExpected]} cyclerec]
    (BDR bog mob dwell availableTime 
         MOBexpected BOGExpected DurationExpected conventional)))
  

