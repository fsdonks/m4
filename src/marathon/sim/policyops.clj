(ns marathon.sim.policyops
  (:require [spork.util.metaprogramming :refer [keyvals->constants]]
            [spork.cljgraph.core :as graph]
            [marathon.sim.core :refer [+inf+]]
            [marathon.policy.policydata :as policydata]            
            [marathon.data.protocols :as core :refer [Bogging 
                                                      Dwelling 
                                                      BogDeployable 
                                                      DwellDeployable 
                                                      Deployable 
                                                      Overlapping
                                                      ReturnToDeployable
                                                      NotDeployable
                                                      Spawning
                                                      Waiting
                                                      Deploying
                                                      AC12  
                                                      AC13  
                                                      RC14  
                                                      RC15  
                                                      AC11 
                                                      RC11 
                                                      RC12 
                                                      GhostPermanent12 
                                                      GhostPermanent13 
                                                      GhostTransient12 
                                                      GhostTransient13 
                                                      reset 
                                                      train 
                                                      ready 
                                                      available 
                                                      deployed
                                                      demobilization]]))

;;The functions in this library focus on quickly deriving policies from existing templates.  There are a
;;multitude of hard-coded templates in here that describe canonical policies, which are used in
;;the PolicyCreation module to easily derive new policies from the template.  This is more or less a
;;policy library.  Note, all of these policies, as of 12 Sep 2012, have been documented in a serialized
;;format.  Technically, the use of policy templates does not require VBA code, and will be pushed outside
;;of VBA or the host platform in the near future.  By exposing the templates as data, it makes it easier
;;to add new templates to the library, which end users can stitch together and derive from.  Additionally,
;;pushing the specification outside of VBA should allow for specialized, graphical tools to build policies,
;;which are then imported into Marathon during pre-processing.
(def ^:constant +century+ (* 100 365))

(defn century-ceiling [length] (if (> length +century+) +inf+ length))

;;helper function.  Determines the type of cycle transform.
;;treat large cycles as effectively infinite for this function.
(defn length-type [t lengtha lengthb]
  (let [lengtha (century-ceiling lengtha)
        lengthb (century-ceiling lengthb)]
    (if (== lengtha lengthb) 
      :equiv   ;position = cycleTimeA / cyclelengthB
      (if (and (== lengtha +inf+)
               (== lengthb +inf+)) 
        :normal
        (if (== lengtha +inf+)
          (if (< t lengthb) 
            :infminus ;cyclelength is infinite, but cycletime is effectively less than targetted cycle.
            :infplus  ;cyclelength is infinite, cycletime is > than targetting cycle.
            )
          (throw (Exception. "unknown length case!")))))))               


;;Tom change 24 Sep 2012 -> added to deal with infinite cycle transitions.  Provides a projection function to determine
;;how far a unit will project, proportionally, onto a cycle it;;s changing to.
(defn compute-proportion [t lengtha lengthb]
  (case (length-type t lengtha lengthb)
    :normal (/ t lengtha) ;;produces (ctA * clB)/Cla
    :equiv  -1     ;;infinite, not defined
    :infplus 0.9   ;;scaling factor is arbitrary
    :infminus -1)) 

(defn project-cycle-time [proportion t length]
  (cond  (neg? proportion) t
         (pos? proportion) (* proportion length)
         :else (throw (Exception. "Uknown case for projecting cycle time!"))))
    

;;Aux function, may be able to get rid of this...
(defn get-delta [position deltas] (get deltas position 0))

(defmacro with-deltas [[binds deltasource] & expr]
  (let [ds (symbol "deltas")]
    `(let [~ds ~deltasource
           ~@(flatten (for [position binds]
                        [position `(get-delta ~position ~ds)]))]
       ~@expr)))

;;TOM Hack 24 July 2012-> This is a good idea.
;;Describe a base policy with recovery built in.
(defn recoverable-template [& {:keys [recoverytime] :or {recoverytime 90}}]
  (-> (policydata/make-policy :name "Recoverable")
      (core/add-positions {:Recovery :Recovering 
                           :Recovered :Recovered})
      (core/add-routes [[:Recovery :Recovered recoverytime]])))
 

(defn overlapping? [x] (= x Overlapping))
(defn ensure-positive [msgf x] 
  (if (not (neg? x)) x
      (throw (Exception. (str (msgf  x))))))

;;it seems like a nice, high-level way to describe policy alterations
;;is to define transforms, like insertions

(defn modified-routes [rts overlap deltas]
  (mapv (fn [[from to t]]
          (->> (cond (overlapping? from) (+ overlap t)
                     (overlapping? to)   (- t overlap)
                     :else t)
               (+ (get-delta from deltas))
               (ensure-positive #(str "nonpositive in modified-routes" 
                                      {:from from :to to :overlap overlap :deltas deltas :x %}))
               (vector from to)))
        rts))        

;; (defn ac12-defaults [overlap]
;;   {reset, 182, 
;;    train, 183, 
;;    ready, 365, 
;;    available, 365, 
;;    deployed, (- 365 overlap),
;;    Overlapping, overlap})

(defn compute-cycle-length [p]
  (let [{:keys [startstate endstate positiongraph]} p
        res  (graph/depth-first-search positiongraph startstate endstate {:weightf graph/arc-weight})
        lngth (get-in res [:distance endstate])]
    (+ lngth (graph/arc-weight positiongraph endstate startstate))))    


(defn altered-routes [routes deltas]
  `[~@ (for [[from to time] routes]
         `[~from ~to (+ ~time (get-delta ~deltas ~from))])])

;;A registry of all defined templates.  Gives us easy access.
;;For the most part, templates will be defined  (or even redefined)
;;here.....Consider these built-ins, with the ability of power users 
;;to define new templates.
(def templates (atom nil))
(defn get-template [nm] 
  (if-let [res (get @templates nm)]
    res
    (throw (Exception. (str "undefined policy template name: " nm)))))

;;The entire point of defining policies is defining state transition
;;graphs...really..
;;Entities interpret policies to determine what to do/where to
;;go/when, etc.  

;;For our generic policies, they ALL have an overlap...they all have 
;;a start state at Reset and and end state at Available.

;;It'd be nice to be able to define basic templates 

(def default-positions 
  {reset :dwelling 
   train :dwelling 
   ready :dwelling 
   available :dwelling 
   deployed :bogging
   Overlapping :overlapping})


;;this is really ac12 routes.
(def default-routes 
  [[reset train           182]
   [train ready           183]
   [ready available       365]
   [available reset       365]
   [deployed Overlapping  365]
   [Overlapping reset     0]])

;;Note -> these are really basic state transition templates.....we
;;could probably lift them out to spork at some point.

;;State transitions, used to define a range of policies that 
;;vary by time in state.
(def default-routing [[reset train ready available reset]  
                      [deployed Overlapping reset]])

(def rc-routing      [[reset train ready available reset]  
                      [deployed Overlapping demobilization reset]])

(def ghost-routing [[Spawning Deployable Waiting Deploying NotDeployable 
                     deployed Overlapping ReturnToDeployable Deployable]])

(defn waits->routes 
  [wait-times routing]
  (vec (mapcat (fn [xs] (map (fn [[from to]] 
                          [from to (get wait-times from)])
                        (partition 2 1 (filter #(get wait-times %) xs))))   routing)))
  

;;wait-times are parametric, and usually dependent on the policy...
;;we can embed this information in the policy, and have the wait times 
;;constructed as a function of the policy....

;; (defmacro defwaits [name args & body]
;;   (let [stats (gensym "stats")]
;;     `(defn ~name 
;;        (~args ~@body)
;;        ([~stats] (~name ~@(map (fn [arg] `(get ~stats ~(keyword arg))) args))))))

 
;;These are wait times based on state transition graphs.
;;We can probably abstract this out further, but for now it'll work...
(defn ac11-waits [{:keys [overlap]}]
  {reset       182 
   train       183 
   available   365
   deployed    (- 365 overlap) 
   Overlapping overlap})


;;see if we can roll all these guys up....there's no need to 
;;really have them explicit like that...

;; (defn ac-waits [{:keys [overlap maxbog]}]
;;     {reset       182 
;;      train       183 
;;      ready       maxbog
;;      available   maxbog
;;      deployed    (- maxbog overlap) 
;;      Overlapping overlap})

(defn ac12-waits [{:keys [overlap]}]
  {reset       182 
   train       183 
   ready       365
   available   365
   deployed    (- 365 overlap) 
   Overlapping overlap})

(defn ac13-waits [{:keys [overlap]}]
  {reset     182 
   train     183 
   ready     460 
   available 270 
   deployed (- 270 overlap) 
   Overlapping overlap})

(defn rc11-waits [{:keys [overlap]}]
  {reset 182 
   train 183 
   available 365
   deployed (- 270 overlap)
   Overlapping overlap
   demobilization 95})

(defn rc12-waits [{:keys [overlap]}]
  {reset 365 
   train 365 
   available 365
   deployed (- 270 overlap)
   Overlapping overlap
   demobilization 95})

(defn rc14-waits [{:keys [overlap]}]
  {reset 365 
   train 365 
   ready 730 
   available 365 
   deployed (- 270 overlap) 
   Overlapping overlap 
   demobilization 95})

(defn rc15-waits [{:keys [overlap]}]
  {reset 730 
   train 365
   ready 730 
   available 365 
   deployed (- 270  overlap)
   Overlapping overlap
   demobilization 95})  

(defn ghost-waits [{:keys [maxbog overlap]}]
  {Spawning 0
   Deployable 0
   Waiting +inf+
   Deploying 0
   NotDeployable 0
   deployed (- maxbog overlap)
   Overlapping overlap 
   ReturnToDeployable 0})

;;max utilization is pretty simple too...
(defn max-util-waits [{:keys [overlap maxbog mindwell]}]
  {reset mindwell
   Deployable 0
   available +inf+
   NotDeployable 0
   deployed (- maxbog overlap)
   Overlapping overlap})

(defn route-by [waitfn routes]
  (fn [stats] 
    (waits->routes (waitfn stats) routes)))

(defn unzip [xs]  (reduce (fn [acc [x y]]  (-> acc (conj x) (conj y))) [] xs))

;; (def ^:dynamic *stats* 
;;   {:startstate reset
;;    :endstate   available
;;    :overlap    45 
;;    :recovery   90
;;    :bogbudget  365
;;    :maxbog     365
;;    :maxdwell   +inf+
;;    :maxMob     365                   
;;    :mindwell   0})

(defn and-stats [base-stats & kvps] 
  (reduce (fn [m [k v]] (assoc m k v)) base-stats (partition 2 kvps)))

;;We end up needing to vary policies (that is the scheduled state
;;changes for entities) a LOT.  Consequently, rather than hardcode
;;them in (as we did in previous versions), there's a desire to make
;;policies highly data driven and user-defined.  The initial
;;implementation had a minimal set of built-in, or template, policies,
;;which the users could parametrically alter via a simple table of
;;data.  Additionally, users could define new policies by composing
;;the atomic policies via sequencing and period-mapping, creating 
;;stacked policies and policies mapped to periods of time. 

;;We still provide that functionality here, but in a much more refined
;;and extensible (and short!) form via deftemplate and friends.  We 
;;define a set of macros that provide legacy policy templates,
;;allowing users to compose them in aforementioned ways.  However, 
;;we also will expose the api so that users can define their own
;;template policies for additional composition.  There is probably 
;;room for a specific DSL here, but right now we just provide a simple
;;API for policy constructors.

(defmacro deftemplate 
  "Defines a named policy constructor useful for deriving new policies."
  [name & {:keys [routes positions doc stats] :or 
           {routes     default-routes 
            positions  default-positions
            stats      '*stats*}}]
  (let [doc (or doc (str "Policy constructor for " name " takes options [:name :deltas :startstate :endstate :overlap]"))]
    `(do 
       (defn ~name ~doc 
         [& {:keys [~'name ~'deltas ~'stats] 
             :or {~'name ~(str name)      ~'deltas nil }}]       
         (let [stats#  (merge ~stats ~'stats)
               routes# ~routes 
               routes# (if (fn? routes#) (routes# stats#) routes#)]
           (-> (apply policydata/make-policy (unzip (seq (assoc stats# :name ~'name))))
               (core/merge-policy-stats stats#)
               (core/add-positions ~positions)
               (core/add-routes (modified-routes routes# (:overlap stats#) ~'deltas)))))
         (swap! templates assoc ~(str name) ~name)
         (quote ~name))))

;;It may be easier to just push a base policy through multiple
;;transforms, that constitutes a "template"...

(defmacro simple-template 
  ([name doc routes base-stats & optional-stats]
     `{:name (quote ~name) :doc ~doc :routes (quote ~routes) :stats (and-stats ~base-stats ~@optional-stats)})
  ([[name doc routes base-stats & optional-stats]]  
     `{:name (quote ~name) :doc ~doc :routes (quote ~routes) :stats (and-stats ~base-stats ~@optional-stats)}))

(defn eval-template [td]
  (eval `(deftemplate ~(:name td) ~@(unzip (dissoc td :name)))))

(defmacro deftemplates [base-stats & xs]
  (doseq [x xs]
    (if (vector? x) 
      (let [[name doc routes & optional-stats] x]
        (eval-template (eval `(simple-template ~name ~doc ~routes ~base-stats ~@optional-stats))))
      (eval-template x))))

;;these are basic policies that we can easily derive other policies
;;from.
(deftemplates {:startstate reset
               :endstate   available
               :overlap    45 
               :recovery   90
               :bogbudget  365
               :maxbog     365
               :maxdwell   +inf+
               :maxMob     365                   
               :mindwell   0}
  [ac12          "AC 1:2 template for MCU"       (route-by ac12-waits default-routing)]
  [ac12-enabler  "AC 12 template for enablers"   (route-by ac12-waits default-routing)  :overlap 30]
  [ac13          "AC 1:3 template for MCU"       (route-by ac13-waits default-routing)  :overlap 45]
  [ac13-enabler  "AC 13 template for MCU"        (route-by ac13-waits default-routing)  :overlap 30]
  [ac11          "AC 1:1 template for MCU"       (route-by ac11-waits default-routing)  :overlap 0 ])

(deftemplates   {:startstate reset
                 :endstate   available
                 :overlap    45 
                 :recovery   90
                 :bogbudget  270
                 :maxbog     270
                 :maxdwell   +inf+
                 :maxMob     270                   
                 :mindwell   0 }
  [rc11           "RC 1:1 template for MCU"       (route-by rc11-waits  rc-routing)  :overlap 45]
  [rc11-enabler   "RC 1:1 template for enablers"  (route-by rc11-waits  rc-routing)  :overlap 30]   
  [rc12           "RC 1:2 template for MCU"       (route-by rc12-waits  rc-routing)  :overlap 45]
  [rc12-enabler   "RC 1:2 template for enablers"  (route-by rc12-waits  rc-routing)  :overlap 30]     
  [rc14           "RC 1:4 template for MCU"       (route-by rc14-waits  rc-routing)  :overlap 45]
  [rc14-enabler   "RC 1:4 template for enablers"  (route-by rc14-waits  rc-routing)  :overlap 30]
  [rc15           "RC 1:5 template for MCU"       (route-by rc15-waits  rc-routing)  :overlap 45]
  [rc15-enabler   "RC 1:5 template for enablers"  (route-by rc15-waits  rc-routing)  :overlap 30]
  [rc14-remob     "Template for rc that enables multiple mobs" (route-by rc14-waits  rc-routing)
   :overlap 45 :recovery 365 :bogbudget (* 270 2) :maxmob 95]
  [rc14-remob-enabler     "Template for rc that enables multiple mobs" (route-by rc14-waits  rc-routing)
   :overlap 45 :recovery 365 :bogbudget (* 270 2) :maxmob 95])

(deftemplates {:startstate Deployable
               :endstate   ReturnToDeployable
               :overlap    45 
               :recovery   90
               :bogbudget  365
               :maxbog     365
               :maxdwell   +inf+
               :maxMob     270                   
               :mindwell   0 }
  [ghost           "Ghost template"              (route-by ghost-waits  ghost-routing) :overlap 45]
  [ghost-enabler   "Ghost template for enablers" (route-by ghost-waits  ghost-routing) :overlap 30])

(deftemplates 
  {:overlap         45 
   :recovery        90
   :bogbudget       365
   :maxdwell        +inf+
   :mindwell        0 
   :cyclelength     +inf+
   :maxbog          365
   :startdeployable 0
   :stopdeployable  +inf+
   :startstate      reset 
   :endstate        available}
  [max-utilization              "Max Utilization policy for AC 45" (route-by max-util-waits  default-routing)]
  [max-utilization-enabler      "Max Utilization policy for AC 30" (route-by max-util-waits  default-routing) :overlap 30]
  [near-max-utilization         "Max Utilization policy for RC 30" (route-by max-util-waits  default-routing) :bogbudget 270]
  [near-max-utilization-enabler "Max Utilization policy for RC 30" (route-by max-util-waits  default-routing) :overlap 30 :bogbudget 270])

;;Constructor for building policy instances ...
;;We want to flexibly create Marathon policies .....
;;A policy:
;;defines the structure of a Rotational lifecycle ...
;;Location transitions, time spent at locations, etc.
;;edges on a graph, where nodes are states (locations) and weights are time
;;At least one location must be the starting point for a cycle, one is the end.
;;defines parameters for availability
;;Deployment windows (Megan;;s parameter)
;;Strict vs. Non-Strict (single day available/lifecycle vs. else)
;;defines
;;#TODO Policy cycle lengths default to +inf+, we really want to
;;derive the length of the policy...
(defn register-template [name maxdwell mindwell maxbog startdeployable stopdeployable & {:keys [overlap deltas deployable-set]}]
  (if-let [ctor (get @templates name (get @templates (keyword name)))]
    (let [stats      {:maxdwell maxdwell :mindwell mindwell :maxbog maxbog :startdeployable startdeployable :stopdeployable stopdeployable}
          stats      (if overlap (assoc stats :overlap overlap) stats)
          base       (ctor :deltas deltas :stats stats)
          baselength (core/compute-cycle-length base)
          _          (println [name (min baselength +inf+)])]
      (-> base
          (core/set-deployable startdeployable stopdeployable)
          (assoc  :cyclelength (min baselength +inf+))))
    (throw (Exception. (str "Unknown template: " name)))))

;;Possible vestigial design cruft....we may be able to unify this and
;;remove excess....
(defn register-ghost-template [name  maxbog & {:keys [overlap]}]
   (ghost :name name :stats {:maxbog maxbog :maxdwell +inf+ :mindwell 0 :startdeployable 0 :stopdeployable +inf+}))

;;computes the cycle length assuming startnode and end-node are
;;adjacent members of a cycle.
(defn cycle-length [g startnode endnode]
  (when-let [w (graph/arc-weight g endnode startnode)]
    (-> (graph/depth-first-search g startnode endnode {:weightf graph/arc-weight})
        (:distance)
        (get endnode)
        (+ w))))      

;;adding a bunch of default templates, these are basically just
;;aliases....

(def aliases
  {"AC12"  ac12
   "AC13"  ac13
   "AC11"  ac11
   "RC14" rc14
   "RC15" rc15 
   "RC12" rc12 
   "Ghost" ghost
   "RC14ReMob" rc14-remob
   "MaxUtilization" max-utilization
   "NearMaxUtilization" near-max-utilization
   ;temporarily aliased until I get them ported....
   "ACFFG" ac13
   "RCFFG" ac13
   "FFGMission" ac11
   "RCOpSus" rc14
   })


(doseq [[k ctor] aliases]
   (swap! templates merge 
          {k ctor
           (keyword k) ctor}))
      
         
 

    
;;(defn from-template [name deltas stats];;
(comment 



Public Function FromTemplate(name As String, Optional overlap As Long, Optional deltas As Dictionary, _
                                Optional recoverytime As Long, Optional maxbog As Long, Optional mindwell As Long) As TimeStep_Policy
Select Case name
    Case Is = AC12
        Set FromTemplate = AC12Template(AC12, overlap, deltas)
    Case Is = AC13
        Set FromTemplate = AC13Template(AC13, overlap, deltas)
    Case Is = AC11
        Set FromTemplate = AC11Template(AC11, overlap, deltas)
    Case Is = RC14
        Set FromTemplate = RC14Template(RC14, overlap, deltas)
    Case Is = RC15
        Set FromTemplate = RC15Template(RC15, overlap, deltas)
    Case Is = RC11
        Set FromTemplate = RC11Template(RC11, overlap, deltas)
    Case Is = RC12
        Set FromTemplate = RC12Template(RC12, overlap, deltas)
    Case Is = RC14ReMob
        Set FromTemplate = RC14ReMobTemplate(RC14ReMob, overlap, deltas, recoverytime)
        ;;Added 24 July 2012
    Case Is = MarathonEnumsAndConstants.FFGMission ;;Added 10 Sep 2012
        Set FromTemplate = FFGMissionTemplate(name, overlap, deltas)
    Case Is = MarathonEnumsAndConstants.ACFFG
        Set FromTemplate = ACFFGTemplate(name, overlap, deltas)
    Case Is = MarathonEnumsAndConstants.RCFFG
        Set FromTemplate = RCFFGTemplate(name, overlap, deltas)
    Case Is = MarathonEnumsAndConstants.RCOpSus
        Set FromTemplate = RCOpSusTemplate(name, overlap, deltas)
    Case MarathonEnumsAndConstants.MaxUtilization, MarathonEnumsAndConstants.NearMaxUtilization
        Set FromTemplate = maxUtilizationTemplate(name, CSng(maxbog), CSng(overlap), CSng(mindwell))
    Case Else
        Err.Raise 101, , "Template " & name & " does not exist!"
End Select

With FromTemplate
    If .startstate = vbNullString Then .startstate = reset
    If .endstate = vbNullString Then .endstate = available
    
    ;;Err.Raise 101, , "Fix the line below, need to update cyclelength!"
    ;;.cyclelength = .PositionGraph.pathlength(.PositionGraph.getPath(.PositionGraph.FindCycle(.StartState, .EndState)))
    .cyclelength = cycleSearch(.PositionGraph, makeDepthFringe(), .startstate).distance(.startstate)
    If .cyclelength = 0 Then Err.Raise 101, , "Cycle length is 0, check your rotational policy!"
End With

End Function




;;TOM Hack 24 july 2012
;;Mission Pool template
Public Function FFGMissionTemplate(name As String, overlap As Long, Optional deltas As Dictionary) As TimeStep_Policy
Set FFGMissionTemplate = New TimeStep_Policy
With FFGMissionTemplate
    .overlap = overlap
    .name = name
    .AddPosition reset, "Dwelling", available, "Dwelling", deployed, "Bogging", Overlapping, "Overlapping", _
                    "Deployable", "Deployable", "NotDeployable", "NotDeployable"
    .AddRoute reset, "Deployable", 0 + getdelta("Deployable", deltas)
    .AddRoute "Deployable", available, 0
    .AddRoute available, "NotDeployable", inf
    .AddRoute "NotDeployable", reset, inf + getdelta(available, deltas)
    .AddRoute deployed, Overlapping, 365 - overlap + getdelta(deployed, deltas)
    .AddRoute Overlapping, reset, overlap + getdelta(Overlapping, deltas)
    ;;TOM Change 27 Sep 2012
    .startDeployable = inf
End With
;;policyname, from, to, time, state
End Function

;;TOM Hack 24 July 2012
;;template for AC Future Force Generation policies, we use the parameters to grow and shrink pools
Public Function ACFFGTemplate(name As String, overlap As Long, Optional deltas As Dictionary) As TimeStep_Policy
Set ACFFGTemplate = New TimeStep_Policy
With ACFFGTemplate
    .overlap = overlap
    .name = name
    .AddPosition reset, "Dwelling", train, "Dwelling", ready, "Dwelling", available, "Dwelling", deployed, "Bogging", Overlapping, "Overlapping"

    .AddRoute reset, train, 91 + getdelta(available, deltas)
    .AddRoute train, ready, 91 + getdelta(train, deltas)
    .AddRoute ready, available, 183 + getdelta(ready, deltas)
    .AddRoute available, reset, 365 + getdelta(available, deltas)
    .AddRoute deployed, Overlapping, 365 - overlap + getdelta(deployed, deltas)
    .AddRoute Overlapping, reset, overlap + getdelta(Overlapping, deltas)
End With
;;policyname, from, to, time, state
End Function

;;TOM Hack 24 July 2012
;;template for RC Future Force Generation policies, we use the parameters to grow and shrink pools
Public Function RCFFGTemplate(name As String, overlap As Long, Optional deltas As Dictionary) As TimeStep_Policy
Set RCFFGTemplate = RC14Template(name, overlap, deltas)
End Function

;;TOM Hack 24 July 2012 -> Operational and Sustainment template.
;;The difference with the O&S policy, is that upon returning from deployment, they should not go back
;;to O&S, they should remain in roto status.  Another difference is that they;;re Deployable window is
;;shorter than an equivalent RC....and it takes longer for any of them to deploy.
Public Function RCOpSusTemplate(name As String, overlap As Long, Optional deltas As Dictionary) As TimeStep_Policy
Set RCOpSusTemplate = New TimeStep_Policy
Dim os As String
os = "OS_"
With RCOpSusTemplate
    .MaxMOB = 95
    .overlap = overlap
    .name = name
    .AddPosition os & reset, "Dwelling", os & train, "Dwelling", os & ready, "Dwelling", os & available, "Dwelling", _
                 deployed, "Bogging", Overlapping, "Overlapping", demobilization, "DeMobilizing", _
                    "Promoting", "CheckPromotion"
    .AddRoute os & reset, os & train, 365 + getdelta(reset, deltas)
    .AddRoute os & train, os & ready, 365 + getdelta(train, deltas)
    .AddRoute os & ready, os & available, 730 + getdelta(ready, deltas)
    .AddRoute os & available, os & reset, 365 + getdelta(available, deltas)
    .AddRoute deployed, "Promoting", 0
    .AddRoute "Promoting", Overlapping, 270 - overlap + getdelta(deployed, deltas)
    ;;TOM Change 13 July 2011
    ;;.AddRoute Overlapping, Reset, overlap + getdelta(Overlapping, deltas)
    .AddRoute Overlapping, demobilization, overlap + getdelta(ready, deltas)
;;    .AddRoute demobilization, reset, 95 + getdelta(ready, deltas)
    ;;This is different than the RC14 policy.  After deploying, units do not go back to OS_Reset....
    ;;Instead, units go to a promotion state, where they get change policies to the prevailing default RC policy.
    .AddRoute demobilization, os & reset, 95 + getdelta(ready, deltas)
    .startstate = os & reset
    .endstate = os & available
End With
End Function


;;We define a way to read policy specs....


;;TOM TODO ->
;;Need a more declarative way to do this, the numerical values hide what;;s going on in the function
;;TOM NOTE 21 MAr 2011 -> Need to parametrically vary these.  I was screwing up the DeployableStart
;;and DeployableStart by winging it.  Basically a data error.
Public Function DefaultArforgenPolicies() As Dictionary

Dim pol
Dim policy As TimeStep_Policy
Dim policies As Dictionary

Set policies = New Dictionary

Set policy = RegisterTemplate(AC12, 365 * 3, 365 * 2, 365, 365 * 2, 365 * 2 + 1, 45)
;;RegisterPolicyLocations policy
policy.name = "AC12Strict"
policies.add policy.name, policy

;;Changed from +- 90, to +- 180
Set policy = RegisterTemplate(AC12, 365 * 3, 365 * 2, 365, 365 * 2 - 180, 365 * 2 + 180, 45)
;;RegisterPolicyLocations policy
policy.name = "AC12"
policies.add policy.name, policy

;;units can go until end of available
Set policy = RegisterTemplate(AC12, 365 * 3, 365, 365, 365 * 1, 1095, 45)
policy.name = "AC12Loose"
policies.add policy.name, policy


Set policy = RegisterTemplate(AC13, 365 * 3, 825, 270, 825, 825 + 1, 45)
;;RegisterPolicyLocations policy
policy.name = "AC13Strict"
policies.add policy.name, policy

;;change +-180
Set policy = RegisterTemplate(AC13, 365 * 3, 825, 270, 825 - 180, 825 + 180, 45)
;;RegisterPolicyLocations policy
policy.name = "AC13"
policies.add policy.name, policy

;;made this actually loose.
Set policy = RegisterTemplate(AC13, 365 * 3, 365, 270, 365, 365 * 3, 45)
policy.name = "AC13Loose"
policies.add policy.name, policy


Set policy = RegisterTemplate(AC11, 365 * 2, 365, 365, 365, 365 * 2, 0) ;;0 overlap
;;RegisterPolicyLocations policy
policies.add policy.name, policy

Set policy = RegisterTemplate(RC14, 365 * 5, 365 * 2, 270, 365 * 4, 365 * 4 + 1, 45)
;;RegisterPolicyLocations policy
policy.name = "RC14Strict"
policies.add policy.name, policy

;;+- 180
Set policy = RegisterTemplate(RC14, 365 * 5, 365 * 2, 270, 365 * 4 - 180, 365 * 4 + 180, 45)
;;RegisterPolicyLocations policy
policy.name = "RC14"
policies.add policy.name, policy

Set policy = RegisterTemplate(RC14, 365 * 5, 365 * 2, 270, 365 * 2, 365 * 5 - 90, 45)
policy.name = "RC14Loose"
policies.add policy.name, policy


Set policy = RegisterTemplate(RC15, 365 * 6, 365 * 3, 270, 365 * 5, 365 * 5 + 1, 45)
;;RegisterPolicyLocations policy
policy.name = "RC15Strict"
policies.add policy.name, policy

;;+- 180
Set policy = RegisterTemplate(RC15, 365 * 6, 365 * 3, 270, 365 * 5 - 180, 365 * 5 + 180, 45)
;;RegisterPolicyLocations policy
policy.name = "RC15"
policies.add policy.name, policy

Set policy = RegisterTemplate(RC15, 365 * 6, 365 * 3, 270, 365 * 5 - 90, 365 * 5 + 90, 45)
policy.name = "RC15Loose"
policies.add policy.name, policy

;;This is the RC surge policy...
;;Note -> changed to 0 overlap for surge.
;;TOM Change 13 July 2011
Set policy = RegisterTemplate(RC12, 365 * 3, 365, 270, 365 * 2, (365 * 3) - 90, 0)
policy.name = "RC12"
policies.add policy.name, policy

Set policy = policies(AC12).clone
policy.name = GhostPermanent12
policies.add policy.name, policy

Set policy = policies(AC13).clone
policy.name = GhostPermanent13
policies.add policy.name, policy

Set policy = RegisterGhostTemplate("Ghost365_45", 365, 45)
policies.add policy.name, policy

Set policy = RegisterGhostTemplate("Ghost270_45", 270, 45)
policies.add policy.name, policy

Set policy = RegisterGhostTemplate("BOGForever", inf, 0)
policies.add policy.name, policy


;;;;Enabler policies....i.e. 30 day overlap

Set policy = RegisterTemplate(AC13, 365 * 3, 825, 270, 825 - 180, 825 + 180, 30)
policy.name = "AC13_Enabler"
policies.add policy.name, policy

;;units can go until end of available
Set policy = RegisterTemplate(AC12, 365 * 3, 365, 365, 365 * 1, 1095, 30)
policy.name = "AC12Loose_Enabler"
policies.add policy.name, policy

Set policy = RegisterTemplate(RC15, 365 * 6, 365 * 3, 270, 365 * 5 - 180, 365 * 5 + 180, 30)
;;RegisterPolicyLocations policy
policy.name = "RC15_Enabler"
policies.add policy.name, policy

Set policy = RegisterTemplate(RC14, 365 * 5, 365 * 2, 270, 365 * 2, 365 * 5 - 90, 30)
policy.name = "RC14Loose_Enabler"
policies.add policy.name, policy

Set policy = RegisterGhostTemplate("Ghost365_30", 365, 30)
policy.name = "Ghost365_30"
policies.add policy.name, policy

Set policy = Nothing

;;Set DefaultArforgenPolicies = listVals(policies)
Set DefaultArforgenPolicies = policies
Set policies = Nothing

End Function
;;
Public Function TFPolicies() As Dictionary
Dim policy As TimeStep_Policy
Set TFPolicies = New Dictionary

;;This is a special policy adapted for T.F;;s study.
;;RC has an extra year of availability to deploy.  I don;;t think it will matter.
Set policy = RegisterTemplate(RC14, 365 * 6, 365 * 2, 270, 365 * 2, 365 * 6 - 90, 45, _
                                newdict(available, 365))
policy.name = "RC14Loose_3Year"
TFPolicies.add policy.name, policy

;;This is a special policy adapted for T.F.;;s study.
Set policy = RegisterTemplate(RC14ReMob, 365 * 7, 365 * 2, 270, 365 * 2, 365 * 7 - 90, 45, _
                                newdict(available, 730))
policy.name = RC14ReMob
TFPolicies.add policy.name, policy

;;This is a special policy adapted for T.F;;s study.
;;RC has an extra year of availability to deploy.  I don;;t think it will matter.
Set policy = RegisterTemplate(RC14, 365 * 6, 365 * 2, 270, 365 * 2, 365 * 6 - 90, 30, _
                                newdict(available, 365))
policy.name = "RC14Loose_3Year_Enabler"
TFPolicies.add policy.name, policy

;;This is a special policy adapted for T.F.;;s study.
    Set policy = RegisterTemplate(RC14ReMob, 365 * 7, 365 * 2, 270, 365 * 2, 365 * 7 - 90, 30, _
                                newdict(available, 730))
policy.name = RC14ReMob & "_Enabler"
TFPolicies.add policy.name, policy

End Function

;;Integrated 10 Sep 2012
;;TOM Hack! 24 July 2012 -> this is a temporary patch.  There;;s no reason this shouldn;;t be data driven...blah
Public Function FFGPolicies() As Dictionary
Set FFGPolicies = New Dictionary

Dim policy As TimeStep_Policy

Set policy = ACFFGTemplate("FFGACRoto", 45)
FFGPolicies.add policy.name, policy

Set policy = ACFFGTemplate("FFGACRoto_Enabler", 30)
FFGPolicies.add policy.name, policy


Set policy = RCFFGTemplate("FFGRCRoto", 45)
FFGPolicies.add policy.name, policy


Set policy = RCFFGTemplate("FFGRCRoto_Enabler", 45)
FFGPolicies.add policy.name, policy


Set policy = FFGMissionTemplate("FFGMission", 45)
FFGPolicies.add policy.name, policy


Set policy = FFGMissionTemplate("FFGMission_Enabler", 30)
FFGPolicies.add policy.name, policy


Set policy = MarathonPolicy.RCOpSusTemplate("RCOpSus", 45)
FFGPolicies.add policy.name, policy


Set policy = MarathonPolicy.RCOpSusTemplate("RCOpSus_Enabler", 30)
FFGPolicies.add policy.name, policy


;;9999999 730 9999999 730 9999999 30  0   Auto    {}

End Function


Public Function MaxUtilizationPolicies() As Dictionary
Set MaxUtilizationPolicies = New Dictionary
Dim policy As TimeStep_Policy

Set policy = MarathonPolicy.maxUtilizationTemplate("MaxUtilization", 365, 45, 0)
MaxUtilizationPolicies.add policy.name, policy

Set policy = MarathonPolicy.maxUtilizationTemplate("MaxUtilization_Enabler", 365, 30, 0)
MaxUtilizationPolicies.add policy.name, policy

Set policy = MarathonPolicy.maxUtilizationTemplate("NearMaxUtilization", 270, 45, 730)
MaxUtilizationPolicies.add policy.name, policy

Set policy = MarathonPolicy.maxUtilizationTemplate("NearMaxUtilization_Enabler", 270, 30, 730)
MaxUtilizationPolicies.add policy.name, policy

End Function


)
;;
;;
;;;;create some new policies. Let;;s see if we can interactively build this badboy
;;Public Sub tst()
;;Dim p As TimeStep_Policy
;;Set p = New TimeStep_Policy
;;
;;End Sub


;;;;This is part of a reorganization of a lot of the embedded functionality in the early object oriented
;;;;implementation for marathon.  All functions in this module either consume no arguments, to produce
;;;;policies, or modify existing policies in some way.  Most are a port from TimeStep_Policy
;;Private Type routerec
;;  source As String
;;  dest As String
;;  distance As Single
;;End Type
;;Dim Key
;;Option Explicit
;;Public Function create(positions As Dictionary, routes As Dictionary) As TimeStep_Policy
;;Set create = New TimeStep_Policy
;;End Function
;;Private Function addPositions(positions As Dictionary, targetpolicy As TimeStep_Policy) As TimeStep_Policy
;;Set addPositions = targetpolicy
;;
;;With addPositions
;;    For Each Key In positions.keys
;;        .AddPosition CStr(Key), CStr(positions(Key))
;;    Next Key
;;End With
;;End Function
;;;;expects a dictionary with triples as keys
;;Private Function getRoute(routes As Dictionary, Key As String) As routerec
;;;;Dim rt As Collection
;;;;With getRoute
;;
;;End Function
;;Private Function addRoutes(routes As Dictionary, targetpolicy As TimeStep_Policy) As TimeStep_Policy
;;;;Set addRoutes = targetpolicy
;;;;With addRoutes
;;;;    For Each key In routes.keys
;;End Function
;;
;;
;;;;Policy  Type    Schedule    Path    ExpectedBOG ExpectedDwell   Overlap ExpectedCycleLength TimeInterval
;;Public Function fromRecord(inrec As GenericRecord) As TimeStep_Policy
;;
;;End Function
;;Public Function fromPolicy(inpolicy As TimeStep_Policy) As TimeStep_Policy
;;Set fromPolicy = inpolicy.clone
;;;;make changes
;;End Function
;;Private Function getdelta(position As String, deltas As Dictionary) As Long
;;getdelta = 0
;;If Not (deltas Is Nothing) Then
;;    If deltas.exists(position) Then
;;        getdelta = deltas(position)
;;    End If
;;End If
;;
;;End Function
;;;;TOM Hack 24 July 2012-> This is a good idea.
;;;;Describe a base policy with recovery built in.
;;Public Function recoverableTemplate(Optional recoverytime As Single) As TimeStep_Policy
;;Set recoverableTemplate = New TimeStep_Policy
;;
;;If recoverytime = 0 Then recoverytime = 90
;;
;;With recoverableTemplate
;;    .name = "Recoverable"
;;    .AddPosition recovery, Recovering, Recovered, Recovered
;;    .AddRoute recovery, Recovered, recoverytime
;;End With
;;
;;End Function
;;;;template for AC policies, we use the parameters to grow and shrink pools
;;Public Function AC12Template(name As String, overlap As Long, Optional deltas As Dictionary) As TimeStep_Policy
;;Set AC12Template = New TimeStep_Policy
;;With AC12Template
;;    .overlap = overlap
;;    ;;.AlterPositions ("AC")
;;    .name = name
;;    .AddPosition reset, "Dwelling", train, "Dwelling", ready, "Dwelling", available, "Dwelling", deployed, "Bogging", Overlapping, "Overlapping"
;;    .AddRoute reset, train, 182 + getdelta(available, deltas)
;;    .AddRoute train, ready, 183 + getdelta(train, deltas)
;;    .AddRoute ready, available, 365 + getdelta(ready, deltas)
;;    .AddRoute available, reset, 365 + getdelta(available, deltas)
;;    .AddRoute deployed, Overlapping, 365 - overlap + getdelta(deployed, deltas)
;;    .AddRoute Overlapping, reset, overlap + getdelta(Overlapping, deltas)
;;End With
;;;;policyname, from, to, time, state
;;End Function
;;Public Function AC13Template(name As String, overlap As Long, Optional deltas As Dictionary) As TimeStep_Policy
;;Set AC13Template = New TimeStep_Policy
;;With AC13Template
;;    .overlap = overlap
;;    ;;.AlterPositions ("AC")
;;    .name = name
;;    .AddPosition reset, "Dwelling", train, "Dwelling", ready, "Dwelling", available, "Dwelling", deployed, "Bogging", Overlapping, "Overlapping"
;;    .AddRoute reset, train, 182 + getdelta(reset, deltas)
;;    .AddRoute train, ready, 183 + getdelta(train, deltas)
;;    .AddRoute ready, available, 460 + getdelta(ready, deltas)
;;    .AddRoute available, reset, 270 + getdelta(available, deltas)
;;    .AddRoute deployed, Overlapping, 270 - overlap + getdelta(deployed, deltas)
;;    .AddRoute Overlapping, reset, overlap + getdelta(Overlapping, deltas)
;;End With
;;End Function
;;Public Function AC11Template(name As String, overlap As Long, Optional deltas As Dictionary) As TimeStep_Policy
;;Set AC11Template = New TimeStep_Policy
;;With AC11Template
;;    .overlap = overlap
;;    ;;.AlterPositions ("AC")
;;    .name = name
;;    .AddPosition reset, "Dwelling", train, "Dwelling", available, "Dwelling", deployed, "Bogging", Overlapping, "Overlapping"
;;    .AddRoute reset, train, 182 + getdelta(reset, deltas)
;;    .AddRoute train, available, 183 + getdelta(train, deltas)
;;    .AddRoute available, reset, 365 + getdelta(available, deltas)
;;    .AddRoute deployed, Overlapping, 365 - overlap + getdelta(deployed, deltas)
;;    .AddRoute Overlapping, reset, overlap + getdelta(Overlapping, deltas)
;;End With
;;End Function
;;Public Function RC14Template(name As String, overlap As Long, Optional deltas As Dictionary) As TimeStep_Policy
;;Set RC14Template = New TimeStep_Policy
;;With RC14Template
;;    .MaxMOB = 95
;;    .overlap = overlap
;;    ;;.AlterPositions ("RC")
;;    .name = name
;;    .AddPosition reset, "Dwelling", train, "Dwelling", ready, "Dwelling", available, "Dwelling", _
;;                 deployed, "Bogging", Overlapping, "Overlapping", demobilization, "DeMobilizing"
;;    .AddRoute reset, train, 365 + getdelta(reset, deltas)
;;    .AddRoute train, ready, 365 + getdelta(train, deltas)
;;    .AddRoute ready, available, 730 + getdelta(ready, deltas)
;;    .AddRoute available, reset, 365 + getdelta(available, deltas)
;;    .AddRoute deployed, Overlapping, 270 - overlap + getdelta(deployed, deltas)
;;
;;    ;;TOM Change 13 July 2011
;;    ;;.AddRoute Overlapping, Reset, overlap + getdelta(Overlapping, deltas)
;;    .AddRoute Overlapping, demobilization, overlap + getdelta(ready, deltas)
;;    .AddRoute demobilization, reset, 95 + getdelta(ready, deltas)
;;End With
;;End Function
;;;;TOM Note 21 Mar 2011 -> Double check the lengths on these policies...
;;Public Function RC15Template(name As String, overlap As Long, Optional deltas As Dictionary) As TimeStep_Policy
;;Set RC15Template = New TimeStep_Policy
;;With RC15Template
;;    .MaxMOB = 95
;;    .overlap = overlap
;;    ;;.AlterPositions ("RC")
;;    .name = name
;;    .AddPosition reset, "Dwelling", train, "Dwelling", ready, "Dwelling", available, "Dwelling", deployed, "Bogging", Overlapping, "Overlapping", _
;;        demobilization, "DeMobilizing"
;;    .AddRoute reset, train, 730 + getdelta(reset, deltas)
;;    .AddRoute train, ready, 365 + getdelta(train, deltas)
;;    .AddRoute ready, available, 730 + getdelta(ready, deltas)
;;    .AddRoute available, reset, 365 + getdelta(ready, deltas)
;;    .AddRoute deployed, Overlapping, 270 - overlap + getdelta(deployed, deltas)
;;        ;;TOM Change 13 July 2011
;;    ;;.AddRoute Overlapping, Reset, overlap + getdelta(Overlapping, deltas)
;;    .AddRoute Overlapping, demobilization, overlap + getdelta(ready, deltas)
;;    .AddRoute demobilization, reset, 95 + getdelta(ready, deltas)
;;End With
;;End Function
;;Public Function RC11Template(name As String, overlap As Long, Optional deltas As Dictionary) As TimeStep_Policy
;;Set RC11Template = New TimeStep_Policy
;;With RC11Template
;;    .MaxMOB = 95
;;    .overlap = overlap
;;    ;;.AlterPositions ("RC")
;;    .name = name
;;    .AddPosition reset, "Dwelling", train, "Dwelling", available, "Dwelling", deployed, "Bogging", Overlapping, "Overlapping", _
;;        demobilization, "DeMobilizing"
;;    .AddRoute reset, train, 182 + getdelta(reset, deltas)
;;    .AddRoute train, available, 183 + getdelta(train, deltas)
;;    .AddRoute available, reset, 365 + getdelta(ready, deltas)
;;    .AddRoute deployed, Overlapping, 270 - overlap + getdelta(deployed, deltas)
;;        ;;TOM Change 13 July 2011
;;    ;;.AddRoute Overlapping, Reset, overlap + getdelta(Overlapping, deltas)
;;    .AddRoute Overlapping, demobilization, overlap + getdelta(ready, deltas)
;;    .AddRoute demobilization, reset, 95 + getdelta(ready, deltas)
;;End With
;;End Function
;;Public Function RC12Template(name As String, overlap As Long, Optional deltas As Dictionary) As TimeStep_Policy
;;Set RC12Template = New TimeStep_Policy
;;
;;With RC12Template
;;    .MaxMOB = 95
;;    .overlap = overlap
;;    ;;.AlterPositions ("RC")
;;    .name = name
;;    .AddPosition reset, "Dwelling", train, "Dwelling", available, "Dwelling", deployed, "Bogging", Overlapping, "Overlapping", _
;;        demobilization, "DeMobilizing"
;;    .AddRoute reset, train, 365 + getdelta(reset, deltas)
;;    .AddRoute train, available, 365 + getdelta(train, deltas)
;;    .AddRoute available, reset, 365 + getdelta(available, deltas)
;;
;;    .AddRoute deployed, Overlapping, 270 - overlap + getdelta(deployed, deltas)
;;
;;    ;;TOM Change 13 July 2011
;;    ;;.AddRoute Overlapping, Reset, overlap + getdelta(Overlapping, deltas)
;;    .AddRoute Overlapping, demobilization, overlap + getdelta(ready, deltas)
;;    .AddRoute demobilization, reset, 95 + getdelta(ready, deltas)
;;End With
;;
;;End Function
;;Public Function GhostTemplate(name As String, bog As Single, overlap As Single) As TimeStep_Policy
;;Set GhostTemplate = New TimeStep_Policy
;;With GhostTemplate
;;    .overlap = overlap
;;    ;;.AlterPositions ("Ghost")
;;    .name = name
;;    .AddPosition "Spawning", "Spawning", "Deployable", "Deployable", "Waiting", "Nothing", _
;;                "Deploying", "Deploying", "NotDeployable", "NotDeployable", _
;;                    deployed, "Bogging", Overlapping, "Overlapping", _
;;                            "BehaviorChange", "BehaviorChange", "ReturnToDeployable", "Nothing"
;;    .AddRoute "Spawning", "Deployable", 0
;;    .AddRoute "Deployable", "Waiting", 0
;;    .AddRoute "Waiting", "Deploying", 9999999
;;    .AddRoute "Deploying", "NotDeployable", 0
;;    .AddRoute "NotDeployable", deployed, 0
;;    .AddRoute deployed, Overlapping, bog - overlap
;;    .AddRoute Overlapping, "ReturnToDeployable", overlap
;;    .AddRoute "ReturnToDeployable", "Deployable", 0
;;    .startstate = "Deployable"
;;    .endstate = "ReturnToDeployable"
;;    .cyclelength = 9999999
;;End With
;;
;;End Function
;;;;template for RC policies with a remob time, we use the parameters to grow and shrink pools
;;;;Allows 2 deployments.  Recovery time dictates the amount of time spent in between bogs.
;;Public Function RC14ReMobTemplate(name As String, overlap As Long, Optional deltas As Dictionary, _
;;                                    Optional recoverytime As Long, Optional bogbudget As Long) As TimeStep_Policy
;;Set RC14ReMobTemplate = New TimeStep_Policy
;;If recoverytime = 0 Then recoverytime = 365
;;If bogbudget = 0 Then bogbudget = 270 * 2 ;;default to 2 deployments
;;With RC14ReMobTemplate
;;    .MaxMOB = 95
;;    .overlap = overlap
;;    ;;.AlterPositions ("RC")
;;    .name = name
;;    .AddPosition reset, "Dwelling", train, "Dwelling", ready, "Dwelling", available, "Dwelling", _
;;                 deployed, "Bogging", Overlapping, "Overlapping", demobilization, "DeMobilizing", _
;;                    recovery, Recovering, Recovered, Recovered
;;    .AddRoute reset, train, 365 + getdelta(reset, deltas)
;;    .AddRoute train, ready, 365 + getdelta(train, deltas)
;;    .AddRoute ready, available, 730 + getdelta(ready, deltas)
;;    .AddRoute available, reset, 365 + getdelta(available, deltas)
;;    .AddRoute deployed, Overlapping, 270 - overlap + getdelta(deployed, deltas)
;;
;;    ;;TOM Change 13 July 2011
;;    ;;.AddRoute Overlapping, Reset, overlap + getdelta(Overlapping, deltas)
;;    ;;.AddRoute Overlapping, demobilization, overlap + getdelta(ready, deltas)
;;;;    .AddRoute demobilization, reset, 95 + getdelta(ready, deltas)
;;    .AddRoute Overlapping, recovery, overlap + getdelta(Overlapping, deltas)
;;    .AddRoute recovery, Recovered, CSng(recoverytime)
;;    .AddRoute Recovered, demobilization, 95 + getdelta(ready, deltas)
;;    .AddRoute demobilization, reset, 0
;;    .bogbudget = bogbudget
;;End With
;;;;policyname, from, to, time, state
;;End Function
;;
;;;;TOM Hack 24 July 2012
;;Public Function maxUtilizationTemplate(name As String, bog As Single, overlap As Single, mindwell As Single) As TimeStep_Policy
;;Set maxUtilizationTemplate = New TimeStep_Policy
;;With maxUtilizationTemplate
;;    .name = name
;;    .overlap = overlap
;;    .name = name
;;    .AddPosition reset, "Dwelling", available, "Dwelling", deployed, "Bogging", Overlapping, "Overlapping", _
;;        "Deployable", "Deployable", "NotDeployable", "NotDeployable"
;;    .AddRoute reset, "Deployable", mindwell
;;    .AddRoute "Deployable", available, 0
;;    .AddRoute available, "NotDeployable", 9999999
;;    .AddRoute "NotDeployable", reset, 0
;;    .AddRoute deployed, Overlapping, bog - overlap
;;    .AddRoute Overlapping, reset, overlap
;;    .cyclelength = 9999999
;;    .MaxBOG = bog
;;    .mindwell = mindwell
;;    .MaxDwell = 9999999
;;    .startDeployable = mindwell
;;    .stopDeployable = 9999999
;;    .startstate = reset
;;    .endstate = available
;;End With
;;
;;End Function
;;;;MaxUtilization
;;;;ACFFG
;;;;RCOpSus
;;;;RCFFG
;;
;;;;TOM Hack 24 july 2012
;;;;Mission Pool template
;;Public Function FFGMissionTemplate(name As String, overlap As Long, Optional deltas As Dictionary) As TimeStep_Policy
;;Set FFGMissionTemplate = New TimeStep_Policy
;;With FFGMissionTemplate
;;    .overlap = overlap
;;    .name = name
;;    .AddPosition reset, "Dwelling", available, "Dwelling", deployed, "Bogging", Overlapping, "Overlapping"
;;    .AddRoute reset, available, 0 + getdelta(reset, deltas)
;;    .AddRoute available, reset, 9999999 + getdelta(available, deltas)
;;    .AddRoute deployed, Overlapping, 365 - overlap + getdelta(deployed, deltas)
;;    .AddRoute Overlapping, reset, overlap + getdelta(Overlapping, deltas)
;;    .startDeployable = 9999999
;;End With
;;;;policyname, from, to, time, state
;;End Function
;;
;;;;TOM Hack 24 July 2012
;;;;template for AC Future Force Generation policies, we use the parameters to grow and shrink pools
;;Public Function ACFFGTemplate(name As String, overlap As Long, Optional deltas As Dictionary) As TimeStep_Policy
;;Set ACFFGTemplate = New TimeStep_Policy
;;With ACFFGTemplate
;;    .overlap = overlap
;;    .name = name
;;    .AddPosition reset, "Dwelling", train, "Dwelling", ready, "Dwelling", available, "Dwelling", deployed, "Bogging", Overlapping, "Overlapping"
;;    .AddRoute reset, train, 91 + getdelta(available, deltas)
;;    .AddRoute train, ready, 91 + getdelta(train, deltas)
;;    .AddRoute ready, available, 183 + getdelta(ready, deltas)
;;    .AddRoute available, reset, 365 + getdelta(available, deltas)
;;    .AddRoute deployed, Overlapping, 365 - overlap + getdelta(deployed, deltas)
;;    .AddRoute Overlapping, reset, overlap + getdelta(Overlapping, deltas)
;;End With
;;;;policyname, from, to, time, state
;;End Function
;;;;TOM Hack 24 July 2012
;;;;template for RC Future Force Generation policies, we use the parameters to grow and shrink pools
;;Public Function RCFFGTemplate(name As String, overlap As Long, Optional deltas As Dictionary) As TimeStep_Policy
;;Set RCFFGTemplate = RC14Template(name, overlap, deltas)
;;End Function
;;
;;;;TOM Hack 24 July 2012 -> Operational and Sustainment template.
;;;;The difference with the O&S policy, is that upon returning from deployment, they should not go back
;;;;to O&S, they should remain in roto status.  Another difference is that they;;re Deployable window is
;;;;shorter than an equivalent RC....and it takes longer for any of them to deploy.
;;Public Function RCOpSusTemplate(name As String, overlap As Long, Optional deltas As Dictionary) As TimeStep_Policy
;;Set RCOpSusTemplate = New TimeStep_Policy
;;Dim os As String
;;os = "OS_"
;;With RCOpSusTemplate
;;    .MaxMOB = 95
;;    .overlap = overlap
;;    .name = name
;;    .AddPosition os & reset, "Dwelling", os & train, "Dwelling", os & ready, "Dwelling", os & available, "Dwelling", _
;;                 deployed, "Bogging", Overlapping, "Overlapping", demobilization, "DeMobilizing", _
;;                    "Promotion", "PolicyChange"
;;    .AddRoute os & reset, os & train, 365 + getdelta(reset, deltas)
;;    .AddRoute os & train, os & ready, 365 + getdelta(train, deltas)
;;    .AddRoute os & ready, os & available, 730 + getdelta(ready, deltas)
;;    .AddRoute os & available, os & reset, 365 + getdelta(available, deltas)
;;    .AddRoute deployed, Overlapping, 270 - overlap + getdelta(deployed, deltas)
;;    ;;TOM Change 13 July 2011
;;    ;;.AddRoute Overlapping, Reset, overlap + getdelta(Overlapping, deltas)
;;    .AddRoute Overlapping, demobilization, overlap + getdelta(ready, deltas)
;;;;    .AddRoute demobilization, reset, 95 + getdelta(ready, deltas)
;;    ;;This is different than the RC14 policy.  After deploying, units do not go back to OS_Reset....
;;    ;;Instead, units go to a promotion state, where they get change policies to the prevailing default RC policy.
;;    .AddRoute demobilization, "Promotion", 95 + getdelta(ready, deltas)
;;    .startstate = os & reset
;;    .endstate = os & available
;;End With
;;End Function
;;
;;
;;;;create some new policies. Let;;s see if we can interactively build this badboy
;;Public Sub tst()
;;Dim p As TimeStep_Policy
;;Set p = New TimeStep_Policy
;;
;;End Sub









;;-----------PORTED
;; (defn ac13-routes [overlap]
;;   [[reset train 182]
;;    [train ready 183]
;;    [ready available 460]
;;    [available reset 270]
;;    [deployed Overlapping (- 270 overlap)]
;;    [Overlapping reset overlap]])
     

;;These are default policy routes for RC entities in arforgen.
;;Used as scaffolding for templates.
;; (defn rc-routes [overlap demob & [deltas]]
;;   [[reset train          (max 1 (get-deltas reset deltas))]
;;    [train ready          (max 1 (get-deltas train deltas))]
;;    [ready available      (max 1 (get-deltas ready deltas))]
;;    [available reset      (max 1 (get-deltas available deltas))]
;;    [deployed overlapping (max 1 (- (get-deltas deployed deltas)  overlap))]
;;    [overlapping demobilization (max 1 (+ overlap (get-deltas ready deltas)))]
;;    [demobilization reset (max 1 (+ demob  (get-deltas ready deltas)))]])

;;These are default routes for ghost entities. they are significantly different than AC/RC
;;Used as scaffolding for templates.
;; (defn ghost-routes [& {:keys [bog overlap deltas] :or {bog 0 overlap 0 deltas 0}}]
;;   [[Spawning Deployable 0]
;;    [Deployable Waiting 0]
;;    [Waiting deploying +inf+]
;;    [deploying notDeployable 0]
;;    [notDeployable deployed 0]
;;    [deployed Overlapping (- bog overlap)]
;;    [Overlapping ReturnToDeployable overlap]
;;    [ReturnToDeployable Deployable 0]]) 


;; (defn ac13-template [name overlap & {:keys [deltas]}]
;;     (-> (policydata/make-policy :name name :overlap overlap)
;;         (core/add-positions default-positions)
;;         (core/add-routes [[reset train (+ 182  (get-delta reset deltas))]
;;                           [train ready (+ 183  (get-delta train deltas))]
;;                           [ready available (+ 460  (get-delta ready deltas))]
;;                           [available reset (+ 270  (get-delta available deltas))]
;;                           [deployed Overlapping (+ (- 270 overlap)  (get-delta deployed deltas))]
;;                           [Overlapping reset (+ overlap (get-delta Overlapping deltas))]])))



;; (defn ac-routes [overlap & [deltas]]
;;   [[reset train (max 1 (get-deltas available deltas))]
;;    [train ready (max 1 (get-deltas train deltas))]
;;    [ready available (max 1 (get-deltas ready deltas))]
;;    [available reset (max 1 (get-deltas available deltas))]
;;    [deployed overlapping (max 1 (- (get-deltas deployed deltas)  overlap))]
;;    [overlapping reset (max 1 (get-deltas overlapping deltas))]])

;; (defn rc-routes [overlap demob & [deltas]]
;;   [[reset train          (max 1 (get-deltas reset deltas))]
;;    [train ready          (max 1 (get-deltas train deltas))]
;;    [ready available      (max 1 (get-deltas ready deltas))]
;;    [available reset      (max 1 (get-deltas available deltas))]
;;    [deployed overlapping (max 1 (- (get-deltas deployed deltas)  overlap))]
;;    [overlapping demobilization (max 1 (+ overlap (get-deltas ready deltas)))]
;;    [demobilization reset (max 1 (+ demob  (get-deltas ready deltas)))]])

;;These are default routes for ghost entities. they are significantly different than AC/RC
;;Used as scaffolding for templates.
;; (defn ghost-routes [& {:keys [bog overlap deltas] :or {bog 0 overlap 0 deltas 0}}]
;;   [[Spawning Deployable 0]
;;    [Deployable Waiting 0]
;;    [Waiting deploying +inf+]
;;    [deploying notDeployable 0]
;;    [notDeployable deployed 0]
;;    [deployed Overlapping (- bog overlap)]
;;    [Overlapping ReturnToDeployable overlap]
;;    [ReturnToDeployable Deployable 0]]) 


;; (defn ac11-template [name overlap & {:keys [deltas]}]
;;   (-> (policydata/make-policy :name name :overlap overlap)
;;       (core/add-positions default-positions)
;;       (core/add-routes [[reset train (+ 182  (get-delta reset deltas))]
;;                         [train ready (+ 183  (get-delta train deltas))]
;;                         [available reset (+ 365  (get-delta available deltas))]
;;                         [deployed Overlapping (+ (- 365 overlap)  (get-delta deployed deltas))]
;;                         [Overlapping reset (+ overlap (get-delta Overlapping deltas))]])))


;; (defn rc14-template [name overlap & {:keys [deltas]}]
;;   (-> (policydata/make-policy :name name :overlap overlap :MaxMOB 95)
;;       (generic/add-positions default-rc-positions)
;;       (add-routes [[reset train (+ 365  (get-delta reset deltas))]
;;                    [train ready (+ 365  (get-delta train deltas))]
;;                    [ready available (+ 730  (get-delta ready deltas))]
;;                    [available reset (+ 365  (get-delta available deltas))]
;;                    [deployed Overlapping (+ (- 270 overlap)  (get-delta deployed deltas))]
;;                    [Overlapping demobilization (+ overlap (get-delta Overlapping deltas))]
;;                    [demobilization reset   (+ 95 (get-delta demobilization deltas))]
;;                    ])))

;; (defn rc15-template [name overlap deltas]
;;     (-> (policydata/make-policy :name name :overlap overlap :MaxMOB 95)
;;         (generic/add-positions default-rc-positions)
;;         (add-routes [[reset train (+ 730  (get-delta reset deltas))]
;;                      [train ready (+ 365  (get-delta train deltas))]
;;                      [ready available (+ 730  (get-delta ready deltas))]
;;                      [available reset (+ 365  (get-delta available deltas))]
;;                      [deployed Overlapping (+ (- 270 overlap)  (get-delta deployed deltas))]
;;                      [Overlapping demobilization (+ overlap (get-delta Overlapping deltas))]
;;                      [demobilization reset   (+ 95 (get-delta demobilization deltas))]
;;                      ])))
  
;; (defn rc11-template [name overlap deltas]
;;     (-> (policydata/make-policy :name name :overlap overlap :MaxMOB 95)
;;         (generic/add-positions default-rc-positions)
;;         (add-routes [[reset train (+ 182  (get-delta reset deltas))]
;;                      [train available (+ 183  (get-delta train deltas))]
;;                      [available reset (+ 365  (get-delta available deltas))]
;;                      [deployed Overlapping (+ (- 270 overlap)  (get-delta deployed deltas))]
;;                      [Overlapping demobilization (+ overlap (get-delta Overlapping deltas))]
;;                      [demobilization reset   (+ 95 (get-delta demobilization deltas))]
;;                      ])))

;(def default-rc-positions (assoc default-positions demobilization "DeMobilizing"))


;; Public Function RC12Template(name As String, overlap As Long, Optional deltas As Dictionary) As TimeStep_Policy
;; Set RC12Template = New TimeStep_Policy

;; With RC12Template
;;     .MaxMOB = 95
;;     .overlap = overlap
;;     ;;.AlterPositions ("RC")
;;     .name = name
;;     .AddPosition reset, "Dwelling", train, "Dwelling", available, "Dwelling", deployed, "Bogging", Overlapping, "Overlapping", _
;;         demobilization, "DeMobilizing"
;;     .AddRoute reset, train, 365 + getdelta(reset, deltas)
;;     .AddRoute train, available, 365 + getdelta(train, deltas)
;;     .AddRoute available, reset, 365 + getdelta(available, deltas)
    
;;     .AddRoute deployed, Overlapping, 270 - overlap + getdelta(deployed, deltas)
    
;;     ;;TOM Change 13 July 2011
;;     ;;.AddRoute Overlapping, Reset, overlap + getdelta(Overlapping, deltas)
;;     .AddRoute Overlapping, demobilization, overlap + getdelta(ready, deltas)
;;     .AddRoute demobilization, reset, 95 + getdelta(ready, deltas)
;; End With

;; End Function

;; (defn rc12-template [name overlap deltas]
;;     (-> (policydata/make-policy :name name :overlap overlap :MaxMOB 95)
;;         (generic/add-positions default-rc-positions)
;;         (add-routes [[reset train (+ 182  (get-delta reset deltas))]
;;                      [train available (+ 183  (get-delta train deltas))]
;;                      [available reset (+ 365  (get-delta available deltas))]
;;                      [deployed Overlapping (+ (- 270 overlap)  (get-delta deployed deltas))]
;;                      [Overlapping demobilization (+ overlap (get-delta Overlapping deltas))]
;;                      [demobilization reset   (+ 95 (get-delta demobilization deltas))]
;;                      ])))


;; Public Function RC12Defaults(overlap As Long) As Dictionary
;; Set RC12Defaults = _
;;     newdict(reset, 365, train, 365, available, 365, _
;;                 deployed, 270 - overlap, Overlapping, overlap, demobilization, 95)
;; End Function

;; Public Function GhostTemplate(name As String, bog As Single, overlap As Single) As TimeStep_Policy
;; Set GhostTemplate = New TimeStep_Policy
;; With GhostTemplate
;;     .overlap = overlap
;;     ;;.AlterPositions ("Ghost")
;;     .name = name
;;     .AddPosition "Spawning", "Spawning", "Deployable", "Deployable", "Waiting", "Nothing", _
;;                 "Deploying", "Deploying", "NotDeployable", "NotDeployable", _
;;                     deployed, "Bogging", Overlapping, "Overlapping", _
;;                             "BehaviorChange", "BehaviorChange", "ReturnToDeployable", "Nothing"
;;     .AddRoute "Spawning", "Deployable", 0
;;     .AddRoute "Deployable", "Waiting", 0
;;     .AddRoute "Waiting", "Deploying", inf
;;     .AddRoute "Deploying", "NotDeployable", 0
;;     .AddRoute "NotDeployable", deployed, 0
;;     .AddRoute deployed, Overlapping, bog - overlap
;;     .AddRoute Overlapping, "ReturnToDeployable", overlap
;;     .AddRoute "ReturnToDeployable", "Deployable", 0
;;     .startstate = "Deployable"
;;     .endstate = "ReturnToDeployable"
;;     .cyclelength = inf
;; End With

;; End Function



;; Public Function GhostDefaults(bog As Long, overlap As Long) As Dictionary
;; Set GhostDefaults = newdict("Spawning", 0, _
;;                             "Deployable", 0, _
;;                             "Waiting", inf, _
;;                             "Deploying", 0, _
;;                             "NotDeployable", 0, _
;;                             deployed, bog - overlap, _
;;                             Overlapping, overlap, _
;;                             "ReturnToDeployable", 0)
;; End Function


;;template for RC policies with a remob time, we use the parameters to grow and shrink pools
;;Allows 2 deployments.  Recovery time dictates the amount of time spent in between bogs.
;; Public Function RC14ReMobTemplate(name As String, overlap As Long, Optional deltas As Dictionary, _
;;                                     Optional recoverytime As Long, Optional bogbudget As Long) As TimeStep_Policy
;; Set RC14ReMobTemplate = New TimeStep_Policy
;; If recoverytime = 0 Then recoverytime = 365
;; If bogbudget = 0 Then bogbudget = 270 * 2 ;;default to 2 deployments
;; With RC14ReMobTemplate
;;     .MaxMOB = 95
;;     .overlap = overlap
;;     ;;.AlterPositions ("RC")
;;     .name = name
;;     .AddPosition reset, "Dwelling", train, "Dwelling", ready, "Dwelling", available, "Dwelling", _
;;                  deployed, "Bogging", Overlapping, "Overlapping", demobilization, "DeMobilizing", _
;;                     recovery, Recovering, Recovered, Recovered
;;     .AddRoute reset, train, 365 + getdelta(reset, deltas)
;;     .AddRoute train, ready, 365 + getdelta(train, deltas)
;;     .AddRoute ready, available, 730 + getdelta(ready, deltas)
;;     .AddRoute available, reset, 365 + getdelta(available, deltas)
;;     .AddRoute deployed, Overlapping, 270 - overlap + getdelta(deployed, deltas)
    
;;     ;;TOM Change 13 July 2011
;;     ;;.AddRoute Overlapping, Reset, overlap + getdelta(Overlapping, deltas)
;;     ;;.AddRoute Overlapping, demobilization, overlap + getdelta(ready, deltas)
;; ;;    .AddRoute demobilization, reset, 95 + getdelta(ready, deltas)
;;     .AddRoute Overlapping, recovery, overlap + getdelta(Overlapping, deltas)
;;     .AddRoute recovery, Recovered, CSng(recoverytime)
;;     .AddRoute Recovered, demobilization, 95 + getdelta(ready, deltas)
;;     .AddRoute demobilization, reset, 0
;;     .bogbudget = bogbudget
;; End With
;; ;;policyname, from, to, time, state
;; End Function



;; ;;TOM Hack 24 July 2012
;; Public Function maxUtilizationTemplate(name As String, bog As Single, overlap As Single, mindwell As Single) As TimeStep_Policy
;; Set maxUtilizationTemplate = New TimeStep_Policy
;; With maxUtilizationTemplate
;;     .name = name
;;     .overlap = overlap
;;     .name = name
;;     .AddPosition reset, "Dwelling", available, "Dwelling", deployed, "Bogging", Overlapping, "Overlapping", _
;;         "Deployable", "Deployable", "NotDeployable", "NotDeployable"
;;     .AddRoute reset, "Deployable", mindwell
;;     .AddRoute "Deployable", available, 0
;;     .AddRoute available, "NotDeployable", inf
;;     .AddRoute "NotDeployable", reset, 0
;;     .AddRoute deployed, Overlapping, bog - overlap
;;     .AddRoute Overlapping, reset, overlap
;;     .cyclelength = inf
;;     .maxbog = bog
;;     .mindwell = mindwell
;;     .maxdwell = inf
;;     .startDeployable = mindwell
;;     .stopDeployable = inf
;;     .startstate = reset
;;     .endstate = available
;; End With

;;End Function

;; Public Function RegisterGhostTemplate(name As String, maxbogdays As Single, Optional overlap As Single) As TimeStep_Policy

;; Set RegisterGhostTemplate = GhostTemplate(name, maxbogdays, overlap)

;; ;;parameterize the policy
;; With RegisterGhostTemplate
;;     .maxdwell = inf
;;     .mindwell = 0
;;     .maxbog = maxbogdays ;;TOM NOTE 21 Mar 2011 -> I think this is backwards
;;     .startdeployable = 0
;;     .stopdeployable = inf
;;     ;;.StartIndex = 9999999 ;;LocatiOnMap(.StartState) ;;TOM TODO fix this .
;;     ;;.EndIndex = LocatiOnMap(.EndState)  ;;TOM TODO fix this.
;; End With

;; End Function


