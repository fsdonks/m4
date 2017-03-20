(ns marathon.ces.policyops
  (:require
            [spork.util.metaprogramming :refer [keyvals->constants]]
            [spork.cljgraph.core :as graph]
            [marathon.ces.core :refer [+inf+]]
            [marathon.policy.policydata :as policydata]            
            [marathon.data.protocols :as core :refer
     ;States and policy constants
     [Bogging 
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
      demobilization
      ;;srm locs...
      PB_C3    
      PB_C4     
      PT_C4     
      PL_C4     
      R_C1      
      R_C2      
      MP_DA_C1  
      MP_NDA_C3 
      MA_DA_C1  
      MA_DA_C2  
      MA_NDA_C3 
      MD_DA_C1  
      MD_DA_C2  
      MD_NDA_C3]]))

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
;;a start state at Reset and an end state at Available.

;;It'd be nice to be able to define basic templates 

(def default-positions 
  {reset     #{:dwelling :c5} 
   train     #{:dwelling :c4}
   ready     #{:dwelling :c2} 
   available #{:dwelling :c1}
   deployed    :bogging
   Overlapping :overlapping
   })

(def srm-positions
     ;;SRM Stuff...may change.  I hate SRM.  SRM introduces
  ;;a couple of new states....
  {PB_C3     #{:dwelling :c3 :deployable}
   PB_C4     #{:dwelling :c4}
   PT_C4     #{:dwelling :c4}
   PL_C4     #{:dwelling :c4}
   R_C1      #{:dwelling :c1 :deployable}
   R_C2      #{:dwelling :c2 :deployable}
   MP_DA_C1  #{:mission :c1}  ;;mission state is interpeted by behavior, may bog, may not.
   MP_NDA_C3 #{:mission :c3}  
   MA_DA_C1  #{:mission :c1}
   MA_DA_C2  #{:mission :c2}
   MA_NDA_C3 #{:mission :c3}
   MD_DA_C1  #{:mission :c1}
   MD_DA_C2  #{:mission :c2}
   MD_NDA_C3 #{:mission :c3}})

;;Note: these are all in the routing data; looking for a way
;;to pull this in programatically, although this is pretty easy...
;;we can always stick routing information in script files.
;;SRM specific routing information.  Sustainable readiness is
;;different from ARFORGEN....
(def SRMAC-routes
  (mapv vec
    (partition 3
             [PT_C4	PB_C3	90
              PB_C3	R_C2	55
              R_C2      R_C1	550
              R_C1	PB_C3	35
              ;;these arcs create acyclical processes. 
              ;; MA_DA_C1	[:acyclic  PB_C3]	90
              ;; MA_DA_C2	[:acyclic  PB_C3]	90
              ;; MP_DA_C1	[:acyclic  R_C1]	999999
              MD_DA_C1	 PB_C3	90 ;;Added...2x check
              MA_DA_C1	 PB_C3	90
              MA_DA_C2	 PB_C3	90
              MP_DA_C1	 R_C1	999999
              MA_NDA_C3	PT_C4	90
              MD_NDA_C3	PT_C4	90
              MP_NDA_C3	PT_C4	90
              ])))

(def SRMRC-routes
  (mapv vec
    (partition 3
             [PT_C4	PB_C4	730
              PB_C4	PB_C3	365
              PB_C3	R_C2	365
              R_C2	PT_C4	365
              MA_DA_C1	PT_C4	90
              MA_DA_C2	PT_C4	90
              MA_NDA_C3	PT_C4	90
              MD_NDA_C3	PT_C4	90
              MP_NDA_C3	PT_C4	90
              ;;these arcs create acyclical processes. 
              [:acyclic  PB_C3]	 MA_NDA_C3	90
              [:acyclic  PB_C3]	 MD_NDA_C3	90
              [:acyclic  PB_C3]	 MP_NDA_C3	90
              ])))

;;special for "some" unit types...
(def SRMRC13-routes
  (mapv vec
    (partition 3
            [PT_C4	PB_C4	90 ;;added....should we skip it?
             PB_C4	PB_C3	730
             PB_C3	R_C2	365
             R_C2	PB_C4	365
             MA_DA_C1	PB_C4	90    
             MA_DA_C2	PB_C4	90
             MA_NDA_C3	PT_C4	90 ;;should these be PB_C4?
             MD_NDA_C3	PT_C4	90
             MP_NDA_C3	PT_C4	90
             ;;these arcs create acyclical processes. 
             [:acyclic  PB_C3]	 MA_NDA_C3	90
             [:acyclic  PB_C3]	 MD_NDA_C3	90
             [:acyclic  PB_C3]	 MP_NDA_C3	90
             ])))
  
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

(def max-util-routing [[reset Deployable available reset] 
                       [deployed Overlapping NotDeployable reset]])

;;Routes for the adapt arforgen portion of the ac.
(def adapt-ac-routing
  [[reset train ready available train]  
   [deployed Overlapping reset]])

;;SRM doesn't use the above....we define the policy a bit more directly (for the
;;time being)

(defn max-util-waits [{:keys [overlap maxbog mindwell]}]
  {reset mindwell
   Deployable 0
   available +inf+
   NotDeployable 0
   deployed (- maxbog overlap)
   Overlapping overlap})

(defn waits->routes 
  [wait-times routing]
  (vec (mapcat (fn [xs] (map (fn [[from to]] 
                          [from to (get wait-times from)])
                        (partition 2 1 (filter #(get wait-times %) xs))))   routing)))

 
;;These are wait times based on state transition graphs.
;;We can probably abstract this out further, but for now it'll work...
(defn ac11-waits [{:keys [overlap]}]
  {reset       182 
   train       183 
   available   365
   deployed    (- 365 overlap) 
   Overlapping overlap})

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

;;RC13 Template, missing from legacy port.
(defn rc13-waits [{:keys [overlap]}]
  {reset 365 
   train (* 365 2) 
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
           {routes     'marathon.ces.policyops/default-routes 
            positions  'marathon.ces.policyops/default-positions
            stats      '*stats*}}]
  (let [doc (or doc (str "Policy constructor for " name " takes options [:name :deltas :startstate :endstate :overlap]"))]
    `(do 
       (defn ~name ~doc 
         [& {:keys [~'name ~'deltas ~'stats] 
             :or   {~'name ~(str name) ~'deltas nil}}]       
         (let [stats#  (merge ~stats ~'stats)
               routes# ~routes 
               routes# (if (fn? routes#) (routes# stats#) routes#)]
           (-> (apply policydata/make-policy (unzip (seq (assoc stats# :name ~'name))))
               (core/merge-policy-stats stats#)
               (core/add-positions ~positions)
               (core/add-routes (modified-routes routes# 0 ;(:overlap stats#)
                                                 ~'deltas)))))
         (swap! templates assoc ~(str name) ~name)
         (quote ~name))))

  
;;It may be easier to just push a base policy through multiple
;;transforms, that constitutes a "template"...

(defmacro simple-template 
  ([name doc routes positions base-stats & optional-stats]
     `{:name (quote ~name) :doc ~doc :routes (quote ~routes) :positions ~positions :stats (and-stats ~base-stats ~@optional-stats)})
  ([[name doc routes positions base-stats & optional-stats]]  
     `{:name (quote ~name) :doc ~doc :routes (quote ~routes)  :positions ~positions :stats (and-stats ~base-stats ~@optional-stats)}))

(defn eval-template [td]
  (eval `(deftemplate ~(:name td) ~@(unzip (dissoc td :name)))))

(defmacro deftemplates [base-stats & xs]
  (let [positions (or (:positions base-stats) default-positions)]
    (doseq [x xs]
      (if (vector? x) 
        (let [[name doc routes & optional-stats] x]
          (eval-template (eval `(simple-template ~name ~doc ~routes ~positions ~base-stats ~@optional-stats))))
        (eval-template x)))))

;;__SRM Policies__
(deftemplate SRMAC
  :doc "Default template for Sustainable Readiness Model AC units."
  :routes SRMAC-routes
  :positions srm-positions
  :stats   {:startstate PB_C3
            :endstate   R_C1
            :overlap    0 ;determined by demand
            :recovery   90
            :bogbudget  365
            :maxbog     365
            :maxdwell   730
            :maxMob     365                   
            :mindwell   55})

(deftemplate  SRMRC
  :doc "Default template for Sustainable Readiness Model RC units."
  :routes SRMRC-routes
  :positions srm-positions
  :stats   {:startstate PT_C4
            :endstate   R_C2
            :overlap    0 ;determined by demand
            :recovery   90
            :bogbudget  365
            :maxbog     365
            :maxdwell   1825
            :maxMob     365                   
            :mindwell   1460})

(deftemplate  SRMRC13
  :doc "template for Sustainable Readiness Model RC units that have a 1:3 cycle."
  :routes SRMRC13-routes
  :positions srm-positions
  :stats   {:startstate PB_C4
            :endstate   R_C2
            :overlap    0  ;determined by demand
            :recovery   90
            :bogbudget  365
            :maxbog     365
            :maxdwell   1460
            :maxMob     365                   
            :mindwell   1095})

(defn ac11-waits [{:keys [overlap]}]
  {reset       182 
   train       183 
   available   365
   deployed    (- 365 overlap)
   Overlapping overlap})

;;Policy transitions for adapt arforgen
;;AC stuff.
(defn adaptac-waits [{:keys [overlap]}]
  {reset        95
   train        90
   ready        180
   available    365
   deployed     (- 365 (or overlap 0))
   Overlapping  (or overlap 0)})

;;Adapt ARFORGEN....
(deftemplates 
  {:startstate reset
   :endstate   available   
   :recovery   90
   :bogbudget  270
   :maxbog     270
   :maxdwell   730
   :maxMob     0                   
   :mindwell   95}
  [AdaptAC "template for Adapt ARFORGEN units, following a 24 month lifecycle."
   (route-by adaptac-waits  adapt-ac-routing)
   :overlap 45])

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
                 :mindwell   0}
  [rc11           "RC 1:1 template for MCU"       (route-by rc11-waits  rc-routing)  :overlap 45]
  [rc11-enabler   "RC 1:1 template for enablers"  (route-by rc11-waits  rc-routing)  :overlap 30]   
  [rc12           "RC 1:2 template for MCU"       (route-by rc12-waits  rc-routing)  :overlap 45]
  [rc12-enabler   "RC 1:2 template for enablers"  (route-by rc12-waits  rc-routing)  :overlap 30]
  ;;Added missing legacy templates.
  [rc13           "RC 1:3 template for MCU"       (route-by rc13-waits  rc-routing)  :overlap 45]
  [rc13-enabler   "RC 1:3 template for enablers"  (route-by rc13-waits  rc-routing)  :overlap 30]     
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
               :mindwell   0}
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
   :endstate        available
   :positions      (-> default-positions
                       (dissoc train ready)
                       (merge {available #{:dwelling :deployable :c1}}))
   }
    [max-utilization              "Max Utilization policy for AC 45" (route-by max-util-waits  max-util-routing)]
    [max-utilization-enabler      "Max Utilization policy for AC 30" (route-by max-util-waits  max-util-routing) :overlap   30]
    [near-max-utilization         "Max Utilization policy for RC 30" (route-by max-util-waits  max-util-routing) :bogbudget 270]
    [near-max-utilization-enabler "Max Utilization policy for RC 30" (route-by max-util-waits  max-util-routing) :overlap 30 :bogbudget 270])

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
;;Need to clamp down on the semantically infinite policy numbers.  We're having problems with
;;9999999 vs 999999999, getting errors naturally.  So, for now, we'll clamp down the
;;numbers to ensure the policy is valid.  We'll throw warnings if the numbers are beyond the
;;expected range and interpret them correctly.
(defn clamp-stats [name m]
  (reduce-kv (fn [acc k n]
               (assoc acc k
                      (if (and (not (neg? n))
                               (<= n +inf+))
                        n
                        (if (neg? n) (throw (Exception. (str [:negative-value name m k n])))
                            (do ;(println [:interpreting-as-infinite name k n])
                                +inf+)))))
             m m))

;;Note: if we don't go through this template, we don't get deployable times set,
;;we we end up with policies that can't deploy.  I had the same problem with
;;AdaptAC.
(defn register-template [name maxdwell mindwell maxbog startdeployable stopdeployable & {:keys [overlap deltas deployable-set]}]
  (try  (if-let [ctor (get @templates name (get @templates (keyword name)))]
          (let [stats      {:maxdwell maxdwell :mindwell mindwell :maxbog maxbog :startdeployable startdeployable :stopdeployable stopdeployable}
                stats      (if overlap (assoc stats :overlap overlap) stats)
                stats      (clamp-stats name stats)
                base       (ctor :deltas deltas :stats stats)
                baselength (core/compute-cycle-length base)                
               ]
            (-> base
                (core/set-deployable (:startdeployable stats) (:stopdeployable stats))
                (assoc  :cyclelength (min baselength +inf+))))
          (throw (Exception. (str "Unknown template: " name))))
        (catch Exception e
          (throw (Exception. (str  [:trying   [name maxdwell mindwell maxbog startdeployable stopdeployable]
                                   
                                    :e e]))))))

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

;;Aliasing is causing us problems....we're getting strange
;;run-time errors.  Need to sort this out...
(def aliases
  {"AC12"  ac12
   "AC13"  ac13
   "AC11"  ac11       
   "RC11"  rc11 ;added missing template
   "RC12"  rc12   
   "RC13"  rc13 ;added missing template
   "RC14"  rc14
   "RC15"  rc15   
   "Ghost" ghost
   "RC14ReMob"      rc14-remob
   "RC14Remob"      rc14-remob ;added alias for compatibility
   "MaxUtilization" max-utilization
   "NearMaxUtilization" near-max-utilization
   ;temporarily aliased until I get them ported....
   "ACFFG" ac13
   "RCFFG" rc14
   "FFGMission" ghost
   "RCOpSus" rc14
   "SRMAC"   SRMAC
   "SRMRC"   SRMRC
   "SRMRC13" SRMRC13
   "AdaptAC" AdaptAC ;(AdaptAC "AdaptAC" 730 185 270 185 730 :overlap 0)
   "AdaptRC" RC15
   })

(doseq [[k ctor] aliases]
   (swap! templates merge 
          {k ctor
           (keyword k) ctor})) 
