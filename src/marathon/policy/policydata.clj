;;Rotational policy data definitions.  Both atomic and composite policies are 
;;represented here.
(ns marathon.policy.policydata
  (:use [spork.util.record :only [defrecord+]])
  (:require [marathon.data [protocols :as core]]
            [spork.cljgraph [core :as graph]]))

;__TODO__ Extend core/IRotationPolicy protocol to policy and
;policycomposite..

;;Generic ops on position graphs in policies.
(defn insert-modifier [policy cycletime & {:keys [name] :or {name :modified}]]
  (let [x     (get-position policy cycletime)
        nxt   (core/next-position policy x)                              
        tprev (-> (graph/depth-first-search (core/get-position-graph policy) (core/start-state policy) position)
                  (get :distance)
                  (get x))
        offset (- cycletime tprev)
        dnxt   (- (graph/arc-weight x nxt) offset) ]                          
    (core/set-position-graph policy
       (-> positiongraph 
           (graph/disj-arc x nxt) 
           (graph/conj-arc x [name x] offset)
           (graph/conj-arc [name x]  nxt dnxt)))))

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
                    [positiongraph  graph/empty-graph]
                    [startstate :spawn]
                    [endstate :spawn]
                    [overlap 45]]
  core/IRotationPolicy 
  (atomic-name         [p] name)
  (bog-budget          [p] maxbog)
  (get-active-policy   [p] p)
  (get-policy          [p period] p)
  (policy-name         [p] name)
  (next-position       [p position] (first (graph/sinks positiongraph)))
  (overlap             [p] overlap) 
  (get-position-graph  [p] positiongraph)  
  (set-position-graph   [p g] (assoc p :positiongraph g)) 
  (previous-position    [p position] (first (graph/sources positiongraph)))
  (set-deployable       [p tstart tfinal] (-> p 
                                              (insert-modifier tstart :name :deployable)
                                              (insert-modifier tstop  :name :non-deployable)))
  (set-deployable-start [p cycletime]  (insert-modifier p cycletime :name :deployable))
  (set-deployable-stop  [p cycletime]  (insert-modifier p cycletime :name :non-deployable))
  (start-deployable     [p] startdeployable)
  (stop-deployable      [p] stopdeployable)
  (start-state          [p] startstate)
  (transfer-time    [p start-position end-position] (graph/arc-weight positiongraph start-position end-position))
  (add-position     [p name state] (assoc p :positiongraph (graph/conj-node positiongraph name state)))
  (add-route        [p start destination transfer-time] (assoc p :positiongraph (graph/conj-arc start destination transfer-time)))
  (cycle-length     [p] cyclelength)
  (deployable?      [p position] (core/deployable-state? (graph/get-node positiongraph position)))
  (end-state        [p] endstate)
  (get-cycle-time   [p position] (if (= position startstate) 0 
                                     (-> (graph/depth-first-search positiongraph startstate position)
                                         (get :distance)
                                         (get position))))                                          
  (get-policy-type  [p] :atomic)
  (get-position     [p cycletime] (loop [pos startstate
                                         t   0]
                                    (if-let [nxt (first (graph/sinks positiongraph pos))]
                                      (let [tnxt (+ t (graph/arc-weight pos nxt))]
                                        (if (>= tnxt cycletime) acc
                                            (recur nxt tnxt)))
                                      (throw (Excption. "Cycletime exceeds policy!")))))                                    
  (get-state        [p position]   (graph/get-node positiongraph position))
  (deployable?      [p cycletime]  (core/deployable? (.get-position p cycletime)))
  (dwell?           [p position]   (core/dwell?  (graph/get-node positiongraph position)))
  (max-bog          [p]            maxbog)
  (max-dwell        [p]            maxdwell)
  (max-mob          [p]            maxmob)
  (min-dwell        [p]            mindwell)
  (get-locations    [p]            (graph/get-node-labels positiongraph))
  (add-policy       [p policy]     (throw (Exception. "Cannot add policies to atomic policies!"))))

(def empty-policy (make-policy))

;;Boiler plate reduction stuff.  This lets us provide shell
;;implementations in the composite policy, and expands out into the
;;actual error being thrown.  Should not be necessary, but hey, be defensive.
(defmacro atomic-mod-err []
  `(throw (Exception. "Atomic policies may not be modified inside of composite policies!")))

;policies defined by more than one atomic policy.
;;Debate turning policies away from a map....so we can support more
;;than one policy composition type based off of data structure used to 
;;contain the policies.
(defrecord+ policycomposite [name
                             activepolicy
                             activeperiod
                             [policies {}]]
  core/IRotationPolicy 
  (atomic-name         [p] (core/atomic-name activepolicy))
  (bog-budget          [p] (core/bog-budget activepolicy))
  (get-active-policy   [p] activepolicy)
  (get-policy          [p period] (get policies period))
  (policy-name         [p] name)
  (next-position       [p position] (core/next-position activepolicy position))
  (overlap             [p] (core/overlap activepolicy))
  (get-position-graph  [p] (core/get-position-graph activepolicy))
  (set-position-graph   [p g] (atomic-mod-err))
  (previous-position    [p position] (core/previous-position activepolicy position))
  (set-deployable       [p tstart tfinal] (atomic-mod-err))
  (set-deployable-start [p cycletime]  (atomic-mod-err))
  (set-deployable-stop  [p cycletime]  (atomic-mod-err))
  (start-deployable     [p] (core/start-deployable activepolicy))
  (stop-deployable      [p] (core/stop-deployable activepolicy))
  (start-state          [p] (core/start-state activepolicy))
  (transfer-time    [p start-position end-position] (core/transfer-time activepolicy start-position end-position))
  (add-position     [p name state] (atomic-mod-err))
  (add-route        [p start destination transfer-time] (atomic-mod-err))
  (cycle-length     [p] (core/cycle-length activepolicy))
  (deployable?      [p position] (core/deployable? activepolicy position))
  (end-state        [p] (core/end-state active-policy))
  (get-cycle-time   [p position] (core/get-cycle-time activepolicy position))     
  (get-policy-type  [p] :composite)
  (get-position     [p cycletime] (core/get-position activepolicy))                                    
  (get-state        [p position]  (core/get-state activepolicy))
  (deployable?      [p cycletime]  (core/deployable? activepolicy cycletime))
  (dwell?           [p position]   (core/dwell?  activepolicy position))
  (max-bog          [p]            (core/max-bog activepolicy))
  (max-dwell        [p]            (core/max-dwell activepolicy))
  (max-mob          [p]            (core/max-mov activepolicy))
  (min-dwell        [p]            (core/min-dwell activepolicy))
  (get-locations    [p]            (core/get-locations activepolicy))
  (add-policy       [p policy]     (atomic-mod-err)))

(def empty-composite-policy (make-policycomposite))
