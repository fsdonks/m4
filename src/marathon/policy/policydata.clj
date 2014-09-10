;;Rotational policy data definitions.  Both atomic and composite policies are 
;;represented here.
(ns marathon.policy.policydata
  (:use [spork.util.record :only [defrecord+]])
  (:require [marathon.data [protocols :as core]]
            [spork.cljgraph [core :as graph]]))

;__TODO__ Extend core/IRotationPolicy protocol to policy and policycomposite..

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
                    [overlap 45]
                    [subscribers {}]]
  core/IRotationPolicy 
  (atomic-name       [p] name)
  (bog-budget        [p] maxbog)
  (get-active-policy [p] p)
  (get-policy        [p period] p)
  (policy-name       [p] name)
  (next-position     [p position] (first (graph/sinks positiongraph)))
  (overlap           [p] overlap) 
  (position-graph    [p] positiongraph) ;if we have this, we can build the rest... 
  (previous-position    [p position] (first (graph/sources positiongraph)))
  (set-deployable       [p tstart tfinal] )
  (set-deployable-start [p cycletime])
  (set-deployable-stop  [p cycletime])
  (start-deployable     [p] startdeployable)
  (stop-deployable      [p] stopdeployable)
  (start-state          [p] startstate)
  (transfer-time    [p start-position end-position] (graph/arc-weight positiongraph start-position end-position))
  (add-position     [p name state] (assoc p :positiongraph (graph/conj-node positiongraph name state)))
  (add-route        [p start destination transfer-time] (assoc p :positiongraph (graph/conj-arc start destination transfer-time)))
  (cycle-length     [p] cyclelength)
  (deployable?      [p position] (core/deployable-state? (graph/get-node positiongraph position)))
  (end-state        [p])
  (get-cycle-time   [p position])
  (get-policy-type  [p])
  (get-position     [p cycletime])
  (get-state        [p position])
  (deployable?      [p cycletime])
  (dwell?           [p position])
  (max-bog          [p])
  (max-dwell        [p])
  (max-mob          [p])
  (min-dwell        [p])
  (add-policy       [p policy])
  (get-locations    [p])
  )

(def empty-policy (make-policy))

;policies defined by more than one atomic policy.
(defrecord+ policycomposite [name
                             subscribers ;probably drop this field....
                             activepolicy
                             activeperiod
                             [policies {}]])

(def empty-composite-policy (make-policycomposite))
