;;Rotational policy data definitions.  Both atomic and composite policies are 
;;represented here.
;;The general idea is that rotational policies correspond to state
;;transitions.  Positions in the policy have associated state data.
;;We can define complex policies by composing primitive policies
;;together using sequences and policies defined by period (or event
;;really...).  Policies conform to three basic protocols that 
;;define policy queries and alterations.  Also, policies have 
;;state transitions represented as a position graph, a directed 
;;graph with special properties.  
(ns marathon.policy.policydata
  (:use [spork.util.record :only [defrecord+]])
  (:require [marathon.data  [protocols :as core]]
            [spork.cljgraph [core :as graph]]))

;__TODO__ Extend core/IRotationPolicy protocol to policy and
;policycomposite..

;a structure for unit entity policies.
(defrecord+ policy [[name "BlankPolicy"]
                    [cyclelength core/+inf+]
                    [mindwell 0]
                    [maxdwell  core/+inf+]
                    [maxbog    core/+inf+]
                    [maxMOB    core/+inf+]
                    [recovery  90]
                    [startdeployable 0]
                    [stopdeployable core/+inf+]
                    [positiongraph  graph/empty-graph]
                    [startstate    :spawn]
                    [endstate      :spawn]
                    [overlap 45]]
  core/IRotationPolicy 
  (atomic-name         [p] name)
  (bog-budget          [p] maxbog)
  (get-active-policy   [p] p)
  (get-policy          [p period] p)
  (policy-name         [p] name)
  (next-position       [p position] (first (graph/sinks positiongraph position)))
  (overlap             [p] overlap) 
  (get-position-graph  [p] positiongraph)  
  (previous-position    [p position] (first (graph/sources positiongraph position)))
  (start-deployable     [p] startdeployable)
  (stop-deployable      [p] stopdeployable)
  (start-state          [p] startstate)
  (transfer-time    [p start-position end-position] (graph/arc-weight positiongraph start-position end-position))
  (cycle-length     [p] cyclelength)
  (end-state        [p] endstate)
  (get-cycle-time   [p position] (if (= position startstate) 0 
                                     (-> (graph/depth-first-search positiongraph startstate position {:weightf graph/arc-weight})
                                         (get :distance)
                                         (get position))))                                          
  (get-policy-type  [p] :atomic)
  (get-position     [p cycletime] (loop [pos startstate
                                         t   0]
                                    (if-let [nxt (first (graph/sinks positiongraph pos))]
                                      (let [tnxt (+ t   (long (graph/arc-weight positiongraph pos nxt)))]
                                        (if (>= tnxt cycletime) pos
                                            (if (= nxt startstate) ;we looped around without finding it..
                                              (throw (Exception. (str ["Cycletime exceeds policy, looped! " t name])))
                                              (recur nxt tnxt))))
                                      (throw (Exception. "Cycletime exceeds policy!")))))                                    
  (get-state        [p position]   (graph/get-node positiongraph position))
  (max-bog          [p]            maxbog)
  (max-dwell        [p]            maxdwell)
  (max-mob          [p]            maxMOB)
  (min-dwell        [p]            mindwell)
  (get-locations    [p]           (graph/get-node-labels positiongraph))
  core/IAlterablePolicy
  (set-deployable       [p tstart tfinal] (-> p 
                                              (core/insert-modifier tstart {:name :deployable})
                                              (core/insert-modifier tfinal {:name :non-deployable})
                                              (core/mark-deployable-region)))
  (set-deployable-start [p cycletime]  (core/insert-modifier p cycletime :name :deployable))
  (set-deployable-stop  [p cycletime]  (core/insert-modifier p cycletime :name :non-deployable))
  (set-position-graph   [p g]          (assoc p :positiongraph g)) 
  (add-position         [p name state]
      (let [stateset (or (graph/get-node positiongraph name) #{})]
        (->> (if (set? state)
               (into stateset state)
               (conj stateset state))
             (graph/conj-node positiongraph name)
             (assoc p :positiongraph))))
  (add-route            [p start destination transfer-time] 
      (assoc p :positiongraph (graph/conj-arc positiongraph start destination transfer-time)))
  (merge-policy-stats   [p stats] (merge p stats)))

(def empty-policy (make-policy))
;policies defined by more than one atomic policy.
;;Debate turning policies away from a map....so we can support more
;;than one policy composition type based off of data structure used to 
;;contain the policies.
(defrecord+ policymap [name
                       ^marathon.data.protocols.IRotationPolicy activepolicy 
                       activeperiod
                       [policies {}]]
  core/IRotationPolicy 
  (atomic-name         [p] (.atomic-name activepolicy))
  (bog-budget          [p] (.bog-budget activepolicy))
  (get-active-policy   [p] activepolicy)
  (get-policy          [p period] (get policies period))
  (policy-name         [p] name)
  (next-position       [p position] (.next-position activepolicy position))
  (overlap             [p] (.overlap activepolicy))
  (get-position-graph  [p] (.get-position-graph activepolicy))
  (previous-position   [p position] (.previous-position activepolicy position))
  (start-deployable    [p] (.start-deployable activepolicy))
  (stop-deployable     [p] (.stop-deployable activepolicy))
  (start-state         [p] (.start-state activepolicy))
  (transfer-time       [p start-position end-position] (.transfer-time activepolicy start-position end-position))
  (cycle-length        [p] (.cycle-length activepolicy))
  (end-state           [p] (.end-state activepolicy))
  (get-cycle-time      [p position] (.get-cycle-time activepolicy position))     
  (get-policy-type     [p] :composite)
  (get-position        [p cycletime] (.get-position activepolicy cycletime))                                    
  (get-state           [p position]  (.get-state activepolicy position))
  (max-bog             [p]            (.max-bog activepolicy))
  (max-dwell           [p]            (.max-dwell activepolicy))
  (max-mob             [p]            (.max-mob activepolicy))
  (min-dwell           [p]            (.min-dwell activepolicy))
  (get-locations       [p]            (.get-locations activepolicy))
  core/IPolicyContainer
  (add-policy          [p period policy]   (policymap. name (or activepolicy policy) (or activeperiod period) (assoc policies period policy)))
  (add-policy          [p keyval] (if (coll? keyval) 
                                    (let [[k v] keyval] (.add-policy p k v))
                                    (throw (Exception. "Expected a [period policy] pair for arity 2 add-policy on a policymap")))))

(def empty-policymap (make-policymap))

;;Defines a policy that scripts a sequence of policies, starting from
;;a root policy.  We might want to have a policy offset...depends on
;;how I'm using this in the code.  I think unit behavior was
;;interpreting policy, keeping track of its current state in the policy.
(defrecord+ policyseq [name
                       ^marathon.data.protocols.IRotationPolicy rootpolicy   
                       [idx 0]
                       [policies []]]
  core/IRotationPolicy 
  (atomic-name         [p] (.atomic-name rootpolicy))
  (bog-budget          [p] (.bog-budget rootpolicy))
  (get-active-policy   [p] rootpolicy)
  (get-policy          [p period] (get policies period))
  (policy-name         [p] name)
  (next-position       [p position] (.next-position rootpolicy position))
  (overlap             [p] (.overlap rootpolicy))
  (get-position-graph  [p] (.get-position-graph rootpolicy))
  (previous-position   [p position] (.previous-position rootpolicy position))
  (start-deployable    [p] (.start-deployable rootpolicy))
  (stop-deployable     [p] (.stop-deployable rootpolicy))
  (start-state         [p] (.start-state rootpolicy))
  (transfer-time       [p start-position end-position] (.transfer-time rootpolicy start-position end-position))
  (cycle-length        [p] (.cycle-length rootpolicy))
  (end-state           [p] (.end-state rootpolicy))
  (get-cycle-time      [p position] (.get-cycle-time rootpolicy position))     
  (get-policy-type     [p] :sequential)
  (get-position        [p cycletime]  (.get-position rootpolicy cycletime))                                    
  (get-state           [p position]   (.get-state rootpolicy position))
  (max-bog             [p]            (.max-bog rootpolicy))
  (max-dwell           [p]            (.max-dwell rootpolicy))
  (max-mob             [p]            (.max-mob rootpolicy))
  (min-dwell           [p]            (.min-dwell rootpolicy))
  (get-locations       [p]            (.get-locations rootpolicy))
  core/IPolicyContainer
  (add-policy       [p period policy]   (.add-policy p policy))
  (add-policy       [p policy]  (assert (satisfies? core/IRotationPolicy policy) 
                                        "expected a rotation policy for add-policy arity 2 on a policyseq")
                                (policyseq. name (or rootpolicy policy) idx  (conj policies policy))))

(def empty-policyseq (make-policyseq))

