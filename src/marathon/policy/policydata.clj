;;Rotational policy data definitions.  Both atomic and composite policies are 
;;represented here.
(ns marathon.policy.policydata
  (:use [util.record :only [defrecord+]])
  (:require [marathon.data [protocols :as core]]))

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
                             [policies {}]])

(def empty-composite-policy (make-policycomposite))
