;Post processes a marathon project to determine a set of vectors for unit 
;deployments.  Given a deployments table, scans the table to construct a set
;of deployment intervals or deployment events. 
(ns marathon.processing.deployvectors
  (:require [util [table :as tbl]
                  [general :as general]]))

;our goal is to rip the trends apart into segments, for each unit.
(defrecord deploy-interval [unit
                            first-deployment
                            last-deployment
                            category 
                            duration])

;(defn deployments->unit-histories [deploytable]
;  (let [records-by-unit (group-by #(get % "Unit") 
;                                  (tbl/table-records deploytable))]
(defn keywordize-deploytable [table]
    (tbl/keywordize-field-names table))

(defn demand-record->vector
  "Convert a demand-record to a vector representation of start and duration."
  [rec]  
  [(get rec :DemandStart)  (get rec :Duration)])

(defn vector->interval [v]  [(first v) (+ (first v) (second v))])

(defn within? [interval1 interval2]
  (and (>= (first interval1) (first interval2))
       (<= (second interval1) (second interval2))))

(defn truncate [interval1 interval2]
  (if (within? interval1 interval2)
    interval1
    [(first interval1) (second interval2)]))

(defn bog-interval [tfinal deployrec] 
  (let [demand-interval ((comp vector->interval demand-record->vector) rec)
        t (get  deployrec :DeployInterval)
        bog (get deployrec :BogBudget)
        deploy-interval (truncate [t (+ t bog)] demand-interval)]
    (if (> (second deploy-interval) tfinal)
        [(first deploy-interval) tfinal]
        deploy-interval)))
                          
;how long did the unit deploy for? 
;what was the upper bound on the simulation time? 
;(defn unit-history->deployments [tfinal deployment-recs]
;  (let [ordered-recs (vec (sort-by :DeployInterval deployment-recs))]
;    (reduce (fn [[lastrec deployments i] rec]
;              (let [loc    (:Location acc)
;                    demand (:Demand acc)]
;                (if (not= demand (:Location rec))
;                  [deployment-record (conj deployments 
;                                           (merge deployment-record
;                                                  {:Bog (bog-interval tfinal rec)
;                                                   :DeploymentID (inc i)}))
;                   (inc i)]
;                  [deployment-record (uinc 
;                                                  
;                  
;                  
;))    

  
  
  
    
    

    