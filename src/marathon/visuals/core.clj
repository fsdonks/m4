(ns marathon.visuals.core
  (:require [spork.sketch :as sketch]))


;;can we map an entity (location, state, name) to a projection in
;;space? 
;;If an entity is a small circle, what does it look like? 

;;It might be useful to have the notion of a telemetry database.
;;We have position, direction (velocity), and acceleration.
;;For most things (like watching unit entities), acceleration 
;;is constant.  So we have instantaneous changes in direction.

;;I'd like to view entites as particles flowing in a system.
;;The particle flow is dictated by where the entities are 
;;(location).

;;Maybe we set up the coordinates based on the policy 
;;lifecycle and the positions reachable from there. 



;;(Name) ....
;;What is its color? 
;;   Use the same color scheme we have for AC / RC / NG? 
;;How do we derive a location? 
;; 
;;       Location1 Location2 
;;State1
;;State2             (E1)

;;Another option is to color by state...
;;Or change shape by state....
;;State colors sound good...

;;How about have everyone be red for now...
;;Entities are just red circles...
;;Space is defined by boxes, arrayed in a grid.
;;Boxes contain entities. 
;;They grow in proportion to the number of entities they 
;;contain....

;;As entities move from box-to-box, they change locations...
;;Problem -> boxes are sparse, not a full N x K combinations...

;;X axis.....
;;Entities move within a location based on their time there....

;;A cool way to visualize an entity is to plot it on some course....
;;Dual tracks so-to-speak...
;;A policy timeline....simple (where is the entity in policy space?)
;;Reset Ready Ready_Deployable Available Available_NonDeployable
;;0.............................................................N 

;;A state history....(


;;This is a quick turn deal...
;; (defn make-cycle [name]
;;   [{:name :A 
;;     :start 0
;;     :duration 182
;;     :location "Reset"}
;;    {:name :A
;;     :start 182
;;     :duration 183
;;     :location "Train"}
;;    {:name :A
;;     :start 365
;;     :duration 365
;;     :location "Ready"}
;;    {:name :A
;;     :start 730
;;     :duration 365
;;     :location "Available"}])

;; (defn weighted-sample [ws]  
;;   (let [idx (atom (count ws))]
;;     (if (== (count ws) 1)
;;        (first (keys ws))
;;        (reduce-kv (fn [acc v p]
;;                     (cond  (or (<= acc p) (== @idx 1))
;;                            (reduced v)
;;                            :else 
;;                            (do (swap! idx dec)
;;                                (- acc p))))
;;                   (rand) ws))))              

;; (def transitions {:reset     {:train 1.0}
;;                   :train     {:ready 1.0}
;;                   :ready     {:deployed 0.1 :available 0.9}
;;                   :available {:deployed 0.3 :reset 0.7}
;;                   :deployed  {:reset 0.90 :ready 0.05 :available 0.05}})

;; (defn jump-around [state]
;;  (weighted-sample  (get transitions state)))

;; (defn jumps [init] (iterate jump-around init))

;; (defn random-cycle [xs]
;;   (reduce (fn [[xs & [prev] :as acc] {:keys [start duration] :as r}]
;;             (let [tprv (+ (:start prev) (:duration prev))
;;                   jitter (rand-int 180)]
;;               (conj acc (-> r (assoc :start tprv)
;;                               (assoc :duration (+ duration jitter)))
              
         
;;   

;; (def inspect [obj]
;;   (
    
    
