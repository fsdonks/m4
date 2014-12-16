(ns marathon.visuals.core
  (:require [spork.sketch :as sketch]))

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
    
    
