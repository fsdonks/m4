(ns analysis.random (:require [marathon.analysis :as a]                    
                              [marathon.ces.core :as core]
                              [spork.entitysystem.store :as store]
                              [spork.util.table :as tbl]))

(defn make-recs [rec]
  (for [i (range (:Quantity rec))]
    (if (= (:Component rec) "AC")
      (assoc rec :CycleTime (rand-int 1095) :Quantity 1 :Name (str i "_" (:SRC rec)))
      (assoc rec :CycleTime (rand-int 1825) :Quantity 1 :Name (str i "_" (:SRC rec))))))

(defn get-proj [proj]
  (->> proj
       :tables
       :SupplyRecords
       tbl/table-records
       (mapcat make-recs)
       tbl/records->table
       (assoc-in proj [:tables :SupplyRecords])))

(defn get-demand [sim-stream]
  (->> sim-stream
  (map second)
  (map a/demand-trends)
  (remove nil?)
  (remove #(= % '()))))

(defn get-required [demand-data]
  (map #(reduce + (map :TotalRequired %)) demand-data))

(defn get-filled [demand-data]
  (map #(reduce + (map :Deployed %)) demand-data))

(defn get-dt [demand-data stream]
  (let [dt (->> demand-data
           (map first)
           (map :t)
           (partition 2 1)
           (map #(- (second %) (first %)))
           (into []))
        last-sample-day (:t (first (last demand-data)))
        last-sim-day (first (last stream))]
  (conj dt (- last-sim-day last-sample-day))))

(defn get-fill [demand-data stream]
  (let [required (get-required demand-data)
        filled (get-filled demand-data)
        dt (get-dt demand-data stream)  
        total-time (reduce + dt)
        weights (map / dt (repeat total-time))]
    (float
     (/ (reduce + (map * weights filled))
        (reduce + (map * weights required))))))

(defn get-fills [reps proj]
  (for [i (range reps)]
    (let [new-proj (get-proj proj)
          ctx (a/load-context new-proj)
          stream (a/marathon-stream ctx)
          demand (get-demand stream)]
      (get-fill demand stream))))

(defn mean [xs]
  (/ (reduce + xs) (count xs)))

;(def path "/home/keith/repos/notional/supplyvariation-testdata.xlsx")
;(def proj (a/load-project path))
;(def fills (get-fills 10 proj))
;(def mean (mean fills))
