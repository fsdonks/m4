;This name space contains functions for performing targeting model supply variations.
;For each level of supply multiple runs are made, each with random unit initial cycle times.
;For a given level of supply mean statistcis are reported averaged across runs.
(ns marathon.analysis.random (:require [marathon.analysis :as a]
                                       [marathon.analysis.util :as util]
                                       [marathon.analysis.experiment :as e]
                                       [spork.util.table :as tbl]))

(defn rand-recs
  "Takes a supply record with unit quantity n and creates n supply records, each with unit quantity 1 and a random initial cycle time."
  [rec]
  (for [i (range (:Quantity rec))]
    (if (= (:Component rec) "AC")
      (assoc rec :CycleTime (rand-int 1095) :Quantity 1 :Name (str i "_" (:SRC rec)))
      (assoc rec :CycleTime (rand-int 1825) :Quantity 1 :Name (str i "_" (:SRC rec))))))

(defn rand-proj
  "Takes a project and uses the rand-recs function to create supply records with random unit initial cycle times."
  [proj]
  (->> proj
       :tables
       :SupplyRecords
       tbl/table-records
       (mapcat rand-recs)
       tbl/records->table
       (assoc-in proj [:tables :SupplyRecords])))

(defn mean
  "Calculates the mean across a series of maps for the values associated with a particular key. The map is denoted by fills and the key is denoted by key."
  [fills key]
  (/ (reduce + (map key fills)) (count fills)))

(defn means
  "Takes a series of maps and returns a map with mean and representative values."
  [fills]
  (let [mean-fill (mean fills :fill)
        mean-quantity (mean fills :quantity)
        mean-deployable (mean fills :deployable)
        period (:period (first fills))]
    {:mean-fill mean-fill :quantity mean-quantity :mean-deployable mean-deployable :period period}))

(defn rand-runs
  "Takes a project consisting of one SRC, creates a series of projects with random unit intital cycle times, and returns a sequence of maps (one for each period) with summary statistics."
  [proj reps]
  (let [rand-projs (map rand-proj (repeat reps proj))
        fills (mapcat e/project->period-fill rand-projs)
        grouped-fills (->> fills (group-by :period) vals)]
    (map means grouped-fills)))

(defn rand-target-model
  "Uses the target-model-par-av function from the marathon.analysis.experiment namespace as a base. This function is modified to perform multiple runs for each level of supply. Each run has random unit initial cylce times. Mean statistics are then calculated across runs for each supply level."
  [proj reps]
  (->> (e/split-project proj)
       (reduce
        (fn [acc [src proj]]
          (let [_ (println [:starting src])
                experiments (e/project->experiments proj)
                n           (count experiments)
                _ (println [:experiment-count n])]
            (into acc
                  (util/pmap! 
                   (fn [[idx proj]]
                     (try (let [_    (println [:processing :src src :experiment idx :of n])
                                mean-fills (rand-runs proj reps)]
                            (vec (e/summary-availability-records src proj mean-fills)))
                          (catch Exception e
                            (throw (ex-info (str "unable to compute fill " src)
                                            {:src src :idx idx})))))
                   (map-indexed vector experiments))))) [])
       (apply concat)
       vec))

;Example use:
(comment
  (def path "~/repos/notional/supplyvariation-testdata.xlsx")
  (def proj (a/load-project path))
  (def results (rand-target-model proj 10))
)
