;;tools for comparing projects to possibly factor out
;;redundant runs.
(ns marathon.analysis.factoring
  (:require [marathon.analysis :as a]
            [marathon.analysis.experiment :as e]
            [marathon.analysis.random     :as r]
            [spork.util [table :as tbl] [io :as io]]
            [spork.util.general :as gen]))


;;want to yield a comparable key, so replace all the SRC's
;;in a subproject with "generic".

(defn supply-similar [tbl]
  (->> tbl
       tbl/as-records
       (reduce (fn [acc {:keys [Component Quantity]}]
                (assoc acc Component Quantity)) {})))

(defn drop-src [tbl]
  (tbl/map-field :SRC (constantly "dropped") tbl))

(defn project-key [proj]
  (-> proj
      (update-in [:tables :SupplyRecords] drop-src)
      (update-in [:tables :DemandRecords] drop-src)))


;;there are probably 2 questions here.

;;1 - which SRCs have the same demand?  (demand classes)
;;2 - of those SRCS, which SRCs have the same supply?


;;1 is generally useful.  We can use it to inform 2
;;for any supply.  Since we are doing supply variation,
;;we can tell if the [DemandClass AC RC NG] has been seen
;;already, and return a memoized result.


(defn factor [proj]
  (let [keyf (gen/memo-1 project-key)]
    (group-by keyf (->> proj e/split-project vals))))

(defn factor-by-demand [proj]
  (let [keyf (gen/memo-1 project-key)]
    (group-by keyf (->> proj e/split-project vals (map #(update % :tables dissoc :SupplyRecords))))))

(defn factor-by-supply [proj]
  (let [keyf (gen/memo-1 (fn [p]
                           (->> p :tables :SupplyRecords supply-similar)))]
    (->> proj e/split-project vals (group-by keyf))))


;;testing
(comment
  (def p    (io/file-path "~/workspacenew/notional/base-testdata-v7.xlsx"))
  (def proj (a/load-project p))

  )


;;start extending this to collect stats where [ac rc ng] and demand
;;are the same, get an idea of what proportion of runs can be reused.

#_

(defn rand-target-model
  "Uses the target-model-par-av function from the marathon.analysis.experiment
  namespace as a base. This function is modified to perform a random run for
  each level of supply."
  [proj & {:keys [phases lower upper levels gen seed->randomizer]
           :or   {lower 0 upper 1 gen util/default-gen
                  seed->randomizer (fn [_] identity)}}]
  (let [project->experiments *project->experiments*]
     (->> (assoc proj :phases phases :lower lower :upper upper :levels levels
                 :gen gen  :seed->randomizer seed->randomizer)
          (e/split-project)
          (reduce
           (fn [acc [src proj]]
             (let [experiments (project->experiments proj lower upper)]
               (into acc
                     (filter (fn blah [x] (not (:error x))))
                     (util/pmap! *threads*
                                 (fn [[idx proj]]
                                   (let [rep-seed   (util/next-long
                                                     gen)
                                         supply-randomizer
                                         (seed->randomizer rep-seed)]
                                     (-> proj
                                         (assoc :rep-seed rep-seed
                                                :supply-record-randomizer
                                                supply-randomizer)
                                         (add-transform
                                          random-initials [supply-randomizer])
                                         (try-fill src idx phases))))
                                 (map-indexed vector experiments))))) [])
          (apply concat)
          vec)))
