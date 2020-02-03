;This name space contains functions for performing targeting model supply variations with multiple random runs.
(ns marathon.analysis.random (:require [marathon.analysis :as a]
                                       [marathon.analysis.util :as util]
                                       [marathon.analysis.experiment :as e]
                                       [spork.util.table :as tbl]
                                       [spork.util.general :as gen]))

(defn rand-recs
  "Takes a supply record and creates multiple supply records with random initial cycle time."
  [rec]
  (for [i (range (:Quantity rec))]
    (if (= (:Component rec) "AC")
      (assoc rec :CycleTime (rand-int 1095) :Quantity 1 :Name (str i "_" (:SRC rec)))
      (assoc rec :CycleTime (rand-int 1825) :Quantity 1 :Name (str i "_" (:SRC rec))))))

(defn rand-proj
  "Takes a project and makes a new project with random unit initial cycle times."
  [proj]
  (->> proj
       :tables
       :SupplyRecords
       tbl/table-records
       (mapcat rand-recs)
       tbl/records->table
       (assoc-in proj [:tables :SupplyRecords])))

(defn weighted-fill-stats
  "Truncated copy of this function from the marathon.analysis.experiment namespace.
  Takes a series of frames and gets the amount of time associated with them (dt)."
  [frames]
  (->> frames
       (map (fn [[t ctx]] (assoc (e/fill-stats ctx) :t t)))
       (filter #(pos? (:total-quantity %)))
       (gen/time-weighted-samples :t)))

(defn in-phase?
  "Tests if x is between a and b."
  [[a b] x]
  (if (and (>= x a) (<= x b)) true false))

(defn phase-fill
  "Gets the amount of overlap between the intervals [x, x+d] and [a, b]."
  [[phase a b] [m x d]]
  (let [y (dec (+ x d))]
    (if (and (<= x a) (in-phase? [a b] y))
      (assoc m :phase phase :dt (inc (- y a)))
      (if (and (in-phase? [a b] x) (>= y b))
        (assoc m :phase phase :dt (inc (- b x)))
        (if (and (in-phase? [a b] x) (in-phase? [a b] y))
          (assoc m :phase phase :dt (inc (- y x)))
          (if (and (<= x a) (>= y b))
            (assoc m :phase phase :dt (inc (- b a)))))))))

(defn phase-fill-stats
  "Gets the amount of overlap between the interval [x, x+d] and each phase."
  [phases [m x d]]
  (remove nil? (map phase-fill phases (repeat [m x d]))))

(defn weighted-phase-fill
  "Gets fill stats weighted by the duration of each frame (dt)."
  [fill]
  (assoc fill :total-fill (* (:total-fill fill) (:dt fill))
         :total-quantity (* (:total-quantity fill) (:dt fill))
         :total-deployable (* (:total-deployable fill) (:dt fill))))

(defn project->phase-fill
  "Takes a project and returns weighted fill stats by phase."
  [proj phases]
  (->> proj
       a/load-context
       a/as-stream
       weighted-fill-stats
       (mapcat phase-fill-stats (repeat phases))
       (map weighted-phase-fill)
       (group-by :phase)
       (map (fn [x] {:phase (key x)
                     :total-fill (reduce + (map :total-fill (val x)))
                     :total-quantity (reduce + (map :total-quantity (val x)))
                     :total-deployable (reduce + (map :total-deployable (val x)))}))))

(defn change-bound
  "Modifies the upper or lower bound of supply experiments. Usefull for looking
  at supply levels greater than the current inventory."
  [bound fraction]
  (if (some? bound) (int (Math/ceil (* bound fraction))) bound))

(defn ac-supply-reduction-experiments
  "This is a copy of this function from the marathon.analysis.experiment namespace.
  Upper and lower bounds have been modified so we can look at AC supply levels
  above the current inventory."
  [tables lower upper & {:keys [step] :or {step 1}}]
  (let [init      (-> tables :SupplyRecords)
        groups    (-> init e/grouped-supply)
        low       (-> "AC" groups :Quantity (change-bound lower))
        high      (-> "AC" groups :Quantity (change-bound upper))]
    (when (and high (> high 1))
      (for [n (range high (dec low) (- step))]
        (->> (assoc-in groups ["AC" :Quantity] n)
             vals
             tbl/records->table
             (assoc tables :SupplyRecords))))))

(defn project->experiments
  "This is a copy of this function from the marathon.analysis.experiment namespace.
  This function is modified so we can look at AC supply levels above the
  current inventory."
  [prj lower upper & {:keys [step] :or {step 1}}]
  (for [tbls (ac-supply-reduction-experiments (:tables prj) lower upper :step step)]
    (assoc prj :tables tbls)))

(defn rand-target-model
  "Uses the target-model-par-av function from the marathon.analysis.experiment
  namespace as a base. This function is modified to perform a random run for
  each level of supply."
  [proj phases lower upper]
  (->> (e/split-project proj)
       (reduce
        (fn [acc [src proj]]
          (let [experiments (project->experiments proj lower upper)
                n           (count experiments)]
            (into acc
                  (map 
                   (fn [[idx proj]]
                     (try (let [fill (project->phase-fill (rand-proj proj) phases)]
                            ;;fill (e/project->period-fill proj) period fill and no randomness
                            (vec (e/summary-availability-records src proj fill)))
                          (catch Exception e
                            (throw (ex-info (str "unable to compute fill " src)
                                           {:str src :idx idx})))))
                   (map-indexed vector experiments))))) [])
       (apply concat)
       vec))

(defn rand-runs
  "Runs replications of the rand-target-model function."
  [proj reps phases lower upper]
  (apply concat (pmap rand-target-model (repeat reps proj) (repeat reps phases) (repeat lower) (repeat upper))))

(defn write-output
  "Writes formatted output to a file."
  [file-name results]
  (let [format-string (str (clojure.string/join "," (repeat (count (first results)) "%s")) "\n")
        values (map vals results)
        formatted-values (clojure.string/join (map #(apply format format-string %) values))
        headers (clojure.string/replace (apply format format-string (keys (first results))) ":" "")]
    (spit file-name (str headers formatted-values))))

(comment
  ;way to invoke functions
  (def path "~/repos/notional/supplyvariation-testdata.xlsx")
  (def proj (a/load-project path))
  (def phases [["comp" 1 821] ["phase-1" 822 967]])
  (def results (rand-runs proj 1 phases 0 1.5))
)
