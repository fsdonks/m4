;;original tacmm script, here for posterity
;;and reuse.
(ns marathon.analysis.tacmm.demo
  (:require [marathon [core :as core]
             [analysis :as analysis]]
            [marathon.ces [core :as c]]
            [spork.entitysystem [store :as store]]
            [spork.util [io :as io] [table :as tbl]]))

;;Scripting runs
;;==============

;;Let's assume we have a path to a marathon workbook.
;;Change the path to suit (or create a similar folder structure).
(def path (io/file-path "~/Documents/tacmdemo/base-testdata-v7.xlsx"))

;;THe simplest operation we'd like to perform is to execute
;;a post-processed run against the workbook, as if the user
;;has used the gui.

;;Thankfully, all of the gui functions have similarly-named
;;handler functions designed for programmatic access in the
;;marathon.core namespace, which makes this trivial.

;;The difference is, we'd like to add in some custom output
;;that's NOT turned on by default.  If you look through
;;the function calls in the source, there's a function
;;marathon.run/do-auditied-run, which is leveraged.  This
;;function delegates to marathon.analysis/spit-history!, which
;;handles the low level grunt work of generating the simulation
;;history, computing eventful results, spitting to output files,
;;etc.  To do this, marathon.analysis/spit-history! looks to see
;;which outputs are required.  M4 has a bunch of possible outputs,
;;some of which can be sizeable either to store ot compute.  They
;;are delineated in the set below, copied from
;;marathon.analysis/all-outputs:
#_(def all-outputs #{:history
                   :location-samples ;:locsamples
                   :locations
                   :depsamples
                   :deployment-records
                   :demandtrends
                   :patches
                   :event-log})

;;By default, we don't compute all these outputs.  Rather the sole
;;simulation results that are typically needed (aka legacy outputs).
#_(def legacy-outputs #{:deployment-records
                      :demandtrends
                      :locations
                      :event-log})

;;So, when M4 goes to simulate and spit outputs via
;;marathon.analysis/spit-history!, it consults a var to infer which
;;outputs are required.  hese are stored in
;;marathon.analysis/*outputs* which is a dynamic var in Clojure
;;parlance.  THat means, it's got a default value associated with it -
;;legacy-outputs - but the user can override that value pretty easily
;;by providing a different set of input inputs (which must be a subset
;;of the known all-outputs), and binding that to the *outputs* var
;;prior to a run.

;;In our case, for TACM, we'd like to know additional information about
;;where units are in their readiness disposition, as indicated by unit
;;location.  Fortunately, the :location-samples output contains this
;;information, which when requested, will populated a tab-delimited file
;;that captures the location of every unit at every eventful point in the
;;simulation (e.g. when a unit moves).  this is similar in spirit to the
;;discrete default locations.txt, except that the output provided in
;;locsamples.txt will be sampled for al units every time there's movement,
;;and will be trivially pivoted or agreggated to provide a look at
;;the disposition of units over time (e.g. by location) via something like
;;a stacked area chart.

;;We get the typical post-processed run like normal, including charts,
;;and the standard outputs, but we now have a text file
;;locsamples.txt.

;;More to follow (programmatic examples for muning the inputs to
;;create substitutions and srcs...)

;;we'd like to compute %fill.
;;So, concat all the demandtrends,
;;basically rip through the demandtrends computing totalfilled/totalrequired.
;;Assumes a path to multiple folders with one or more TACMM cases from
;;early experiments.
(defn path [] (io/file-path "~/Documents/tacmm/revised/cases"))

;;derives one or more child runs from a root path p,
;;where runs are paths with "run" in the name.
(defn path->run [p]
  (->> (-> p
           (clojure.string/split io/re-sep))
       (some #(when (clojure.string/includes? % "run") %))))

;;given a root path with one or more child cases,
;;where output exists (detected by the presence of DemandTrends)
;;and scrapes them into a labeled run map.
(defn derive-runs [root]
  (let [xs (->> root
                io/file
                file-seq
                (map io/fpath)
                (filter (fn [p]
                          (clojure.string/includes? p "DemandTrends.txt"))))]
    (for [p xs]
      (let [root      (io/parent-path p)
            run-label (path->run p)]
        {:label run-label :root root}))))



;;copied from marathon-schemas...
(def demandtrend-schema
  {:t  	        :number
   :Quarter	:number
   :SRC	        :text
   :TotalRequired	:number
   :TotalFilled	:number
   :Overlapping	:number
   :Deployed	        :number
   :DemandName	:text
   :Vignette	        :text
   :DemandGroup	:text
   :ACFilled	        :number
   :RCFilled	        :number
   :NGFilled	        :number
   :GhostFilled	:number
   :OtherFilled	:number
   :ACOverlap  :number
   :RCOverlap  :number
   :NGOverlap  :number
   :GhostOverlap :number
   :OtherOverlap  :number
   :deltaT :number
   })

(defn compute-fills [runs]
  (for [{:keys [label root]} runs]
    (->> (tbl/tabdelimited->records (io/file-path root "DemandTrends.txt")
                                    :schema demandtrend-schema)
         (reduce (fn [acc {:keys [SRC TotalRequired Deployed] :as r}]
                   (let [{:keys [filled required] :as stats}
                          (get acc SRC {:filled 0 :required 0})]
                     (assoc acc SRC {:filled   (+ filled Deployed)
                                     :required (+ TotalRequired required)})))
                 {})
         (assoc {:run label} :fills))))

(def deprecordschema
  (assoc proc.schemas/deprecordschema "DwellYearsBeforeDeploy" :number))

(defn compute-dwells [runs]
  (for [{:keys [label root]} runs]
    (->> (tbl/tabdelimited->records
          (io/file-path root "AUDIT_Deployments.txt")
          :schema deprecordschema)
         (transduce (filter #(= (:Category %) "Rotational"))
                    (completing
                     (fn [acc {:keys [UnitType DwellYearsBeforeDeploy Category]}]
                       (let [{:keys [total n] :as stats}
                             (get acc UnitType {:total 0 :n 0})]
                         (assoc acc UnitType {:total (+ total DwellYearsBeforeDeploy)
                                              :n     (inc n)}))))
                    {})
         (assoc {:run label} :dwells))))

(defn compute-table [root]
  (let [runs (derive-runs root)
        fills (compute-fills runs)
        dwells (compute-dwells runs)]
    (apply concat
           (for [[run xs] (group-by :run (concat fills dwells))]
             (let [{:keys [run fills dwells]} (apply merge xs)]
               (for [[k v] fills]
                 (let [{:keys [total n filled required]} (merge v (dwells k))]
                   {:Run run :SRC k
                    :PercentFill (double (/ filled required))
                    :AverageDwell (double (/ total n))})))))))

;;operations for mucking with inputs
;;we'd like to shift all our workbook demand and periods.

;;all demands that end at the end of presurge
;;grow by k, all later demands start + k.

;;pe-surge period end-time ends K days later,
;;all later surge period end times start K days later.

;;note: we have some really rich functionality
;;for this in spork.util.sampling and friends,
;;related to stoke...
(defn offset-demands [tstop offset xs]
  (for [{:keys [StartDay Duration] :as r} xs
        :when (:Enabled r)]
    (let [start-offset (if (< StartDay tstop) 0 offset)
          duration-offset (if (= tstop (+ StartDay Duration))
                            offset 0)]
      (assoc r :StartDay (+ StartDay start-offset)
             :Duration (+ Duration duration-offset)))))

(defn offset-periods [tstop offset xs]
  (let [new-start (+ tstop offset)]
    (for [{:keys [FromDay ToDay] :as r} xs]
      (let [start-offset (if (< FromDay tstop) 0 offset)
            to-offset    (cond (pos? start-offset) offset
                               (= tstop ToDay)     offset
                               :else 0)]
        (assoc r :FromDay (+ FromDay start-offset)
               :ToDay   (+ ToDay to-offset))))))

(defn xform-records [t f]
  (let [fields (vec (tbl/table-fields t))]
    (->> t
         tbl/table-records
         f
         tbl/records->table
         (tbl/order-fields-by fields))))

(defn offset-tables [tstop offset tbls]
  (-> tbls
      (update :DemandRecords xform-records #(offset-demands tstop offset %))
      (update :PeriodRecords xform-records
              #(offset-periods (dec tstop) offset %))))

(defn merge-parameters [xs m]
  (let [m (reduce-kv (fn [acc k v]
                       (if (keyword? k)
                         (assoc acc (name k) v)
                         acc)) m m)]
    (reduce (fn [acc {:keys [ParameterName Value] :as r}]
              (if-let [v (m ParameterName)]
                (conj acc (assoc r :Value v))
                (conj acc r))) [] xs)))

(defn offset-and-truncate [tstop offset tbls]
  (-> (offset-tables tstop offset tbls)
      (update :Parameters xform-records
        #(merge-parameters % {:LastDayDefault (dec (+ tstop offset))}))))

;;let's do a bunch of runs, and compute our dumb table, THIS time,
;;we'll use a 10-year competition phase by modifying the input
;;tables at runtime....
(defn do-the-runs [source & {:keys [tstop offset chart-ops]
                             :or {tstop 822
                                  offset (- (* 365 10) 822)}}]
  (binding [marathon.analysis/*table-xform*
            #(offset-and-truncate tstop offset %)]
    (doseq [{:keys [root]} (derive-runs source)]
      (let [nm (str (io/fname root) ".xlsx")
            wbpath  (io/file-path (io/parent-path root) nm)
            _       (println [:running! wbpath])
            chart-ops (or chart-ops marathon.core/*chart-ops*)
            chart-ops (if-let [filename (-> chart-ops :ppt :filename)]
                        (assoc-in chart-ops [:ppt :filename]
                                  (io/file-path root filename))
                        chart-ops)
            _       (println [:spitting-powerpoint (-> chart-ops :ppt :filename)])]
        (binding [marathon.core/*chart-ops* chart-ops]
          (core/post-processed-run wbpath))
        (->> source
             compute-table
             tbl/records->table
             (tbl/spit-table (io/file-path source "tacmfill.txt")))))))
