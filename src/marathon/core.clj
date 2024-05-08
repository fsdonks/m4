;;Refactored. This was originally a combined front end and scripting hub, where
;;we had user dialogues wired up to a gui coupled with backend scripts (e.g.
;;functions) invoked at runtime. Going forward, we retain the backend
;;implementation here, but move the gui and user dialogues to the marathon.ui
;;namespace, which lives in the m4top project as a superior project that depends
;;on the marathon backend here. So we have a clean decoupling of the frontend
;;gui client and the backend computational infrastructure, which allows better
;;dependency and bundling as an application. For additional deployment models -
;;like web-based or container-based - this allows additional flexibility in the
;;future.
(ns marathon.core
  (:require [spork.util [table :as tbl] [io :as io]]
            [helmet [core :as helm]]
            [marathon.processing [highwater :as hw]]
            [marathon [run :as run] [demo :as demo]]
            [marathon.analysis [requirements :as requirements]]
            [marathon.sampledata [branches :as branches]]
            [proc [demandanalysis :as da]])
  (:use    [marathon.project] [clojure.repl]))

(def +version+ "4.2.16-SNAPSHOT")

(def noisy (atom true))
(defn toggle-noisy [] (swap! noisy (fn [n] (not n))))

;;new global chart options for controlling visualization
;;and rendering of multiple runs.  simplifies configuration
;;and scripting.  Default mirrors legacy behavior,
;;visuals are displayed, but no pptx is emitted.
(def ^:dynamic *chart-ops* {:vis true
                            :ppt nil})

;;From Stuart Sierra's blog post, for catching otherwise "slient" exceptions
;;Since we're using multithreading and the like, and we don't want
;;exceptions to get silently swallowed
(let [out *out*]
  (Thread/setDefaultUncaughtExceptionHandler
   (reify Thread$UncaughtExceptionHandler
     (uncaughtException [_ thread ex]
       (when @noisy
         (binding [*out* out]
           (println ["Uncaught Exception on" (.getName thread) ex])))))))


;;Component scripting services for high level tasks.
;;=================================================

;;This is helmet specific.
(defn casekey->filename [[case-name future]]
  (str case-name \_ future ".txt"))

(defn spit-tables [futures path]
  (let [root-dir (io/as-directory path)]
    (doseq [[case-key tbl] futures]
      (let [file-name (casekey->filename case-key)]
        (io/hock  (io/relative-path root-dir [file-name])
                  (tbl/table->tabdelimited tbl))))))

(defn stoch-demand [wbpath]
  (let [dump-folder  (io/parent-path wbpath)
        _ (println (str "dumping to " dump-folder))]
    (spit-tables
     (helm/futures->tables
      (helm/xlsx->futures wbpath :ignore-dates? true :log? false)) dump-folder)))

(defn capacity-analysis [wbpath]
  (let [root         (io/parent-path wbpath)
        target       (last    (io/list-path wbpath))
        _            (println [:running :capacity-analysis :at wbpath])]
    (run/run-cases root [target])))

(defn high-water-batch-from [path] (hw/highwater-batch-from (io/parent-path path)))
(defn high-water [path] (hw/highwater-batch [(io/file-path path)]))

(defn default-interests [& args]
  (do (println [:using-default-interests 'marathon.sampledata.branches/branches])
      branches/branches))

(defn default-post-processed-run [wbpath & {:keys [path->interests]}]
  (let [path->interests (or path->interests default-interests)
        root         (io/parent-path wbpath)
        destination  (clojure.string/replace wbpath ".xlsx" io/+separator+)
        ipath        (str root "interests.clj")
        interests    (if (io/file? ipath)
                          (do (println [:using-interests-at ipath])
                              (clojure.edn/read-string (slurp ipath)))
                          (path->interests wbpath))
        _            (println [:running :post-processed-analysis :at wbpath])]
    (demo/run-it :root        wbpath
                 :destination destination
                 :interests   interests
                 :vis         (:vis *chart-ops*)
                 :ppt         (:ppt *chart-ops*))))

;;this adds location-samples to the default outputs...do we always
;;want to do that?  curious to see if there's any big overhead.
;;we brought them back into scope for some niche analysis.
(defn post-processed-run [wbpath & {:keys [path->interests]}]
  (default-post-processed-run wbpath :path->interests path->interests))

(defn post-processed-run-with-samples [wbpath & {:keys [path->interests]}]
  (binding [marathon.analysis/*outputs*
            (conj marathon.analysis/*outputs* :location-samples)]
    (default-post-processed-run wbpath :path->interests path->interests)))

(defn requirements-analysis [wbpath]
  (requirements/requirements-run wbpath))

;;sparkcharts...
;;currently mimicing the api from da.
(comment
(defn emit-spark-charts
  "Load spark charts from a marathon audit trail directory specified by in and
   save all spark charts to the out directory.
   Calling an optional function f on each chart before saving."
  [in out & {:keys [group-fn f phase-lines? bounds]
             :or {group-fn (fn [s] "All")
                  f identity
                  phase-lines? true
                  bounds nil}}]
  (da/dump-spark-charts (io/as-directory outdir)
                        (io/as-directory outdir)))
)
