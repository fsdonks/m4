;This namespace could probably be changed into a more general stream-processing
;library.  It was built as a single-purpose script, namely to process a large
;time series, sampling by quarter, to produce a series of peaks in the data
;and a much smaller set of data to examine.  There's definitely a general
;time-series set of reductions hiding in here somewhere...
(ns marathon.processing.highwater
  (:require [spork.util [io :as io]
                        [table :as tbl]
                        [general :refer [clumps]]]))

;;Highwater is a type of reduction performed on DemandTrends.txt, which is a
;;fairly large log file produced during simulation. The log file is in the
;;tab-delimited format.

;;Demandtrends are organized by time - so we have multiple trends sampled
;;at a point in time - with potentially many SRCs across many demands.
;;Our goal is to break up these interwoven signals [t SRC] into
;;k separate signals - one for each SRC.  From there, we'd like to
;;find - for each SRC - the samples occuring within a quarter that
;;represent the high-water mark of active demand for deployed entities:
;;that is, the total number of entities filled or otherwise employed
;;by the demand.

(defn altmain
  "Uses the table operations to handle i/o, rather than streaming everything.
   Wipes out outfile."
  [infile outfile & {:keys [entry-processor]}]
  (spit (clojure.java.io/writer (io/make-file! outfile))
        (tbl/table->tabdelimited
          (file->highwater-table infile :entry-processor entry-processor)))) 


;This is a process, I'd like to move it to a higher level script....
(defn find-demandtrend-paths
  "Sniff out paths to demand trends files from root."
  [root]
  (map io/fpath (io/find-files root #(= (io/fname %) "DemandTrends.txt"))))

(defn highwater-batch
  "Computes high water for for each p in path. dumps a corresponding highwater.
   in the same directory, overwriting."
  [paths & {:keys [entry-processor]}]
  (doseq [source paths]
    (let [target (io/relative-path
                   (io/as-directory (io/fdir source)) ["highwater.txt"])]
      (if (io/fexists? (clojure.java.io/file source))
        (let [_ (println (str "Computing HighWater : " source" -> " target))] 
            (do (highwater source target
                  :entry-processor entry-processor)))
        (println (str "Source file: " source" does not exist!"))))))


(defn highwater-batch-from
  "Compiles a batch of highwater trends, from demand trends, from all demand
   trends files in folders or subfolders from root."
  [root & {:keys [entry-processor]}]
  (highwater-batch (find-demandtrend-paths root)
         :entry-processor entry-processor))


