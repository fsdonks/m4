(ns marathon.processing.surgereader
  (require [marathon.processing [forgereader :as forge]]
           [util [io :as io] 
                 [table :as tbl]]))

(def surgepath 
  (io/relative-path :docs
                 ["TAA 15-19" "Unconstrained Runs"
                  "Derived Data" "Demand" "Surge3 Conversion"]))

(defn process-surge3
  "Processes surge demands from path/phases. txt, path/events.txt,
   path/periods. txt. Spits a tab-delimited table of demand records to
   path/demands. txt"
  [& [path]]
  (let [p (if path path surgepath)
        tbls (forge/load-tables p)
        records (forge/tbls->demandrecords tbls "Surge3")]
    (io/hock (io/relative-path p ["surge3demands.txt"])
             ((comp tbl/table->tabdelimited tbl/records->table) records))))