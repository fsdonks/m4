;A post processor for determining fill rates over time.
(ns marathon.processing.fillrates
  (:require [spork.util [io :as io] 
                        [table :as tbl]]))

(defrecord trend [t SRC OITitle TotalRequired TotalFilled 
                  DemandName Vignette DemandGroup])



