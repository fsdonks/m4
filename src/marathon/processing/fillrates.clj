;A post processor for determining fill rates over time.
(ns marathon.processing.fillrates
  (:require [util [io :as io] 
                  [table :as tbl]]))

;(defrecord trend [t SRC OITitle TotalRequired TotalFilled 
;                  DemandName Vignette DemandGroup])

;basically, we take demandtrends (or preferably highwater) as an output. 
;From there, we compute, by src, the percent fill, by src, over time. 

;output is then [SRC OITitle TotalRequired TotalFilled dt]
