;This namespace is used to compute the daily deployed readiness of units.
(ns marathon.analysis.dailydeployed
  (:require [marathon.analysis :as a]
            [marathon.analysis.util :as util]
            [marathon.ces.core :as c]
            [spork.util.table :as tbl]))

;; Marathon [t ctx] (stream)
;; Unit entities
;; marathon.ces.core/current-units, interpolates 
;; Filter deployed
;; Of those deployed to location
;; Group-by src and readiness function
;; analysis.util/readiness
