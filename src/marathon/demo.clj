;;this is a demonstration of how to do a marathon
;;run, to replicate the results submitted.
(ns marathon.demo
  (:require [marathon         [analysis :as a]
                              [run :as run]]
            [marathon.ces     [core :as core]]
            [marathon.visuals [patches :as patches]]
            [marathon.project :as project]
            [spork.util       [io :as io]]
            [proc.example :as proc]))

;;dumb helper....cross system deving...
(defn hpath [p]
  (str io/home-path p))

(def full-path (hpath "\\Documents\\srm\\tst\\notionalv2\\maxbase.xlsx"))
(def out-path  (hpath "\\Documents\\srm\\tst\\notionalv2\\maxbase\\"))

;(def full-path "C:\\Users\\tspoon\\Documents\\srm\\tst\\notionalv2\\maxbase.xlsx")
;(def out-path  "C:\\Users\\tspoon\\Documents\\srm\\tst\\notionalv2\\maxbase\\")

(def test-interests
  {:SRC1   ["SRC1" ["SRC1"]]
   :SRC2   ["SRC2" ["SRC2"]]
   :SRC3   ["SRC3" ["SRC3"]]
   :SRC4   ["SRC4" ["SRC4"]]})

(defn run-it []
  (do (run/do-audited-run full-path out-path)
      (proc/run-sample! out-path    :interests test-interests)
      (proc/do-charts-from out-path :interests test-interests)))

(comment
  (def ctx (a/load-context full-path))
  (defn run [] (last  (a/marathon-stream ctx)))
  )

          
