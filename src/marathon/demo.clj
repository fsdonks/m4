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
(defn hpath [p] (str io/home-path p))

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
  (def basepath
    "Documents\\FromCAA\\testdatacheck\\")
  (defn run [] (last  (a/marathon-stream ctx)))

  (def taa-ints
    {:BCTS    ["BCTS"
               ["47112K000"
                "77202K000"
                "77202K100"
                "87312K000"]] 
     :GSAB    ["GSAB"
               ["01225K000"]] 
     :CAB     ["CAB"
               ["01302K000"]]   
     :CSSB    ["CSSB"
               ["63426K000"]] 
     :QMSUPP  ["QM Supply Co"
               ["10473R100"]] 
;     :QMWATER ["QM Water Supp Co"
 ;              ["10460R000"]] 
     :QMPOL   ["QM Pol"
               ["10527RA00"
                "10527RC00"
                "10527RF00"
                "10527RG00"]] 
     :PATRIOT ["ADA Patriot"
               ["44635K000"]] ;in scope
   ;  :AVENGER ["ADA Avenger"
   ;            ["44615R600"]
               }) ;in scope
  (def basepath (hpath "\\Documents\\FromCAA\\testdatacheck\\"))
  (def fourpath (hpath "\\testdata-v3\\"))
  (defn sample-charts [path & {:keys [interests]
                               :or   {interests taa-ints}}]
    (do (proc/run-sample! path :interests interests)
        (proc/do-charts-from path :interests interests)))
  
  )

          
