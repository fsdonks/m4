;;this is a demonstration of how to do a marathon
;;run, to replicate the results submitted.
(ns marathon.demo
  (:require [marathon         [analysis :as a]]
            [marathon.ces     [core :as core]]
            [marathon.visuals [patches :as patches]]
            [spork.util       [io :as io]]))

(def ep "C:\\Users\\1143108763.C\\srm\\notionalbase.xlsx")

(defn build-patches [rootpath]
  (do (println [:building-patches (str rootpath "patches.htm")])
      (patches/locations->patches rootpath)))

(defn do-run [from-path target-path]
  (do (a/spit-history! (a/marathon-stream from-path) target-path)
      (build-patches target-path)))

;;This is just a helper to translate from craig's encoding for
;;srm policy positions.
(def translation
  {"MA, DA" "MA_DA_C1"
   "MD, DA" "MD_DA_C1"
   "MA, NDA" "MA_NDA_C1"
   "Ready"   "R_C2"
   "PB"      "PB_C3"
   "MP, NDA"  "MP_NDA_C3"
   "PT"       "PT_C4"
   "MP, DA"   "MP_DA_C1"})

(def root "C:\\Users\\1143108763.C\\Documents\\srm\\cleaninput\\runs\\")
(def root "C:\\Users\\tspoon\\Documents\\srm\\tst\\notionalv2\\")

(def srm "srmbase.xlsx")
(def arf "arfbase.xlsx")
(defn strip [x] (first (clojure.string/split x #"\.")))

(defn run-cases [xs]
  (doseq [x xs]
    (println [:running x])
    (let [nm  (strip    x)
          in  (str root x)
          out (str root nm "\\")
          _   (io/hock (str out "timestamp.txt") (System/currentTimeMillis))]
      (do-run in out))))

(comment ;testing
  (do-run ep "C:\\Users\\1143108763.C\\srm\\newtest\\")
  (def h
    (a/load-context "C:\\Users\\1143108763.C\\Documents\\srm\\cleaninput\\runs\\srmbase.xlsx"))
)
