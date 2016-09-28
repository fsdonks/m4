;;This is a graphical interface to the demo
;;script.
(ns marathon.gui
  (:require [marathon         [analysis :as a]]
            [marathon.ces     [core :as core]]
            [marathon.visuals [patches :as patches]]
            [spork.util       [io :as io]]
            [spork.util [table :as tbl]
                        [io :as io]]
            [marathon.processing.helmet [core :as helm]]
            [marathon.processing.stoke [core :as stoke]
                                       [io :as stokeio]
                                       [scraper :as scraper]]
            [clojure       [pprint :as pprint]]
            [spork.cljgui.components [swing :as gui]]
            [spork         [mvc :as mvc]]
            [spork.events  [observe :as obs]
                           [native :as swing-events]])
  (:use [spork.util.mailbox]
        [marathon.processing.post]
        [marathon.project])
  (:import [javax.swing JFrame]) ;Bah!
  ;(:gen-class :main true)
  )




(def ep "C:\\Users\\1143108763.C\\srm\\notionalbase.xlsx")

(defn build-patches [rootpath]
  (do (println [:building-patches (str rootpath "patches.htm")])
      (patches/locations->patches rootpath)))

(defn do-run [from-path target-path]
  (do (a/spit-history! (a/marathon-stream from-path) target-path)
      (build-patches target-path)))

(def root "C:\\Users\\1143108763.C\\Documents\\srm\\cleaninput\\runs\\")
(def root "C:\\Users\\tspoon\\Documents\\srm\\tst\\notionalv2\\")

(def srm "srmbase.xlsx")
(def arf "arfbase.xlsx")
(defn strip [x] (first (clojure.string/split x #"\.")))

;;this is probably our primary api.
(defn run-cases [xs]
  (doseq [x xs]
    (println [:running x])
    (let [nm  (strip    x)
          in  (str root x)
          out (str root nm "\\")
          _   (io/hock (str out "timestamp.txt") (System/currentTimeMillis))]
      (do-run in out))))

;;we can make this graphical by changing out...
