;functions for visualizing and interacting with marathon projects, 
;which are basically just maps with an expected nested structure.
(ns marathon.project.views.core
  (:require [spork.cljgui.components [swing :as gui]]
            [spork.graphics2d.canvas :refer :all]
            [spork.geometry.shapes   :refer :all]
            [spork.graphics2d.image  :refer :all]))
            
(def painter (atom nil))


(def the-func (fn [g] (when-let [f @painter]
                        (f g)))) 

(def board (gui/paintpanel the-func))            

(defn change-drawing! [f]
  (do (swap! painter (fn [_] f))
      (.repaint board)))