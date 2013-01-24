(ns marathon.core
  (:require [util.gui :as gui]
            [util.table :as tbl])
  (:use [marathon.processing.post]
        [marathon.project]))

(defn notify [msg]
  (fn [] (gui/alert msg)))

(def ^:dynamic *project*)

(def project-management-menu  
  (gui/map->menu "Project-Management"
   {"Load-Project" 
      (notify "Loads a project into the context.")
    "Save-Project" 
      (notify "Saves a project into the project path.")
    "Save-Project-As" 
      (notify "Saves a currently-loaded project into path.")
    "Convert-Project" 
      (notify "Analyzes deployments")
    "Derive-Project"
      (notify "Allows one to derive multiple projects from the current.")}))
          
(def processing-menu
  (gui/map->menu "Post-Process"
       {"Clean" (notify "Cleans a run")
        "HighWater" (notify "Computes HighWater trails")
        "DeploymentVectors" (notify "Analyzes deployments")
        "Custom" (notify "Run a custom script on the project")
        "Eval"   (notify "Evaluate an expression in the context")}))

(def main-menu 
  (gui/menu-bar project-management-menu 
                processing-menu))

(defn interact []
  (gui/display (->> (gui/empty-frame "Marathon Project Management")
                    (gui/add-menu main-menu))
  (gui/button "hello!")))
                   
                 
               
               