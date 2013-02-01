(ns marathon.core
  (:require ;[util.gui :as gui]
            [util.table :as tbl]
            [clojure [pprint :as pprint]]
            [cljgui.components [swing :as gui]]
            [cljgui [mvc :as mvc]]
            [cljgui.events [observe :as obs]
                           [native :as swing-events]])
  (:use [util.mailbox]
        [marathon.processing.post]
        [marathon.project]))

(defn notify [msg]
  (fn [] (gui/alert msg)))

(defn not-implemented 
  ([msg] (fn [] (gui/alert (str msg " Not Implemented"))))
  ([] (not-implemented "")))

;project-manager is a simple agent that maintains internal state. 
;we design the event handling functions as simple events that the user enters,
;can communicate them to project-manager. 

;Project-manager then routes tasks, using the currently-loaded project as its 
;environment. 


(defn project-mvc [routes init-state]
  {:model (agent {:state init-state
                  :routes routes})
   :view nil 
   :control route-message})


(def sample-path 
  "C:\\Users\\thomas.spoon\\Documents\\Marathon_NIPR\\OngoingDevelopment\\smallsampling")
(def sample 
  "C:\\Users\\thomas.spoon\\Documents\\Marathon_NIPR\\OngoingDevelopment\\smallsampling\\MPI_3.760298326.xlsm")


;This is a hack, need a more elegant solution.
(defn tbl->view [t & {:keys [sorted] :or 
                       {sorted true}}]    
    (gui/->swing-table (tbl/table-fields t)
                       (tbl/table-rows t) 
                       :sorted sorted))   

(def project-routes 
  {:clear-project (message-handler 
                    (assoc-in env 
                        [:state :current-project] {})) ;state->msg->state   
   :load-project (message-handler
                   (assoc-in env 
                      [:state :current-project]
                        (load-project msg-data))) ;state->msg->state   
   :save-project (message-handler  
                   (pass-through #(save-project % msg-data))) ;state->msg->state
   
   :view-project (effect (not-implemented :view-project)) ;state->msg->state
   
   :view-table   (pass-through 
                     #(gui/view (tbl->view (get-table (:current-project state)
                                           msg-data)
                                :sorted true)
                                :title (str msg-data))) ;state->msg->state   
   :add-table    (message-handler
                   (let [[name table] msg-data]
                     (assoc-in env [:state :current-project] 
                       (add-table (:current-project state)
                          msg-data))))}) ;state->msg->state

(comment 
  (def project-manager
    (project-mvc (merge default-routes project-routes)   {}))
  
  (def sample-table {:First ["Tom"]
                     :Last  ["Spoon"]})
)

(def project-menu-spec  
   {"Load-Project" "Loads a project into the context."
    "Save-Project"  "Saves a project into the project path."
    "Save-Project-As" "Saves a currently-loaded project into path."
    "Convert-Project" "Convert a project from one format to another."
    "Derive-Project" "Allows one to derive multiple projects from the current."})

(def processing-menu-spec
  {"Clean" "Cleans a run"
   "HighWater" "Computes HighWater trails"
   "DeploymentVectors" "Analyzes deployments"
   "Charts" "Generate plots."   
   "Custom" "Run a custom script on the project"
   "Eval"   "Evaluate an expression in the context"})

(def preferences-menu-spec
  {"Update" "Check for updates to Marathon."
   "Eval"   "Evaluate an expression in the context"})

(defn hub []
  (let [project-menu   (gui/map->reactive-menu "Project-Management"  
                                               project-menu-spec)
        processing-menu (gui/map->reactive-menu "Processing"
                                                processing-menu-spec)
        main-menu (gui/menu-bar (:view project-menu)
                                (:view processing-menu))
        menu-events (obs/merge-obs (-> project-menu :control :event-stream )
                                    (-> processing-menu :control :event-stream)) 
        textlog (gui/label "Idle")
        reflect-selection (->> menu-events 
                            (obs/subscribe  #(gui/change-label textlog %)))]
    (mvc/make-modelview 
      (agent {:state init-state
              :routes routes})       
      (gui/display (->> (gui/empty-frame "Marathon Project Management")
                     (gui/add-menu main-menu))
                   (gui/stack textlog  
                        (gui/text-field "Enter an expression...") 
                        (gui/button "Eval!")))
      {:menu-events menu-events})))
                  
  
               
               