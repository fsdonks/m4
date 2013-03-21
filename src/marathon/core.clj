(ns marathon.core
  (:require [util [table :as tbl]
                  [io :as io]]
            [marathon.processing [helmet :as helm]]
            [clojure [pprint :as pprint]]
            [cljgui.components [swing :as gui]]
            [cljgui [mvc :as mvc]]
            [cljgui.events [observe :as obs]
                           [native :as swing-events]])
  (:use [util.mailbox]
        [marathon.processing.post]
        [marathon.project])
  (:gen-class :main true))

(def  path-history (atom [(System/getProperty "user.dir")]))
(defn add-path! [p] (swap! path-history conj p))
(defn active-path [] (last @path-history))

(defn select-file []
  (let [p (gui/select-file (active-path))]
    (do (add-path! p)
      p)))

(defn select-folder []
  (let [p (gui/select-folder (active-path))]
    (do (add-path! p)
      p)))

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
   {"Load-Project"    "Loads a project into the context."
    "Save-Project"    "Saves a project into the project path."
    "Save-Project-As" "Saves a currently-loaded project into path."
    "Convert-Project" "Convert a project from one format to another."
    "Derive-Project"  "Allows one to derive multiple projects from the current."})

(def processing-menu-spec
  {"Clean" "Cleans a run"
   "High-Water" "Computes HighWater trails"
   "Deployment-Vectors" "Analyzes deployments"
   "Charts" "Generate plots."
   "Stochastic-Demand" "Generate stochastic demand files from a casebook."   
   "Custom" "Run a custom script on the project"
   "Eval"   "Evaluate an expression in the context"})

(def scripting-menu-spec
  {"Load-Script" "Load a clojure script into the environment."
   "REPL"   "Jack in to a Read Evaluate Print Loop."})

(def preferences-menu-spec
  {"Update" "Check for updates to Marathon."
   "Eval"   "Evaluate an expression in the context"})

(defn reactive-menu-system
  "Given a map of menu specs, builds a menu-system model with an integrated
   event stream that has a unique event for each menu item selected.  Returns 
   an event stream that is a union or merge of all the menu events."
  [specs]
  (let [menus (reduce (fn [acc [name spec]]
                        (assoc acc name 
                               (gui/map->reactive-menu name spec))))]
    (mvc/make-modelview nil menus 
      {:menu-events (obs/multimerge-obs (vals menus))})))       


(defn casekey->filename [[case-name future]]
  (str case-name \_ future ".txt"))

(defn spit-tables [futures path]
  (let [root-dir (io/as-directory path)]        
    (doseq [[case-key tbl] futures]
      (let [file-name (casekey->filename case-key)]
        (io/hock  (io/relative-path root-dir [file-name]) 
                  (tbl/table->tabdelimited tbl))))))

;a quick plugin for stochastic demand generation.
(defn stoch-demand-dialogue []
  (do (gui/alert "Please select the location of valid case-book.")
    (let [wbpath      (select-file)           
          cases       (helm/futures->tables 
                        (helm/xlsx->futures wbpath))
          ;dump-same?  @(future (gui/yes-no-box "Dump cases in same location?"))
          dump-folder ;(if dump-same?
                        (apply str (interleave (butlast (io/list-path wbpath))
                                               (repeat "\\")))
                        ;(select-folder))
          _ (print (str "dumping to " dump-folder))]
      
      (spit-tables cases dump-folder))))

(defn hub [& {:keys [project]}]
  (let [project-menu   (gui/map->reactive-menu "Project-Management"  
                                               project-menu-spec)
        processing-menu (gui/map->reactive-menu "Processing"
                                                processing-menu-spec)
        main-menu (gui/menu-bar (:view project-menu)
                                (:view processing-menu))
        menu-events (obs/merge-obs (-> project-menu :control :event-stream)
                                    (-> processing-menu :control :event-stream)) 
        textlog (gui/label "Idle")
        audit   (gui/button "Audit" (fn [_] 
                                      (obs/notify! menu-events :audit)))
;        textbox (gui/text-box)
        reflect-selection (->> menu-events 
                            (obs/subscribe  #(gui/change-label textlog %)))
        _                 (->> menu-events 
                            (obs/filter-obs #(= % :stochastic-demand))
                            (obs/subscribe (fn [_] (stoch-demand-dialogue))))]
    (mvc/make-modelview 
      (agent {:state (if project {:current-project project} {})
              :routes (merge default-routes project-routes)})       
      (gui/display (->> (gui/empty-frame "Marathon Project Management")
                     (gui/add-menu main-menu))
                   (gui/stack textlog  
                              (gui/text-box) 
                              audit))
      {:menu-events menu-events})))

(defn -main [& args] 
  (hub))

;       :add-table   
;       :clear-project     
;       :load-project   
;       :save-project        
;       :view-project  
;       :view-table   
;       :add-table})))
                  




               
               