(ns marathon.core
  (:require [util.gui :as gui]
            [util.table :as tbl]
            [clojure [pprint :as pprint]])
  (:use [marathon.processing.post]
        [marathon.project]))

(defn notify [msg]
  (fn [] (gui/alert msg)))

;project-manager is a simple agent that maintains internal state. 
;we design the event handling functions as simple events that the user enters,
;can communicate them to project-manager. 

;Project-manager then routes tasks, using the currently-loaded project as its 
;environment. 

;These are generic routing functions for a service built on message-passing.
;I could abstract this into a simple library.

(defrecord packet [msg-type msg-data])


(defmacro message-handler [& body]
  `(fn [~'state ~'msg]
     (let [~'*msg-data* (:msg-data ~'msg)
           ~'*msg-type* (:msg-type ~'msg)]
       ~@body)))

(defmacro pass-through [f]
  `(message-handler (do (~f ~'state) ~'state)))

(defmacro effect [f] 
  `(message-handler (do (~f) ~'state)))

(defn not-implemented 
  ([msg] (fn [] (gui/alert (str msg " Not Implemented"))))
  ([] (not-implemented "")))

(def empty-control-state {:routes {} :state {}})

(defn route-message [{:keys [routes state] :as s} msg]
  (if-let [f (get routes (:msg-type msg))] ;finds a route if applicable
    (f state msg)
    (do ((get s :printer pprint/pprint) 
          ["Message type unknown" msg]) s))) 

(defn make-mvc [routes init-state]
  {:model (agent {:state init-state
                  :routes routes})
   :view nil 
   :control route-message})

(defn send-and-ignore 
  ([agt msg msgdata]
    (do (send-off agt route-message (->packet msg msgdata))
      :sent))
  ([agt msg] (send-and-ignore agt msg nil)))
    

(defn mvc->model [mvc]   (deref (:model mvc)))
(defn mvc->routes [mvc]  (comp (:routes mvc->model) mvc))
(defn mvc->view [mvc]    (:view mvc))

(def sample-path 
  "C:\\Users\\thomas.spoon\\Documents\\Marathon_NIPR\\OngoingDevelopment\\smallsampling")
(def sample 
  "C:\\Users\\thomas.spoon\\Documents\\Marathon_NIPR\\OngoingDevelopment\\smallsampling\\MPI_3.760298326.xlsm")

;default routes for our project management controller...
;all we have to do is wire up a gui to send messages to the controller, and 
;voila...

(def default-routes 
  {:ping         (effect #(pprint/pprint :pong))
   :set-routes   (message-handler 
                   (assoc state 
                          :routes *msg-data*)) ;state->msg->state
   :assoc-route  (message-handler  
                   (assoc-in state [:routes (first *msg-data*)]                            
                             (second *msg-data*)))
   :dissoc-route (message-handler  
                   (assoc-in state [:routes (first *msg-data*)]                            
                             (second *msg-data*)))
   :print        (message-handler 
                  (do ((get state :printer (pprint/pprint)) 
                     *msg-data*)
                    state))
   :eval         (message-handler 
                   (do ((get state :printer (pprint/pprint) 
                             (eval *msg-data*)
                     state))))
   :set-state    (message-handler 
                   (assoc state :state *msg-data*))})

(def project-routes 
  {:clear-project (message-handler 
                   (assoc-in state 
                          [:state :current-project] nil)) ;state->msg->state
   
   :load-project (message-handler 
                   (assoc-in state 
                          [:state :current-project]
                          (load-project *msg-data*))) ;state->msg->state
   
   :save-project (pass-through #(save-project % *msg-data*)) ;state->msg->state
   
   :view-project (effect (not-implemented :view-project)) ;state->msg->state
   
   :view-table   (pass-through 
                     #(gui/view (get-table (:current-project state)
                                           *msg-data*)
                                :sorted true)) ;state->msg->state   
   :add-table    (message-handler 
                   (add-table (:current-project state)
                              *msg-data*))}) ;state->msg->state

(defn ->load-project [path] 
  (->packet :load-project path))


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
                   
                 
               
               