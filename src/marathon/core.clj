(ns marathon.core
  (:require ;[util.gui :as gui]
            [util.table :as tbl]
            [clojure [pprint :as pprint]]
            [cljgui.components [swing :as gui]]
            [cljgui.events [observe :as obs]
                           [native :as swing-events]])
  (:use [util.mailbox]
        [marathon.processing.post]
        [marathon.project]))

(defn notify [msg]
  (fn [] (gui/alert msg)))

(def *event-pump* (obs/make-observable))
(defn trigger! [msg] (obs/notify! *event-pump* msg))
(defn action-event
  "A dumb event, typically used to identify unique actions such as button 
   presses and menu selections."
  [name & type]
  (fn [] (trigger! {:name name :type (or (first type) :generic)} )))


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

(defn ->load-project [path] 
  (->packet :load-project path))

(defn ->ui-spec [name & {:keys [doc event-type event] :as opts}]
  (merge {:name name}  opts))
   
  
(defn menu-spec->ui-map [spec & {:keys [keywordize?] :or 
                                        {keywordize? true}}]
  (reduce (fn [acc k]
            (let [event-type 
                  (if keywordize? ((comp keyword 
                                         clojure.string/lower-case) k)
                    k)]
            (assoc acc k (->ui-spec k :doc (get spec k)
                                      :event-type event-type 
                                      :event event-type))))
                   {} (vec (keys spec))))

(defn reactive-menu-spec 
  "Converts a specification of reactive ui elements into their observable 
   events."
  [menu-spec & {:keys [subscribers]}]
  (let [ui-map (menu-spec->ui-map menu-spec)
        event-stream (obs/make-observable)
        _ (when subscribers (doseq [s subscribers]
                              (obs/subscribe! event-stream s)))
        activate! (fn [name]
                    (let [ui (get ui-map name)]
                      (obs/notify! event-stream (get ui :event-type))))]
    (-> (into {} (for [[k v] ui-map]
                          [k (fn [] (activate! k))])) 
         (with-meta {:ui-map ui-map
                     :event-stream event-stream}))))


(def project-menu-spec  
   {"Load-Project" "Loads a project into the context."
    "Save-Project"  "Saves a project into the project path."
    "Save-Project-As" "Saves a currently-loaded project into path."
    "Convert-Project" "Convert a project from one format to another."
    "Derive-Project" "Allows one to derive multiple projects from the current."})

(defn map->reactive-menu [name spec]
  (let [reactive-spec (reactive-menu-spec spec)]
    {name (gui/map->menu name reactive-spec)
     :control (meta reactive-spec)}))

(def project-management-menu  
  (map->reactive-menu "Project-Management" project-menu-spec))

                       
(def processing-menu-spec
  {"Clean" "Cleans a run"
   "HighWater" "Computes HighWater trails"
   "DeploymentVectors" "Analyzes deployments"
   "Custom" "Run a custom script on the project"
   "Eval"   "Evaluate an expression in the context"})

(def processing-menu (map->reactive-menu "Processing" processing-menu-spec))

(defn get-events [obj]  (:event-stream (meta obj)))
(def main-menu
   {"Main" (gui/menu-bar (get project-management-menu "Project-Management")
                         (get processing-menu "Processing"))
    :control {:event-stream (obs/merge-obs (-> project-management-menu 
                                             :control :event-stream )
                                           (-> processing-menu 
                                             :control :event-stream ))}})
(defn interact []
  (let [menu (get main-menu "Main") 
        menu-events (-> main-menu :control :event-stream)
        textlog (gui/label "Idle")
        _       (obs/map-obs (fn [evt] 
                               (gui/change-label textlog (fn [_]
                                                           (str evt)))) 
                             menu-events)]        
    (gui/display (->> (gui/empty-frame "Marathon Project Management")
                   (gui/add-menu menu))
                 (gui/stack textlog  
                            (gui/text-field "Enter an expression...") 
                            (gui/button "Eval!")))))
                   
;(defn random-lines-demo
;  "Shows how to display a 'somewhat' complicated gui.  We use a paintbot to 
;   manage drawing state asynchronously, which allows us to ignore the problems 
;   of running on swing's Event Dispatch Thread.  The paintbot allows us to 
;   add shapes, clear shapes, etc.  in a thread-safe, mutable context.  Note the
;   ease of composition for building interactive guis.  In my opinion, this would
;   be a LOT rougher in Java."
;  [& {:keys [pb stretched?] 
;      :or {pb (make-paintbot) 
;           stretched? true}}]
;  (let [panelfunc (if stretched? get-stretched-panel get-panel)
;        resetbtn (button "Click to clear lines!")
;        lbl (label "These two are mirrored")
;        addline (fn [e] 
;                  (add-shape pb (->line :black (rand 500) (rand 500) 
;                                        (rand 500) (rand 500) )))  
;        counter (label "0")
;        zero-counter (fn [c e] (change-label c (fn [_] "0") e))
;        shapecounter (shelf (label "Shape Count: ")
;			                         counter)                      
;        
;        clearclick (->> (get-observer resetbtn :action)
;                     (:actionPerformed)
;                     (subscribe 
;                       (fn [e]
;                         (zero-counter counter e)
;                         (clear-shapes pb))))
;        
;        drag-lines (fn [panel]  
;                     (do (->> (get-observer panel :mouse)
;                           (:dragged)
;                           (map-obs #(doto % (addline)))
;                           (subscribe 
;                             (partial change-label counter 
;                                      (fn [_] (str (count (:shapes @pb))))))) 
;                       panel))] 
;    (make-modelview 
;      pb 
;      (stack shapecounter
;					        (drag-lines (panelfunc pb))                
;					        resetbtn
;					        (drag-lines (panelfunc pb)))
;					     {:clearclick clearclick 
;					      :drag-lines drag-lines})))
;  
  
               
               