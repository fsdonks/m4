(ns marathon.core
  (:require [spork.util [table :as tbl]
                        [io :as io]]
            [marathon.processing.helmet [core :as helm]]
            [marathon.processing.stoke [core :as stoke]
                                       [io :as stokeio]
                                        ;[scraper :as scraper]
             ]
            [marathon.run :as run]
            [marathon.analysis [requirements :as requirements]]
            [clojure       [pprint :as pprint]
                           [set :as set]]
            [spork.cljgui.components [swing :as gui]]
            [spork         [mvc :as mvc]]
            [spork.events  [observe :as obs]
                           [native :as swing-events]]
            [nightclub [core :as night]]
            [nightcode [repl :as repl]]
            [seesaw.core]
            )
  (:use [spork.util.mailbox] ;;should be able to deprecate this.
        [marathon.processing.post]
        [marathon.project]
        [clojure.repl])
  (:import [javax.swing JFrame UIManager]))

(def noisy (atom true))
(defn toggle-noisy [] (swap! noisy (fn [n] (not n))))
;;From Stuart Sierra's blog post, for catching otherwise "slient" exceptions
;;Since we're using multithreading and the like, and we don't want
;;exceptions to get silently swallowed
(let [out *out*]
  (Thread/setDefaultUncaughtExceptionHandler
   (reify Thread$UncaughtExceptionHandler
     (uncaughtException [_ thread ex]
       (when @noisy 
         (binding [*out* out]
           (println ["Uncaught Exception on" (.getName thread) ex])))))))

(def  path-history   (atom [(System/getProperty "user.dir")]))
(defn add-path! [p]  (swap! path-history conj p))
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

#_(defn repl-panel
  "Creates a swing-repl that we can send code to for remote 
   evaluation, or allow the user to interactively eval 
   expressions."
  ([opts]
   (org.dipert.swingrepl.main/make-repl-jconsole
    (merge org.dipert.swingrepl.main/default-opts opts)))
  ([w h]
   (doto (repl-panel {})
     (.setPreferredSize (java.awt.Dimension. w h))))
  ([w h opts]
   (doto (repl-panel opts)
         (.setPreferredSize (java.awt.Dimension. w h)))))

;;Probably need to modify this....
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
  {"Examine-Project"    "Provides a visual presentation of a project."   
  ;  "Save-Project"    "Saves a project into the project path."
  ;  "Save-Project-As" "Saves a currently-loaded project into path."
 ;   "Convert-Project" "Convert a project from one format to another."
 ;   "Derive-Project"  "Allows one to derive multiple projects from the current."
 ;   "Migrate-Project" "Port data from a legacy version of marathon to a new one."
   ; "Audit-Project"   "Audits the current project."
   ; "Audit-Projects"  "Audits multiple projects"
    })

(def processing-menu-spec
  {;"Clean"              "Cleans a run"
   ;"High-Water"         "Computes HighWater trails"
   ;"Deployment-Vectors" "Analyzes deployments"
   ;;"Charts"             "Generate plots."   
   "Capacity-Analysis"     "Performs a capacity run (default)"
   "Requirements-Analysis" "Performs a Requirements Run"
   "Stochastic-Demand"     "Generate stochastic demand files from a casebook."
   "Compute-Peaks"         "Extract the peak concurrent demands from a folder."
   "Debug-Run"             "Runs the capacity analysis with output directed toward a file"
;;   "Custom"             "Run a custom script on the project"
;;   "Eval"               "Evaluate an expression in the context"
   })

(def debug-menu-spec  
  {"Debug-Run"
      "Performs a capacity analysis with copious amounts of debug info."
    "Debug-Run-Heavy"
       "Capacity Analysis With ai-behavior level output."
  ;  "Save-Project"    "Saves a project into the project path."
  ;  "Save-Project-As" "Saves a currently-loaded project into path."
 ;   "Convert-Project" "Convert a project from one format to another."
 ;   "Derive-Project"  "Allows one to derive multiple projects from the current."
 ;   "Migrate-Project" "Port data from a legacy version of marathon to a new one."
   ; "Audit-Project"   "Audits the current project."
   ; "Audit-Projects"  "Audits multiple projects"
    })

(def scripting-menu-spec
  {"Load-Script" "Load a clojure script into the environment."})
(def help-menu-spec
  {"Search-For" "Interactive Help"})

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

;;This is helmet specific.
(defn casekey->filename [[case-name future]]
  (str case-name \_ future ".txt"))

(defn spit-tables [futures path]
  (let [root-dir (io/as-directory path)]        
    (doseq [[case-key tbl] futures]
      (let [file-name (casekey->filename case-key)]
        (io/hock  (io/relative-path root-dir [file-name]) 
                  (tbl/table->tabdelimited tbl))))))

(defmacro with-alert [alert body]
  `(do (~'gui/alert ~alert)
       ~body))

(defmacro request-path [[bind alert] body]
  `(when-let [~bind (with-alert ~alert (~'select-file))]
     ~body))

(defn compute-peaks-dialogue []
  (request-path [the-path "Please select a file co-located in a folder with demand case files."]
    (let [dump-folder (apply str (interleave (butlast (io/list-path the-path))
                                               (repeat "\\")))
          target (str dump-folder "peaks\\")          
          _ (gui/alert (str "dumping peaks files to " target))]            
      (stokeio/compute-peaks dump-folder target))))

;;a quick hack to compute the deciles from a set of peaks, in case we 
;;don't need to compute the peaks and the deciles again.  We may 
;;separate this from the compute-peaks entirely.
(defn compute-deciles-dialogue []
  (request-path [the-path "Please select a file co-located in a folder with demand peak files."]
    (let [dump-folder (apply str (interleave (butlast (io/list-path the-path))
                                               (repeat "\\")))
          target dump-folder          
          _ (gui/alert (str "dumping peaks stats to " target))]            
      (stokeio/compute-peak-stats dump-folder target))))

;a quick plugin for stochastic demand generation.
(defn stoch-demand-dialogue []
  (request-path [wbpath "Please select the location of valid case-book."]
    (let [dump-folder (apply str (interleave (butlast (io/list-path wbpath))
                                               (repeat "\\")))
          _ (gui/alert (str "dumping to " dump-folder))]            
      (spit-tables 
       (helm/futures->tables 
        (helm/xlsx->futures wbpath :ignore-dates? true :log? false)) dump-folder))))

(defn clean-demand-dialogue []
  (request-path [wbpath "Please select the location of valid case-files."]
    (let [fl          (clojure.java.io/file wbpath)
          cases       {(str (io/fname fl) \_ "split.txt") (tbl/read-table fl)}
          ;dump-same?  @(future (gui/yes-no-box "Dump cases in same location?"))
          dump-folder ;(if dump-same?
          (apply str (interleave (butlast (io/list-path wbpath))
                                 (repeat "\\")))
          ;(select-folder))
          _ (print (str "dumping to " dump-folder))]      
      (spit-tables cases dump-folder))))

;;legacy auditing of marathon workbooks.....needs verification.
(defn audit-project-dialogue []
  (request-path [wbpath "Please select the location of valid case-files."]  
    (let [fl           (clojure.java.io/file wbpath)
          cases       {(str (io/fname fl) \_ "split.txt") (tbl/read-table fl)}
          ;dump-same?  @(future (gui/yes-no-box "Dump cases in same location?"))
          dump-folder ;(if dump-same?
          (apply str (interleave (butlast (io/list-path wbpath))
                                   (repeat "\\")))
            ;(select-folder))
            _ (print (str "dumping to " dump-folder))]      
      (spit-tables cases dump-folder))))

(defn capacity-analysis [wbpath]
  (let [pieces       (clojure.string/split wbpath #"\\")
        root         (io/as-directory (clojure.string/join "\\" (butlast pieces)))
        target       (last    pieces)
        _            (println [:running :capacity-analysis :at wbpath])]
    (run/run-cases root [target])))

(defn capacity-analysis-dialogue []
    (request-path [wbpath "Please select the location of a valid MARATHON project file."]  
                  (capacity-analysis wbpath)))

(defn requirements-analysis-dialogue []
    (request-path [wbpath "Please select the location of a valid MARATHON requirements project file."]  
                  (requirements/requirements-run wbpath)))

(defn debug-run-dialogue []
  (request-path [wbpath "Please select the location of a valid MARATHON project file."]
    (let [pieces       (clojure.string/split wbpath #"\\")
          root         (io/as-directory (clojure.string/join "\\" (butlast pieces)))
          target       (last    pieces)
          dbgtgt       (str root "debug.txt")
          _ (println [:performing-debug-run :to dbgtgt])]
      (with-open [wrtr (clojure.java.io/writer dbgtgt)]
        (binding [*out* wrtr]
          (marathon.ces.core/debugging                
           (capacity-analysis wbpath)
         ))))))

(defn debug-run-dialogue! []
  (request-path [wbpath "Please select the location of a valid MARATHON project file."] 
     (marathon.ces.core/debugging!                
      (capacity-analysis wbpath)
      )))

(defn examine-project-dialogue []
  (request-path [wbpath "Please select the location of a valid MARATHON project file."] 
     (run/examine-project wbpath)))

;;holy wow this is terrible.  must be a better way...
(defn menu-handler
  "Default menu-handling function.  Handles events coming from the 
   swing menu, which are typically just keywords, and dispatches them 
   to the appropriate command."
  [rpl]
  (fn [e]
    (let [expr 
          (case e
            :stochastic-demand  '(stoch-demand-dialogue)
            :compute-peaks      '(compute-peaks-dialogue) 
            :say-hello          '(println "hello!")
            :capacity-analysis  '(capacity-analysis-dialogue)
            :requirements-analysis  '(requirements-analysis-dialogue)
            :debug-run          '(debug-run-dialogue)
            :examine-project    '(examine-project-dialogue)
            :search-for         '(pprint/pprint
                                  (apropos
                                   (gui/input-box
                                    :prompt "Enter A Topic")))
            :load-script        '(load-file
                                  (gui/input-box
                                   :prompt "Select a Clojure script"))
            `(~'println ~e))]
      #_(org.dipert.swingrepl.main/send-repl rpl (str expr))
      (repl/echo-repl! expr))))

;;TODO: migrate the rest of this to seesaw, get rid of the old redundant
;;crud...
(defn hub [& {:keys [project exit? repl-options]}]
  (seesaw.core/invoke-later
   (binding [*ns* *ns*]
           (in-ns 'marathon.core)
           (let [project-menu    (gui/map->reactive-menu "Project-Management"  
                                                         project-menu-spec)
                 processing-menu (gui/map->reactive-menu "Processing"
                                                         processing-menu-spec)
                 debug-menu      (gui/map->reactive-menu "Debug"
                                                         debug-menu-spec)
                 scripting-menu  (gui/map->reactive-menu "Scripting"
                                                         scripting-menu-spec)
                 help-menu       (gui/map->reactive-menu "Help"
                                                         help-menu-spec)
                 main-menu       (gui/menu-bar (:view project-menu)
                                               (:view processing-menu)
                                               (:view scripting-menu)
                                               (:view debug-menu)
                                               (:view help-menu))        
                 menu-events     (obs/multimerge-obs [(-> project-menu :control :event-stream)
                                                      (-> processing-menu :control :event-stream)
                                                      (-> debug-menu   :control :event-stream)
                                                      (-> help-menu    :control :event-stream)
                                                      (-> scripting-menu :control :event-stream)
                                                      ])
                 handle-menu    (menu-handler nil)                    
                 _                 (->> menu-events 
                                        (obs/subscribe  #(handle-menu %)))        
                 ]
             (night/attach! :window-args
                            {:menubar main-menu
                             :on-close (if exit? :exit :dispose)})))))

(defn -main [& args]  (hub :exit? true))

;       :add-table   
;       :clear-project     
;       :load-project   
;       :save-project        
;       :view-project  
;       :view-table   
;       :add-table}))
                  


;;Just some mucking around with parallel mapping stuff.
;;Didn't see a boost.  Need to tweak this guy.
;; (defn chunked-pmap [f size coll]
;;   (->> coll 
;;        (partition-all size)
;;        (pmap (comp doall 
;;                    (partial map f)))
;;        (apply concat)))

               
               
