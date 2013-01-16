;TOM SPOON 10 July
;A simple package for enabling file and folder selection dialogues, assumably 
;from the command line.  Uses Swing GUI componenets.
;Also exposes a few simple methods for viewing data.  The inspiration for a 
;generic 'view' method came from Incanter, which is REALLY nice.  I just 
;wanted a lightweight swing viewer for certain instances.  For heavier 
;stuff, use cljgui, which has a full-featured user interface framework. 

(ns util.gui
  (:require [util.table :as tbl])
  (:import [javax.swing JFileChooser JTable JScrollPane JFrame]
           [java.util Vector]))

(defn select-file
  "Initiates a file selection dialogue using a Swing file chooser.  
   Returns the path to the selected file."
  ([initpath]
  (let [fc (JFileChooser. initpath)
        res (. fc showOpenDialog nil)]
    (if (= res (. JFileChooser APPROVE_OPTION))
      (str (.getSelectedFile fc))
      nil)))
  ([] (select-file (System/getProperty "user.dir"))))

(defn- folder-chooser [initpath]
  (let [f (JFileChooser. initpath)]
    (do 
      (.setFileSelectionMode f JFileChooser/DIRECTORIES_ONLY)
      f)))

(defn select-folder
  "Initiates a folder selection dialogue using a swing folder chooser.
   Returns the path to the selected folder."  
  ([initpath]   (let [f (folder-chooser initpath)
                      res (. f showOpenDialog nil)]
                  (if (= res (. JFileChooser APPROVE_OPTION))
                    (str (.getSelectedFile f))
                    nil)))

  ([] (select-folder (System/getProperty "user.dir"))))

;again, inspired by the awesome work from Incanter!  
(defmulti view (fn [obj & options] (if (tbl/tabular? obj) :ITable (class obj))))
(defmethod view :ITable [obj & {:keys [title] :or {title "Table"}}]
   (let [col-names (tbl/get-fields obj)
         column-vals (tbl/table-rows obj)]
     (doto (JFrame. title)
       (.add (JScrollPane. (JTable. (Vector. (map #(Vector. %) column-vals))
                                    (Vector. col-names))))
       (.setSize 400 600)
       (.setVisible true))))  
  

