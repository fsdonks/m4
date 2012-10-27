;TOM SPOON 10 July
;A simple package for enabling file and folder selection dialogues, assumably 
;from the command line.  Uses Swing GUI componenets.
(ns util.gui
  (:import [javax.swing JFileChooser]))

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
