;;Provides support for a linked project,
;;which will reload from source data if the file
;;has been modified.
;;Otherwise, tables are cached.
;;This is mainly to support supply and demand
;;modifications for testing I'm doing....
;;Later, it'll let users maintain an interface
;;with excel so they can modify data and do
;;runs interactively.
(ns marathon.project.linked
  (:require [marathon.project.excel :as mxl]
            [clojure.java.io :as io]
            [spork.util.excel [core :as xl] [docjure :as docj]]
            [spork.util.table :as tbl]))

;;This works pretty well...
;;The only downside is that it's memory intensive to load workbooks
;;this way, so we'd prefer to use a streaming variant.  We also
;;get sharing violations if we open up and edit/save using the
;;file-based stream solution, so for now we use the workbook
;;factory method to be safe.  That provides the interactive
;;feedback loop we're looking for.  Enough to edit in excel
;;and go on from there.
(def p "C:\\Users\\tspoon\\Documents\\srm\\notionalbase.xlsx")
(defn mod-date [path]
  (if path
    (.lastModified ^java.io.File (io/file path))
    (throw (Exception. (str [:null-path path])))))

(def tables    (atom nil))
(defn set-path [path]
  (let [t (mod-date path)]
  (reset! tables {:path path
                  :date t})))
(defn fresh? []
  (when-let [path (:path @tables)]
    (= (mod-date path) (:date @tables))))

(defn marathon-book->marathon-tables 
  [wbpath & {:keys [tables] :or {tables 
                                 mxl/marathon-workbook-schema}}]
  "Extract a map of canonical tables to a map with the same name.  Caller can 
   supply additional tables, or supply the :all keyword to get all tables."
  (let [wb (xl/as-workbook wbpath)
        res     (into {} (for [[nm sheetname] (seq tables)]
                           (do (println nm)
                               [nm (xl/sheet->table (xl/as-sheet sheetname wb))])))]
    (.close wb)
    res))

;;these act like a drop-in replacement for sampledata.
;;So, we can interact with a project from Excel and jack around.
;;Turn stuff on/off, etc.
;;typically, we want to support a get-table function.
(defn get-table [nm]
  (if  (and (fresh?) (contains? @tables nm))
    (get @tables nm)
    (let [_ (println (str [:reloading nm :from (:path @tables)]))
          loaded-tables (mxl/marathon-book->marathon-tables
                         (or (:path @tables) (throw (Exception. "no path set!")))
                         :tables {nm (get mxl/marathon-workbook-schema nm)})
          _           (swap! tables merge (assoc loaded-tables
                                                 :date (mod-date (:path @tables))))]
      (get @tables nm))))
(defn get-table-records  ([name] (tbl/record-seq (get-table  name))))
                   



