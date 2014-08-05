(ns marathon.processing.stoke.scraper
  (:require [spork.util [io :as io]
                        [table :as tbl]
                        [temporal :as temporal]]
            [spork.util.excel [core :as xl]]
            [marathon.processing.stoke [core :as stoke]]))

(defn demand-case? [path]
  (let [path (clojure.string/upper-case path)]
    (and (.contains path "CASE")
         (.contains path ".TXT")
         (not (.contains path ".PEAK")))))

(defn demand-file-paths
  "Rips some stochastic futures from a path."
  [path file-filter]
  (->> (io/list-files path)
       (map io/fpath)
       (filter (fn [name] (.contains name ".txt")))))

(defn demand-file->table [path]
  (tbl/tabdelimited->table (slurp path) :parsemode :no-science))
(defn table->peaks [xs]
  (temporal/peaks-by :SRC (tbl/table-records xs)
                     :start-func :StartDay :duration-func :Duration
                     :peak-function stoke/total-quantity-demanded))
(defn peaks->records [peaks]
  (->> (for [[k v] peaks] (:actives v))
       (reduce (fn [acc xs] (reduce conj acc xs)))))

(defn peak-name [path] (str path ".peak.txt"))
(defn peak-table->path [path xs] 
  (spit (peak-name path) (tbl/table->tabdelimited xs)))
(defn folder->futures 
  "Given a folder path, produces a demand stream of all the futures."
  [path & {:keys [file-filter] :or {file-filter demand-case?}}]
  (->> (demand-file-paths path file-filter)
       (map demand-file->table)))

;;useful for stoke
(defn folder->random-demand-tables [path]
  (->> (folder->futures path)
       (map table->peaks)
       (map peaks->records)
       (map tbl/records->table)))

(defn dump-peaks 
  [root peak-tables  
   & {:keys [names] :or {names (map peak-name (range (count peak-tables)))}}]
  (let [abs-path (partial io/relative-path root)]
    (doseq [[name tbl] (map vector names peak-tables)]
      (io/hock (abs-path [name]) (tbl/table->tabdelimited tbl)))))

(defn compute-peaks [path & [target]]
  (->> (folder->random-demand-tables path)
       (dump-peaks (or target path))))

(defn workbook->stoke-project [path] (xl/wb->tables (xl/as-workbook path)))
;;

;; (defn get-srcs [stoke-project]
;;   (->> (get "InitialSupply" stoke-project)
;;        (reduce (fn [[supply srcs compos] r]
;;                   (let [src (get "SRC" r)
;;                         compo (get "Compo" r)]
;;                     [(conj supply [(get "SRC" r) (get "Compo")
                        
