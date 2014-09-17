;;An interactive application for Stoke.
(ns marathon.processing.stoke.app
  (:require [marathon.processing.stoke  [core :as stoke]
                                        [scraper :as scraper]
                                        [io :as stokeio]]
            [spork.cljgui.components [swing :as gui]]
            [spork.util [table :as tbl]
                        [io :as io]]))


(defmacro with-path
  "Given a root directory, and a collection of bindings in the form 
   [path [subdir1 subdir2...file]], evals body inside an expression 
   with *root* bound to the root path, and each binding available as 
   a fully-realized file path (relative-path *root* %) is called on 
   each pathlist)."
  [root bindings body]
  (let [binds (mapcat 
                (fn [[nm pathlist]] 
                  (list nm (list 'io/relative-path '*root* pathlist)))
                    (partition 2 bindings))]               
    `(let [~'*root* ~root
           ~@binds]
       ~body)))

(defn demand-case? [path]
  (let [path (clojure.string/upper-case path)]    
    (and (or (.contains path ".PEAK") (.contains path "CASE"))
         (.contains path ".TXT"))))

;;This is just to tie together two currently disparate pieces 
;;of the framework.  The stuff in scraper (poorly named) and the
;;main engine in stoke.core.

(defn folder->demand-stream [path]
  (map tbl/table-records (stokeio/folder->futures path :file-filter demand-case?)))

;;This is quite poor for performance....
(defn folder->rand-demand-stream [path]
  (shuffle (map tbl/table-records (stokeio/folder->futures path))))

;;Read a stoke workbook and perform a portfolio analysis.
(defn build-portfolio [project]
  (let [case-name      (-> project :Case :Case)
        futures-path   (-> project :Case :Futures)
        portfolio-size (-> project :Case :PortfolioSize)
        eval-size      (-> project :Case :EvaluationSize)
        hfill          (scraper/flow-filler project)
        supply         (:supply (meta hfill))
        demand-stream  (folder->demand-stream futures-path)
        evaluator      (stoke/strength-weighted-supply-evaluator 
                        (:SRCs project))
        results        (stoke/stoke-portfolio supply portfolio-size 
                          eval-size demand-stream 
                          (fn [s r] (:supply (hfill r :supply s))) 
                          evaluator)
        summary        (stoke/summarize-stoke-results results)]
    {:case      case-name
     :portfolio results
     :summary   summary
     :table     (stoke/performance-table summary)
     }))

(defn portfolio->supplies [p]
  (for [[k m] (:forces (:portfolio p))]
    {:force k :total-strength (:total-strength m) :supply (:supply m)}))

(defn supply->records [s]
  (for [[[src compo] qty] s]
    {:src src :compo compo :quantity qty}))

(defn spit-forces [outfolder p]
  (doseq [force (portfolio->supplies p)]
    (let [name (:force force)
          supply (supply->records (:supply force))]
      (do (io/hock (str outfolder name ".txt") 
                   (tbl/spit-records [(dissoc force :force)]))
          (io/hock (str outfolder "\\forces\\" name ".supply.txt")
                   (tbl/spit-records supply))))))

;;Note-> there's a bug in the io/with-path macro that's assuming 
;;everything is namespace relative.

(defn save-portfolio! [outfolder portfolio]
  (let [summary (:summary portfolio)
        folio   (:portfolio portfolio)
        forces  (:forces folio)
        futures (:futures folio)
        peaks   (:peaks folio)
        perf    (:performance folio)]
    (with-path outfolder 
      [comparison  ["Comparison.txt"]
       best        ["Best.txt"]
       performance ["Performance.txt"]
       readme      ["Readme.txt"]]
         (do (io/hock readme (str "Output for Case: " (:case portfolio)))
             (io/hock comparison (tbl/table->tabdelimited (:table portfolio)))
             (io/hock best   (str (vec (:top-performers summary))))
             (io/hock performance (tbl/spit-records perf))
             (spit-forces outfolder portfolio)))))  

(defn do-cases [source-project root-path] 
  (let [future-paths (map spork.util.io/fpath 
                          (filter spork.util.io/folder? 
                                  (spork.util.io/list-files 
                                   root-path)))] 
    (doseq [p future-paths]
      (let [futures (str p "\\peaks\\")
            _       (println [:building p])
            res (build-portfolio (assoc-in source-project [:Case :Futures] futures))
            outpath (str p "\\results\\")
            _       (when (spork.util.io/fexists? (clojure.java.io/as-file outpath)) 
                      (do (println [:clearing outpath])            
                          (spork.util.io/clear-folders! outpath)))
            _       (println [:saving outpath])]
        (save-portfolio! outpath res)))))

;;Helpful analysis functions.  Probably move to stoke.app
(defn supply->table [supply & {:keys [compo-map]
                               :or {compo-map {'RCAD 'RC
                                               'RCAD-BIG 'RC}}}]
  (let [src->strength (:src->strength supply)
        get-compo (fn [compo] (get compo-map compo compo))]
    (-> (for [[[src compo] qty] (:supply supply)]
          {:SRC src :SubComponent compo :Component (get-compo compo) 
           :Quantity qty :Strength (src->strength src)})
        (tbl/records->table))))

(defn fills->table [fill-results]                      
  (let [fields (into [:fill-index :fill-flow :fill-SRC :Component 
                      :str-required :total-flow :flow-cost]  
                      (keys (:demand (first fill-results))))]                      
    (->> fill-results
         (map-indexed 
          (fn [idx {:keys [demand str-required fills total-flow flow-cost] :as r}]
            (map (fn [fill] (merge demand 
                                   (dissoc r :demand :fills)
                                   {:fill-index idx
                                    :fill-flow (:quantity fill)
                                    :Component (:compo fill)
                                    :fill-SRC  (:src fill)}))
                 fills)))
         (flatten)                                                            
         (tbl/records->table)
         (tbl/order-fields-by fields))))


(defn supply->txt  [supply]  (tbl/table->tabdelimited (supply->table supply)))
(defn fills->txt   [fills]   (tbl/table->tabdelimited (fills->table fills)))
(defn paste-supply [supply]  (spork.util.clipboard/paste! (supply->txt supply)))
(defn paste-fills  [fills]   (spork.util.clipboard/paste! (fills->txt fills)))

;;testing 
(comment 
(def basepath "C:\\Users\\thomas.spoon\\Documents\\stochdemand\\stoke-beta\\")
(def projpath450 "C:\\Users\\thomas.spoon\\Documents\\stochdemand\\stoke-beta\\FMCA450.stoke.xlsx")
(def projpath420 "C:\\Users\\thomas.spoon\\Documents\\stochdemand\\stoke-beta\\FMCA420.stoke.xlsx")
(def projpath450-acfirst "C:\\Users\\thomas.spoon\\Documents\\stochdemand\\stoke-beta\\respanel\\FMCA450.stoke-acfirst.xlsx")
(def outpath "C:\\Users\\thomas.spoon\\Documents\\stochdemand\\stoke-beta\\results\\")
(def the-project (scraper/read-project (or (gui/select-file) projpath)))


(def excroot "C:\\Users\\thomas.spoon\\Documents\\stochdemand\\stoke-beta\\respanel\\")
;(def hfill       (partial scraper/proj->supply-gen the-project))
;(def demands (
;(def port  (stoke/supply->portfolio 
(def res (build-portfolio the-project))



)


    
        

