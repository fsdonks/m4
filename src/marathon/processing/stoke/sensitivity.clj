(ns marathon.processing.stoke.sensitivity
  (:require [marathon.processing.helmet [core :as helm]  [split :as split]]
            [marathon.processing.stoke  [io :as stokeio] [scraper :as scraper] [app :as app] [core :as stoke]]
            [spork.util [table :as tbl]]))




(def records->peak-records (comp stokeio/peaks->records stokeio/table->peaks))

;;This is a quick hack to facilitate generating one or more splits
;;based off of a demand future case.  Given a set of demand splits, we
;;use the same demand split map defined in the workbook, and basically
;;just tweak the parameters.
(defn demand-sensitivity [wbpath & {:keys [splits peaks?] :or {splits [60 90 180 360] peaks? nil}}]
  (let [db  (helm/read-casebook :wbpath wbpath :ignore-dates? true)
        sm  (helm/validate-splitmap 
                   (helm/table->lookup db :DemandSplit :DemandSplit))]
    (let [res  (->> (helm/post-process-cases db (helm/compile-cases db) :log? false :processes #{:collide})
                    (mapcat (fn  [[case-key case-records]]                  
                              (->>  
                               (split/future->multiple-splits case-records  sm splits)
                               (map-indexed (fn [idx fut]
                                              [(into case-key ["split" (nth splits idx)])  fut]))))))]
      (if peaks? (map (fn [[case-key case-records]]
                        [(conj case-key "peak")
                         (records->peak-records case-records)])
                      res)
          res))))

  
;;A simple supply sensitivity analysis.  We compute multiple supplies
;;relative to the 
                                         
(defn build-sensitivity-portfolio [project finite-demands]
  (assert (vector? finite-demands) "cannot pass a potentially infinite data structure")
  (let [n              (count finite-demands)
        case-name      (-> project :Case :Case)
        futures-path   (-> project :Case :Futures)
        portfolio-size (-> project :Case :PortfolioSize)
        eval-size      (-> project :Case :EvaluationSize)
        hfill          (scraper/flow-filler project)
        supply         (scraper/build-initial-supply project)
        demand-stream  (cycle finite-demands)
        evaluator      (stoke/strength-weighted-supply-evaluator 
                        (:SRCs project))
        results        (stoke/stoke-portfolio supply n 
                          n demand-stream 
                          (fn [s r] (:supply (hfill r :supply s))) 
                          evaluator)
        summary        (stoke/summarize-stoke-results results)]
    {:case      case-name
     :portfolio results
     :summary   summary
     :table     (stoke/performance-table summary)
     }))        

                                         
(defn build-sensitivity-supplies [project finite-demands]
  (assert (vector? finite-demands) "cannot pass a potentially infinite data structure")
  (let [n              (count finite-demands)
        case-name      (-> project :Case :Case)
        hfill          (scraper/flow-filler project)
        supply         (scraper/build-initial-supply project)
        demand-stream  (cycle finite-demands)]    
    (map (fn [[case-name fut]] 
           (let [res (hfill fut :supply supply)]
             [case-name {:supply (:supply res) :fills (:fills res)}])) finite-demands)))

(defn case->filename [coll]
  (clojure.string/join "_" coll))

;;This will be slower...
;; (defn roll-tables [kvps field-name]
;;   (tbl/concat-tables
;;    (for [[ k v] kvps]
;;      (let [t (app/supply->table v)
;;            n  (tbl/count-rows t)]      
;;        (tbl/conj-field [field-name (vec (take n (repeat k)))]
;; t)))))

(defn roll-tables [kvps field-name]
  (->> (reduce (fn [records [k table]]
                 (reduce (fn [acc rec]
                           (->> (assoc rec field-name k)
                                (conj! acc)))
                         records 
                         (tbl/table-records table)))
               (transient [])
               kvps)
       (persistent!)
       (tbl/records->table)))          


(defn spit-supplies [outfolder supply-pairs]
  (let [fillpath (str outfolder "\\fills\\")
        supplypath (str outfolder "\\forces\\")]
    (doseq [[case {:keys [supply fills]}] supply-pairs]
      (do (spork.util.io/hock (str fillpath (case->filename case) ".txt") (app/fills->txt fills))
          (spork.util.io/hock (str supplypath (case->filename case) ".txt") (app/supply->txt supply))))))

(defn spit-supplies-rolled [outfolder supply-pairs]
  (let [fillpath   (str outfolder "\\fills\\fills.txt")
        supplypath (str outfolder "\\forces\\forces.txt")
        fills      (map (fn [[case m]] [(case->filename case) (app/fills->table  (:fills m))])  supply-pairs)
        supplies   (map (fn [[case m]] [(case->filename case) (app/supply->table (:supply m))]) supply-pairs)]
    (do (spork.util.io/hock fillpath (tbl/table->tabdelimited (roll-tables fills :SensitivityCase)))
        (spork.util.io/hock supplypath   (tbl/table->tabdelimited (roll-tables supplies :SensitivityCase))))))

;; (defn spit-supplies-wb [outfolder supply-pairs]
;;   (let [fillpath (str outfolder "\\fills.xlsx")
;;         supplypath (str outfolder "\\forces.xlsx")
;;         [fills supplies] (reduce (fn [[fs supps] [case {:keys [fills supply]}]] 
;;                                    (let [name (case->filename case)]
;;                                      [(assoc fs name (app/fills->table fills)) (assoc supps name (app/supply->table supply))]))
;;                                  [{} {}]
;;                                  supply-pairs)]
    
;;       (do (spork.util.excel.core/tables->xlsx fillpath    fills)
;;           (spork.util.excel.core/tables->xlsx supplypath  supplies))))



(comment                                         


(def split-path "C:\\Users\\thomas.spoon\\Documents\\stochdemand\\stoke-beta\\StochasticDemandInput_1519Baseline_sensitivity.xlsx")
(def sensitivity-path "C:\\Users\\thomas.spoon\\Documents\\stochdemand\\stoke-beta\\sensitivity")

(def splits  (demand-sensitivity split-path :peaks? true))
(def basepath "C:\\Users\\thomas.spoon\\Documents\\stochdemand\\stoke-beta\\")
(def projpath450 "C:\\Users\\thomas.spoon\\Documents\\stochdemand\\stoke-beta\\FMCA450.stoke.xlsx")
(def projpath420 "C:\\Users\\thomas.spoon\\Documents\\stochdemand\\stoke-beta\\FMCA420.stoke.xlsx")
(def projpath450-acfirst "C:\\Users\\thomas.spoon\\Documents\\stochdemand\\stoke-beta\\FMCA450.stoke-acfirst.xlsx")
(def projmap {"C:\\Users\\thomas.spoon\\Documents\\stochdemand\\stoke-beta\\FMCA450.stoke-nomixconstraints.xlsx"
               (str sensitivity-path "nomixconstraints")
               "C:\\Users\\thomas.spoon\\Documents\\stochdemand\\stoke-beta\\FMCA450.stoke-unconstrained-acfirst.xlsx"
               (str sensitivity-path "unconstrained-acfirst")})


(def outpath "C:\\Users\\thomas.spoon\\Documents\\stochdemand\\stoke-beta\\results\\")
(def the-project (scraper/read-project (or (gui/select-file) projpath)))

(def excroot "C:\\Users\\thomas.spoon\\Documents\\stochdemand\\stoke-beta\\demand_excursions\\excursion-90\\")
;(def hfill       (partial scraper/proj->supply-gen the-project))
;(def demands (
;(def port  (stoke/supply->portfolio 
(def res (build-sensitivity-portfolio the-project (vec splits)))


)
