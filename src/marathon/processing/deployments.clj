;;Processes the deployment records from a Marathon project and projects them 
;;onto one or more Incanter scatter plots.  Consistent with the legacy 
;;deployment dot plots from Excel, but more flexible and interactive.
(ns marathon.processing.deployments
  (:require [spork.util [io :as io]])
  (:use [spork.util.table]
        [spork.incanter.extensions]
        [incanter core charts]))

;Processes a table of records.
;First, splits the table into N subtables.
(defn SRCtext->table
  "Rips text from string txt, parsing under the assumption that certain
   alphanumeric codes for SRCs should be parsed as strings, not scientific
   notated numerals."
  [txt]
  (tabdelimited->table txt :parsemode :noscience))

(defn make-subtables
  "Acts like group-by for sequences, except it is applied
   to a table. Creates N subtables, one for each value produced by f."
  [tbl f]
  (let [newtable (merge empty-table {:fields (get-fields tbl)} )]
    (loop [subtables {}
           xs (record-seq tbl)]
      (if (seq xs)
        (let [y (f (first xs))
              acc (as soc subtables y
                      (let
                        [subtable (get subtables y newtable)]
                        (merge subtable
                               {:records (conj (:records subtable)
                                               (vec (vals (first xs))))}
                               )))]
          (recur acc (rest xs)))
        subtables))))

(defn get-src [rec] (get rec "UnitType"))
(defn get-compo [rec] (get rec "Component"))
(defn deploytable->srctables
  "Converts a deployments table to a map of sub tables, grouped by the SRC
   field. "
  [tbl]
  (make-subtables tbl get-src))

(defn get-edges
  "Computes the set of edges in an undirected graph from xs."
  [xs]
  (let [vs (into [] xs)]
    (for [i (range (count vs))
          j (range (count vs)) :when (> j i)]
      [(nth vs i) (nth vs j)])))


(defn group-key
  "Retrieves the unique value associated with the kvmap. Incanter's $group-by
   creates keys of {group val}, where the map itself is a key. This is an aux
   function for retrieving the actual group value."
  [kvmap]
  (first (vals (key kvmap))))

(defmacro with-fields [binds & body]
  `(let [~@ (mapcat (fn [[alias nm]] (list alias nm)) (partition 2 binds))]
     ~@body))

(def fieldmap {:dwell     "DwellBeforeDeploy" :supplysrc "UnitType"
               :demandsrc "DemandType"        :compo "Component"
               :followon  "FollowOn"})

(defn fieldname [s] (get fieldmap s))

;define some criteria ...
(def  first-deployment? {"FollowOn" "FALSE"})
(defn src? [src] {"UnitType" src})
(defn demand? [src] {"DemandType" src})

;a compound query
(defn get-deployers
  ([src deployset] ($where (merge first-deployment? (src? src)) deployset))
  ([deployset] ($where first-deployment? deployset)))

(defn dwell-scatter [deployerset]
  (scatter-plot "Deploylnterval" "DwellBeforeDeploy"
                :data deployerset
                :group-by "Component"
                :series true
                :title "Deployment Visualization"
                :x-label "Deploy Day"
                :y-label "Dwell Before Deployment (Days)"
                :legend true))
 
(defrecord chart [type trends colormap])

(defn dwell-data [deployset time-unit]
  (let [temporal-map (fn [x] (->> (if (seq? x) x (list x))
                               (map timef)))
        timef (case time-unit
                :day identity
                :year #(/ % 365)
                :quarter #(/ % 90))
        components ($group-by "Component" deployset)]
    (for [kv (sort-by #(first (vals (key %))) components)]
      {:title (group-key kv)
       :xs (temporal-map ($ "Deploylnterval" (val kv)))
       :ys (temporal-map ($ "DwellBeforeDeploy" (val kv)))})))

(defn dwell-plot
  "A function that renders data from a Marathon deployments table into a
   canonical scatter plot. Trends are seperated by component. The title
   of the plot is src, the plot contains N trends, one for each value in
   the 'Component' field."
  ([title xs ys time-unit]
    (let [chrt (doto (empty-xy) (set-title title))]
      (add-point-series chrt xs ys compo))
    (doto chrt
      (set-x-Label "Year Deployed")
      (set-y-Label "Dwell Prior to Deployment (years)")))
  ([deployset title] (dwell-plot deployset title :year)))

(defn string-eq?
  "A compares where the strings contain similar characters, case insensitive."
  [sl s2]
  (= (clojure.string/capitalize sl) (clojure.string/capitalize s2)))

(defn truthy?
  "Hack to allow for literal booleans in strings."
  [x]
  (if (string? x)
    (= (clojure.string/capitalize x) "TRUE")
    (true? x)))

(defn basic-substring?
  "Capitalizes sl and s2, returns true if 82 is contained by 81."
  [s1 s2]
  (.contains (clojure.string/capitalize s1)
    (clojure.string/capitalize s2)))

(defn osmotic-deployment?
  "Returns true for deployments that were from pools known
   to cause a change in status."
  [row]
  (let [pred (partial basic-substring? (row "Policy"))]
    (or (pred "OpSus")
        (pred "Mission"))))

(defn proxy-deployment?
  "Returns true if a deployment record was not an artificial
   proxy record."
  [row]
  (string-eq? (row "DemandGroup") "NotUtilized"))

(defn follow-on? [row]
  (truthy? (row "FollowOn")))

(defn real-deployment?
  "Returns true if a deployment record was not an artificial
   proxy record or a follow-on deployment."
  [row]
  (not (or (proxy-deployment? row)
           (follow-on? row))))

(defn load-samples
  "Loads deployment samples from a deployments table text file."
  [& {:keys [workdir] :or {workdir (relative-path :docs ["Martian"])}}]
  (let [workfile (relative-path workdir ["deployments.txt"])]
    (table->dataset
      (tabdelimited->table
        (slurp ((comp uri->file path->uri) workfile)) :parsemode :noscience))))

(defn multi-filter [filters]
  "Creates a filter that is the logical AND of multiple predicate functions."
  (fn [x]
    (loop [fs filters
           res true]
      (when res
        (if-let [f (first fs)]
          (recur (rest fs) (and res (f x)))
          res)))))

(defn dwell-plot-seq
  "Given a deployment table, computes a lazy sequence of dwell plots for each
   SRC. "
  [d & {:keys [filters] :or {filters [real-deployment?]}}]
  (let [srcdataset (->> (query-dataset d (multi-filter filters))
                     ($group-by "UnitType"))]                     
    (map (fn [[kv subset]]
           (let [g (first (vals kv))]
             [g (dwell-plot subset g)]))
         srcdataset)))

(defn dwell-groups
  "Given a deployment table, computes a lazy sequence of dwell plots for each
   SRC."
  [d & {:keys [filters] :or {filters [real-deployment?]}}]
  (let [srcs (->> (query-dataset d (multi-filter filters))
               ($group-by"UnitType"))]
    (seq srcs)))

(def BCTlookup
  (dataset ["SRC" "BCT" "OITitle"]
           [["77302R200" "IBCT" "HEADQUARTERS" "INFANTRY BRIGADE COMBAT TEAM"]
            ["77302R000" "IBCT" "HEADQUARTERS" "INFANTRY BRIGADE COMBAT TEAM"]
            ["87302R100" "HBCT" "HEADQUARTERS" "HEAVY BRIGADE COMBAT TEAM (HBC"]
            ["47102R500" "SBCT" "HEADQUARTERS" "AND HEADQUARTERS COMPANY, STRYK"]]))

;It'd be nice to design a language that, given the vast set of deployments,
;scales the results for us ...
;Trying to talk about dots declaratively.
; (->dotplot (->trend xs ys : red)
; (->trend
