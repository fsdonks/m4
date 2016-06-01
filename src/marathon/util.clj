;;A little namespace for app-wide utilities.
;;If this stuff is general enough, it may get
;;ported to spork.
(ns marathon.util
  (:require [spork.util.io :as io]
            [clojure.java.io :as jio]
            [spork.cljgraph.core :as graph]
            [spork.cljgraph.jungapi :as jung]
            [spork.sketch :as sketch]
            [clojure.pprint :as pprint]))

;;eliminate tempfiles...
(def tempre #"[#~]")
(def clj    #".clj")

(defn valid? [name]
  (and (not (re-find tempre name))
       (re-find clj  name)))

;;Let's reify our project dependencies...
(defn crawl-project-source []
  (->>  (file-seq (jio/file "./src/marathon/"))
        (map io/fpath)
        (filter valid?)))

(def reqreg  #"(:require .*)")
(def nsreg   #"\(ns.*\)")

;;Assuming the ns is in the top of the file..
(defn get-form
  ([^String src from to]
  ;;this is an ugly, ugly way to parse the namespace block.
   (let [bound to]
     (loop [idx   from
            start nil
            depth 0]
       (if (== idx bound) nil ;nothing found.
           (let [c   (.charAt src idx)
                 nxt (unchecked-inc idx)]       
             (cond
               ;;begin parens.
               (= c \()   (recur nxt (or start idx) (unchecked-inc depth))
               (= c \))   (if (== depth 1) ;;finished the form
                            [start nxt];(.substring src start nxt)  ;;bail                           
                            (recur nxt start (unchecked-dec depth)))
               :else (recur nxt start depth)))))))
  ([^String  src] (get-form src 0 (count src))))


;;deompose a namespace declaration into
;;dependency info we care about...
(defn ns-form->ns-map [frm]
  (let [[_ namespace & decs] frm]
    (loop [acc {:ns namespace}
           xs decs]
      (if-let [expr (first xs)]
        (recur (assoc acc (first expr) (rest expr)) (rest xs))
        acc))))

(defn ns-decl [^String x]
  (let [bound (count x)]
    (loop [idx 0]
      (when-let [[l r] (get-form x idx bound)]
        (let [frm (.substring x l r) ]
          (if (.contains ^String frm "ns")
            (clojure.edn/read-string frm)
            (recur (unchecked-inc r))))))))

(defn project->ns-maps []
  (map (fn [path]
         (-> path
             (slurp)
             (ns-decl)
             (ns-form->ns-map)
             (assoc :path path))) (crawl-project-source)))


;;[spork.ai.core [:as ai :refer [deref! fget fassoc push-message- map->entityctx debug ->msg]] ]
;;xs is either...
;;[:as ai :refer [deref! fget fassoc push-message- map->entityctx debug ->msg]]
;;or [[blah :As blee] [y :as y]]
;;If all vectors, then they're all xtensions of the base namespace.

(defn requires->arcs [xs]
  (apply concat  (for [[basens & decs] xs]
                   (if (and (seq decs)
                            (every? vector? decs))
                     (for [[term & blah] decs]
                       (symbol (str basens "." term)))
                     [basens]))))

(defn imports->arcs [xs]
  (for [[basens & decs] xs
        cls decs]
    (symbol (str basens "." cls))  ))

(defn project->arcs [xs]
  (apply concat 
         (for [{:keys [ns require import]} xs]
           (concat 
                  (map (fn [x]
                         [ns x :require]) (requires->arcs require))
                  (map (fn [x]
                         [ns x :import]) (imports->arcs import))
                  ))))

(defn project->graph []
  (graph/arcs->graph 
   (project->arcs (project->ns-maps))))



(defn labeled-node [id lbl]
  (pprint/cl-format nil
      "<section name=\"node\">
		<attribute key=\"id\" type=\"int\">~A</attribute>
		<attribute key=\"label\" type=\"String\">~A</attribute>
	</section>"
      id lbl))

(defn edge [source target w]
  (pprint/cl-format nil 
      "<section name=\"edge\">
          <attribute key=\"source\" type=\"int\">~A</attribute>
          <attribute key=\"target\" type=\"int\">~A</attribute>
          <attribute key=\"label\" type=\"String\">~A</attribute>
      </section>"
      source target w))

(defn edge [source target w]
  (pprint/cl-format nil 
      "<section name=\"edge\">
          <attribute key=\"source\" type=\"int\">~A</attribute>
          <attribute key=\"target\" type=\"int\">~A</attribute>
      </section>"
      source target w))


(defn xgml-graph [gr]
  (let [header "<?xml version=\"1.0\" encoding=\"Cp1252\"?>"
        nds (graph/nodes gr)
        nd->int (into {} (map-indexed (fn [idx [k v]] [k idx]) nds))]
    (clojure.string/join
     \newline
     (concat [header
             "<section name=\"xgml\">"
             "<section name=\"graph\">"]
             (map (fn [[lbl id]] (labeled-node id lbl)) nd->int)
             (map (fn [[from to w]]
                    (edge (nd->int from)
                          (nd->int to)
                          w)) (graph/arc-seq gr))
             ["</section>"
              "</section>"]))))

(defn project->xgml [path]
  (let [g   (project->graph)
        nds (graph/nodes  g)]
    (spit path 
          (xgml-graph g))))

(defn subproject->xgml [path rootns filterf]
  (let [g   (graph/subgraph (project->graph) rootns)
        dropped (filter (complement filterf) (graph/get-node-labels g))
        g (graph/drop-nodes g dropped)
        nds (graph/nodes  g)]
    (spit path 
          (xgml-graph g))))


    
                             

    
  
  
  
