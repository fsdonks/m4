;;A little namespace for app-wide utilities.
;;If this stuff is general enough, it may get
;;ported to spork.
(ns marathon.util
  (:require [spork.util.io :as io]
            [clojure.java.io :as jio]
            [spork.cljgraph.core :as graph]
;            [spork.cljgraph.jungapi :as jung]
;            [spork.sketch :as sketch]
            [clojure.pprint :as pprint]))

(defn context? [x]
  (instance? spork.sim.simcontext.simcontext x))

;;util functions, move these out...
(defn compare-lines [l r]
  (with-open [left  (clojure.java.io/reader l)
              right (clojure.java.io/reader r)]
    (let [ls (line-seq left)
          rs (line-seq right)]
      (->> (map (fn [l r] [l r]) ls rs)
           (map-indexed (fn [i [x y]]
                          {:line i
                           :l x
                           :r y})
                        )
           (filter (fn [{:keys [l r]}]
                     (not= l r)))
           (first)))))

(defn compare-lines! [l r]
  (with-open [left  (clojure.java.io/reader l)
              right (clojure.java.io/reader r)]
    (let [ls (line-seq left)
          rs (line-seq right)]
      (->> (map (fn [l r] [l r]) ls rs)
           (map-indexed (fn [i [x y]]
                          {:line i
                           :l x
                           :r y})
                        )
           (filter (fn [{:keys [l r]}]
                     (not= l r)))
           (vec)))))

(defn tsv->csv [path]
  (with-open [rdr  (clojure.java.io/reader (str path))
              wrtr (clojure.java.io/writer (str path ".csv"))]
    (doseq [^String ln (line-seq rdr)]
      (.write wrtr (str (clojure.string/replace ln \tab \,) \newline)))))


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

(defn flip-graph [g]
  (graph/add-arcs
   graph/empty-graph
   (for [[from to w] (graph/arc-seq g)]
     [to from w])))

(defn callers [g target]
  
   (graph/subgraph (flip-graph  g) target))

(defn subproject->xgml [path rootns filterf]
  (let [g   (graph/subgraph (project->graph) rootns)
        dropped (filter (complement filterf) (graph/get-node-labels g))
        g (graph/drop-nodes g dropped)
        nds (graph/nodes  g)]
    (spit path 
          (xgml-graph g))))


(defn callgraph [path rootns]
  (let [g   (callers (project->graph) rootns)
;        dropped (filter (complement filterf) (graph/get-node-labels g))
;        g (graph/drop-nodes g dropped)
        nds (graph/nodes  g)]
    (spit path 
          (xgml-graph g))))
                             

    
  
  
  
(comment
  (defn flatten-requires [[req & xs]]
    (flatten 
     (for [reqs xs]
       (case (count reqs)
         1   (first reqs)
         (let [[root & decls] reqs]
           (or (seq (for [d (take-while #(not= % :refer) decls)
                     :when (coll? d)]
                      (symbol (str (name root) "." (name (first d))))))
               root))))))
  (defmacro timed-require [xs]
    (let [reqs (flatten-requires xs)]
      `(do ~@(for [r reqs]
               `(do (println (quote ~r))
                    (time (require (quote ~r) :reload)))))))
                                  
  '(:require [spork.util.table]            
             [marathon.ces
              [core     :as core]
              [engine   :as engine]
              [setup    :as setup]
              ]             
             [clojure.core.reducers :as r]
             [spork.util.reducers]
             [clojure.pprint :refer [pprint]]
             [marathon [project  :as proj]]
             [marathon.project [linked :as linked]
              [excel  :as xl]]
             [spork.entitysystem
              [diff    :as diff]
              [store   :as store]]
             [spork.sim.simcontext     :as sim]
             [marathon
              [observers :as obs]
              [serial    :as ser]
              [util      :as util]]
             )


;;from https://rosettacode.org/wiki/Linear_congruential_generator#Clojure
  (defn lcg [a b]
    (let [k (long (Math/pow 2 31))]
      (fn [n]
        (mod (+ (* n a)
                b)
             k))))

  (defn lcgen [a b init]
    (let [f     (lcg a b)
          prior (atom init)]
      (fn []
        (reset! prior (f @prior)))))
  ;;state_{n+1}=1103515245 * state_{n}+12345 mod 2^31
  ;;rand_{n}=state_{n}
  ;;rand_{n} is in range 0 to 2147483647.
  (defn bsdlcg [init] (lcgen 1103515245 12345 init))
  ;;state_{n+1}=214013 * state_{n}+2531011 mod 2^31
  ;;rand_{n}=state_{n} / 2^16
  ;;rand_{n} is in range 0 to 32767.
  ;;msvcrt prng from rand()
  (defn mslcg [init]
    (let [f (lcgen 214013 2531011 init)]
      (fn [] (bit-shift-right (f) 16))))

  (defn bsdlcg-stream [init]
    (let [f (bsdlcg init)]
      (repeatedly f)))
  (defn mslcg-stream [init]
    (let [f (mslcg init)]
      (repeatedly f)))
)
