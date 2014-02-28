;;A namespace for determining parent-child relations, deriving
;;most likely parents, etc.
(ns marathon.processing.pedigree
  (:require [spork.cljgraph [core :as graph]]
            [spork.util [table :as tbl]]))

;;Yet another implementation of levenshtein edit distance.
;;This can be made faster by using a mutable array.  Later...
;;Also, this is a port from wikipedia.  I should implement it more
;;idiomatically.
(defn levenshtein 
  "Computes the edit distance between strings s1 and s2"
  (let [m (count s1)
        n (count s2)
        get-distance (fn [d i j] (get-in d [i j] 0))
        set-distance (fn [d i j dist] (assoc-in d [i j] dist))]
    (get-distance 
     (->> (for [j (range n) i (range m)] [j i])
          (reduce 
           (fn [d from-to]
             (let [i (second from-to)
                   j (first from-to)]
               (if (= (nth s1 i) (nth s2 j))
                 (set-distance d i j (get-distance d (dec i) (dec j)))
                 (set-distance d i j (min (inc (get-distance d (dec i) j))
;deletion
                                          (inc (get-distance d i   (dec j)))
;insertion
                                          (inc (get-distance d (dec i) (dec j))))))))
;substitution
           {}))
     (dec m) (dec n))))

;;missing sample data.  Just a supply table.
(def sample-data nil)
                                            
(def sample-table (tbl/tabdelimited->table sample-data))
;;create an initial graph structure based on the adjacencies
;;in the table data.
(defn tabl->arcs [the-table]
  (map (juxt :Parent :SRC (keyword "Unit Count")) (tbl/table-records the-table)))

(def dependencies (graph/arcs->graph (table->arcs sample-table)))
(def components   (graph/decompose dependencies))
