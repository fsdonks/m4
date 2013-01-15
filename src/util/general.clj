;A collection for general utilities.  This is basically a dumping ground for
;small utilities that are undeserving of a seperate library.
(ns util.general)


;helper functions....I need these somewhere else, since they're universal.

(defn clump
  "Returns a vector of a: results for which keyf returns an identical value.
   b: the rest of the unconsumed sequence."
  ([keyf coll]
    (when (seq coll) 
      (let [k0 (keyf (first coll))]
        (loop [acc (transient [(first coll)])
               xs (rest coll)]
          (if (and (seq xs) (= k0 (keyf (first xs))))
            (recur (conj! acc (first xs)) (rest xs))
            [[k0 (persistent! acc)] xs])))))
  ([coll] (clump identity coll)))
                                
(defn clumps
  "Aux function. Like clojure.core/partition-by, except it lazily produces
   contiguous chunks from sequence coll, where each member of the coll, when
   keyf is applied, returns a key. When the keys between different regions
   are different, a chunk is emitted, with the key prepended as in group-by."
  ([keyf coll]
    (lazy-seq
      (if-let [res (clump keyf coll)]
        (cons  (first res) (clumps keyf (second res))))))
  ([coll] (clumps identity coll)))

(defn unfold
  "unfold takes a generating function, f :: state -> state | nil,
   a halting function, halt?:: state -> bool, and an intial state s.  Returns
   a sequence of the application of (f (f (f s))) while not halt?"
  [halt? f s]
  (take-while #(not (halt? %)) (iterate f s)))     

(defn generate
  "generate is akin to unfold, except it uses recursion instead of sequences to
   avoid overhead associated with sequences - if needed.  Bear in mind that
   unfold may be about 5x slower due to uses of seqs (from naive testing), which
   makes generate more useful when performance matters.  Takes a generating
   function, f :: state -> state | nil, a halting function,
   halt?:: state -> bool, and an intial state s."
  [halt? f s]
  (loop [state s
         nextstate s]
    (if (halt? nextstate)
      state
     (recur  nextstate (f nextstate)))))


(defn serial-comparer
  "Given a sequence of functions [f & rest], where f is a function of 
   two arguments that maps to comparison values: 
      |0   -> x = y 
      |< 0 -> x < y
      |> 0 -> x > y     
   composes a function of two arguments that applies each comparison in turn, 
   terminating early if a non-equal comparison is found."
  [comparers]
  (fn [x y] 
    (loop [cs comparers]
      (if (empty? cs)
          0
          (let [res ((first cs) x y)]
            (if (zero? res)
              (recur (rest cs))
              res))))))              

(defn orient-comparer
  "If direction is descending, wraps f in a function that negates its comparison 
   result."
  [f direction]
  (if (= direction :descending)
      (fn [x y] (* -1  (f x y)))
      f))
  

