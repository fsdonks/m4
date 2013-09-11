;;TOM HACK 24 july 2012->
;;Putting a bunch of extra logic in here to handle things...
;;A -> policy matters.
;;  For Future Force Generation....
;;  Units in the Mission pool are highest priority to fill demands relative to their demandgroup.
;;       Depending on the study, Mission Pool units may simply be off limits.
;;       Set this by their demand profile.
;;  Units in the Rotational pool are distributed uniformly, relative to their normalized cycle times.
;;       For each unit;;s cycle, we project it onto a normalized space.
;;           In the case of cycles with infinite length....we just use the dwell time.
;;           In the case of all other policies, we represent the unit;;s policy coordinate as
;;             the percentage completion of its current policy.
;;             This eliminates differences between AC and RC policies, and uniformly distributes
;;             relative readiness, rather than draining on a pool by pool basis.
;;  Units in the Operational and Sustainment Pool
;;       O&S units have a late deployability.  They are evaluated at 3/5 of their actual cycle completion.

(ns marathon.port.data.unitcomparers
  (:require [spork.util [general :as gen]
                        [tags    :as tags]]
            [marathon.sim [fill :as fill]]))

;;All other policies are identical.  In fact, we leave previous policies intact.  
;;So that we can reproduce older study results.

;;Simple and stupid.  For now, we just have a context that can be bound.
;;Some comparison functions want a context, or extra stuff, so if they do, 
;;the caller has to provide a binding, or use a with-comparison-context 
;;macro.
(def ^:dynamic *comparison-context*)

;;Interface to allow comparison functions to uniformly fetch stuff from the 
;;comparison context aether...(the dynamic binding).
(defn get-compare-ctx [k] (get *comparison-context* k))
(defn context? [] (empty? *comparison-context*))

;;Helper functions for common comparisons.
(defn unit-dwell [unit]   (-> (:currentcycle unit) :dwell)) 
;;obe
(defn sort-key-ac [unit] 
  (case (:component unit)
     "AC" (unit-dwell unit)
     (/ (unit-dwell unit) 3.0)))

(defn invert [f] (fn [l r] (f r l))) 

;;Needs to be generalized.  It's a partial application of a bias function.
;;this is really a bias toward "non-AC" units.
(defn sort-key-rc [unit]
  (if (not= (:component unit) "AC")
    (unit-dwell unit)
    (/ (unit-dwell unit) 3.0)))

(defn sort-key-followon [unit] (* (-> unit (:currentcycle :bogbudget)) 1000))

(defn sequential-comparer
  "Threads a comparison context through a series of comparisons, xs, in a 
   serial, fail-first fashion. xs are funcs that map [l r] -> comparison.
   Some functions have read access to the comparison context, using 
   get-comparison-context, and can have dynamic behavior if a context is 
   provided."
  ([ctx xs]
    (binding [*comparison-context* ctx]
      (fn [l r]       
        (loop [remaining xs
               acc 0]
          (if-let [f (first remaining)]
            (let  [res (f ctx l r)]
              (if  (not= res 0) res
                (recur (rest xs) 
                       acc)))
            acc)))))
  ([xs] (sequential-comparer {} xs))) 

;;(defcomparer [l r]     )    -> context independent
;;(defcomparer [ctx l r] )    -> context dependent

;;comparers ALL take a context...
;;or comparers ALL take 
;;Uniform progress in a unit's life cycle.
(defn uniform-sort-key [unit]
  (let [c (:currentcycle unit)]
    (/ (cycletime unit) (:durationexpected c))))

;;figure out how to merge this later. 
(defn opsus-sort-key [unit] (* (uniform-sort-key unit) (/ 3.0 5.0)))

;;parsing comparison rules...
;;at the end of the day, we want a simple sequence of comparisons to combine.
;;not unlike parser combinators, or other little grammars.

;;a comparison rule can be:
;;|f:: a -> a -> comparison
;;|{:key-fn f} :: map (a, (a->b))
;;|{:compare-fn f}     :: map (a, (a->a->comparison))
;;So we use comparison-type as a dispatch function for a multimethod.
;;We'll read in "specs" to parse a comparer, and use this build our comparers 
;;up from specs (functions, or maps of {:key-fn f}|{:compare-fn f}

(defn comparison-type [x] 
  (cond (map? x) (cond (contains? x :key-fn)      :key-fn 
                       (contains? x :compare-fn)  :compare-fn)
        (seq? x) :serial 
        (fn?  x)  :fn))

;;utility function to convert maps of {:function|:key v} into functions that 
;;can be used to compare two values.
(defmulti  as-comparer comparison-type) 
;;return the comparison function directly.
(defmethod as-comparer :fn     [f] f)
;;create a comparer that uses the key-function  
(defmethod as-comparer :key-fn [m] 
  (let [keyf (get m :key-fn)] 
    (fn [l r] (compare (keyf l) (keyf r)))))
;;unpack the comparer 
(defmethod as-comparer :compare-fn [m] (get m :compare-fn))

;;allow composite comparer rules.  Given a (possibly nested) sequence of 
;;comparisons, it'll compile the comparers into a single rule.
(defmethod as-comparer :serial     [xs]
  (let [parsed-rules (vec (map as-comparer xs))] ;not complete.  Need more.
    (fn [l r] (loop [fs  parsed-rules
                     acc 0]
                (if (or (empty? fs) (not= acc 0)) acc
                    (let [f  (first fs)]
                      (recur (rest fs) (f l r))))))))
;;utility to tag values as key-generators to be used when comparing.
(defn ->key [x]     {:key-fn x})
;;utility to tag values as direct comparison functions that can compare items.
(defn ->compare [x] {:compare-fn x})

;;need a defcomparer....we have something like this in util.table, and util.record.
(defmacro defcomparer
  "Defines unit comparison functions.  Creates a sequential comparer out of the 
   key functions provided.  For now, only sequential comparison is supported.
   Optionally, user may supply an argument for a context."
  [name key-funcs]
    (let [cs (if (coll? key-funcs) key-funcs [key-funcs])]
      `(let [sc# (~'gen/serial-comparer ~cs)]
         (~'defn ~'name  [~'l ~'r] (sc# l r)))))

;;Uses uniform-compare 
(defcomparer uniform-compare  (->key uniform-sort-key))
(defcomparer ac-first         (->key sort-key-ac))
(defcomparer rc-first         (->key sort-key-rc))

;;followon compare only matters if we have a followon code.
;;can we generalize? 
;;pass in some comparison context as an optional third arg.
(defcomparer followon-compare
  (->key #(if-let [followoncode (get-compare-ctx :followoncode)]
            (sort-key-followon %)  0)))

;;Getting bad vibes from shoveling too much context around...Ah well.
;;Generates a comparable key based on the whether the unit is fenced to a 
;;particular demand.
(defn fenced-key [uic]
  (if (not (context?))  0 ;only matters when we have context, i.e. tags, demandname, etc. 
    (let [{:keys [demandname followoncode tags]} *comparison-context*]
      (fill/inside-fence? uic))))

;;A comparer that uses fencing information, supplied by the dynamic binding to 
;;the comparison context, to prefer units that are fenced to a relative demand.
(defcomparer fenced-compare 
  (fn [l r] 
    (let [fenced-left?  (fenced-key l)
          fenced-right? (fenced-key r)]
      (cond (and fenced-left? (not fenced-right?)) 1 
        (and (not fenced-left?) fenced-right?) -1
        0))))                                    

(defcomparer default-compare [fenced-compare followon-compare uniform-compare])




 190:   res = FencedCompare(u1, u2)
 191:   If res = equal Then res = followOnCompare(u1, u2)
 192:   If res = equal Then res = uniformCompare(u1, u2)

 