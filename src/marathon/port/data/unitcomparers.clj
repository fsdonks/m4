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
  (:require [spork.util   [general :as gen]
                          [tags    :as tag]]
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
(defn get-compare-ctx! [k] (get *comparison-context* k))
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

(defn sort-key-followon [unit] (* (-> unit :currentcycle :bogbudget) 1000))

;;(defcomparer [l r]     )    -> context independent
;;(defcomparer [ctx l r] )    -> context dependent

;;comparers ALL take a context...
;;or comparers ALL take 
;;Uniform progress in a unit's life cycle.
(defn uniform-sort-key [unit]
  (let [c (:currentcycle unit)]
    (/ (:cycletime unit) (:durationexpected c))))

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
        (sequential? x)  :serial 
        (fn?  x)  :fn
        :otherwise (do (println x)
                       (throw (Exception. "Unknown dispatch")))))

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

;;For nested rules, we traverse the rule set and compile them.  Just an 
;;optimization step.

(defn compile-rules 
  "Compiles a possibly nested comparer by walking the rules and evaluating 
   as-comparer in a depth-first fashion." 
  [rule]
  (case (comparison-type rule)
    :serial (vec (map compile-rules rule))
    (as-comparer rule)))

;;allow composite comparer rules.  Given a (possibly nested) sequence of 
;;comparisons, it'll compile the comparers into a single rule.
(defmethod as-comparer :serial  [xs]
  (let [parsed-rules (compile-rules xs)]
    (gen/serial-comparer xs)))
  
;;utility to tag values as key-generators to be used when comparing.
(defn ->key [x]     {:key-fn x})
;;utility to tag values as direct comparison functions that can compare items.
(defn ->compare [x] {:compare-fn x})

;;need a defcomparer....we have something like this in util.table, and util.record.
;;You could do something really cool here, and actually provide a special 
;;scripting language.  Maybe later.
(defmacro defcomparer
  "Defines unit comparison functions.  Creates a sequential comparer out of the 
   key functions provided.  For now, only sequential comparison is supported.
   Optionally, user may supply an argument for a context."
  [name rule]
  `(let [sc# (as-comparer ~rule)]
     (defn ~name  [~'l ~'r] (sc# ~'l ~'r))))

;;#Default Comparers 
;;These are legacy comparison functions that were drawn from the version of 
;;marathon immediately prior to porting.  Note -> by default, these comparers 
;;will return a sorting in ascending order.  We typically frame the notion 
;;of suitability as the "most" suitable, which manifests as a descending order.
;;Since we're in clojure, we can sort (and reverse the order in O(1) time), or 
;;flip the top-level comparison function.  There's a special function above, 
;;__invert__, which does just that.  So a typical idiom is to define a 
;;comparison, or a key function that will be used under assumably ascending 
;;order conditions, then use invert if needed.  There will be cases where a 
;;variety of sorting orders are required, and directionality/preference may 
;;change along dimension.  We can still handle this using function composition,
;;since the values returned by our comparison-builders ultimately yield 
;;functions of the same signature.

;;The following are ported more-or-less straight from the original source. 
;;They will likely be replaced with more general operators, as we extend the 
;;prioritization language to include more expressive operators.

;;Uses uniform-compare to prefer units that have a higher "relative" time in 
;;their expected lifecycle.  For unbounded lifecycles, the progress is defined 
;;relative terms of the maximum machine precision float.
(defcomparer uniform-compare  (->key uniform-sort-key))

;;A comparison that examines units and prefers units with components equal
;;to "AC"
(defcomparer ac-first         (->key sort-key-ac))

;;A comparison that examines units and prefers units with components equal
;;to "RC"
(defcomparer rc-first         (->key sort-key-rc))

;;followon compare only matters if we have a followon code.
;;can we generalize? 
;;pass in some comparison context as an optional third arg.
(defcomparer followon-compare
  (->key #(if-let [followoncode (get-compare-ctx! :followoncode)]
            (sort-key-followon %)  0)))

;;Getting bad vibes from shoveling too much context around...Ah well.
;;Generates a comparable key based on the whether the unit is fenced to a 
;;particular demand.
(defn fenced-key [uic]
  (if (not (context?))  0 ;only matters when context, i.e. tags, demandname, etc. 
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
            :otherwise 0))))                                    

(defcomparer default-compare [fenced-compare followon-compare uniform-compare])

;;Testing 
(comment
;; a dummy cycle 
  (defn ->cycle [durationexpected bogbudget dwell cycletime]
    {:durationexpected durationexpected :bogbudget bogbudget 
     :dwell dwell :cycletime cycletime})      
  (defn ->unit [id compo cycletime cyc] 
    {:currentcycle cyc :id id :cycletime cycletime :component compo})
  (def units [(->unit "ac-dwelling" "AC" 0 (->cycle 1095 365 0 0))
              (->unit "rc-dwelling" "RC" 566 (->cycle 1825 270 566 566))
              (->unit "ac-later" "AC" 1000 (->cycle 1095 365 1000 1000))]) 
  
)  
