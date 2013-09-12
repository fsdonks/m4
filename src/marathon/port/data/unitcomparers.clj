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
  (:require [spork.util   [tags       :as tag]]
            [spork.util.comparison :refer :all]
            [marathon.sim [fill :as fill]]))

;;Some comparers require context to make comparisons; rather than thread it 
;;through as an explicit argument to each comparer, I'd 

;;Simple and stupid.  For now, we just have a context that can be bound.
;;Some comparison functions want a context, or extra stuff, so if they do, 
;;the caller has to provide a binding, or use a with-comparison-context 
;;macro.
(def ^:dynamic *comparison-context*)

;;Interface to allow comparison functions to uniformly fetch stuff from the 
;;comparison context aether...(the dynamic binding).
(defn get-in-compare-ctx! [k] (get *comparison-context* k))
(defn context? []             (empty? *comparison-context*))

;;All other policies are identical.  In fact, we leave previous policies intact.  
;;So that we can reproduce older study results.

;;Helper functions for common comparisons.
(defn unit-dwell [unit] (-> (:currentcycle unit) :dwell)) 
;;obe
(defn sort-key-ac [unit] 
  (case (:component unit)
     "AC" (unit-dwell unit)
     (/ (unit-dwell unit) 3.0)))

;;Needs to be generalized.  It's a partial application of a bias function.
;;this is really a bias toward "non-AC" units.
(defn sort-key-rc [unit]
  (if (not= (:component unit) "AC")
    (unit-dwell unit)
    (/ (unit-dwell unit) 3.0)))

(defn sort-key-followon [unit] (* (-> unit :currentcycle :bogbudget) 1000))

;;Uniform progress in a unit's life cycle.
(defn uniform-sort-key [unit]
  (let [c (:currentcycle unit)]
    (/ (:cycletime unit) (:durationexpected c))))

;;figure out how to merge this later. 
(defn opsus-sort-key [unit] (* (uniform-sort-key unit) (/ 3.0 5.0)))

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
(defcomparer ac-first         (->where :component "AC"))

;;A comparison that examines units and prefers units with components equal
;;to "RC"
(defcomparer rc-first         (->where :component "RC"))

(defn can-follow? [x] 
  (when-let [followoncode (get-in-compare-ctx! :followoncode)]
    (= followoncode x))) 

;;followon compare only matters if we have a followon code.
;;can we generalize? 
;;pass in some comparison context as an optional third arg.
(defcomparer followon-compare (->key can-follow?))

;;Getting bad vibes from shoveling too much context around...Ah well.
;;Generates a comparable key based on the whether the unit is fenced to a 
;;particular demand.
(defn fenced-key [uic]
  (when-let [ctx *comparison-context*] ;only matters when context, i.e. tags, demandname, etc. 
    (let [{:keys [demandname followoncode tags]} ctx]
      (fill/inside-fence? uic demandname followoncode tags))))

;;A comparer that uses fencing information, supplied by the dynamic binding to 
;;the comparison context, to prefer units that are fenced to a relative demand.
(defcomparer fenced-compare 
  (fn [l r] 
    (let [fenced-left?  (fenced-key l)
          fenced-right? (fenced-key r)]
      (and fenced-left? (not fenced-right?))))) 
                                                
(defcomparer default-compare [fenced-compare followon-compare uniform-compare])

(defn order-units
  [xs & {:keys [ctx comparer] :or {ctx nil comparer default-compare}}]
  (binding [*comparison-context* ctx]
    (sort comparer xs)))   

;;Testing 
(comment
;; a dummy cycle 
  (defn ->cycle [durationexpected bogbudget dwell cycletime]
    {:durationexpected durationexpected :bogbudget bogbudget 
     :dwell dwell :cycletime cycletime})      
  
  (defn ->unit [id compo cycletime cyc] 
    {:currentcycle cyc :id id :cycletime cycletime :component compo})
  
  (def units [(->unit "ac-dwelling" "AC" 0      (->cycle 1095 365 0 0))
              (->unit "rc-dwelling" "RC" 566    (->cycle 1825 270 566 566))
              (->unit "ac-later"    "AC" 1000   (->cycle 1095 365 1000 1000))
              (->unit "ng-dwelling" "NG" 3      (->cycle 1825 270 3 3))]) 

  
)  
