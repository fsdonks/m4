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

;;need a defcomparer....we have something like this in util.table, and util.record.
(defmacro defcomparer
  "Defines unit comparison functions.  Creates a sequential comparer out of the 
   key functions provided.  For now, only sequential comparison is supported.
   Optionally, user may supply an argument for a context."
  ([name key-funcs]
    (let [cs (if (coll? key-funcs) key-funcs [key-funcs])]
      `(let [sc# (~'gen/serial-comparer ~cs)]
         (~'defn ~'name  [~'l ~'r] (sc# l r)))))
  ([name args key-funcs]
    (let [cs (if (coll? key-funcs) key-funcs [key-funcs])]
      `(let [sc# (~'gen/serial-comparer ~cs)]
         (~'defn ~'name  [~'l ~'r] (sc# l r))))))

;;Uses uniform-compare 
(defcomparer uniform-compare   [{:key uniform-sort-key}])
(defcomparer ac-first          [{:key sort-key-ac}])
(defcomparer rc-first          [{:key sort-key-rc}])
;;followon compare only matters if we have a followon code.
;;can we generalize? 
;;pass in some comparison context as an optional third arg.
(defcomparer followon-compare
  [{:key #(if-let [followoncode (get-compare-ctx :followoncode)]
                  (sort-key-followon %)  0)}])

 167:   Private Function FencedCompare(u1 As TimeStep_UnitData, u2 As TimeStep_UnitData) As Comparison
 168:   Dim l As Boolean, r As Boolean
 169:   l = MarathonOpFill.isInsideFence(u1, demandname, followoncode, tags)
 170:   r = MarathonOpFill.isInsideFence(u2, demandname, followoncode, tags)
 171:   If l And r Then
 172:       FencedCompare = equal
 173:   Else
 174:       If l And (Not r) Then
 175:           FencedCompare = greaterthan
 176:       ElseIf (Not l) And r Then
 177:           FencedCompare = lessthan
 178:       End If
 179:   End If
 180:  
 181:   End Function

 189:  'TOM Change 27 SEp 2012 -> allow fencing of supply via tags...We pass information to the comparer
 190:  'if the unit is fenced to the relative demand or demand group.  If a unit is fenced to a different
 191:  'demand or group, we return false.

;;Getting bad vibes from shoveling too much context around...Ah well.
(defn fenced-key [uic]
  (if (not (context?))  0 ;only matters when we have context, i.e. tags, demandname, etc. 
    (let [{:keys [demandname followoncode tags]} *comparison-context*]
      (fill/inside-fence? uic))))

(defcomparer fenced-compare 
  {:comparer (fn [l r]   )))})                                    

(defn invert [f] (fn [l r] (f r l))) 

(defcomparer default-compare [fenced-compare followon-compare uniform-compare])



 

                

 190:   res = FencedCompare(u1, u2)
 191:   If res = equal Then res = followOnCompare(u1, u2)
 192:   If res = equal Then res = uniformCompare(u1, u2)

 