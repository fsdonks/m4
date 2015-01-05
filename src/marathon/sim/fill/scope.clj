;;Functions that illuminate dependencies and infeasible data for 
;;marathon runs based on the topology of fill graphs.  
(ns marathon.fill.scope
  (:require [marathon.sim [core :as core]]
            [spork.sim    [simcontext :as sim]]))

;;Given a fill graph  g, finds the islands (components of size 1 when :filled
;;and :unfilled are disabled)  Returns a map of srcs that are out of
;;scope and the reason, as well as srcs that are in scope.         
(defn derive-scope [g]
  (let [islands      (find-islands g)
        out-supply   (get islands :supply)
        out-demand   (get islands :demand)]
    {:out-of-scope 
     (->as scoped  (reduce (fn [outofscope isle]
                             (assoc outofscope (source-root isle) "No Demand")) {}
                             supply)
           
           (reduce (fn [outofscope isle]
                     (assoc outofscope (sink-root isle) "No Supply")) scoped
                     demand))
     :in-scope
     (reduce-kv (fn [scope label reason]
                  (let [src  (case reason
                               :supply (source-root label)
                               :demand (sink-root label))]
                    (assoc scope src reason))) 
                in-scope (get islands :in-scope))
     :islands islands}))

;;Notify listeners that we found unused supply and removed them from scope.
(defn scoped-supply! [islands ctx]
  (let [recs (:supply islands)]
    (sim/trigger-event :ScopedSupply :Anonymous :Anonymous 
        (core/msg "FillManager found " (count recs) " Unused Supply Sources") recs ctx)))

;;Notify listeners that we found unfillable demand and removed them from scope.
(defn scoped-demand! [islands ctx] 
  (let [recs (:demand islands)]
    (sim/trigger-event :ScopedDemand :Anonymous :Anonymous 
        (core/msg "FillManager found " (count recs) " Unfillable Demand Sinks") recs ctx)))

;;Given  scopeing information, specifically a nested map of srcs that 
;;are in scope or out of scope, applies the implicit scoping rules to
;;the simulation context, removing entities are unnecessary.  
(defn apply-scope    
  ([scopeinfo ctx]
     (core/with-simstate [[demandstore supplystore parameters fillstore] ctx]
       (let [{:keys [in-scope out-of-scope islands]} scopeinfo]
         (->> ctx
              (scoped-demand!  islands)
              (scoped-supply!  islands)
              (core/merge-updates 
               {:demandstore (demand/scope-demand demandstore (:demand islands))
                :supplystore (supply/scope-supply supplystore (:supply islands))
                :parameters  (-> parameters 
                                 (update-in [:SRCs-In-Scope] merge in-scope)
                                 (update-in [:SRCs-Out-Of-Scope] merge out-of-scope))
             ;;temporarily removed, appears vestigial
             ;; :fillstore   (-> fillstore 
             ;;                  (update-in [:outofscope] merge out-of-scope))
                })))))
  ([ctx] (if-let [g (get (core/get-fillstore ctx) :fillgraph)]
           (apply-scope (derive-scope g) ctx)
           (throw (Exception. (str "No fillgraph exists, cannot scope!"))))))

;;Although we have the capacity to divide a very large run into N independent runs,
;;we arent currently doing that.  There is some necessary bookeeping to perform to pull that off,
;;primarily for output metrics stuff, since we have essentially N different system states, with
;;a non-synchronous number of events triggering different sampling rates.
;;It makes a LOT of sense to do this tough, because you get to take advantage of the event stepping
;;even more.  With a large, synchronized system, as N goes up, you spread out your sampling as well,
;;so you end up sampling almost every day.  This is inefficient, especially when there is a lot of
;;change.

;;One possible way to implement this approach is to perform the graph decomposition as intended,
;;developing our N independent sets.  We then add tags to each demand and uic associated with
;;a particular set (should be really easy to derive from the GenericTags actually).
;;As a result, when we iterate through our N independent runs, we just catalog the name of the set
;;were running.  We only affect the tagged supply (constant time lookup) in the unitmap, and the
;;tagged demands (constant time lookup) in the demandmap.  Policy changes are essentially global,
;;so they have to be kept track of during each run (unless we revert the policy to its original
;;setting).

;;The only hiccup is capturing all the output, artificially synchronizing it in accordance with
;;the independent runs.  This CAN be done as a separate measure (via an accumulating observer that
;;keeps statistics up-to-date across all runs.  Basically, we do our n independent runs, then
;;calculate all the sample days.  All we do then is accumulate the sampledays from the n independent
;;runs.  We shouldnt have an egregious amount of runs to do....so it will probably be ok.
;;We have the option of spewing out all of our run data, or just accumulating it in place.
;;Seems like wed want to capture all deployments, this is cumulative/trivial.
;;Also want to capture all summary statistics.
;;This is also cumulative.
;;SandTrends for each SRC, with a cumulative total at the end?
;;This can lead to a large history file....
;;Might want to just target specific SRCs, or tagged groups.
;;Id stick this in a DB and let sql sort it out....
