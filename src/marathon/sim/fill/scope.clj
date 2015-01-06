;;Functions that illuminate dependencies and infeasible data for 
;;marathon runs based on the topology of fill graphs.  
(ns marathon.sim.fill.scope
  (:require [marathon.sim [core :as core] [demand :as demand] [supply :as supply]]
            [marathon.sim.fill [fillgraph :as fillgraph]]
            [spork.sim    [simcontext :as sim]]
            [spork.cljgraph [core :as graph]]))

(defn get-supplies [g] (graph/source-map g :filled))
(defn get-demands  [g] (graph/sink-map g :unfilled))

;;given a raw fillgraph, returns a map of {:supply ... :demand ...
;;:in-scope ...}
;;where vals assocd to :supply are islands of supply that cannot be
;;used to fill demand, and vals assocd to :demand are elements of
;;supply that cannot be used to fill demand.  We know an element is an
;;island if it has no 
(defn find-islands [g]
  (let [stripped       (graph/drop-nodes g [:filled :unfilled])      
        supplies       (get-supplies g)
        demands        (get-demands g)
        isle-type      (fn [nd] (cond (supplies nd) :supply
                                     (demands nd) :demand
                                     :else (throw (Exception.
                                                   (str "Neither supply nor demand, err. " [nd g])))))]
    (reduce (fn [acc nd] (update-in acc [(isle-type)] conj nd))
            {:demand nil :supply nil} (graph/islands stripped))))

;;Given a fill graph  g, finds the islands (components of size 1 when :filled
;;and :unfilled are disabled)  Returns a map of srcs that are out of
;;scope and the reason, as well as srcs that are in scope.         
;;TODO# remove the source-root and sink-root, these are just 
;;calls to second at the moment.
(defn derive-scope [g]
  (let [islands      (find-islands g)
        out-supply   (get islands :supply)
        out-demand   (get islands :demand)
        in-supply    (reduce dissoc (get-supplies g) out-supply)
        in-demand    (reduce dissoc (get-demands  g) out-demand)]
    {:out-of-scope 
     (as->   (reduce (fn [outofscope isle]
                               (assoc outofscope (fillgraph/source-root isle) "No Demand")) {}
                               out-supply)
           scoped
           (reduce (fn  [outofscope isle]
                     (assoc outofscope (fillgraph/sink-root isle) "No Supply")) scoped
                     out-demand))
     :in-scope ;should be a smarter way to do this.
     (->> 
      (concat  (map (fn [[k v]] [k :supply]) in-supply)
               (map (fn [[k v]] [k :demand]) in-demand))          
      (reduce (fn [scope [label reason]]
                (let [src  (case reason
                             :supply (fillgraph/source-root label)
                             :demand (fillgraph/sink-root label))]
                  (if (contains? scope src) 
                    (assoc scope src :both)
                    (assoc scope src reason))))  {}))
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
