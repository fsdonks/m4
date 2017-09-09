;;Requirements Analysis implementation.
(ns marathon.analysis.requirements
  (:require [spork.util [record :as r] [table :as tbl] [io :as io] [temporal :as temporal]]
            [spork.sim [simcontext :as sim]]
            [spork.entitysystem [store :as store]]
            [clojure [pprint :as pprint]]
            [marathon.ces [core :as core]
                          [engine :as engine]
                          [setup :as setup]
                          [demand :as demand]]
            [marathon [analysis :as a] [observers :as obs]]
            [clojure.core [async :as async]]))

;;Utility functions
;;=================
(defn hpath [p]
  (str io/home-path p))

(defmacro get-or [m k & else]
  `(if-let [res# (get ~m ~k)]
     res#
     ~@else))

(defn procs [] (.. Runtime getRuntime  availableProcessors))

;;this isn't a huge deal; multiplying by ratios returns bigints...
(defn distribute-rationally [n xs] (mapv (fn [r] (* n r)) xs))
(defn sums-to-one?   [xs] (== 1.0  (double (reduce + xs))))
;;useful utility function...
(defn sum-by [f init xs] (transduce (map f) (completing +) init xs))

;;useful function for spork.util.table todo: move there...
(defn apply-schema
  "Given schema s and table t, attempts to coerce the table entries into 
   the appropriate data format for all fields defined in s.  Coerces 
   field entries to strings prior to conversion, so there may be a 
   penalty.  Typically most useful working with string-ified data, 
   such as a literal table.  Fieldnames must match exactly, i.e.  
   string fieldnames will not match keyword schema names."
  [s t]
  (let [parse-field (fn [fld col]
                      (if-let [f (spork.util.parsing/parse-defaults
                                    (get-or s fld))]
                        [fld (mapv (comp f str) col)]
                        [fld col]))]
    (spork.util.table/order-fields-by
     (spork.util.table/table-fields t)
     (->  (map parse-field
               (spork.util.table/table-fields  t)
               (spork.util.table/table-columns t))           
          (spork.util.table/conj-fields spork.util.table/empty-table)))))

(defn set-field
  "Replace the values associated with field k in t, with 0 values."
  [t k v]
  (let [n (tbl/count-rows t)]
    (tbl/conj-field  [k (vec (repeat n v))] t)))

(defn zero-supply [t] (set-field t :Quantity 0))
(defn increment-key [r k n] (update r k (fn [q] (+ q n))))

;;use tbl/subtables-by to get this implemented.
;;Useful for splitting up requirements states...
;; (defn split-by-field
;;   "Given a field to split on, creates n new databases, 
;;    where each database contains tables (with the field) 
;;    that have identical values for the field."
;;   [tbls & {:keys [field tables]}]
;;   (let [[base variable]

;;Profiling for informing lower bounds and hueristics...
;;======================================================
(defn daily-totals [actives]
  (reduce (fn daily [acc {:keys [SRC Quantity]}]
            (update acc SRC #(+ (or % 0) Quantity)))
          {} actives))
(defn merge-by-max [l r]
  (reduce-kv (fn total [totals src q]
               (let [qprev (get totals src 0)]
                 (if-not (== q qprev)
                   (assoc totals src (max q qprev))
                   totals)))
             l r))

(defn demands->src-peaks
  "Given a sequence of DemandRecords, recs, computes a map of 
   the peak concurrent demand for each SRC, keyed by {SRC Peak}.
   This is intended to be used as a lower-bound for requirements 
   analysis."
  [recs]
  (->> (-> (filter  :Enabled recs)      
           (temporal/activity-profile  :start-func :StartDay))
       (reduce (fn [totals [t {:keys [actives]}]]
                 (let [tm (daily-totals actives)
                       _ (assert (every? number? (vals tm)) (str ["Should be numeric!" [t tm]]))]
                   (merge-by-max totals tm))) {})))

;;Question: Are there populations that are "duplicate", i.e. they
;;have the same aggregate distribution and demand records?
;;If so....we should be able to reduce the required number of runs...
;;  If we break down the demands by src, and have a way to get a
;;  canonical hash of the demand signal by [start,duration, quantity]
;;  We have the possibility tha multiple demands are identical...

;;Data munging and records
;;========================

;;If we use records, we get ordered fields automatically.  Better strategy.
;;generic supply record.  Should probably tie these to marathon.schemas
(defrecord srecord [Type Enabled Quantity SRC Component OITitle
                    Name Behavior CycleTime Policy Tags Spawntime Location Position Original])
;;outstreaam/mystream is output....canonically RequirementsGeneratedSupply.csv
;;may not use this guy...we'll see.
(r/defrecord+ outrecord [[Iteration 0]
                         [SRC ""]
                         [Component ""]
                         [Quantity   0]])

;;generate a new supplyrecord.
(defn ->supply-record
  ([src component count]
   (->srecord "SupplyRecord" true count src component (str "Generated_"  src)
              "Auto" "Auto" 0 "Auto" "Auto" 0 "Auto" "Auto" false))
  ([src component] (->srecord src component 0)))


;;Distributors
;;============
;;distributors need to be implemented....
;;default is binned, provides deterministic
;;round-robin growth.
;;Note: We can also have deterministic growth
;;using a seeded prng...

;;kind of lame at the moment
;;should return a map of [compo val]
(defprotocol IDistributor
  (distribute- [obj n]))

;;NOTE: This is a TEMPORARY HACK to get
;;around the breaking change in clojure 1.8,
;;extends? no longer works as God intended.
;;Prefer satisfies? with memoization for now.
;;inst? is coming in 1.9.
(def inst?
  (memoize (fn [protocol x]
             (satisfies? protocol x))))

(def bad (atom nil))

(defn distribute-by [f n]
  (let [tf f #_(type f)]
    (cond (#_extends? inst? IDistributor tf)
            (distribute- f n)
          (#_extends? inst? clojure.core.protocols/IKVReduce tf)
            (reduce-kv (fn [acc k prop]
                         (assoc acc k (* prop n))) {} f)
          (fn? f) (f n)
          :else
          (do (reset! bad f)
              (throw (Exception. (str "unknown distributor!" f)))))))

;;Requirements State
;;==================
;;creates a "lightweight" context
;;for requirements analysis, so we can
;;go faster.  Basically, drop any observers that
;;aren't useful.
;;For now, we don't do anything special, although
;;ripping out some of the observer stats could be useful.
;;  We don't need the capacity-analysis defaults for now.
;;  I'll look into ripping them out in the future,
;;  since they add some overhead.  But for requirements,
;;  we only care about demand fill.  Anything else is
;;  incidental.
(defn requirements-ctx [tbls & {:keys [observer-routes]
                                :or   {observer-routes obs/default-routes}}]
  ;;we'll basically do the same thing we normally do.
  ;;For now at least...
  ;;use init-context here, but not setting up observers.  
  #_(->>  (setup/simstate-from ;;allows us to pass maps in, hackey
           tbls
           core/emptysim)  
          (sim/add-time 1)
                                        ;(sim/register-routes obs/default-routes)
          )
  (a/load-context tbls :init-ctx core/emptysim :observer-routes {}))

;;Note: we need a higher-order function that wraps
;;performing RA for multiple srcs...
;;I think the chain of causality is...
;;Given an SRC, some tables,
;;  Filter the supply,demand,policy records...
;;  Compute requirements (however, typically iterative convergence)
;;    ;;Create a simcontext from filtered tables.
;;This gives us the mapping of src->requirements
;;really, src->supply-records

;;if we have an initial supply, we have a floor on the
;;supply estimates.  RA will not go below this
;;amount.  We want to compute a supply-table
;;that reflects the initial supply.
;;If the supply-table is empty, then
;;we need to build one.
(defn initial-supply [src supply-table compo-distros
                      & {:keys [zero?] :or {zero? true}}]
  (let [growth-compos (set (for [[k v] compo-distros :when (pos? v)]  k))
        known         (set (tbl/field-vals
                            (tbl/get-field :Component supply-table)))
        new-compos    (clojure.set/difference growth-compos known)
        new-records   (for [compo new-compos
                            :when (pos? (compo-distros compo))]
                        (->supply-record src compo 0))]
    (do (println [:computing-initial-supply])    
        (concat (map (fn [r] (if-not (:Enabled r)
                               (assoc r :Quantity 0 :Enabled true) r)) (tbl/table-records supply-table))
                new-records))))

(defn zero-supply
  "Simple function to zero our supply records.  Replaces 
   quantity (if any) with a value of 0."
  [xs]
  (mapv (fn [r] (assoc r :Quantity 0)) xs))


;;we'll explicitly pass in SRC as a filter for now.
;;It'd be really nice to share the context once we've
;;created it....although policies and stuff might change...
;;there's a bit of overhead in the policy stuff, so it's
;;easier to move from ctx->ctx than otherwise...
;;For now, we'll just suck it up. And redo every time,
;;see how expensive it ends up being.

(defn ->requirements-state
  "Given a map of tables (a marathon project), 
   creates a map that contains all of the state 
   we'll need for our requirements analysis."
  [tables src compo-distros]
  (let [;ctx0 (requirements-ctx tables)
        s    (initial-supply  src (:SupplyRecords tables) compo-distros)
        mins (into {} (for [[c [r]] (group-by :Component s)]
                                [c (:Quantity r)]))]
    {;:ctx    ctx0 ;initial simulation context.
     :tables tables
     :supply s    ;initial seq of supply records.
     :minimum-supply mins
     :src src
     :distributions compo-distros
     :steps     []
     :iteration 0}))

(defn has-demand? [src dtbl]
  (->> dtbl
       (tbl/table->flyrecords)
       (filter :Enabled)
       (some #(= (:SRC %) src))))

;;interesting notes on machine precision and clojure's numeric tower.
;;(def rats '(1696969696969697/4000000000000000 0N 5757575757575757/10000000000000000))
;;(reduce + rats) => 19999999999999999/20000000000000000 
;;(/ 19999999999999999.0 20000000000000000.0) => 1.0
;;(long   (double (reduce + rats))) => 1
;;(double (reduce + rats)) => 1.0
;;(long   (reduce + rats)) => 0
;;(bigint (reduce + rats)) => 0N

;;Basic Algorithm
;;===============
;;Requirements analysis is the process of calculating some required additional
;;supply in the face of a given demand signal.  Technically, requirements
;;analysis is indifferent to the existence of supply.  We can calculate a
;;requirement regardless of pre-existing supply.  In fact, we can grow a
;;requirement.

;;Traditional requirements analysis consisted of a fixed-point function:
;;- RequiredSupply(SupplyInitial) = SupplyInitial + Generated(SupplyInitial, Demand)
;;  - Do until SupplyNext = SupplyInitial
;;    - SupplyInitial <- SupplyNext
;;    - SupplyNext <- Distribute(RequiredSupply(SupplyInitial), SupplyInitial)

;;Estimating Bounds
;;=================
;;We will use DemandAnalysis and Supply Analysis to help the convergence go
;;much faster. As a pre-process step, we utilize Static Demand Analysis to
;;determine the peak demands and the maximum deployment acceleration
;;experienced during the run.
;;- This provides definite constraints on our minimum required supply.
;;  - ANY supply must have a theoretical capacity >= the peak demand.

;;We then utilize Static Supply Analysis to determine the theoretical
;;rotational capacity for the initial supply.  (If we have no supply,
;;capacity is nil). Prior to starting the convergence algorithm, we
;;distribute this intial gap in supply.

;;Then run until convergence.
    ;;Should take less time.

;;Note -> Requirements analysis does not utilize substitutions.
;;- This is a pure 1:1 demand to supply.
;;- This makes the problem essentially much easier (and faster).

;;It also makes calculating requirements even faster and more effecient,
;;because we can parallelize the entire analysis into a set of n
;;independent simulations, which only considers the supply and demand of
;;a single type.

;;Changes from VBA
;;What we'll do instead of having file i/o driving this, is we'll
;;derive any necessary i/o from the sequence we compute as we converge.
;;Basically, using reductions, we'll derive the supply table each iteration.

;;oh, we need to be able to generate supply...
;;Well, we can delegate that to RA....
;;Instead of creating "supply generators"
;;that are complicated, we can add additional
;;supply after-the-fact, and then fill again with
;;the next context.  So, basically, add-ghosts if
;;there are missed demands.

;;Growing Supply
;;==============
;;note:
;;the act of "creating supply" from a supply record
;;transforms a serialized form into a sequence of instructions
;;that create entities and schedule initial behavior, like spawning.
;;So, one way to view it is that the record maps to a sequence of
;;instructions, which yield entities.

;;We're basically altering the initial supply via supply records....
;;The reason we do so is because the system is setup to initialize
;;the context from raw tables, so changing the data creates
;;new sim contexts automagically.

;;As we look at how to vary supply programmatically, rather than
;;"just" varying the table, we could look into varying the
;;context directly....

;;reads distributions from a table of [src ac rc ng]
;;probably a better way to do this...legacy implementation
;;We can also provide a way to compute this empirically right?
(defn compute-aggregate-supply
  "Given a table of supply-records, computes a sequence of 
   records by src, compo, summing by the Quantity field."
  [xs]
  (let [quantities (fn quantities [xs]
                     (sum-by :Quantity 0 xs))]
    (->> (for [[src src-groups] (group-by :SRC       xs)]
           (let [compo-xs       (group-by :Component src-groups)]
             {:Type "GhostProportionsAggregateRecord"
              :Enabled true
              :SRC src
              :AC (quantities  (get compo-xs "AC"))
              :NG (quantities  (get compo-xs "NG"))
              :RC (quantities  (get compo-xs "RC"))})))))

;;note: this currently bombs if sum is zero...
;;Now, we filter out zero-summed records.  That means
;;if the components aren't recognized (ala Ghost) then
;;we exclude said records.  We won't compute a requirement
;;for them, which will show up in post processing.
(defn aggregate-proportions
  "Computes proportional values for each record in xs, by component, 
   where the associated component value is a proportion of the sum of 
   component values.  Retains rational precision by default."
  [xs & {:keys [float?]}]
  (let [divide (if float? (fn [l r] (double (/ l r)))
               / )]
    (into [] (comp (map (fn [{:keys [AC NG RC] :as r}]
                          (let [sum (+ AC NG RC)]
                            (when (pos? sum) 
                              (merge r {:AC (divide AC sum)
                                        :NG (divide NG sum)
                                        :RC (divide RC sum)})))))
                   (filter identity))
        xs)))

(defn aggregate-distributions
  "Given a map of tables, computes the aggregate supply proportions  
   from the input.  User may specify alternate tables to use for the 
   proportions.  If no table is found, will compute the proportions 
   from the input supply."
  [tbls & {:keys [distribution-table dtype float?]
           :or   {distribution-table :GhostProportionsAggregate
                  dtype :bin}}]
  (let [make-distributor (fn [n] (fn [k] (* k n)))] 
    (->> (get-or tbls  distribution-table
                 (->> (:SupplyRecords tbls)                                           
                      (tbl/table-records)
                      (filter :Enabled)
                      (compute-aggregate-supply)
                      (aggregate-proportions)))
         (transduce (filter :Enabled)
                    (completing
                     (fn [acc r]
                       (let [{:keys [SRC AC RC NG]} r]
                         (assoc acc SRC {"AC" AC "RC" RC "NG" NG}))))
                                {}))))

;;fill this in...
;;probably need some state.
;;TOM Change 19 April 2012
;;For an src, use our local distributor to compute how x ghosts should be converted into
;;[X,Y,Z..] units of supply, by component
;;Note: renamed 'ns to 'steps.  Embeding stuff in a requirements state
;;map.
;;we have the supply records in reqstate/supply
;;goal is to update the quantities incrementally.

;;this creates a map of {compo amount}
(defn compute-amounts [reqstate src n]
  (distribute-by (:distributions reqstate) n))

;;Replacement method for an earlier hack.  We now separate the process of calculating and applying
;;distributions.  Given a set of distributions, by component, apply them (whatever that means)
;;to the src.  Our goal is to update the the supply-table.
;;Note: it doesn't actually matter if they're a spork.util.table
;;or records...We can just keep the supply records
;;as a record seq now, don't have to stick with
;;table...
(defn apply-amounts
  "Given a requirementstate ,reqstate, and 
   a map of {component amount}, increments the 
   supply records in reqstate where compo matches, 
   according to (+ (minimum-supply component) amount)"
  [reqstate compo-amounts]
  (let [mins (:minimum-supply reqstate)]
    (update reqstate :supply          
            #(->> %
                  (map (fn [r]
                         (if-let [n (get compo-amounts (:Component r))]
                           (assoc r :Quantity (+ n (get mins (:Component r) 0)))
                           r)                
                ))))))

;;note: strange incidence of getting 50 instead of 25 for total on an
;;initial bisection step...
;;I think we're misusing :total-ghosts, not sure we even need it.
;;So, each time we add supply, we conceptually take a growth step.
(defn distribute [reqstate src n]
  (let [steps   (or (:steps reqstate) [])
        total   n #_(if (empty? steps) n
                    (:total #_:total-ghosts (last steps)))
        amounts (compute-amounts reqstate src total)]
    (-> reqstate
        (apply-amounts  amounts) 
        (assoc  :steps ;;record the step we took.
           (conj steps {:src    src
                        :count  amounts
                        :total-ghosts (+ total n)
                        :added  amounts
                        :total  total ;n
                        })))))

;;This is an auxillary function to handle each run of the requirements analysis.
;;Given a simulation that is already primed and loaded, and possibly an initial supply of units, calculate the
;;units needed (the requirement), as represented by the amount of ghosts created by SRC.  The requirement is then
;;applied to the distributor, which transforms the homogeneous supply of ghosts into a set of units that are to be added
;;to the final result. The return is a dictionary of (SRC|Component, count) pairs.  This allows us to trivially update
;;the supplytable, by incrementing.

;;The idea here is to just layer on another fill step,
;;if there are unmet demands, we allow ghosts to be created and
;;used to fill.  This is different than the scheme in vba, where
;;we had "supply generators" that were a little squirrely.  We'll
;;just move to a multipass fill, and use a ghost-fill function as
;;the last stage.  Easy peasy. [IF WE WANT TO USE GHOSTS].

;;[No Ghosts]
;;There's actually a simpler way to do this.  If we ignore ghosts,
;;we don't need to generate ghost entities at all, just stop on the
;;first day we miss demand(s).  Then use the quantities missed as
;;a hueristic to generate ghosts.
(defn unconstrained-ghost-step 
  "Primary state transition function for Marathon Requirements Analysis. "
  [ctx]
  ;;At this point
  (throw (Exception. (str "placeholder for stepping with ghost-fills."))))

(defn unfilled-demand
  "Computes a scalar quantity of unfilled demand from a simulation
   context."
  [ctx]
  (->> ctx
       (demand/unfilled-demand-count) 
       (map :unfilled)
       (reduce +)))

;;probably want to stick this in marathon.analysis...
;;Given a history, compute the maximum amount of ghosts
;;(high-water mark) over time.  We should be able to
;;determine this easily by selecting entities with a
;;"Ghost" component at the end of the simulation.
(defn history->ghosts [h]
  ;;The crude idea here is to traverse the history until
  ;;we find the first time we actually miss demand.
  ;;We compute total misses on said day, and report the
  ;;number.  Simple.
  (->> h
       (map (comp unfilled-demand second))
       (filter identity)
       (filter pos?)
       (first)))

(defn history->indexed-ghosts [h]
  ;;The crude idea here is to traverse the history until
  ;;we find the first time we actually miss demand.
  ;;We compute total misses on said day, and report the
  ;;number.  Simple.
  (->> h
       (map (fn [[t ctx]]  (when-let [xs (unfilled-demand ctx)]
                             (when (pos? xs)
                               [t xs]))))
       (filter identity)
       ;(filter pos?)
       (first)))

;;I think we'll prefer to work with the history, so probably using a marathon-stream
;;instead of this approach...
;;TODO: Replace event-step-marathon with the appropriate simreducer or whatnot.
;;Returns the next requirement state, if we actually have a requirement.
;;Otherwise nil.
;;I think we can just use analysis/load-context...
(defn calculate-requirement
  "Computes requirements from an initial set of tables.  Workhorse function for 
   requirements analysis."
  ([reqstate distance-function]
   (calculate-requirement reqstate distance-function requirements-ctx))
  ([{:keys [tables src steps supply] :as reqstate} distance-function tables->ctx]
   (-> (update reqstate :tables assoc :SupplyRecords supply)
       (tables->ctx)
       (distance-function))))

;;calculate-requirement works on one requirement...
;;to perform a requirements analysis, we want to 
(def default-distance (comp history->ghosts a/marathon-stream))
(def indexed-distance (comp history->indexed-ghosts a/marathon-stream))

(def prior (atom nil))

;;So, this is actually pretty good for smallish requirements..
;;Except some structure has ginormous requirements....
;;There may be a hueristic we can use to switch between
;;depth-first and bisection.

;;We have degenerate cases where the preponderance of the
;;growth occurs in compos with high rotational discounts...
;;so we're really ineffecient.  Combine that with
;;growth-steps of 1....and we end up with tons of iterations.

;;Can we pre-process the demand to get a better estimate?
;;That is, determine an initial seed - and pre-distribute
;;up to said seed - to get us closer to the goal to
;;begin with?

;;Lower-bound analysis indicates that the peak concurrent
;;demand is - assuming perfect usage of supply, i.e.
;;no rotational policy in place, we need at least
;;enough supply - at a given point in time -
;;to accmodate the peak demand.

;;We may be able to fold this into calculate-requirements...
(defn iterative-convergence
  "Given a requirements-state, searches the force structure 
   space by varying the supply of the requirements, until 
   it converges on a minimum feasible force structure.
   At the low end, we'll just be performing multiple 
   capacity analyses..."
  [reqstate & {:keys [distance]
               :or   {distance default-distance}}]
  (let [echo (fn [{:keys [src iteration] :as reqs} dist]
               (do (println
                    (pprint/cl-format nil "Generated ~a ghosts of ~a on iteration ~a"
                                      dist src iteration))
                   reqs))]
  (loop [reqs      reqstate]
    (if-let [dist (calculate-requirement reqs distance)] ;;naive growth.
      (-> reqs
            (echo dist)
            (distribute (:src reqstate) dist)
            (update :iteration inc)            
            (recur))
      reqs))))


;;We can save a lot of redundant effort if
;;we limit ourselves to only loading supply...
;;I.e., we keep policy and demand (initial demand)
;;in place.

;;We can call this the root context.
;;The root context then only has to build units
;;from records....so...

;;If we want to reset a requirements context....
;;We need to drop the supply.
;;Demand doesn't change.
;;We could reset the demand....

;;To create a root context...
;;Build from a file.
;;From wipe out the supply.
;;Wiping supply implies

(defn clear-supply
  "Given a context, removes a unit entity from the context."
  [ctx]
  (let [us     (core/units ctx)
        ids    (map :name   us)
        pstore (reduce (fn [acc u]
                      (marathon.ces.policy/unsubscribe-unit u (:policy u) acc))
                   (core/get-policystore ctx)
                   us)
        _ (println (:subscriptions pstore))]
    (-> (->> (marathon.ces.supply/drop-units ctx ids)
             (sim/merge-entity {:PolicyStore pstore}))
        (sim/drop-entity-updates (set ids)))))

;;We could use a heuristic function, for the big
;;entity runs.  Alternately, make supply updates
;;and unit construction/registration much cheaper.

(defn quick-context
  "Yields a function that provides a reusable context 
   so that we don't pay i/o costs everytime we build a 
   new supply excursion.  Strips down the initial context
   into a simplified context that has no unit-entities or 
   supply."
  [tbls]
  (let [base-ctx (requirements-ctx tbls)     
        base-ctx (clear-supply base-ctx)]    
    (fn [tbls]
      (-> base-ctx
          (setup/default-supply :records (:SupplyRecords tbls))))))
;;note: we need to account for pre-existin supply.
;;We want to define proportional growth as a function of the initial
;;supply...that is...
;;if we have [ac0 ng0 rc0] that's our initial-supply.
;;we grow proportionally from there (there's a fixed amount to begin with).

(defn find-bounds
  "Bounding hueristic. Using an initial guess at a lower and an upper bound, 
   tries to bracket in an on empirical lower and upper bound, returning 
   a vector of [lower upper]."
  [reqstate & {:keys [distance init-lower init-upper]
               :or   {distance default-distance
                      init-lower 1
                      init-upper 10}}]
  (if (and (pos? (reduce + (vals (:minimum-supply reqstate))))
           (not (calculate-requirement reqstate distance)))
    (do (println [:minimum-supply-sufficient])
        (println [:bounded! [0 0]])
        [0 0])
    (loop [reqs      reqstate
           lower     init-lower
           upper     init-upper]
      (let [reqs  (-> reqs
                      (distribute (:src reqs) upper)
                      (update  :iteration inc))]
        (if-let [res (calculate-requirement reqs distance)] ;;naive growth.
          (do (println [:guessing-bounds [lower upper] :at upper :got res])
              (recur reqstate (inc upper)  (* 2 upper)))
          (do (println [:guessing-bounds [lower upper] :at upper :got 0])
              (let [res  [lower  upper]
                    _    (println [:bounded! res])]
                res)))))))

(defn iterative-convergence-shared
  "Given a requirements-state, searches the force structure 
   space by varying the supply of the requirements, until 
   it converges on a minimum feasible force structure.
   At the low end, we'll just be performing multiple 
   capacity analyses...Uses a shared base context to 
   save time on i/o."
  [reqstate & {:keys [distance]
               :or   {distance default-distance}}]
  (let [tables->ctx (quick-context  (:tables reqstate))
        echo (fn [{:keys [src iteration] :as reqs} dist]
               (do (println
                    (pprint/cl-format nil "Generated ~a ghosts of ~a on iteration ~a"
                                      dist src iteration))
                   reqs))]
  (loop [reqs      reqstate]
    (if-let [dist (calculate-requirement reqs distance tables->ctx)] ;;naive growth.
      (-> reqs
            (echo dist)
            (distribute (:src reqstate) dist)
            (update :iteration inc)            
            (recur))
      reqs))))

;;Currently 2x slower than ic....wonder if we can speed this
;;up?  Problem is, we end up doing a lot of higher-supply
;;runs, which hurts performance.  Doing more volume of
;;work than IC.  IC makes many small jumps.  BS
;;makes some large jumps, and some small jumps.

;;We need to modify this to figure out the bounds first.
;;We're wasting time on needless bisecting, when we haven't
;;established.
(defn bisecting-convergence
  [reqstate & {:keys [distance init-lower init-upper log]
               :or   {distance default-distance
                      init-lower 0
                      init-upper 10
                      log println
                     }}]
  (let [known?     (atom {})
        converge   (fn [dir reqs n]
                     (do (log [:converged dir n])
                         (distribute reqs (:src reqstate) n)))
        amount     (fn amt [reqs n]
                     ;(log [:amount n])
                     (get-or @known? n
                        (let [res (or (calculate-requirement reqs distance) 0)
                              _   (swap! known? assoc n res)]
                          res)))
        _ (assert (not= init-lower init-upper) "need a valid interval!")]
    (loop [reqs      reqstate
           lower init-lower
           upper init-upper]
      (let [hw    (quot (- upper lower) 2)
            mid   (+ lower hw)]
        (if (= mid lower)
          (case (mapv zero? [(amount reqs lower) (amount reqs upper)])
            [true  true] (if (pos? lower) (converge :left  reqs lower)
                             (converge :right  reqs lower))
              [false true] (converge :right reqs upper)              
              (throw (Exception. (str [:wierd-case! lower upper  @known? (:supply reqs)]))))
          (let [reqs (update reqs :iteration inc)
                rtest  (-> reqstate
                           (distribute (:src reqstate) mid)
                           )
                res (amount rtest mid)
                _   (log [:guessing [lower upper] :at mid :got res])]
            (if (pos? res)
              (recur reqs mid upper)
              (recur reqs lower mid))))))))
  
;;The iterative convergence function is a fixed-point function that implements the algorithm described in the declarations section.
;;During iterative convergence, we don;;t care about intermediate results, only the final fixed-point calculation.
;;After we determine the fixed-point, we can perform a final dynamic analysis (capacity analysis) on the output.
;;The concrete implementation follows:
    ;;Assuming we have a simulation object, we can call its FromExcel method to load all demand and supply from Excel.
    ;;Future iterations will avoid re-parsing demand and supply, but for now, we;;ll just reload the whole thing everytime.
        ;;Possibly use a "limited-reload? or hot-load" method.
    ;;Each time the simulation runs, it will load supply from the SupplyRecords input (which is a worksheet).
    ;;Our goal is to effectively generate supply records.
    ;;In the extreme case, we start with no supply, thus no supply records.
    ;;We must have ghost relation rules in effect (i.e. ghostables for all the SRCs), and a set of demands to simulate.
    ;;Prior to the first iteration, we attach a special observer to the simulation;;s event pump.
        ;;This observer, the GhostWatcher, will maintain statistics for all the ghosts spawned, by SRC, etc. during the simulation.
        ;;The GhostWatcher will serve as the GhostRecord generator that we will need in our distribution function.
    
    ;;On the first iteration, the simulation is initialized "fromexcel", which pulls in all supply, demand, policy, etc.
        ;;Note, again, there may be no supply.
    
    ;;We then pass the primed simulation, a valid distribution function, and the ghostwatcher, into the CalculateRequirement function.
        ;;The simulation is run in a non-interactive mode, with no supply, which triggers the generation of ghosts.
            ;;We want each run to be as fast as possible, thus we run the sim in its most effecient state, avoiding log files
            ;;and other detritus.
            
            ;;The externally-attached Ghost Watcher observes all of these ghost spawning events, noting the SRC count of ghosts
            ;;generated during the run.
            
        ;;CalculateRequirement;;s return value is simply a function of the application of the distributor to the observerd quantities of ghosts, by SRC,
        ;;in the ghostwatcher.  This should be a number of units, by SRC, by component.
    
    ;;Given a set of new units, we simply update the supply records data (worksheet), possibly recording the amount of supply added during each
        ;;iteration.
    
    ;;Given a set of no new units (i.e. zero ghosts generated), iterative convergence returns the reported supply.  The data is already on-hand for
    ;;additional analysis (namely capacity analyis), if desired.



(defn load-src [tbls src compo->distros] ;;for each src, we create a reqstate
  (let [src-filter (a/filter-srcs [src])
        src-tables (src-filter     tbls)] ;alters SupplyRecords, DemandRecords
    (->requirements-state src-tables ;create the searchstate.
                          src compo->distros)))

;;So, need a way to apply the step-function to the
;;current supply, compute new supply records, etc.
;;should be keeping a running tally of the
;;ratioal totals at any given time.
;;Perhaps, we store the actual value
;;in the supply records.
;;From there, we apply the growth step
;;by adding the rationals.
;;Then we coerce prior to running...

;;So, when we go to distribute

;;Maybe we have search do the work of creating
;;the context?
;;So the context is local to the search state..

;;We have an alternate implementation....
;;This is our entry point....
;;Note: If we filter out all demands, i.e. we have an src in
;;proportions that's not in the demand, and we're left with no
;;demands, we should skip the src and accumulate a warning or
;;something.
(defn tables->requirements
  "Given a database of distributions, and the required tables for a marathon 
   project, computes a sequence of [src {compo requirement}] for each src."
  [tbls & {:keys [dtype search src-filter]
           :or {search bisecting-convergence ;iterative-convergence
                dtype  :proportional
                src-filter (fn [_] true)}}]
  (let [;;note: we can also derive aggd based on supplyrecords, we look for a table for now.
        distros (into {} (->> (aggregate-distributions tbls :dtype dtype)
                              (filter (fn [[src _]]
                                        (src-filter src)))))
        peaks   (->>  (:DemandRecords tbls)
                      (tbl/table-records)
                      (filter #(and (:Enabled %)
                                    (distros (:SRC %))))
                      (demands->src-peaks))
        n       (atom (count peaks))]
    (->> distros
         (map (fn [[src compo->distros]] ;;for each src, we create a reqstate
                 (if-let [peak (peaks src)]
                   (let [_ (println [:computing-requirements src :remaining (swap! n dec)])                                     
                         reqstate       (load-src tbls src compo->distros)
                         _              (println [:growing-by dtype :from (:minimum-supply reqstate)])
                         [lower upper]  (find-bounds reqstate :init-lower 0 :init-upper peak)]
                     (if (== lower upper 0)
                       [src reqstate]
                       [src (search reqstate :init-lower lower :init-upper upper)]))
                   (do (println [:skipping-src src :has-no-demand])
                       [src nil])
                   ))))))


(defn requirements-by
  "Helper function for our parallel requirements computation."
  [tbls peaks search n]
  (fn [[src compo->distros]]
    ;;for each src, we create a reqstate
    (if-let [peak (peaks src)]
      (let [_ (println [:computing-requirements src :remaining (swap! n dec)])
            ;src-filter (a/filter-srcs [src])
            ;src-tables (src-filter     tbls) ;alters SupplyRecords, DemandRecords                                       
            reqstate   (load-src tbls src compo->distros)
                        #_(->requirements-state src-tables ;create the searchstate.
                                             src compo->distros)
            [lower upper]  (find-bounds reqstate :init-lower 0 :init-upper peak)]
        [src (search reqstate :init-lower lower :init-upper upper :log (fn [msg]
                                                                         (println (str [src msg]))))])
      (do (println [:skipping-src src :has-no-demand])
          [src nil]))))

;;https://gist.github.com/stathissideris/8659706
(defn seq!! 
   "Returns a (blocking!) lazy sequence read from a channel." 
   [c] 
   (lazy-seq 
    (when-let [v (async/<!! c)] 
      (cons v (seq!! c)))))

;;Trying to avoid the crap that's happening
;;with pipeline...we get stalled out on
;;long-running tasks, when we could still be making
;;progress...
(defn producer->consumer!! [n out f jobs]
  (let [;jobs    (async/chan 10)
        done?   (atom 0)
        res     (async/chan n)        
        workers (dotimes [i n]
                  (async/thread
                    (loop []
                      (if-let [nxt (async/<!! jobs)]
                        (let [res (f nxt)
                              _   (async/>!! out res)]
                          (recur))
                        (let [ndone (swap! done? inc)]
                          (when (= ndone n)
                            (do (async/close! out)
                                (async/>!! res true))))))))]
    res))

(defn producer->consumer [n out f jobs]
  (let [;jobs    (async/chan 10)
        done?   (atom 0)
        res     (async/chan n)        
        workers (dotimes [i n]
                  (async/go
                    (loop []
                      (if-let [nxt (async/<! jobs)]
                        (let [res (f nxt)
                              _   (async/>! out res)]
                          (recur))
                        (let [ndone (swap! done? inc)]
                          (when (= ndone n)
                            (do (async/close! out)
                                (async/>! res true))))))))]
    res))

(defn pmap! [n f xs]
  (let [out     (async/chan 10)
        in      (async/chan 10)
        _       (async/onto-chan in (seq xs))
        pipe    (producer->consumer
                    n  #_(.availableProcessors (Runtime/getRuntime)) ;; Parallelism factor
                                        ;                 (doto (a/chan) (a/close!))                  ;; Output channel - /dev/null
                   out
                   f
                   in)]
    (seq!! out)))
    
;;using core.async to pipeline this dude...
(defn tables->requirements-async
  "Given a database of distributions, and the required tables for a marathon 
   project, computes a sequence of [src {compo requirement}] for each src."
  [tbls & {:keys [dtype search src-filter]
           :or {search bisecting-convergence ;iterative-convergence
                dtype  :proportional
                src-filter (fn [_] true)}}]
  (let [;;note: we can also derive aggd based on supplyrecords, we look for a table for now.
        distros (into {} (->> (aggregate-distributions tbls :dtype dtype)
                              (filter (fn [[src _]]
                                        (src-filter src)))))
        peaks   (->>  (:DemandRecords tbls)
                      (tbl/table-records)
                      (filter #(and (:Enabled %)
                                    (distros (:SRC %))))
                      (demands->src-peaks))
        n       (atom (count peaks))
        src-distros->requirements  (requirements-by tbls peaks search n)
        out     (async/chan 10)
        in      (async/chan 10)
        _       (async/onto-chan in (seq distros))
        pipe    #_(async/pipeline-blocking
                   (.availableProcessors (Runtime/getRuntime)) ;; Parallelism factor
                                        ;                 (doto (a/chan) (a/close!))                  ;; Output channel - /dev/null
                   out
                   (map src-distros->requirements)
                   in)
                 (producer->consumer
                    2  #_(.availableProcessors (Runtime/getRuntime)) ;; Parallelism factor
                                        ;                 (doto (a/chan) (a/close!))                  ;; Output channel - /dev/null
                   out
                   src-distros->requirements
                   in)
        ]
    (seq!! out)))

(def supply-fields [:Type :Enabled :Quantity :SRC :Component :OITitle :Name
                    :Behavior :CycleTime :Policy :Tags :Spawntime :Location :Position :Original])

(defn requirements->table
  "Computes a finalized table of supply records representing the 
   required supply."
  [rs]
  (->> rs
       (filter second) ;eliminating nil resultsx
       (mapcat (comp :supply second))       
       (tbl/records->table)
;       (tbl/order-fields-by supply-fields)
       (tbl/map-field :Quantity long)))

(defn requirements-run
  "Primary function to compute  requirements analysis.  Reads requirements 
   project from inpath, computes requirement, and spits results to a tsv 
   table in the same root folder as inpath, requirements.txt"
  [inpath]
  (let [inpath (clojure.string/replace inpath #"\\" "/")
        base (->> (clojure.string/split inpath #"/")
                  (butlast)
                  (clojure.string/join "/"))
        outpath (str base "/requirements.txt")]
    (do (println ["Analyzing requirements for" inpath])        
        (->> (-> (a/load-requirements-project inpath)
                 (:tables)
                 (tables->requirements  :search iterative-convergence-shared)
                 (requirements->table)
                 (tbl/table->tabdelimited))
             (spit outpath))
        (println ["Spit requirements to " outpath]))))
              
(comment ;testing
;;   #_(def root (hpath "\\Documents\\srm\\tst\\notionalv2\\reqbase.xlsx"))
;;   (def ags "Type	Enabled	SRC	AC	NG	RC	Note
;; Blah	TRUE	43429R000	0.188405797	0.202898551	0.608695652	This produces a huge requirement lol.  Great pathological case.")
;;   (def agg-table
;;     #spork.util.table.column-table{:fields [:Type :Enabled :SRC :AC :NG :RC :Note],
;;                                    :columns [["Blah"] [True] ["43429R000"] [13/69] [42/69] [14/69]
;;                                              ["This produces a huge requirement lol.  Great pathological case."]]})
;;   (def agg-table
;;     #spork.util.table.column-table{:fields [:Type :Enabled :SRC :AC :NG :RC :Note],
;;                                    :columns [["Blah"] [True] ["10560RN00"] [1] [0] [0]
;;                                              ["This produces a huge requirement lol.  Great pathological case."]]})
  (def root (hpath "\\Documents\\marv\\vnv\\m4v6\\testdata-v6.xlsx"))
  (require '[marathon.analysis [dummydata :as data]])
  (def dummy-table
    (apply-schema (marathon.schemas/get-schema :SupplyRecords)
                  (tbl/keywordize-field-names (tbl/records->table  data/dummy-supply-records))))
  (def tbls (a/load-requirements-project root))
  
  ;;derive a requirements-state...
  (def icres (requirements->table
              (tables->requirements (:tables tbls) :search iterative-convergence)))
  (def bsres (requirements->table
              (tables->requirements (:tables tbls) :search bisecting-convergence)))
  (def bsresa (requirements->table
              (tables->requirements-async (:tables tbls) :search bisecting-convergence)))

  ;;Much better...This ends up testing a huge case.
  (def bigres (requirements->table
              (tables->requirements (assoc (:tables tbls) :GhostProportionsAggregate agg-table) :search bisecting-convergence)))
  (def s1 {"AC" 1696969696969697/4000000000000000
           "RC" 0N
           "NG" 5757575757575757/10000000000000000})
  (def rootbig "C:/Users/tspoon/Documents/srm/tst/notionalv2/reqbasebig.xlsx")
  (def tbls  (a/load-requirements-project rootbig))
  (def icres (requirements->table
              (tables->requirements (:tables tbls) :search iterative-convergence)))
  (def bsres (requirements->table
              (tables->requirements (:tables tbls) :search bisecting-convergence)))
  (def icsres  (requirements->table
                (tables->requirements (:tables tbls) :search iterative-convergence-shared)))

  (def pks (demands->src-peaks (tbl/table-records (:DemandRecords (:tables tbls)))))
  ;;These are massive src requirements...
  (def massives
    [{:SRC "42529RE00", :Required 1479, :Peak 201} ;;~5x
     {:SRC "41750R100", :Required 1240, :Peak 102}
     {:SRC "12567RE00", :Required 1152, :Peak 160}
     {:SRC "27523RC00", :Required 916,  :Peak 127}
     {:SRC "19539RB00", :Required 743,  :Peak 188}
     {:SRC "09537RB00", :Required 675,  :Peak 227}
     {:SRC "14527RB00", :Required 649,  :Peak 128}
     {:SRC "19473K000", :Required 563,  :Peak 262}
     {:SRC "10527RC00", :Required 503,  :Peak 115}])


  (require '[incanter [core :as i] [charts :as c]])
  (def root (hpath "\\Documents\\marv\\vnv\\m4v6\\testdata-v6.xlsx"))
  (def tbls (a/load-requirements-project root))
  (def razero (load-src (:tables tbls) "42529RE00" {"AC" 0 "NG" 0.125 "RC" 0.875}))     


  (def bisections ;;from a previous run
     [[ 3216 0]
      [  2412  0]
      [  2010  0]
      [  1809  0]
      [  1709  0]
      [  1659  9]
      [  1684  4]
      [  1696  0]
      [  1690  1]
      [  1693  0]
      [  1691  0]])
  ;;computing data on requirement holes..  
  (def emps
    (if false ;(io/exists? "emps.edn")
      (clojure.edn/read-string (slurp "emps.edn"))      
      (->> (range 1 1690)
           (pmap! 2 (fn [i] (do (println i) [i (calculate-requirement (distribute razero (:src razero) i) default-distance)])) )
           (into [] ))))

  (def emps (into emps bisections))

  (def bound-steps [[201 1]
                    [402 1]
                    [804  1]
                    [1608 1]
                    [3216 0]])

  
  (i/view (c/scatter-plot (map first emps) (map (comp (fn [n] (or n 0)) second) emps)
                          :title "Earliest Missed Demand x Growth Step" :x-label "Step" :y-label "Quantity Earliest Unfilled Demands"))
  (i/view (c/scatter-plot (map first emps) (map (comp #(if (pos? %) 1.0 0)(fn [n] (or n 0)) second) emps)
                          :title "Missed Demand? by growth Step" :x-label "Step" :y-label "Missed Demand? [1 => True 0 => False]"))

  (def distros {"AC" 0.0, "RC" 0.875, "NG" 0.125})
  (def clength 2190)
  (defn inits [n] (for [[c q] (distribute-by distros n) :when (pos? q)] (marathon.ces.entityfactory/intervals q clength)))
  (defn init-points [n] (apply concat (map-indexed (fn [i xs] (let [x (+ (* i 0.1) i)] (map vector (repeat x) (rest xs)))) (inits n))))
  (defn init-chart [n] (let [xs (init-points n)] (i/view (c/scatter-plot (map first xs) (map second xs)))))
  
  )




;; 'TOM Change 3 August -> implemented a bracketing algorithm not unlike binary search.
;; 'This is meant to be performed on a single SRC, i.e. a single independent requirement.
;; 'Bisection requires an src as the arguement.
;; Public Sub Bisect(sim As TimeStep_Engine, ns As Collection, left As Dictionary, right As Dictionary, Iteration As Long)

;; Dim searchstate As Collection
;; Dim middle As Long
;; Dim lower As Long
;; Dim upper As Long
;; Dim src As String
;; Dim lowest As Long
;; Dim uppermoved As Boolean
;; Dim idx As Long, bin As Long
;; Dim binstate As Dictionary

;; Dim ghosts As TimeStep_ObserverGhostWatch

;; Set searchstate = New Collection

;; lower = left.item("totalghosts")
;; lowest = lower
;; upper = right.item("totalghosts")
;; middle = lower + (upper - lower) \ 2

;; src = left("src")
   
;; 'determine what the next step should be

;; While upper - lower > 1
;;     Iteration = Iteration + 1
;;     Debug.Print "Iteration " & Iteration & ", Bracketing solution between n = [" & lower & ", " & upper & "] ghosts."

;;     Set supplyTable = copysupply(left("total")) 'starting from our last step
;;     Distribute src, middle - lowest, , searchstate  'add our ghosts.
    
;;     If noio Then 'don't bother writing to the sheet
;;         sim.Reset_Engine_FromExcel True, supplyTable 'this will reset marathon, using the GeneratedSupply Worksheet to pull in initial supply.
;;     Else
;;         updateGeneratedSupply
;;         sim.Reset_Engine_FromExcel True
;;     End If

;;     Set ghosts = sim.outputmanager.observers("Ghosts")


;;     'test sufficiency with new supply
;;     If Not CalculateRequirement(sim, ghosts, , searchstate) Then
;;         'did not generate ghosts.....
;;         'this means our middle value is now our right, upperbound.
;;         upper = middle 'move the bracket <<<<<<-
;;         middle = lower + (upper - lower) \ 2
;;         'no need to redistribute.
;;         uppermoved = True
;;     Else
;;         'we added ghosts, which means middle is insufficient.
;;         lower = middle 'move the bracket ->>>>>>>
;;         middle = lower + (upper - lower) \ 2
;;         'Set left = searchstate(searchstate.count)
;;         uppermoved = False
;;     End If
;; Wend

;; If upper - lower = 1 Then 'ubound is the answer
;;     lower = upper
;;     Set supplyTable = copysupply(left("total")) 'starting from our last step
;;     Distribute src, upper - lowest, , searchstate  'add our ghosts.
;; ElseIf upper - lower = 2 Then 'middle is the answer
;;      If uppermoved Then
;;         lower = lower + 1
;;      Else
;;         lower = upper
;;      End If
;;      Distribute src, upper - lowest, , searchstate
;; Else
;;     Err.Raise 101, , "convergence is off"
;; End If

;; Debug.Print "No More ghosts to generate.  Binary search converged on " & lower & " Ghosts for src " & src
;; If noio Then finalIO sim
;; updateGeneratedSupply
;; Set sim = Nothing
;; End Sub

;; Private Sub makeghost()

;; Dim grecord As GenericRecord
;; Set grecord = New GenericRecord

;; With grecord
;;     .AddField "Type", "SupplyRecord"
;;     .AddField "Enabled", True
;;     .AddField "Quantity", 1
;;     .AddField "SRC", "Ghost"
;;     .AddField "Component", "Ghost"
;;     .AddField "OITitle", "Anything"
;;     .AddField "Name", "Auto"
;;     .AddField "Behavior", "Ghost365_45"
;;     .AddField "CycleTime", 0
;;     .AddField "Policy", "Ghost365_45"
;;     .AddField "Tags", "Auto"
;;     .AddField "SpawnTime", 0
;;     .AddField "Location", "Auto"
;;     .AddField "Position", "Auto"
;;     .AddField "Original", False
;; End With

;; supplyTable.add "Ghost", grecord

;; Debug.Print "Asked to do requirements analysis without a ghost," & _
;;             " added Default ghost record to generated supply table."
;; End Sub


;;GeneratedSupply.csv  is out default output, apparently.
;;Don't think we really need these...

;;Distributors
;;============
;;I think we're going to skip this and just define a
;;distribute function..
  
;; Private Function addDistributor(src As String, compoDistributions As Dictionary, Optional dtype As DistributorType)
;; Dim distributor As Dynamic_Distributors
;; Dim compo

;; For Each compo In compoDistributions
;;     If compoDistributions(compo) = 0 Then
;;         compoDistributions.Remove (compo)
;;     End If
;; Next compo

;; Set distributor = New Dynamic_Distributors
;; Select Case dtype
;;     Case binned
;;         distributor.initBinned compoDistributions, src
;;         SRCdistributors.add src, distributor
;;     Case continuous1418
;;         distributor.initContinuous1418 compoDistributions
;;         SRCdistributors.add src, distributor
;;     Case rounding1418
;;         distributor.initRounding1418 compoDistributions
;;         SRCdistributors.add src, distributor
;; End Select

;; End Function

;;note: we need to filter only positive compodistriutions.

;;multimethod for constructing different distributors.
(defmulti distributor identity)
(defmethod distributor :binned [n] nil)
(defmethod distributor :continuous1418 [kw] nil)
(defmethod distributor :rounding1418 [kw] nil)



;; ''TOM Change 3 August -> implemented a bracketing algorithm not unlike binary search.
;; ''This is meant to be performed on a single SRC, i.e. a single independent requirement.
;; ''Bisection requires an src as the arguement.
;; Public Sub BisectionConvergence(Optional logevents As Boolean, Optional addcapacity As Boolean)

;; Dim tstrt As Single
;; Dim logger As TimeStep_ObserverLogFile
;; Dim ghosts As TimeStep_ObserverGhostWatch
;; Dim max As Long, min As Long
;; Dim nextX As Long
;; Dim generatedGhosts As Boolean
;; Dim fX As Long
;; Dim bracketed As Boolean
;; Dim src As String
;; Dim ky

;; Iteration = 0
;; importSupplyRecords
;; importAggregateDistributions
;; Err.Raise 101, , "Needs updating!"

;; If sim Is Nothing Then
;;     Set sim = New TimeStep_Engine
;;     sim.noio = noio
;;     sim.Initialize_Engine_FromExcel New TimeStep_SimState, True 'this will cause overhead....
;; Else
;;     sim.noio = noio
;;     sim.Reset_Engine_FromExcel
;; End If


;; If logevents Then
;;     Set logger = New TimeStep_ObserverLogFile
;;     logger.init "ReqEvents" & Iteration, sim.EventManager.evtstream
;; End If

;; For Each ky In sim.DemandManager.demandmap
;;     src = sim.DemandManager.demandmap.item(ky).src
;;     If src <> "Ghost" Then Exit For
;; Next ky

;; If src = "Ghost" Then Err.Raise 101, , "Only registered ghost srcs"


;; tstrt = Timer()

;; max = 10
;; min = 0
;; fX = max
;; nextX = max
;; bracketed = False

;; While max <> min
;;     Set ghosts = sim.outputmanager.observers("Ghosts")
;;     Iteration = Iteration + 1
;;     'determine what the next step should be
;;     Debug.Print "Searching for solution using " & nextX & " ghosts."
;;     fX = search(src, nextX, sim, ghosts)

;;     If fX = 0 Then
;;         bracketed = True
;;         max = nextX
;;         nextX = (max - min) \ 2 + min
;;     ElseIf fX > 0 Then
;;         If bracketed Then
;;             min = max
;;             max = 2 * max
;;             nextX = (max - min) \ 2
;;         Else
;;             If fX > max Then max = max + fX
;;             'ElseIf fX < max Then
;;                 'max = fX
;;             'End If
;;             min = max
;;             max = 2 * max
;;             nextX = max
;;         End If
;;     End If

;;     tstrt = Timer() - tstrt

;;     If noio Then 'don't bother writing to the sheet
;;         sim.Reset_Engine_FromExcel True, supplyTable 'this will reset marathon, using the GeneratedSupply Worksheet to pull in initial supply.
;;     Else
;;         updateGeneratedSupply
;;         sim.Reset_Engine_FromExcel True
;;     End If

;;     If logevents Then
;;         Set logger = Nothing
;;         Set logger = New TimeStep_ObserverLogFile
;;         logger.init "ReqEvents" & Iteration, sim.EventManager.evtstream
;;     End If
;; Wend

;; Debug.Print "No More ghosts to generate.  Binary search converged on " & max & " Ghosts for src " & src

;; If noio Then finalIO sim

;; updateGeneratedSupply

;; Set logger = Nothing
;; Set sim = Nothing

;; End Sub

;; 'write out the final, summary report, specifically the final supply, the number of ghosts, etc.
;; Private Sub finalIO(sim As TimeStep_Engine, Optional addcapacity As Boolean)

;; updateGeneratedSupply
;; If addcapacity Then
;;     sim.noio = False
;;     sim.Reset_Engine_FromExcel True
;;     sim.EventStepMarathon
;; End If


;; End Sub

;; Public Function search(src As String, ghostcount As Long, sim As TimeStep_Engine, ghostwatcher As TimeStep_ObserverGhostWatch) As Long
;; Static counts As Dictionary
;; search = 0
;; Distribute src, ghostcount, True 'update the ghost solution
;; sim.EventStepMarathon 'execute the simulation, may produce ghosts.
;; Set counts = getGhostCounts(ghostwatcher)
;; If counts.count > 0 Then
;;     Debug.Print "Generated ghosts on iteration " & Iteration
;;     search = counts(src)
;; End If
;; End Function





(comment ;;possibly obe
  ;;we add a ghost record though
  (defn make-ghost []
    (supply-record  "SupplyRecord"  true 1 "Ghost" "Ghost"
                    "Anything" "Auto"  "Ghost365_45" ;default behavior...
                    0 "Ghost365_45" "Auto"  0 "Auto" "Auto" false))

  ;;import the original provided supply records (currently from excel) into the supplytable
  (defn import-supply-records [ctx]) ;;may be uncessary.
  (defn clear-supply
    "Eliminate all the unit entities from the context."
    [ctx]
    )

  #_(defn reload-supply [ctx]
      (default-supply))

  ;;allows a nice handle on 
  #_(defn load-variable-supply-context [tbls]
      (fn [supply-records]))
)


;;Possibly OBE...

;;Compute a sequence of "empty" supply records
;;from the proportions indicated 
#_(defn proportion-record->supply-records [r]
  (let [src    (:SRC r)
        compos [:AC :RC :NG]]
    (for [c compos
          :let [n (get r c)]
          :when (pos? n)]
      (->supply-record src c n))))


;;Note: Currently not in use, OBE?

;;I think we'll have this as part of the reqstate...
;;rather than a separate output. Redo this...
#_(defn write-record
    [reqstate iteration src component quantity]
    (update-in reqstate  [:iterations src] conj
               (->outrecord iteration src component quantity)))
