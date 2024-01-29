;;Hopefully we provide a centralized namespace and
;;functionality to tame the legacy diaspora of
;;rule definitions.  We also decouple the primitive
;;comparison functions and define higher-order combinations
;;here that let us keep those details in a lower strata.
(ns marathon.ces.rules
  (:require [marathon.ces.query.primitive :as prim :refer
             [predicate same-val? defcomparison ord-fn key-compare
              key-pref  key-valuations flip only-keys-from change-if
              is? is-not is]]
            [marathon.ces  [core   :as core]
             [unit   :as unit]
             [supply :as supply]
             [rules  :as rules]
             [util :as util]]
            [marathon.ces.fill.fillgraph]
            [spork.entitysystem.store :as store]
            [spork.data [lazymap :as lm]]
            [spork.util [tags :as tag] [general :as gen]]))

;;utils
;;=====

;;mimick functionality from legacy m3.  NOTE: this is more constrained
;;than we'd like, and is only here for legacy support!.
;;Note: Since M3-v82, we fixed the legacy system to include all possible
;;supply independent of the presence of deployable supply.  Thus,
;;we eliminate the legacy dependency on the existence of a deployable
;;bucket prior to searching for extra supply (a useless constraint).

(defn append-vals-lazily
  "A dumb function to merge two maps, target and appendee, only where the keys in
  both match. Returns the resulting map where keys/vals from target have added
  keys/vals from appendee, as opposed to merge, where a biased union is
  performed. This function lives only to support legacy modes of computing
  ad-hoc categories of supply, and will be replaced in the future with a version
  that merges regardless of key presence."
  [appendee target]
  (reduce-kv (fn [acc k v]
               (if-let [m (get acc k)]
                 (assoc acc k (lm/lazy-map (merge v m)))
                 acc))
             target appendee))

;;Note: port this to general purpose lib later...
(defn lazy-merge
  "A dumb function to merge two maps, target and appendee.  Rather than 
   eagerly merging the values, new values are lazy-maps computed from a 
   delayed merge."
  ([m1 m2]
   (merge-with (fn [ml mr]
                 (lm/lazy-map (merge ml mr))) m1 m2)))


;;dynamic query environment, typically contains
;;use-supplied information for fitlering, ordering, and
;;similar projections.
(def ^:dynamic *env* {})

;;Utility functions for working with tags [move to spork.util.tags]
;;=================================================================

;;tag
;;not-tag
(defn tag
  ([tags tag]
     (ord-fn [l r]
             (key-compare #(tag/has-tag? tags tag % ) (get l :name) (get r :name))))
  ([tag]
     (ord-fn [l r]
             (key-compare #(tag/has-tag? (get *env* :tags) tag % ) (get l :name) (get r :name)))))

;;These are all obsolete right?

;;ANDTags
(defn and-tags
  ([tags xs]
     (let [tagged (memoize (fn [name] (tag/has-tags? tags name xs)))] ;hopefully this doesn't kill us.
       (ord-fn [l r]
               (let [lefts  (tagged (:name l))
                     rights (tagged (:name r))]
                 (cond (= lefts rights) 0
                       lefts -1
                       rights 1)))))
  ([xs] (let [tagged (fn [name] (tag/has-tags? (get *env* :tags) name xs))] ;hopefully this doesn't kill us.
          (ord-fn [l r]
                  (let [lefts  (tagged (:name l))
                        rights (tagged (:name r))]
                    (cond (= lefts rights) 0
                          lefts -1
                          rights 1))))))
;;NANDTags
(defn nand-tags
  ([tags xs] (flip (and-tags tags xs)))
  ([xs]      (flip (and-tags xs))))

;;ORTags
(defn or-tags
  ([tags xs]
     (let [tagged (memoize (fn [name] (tag/some-tags? tags name xs)))] ;hopefully this doesn't kill us.
       (ord-fn [l r]
               (let [lefts  (tagged (:name l))
                     rights (tagged (:name r))]
                 (cond (= lefts rights) 0
                       lefts -1
                       rights 1)))))
  ([xs] (let [tagged (fn [name] (tag/some-tags? (get *env* :tags) name xs))] ;hopefully this doesn't kill us.
          (ord-fn [l r]
                  (let [lefts  (tagged (:name l))
                        rights (tagged (:name r))]
                    (cond (= lefts rights) 0
                          lefts -1
                          rights 1))))))

;;NORTags
(defn nor-tags
  ([tags xs] (flip (nand-tags tags xs)))
  ([xs]      (flip (nand-tags xs))))


;;SRC Substition / Preference Rules
;;=================================

(def  src->fillrule
  (gen/memo-1 (fn [src] (marathon.ces.fill.fillgraph/sink-label src))))

;;Provides an ordered vector of suitable supply buckets to look.
(defn src->srcs [srcmap src]
  (->> (for [[rule cost] (get srcmap (src->fillrule src))]
         [(marathon.ces.fill.fillgraph/source-root rule) cost])
       (sort-by second)
       (mapv first)))

;;Refactor?!
;;Note: this seems a little bit odd, or is it elegant?
;;This is redefined....
(def srcs->prefs
  (gen/memo-1
   (fn [srcs]
     (into {} (map-indexed (fn [idx src] [src idx]) srcs)))))

;;TODO: Do we want to memoize? Looks like we tried before.
(defn src->prefs [srcmap src]  (srcs->prefs (src->srcs srcmap src)))


;;Common Entity comparisons
;;=========================

;;TODO# relook what needs to be in the general query namespace.
;;Maybe we should move these to sim.unit, or another namespace that
;;pertains to unit comparison?

;;Basic value-based orderings.  These provide numbers we can easily
;;compare on.  These will define min/max-[name], where [name] is
;;the symbol on the LHS, and the result will be a valid ord-fn
(key-valuations cycletime  :cycletime
                bog-budget unit/get-bog-budget
                dwell      unit/get-dwell
                bog        unit/get-bog 
                proportional-dwell unit/normalized-dwell
                relative-cycletime (fn [u] (float (/ (:cycletime u) (unit/get-cyclelength u))))
                unit-index  :unit-index
                unit-weight :unit-weight
                mod-level   :mod)

;;predicates...

;;Already defined...
;;whereCompo
;;whereNotCompo
;; (def MinDwell    (ordering unit/normalized-dwell))
;; (def MaxDwell    (flip MinDwell))

(def compo-pref #(key-pref :component %))
(def where-compo  compo-pref)
(def except-compo (comp flip compo-pref))

(def components ["AC" "RC" "NG" "RCAD" "RCAD-BIG"])
;;Emit preferences for standard component values.
(doseq [c components]
  (eval `(def ~(symbol c) (compo-pref ~c))))

(defn lazy-group-units
  "Groups the units by {:src {:unit-name unit-record}} into a lazy
  map.
  This is the expected data type for the :computed field in a rule."
  [units]
  (into {}
          (for [[src xs]  (group-by :src units)]
            [src (lm/lazy-map (into {} (map (juxt :name identity)) xs))])))

(defn nonbog-where [{:keys [src where] :or 
                     {src :any where identity} :as
                     env} ctx & {:keys [unit-pred-or
                                        unit-pred-and] :or
                                 {unit-pred-or (fn [u] false)
                                  unit-pred-and identity}}]
  (let [src-map (src->prefs (core/get-fillmap ctx) src) ;;only grab
        ;;prefs we want.
        ]
    #(and (where %)
          (src-map (:src %))
          (unit-pred-and %)
          (or (unit/can-non-bog? %)
              (unit-pred-or %)))))
;;Computed Categories
;;====================
;;In addition to our categories of supply that are actively monitored and cached
;;(perhaps overly so), we have one or more categories of supply that fly in the
;;face of typical notions of useable resources. Also, we may expect that these
;;categories are somewhat ad-hoc, or occur infrequently enough to justify
;;caching and book-keeping.

;;In this case, we can define computed categories, which merely provide a means
;;for projecting the current context onto an ad-hoc set of supply relative to a
;;niche query.

;;We need to define which categories are computed, and provide means for
;;projection (i.e. a function of ctx -> supply)

;;We also need to hook into the primary API so that, when finding supply for
;;said category, we have a way to check for a computed category. We might also
;;want to define a way to compose categories...

;;Note: There's an implicit constraint in M3, which we must respect in M4, that
;;only entities in the category of "NonBOG", i.e. entities tagged as nonbog
;;eligible, are able to engage in non-bog relations. We need to specifically
;;exclude followon-eligible demands from this query, since that will
;;unintentionally drag additional supply into the mix that we don't
  ;;want.
(defn compute-nonbog [env ctx & {:keys [unit-pred-or
                                         unit-pred-and
                                         change-units] :or
                                 {change-units identity
                                  unit-pred-or (fn [u] false)
                                  unit-pred-and identity}
                                 :as unit-fns}]
  (let [es      (store/select-entities ctx
                                       :from   [:unit-entity]
                                       :where (nonbog-where env ctx
                                                            unit-fns))]
    ;;take some units here maybe.
    (lazy-group-units (change-units es))))

(defn filter-sort-take
  "Filter a subset of records matching a predicate (or a function that
  returns logically true values), sort the logically true records
  according to sorter, and then take a portion of
  those records, rounded down, concatenating the result with the rest
  of the records that were logically false according to f.
  Intended to be used with compute-nonbog :change-units."  
  [f sorter portion xs]
  (let [groups (group-by (comp #(boolean %) f) xs)        
        trues (groups true)
        falses (groups false)
        n (int (* portion (count trues)))
        sort-map {:order-by sorter}]
    (->> (util/select sort-map trues)
         (take n)
         (concat falses))))

(defn has-states?
  "Does a unit have each of the states in wait-states?"
  [u wait-states]
  (->> wait-states
       (map (fn [state] (unit/has-state? u state)))
       (every? identity)))

(defn computed-with
  [wait-states & {:keys [change-units] :or {change-units identity}}]
  (fn [env ctx]
    (let [nonbogs (compute-nonbog env ctx
                                  :unit-pred-or
                                  #(has-states? % wait-states)
                                  :change-units change-units)]
      (lazy-merge
       nonbogs
       (store/get-ine ctx [:SupplyStore   ;;<-iff like-keys exist here
                           :deployable-buckets
                           :default])))))

;;need to ensure that the other donating demand is tagged with a :donor
;;TODO: add another parameter for how many cannibals to take.
(defn nonbog-rule-with
  "For packing in a :donors value to the category so that we can spec this."
  [wait-states & {:keys [change-units] :or {change-units identity}}]
    {:donors wait-states
     :restricted  "NonBOG"
     :computed (computed-with wait-states :change-units change-units)
     :effects   {:wait-time   999999
                 :wait-state  #{:waiting :unavailable}}})

(defn first-day? "Is this the first day the demand is active?"
  [{:keys [startday] :as demand} ctx]
  (= startday (spork.sim.simcontext/get-time ctx)))

(defn add-donor "Add the demand name to a set of :donors for a
  unit."
  [demand unit-e]
  (let [demand-name (:name demand)]      
    (update unit-e :donors #(conj (into #{} %) demand-name))))

(defn can-return? "Can the unit return to this demand on any day after
  the activation day of the demand after being donated to another demand?"
  [{:keys [donors] :as unit-e} demand ctx start?]
  (let [demand-name (:name demand)]
    (if start?
      ;;
      true
      (contains? donors demand-name))))

(defn tag-donors
  [demand es]
  (for [u es]
    (add-donor demand u)))

(defn nonbog-donor-return
  "There may be a case where we want units to donate to another
  demand and then be able to return to the original demand if the
  recipient demand deactivates before the original demand. The unit
  would progress through any transitive donations on the day it's
  released from the deactivated demand.  Untested
  for now, but left as a concept."
  [{:keys [src cat order-by where collect-by
           demand] :or 
    {src :any cat :default where identity} :as env}
   ctx ]
  (let [start? (first-day? demand ctx)]
    (compute-nonbog env ctx :unit-pred-and
                    #(can-return? % demand ctx start?)
                    :change-units tag-donors)))

;;Reducer/seq that provides an abstraction layer for implementing queries over
;;deployable supply. I really wish I had more time to hack out a better macro
;;for the reducers, but this works for now. Deployers provides a view of the
;;deployable supply partitioned by category and src. We'll likely have even more
;;categories later, but for now these are the main branches that we organize
;;deployable supply by.
(defn ->deployers
  "Given a supply store, returns a seqable, reducible object that can filter on
  category, the keys of the supply buckets in the supply store, on src, the
  subset of srcs within a specific category, or on a specific unit name. In the
  partitioning of unit entities, we have a coordinate that maps to a specific
  unit of supply. The coordinate is defined by [category src name], where name
  is the name of the unit."
  [supply & {:keys [cat src unit weight nm->unit]
             :or {cat  identity
                  src  identity
                  unit identity
                  weight (fn [_ _] 1.0)
                  nm->unit identity}}]
  (let [catfilter cat
        srcfilter src
        unitfilter unit]
    (reify
      clojure.lang.Seqable
      (seq [this]
        (filter identity
                (for [[cat srcs]    supply
                      [src units]   (only-keys-from srcfilter srcs)
                      [nm u]        units
                      :when (and (catfilter cat) (srcfilter src))]
                  (let [u (nm->unit u)]
                    (when (unitfilter u)
                      [[cat src (weight cat src)] u])))))
      clojure.core.protocols/CollReduce
      (coll-reduce [this f1]
        (reduce-kv (fn [acc cat srcs]
                     (change-if acc (catfilter cat)
                                (reduce-kv (fn [acc src units]
                                             (change-if acc (srcfilter src)
                                                        (reduce-kv (fn [acc nm unit]
                                                                     (let [unit (nm->unit nm)]
                                                                       (change-if units (unitfilter unit)
                                                                                  (f1 acc [[cat src (weight cat src)] unit])))) acc units)))
                                           acc (only-keys-from srcfilter srcs)))) (f1) supply))
      (coll-reduce [_ f1 init]
        (reduce-kv (fn [acc cat srcs]
                     (change-if acc (catfilter cat)
                                (reduce-kv (fn [acc src units]
                                             (change-if acc (srcfilter src)
                                                        (reduce-kv (fn [acc nm unit]
                                                                     (let [unit (nm->unit nm)]
                                                                       (change-if units (unitfilter unit)
                                                                                  (f1 acc [[cat src (weight cat src)] unit])))) acc units)))
                                           acc (only-keys-from srcfilter srcs)))) init supply)))))

(defn find-deployable-supply  [supply src]
  (keys (get (supply/get-buckets supply) src)))

;;#TODO expose a sql-like query interface for the simstate/simcontext.
;;There's a more primive query mechanism here...
;;Tables exposed by the simstate are..
;;demandstore [demands activations deactivations tags]
;;supplystore [units   deployables categories    tags]
;;policystore []
;;fillstore   []
;;events      ...
;;We may think of a general way to organize our data - entitystore is
;;really desireable, but currently not configured that way.

;;For now, we'll implement specific queries that directly examine
;;stuff, then abstract and migrate later.


;;For instance, we might have an select-entity query, via
;;spork.entitystore, that only works on units of supply.

;;The domains of supplystore are:
;;[unitdata deployable category cycle policy supplytags followon]

;;The domains of demandstore are:
;;[demanddata active unfilled activation deactivation changed demandtags]

;;Note: similar domains exist for the fillstore and policystore.


;;These should become basic supply queries.  We can change the
;;implementation strategy (likely to tag/components) after the
;;fact...Also, they're primitive supply for our more general fill
;;query language...


;;When we apply a primitive query, we want some notion of the original
;;distance or weight associated with the supply.  When we're returning
;;the elements, we have [[rule w] entry] as the results.
;;We can then pass these through additional transformation pipelines
;;likes filters and sorters to get the final ordering of
;;entries, but we should maintain the [rule w] as a context for
;;the rule and for the distance for said entry.  This will protect
;;us from sorting unintentionally outside the confines of the
;;overall preference for instance.  [Note: supply queries are
;;also "just" entity queries over the store.  We don't need to
;;tack ourselves down to looking at entities in the supply
;;map, or deployables for instance]

;;#TODO supplement this with supply queries, so we can change the
;;sort-order, etc.  allow caller to provide custom sort
;;function...this is pretty huge.  From that, we can build all kinds
;;of queries.  I inlined the sorting so that the initial supply is
;;ordered by preference.  We may want to do this lazily, since we
;;may not need all the supply.  Possible performance optimization.
(defn find-feasible-supply
  ([supply srcmap category src nm->unit]
   (let [#_   (println [:query/find-feasible5 srcmap category src])
         any-src?      (or (identical? src :any) (identical? src  :*))
         any-category? (or (identical? category :any) (identical? category  :*))
         src-selector  (cond any-src?  identity 
                             (fn? src) src
                             :else     (is? src))
         category-selector (cond any-category?  identity
                                 (set? category) (fn [x] (category x))
                                 (fn? category) category
                                 :else          (is? category))
         ;_ (println [:finding-supply src category])
         ]
     (if (and any-category?  any-src?) ;if both category and src are unconstrained, we  can pull any unit.
           (do ;(println [:any :any])
               (rules/->deployers supply :src src-selector :nm->unit nm->unit ))
             ;;if category is constrained, but src is not, then we can pull any unit within the category.
           (let [
                 prefs (src->prefs  srcmap src)
                 src-selector (if any-src? identity ;;ensure we enable filtering if indicated.
                                  prefs)
                 ;- (println category-selector)
                 ]
             (->>  (rules/->deployers supply :src src-selector :cat category-selector
                                :weight (fn [_ src] (get prefs src Long/MAX_VALUE))
                                :nm->unit nm->unit)
               ;  (r/map (fn [[k v]]
               ;           [(conj k (get prefs (second k) Long/MAX_VALUE)) v])) ;sort by a score.
                 (into [])
                 ;(sort-by  (fn [[k v]]  (nth k 2))) ;;note, this is just a way of assigning distance.
                 )))))
  ([supply srcmap src]
      (do ;(println [:query/find-feasible3])
          (find-feasible-supply supply srcmap :default src)))
  ([ctx src]
   (do ;(println [:query/find-feasible2])
       (find-feasible-supply (store/gete ctx :SupplyStore :deployable-buckets) (:fillmap (core/get-fillstore ctx)) :default src
                             (fn [nm] (store/get-entity ctx nm) )))))

;;__New definition of feasibility__
;;Since we have more dimensions of compatibility and preference to consider,
;;we extend the definition of feasibility...
;;Feasibility now implies:

;;Missionable:
;;  Globally Deployable or
;;  (Remissionable relative to Theater and Priority) or
;;  (Fenced to Theater)
;;  (Fenced to Location)
;;And
;;Substitutable (already doing this....)
;;And
;;Compatible with the Category (i.e. AC-Only, RC-Only, etc.)

;;Current setup goes:
;;  Find all supply, in order, by deployable bucket, by SRC substitution
;;  length.  Allows us to look for supply in the right places...
;;  For each bucket, filter the supply using any filters defined
;;  by the rules, then sort the bucket according to suitability
;;  function provided by the supplied preferences.
;;  The resulting sequence of supply is a lazily ordered set of
;;  units that could fill the demand.

;;We typically prosecute that strategy by looking at a different
;;set of "buckets" first:
;;  Namely, if we have any followon-supply left, we have a set of
;;followon buckets, keyed by followon code, that we can apply the
;;aforementioned strategy on.
;;  So, fill is a higher-order process that happens in stages...
;;  Concretely, fill using followons first.
;;  Then fill using globally available supply.
;;  Done.
;;  What we'd like to do is reuse as much of this infrastructure
;;  as possible.  So, if we can't fill, we want to provide
;;  remissionable supply as another solution.
;;  If possible, we'd like to weave followon into remissionable,
;;  and eliminate the discrete stages if possible.
;;  For now, I'll take concrete...
;;  Remissionable supply is supply that's deployed at the time
;;  of the attempted fill.  It's like followon, except instead of
;;  being in a holding state, it's in a deployed state.
;;  While the unit is deployed, we can always tap it for supply.
;;  Remissionability is determined - for now - by a deployed
;;  unit's relation to the demand we're considering remissioning.
;;  If there are units in the same theater, at a lower priority
;;  demand, then the unit may be remissioned to support the
;;  higher priority demand (while the unit has BOG).
;;  Remissioning results in a followon deployment.  Note:
;;  if the higher-priority demand is short enough, it should
;;  hold that the unit that was remissioned could go back
;;  to its lower-priority mission if the mission is still
;;  active.

;;  We should be able to use find-feasible-supply
;;  applied against a population of deployed entities, rather
;;  than deployers...Basically, the underlying population function
;;  is ->deployed instead of just ->deployers,
;;  where ->deployed probably takes theater as an argument...


;;We now want to include remissionability into the mix.
;;Remissionability means that we have supply in deployable



;;  FollowOn-deployable (not necessarily remissionable....)  We fully
;;  intend to use this supply if possible, rather than sending it
;;  back home to reentry (and wasting time).
;;  


;;Suitability is based on:
;;
;;Sourcing preference (SourceFirst rules, already implemented....)




;;might it not be easier to just define rules loosley?  We've got a lot o infrastructure built up here
;;that we might be able to just pass via functions....

;;It'd be nice to describe this via relations, possibly graph arcs in a multigraph.
;;We have several partitioned nodes of information; might be possible to define more
;;declarative queries that return ordered traversals instead of outright queries like
;;this.

;;Once we find a supply...we want to sort the supply.
;;This is where a slew of suitability functions come into play.
;;We used to have this in a dedicated unit comparison module, along
;;with a specialized unit comparator object that "interpreted" simple 
;;enum combinations to nest comparators into complicated rules, like 
;;prefer ac, then rc, by order of maximum dwell.

;;FMCA represents a complex set of rules for generating units...
;;For a rule like:


;;Computed Supply
;;===============

;;common choke point for us to find entities.  Currently, we don't update them
;;when we're looking for them.

;;find-entity can be a projection, where we quickly or loosely update the entity
;;based on its current state, then save the rest of the updates for later with the
;;hope that we'll have an interpolated entity we can use to judge fill.
;;Can we establish fixed rules for mapping state->stats?  Note: we already do this
;;inside update-entity via behaviors. 


  ;;rather than doing the full entity update....
  ;;can we score it based on its dwell/bog state?
  ;;perhaps fall back into a full update iff we have to?

  ;;combined with last-update, this keeps us from having to
  ;;pay the cost of updating entirely on fill.
  ;;If not, we need a way to retain the updated context.

  ;;We still have the fill-data or fill promise that we can
  ;;store potential stuff in...so...
  

;;BUGFIX: Non-synchronized units are considered based on their last-known
;;dwell, rather than dwell at the time of querying.  Unit behavior updates
;;can age the unit, but we'd need to fold the context into the update
;;process (currently we just look at units....).
;;Alternative is to compute an expected cycle-time based on the unit's
;;last update and the time-of-fill.  That'll provide our criteria for
;;the unit's normalized dwell stats, then "if" the unit is selected based
;;on other criteria, and the fill order, we perform a full update of
;;the unit.

;;find-feasible-supply returns a reducer....
;;we could do full updates (for now) and pack the resulting updated
;;context as meta data.  This is much heavier than computing the
;;cyclelength directly based off time and last-update....

;;Argument for adding the component, i.e. :last-touched....
;;unit/normalized-dwell could then use that component to determine if
;;the entity had been examined...i.e. (max :touched :lastupdate)...
;;unit behavior only uses :lastupdate.....lite-synched tasks like
;;fills could use :last-touched to compute stats, iff last-update <>
;;time-of-fill.

;;So, when we query fill,
;;a) all units with lastupdate <> time-of-fill, or last-touch <> time-of-fill,
;;   get time-of-fill added as :last-touch (entity records pulled during query).

;;b) we need a :dt to add to cycle-length stat in unit/normalized-dwell
;;   and other interpolable statistics....
;;   thus, our stats can be smart about interpolating when we touch an entity?
;;   at least in the case of fill (really the only one we use I believe....)
;;   if we have temporary-dt

;;We'll just add an interpolation process....with ephemeral data
;;for dt each time step.  alternately, we can just add to the dt
;;every time we touch an entity?
;;Easy enough, and general....that way, we can at least compute
;;synchronizations for lerping.

;;Formalizing interpolation allows us to do lerping for other functions
;;as well...
;;If/when we touch a unit, we just update the dt + last-touched.
;;When we update a unit, part of the update can be to drop the dt + last-touched...
;;or, we zero-out dt and make last-touched == last-update?

;;probably stick this in ces.core, allow other namespaces to use it.
;;so (defn touch [ctx id] ...)
;;gives us an entity map with the :dt computed relative to the
;;ctx time.  more general-purpose than fill (although fill will be
;;what uses it initially).

;;We have the current time from the context.

(defn compute-supply
  "Interstitial function that provides a hook-site for adding computed supply - if
  need be - else we return our legacty means of finding deployable supply.
  Currently assumes one category - we can and will extend this to allow multiple
  categories. Right now, we use the function associated with the category as a
  sort of pre-processor, making availabile the entire context and the query
  information, associating the result onto the supply used for the actual
  deployer query."
  [{:keys [src cat order-by where collect-by computed] :or
    {src :any cat :default} :as env} ctx]
  (let [supply (store/gete ctx :SupplyStore :deployable-buckets)]
    (if (not computed) supply ;;more common case.
        (assoc supply cat (computed env ctx)))))

;;Environmental queries
;;=====================
;;TODO: Look into dropping this, no longer in use.
(predicate followon  [l r] 
   (if (same-val? :followon l r) 0
       (cond 
        (same-val? :followon l *env*) 1
        (same-val? :followon r *env*) -1)))

;;Temporarily located here, amenable to refactoring.
;;================================================== 

;;Tag-related queries for filling:  

;;Determines if the unit is tagged with compatible information for either the 
;;demand name, of the general class of followoncode.  This is a more general 
;;concept that we need to abstract out, but for now it's ported as-is.
(defn inside-fence? [uic demandname followoncode tags]
  (let [unitname (:name uic)]
    (or (tag/has-tag? tags unitname followoncode)
        (tag/has-tag? tags unitname demandname))))
 
;;Determines if the unit is outside of any fencing.  We use a general tagging 
;;mechanism to partition this possible, and serve as a quick first check.
;;Units not explicitly tagged as :fenced are possible matches to the demandname
;;or followoncode criteria.  So feasible fenced units must be both fenced and 
;;fenced to a particular demand.
(defn outside-fence? [uic demandname followoncode tags]
  (when (tag/has-tag? tags :fenced (:name uic))
    (inside-fence? uic demandname followoncode tags)))

(defn fenced-compare [demandname followoncode tags l r]
  (let [x (inside-fence? l demandname followoncode tags)
        y (inside-fence? r demandname followoncode tags)]
    (cond (and l r) 0
          (and l (not r)) 1
          (and (not l) r) -1)))

;;maybe unnecessary
(defn fenced-by [demandname followoncode tags]
  (ord-fn [l r] 
     (fenced-compare demandname followoncode tags l r)))

(predicate fenced  [l r]
    (fenced-compare (get *env* :demandname) (get *env* :followoncode) (get *env* :tags) l r))


;;Stock Rules From Old Marathon
;;=============================

;;#TODO implement these guys, or optionally read them from the default project.
;; (def fencedto	builtin
;;      maxdwell	builtin
;; uniform	[fenced  followon maxdwell] 
;; mindwell	(flip maxdwell)
;; ac_first	[fenced followon (where-compo "AC") maxdwell]
;; rc_first	[fenced followon (where-compo "RC") maxdwell]
;; ng_first	[fenced followon (where-compo "NG") maxdwell]
;; ar_first	[fenced followon (where-compo "AR") maxdwell]
;; not_ac	(except-compo "AC")
;; title32	[(where-compo "NG") mindwell]

(def uniform  [when-fenced  max-proportional-dwell min-unit-weight])
(def min-dwell  [when-fenced  min-proportional-dwell min-unit-weight])
(def ac-first [when-fenced  AC max-proportional-dwell min-unit-weight])
(def rc-first [when-fenced  RC max-proportional-dwell min-unit-weight])
(def ng-first [when-fenced  NG max-proportional-dwell min-unit-weight])
;;(def ar-first [when-fenced when-followon AR max-proportional-dwell])
;;TODO: Revisit the definitions here, potentially using a better candidate for
;;predicate equality.  The inversion/flipping stuff is potentially awkward.
(def not-ac #(is-not (:component %) "AC"))
(def not-rc #(is (:component %) "AC"))
(def cannibalized #(is (unit/cannibalized? %)))

;;implies max dwell.
;;shifting to capitalizing compound rules...
;;when-fenced checks to see if the units is fenced for the demand.
;;min-unit-weight ensures that we always have a deterministic ordering
;;of units by sorting by unit ID at the end.
;;Rules defined here must be added to the +default-rules+ map for
;;registering.
(def NOT-AC      [when-fenced not-ac max-proportional-dwell min-unit-weight])
(def NOT-AC-MIN  [when-fenced not-ac min-proportional-dwell min-unit-weight])
(def cannibalized-not-ac-min [when-fenced cannibalized not-ac
                      min-proportional-dwell min-unit-weight])
;;max dwell
(def NOT-RC      [when-fenced not-rc max-proportional-dwell min-unit-weight])
;;Added for forward stationed/assigned units
;;Use min-proportional-dwell to grab the least ready units to allow
;;the more ready units to be used for rotational stuff.
(def NOT-RC-MIN      [when-fenced not-rc min-proportional-dwell
                  min-unit-weight])

(def title32 [#(is (:component %) "NG") min-proportional-dwell min-unit-weight])
;;apparently identical.
(def hld [#(is (:component %) "NG") min-proportional-dwell min-unit-weight])

;;this is a generic distance metric...
(defn distance [f n]
  #(Math/abs (double (- n (f % )))))

;;We use a weighted distance function
;;to apply a penalty to values that are
;;greater than the target n.
;;For values of f <= n, returns
;;(- n f).  For values of f > n,
;;returns (* 100  (abs (- n f))).
;;Creates a "downward bias" toward
;;values less than n.  Assumes
;;a penalty of 100 is sufficient (e.g.
;;original intent is cardinalty of 3).
(defn downward-biased-distance [f n]
  #(let [d (- n (f % ))]
     (if (pos? d) d
         (* d -100))))

;;Tom Change 8/19/2019
;;Reference for future use....This was our initial hack at mod.
;;The revised look establishes a preference for mod levels <=
;;to the target, then allows for mod levels > than the target
;;based on min distance from the target mod level,
;;where the original preferred min distance, max mod.

(defn mod-distance [n]
  (distance #(or (get % :mod)
                 (throw (ex-info "expected a :mod key, none found!" {:in %}))) n))

;;Added a rule that prefers <= a mod level above any value
;;> than the mod level, with proportional preference for values
;;> than the mod level that are closest to n otherwise.
(defn downward-mod-distance [n]
  (downward-biased-distance
   #(or (get % :mod)
        (throw (ex-info "expected a :mod key, none found!" {:in %}))) n))


;;may not need these....we probably want to customize the demand's source first
;;based on its mod level, e.g. let the data drive it.
(def MOD1 [(mod-distance 1) uniform])
(def MOD2 [(mod-distance 2) uniform])
(def MOD3 [(mod-distance 3) uniform])

(def <=MOD1 [(downward-mod-distance 1) uniform])
(def <=MOD2 [(downward-mod-distance 2) uniform])
(def <=MOD3 [(downward-mod-distance 3) uniform])

(def MOD1-Target-AC [(mod-distance 1) AC min-proportional-dwell min-unit-weight])

;;#TODO elevate stock queries into user-defined rules.
;;#MOVE TO ces.query
(def stock-queries (atom {}))

(defn- register-sourcing-rule!
  [k rule]
  (when-let [r (get @stock-queries k)]
    (println [:overwriting-sourcing-rule k]))
  (swap! stock-queries assoc k rule))

(defn register-rule!
  "Associate k with a sourcing rule, e.g.
   a means to sort a selection of entities to
   establish a total ordering for fill queries.
   k may be a keyword or a string; both will be
   registered.  This should make the rule
   available as meaningful input within
   a DemandRecord's :source-first field."
  [k rule]
  (cond (string? k) (do (register-sourcing-rule! k rule)
                        (register-sourcing-rule! (keyword k) rule))
        (keyword? k) (do (register-sourcing-rule! k rule)
                         (register-sourcing-rule! (name k) rule))
        :else (throw (ex-info "expected string or keyword for source-first rule key!"
                              {:k k :rule rule}))))
;;These are not case sensititve.
(def +default-rules+
  {"AC-FIRST"   ac-first
   "AC"         ac-first
   "RC-FIRST"   rc-first
   "RC"         rc-first
   "NG-FIRST"   ng-first
   "NG"         ng-first
   "RCAD"       RCAD
   "RCAD-BIG"   RCAD-BIG
   "UNIFORM"    uniform
   "MIN-DWELL"  min-dwell
   "NOT-AC-MIN" NOT-AC-MIN
   "CANNIBALIZED-NOT-AC-MIN" cannibalized-not-ac-min
   "NOT-AC"     NOT-AC
   "NOT-RC"     NOT-RC
   "NOT-RC-MIN" NOT-RC-MIN
   "MOD1"       MOD1
   "MOD2"       MOD2
   "MOD3"       MOD3
   "<=MOD1"     MOD1
   "<=MOD2"     MOD2
   "<=MOD3"     MOD3
   "MOD1-TARGET-AC" MOD1-Target-AC})

(doseq [[k r] +default-rules+]
  (register-rule! k r))


;;Category API
;;============
(def categories (atom {}))

(defn register-category! [k v]
  (swap! categories assoc k v)
  k)

(defn register-categories! [kvs]
  (doseq [[k v] kvs]
    (register-category! k v)))

(defn computed [cat]
  (when-let [c (@categories cat)]
    (c :computed)))

;;notes moved from disparate locations.

;;computed categories indicate which categories can be derived on-demand by
;;applying a function against the context.

;;Note: Changed to account for updated, i.e. corrected behavior in M3-82, we now
;;fully merge (if there is deployable supply or not).

;;Tom Hack 26 May 2016
;;If we're not SRM demand, i.e. the category is something other than
;;SRM, we use the default category so as to not restrict our fill.
(def rc-cannibalization-rule
  {:restricted  "NonBOG"
    :filter   (fn [u] (not= (:component u) "AC"))
   :computed (fn [{:keys [where] :as env}  ctx]
                (lazy-merge
                 (compute-nonbog
                  (assoc env :where
                         (fn [u] (not= (:component u) "AC")))
                  ctx) ;;<-merge these in
                 (store/get-ine ctx [:SupplyStore   ;;<-iff like-keys exist here
                                     :deployable-buckets
                                     :default])))
    :effects   {:wait-time   999999
                :wait-state  #{:waiting :unavailable :cannibalized}}})
(def +default-categories+
  {:default   {:filter (fn [u] (not (:fenced? u)))} ;;maybe filter not necessary?
   "SRM"      {:restricted "SRM"}
   "NonBOG"
   {:restricted  "NonBOG"
    :computed  (fn [env ctx]
                 (lazy-merge
                  (compute-nonbog env ctx) ;;<-merge these in
                  (store/get-ine ctx [:SupplyStore   ;;<-iff like-keys exist here
                                      :deployable-buckets
                                      :default])))
   :effects    {:wait-time   999999
                :wait-state  :waiting}}
   "Forward"
   {:doc "Like NonBOG, but only exists for compo1 units; aligns with a region, and provides
          additional metadata about the unit's status (e.g. for visualization and recording
          purposes).  Filter only those units that are aligned to the
  forward region."
    :restricted  "NonBOG"
    :computed  (fn [env ctx]
                 (lazy-merge
                  (compute-nonbog env ctx)
                  (store/get-ine ctx [:SupplyStore   ;;<-iff like-keys exist here
                                      :deployable-buckets
                                      :default])))
    :effects    {:wait-time   999999
                 :wait-state  #{:waiting :forward}}
    :filter
    (fn fenced [u]
      (let [demand (*env* :demand)]
        (and (= (u :aligned)
                (demand  :region)))))}
   "RC_Cannibalization" ;;new category for cannibalized demand, was NonBOG-RC-Only
   rc-cannibalization-rule
   ;;Keeping here so that we can still work with legacy data.
  "NonBOG-RC-Only"
   rc-cannibalization-rule
   ;;deployable buckets
   ;;computed first, then filter, then ordering rules
   ;;
   ;;HLD can-non-bog?  similar to is-cannibalization?
   ;;effects: add
   ;;cannibalized instead of compute-nonbog
   ;;:filter is the hook site for :state (look in marathon.ces.unit
   ;;for getting state data from a unit.
   ;;HLD-all draw from units in a demand.
   ;;compute-nonbog selecting all entities, filtering if can-non-bog?
   ;;and will find out that they cant
   ;;compute-cannibalized-hld
   ;;another rule only draws half of the supply.  compute-non-bog
   ;;returns a selection of entities. filter all entities substitutale
   ;;for this src that are non bog and meet higher filters.  50% per
   ;;SRC. group by src es
   ;;OR HLD taps into default, nonbog
   ;;src to entityname to entity (nested map) and then from there
   ;;provided percentages for how many to supply
   ;;use

   ;;HLD is NonBOG NOT-AC-MIN preference.
   ;;Use this rule for using cannibalized units in some other demand.
   ;;We now allow units to change waiting demands, even if they filled
   ;;one waiting demand earlier in the filling process on the same day.
   ;;Note that units won't be back filled on the same day after a unit
   ;;leaves the cannibalized demand for this demand.
   ;;This allows all cannibalized units to fill demands matching this
   ;;rule.
   "nonbog_with_cannibals"
   (nonbog-rule-with [:cannibalized])
   ;;This allows 50% of the cannibalized units to fill demands
   ;;matching this rule.
   "nonbog_with_0.5_cannibals"
   (nonbog-rule-with [:cannibalized]
                     :change-units
                     (partial filter-sort-take
                              unit/cannibalized?
                              cannibalized-not-ac-min
                              0.5))
   ;;Added to provide a filtering criteria for modernized demands.
   ;;We never modernize mod 1, since that's considered the absolute
  ;;highest mod level.
  "Modernization"
   {:restricted "NonBOG"
    :filter (fn [u] (>= (get u :mod) 2))
    :computed
    (fn [{:keys [where] :as env}  ctx]
      (lazy-merge
       (compute-nonbog
        (assoc env :where
               (fn [u] (>= (get u :mod) 2)))
        ctx) ;;<-merge these in
       (store/get-ine ctx [:SupplyStore   ;;<-iff like-keys exist here
                           :deployable-buckets
                           :default])))
    :effects    {:wait-time   365
                 :wait-state  #{:waiting :modernizing}}}

  "Modernization-AC"
   {:restricted "NonBOG"
    :filter
    (fn [u] (and (= (:component u) "AC")
                 (>= (get u :mod) 2)))
   :computed
   (fn [{:keys [where] :as env}  ctx]
     (lazy-merge
      (compute-nonbog
       (assoc env :where
              (fn [u] (and (= (:component u ) "AC")
                           (>= (get u :mod) 2))))
       ctx) ;;<-merge these in
      (store/get-ine ctx [:SupplyStore   ;;<-iff like-keys exist here
                          :deployable-buckets
                          :default])))
    :effects {:wait-time   365
              :wait-state  #{:waiting :modernizing}}}
   "Fenced"
   {:restricted "Fenced"
    :computed
    (fn [{:keys [where] :as env} ctx]
      (store/get-ine ctx [:SupplyStore   ;;<-iff like-keys exist here
                          :deployable-buckets
                          :default]))
    :filter
    (fn fenced [u]
      (let [demand (*env* :demand)
            #_ (println [:checking-fence-for (:name demand) (:region demand)
                        (:name u) (:aligned u) (:fenced? u)])
            ]
        (and (u :fenced?)
             (= (u :aligned)
                (demand  :region)))))}
   })

(register-categories! +default-categories+)

;;new rules....should be able to compose these...
;;By default, we get substituable, globally-available supply using our
;;existing query.


;;Example rules:

;;establishes a preference...
;;given the rule "SRC3"
;;We search from an abstract deployable resource

(comment 
'{:select  :*
  :from    :deployable
  :where    {:category  [:default :jit]
             :src       "SRC3"}
  :order-by [[:AC :RCAD] 
              :MaxDwell]}

'{:select  :*
  :from    :deployable 
  :where    {:category  [:default :jit]
             :src       "SRC3"
             :has-tag  #{"Special"}}
  :order-by [[:AC :RCAD] 
              :MaxDwell]}
)



;;(defcomparer rotational-demand [[[RC MaxDwell] 
;;                                 [AC MaxDwell]]
;;                                RCAD
;;                                Generate-RC
;;                                Generate-AC
;;                                Generate-RCAD])

;;The default from Marathon is...
;;Fenced 
;;

;;Note -> Generate-X implies that the unit does not exist.
;;So, the first couple of rules actually apply to a comparison function.
;;The next n rules apply to some generating function for new entities. 
;;We may wish to keep them separate.

;;(defcomparer initial-demand [[AC-First MaxDwell]
;;                             [RC-AD MaxDwell]])


#_
(def demand-filters
  {:default (fn [_] true)
   "Modernization-AC"
   (fn [u] (and (= (:component u) "AC")
                (>= (get u :mod) 2)))

   "Modernization"
   (fn [u] (>= (get u :mod) 2))

   "NonBOG-RC-Only"
   (fn [u] (not= (:component u) "AC"))})

#_
(def computed-categories
  {"NonBOG"
      (fn [env ctx]
          (lazy-merge
            (compute-nonbog env ctx) ;;<-merge these in
            (store/get-ine ctx [:SupplyStore   ;;<-iff like-keys exist here
                                :deployable-buckets
                                :default])))
   "NonBOG-RC-Only"
        (fn [{:keys [where] :as env}  ctx]
          (lazy-merge
            (compute-nonbog 
              (assoc env :where 
                (fn [u] (not= (:component u) "AC")))
               ctx) ;;<-merge these in
            (store/get-ine ctx [:SupplyStore   ;;<-iff like-keys exist here
                                :deployable-buckets
                                :default])))
   ;;Added to provide a filtering criteria for modernized demands.
   ;;We never modernize mod 1, since that's considered the absolute
   ;;highest mod level.
   "Modernization"
   (fn [{:keys [where] :as env}  ctx]
     (lazy-merge
      (compute-nonbog
       (assoc env :where
              (fn [u] (>= (get u :mod) 2)))
       ctx) ;;<-merge these in
      (store/get-ine ctx [:SupplyStore   ;;<-iff like-keys exist here
                          :deployable-buckets
                          :default])))
   "Modernization-AC"
   (fn [{:keys [where] :as env}  ctx]
     (lazy-merge
      (compute-nonbog
       (assoc env :where
              (fn [u] (and (= (:component u ) "AC")
                           (>= (get u :mod) 2))))
       ctx) ;;<-merge these in
      (store/get-ine ctx [:SupplyStore   ;;<-iff like-keys exist here
                          :deployable-buckets
                          :default])))

   })


#_
(def computed-categories
  {"NonBOG"
      (fn [env ctx]
          (lazy-merge
            (compute-nonbog env ctx) ;;<-merge these in
            (store/get-ine ctx [:SupplyStore   ;;<-iff like-keys exist here
                                :deployable-buckets
                                :default])))
   "NonBOG-RC-Only"
        (fn [{:keys [where] :as env}  ctx]
          (lazy-merge
            (compute-nonbog 
              (assoc env :where 
                (fn [u] (not= (:component u) "AC")))
               ctx) ;;<-merge these in
            (store/get-ine ctx [:SupplyStore   ;;<-iff like-keys exist here
                                :deployable-buckets
                                :default])))
   ;;Added to provide a filtering criteria for modernized demands.
   ;;We never modernize mod 1, since that's considered the absolute
   ;;highest mod level.
   "Modernization"
   (fn [{:keys [where] :as env}  ctx]
     (lazy-merge
      (compute-nonbog
       (assoc env :where
              (fn [u] (>= (get u :mod) 2)))
       ctx) ;;<-merge these in
      (store/get-ine ctx [:SupplyStore   ;;<-iff like-keys exist here
                          :deployable-buckets
                          :default])))
   "Modernization-AC"
   (fn [{:keys [where] :as env}  ctx]
     (lazy-merge
      (compute-nonbog
       (assoc env :where
              (fn [u] (and (= (:component u ) "AC")
                           (>= (get u :mod) 2))))
       ctx) ;;<-merge these in
      (store/get-ine ctx [:SupplyStore   ;;<-iff like-keys exist here
                          :deployable-buckets
                          :default])))

   })
