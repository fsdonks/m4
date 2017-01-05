;;A place for defining queries on simulation state.  Serves as a
;;useful hub for defining complicated queries.  May also eventually 
;;extend spork.entitystore protocols to marathon stores.
;;Currently looking into how we can either supplement or
;;replace this with something based on datascript/datomic
;;querying. 
(ns marathon.ces.query
  (:require [marathon.ces  [core :as core]
                           [unit :as unit]
                           [supply :as supply]]
            [spork.entitysystem.store :as store]
            [marathon.ces.fill.fillgraph]
            [spork.util.reducers]
            [spork.util [tags :as tag]]
            [clojure.core [reducers :as r]]))

;; (defmacro napply [f k & xs]
;;   `(fn [m#] (~f (get m# ~k) ~@xs)))

;; (defn klift 
;;   ([k f]       (napply f k))
;;   ([k f v]     (napply f k (v m)))
;;   ([k f v1 v2] (napply f k v1 v2)))  

(defmacro mapfunctor
  [[k v] expr]
  `(fn [m#]
     (let [~v (get m# ~k)]
       ~expr)))

(defmacro mapop 
  [nm args expr]  
  (let [[k & vs] args]
    (if (zero? (count vs))
      `(defn ~nm [~k] 
         (mapfunctor [~k curr#] (~expr curr#)))
      (let [v (first vs)]
        `(defn ~nm [~k ~v]
           (mapfunctor [~k curr#] (~expr curr# ~v)))))))                                    

(defn one? [x] (== x 1))

(doseq [op '[= < > <= >= not=]]
  (eval `(mapop ~(symbol (str "?" op)) [k# v#]  ~op)))

(doseq [op '[even? odd? zero? one? pos?]]
  (eval `(mapop ~(symbol (str "?" op)) [k#]  ~op)))


(defn ?between [k lower upper] 
  (mapfunctor [k curr]
              (and (<= curr upper)
                   (>= curr lower))))

(defn ?outside [k lower upper]
   (let [f (?between k lower upper)] 
     (fn [m]  (not (f m)))))

(defn find-deployable-supply  [supply src] (keys (get (supply/get-buckets supply) src)))
(def  src->fillrule (memoize (fn [src] 
                               (marathon.ces.fill.fillgraph/sink-label src))))

;;Provides an ordered vector of suitable supply buckets to look.
(defn src->srcs [srcmap src] 
  (->> (for [[rule cost] (get srcmap (src->fillrule src))]
             [(marathon.ces.fill.fillgraph/source-root rule) cost]) 
       (sort-by second)
       (mapv first)))

(def srcs->prefs (memoize (fn [srcs]   
                              (into {} (map-indexed (fn [idx src] [src idx]) srcs)))))
(defn src->prefs [srcmap src]  (srcs->prefs (src->srcs srcmap src)))

(defmacro change-if [default test & body]
  `(if ~test
     ~@body
     ~default))


;;we could replace this with an entity query going forward. 
;;For now, it'll work;  We accomplish the same thing in the
;;end: we get a selection of unit entities that meet our
;;selection criteria (namely deployable).

;;really, an equally valid way is
;;for each unit that has a deployable category,

;; (defn ->deployers
;;   "Given a supply store, returns a seqable, reducible object that can 
;;    filter on category, the keys of the supply buckets in the supply 
;;    store, on src, the subset of srcs within a specific category, or 
;;    on a specific unit name.  In the partitioning of unit entities, 
;;    we have a coordinate that maps to a specific unit of supply.
;;    The coordinate is defined by [category src name], where name 
;;    is the name of the unit."
;;   [ctx & {:keys [cat src unit weight]
;;              :or {cat  identity 
;;                   src  identity 
;;                   unit identity
;;                   weight (fn [_ _] 1.0)}}]
;;   (store/select-entities 


;;Reducer/seq that provides an abstraction layer for implementing 
;;queries over deployable supply.  I really wish I had more time 
;;to hack out a better macro for the reducers, but this works for now.
;;Deployers provides a view of the deployable supply partitioned by
;;category and src.  We'll likely have even more categories later,
;;but for now these are the main branches that we organize deployable
;;supply by.
(defn ->deployers
  "Given a supply store, returns a seqable, reducible object that can 
   filter on category, the keys of the supply buckets in the supply 
   store, on src, the subset of srcs within a specific category, or 
   on a specific unit name.  In the partitioning of unit entities, 
   we have a coordinate that maps to a specific unit of supply.
   The coordinate is defined by [category src name], where name 
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
                (for [[cat srcs]    (:deployable-buckets supply)
                      [src units]   srcs
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
                                           acc srcs))) (f1) (:deployable-buckets supply)))
      (coll-reduce [_ f1 init]      
        (reduce-kv (fn [acc cat srcs]
                     (change-if acc (catfilter cat)
                                (reduce-kv (fn [acc src units]
                                             (change-if acc (srcfilter src)
                                                        (reduce-kv (fn [acc nm unit]
                                                                     (let [unit (nm->unit nm)]
                                                                       (change-if units (unitfilter unit)
                                                                                  (f1 acc [[cat src (weight cat src)] unit])))) acc units)))
                                           acc srcs))) init (:deployable-buckets supply))))))


;; (deftype filterfunc [itm _meta]
;;   clojure.lang.IFn
;;   (invoke [x y]   (or (identical? y itm) (= y itm)))
;;   clojure.lang.IObj
;;   (toString [o] (str itm))
;;   (meta [o] _meta)
;;   (withMeta [o m] (filterfunc. itm m)))  

;(defn ->filterfunc [expr]
;  (if (list? expr)
;    (

(defn is? 
  ([x y] (or (identical? x y) (= x y)))
                                        ;([x] (fn [y] (is? x y)))
  ([x]  ;(filterfunc. x nil)
        (fn [y] (is? x y)))
   )


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
   (let [;_   (println [:query/find-feasible5 srcmap category src])
         any-src?      (or (identical? src :any) (identical? src  :*))
         any-category? (or (identical? category :any) (identical? category  :*))
         src-selector  (cond any-src?  identity 
                             (fn? src) src
                             :else     (is? src))
         category-selector (cond any-category?  identity 
                                 (fn? category) category
                                 :else          (is? category))
         ;_ (println [:finding-supply src category])
         ]
     (if (and any-category?  any-src?) ;if both category and src are unconstrained, we  can pull any unit.
           (do ;(println [:any :any])
               (->deployers supply :src src-selector :nm->unit nm->unit ))
             ;;if category is constrained, but src is not, then we can pull any unit within the category.
           (let [
                 prefs (src->prefs  srcmap src)
                 src-selector (if any-src? identity ;;ensure we enable filtering if indicated.
                                  #(contains? prefs %))
                 ;- (println category-selector)
                 ]
             (->>  (->deployers supply :src src-selector :cat category-selector :weight (fn [_ src] (get prefs src Long/MAX_VALUE))
                                :nm->unit nm->unit)
                 (r/map (fn [[k v]]
                          [(conj k (get prefs (second k) Long/MAX_VALUE)) v])) ;sort by a score.
                 (into [])
                 (sort-by  (fn [[k v]]  (nth k 2))) ;;note, this is just a way of assigning distance.
                 )))))
  ([supply srcmap src]
      (do ;(println [:query/find-feasible3])
          (find-feasible-supply supply srcmap :default src)))
  ([ctx src]
   (do ;(println [:query/find-feasible2])
       (find-feasible-supply (core/get-supplystore ctx) (:fillmap (core/get-fillstore ctx)) :default src
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

(defn ordering? [x] (get (meta x) :ordering))
(defmacro ord-fn [[l r] & body]
  `(vary-meta (fn [~l ~r] ~@body) assoc :ordering true))

(defn ordering 
  ([f]  (if (ordering? f) f (ord-fn [l r] (compare (f l) (f r)))))
  ([f & fs]
    (let [fs (into [f] fs)]      
      (ord-fn [l r]
        (reduce (fn [acc f]
                  (let [res (if (ordering? f) (f l r) 
                                (compare (f l) (f r)))]
                    (if (not (zero? res))
                      (reduced res)
                      acc))) 0 fs)))))

(defn by-key [k]
  (ord-fn [l r] 
    (compare (get l k) (get r k))))
 
(defn flip [f] (ord-fn [l r] (f r l)))
(def descending flip)

(defn by-keys 
  ([ks]         (apply ordering (map (fn [k] #(get % k)) ks)))
  ([ks k->comp] (apply ordering (map (fn [k] #(k->comp k)) ks)) ))

;;Note the use of not= .  This seems counterintutive, but 
;;the predicates, used as comparators, means that false 
;;values are less than true.  So if you want a preference, 
;;you need to invert the predicate.
(defn key-pref [k c] 
  (if (fn? c)
    (ordering #(c (get % k)))
    (ordering #(not= (get % k) c))))

;;Basic Orderings
;;===============

;;We need to formalize these later; for now 
;;they'll serve.
;;#TODO allow users to define suitability queries via scripts 
;;or a little language.



;;We have a set of unit comparison criteria that together form a
;;little language for comparing units.

(defn same-val? 
  ([k r1 r2] (= (get r1 k) (get r2 k)))
  ([k r1 r2 & rs]
     (let [l (get r1 k)
           r (get r2 k)]
       (when (= l r)             
         (reduce (fn [acc r]
                   (if (= (get r k) acc) acc
                       (reduced nil)))
                 l
                 rs)))))
            
;;For convenience, we define functions that alias the comparer...
(defmacro defcomparison 
  "Defines a comparison in ascending order.  Derives default ordering functions 
   for both ascending and descending values."
  [type nm [l r] & body]
  (let [[hi lo] (case type 
                  :value     ["min"   "max"]
                  :predicate ["not" "when"])
        hi  (symbol (str hi "-" nm))
        lo  (symbol (str lo "-" nm))]                  
    `(do (def ~hi (ord-fn [~l ~r] 
                 ~@body))
         (def ~lo (ord-fn [~r ~l]
                 ~@body)))))

(def ^:dynamic *env* {})
      
(defmacro pred-compare
  ([pred expr]
     `(if (~pred ~expr) 1 -1))
  ([pred] 
     `(if ~pred 1 -1)))

(defn key-compare [k l r]
  (if (keyword? k)
    (compare (get l k)
             (get r k))
    (compare (k l) (k r))))

;; (defmacro compare* [l r & else]
;;   `(let [res# (compare ~l ~r)]
;;      (if (zero? res#) ~@else
;;          res#)))


(defmacro valuation [nm [l r] & body] 
  `(defcomparison :value ~nm [~l ~r] ~@body))

(defmacro key-valuation [nm k] 
  `(valuation ~nm [l# r#]
       (key-compare ~k l# r#)))

(defmacro key-valuations [& kvps]
  `(do ~@(for [[nm f] (partition 2 kvps)]
          `(key-valuation ~nm ~f))))

;; (defmacro predicates [& kvps]
;;   `(doseq [[k# nm#] ~kvps]
;;      (predicate k#)))

(defmacro predicate [nm [l r] & body] 
  `(defcomparison :predicate ~nm [~l ~r] ~@body))                   


;;TODO# relook what needs to be in the general query namespace.
;;Maybe we should move these to sim.unit, or another namespace that
;;pertains to unit comparison?  

;;Basic value-based orderings.  These provide numbers we can easily
;;compare on.
(key-valuations cycletime  :cycletime
                bog-budget unit/get-bog-budget
                dwell      unit/get-dwell
                bog        unit/get-bog 
                proportional-dwell unit/normalized-dwell
                relative-cycletime (fn [u] (float (/ (:cycletime u) (unit/get-cyclelength u)))))

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

;;Environmental queries
;;=====================

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

;;tag
;;not-tag
(defn tag 
  ([tags tag]
     (ord-fn [l r]
             (key-compare #(tag/has-tag? tags tag % ) (get l :name) (get r :name))))
  ([tag]  
     (ord-fn [l r]
             (key-compare #(tag/has-tag? (get *env* :tags) tag % ) (get l :name) (get r :name)))))


;;Utility functions for working with tags [move to spork.util.tags]
;;=================================================================


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

(defn nand-tags 
  ([tags xs] (flip (and-tags tags xs)))
  ([xs]      (flip (and-tags xs))))
                  
;;NANDTags

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


;;TODO -> make this a bit higher level, provide an API for defining 
;;environmental queries and specifying which vars in an expression are 
;;drawn from the environment (i.e. the state monad....)
(defmacro with-query-env [env & expr]
  `(binding [~'marathon.ces.query/*env* ~env]
     ~@expr))


;;TODO# move these generic functions into a more general namespace.
(defn ands [x fs]
  (reduce (fn [acc f]
            (if (f acc) 
              acc
              (reduced nil))) x fs))

(defn ors [x fs]
  (reduce (fn [acc f]
            (if (f x) 
              (reduced x)
              acc)) nil fs)) 

(defn nands [x fs] (not (ands x fs)))
(defn nors  [x fs] (not (ors x fs)))
  
(defn filt-sort [f ord xs]
     (let [filtered      (if f (filter f xs) xs)]
       (if ord (sort ord filtered) filtered)))

(defn eval-filter [xs]
  (cond (fn? xs) xs        
        (vector? xs) 
           (let [fs (reduce (fn [acc f] (conj acc (eval-filter f))) [] xs)]
             #(ands % fs))
        (nil? xs) nil))

(defn eval-order [xs]
  (cond (or (fn? xs) (keyword? xs))      (ordering  xs)
        (vector? xs)  (apply ordering (reduce (fn [acc f] (conj acc (eval-order f))) [] xs))
        (nil? xs)    nil
        :else (throw (Exception. (str "Unknown ordering expression: " xs)))))

(defn selection? [f]  (get (meta f) :selection))
;;#TODO flesh out the from key.  Maybe it makes sense to define a
;;protocol so we can have tabular queries.  
(defn selection [& {:keys [from where order-by]}]
   (let [filt  (if (fn? where) where (eval-filter where))
         order (if (ordering? order-by) order-by
                   (eval-order order-by))]
     (with-meta (fn [xs] (filt-sort filt order xs))
       {:where filt 
        :order-by order})))

;;#TODO think about composing selections....
(defn select [{:keys [where order-by]} xs]   
  ((selection :where where :order-by order-by) xs))

;;This is where our ordering is falling down.
;;We need to have the option to expose the weight here.
;;By default, all weight is the same.  So, if we alter the preference,
;;it should naturally be accounted for in the sort order.
;;The problem is, we're ignoring it in our order-by clause,
;;only ordering based on the unit information.
;;We could order based on the 
(defn compare-double [^double lw ^double rw]
  (if (== lw rw) 0
      (if (< lw rw) -1
          1)))

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

;;#TODO maybe generalize this further, hide it behind a closure or something?
;;#TODO move this to sim.query or another ns
;;Find all deployable units that match the category "SRC=SRC3"
(defn find-supply [{:keys [src cat order-by where collect-by] :or 
                    {src :any cat :default} :as env} ctx] 
    (let [order-by (eval-order    order-by)
          where    (eval-filter   where)
          t        (core/get-time ctx)] 
      (with-query-env env                                                                           
        (as-> (->> (find-feasible-supply (core/get-supplystore ctx) (core/get-fillmap ctx) 
                                         cat src  (fn [nm]                                                    
                                                    (core/current-entity ctx nm t))) ;;NOTE: Possible updated entity here..
                   (select {:where    (when where   (fn wherf [kv] (where (second kv))))
                            :order-by (when order-by
                                        (ord-fn [^clojure.lang.Indexed l ^clojure.lang.Indexed r]
                                                (let [res (compare-double (.nth ^clojure.lang.Indexed (.nth l 0) 2)
                                                                          (.nth ^clojure.lang.Indexed (.nth r 0) 2))]
                                                  (if (zero? res) ;weights are equal. 
                                                       ;;always compare weight first, by default...                                                    
                                                    (order-by (second l) (second r))
                                                    res))))}))
              res               
             (if collect-by (core/collect collect-by (map second res)) 
                 res)))))


;;What is this doing here?
(defn find-supply! [{:keys [src cat order-by where collect-by] :or 
                    {src :any cat :default} :as env} ctx] 
    (let [order-by (eval-order   order-by)
          where    (eval-filter  where) ] 
      (with-query-env env
        (let [xs       (find-feasible-supply (core/get-supplystore ctx) (core/get-fillmap ctx) cat src)
              _        (println (count xs))
              selected (select {:where    (when where   (fn wherf [kv]    (where (second kv))))
                                :order-by (when order-by (ord-fn [l r] (order-by (second l) (second r))))}
                               xs)]             
          (if collect-by (core/collect collect-by (map second selected)) 
              selected)))))

;;More sophisticated querying API
;;===============================
(defn rule->criteria  [rule] 
  (cond (map? rule) rule
        (string? rule)  {:src rule}
        :else (throw (Exception. "Not sophisticated enough to process " rule))))

;;#TODO - we can incorporate quantity into the rules too...   
;;match-supply could actually incorporate some sophisticated pattern
;;matching logic from core.match, fyi.  Maybe later....We can always extend...
(defn match-supply 
 ([rule constraints features ctx]  
    (find-supply (-> rule (rule->criteria) (assoc :collect-by features) (merge constraints)) ctx))  
 ([rule features ctx]  
  (find-supply (-> rule (rule->criteria) (assoc :collect-by features)) ctx))
 ;;NOTE: this is the typical path we'll take through filling.
 ([rule ctx] (find-supply (rule->criteria rule) ctx)))


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

(def uniform  [when-fenced when-followon max-proportional-dwell])
(def ac-first [when-fenced when-followon AC max-proportional-dwell])
(def rc-first [when-fenced when-followon RC max-proportional-dwell])
(def ng-first [when-fenced when-followon NG max-proportional-dwell])
;(def ar-first [when-fenced when-followon AR max-proportional-dwell])
(def not-ac   #(not= (:component %) "AC"))
(def title32 [#(= (:component %) "NG") min-proportional-dwell])


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
