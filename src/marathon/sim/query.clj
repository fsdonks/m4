;;A place for defining queries on simulation state.  Serves as a
;;useful hub for defining complicated queries.  May also eventually 
;;extend spork.entitystore protocols to marathon stores.
(ns marathon.sim.query
  (:require [marathon.sim.fill.demand   :as fill]
            [marathon.sim  [core :as core]
                           [supply :as supply]
                           [demand :as demand]
                           [unit :as unit]
                           [policy :as policy]
                           [policyio :as policyio]
                           [sampledata :as sd]
                           [entityfactory :as ent]
                           [setup :as setup]
                           [engine :as engine]]                        
            [marathon.data [simstate :as simstate]
                           [protocols :as generic]]
            [spork.sim     [simcontext :as sim]]
            [spork.entitysystem [store :as estore]]
            [spork.util.reducers]
            [clojure.core [reducers :as r]]))

(defn find-deployable-supply  [supply src] (keys (get (supply/get-buckets supply) src)))
(def  src->fillrule (memoize (fn [src] 
                               (marathon.sim.fill.fillgraph/sink-label src))))

;;Provides an ordered vector of suitable supply buckets to look.
(defn src->srcs [srcmap src] 
  (->> (for [[rule cost] (get srcmap (src->fillrule src))]
             [(marathon.sim.fill.fillgraph/source-root rule) cost]) 
       (sort-by second)
       (mapv first)))

(def srcs->prefs (memoize (fn [srcs]   
                              (into {} (map-indexed (fn [idx src] [src idx]) srcs)))))
(defn src->prefs [srcmap src]  (srcs->prefs (src->srcs srcmap src)))

(defmacro change-if [default test & body]
  `(if ~test
     ~@body
     ~default))

;;Reducer/seq that provides an abstraction layer for implementing 
;;queries over deployable supply.  I really wish I had more time 
;;to hack out a better macro for the reducers, but this works for now.
(defn ->deployers [supply & {:keys [cat src unit] :or {cat  identity 
                                                       src  identity 
                                                       unit identity}}]
  (let [catfilter cat
        srcfilter src 
        unitfilter unit]
    (reify     
      clojure.lang.Seqable 
      (seq [this]  
        (for [[cat srcs]    (:deployable-buckets supply)
              [src units]   srcs
              [nm u]        units
              :when (and (catfilter cat) (srcfilter src) (unitfilter u))]
          [[cat src] u]))
      clojure.core.protocols/CollReduce
      (coll-reduce [this f1]        
        (reduce-kv (fn [acc cat srcs]
                     (change-if acc (catfilter cat)
                                (reduce-kv (fn [acc src units]
                                             (change-if acc (srcfilter src)
                                                        (reduce-kv (fn [acc nm unit]
                                                                     (change-if units (unitfilter unit)
                                                                                (f1 acc [[cat src] unit]))) acc units)))
                                           acc srcs))) (f1) (:deployable-buckets supply)))
      (coll-reduce [_ f1 init]      
        (reduce-kv (fn [acc cat srcs]
                     (change-if acc (catfilter cat)
                                (reduce-kv (fn [acc src units]
                                             (change-if acc (srcfilter src)
                                                        (reduce-kv (fn [acc nm unit]
                                                                     (change-if units (unitfilter unit)
                                                                                (f1 acc [[cat src] unit]))) acc units)))
                                           acc srcs))) init (:deployable-buckets supply))))))
(defn is? 
  ([x y] (or (identical? x y) (= x y)))
  ([x] (fn [y] (is? x y))))


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
;;implementation strategy (likely to tags/components) after the
;;fact...Also, they're primitive supply for our more general fill
;;query language...

;;#TODO supplement this with supply queries, so we can change the
;;sort-order, etc.  allow caller to provide custom sort
;;function...this is pretty huge.  From that, we can build all kinds
;;of queries.  I inlined the sorting so that the initial supply is
;;ordered by preference.  We may want to do this lazily, since we
;;may not need all the supply.  Possible performance optimization.
(defn find-feasible-supply 
  ([supply srcmap category src]
   (if (is? category :any)
     (->deployers supply :src (is? src))
     (let [prefs (src->prefs  srcmap src)]
       (->>  (->deployers supply :src #(contains? prefs %) :cat (is? category))
             (into [])
             (sort-by  (fn [[ k v]]   (get prefs (second k))))))))
  ([supply srcmap src] (find-feasible-supply supply srcmap :default src))
  ([ctx src] (find-feasible-supply (core/get-supplystore ctx) (:fillmap (core/get-fillstore ctx)) :default src)))


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

(def MinDwell    (ordering unit/normalized-dwell))
(def MaxDwell    (flip MinDwell))
(def compo-pref #(key-pref :component %))
(def where-compo  compo-pref)
(def except-compo (comp flip compo-pref))

(def components ["AC" "RC" "NG" "RCAD" "RCAD-BIG"])
;;Emit preferences for standard component values.
(doseq [c components]
  (eval `(def ~(symbol c) (compo-pref ~c))))

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

;; (defmacro predicate [nm k] 
;;   `(defcomparison :predicate ~nm [~l ~r] ~@body))                   
  
;;Basic value-based orderings.  These provide numbers we can easily
;;compare on.
(key-valuations cycletime  :cycletime
                bog-budget unit/bog-budget
                dwell  unit/get-dwell
                bog     unit/get-bog 
                proportional-dwell unit/normalized-dwell
                relative-cycletime (fn [u] (float (/ (:cycletime u) (unit/get-cyclelength u)))))

;;predicates...

;;whereCompo
;;whereNotCompo


;;Environmental queries
;;=====================

;;followon 

(predicate followon  [l r] 
   (if (same-val? get l r) 0
       (pred-compare (same-val? get l *env*))))

;;FencedTo
;;NotFencedTo

;;tag 
;;not-tag

;;ANDTags
;;NANDTags

;;ORTags
;;NORTags

;;(defcomparer initial-demand [[AC MaxDwell]
;;                             [RCAD MaxDwell]
;;                             Generate-AC
;;                             Generate-RCAD])


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
