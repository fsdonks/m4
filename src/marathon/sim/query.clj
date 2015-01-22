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
;;implementation strategy (likely to tag/components) after the
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
  `(binding [~'marathon.sim.query/*env* ~env]
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


;;#TODO move this to sim.query or another ns
;;Find all deployable units that match the category "SRC=SRC3"
(defn find-supply [{:keys [src cat order-by where collect-by] :or 
                    {src :any cat :default} :as env} ctx] 
    (let [order-by (eval-order order-by)
          where    (eval-filter where)] 
      (with-query-env env
        (->> (find-feasible-supply (core/get-supplystore ctx) (core/get-fillmap ctx) cat src)
             (select {:where    (when where   (fn wherf [kv]    (where (second kv))))
                      :order-by (when order-by (ord-fn [l r] (order-by (second l) (second r))))})
             ((fn [xs] (if collect-by (core/collect collect-by (map second xs)) xs)))))))

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

(def uniform  [when-fenced when-followon max-proportional-dwell])
(def ac-first [when-fenced when-followon AC max-proportional-dwell])
(def rc-first [when-fenced when-followon RC max-proportional-dwell])
(def ng-first [when-fenced when-followon NG max-proportional-dwell])
;(def ar-first [when-fenced when-followon AR max-proportional-dwell])
(def not-ac   #(not= (:component %) "AC"))
(def title32 [#(= (:component %) "NG") min-proportional-dwell])


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
