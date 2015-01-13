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
  ([ctx src] (find-feasible-supply (core/get-supplystore ctx) (:fillmap (core/get-fillstore ctx)) src)))


