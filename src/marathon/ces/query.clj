;;A place for defining queries on simulation state.  Serves as a
;;useful hub for defining complicated queries.  May also eventually 
;;extend spork.entitystore protocols to marathon stores.
;;Currently looking into how we can either supplement or
;;replace this with something based on datascript/datomic
;;querying.
(ns marathon.ces.query
  (:require [marathon.ces  [core   :as core]
                           [unit   :as unit]
                           [supply :as supply]
                           [rules :as rules]]
            [marathon.ces.fill.fillgraph]
            [marathon.ces.query.primitive :refer [ord-fn ordering ordering?]]
            [spork.util [general :as gen] [metaprogramming :as m]]))

;;convenience imports and exports
(defn known-rules []
  (for [[s v] (ns-publics 'marathon.ces.rules)
        :let [res (deref v)]
        :when (or (vector? res)  (ordering? res))]
    s))

;;pull in any rules defined in marathon.ces.rules
(eval `(m/import-vars
        [marathon.ces.rules
         ~@(known-rules)
         ~'stock-queries
         ~'register-rule!]))

;;TODO -> make this a bit higher level, provide an API for defining
;;environmental queries and specifying which vars in an expression are drawn
;;from the environment (i.e. the state monad....)
(defmacro with-query-env [env & expr]
  `(binding [~'marathon.ces.rules/*env* ~env]
     ~@expr))

;;Query parsing combinators
;;=========================

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
(alter-var-root #'eval-order gen/memo-1)

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

;;This is where our ordering is falling down. We need to have the option to
;;expose the weight here. By default, all weight is the same. So, if we alter
;;the preference, it should naturally be accounted for in the sort order. The
;;problem is, we're ignoring it in our order-by clause, only ordering based on
;;the unit information. We could order based on the [?]
(defn compare-double [^double lw ^double rw]
  (if (== lw rw) 0
      (if (< lw rw) -1
          1)))

;;REMOVE?
(defn ->preference-orderer [order-by]
  (ord-fn [^clojure.lang.Indexed l ^clojure.lang.Indexed r]
          (let [res (compare-double (.nth ^clojure.lang.Indexed (.nth l 0) 2)
                                    (.nth ^clojure.lang.Indexed (.nth r 0) 2))]
            (if (zero? res) ;weights are equal.
              ;;always compare weight first, by default...
              (order-by (second l) (second r))
              res))))

(defn compare-nth
  "Given an index into two values, l and r that satisfiy 
   clojure.lang.Indexed, applies the comparison function f
   on the values at index n.  Optionally, caller may elide 
   the comparison function in lieu of clojure.core/compare"
  ([^long n f ^clojure.lang.Indexed l ^clojure.lang.Indexed r]
   (f (.nth  l n)
      (.nth  r n)))
  ([n l r] (compare-nth n compare l r)))


(defn ->kv-ordering
  "Refactoring of the inner function originally defined in find-supply.
   Given two comparers, one for keys and one for values,, operates on a sequence 
   of pairs:: seq [k v], assuming that keys take precedence over values."
  [key-compare val-compare]
  (ord-fn [^clojure.lang.Indexed l ^clojure.lang.Indexed r]
          (let [res (key-compare  (.nth l 0)
                                  (.nth r 0))]
            (if (zero? res) ;weights are equal. 
              ;;always compare weight first, by default...
              (val-compare (.nth l 1) (.nth r 1))
              res))))

;;Intended to compare [category src weight], convenience fn. 
(def compare-fill-weight #(compare-nth 2 compare-double %1 %2))

(defn ->ordering
  "Refactoring of the inner function originally defined in
  find-supply.  Given an ordering criteria, order-by:: v -> v,
  operates on a sequence of pairs:: seq [[category src score] v], where a default
  key-comparer is supplied."
  [order-by]
  (->kv-ordering compare-fill-weight order-by))

;;#TODO maybe generalize this further, hide it behind a closure or something?
;;#TODO move this to sim.query or another ns
;;Find all deployable units that match the category "SRC=SRC3"
(defn find-supply [{:keys [src cat order-by where collect-by] :or
                    {src :any cat :default} :as env} ctx]
    (let [order-by (eval-order    order-by)
          where    (eval-filter   where)
          t        (core/get-time ctx)]
      (with-query-env env
        (as-> (->> (rules/find-feasible-supply (rules/compute-supply env ctx) (core/get-fillmap ctx)
                      cat src  (fn [nm]
                                 (core/current-entity ctx nm t))) ;;NOTE: Possible updated entity here..
                   (select {:where    (when where   (fn wherf [kv] (where (second kv))))
                            :order-by (when order-by (->ordering order-by))}))
              res
             (if collect-by (core/collect collect-by (map second res))
                 res)))))

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



