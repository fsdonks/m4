(ns marathon.ces.query.primitive
  (:require [marathon.ces [unit :as unit]]
            [spork.util.tags :as tag]))

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

(defmacro change-if [default test & body]
  `(if ~test
     ~@body
     ~default))

;;aux helper.
(defn only-keys-from
  "Like select-keys, but bypasses for non-map ks.
   Also, performs more effeciently if ks is a map."
  [ks origin]
  (cond (map? ks)
        (reduce-kv (fn [acc k v] (assoc acc k (get origin k))) {} ks)
        (coll? ks) (select-keys origin ks)
        :else origin))

(defn is?
  ([x y] (or (identical? x y) (= x y)))
                                        ;([x] (fn [y] (is? x y)))
  ([x]  ;(filterfunc. x nil)
        (fn [y] (is? x y)))
   )


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

;;Note the use of not= . This seems counterintutive, but the predicates, used as
;;comparators, means that false values are less than true. So if you want a
;;preference, you need to invert the predicate.
(defn key-pref [k c]
  (if (fn? c)
    (ordering #(c (get % k)))
    (ordering #(not= (get % k) c))))

;;Basic Orderings
;;===============

;;We need to formalize these later; for now they'll serve.
;;#TODO allow users to define suitability queries via scripts or a little
;;language.

;;We have a set of unit comparison criteria that together form a
;;little language for comparing units.

(defn get! [m k]
  (if-let [res (get m k)]
    res
    (throw (Exception. (str [:no-value k m])))))

(defn same-val?
  ([k r1 r2] (= (get! r1 k) (get! r2 k)))
  ([k r1 r2 & rs]
     (let [l (get! r1 k)
           r (get! r2 k)]
       (when (= l r)
         (reduce (fn [acc r]
                   (if (= (get! r k) acc) acc
                       (reduced nil)))
                 l
                 rs)))))

;;For convenience, we define functions that alias the comparer...
(defmacro defcomparison
  "Defines a comparison in ascending order. Derives default ordering functions for
  both ascending and descending values."
  [type nm [l r] & body]
  (let [[hi lo] (case type 
                  :value     ["min"   "max"]
                  :predicate ["not" "when"]
                  (throw (Exception.
                          (str [:unknown-comparison-type type]))))
        hi  (symbol (str hi "-" nm))
        lo  (symbol (str lo "-" nm))]
    `(do (def ~hi (ord-fn [~l ~r]
                 ~@body))
         (def ~lo (ord-fn [~r ~l]
                 ~@body)))))


;;Changed to invert the ordering... In clojure, truth is "greater" than false,
;;thus sorting by a predicate ends up giving us false->true. In practice, we'd
;;like to have true->false, since we're going in ascending order.
(defmacro pred-compare
  ([pred expr]
   ;`(if (~pred ~expr) 1 -1))
   `(if (~pred ~expr) -1 1))
  ([pred]
   ;`(if ~pred 1 -1)))
   `(if ~pred -1 1)))

;;predicate aliases... This is a bit screwy, since we have to flip the order of
;;comparison values for our predicate-based comparisons. Clojure defaults to (<
;;false true), in practice, we have several functions that expect the
;;opposite (more like filter predicate behavior).
(defn is
  "Predicate alias for equality. If x is equal to target, x will sort earlier than
  items that are not equal to target.. 'Truthy means left!'"
  ([x target] (not= x target))
  ([x] (not x)))
(defn is-not
  "Predicate alias for equality. If x is equal to target, x will sort later than
  items that are not equal to target [default clojure logical predicate
  behavior!]"
  ([x target] (= x target))
  ([x] x))

(defn key-compare [k l r]
  (if (keyword? k)
    (compare (get! l k)
             (get! r k))
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
