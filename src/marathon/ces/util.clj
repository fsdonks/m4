(ns marathon.ces.util
  (:require [marathon.ces.query.primitive :refer
             [ord-fn ordering ordering?]]
            [spork.util [general :as gen]]))

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


(defn eval-order [xs]
  (cond (or (fn? xs) (keyword? xs))      (ordering  xs)
        (vector? xs)  (apply ordering (reduce (fn [acc f] (conj acc (eval-order f))) [] xs))
        (nil? xs)    nil
        :else (throw (Exception. (str "Unknown ordering expression: "
                                      xs)))))

(alter-var-root #'eval-order gen/memo-1)

(defn eval-filter [xs]
  (cond (fn? xs) xs
        (vector? xs)
           (let [fs (reduce (fn [acc f] (conj acc (eval-filter f))) [] xs)]
             #(ands % fs))
        (nil? xs) nil))

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
