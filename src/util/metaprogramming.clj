;A collection of macros and functions useful for metaprogramming and library
;development. 
(ns util.metaprogramming)

(defmacro defmany
  "Define multiple definitions inline.  Takes a collection of bindings, and 
   converts them into def expressions."
  [bindings]
    `(do ~@(->> (partition 2 bindings)
             (map (fn [b] `(def ~(first b) ~(second b)))))))

(defn keyvals->constants
  "Given a map of {symbol val} or {:symbol val}, 
   creates def bindings for each symbol."
  [m]
  (let [as-symbol (fn [k] 
                    (cond (symbol? k) k
                          (keyword? k) (symbol (subs (str k) 1))
                          :else (throw (Exception. "unknown symbol type"))))]
    (eval `(defmany ~(flatten (for [[k v] m] [(as-symbol k) v]))))))
