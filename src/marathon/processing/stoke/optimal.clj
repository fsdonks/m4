(ns marathon.processing.stoke.optimal
  (:require [marathon.processing.stoke [core :as core]]))

;;Filling Approximately Optimally
;;===============================

;;Useful functions for optimizing
(defn sum [xs] (reduce + xs))     
(defn weighted-sum [key-vals key->weight]
  (reduce (fn [acc [k v]] (+ acc (* v (key->weight k)))) 0 key-vals))
  
(defn sum-keys-by
  "Given a map of {k quantity}, where quantity is a number, sums the map using 
   a custom key function.  Used to aggregate maps."
  [keyf m]
  (->> (seq m)
       (reduce (fn [acc [k qty]] (assoc! acc (+ (get acc (keyf k) 0) qty))) 
               (transient {}))
       (persistent!)))

;;Generating supply is fairly straightfoward.  Since we're using clojure's data
;;structure, we can share a lot of data instead of having to put it into array 
;;based structures and shovel indices around.  

;;We formulate a simple optimization, Given:      
;;srcs   - a set of srcs,  
;;compos - a set of components,  
;;peaks  - a map of {src demand},  
;;compo-pref - a map of {demand compo},
;;priority   - a positive real number
;;demand-priority - a map of {demand priority}
;;src-strength - a map of {src strength},  
;;max-end-strength - the maximum size of the supply, 

;;with the following decision variables: 
;;  
;;The quantity of supply assigned by demand, component, and src:  
;;supplied =  {[demand compo src] quantity}  
;;total-fill = {src (sum (supplied [demand compo src])}
;;size-of-supply =     
;;  (sum (for [[src qty] total-fill] (* (src-strength src) qty))) 
       
;;with the following constraints:  
;;  size-of-supply <= max-end-strength   
;;  We'll implement this constraint via a severe penality on the objective 
;   function for solutions that are over-strength. 
;;  
;;the following weight functions:  
;;  (fill-weight qty src) = (* qty (src-strength src) (demand-priority src))  
;;  (compo-weight demand compo) = (if (= (get compo-pref demand) compo) 1 0.5)
;;and the following objectives:  
;;  (weighted-fill supplied) = 
;;       (sum (for [[[demand compo src] qty] supplied] 
;;                (+ (fill-weight qty src) (compo-weight demand compo))))
;;maximize (* (weighted-fill supplied) (strength-penalty supplied))

;;Translated (directly) into a value function:
(defn ->value-function
  "We maximize the weighted fill of the supply, relative to the demand, demand 
   preferences, and strengths, and penalize solutions that are over-strength 
   by dividing the weighted-fill by the amount over strength.  Solutions that 
   are within strength bounds suffer no penalty."
  [max-end-strength compo->pref demand->priority src->strength]
  (let [total-fill (fn [supplied] (sum-keys-by (fn [[_ _ src]] src)  supplied))
        size-of-supply (fn [tot-fill] (weighted-sum tot-fill src->strength))
        fill-weight    (fn [qty src] (* qty (src->strength src) 
                                            (demand->priority src)))
        compo-weight   (fn [demand compo] 
                         (if (= (compo->pref demand) compo) 1 0.5))        
        weighted-fill  (fn [supplied] 
                         (sum (for [[[demand compo src] qty] supplied] 
                                (+ (fill-weight qty src) 
                                   (compo-weight demand compo)))))
        strength-penality (fn [size] (/ 1.0 (min (- size max-end-strength) 1.0)))]
    (fn [supplied] (* (weighted-fill supplied) 
                      (strength-penality (size-of-supply supplied))))))

;;We wrap everything up into a nice function that lets us build the optimization
;;en toto.  Given a demand future, and an empty supply, we construct a 
;;specification for an optimization problem that will generate a supply.




(comment 
(defn unkey [k] 
  (if (keyword? k)
    (subs (str k) 1)
    k))

(defn supply-spec [srcs compos]
  (let [combos (for [s srcs
                     c compos] (symbol (str (unkey s) "_"  (unkey c))))] 
    (reduce conj {}  
       (for [k combos]
         [k [0 ludicrous-amount]]))))

(defn supply-solution [srcs compos]
  (let [spec (supply-spec srcs compos)]   
    (eval `(defsolution ~'supply ~spec))))  
)