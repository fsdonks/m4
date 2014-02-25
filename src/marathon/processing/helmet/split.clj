(ns marathon.processing.helmet.split
  (:require [spork.util [sampling :as sample]]))

;;Document this badboy...
;;Patched..due to some complexities in splitting.

(defn add-group-times [splitmap xs & {:keys [fields]
                                      :or  {fields [:DemandSplit :draw-index]}}]
  (let [gtimes (into {} (for [[[splitkey _] recs]
                            (group-by (apply juxt fields) xs)]
                          [splitkey (min (map :start recs))]))]
    (into {}
          (map (fn [[k rec]] [k (assoc rec :earliest-time (get gtimes k))])
               (seq splitmap)))))

(defn split-record
  "Overriding the function from sample/split-record, since it's boffed.  This is 
   simple split function that bifurcates a record at t.  Assumes the record 
   intersects t, returns 2 records."
  [t record & {:keys [separation] :or {separation 0}}] 
  (let [[s d] [(get record :start) (get record :duration)]
        e (+ s d)
        d1 (- t s) 
        s2 t
        d2 (- e t)]
    [(assoc record :duration (- d1 separation))
     (merge record {:start t :duration d2})]))


;;Patched
;helmet post processing....
;we need to process each future using two scripts.
;This is a little bit of a hack, but it's fairly general...
(defn split-by-time
  "Given a sequence of records, xs, scans the records up to the time  t defined 
   by [k :DayRule] in splitmap, applying a \"split\" that logically partitions
   the records into two groups: those that happen before t, and those that 
   happen after t.  Records that happen after t are additionally transformed 
   by an optional function f.  Note -> t is assumed to be a time relative to the
   records xs, such that xs will be split according to the earliest start in xs
   plus t."
  ([f t xs]
    (if (> (:start (first xs)) t)
        (map f xs)
        (loop [status    :scanning
               remaining xs
               acc       []]
        (if (empty? remaining) 
          acc
          (let [x (first remaining)]         
            (cond 
              (and (= status :scanning) 
                   (sample/segment-intersects? (sample/record->segment x) t))
              (let [[l r] (split-record t x)]
                (recur nil (cons r (rest remaining)) (conj acc l)))
              (= status :scanning)
                (recur :scanning (rest remaining) (conj acc x))
              :else (recur status (rest remaining) (conj acc (f x)))))))))
    ([t xs] (split-by-time identity t xs)))
  
(defn title32? [r] (= (int (:Title10_32 r)) 32))

(defn add-start-dur [r] 
  (merge r {:start (get r :StartDay)
            :duration (get r :Duration)}))
(defn drop-start-dur [r]
  (-> (merge r {:StartDay (get r :start) 
                :Duration (get r :duration)})
    (dissoc :start)
    (dissoc :duration)))

;;Patched to account for unique draws and demandgroups.

(defn split-future
  "Given a map of {demandgroup1 {:DayRule x :SourceFirst y},
                   demandgroup2 {:DayRule x :SourceFirst y}}
   Applies the rules defined by the demand group to the records in xs."
  [splitmap xs & {:keys [exclude?] 
                  :or   {exclude? title32?}}]
  (->> (for [[[splitkey _] split-recs] 
             (group-by (juxt :DemandSplit :draw-index) (map add-start-dur xs))]
        (if-let [{:keys [SourceFirst DayRule]} (get splitmap splitkey)]
          (let  [tmin (reduce min (map :start split-recs))
                 recs-by-src (map #(sort-by :start %)
                                 (vals (group-by :SRC split-recs)))]
            (map (fn [recs]                   
                   (let  [{:keys [in out]} 
                          (group-by #(if (exclude? %) :out :in) recs)]
                     (into out 
                           (when in 
                         (split-by-time 
                           #(merge % {:SourceFirst SourceFirst
                                      :Operation  (str (:Operation %) 
                                                       \_ "Rotational")}) 
                           (+ tmin DayRule) in)))))   recs-by-src))
           split-recs))
    (flatten)
    (map drop-start-dur)))
