(ns marathon.processing.helmet.split
  (:require [spork.util [sampling :as sample]]))

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
    (let [recs    (sort-by :start xs)          
          t-start (:start (first recs))
          t0      (+ t-start t)]
      (loop [status :scanning
             remaining recs
             acc [] ]
        (if (empty? remaining) acc
          (let [x (first remaining)]         
            (cond 
              (and (= status :scanning) 
                   (sample/segment-intersects? (sample/record->segment x) t0))
              (let [[l r] (split-record t0 x)]
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

(defn split-future
  "Given a map of {demandgroup1 {:DayRule x :SourceFirst y},
                   demandgroup2 {:DayRule x :SourceFirst y}}
   Applies the rules defined by the demand group to the records in xs."
  [splitmap xs & {:keys [exclude?] 
                  :or   {exclude? title32?}}]
  (let [grouped (->> xs 
                  (map add-start-dur) 
                  (group-by :DemandGroup))]
    (->>
      (for [[groupkey recs] grouped]
        (if-let [{:keys [SourceFirst DayRule]} (get splitmap groupkey)]
          (let  [{:keys [in out]} (group-by #(if (exclude? %) :out :in) recs)]
            (into out 
              (when in 
                (split-by-time #(merge % {:SourceFirst SourceFirst
                                          :Operation  (str (:Operation %) 
                                                           \_ "Rotational")}) 
                               DayRule in))))
          recs))
      (flatten)
      (map drop-start-dur))))
