(ns dep2)

;Let input be a vector of records, where a “record” is a simple clojure map, 
;in which every record in the vector has the same set of fields:
 ( comment [ (def input
  [{:name "Record1" :start 2   :duration 10 :demand-class nil :priority 3} 
   {:name "Record2" :start 5   :duration 10 :demand-class nil :priority 3}
   {:name "Record3" :start 10  :duration 10 :demand-class "Alpha" :priority 1}
   {:name "Record4" :start 15  :duration 7  :demand-class "Alpha" :priority 1}
   {:name "Record5" :start 20  :duration 30 :demand-class "Beta" :priority 2}
   {:name "Record6" :start 100 :duration 10 :demand-class "Alpha" :priority 1}
   {:name "Record7" :start 120 :duration 10 :demand-class "Alpha" :priority 1}])
       ])    

 (def input
  [ {:name "Record1" :start 1   :duration 5 :demand-class nil :priority 3} 
   {:name "Record2" :start 7   :duration 3  :demand-class nil :priority 3}
   {:name "Record3" :start 15  :duration 17 :demand-class "Alpha" :priority 1}
   {:name "Record4" :start 40  :duration 8  :demand-class "Alpha" :priority 1}
   {:name "Record5" :start 45  :duration 10 :demand-class "Beta" :priority 2}
   {:name "Record6" :start 63 :duration 10  :demand-class "Alpha" :priority 1}
   {:name "Record7" :start 80 :duration 1   :demand-class "Alpha" :priority 1}])

;let collision-classes be a map that associates values for demand-class fields to 
;data used to process records for collisions.
;Any record with a non-nil value associated with the key :demand-class 
;can cause a collision.  classes provides information on how to resolve 
;said collisions.
(def collision-classes 
  {"Alpha" {:priority 1  :minimum-space 5}
   "Beta"  {:priority 2  :minimum-space 5}})

(defn start-time [record] (:start record))
(defn end-time   [record] (+ (:start record) (:duration record)))
(defn duration   [record] (:dur record))
(defn record->segment [r] [(start-time r) (end-time r)])

(defn sort-earliest [events] (sort-by start-time events))

;assumes record2 has bigger start time than record1
(defn near? [space record1 record2] 
  (< (- (:start record2) (end-time record1)) space))
;assumes record2 has bigger start time than record1
;Need to check with Josh over what Overlap means.
(defn overlap? [record1 record2] 
  (< (start-time record2) (end-time record1)))
;assumes record2 has bigger start time than record1
(defn contained? [record1 record2]  
  (< (end-time record2) (end-time record1))) 

(defn priority-same? [record1 record2]
  (= (:priority record1) (:priority record2))) 

(defn priority-greater? [record1 record2]
  (> (:priority record1) (:priority record2))) 
  
;assumes left subsumes right.
(defn merge-records [l r]  
  [(assoc l :duration (- (max (end-time r) (end-time l)) (start-time l)))])
  

;assumes left contains right, returns 3 records.
(defn fix-contained [l r]
  (if (priority-greater? l r)
    [l]; return l if it has higher priority and envelops r
    [(assoc l :duration (- (start-time r) (start-time l))) 
     r 
     (merge l {:start    (end-time r) 
               :duration (- (end-time l) (end-time r))})]))
;fix two overlapping records, depending on their priority.
(defn fix-overlapped 
  [l r]
  (let [[pl pr] (map :priority [l r])]
    (if (> pr pl) ; if the second record has higher priority
     ;truncate end of first record  where the second record starts
      [(assoc l :duration (- (start-time r) (start-time l))) r]  
      ;else if the first record has higher priority, truncate to start2    
      [l (merge r {:start    (end-time l)
                   :duration (- (end-time r) (end-time l))})]))) 

;fix-collision::record -> record -> [record]     
(defn fix-collision [tmin l r]  
  (if (not (near? tmin l  r))  
    (throw (Exception. "Nothing to fix!"))
   ; if two records are within minimum-space of each other
    ;if the two records have same priority
    (cond (priority-same? l r)  (merge-records l r) 
          (contained? l r)      (fix-contained l r)
          (overlap? l r)        (fix-overlapped l r))))

;should-fix?::record -> record -> boolean 
;fix::record -> record -> [record]
;If fix returns nil, we assume  that no fixes were 
;necessary, i.e. l and r were already clean.
(defn fix-records [should-fix? fix xs]
  (loop [remaining xs 
         clean []]
    (cond (empty? remaining) clean
      (=  (count remaining) 1) (into clean remaining)
      :else (let [l (first remaining)
                  r (second remaining)]
              (if (should-fix? l r)
                (if-let [fixed (fix l r)] ;check iff fix actually does something.  
                  (recur (into fixed (drop 2 remaining)) clean) ;process fixed
                  (recur (rest remaining) (conj clean l)));if fixed is nil advance.
                (recur (rest remaining) (conj clean l))))))) ;advance.
      
(defn work-groups [classes xs]
  (let [groups (group-by :demand-class xs)]
    (reduce (fn [acc [k v]] 
              (let [vs (get acc k {})]
                (assoc acc k (concat vs v)))) {}    
                (map first  (for [[k v] groups]
                              (if (contains? collision-classes k)
                                {:collides v}
                                {:static v}))))))
(defn fix-group [space xs]
  (fix-records (partial near? space) 
               (partial fix-collision space) (sort-earliest xs)))

(defn prioritize-groups [classes xs]
  (->> (for [[group-key recs] (group-by :demand-class xs)]
         (let [class-rec (get classes group-key)
               p       (:priority class-rec)
               space (:minimum-space class-rec)]
           {:priority p 
            :space space
            :records (fix-group space (map #(assoc % :priority p) recs))}))
       (sort-by :priority)))

(defn merge-groups [left-group right-group]
  (if (every? (complement empty?) [left-group right-group])
    (let [space (min (:space left-group)
                     (:space right-group))
          ls (:records left-group)
          rs (:records right-group)]
      {:space space :records (fix-group space (concat ls rs))})
    (let [g (if (nil? left-group) left-group right-group)
          space (:space g)]
      {:space space :records (fix-group space (:records g))})))


;let process-collisions be a function::map -> record sequence -> record sequence 
;where, given a map of collision classes, and a record sequence, a new sequence 
;of records is returned that resolves any collisions existing in the input 
;record sequence, according the data derived from the collision-class map.
(defn process-collisions 
  [classes xs] 
  (let [{:keys [collides static]} (work-groups classes xs)]
   (->> (reduce merge-groups (prioritize-groups classes collides))
        (:records)
        (concat static)))) 