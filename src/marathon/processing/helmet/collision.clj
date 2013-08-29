;This is a small script for handling a set of business rules that define 
;classes of collisions between data records.  In retrospect, a better solution
;would be to define classes of collisions via some pattern construct, and then
;add to the patterns as needed.  Anyway, this guy works for now..
(ns marathon.processing.helmet.collision)

(def ^:dynamic *log-collisions* nil) 

(defn start-time [record] (:StartDay record))
(defn end-time [record]   (+ (:StartDay record) (:Duration record)))
(defn duration [record]   (:Duration record))
(defn record->segment [r] [(start-time r) (end-time r)])
;assumes record2 has bigger start time than record1
(defn near? [space record1 record2]
  (< (- (:StartDay record2) (end-time record1)) space))

;assumes record2 has bigger start time than record1
;Need to check with Josh over what Overlap means.
(defn overlap? [record1 record2]   (< (start-time record2) (end-time record1)))  
;assumes record2 has bigger start time than record1
(defn contained? [record1 record2] (< (end-time record2) (end-time record1)))
(defn priority-same? [record1 record2]
  (= (:Priority record1) (:Priority record2)))
(defn priority-greater? [record1 record2]
  (< (:Priority record1) (:Priority record2)))

;added from patch 
(defn higher-priority [l r] (< (:Priority l) (:Priority r)))
(defn adjacent? [l r] (= (end-time l) (start-time r)))


(defn- log-fix [cause in result]
  (println (str "Resolved collision " cause \newline " In: " in 
                    \newline " Out: " result)))

(defmacro with-response [msg in & body]
  `(let [res# ~@body]
     (do (if *log-collisions*      
           (log-fix ~msg ~in res#))
       res#)))

;assumes left subsumes right.
(defn merge-records [l r]
  (with-response :simple-merge [l r]
    [(assoc l :Duration (- (max (end-time r) (end-time l)) (start-time l)))]))

;assumes left contains right, returns 3 records.
(defn fix-contained [l r]
  (if (priority-greater? l r) 
    ; return l if it has higher priority and envelops r
    (with-response :left-envelops-right [l] [l])
    (with-response :right-partitions-left [l r]
      [(assoc l :Duration (- (start-time r) (start-time l)))
       r
       (merge l {:StartDay (end-time r)
                 :Duration (- (end-time l) (end-time r))})])))

;fix two overlapping records, depending on their priority.
(defn fix-overlapped
  [l r]
  (let [[pl pr] (map :Priority [l r])]
        (if (< pr pl) ; if the second record has higher priority
              ;truncate end of first record where the second record starts
          (with-response :right-truncates-left [l r]
            [(assoc l :Duration (- (start-time r) (start-time l))) r])
          ;else if the first record has higher priority, truncate to
          (with-response :left-truncates-right [l r]
            [l (merge r {:StartDay    (end-time l)
                         :Duration (- (end-time r) (end-time l))})]))))

;;New function to fix a third case of collisions.  Accounts for stretch cases.
(defn fix-stretch [l r]
  (when (not (adjacent? l r))
    (if (higher-priority r l)
      (with-response :left-stretches-to-right [l r]
        [(assoc l :Duration (- (start-time r) (start-time l))) r])
      (with-response :right-stretches-to-left  [l r]
        [l (assoc r :StartDay (end-time l))]))))

;;Patched due to a missing case, for records that need to be stretched, we were
;;failing to perform any operation. 

;fix-collision::record -> record -> [record]
(defn fix-collision [tmin l r]
  (if (not (near? tmin l r))
    (throw (Exception. "Nothing to fix!"))
    ; if two records are within minimum-space of each other
    ;if the two records have same priority
    (cond (priority-same? l r) (merge-records l r)
          (contained? l r)     (fix-contained l r)
          (overlap? l r)       (fix-overlapped l r)
          :else                (fix-stretch l r)))) ;new case

;should-fix?::record -> record -> boolean
;fix::record -> record -> [record]
;If fix returns nil, we assume that no fixes were
;necessary, i.e. 1 and r were already clean.
(defn fix-records [should-fix? fix xs]
  (loop [remaining xs
         clean []]
    (cond (empty? remaining) clean
      (= (count remaining) 1) (into clean remaining)
      :else (let [l (first remaining)
                  r (second remaining)]
              (if (should-fix? l r)
                (if-let [fixed (fix l r)] ;check iff fix actually does something.
                  (recur (into fixed (drop 2 remaining)) clean) ;process fixed
                  (recur (rest remaining) (conj clean l)));if fixed is nil advance.
                (recur (rest remaining) (conj clean l))))))) ; advance.

(defn work-groups [classes xs]
  (let [groups (group-by :DependencyClass xs)]
    (reduce (fn [acc [k v] ]
              (let [vs (get acc k {})]
                (assoc acc k (concat vs v)))) {}
            (map first (for [[k v] groups]
                         (if (contains? classes k)
                           {:collides v}
                           {:static v}))))))
(defn fix-group [space xs]
  (fix-records (partial near? space)
               (partial fix-collision space) (sort-by start-time xs)))

(defn prioritize-groups [classes xs]
  (->> (for [[group-key recs] (group-by :DependencyClass xs)]
         (let [class-rec (get classes group-key)               
               p         (:Priority class-rec)
               space     (:MinTime class-rec)]
           (assert (and p space)
                   (str "Could not find Priority or MinTime fields for "
                        group-key))
           {:Priority p
            :space    space
            :records  (fix-group space (map #(assoc % :Priority p) recs))}))
    (sort-by :Priority)))

(defn merge-groups
  ([left-group right-group]
    (if (every? (complement empty?) [left-group right-group])
      (let [space (min (:space left-group)
                       (:space right-group))
            ls (:records left-group)
            rs (:records right-group)]
        {:space space :records (fix-group space (concat ls rs))})
      (let [g (if (nil? left-group) left-group right-group)
            space (:space g)]
        {:space space :records (fix-group space (:records g))}))))

;let process-collisions be a function::map -> record sequence -> record sequence
;where, given a map of collision classes, and a record sequence, a new sequence
;of records is returned that resolves any collisions existing in the input
;record sequence, according the data derived from the collision-class map.
(defn process-collisions-sub
  [classes xs]
  (:records (reduce merge-groups (prioritize-groups classes xs))))

(defn fields->key [fields r]  (apply str (map #(get r %) fields)))

(defn process-collisions
  [classes xs & {:keys [src log? key-fields]
                 :or {src nil 
                      log? nil 
                      key-fields [:SRC :Title10_32]}}]
  (binding [*log-collisions* log?]
	  (let [f (if src
	            #(filter (fn [r] (= (:SRC r) src)) %)
	            identity)
	        {:keys [collides static]} (work-groups classes (f xs))]
	    (->> collides
	      (group-by (partial fields->key key-fields))
	      (vals)        
	      (map (partial process-collisions-sub classes))       
	      (concat)
	      (flatten)        
	      (into static)))))

;(defn read-recs [somefile]
;  (read-string (slurp somefile)))


  
  