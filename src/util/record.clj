(ns util.record)
;stolen from stack overflow
(defn static? [field]
  (java.lang.reflect.Modifier/isStatic
   (.getModifiers field)))

(defn get-record-field-names [record]
  (->> record
       .getDeclaredFields
       (remove static?)
       (map #(.getName %))
       (remove #{"__meta" "__extmap"})))

(defmacro empty-record [record]
  (let [klass (Class/forName (name record))
        field-count (count (get-record-field-names klass))]
    `(new ~klass ~@(repeat field-count nil))))

;(defn replace [record fld v] (merge record {fld v}))

(defn inc-field 
  ([m k amt] (merge m {k (+ (k m) amt)}))
  ([m k] (inc-field m k 1)))

(defn dec-field 
  ([m k amt] (inc-field m k (* amt -1)))
  ([m k] (inc-field m k -1)))

(defn get-vals [m ks]
  (map (partial get m) ks))

(defn get-path 
  ([m k] (get m k)) 
  ([m k & ks] (let [res (get m k)]
                (if ks
                  (recur res (first ks) (next ks))
                  res))))

(defn assoc-many
  "Initialize a record with default value v.  If fields are specified, only 
   the fields received the default"
  ([rec v flds] (merge rec (zipmap flds (repeat v))))
  ([rec v] (assoc-many rec v (keys rec))))


(defn transfer 
  "Transfer key/value pair associated with k from record r1 to 
   records in rs.  The result of the donor record is held in metadata."
  ([r1 k r2] (with-meta [(assoc r2 k (get r1 k))] {:from (dissoc r1 k)}))
  ([r1 k r2 & rs] (let [v (get r1 k)
                        init (transfer r1 k r2)] 
                    (reduce #(conj %1 (assoc %2 k v)) init  rs))))

(defn trans 
  "Transfer key/value pair associated with k from record r1 to 
   records in rs.  The result of the donor record is held in metadata."
  ([r1 k r2] [(dissoc r1 k) (assoc r2 k (get r1 k))])
  ([r1 k r2 & rs] (let [v (get r1 k)
                        init (trans r1 k r2)] 
                    (reduce #(conj %1 (assoc %2 k v)) init  rs))))
  

