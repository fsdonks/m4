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
  ([m k amt] (assoc m k (+ (get m k) amt)))
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

(defn- parse-fields
  "Parses a vector of field definitions for defrecord+ .  Where values are
   nested vectors of symbols, ala [fieldname expr], an entry is added to 
   the defaults map under {:fieldname expr}.  Where values are symbols
   expressions, an entry is added under {:fieldname nil}."
  [xs]
  (loop [defaults {}
         fields xs]
  (if (empty? fields) 
    defaults
    (let [f (first fields)
          nextdef 
          (cond (symbol? f) (assoc defaults (keyword f) nil)
                (vector? f) (assoc defaults (keyword (first f))
                                   (second f))
                :else 
                (throw 
                  (Exception. 
                    (str "bad specification in parse-fields:" (str f)))))]
      (recur nextdef (rest fields))))))

(defn- make-constructor
  "Aux for defrecord+. Makes a parameterized constructor for record defined by 
   name and the fieldmap.  If default values specified by the fieldmap, they 
   get used, otherwise fields are nil.  Allows fine-grained control over the 
   creation of records."
  [name fieldmap]
  (let [keymap  (into {} (map (fn [k] 
                                [(symbol (subs (str k) 1)) k]) 
                              (keys fieldmap)))
        defaults (reduce (fn [acc k] 
                           (assoc acc k (get fieldmap (get keymap k)))) {}
                         (keys keymap))
        args {:keys (into [] (keys keymap)) :or defaults}]
    `(defn ~(symbol (str "make-" name)) [& ~args] 
       (~(symbol (str "->" name)) ~@(get args :keys)))))
    
(defn- parse-record-opts
  "Parses a list of opts+specs to extract doc strings, fields, and specs.
   Not currently used....intended to allow documentation support in defrecord+"
  [opts+specs]
  (loop [acc {}
         args opts+specs]
    (if (empty? args) 
      acc
      (let [arg (first args)
            nextacc (cond (string? arg) (assoc acc :doc arg)
                          (vector? arg) (if (contains? acc :fields) 
                                          (assoc acc :specs arg)
                                          (assoc acc :fields arg))
                          :else 
                          (throw (Exception. 
                                   (str "Unknown option in defrecord+ spec: " 
                                        arg))))]
        (recur nextacc (rest args))))))  

(defmacro defrecord+ 
  "Wrapper for defrecord, which follows the Common Lisp tradition of allowing
   inline definition of default fields ala defstruct.  When parsing the record
   definition, if a vector is encountered as a field definition, the vector is 
   parsed as [name defaultvalue].  A make-[recordname] function is created, 
   which takes keys as optional arguements, one for each field.  It might be 
   nice to include type hinting and all that...but i'm not there yet..."
  [name [& fields] & opts+specs]
  (let [rawfields (into [] (map (fn [x] (if (coll? x) (first x) x)) fields))
        default-constructor (make-constructor name (parse-fields fields))]
    `(do
       (defrecord ~name ~rawfields ~@opts+specs)
       ~default-constructor)))
    
    
                  
  
  
  
  
