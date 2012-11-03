(ns entitysystem.store)
  ;(:use [DEVS.Schedule]))

(comment 
"A component-based architecture is a collection of Domains, Systems, Components,
and entities.  

The central idea behind the architecture is to find a way to decompose the 
traditional hierarchical, intermixed data and functionality present in many 
object-oriented designs along orthogonal, domain-specific lines.  
In the existing simulation framework for Marathon, entities are instances of 
unitdata or demanddata classes.  They have a significant amount of 
functionality associated with (and stuck IN) the class.  There is a sense of 
modularity in the sense that unit policy, behavior, etc. are also objects with 
independent functions.  However, the design suffers from extensibility due to
the encapsulation of domain-specific data and functionality, particularly where 
more than one domain may be accessed.   We have to define the relation between 
unitdata and policy explicitly in the unitdata class…indiciating that all 
unitdata objects (and hence unit entities) must have a policy.  We also see 
data duplication due to encapsulation inside of classes.  Encapsulation also 
limits the visibility of state data (by design) unless everything is made 
public.  No longer can we push around collections of primitive data, we must 
access public members of a class instance (even if we just need a simple 
container for the class data).  As a result, our notion of classes will become 
harder to maintain and extend over time….even with less-primitive OOP 
facilities than in VBA, developing a deep inheritance hierarchy in an OOP 
system inexorably leads to an inflexible and unwieldy design.

A domain is an orthogonal aspect of the simulated environment upon which 
reality is partitioned and modeled.  To describe a component-based architecture, 
we probably need to know what components are.   We’ll do a bit of mutual 
recursion, and rely on another definition to give insight into what a component 
is.  Components are derived from Domains….so what’s a Domain? Domains are the 
orthogonal aspects of a simulated environment upon which the environment is 
partitioned and modeled.   In other words, domains represent clear boundaries 
between functionality, and enable us to abstract a complicated, tangled 
environment into a set of unique domains with clearly defined operations for
each domain, and across said domains.  Domains represent our understanding of
the unique properties of the environment. 

A component is a collection of functionally-similar, or domain-specific data 
(traditionally not functions or methods!)  Strictly speaking, components are 
domain-specific data.   Components derive their meaning from domains because 
they represent an implicit encoding of domain-specific information.  We can 
infer from the set of components, which domains are covered by the environment, 
without having to explicitly state the domains.  This allows for a grass-roots 
approach to design, in which domains are defined in small pieces, and composed.  
The smaller and more specific the component, the more obvious the domain 
context, and the easier it is to factor out duplicate functionality into truly 
unique data (which reinforces our desire to maintain orthogonal domains)."

)


(defprotocol IEntityStore
  "The entitystore is an abstract protocol for defining functionality necessary 
   to manage entities, as defined by collections of component data.  Due to the 
   independent nature of component data (each component maps 1:1 to a specific, 
   orthogonal domain of information, and is self contained), our entity store 
   could be implemented a number of ways.  The obvious implementation is as a 
   single hash-map database on a single machine.  However, we could easily 
   distribute the component maps across multiple nodes, effectively distributing
   our store and allowing for parallel querying/updating functions.  Finally, 
   we could implement the entitystore using a persistent database backend, if it
   makes sense to do that. "
  (has-entity? [db id] "existence check for entities")
  (get-keys [db] "returns a set of all entity keys")
  (get-parts [db id] "derive components id contains")
  (add-record [db id component data] "Associate a record of data /w entity id")
  (drop-record [db id component] "Drop record for id from component")
  (get-record [db id component] "Fetch the component record for ent id")
  (get-entities [db] "Return a seq of [entityid #{components..}]")
  (get-component [db component] "Return the data for component")
  (get-components [db] "Return all components")
  (get-domains [db] "Return a set of all domains in the store"))

;EntityStore is the default implementation of our protocol, and it uses maps 
;to maintain records of component data, keyed by entity ID, as well as a map of
;entities to the set of components they are associated with.  
(defrecord EntityStore [entities components]
  IEntityStore
  (has-entity? [db id] (contains? entities id))
  (get-keys [db] (keys entities))
  (get-parts [db id] (get entities id #{}))
  (add-record [db id component data]
    (EntityStore. (assoc entities id (conj (get entities id #{}) component)) 
                  (assoc components component
                     (merge (get components component {}) {id data}))))
  (drop-record [db id component]
    (let [cnext (dissoc components component id)
          enext (let [parts (disj (get-parts db id) component)]
                      (if (zero? (count parts))
                        (dissoc entities id)
                        (assoc entities id parts)))]
      (EntityStore. enext cnext)))
  (get-record [db id component] (get (get components component) id))
  (get-entities [db] entities) 
  (get-component [db component] (get components component))
  (get-components [db] components)
  (get-domains [db] (keys components)))
  
;helper macro -> we can probably use a more general binding here...
(defmacro with-store
  "Evaluate body in the context of bound vars, where entities and components 
   are assoc'd values in the entitystore"
  [store body]
  `(let [~'entities (:entities ~store) 
         ~'components (:components ~store)]
     ~body))

;We can also implement a destructive store....we just keep the state in an 
;atom, and wrap our operations behind it....
(defn ->MutableStore
  "Defines an implementation of IEntitystore whose add/drop operations are 
   destructive.  Maintains an atom of state, to persist the effects of 
   operations.  Reads are non-destructive.  Still conforms to the functional 
   API of the IEntityStore protocol, in that add/drop still return an 
   entitystore, in this case, the same store that was passed in and mutated. 
   We can easily extend this to provide a fluent interface for database-backed 
   stores, which is pretty useful."
  [ents comps] 
  (let [state (atom (->EntityStore ents comps))]
    (reify IEntityStore 
        (has-entity? [db id] (with-store @state (contains? entities id)))
        (get-keys [db] (with-store @state (keys entities)))
        (get-parts [db id] (with-store @state (get entities id #{})))
        (add-record [db id component data]
          (do 
	          (swap! state (fn [s]
	            (with-store s
	              (EntityStore. 
	                (assoc entities id (conj (get entities id #{}) component)) 
	                (assoc components component
	                       (merge (get components component {}) {id data})))))))
             db)
        (drop-record [db id component]
           (do
	           (swap! state	 (fn [s]
	             (with-store s
		             (let [cnext (dissoc components component id)
		                   enext (let [parts (disj (get-parts db id) component)]
		                           (if (zero? (count parts))
		                             (dissoc entities id)
		                             (assoc entities id parts)))]
		                   (EntityStore. enext cnext))))))
             db)
        (get-record [db id component] 
          (with-store @state (get (get components component) id)))
        (get-entities [db] (with-store @state entities)) 
        (get-component [db component] 
          (with-store @state (get components component)))
        (get-components [db] (:components @state))
        (get-domains [db] (keys (:components @state))))))

;While we're at it...let's allow regular maps to recognize our entitystore 
;functions...
(defn make-mapstore [entities components] 
  {:entities entities :components components})

(extend-type clojure.lang.PersistentArrayMap 
  IEntityStore
	  (has-entity? [db id] (with-store db (contains? entities id)))
	  (get-keys [db] (with-store db (keys entities)))
	  (get-parts [db id] (with-store db (get entities id #{})))
	  (add-record [db id component data]
	    (with-store db 
         (make-mapstore  
             (assoc entities id (conj (get entities id #{}) component)) 
             (assoc components component
                (merge (get components component {}) {id data})))))
	  (drop-record [db id component]
	    (with-store db
		    (let [cnext (dissoc components component id)
		          enext (let [parts (disj (get-parts db id) component)]
		                      (if (zero? (count parts))
		                        (dissoc entities id)
		                        (assoc entities id parts)))]
		      (make-mapstore enext cnext))))
	  (get-record [db id component] (with-store db 
	                                  (get (get components component) id)))
	  (get-entities [db] (with-store db entities)) 
	  (get-component [db component] (with-store db (get components component)))
	  (get-components [db] (with-store db components))
	  (get-domains [db] (with-store db (keys components))))



(def emptystore (->EntityStore {} {}))
(defn make-mutable 
  "Create an empty mutable entity store, or derive one from an existing 
   store."
  ([] (->MutableStore {} {}))
  ([store] (->MutableStore (:entities store) (:components store))))

;components define a unique domain, and some data associated with the domain.
;in most setups, data is statically typed, so that the components are homogenous
;we'll follow that practice, but nothing is technically stopping us from having
;heterogenous data across a component.
(defrecord component [domain data]) 

;The original design for entities had something like this...
;#entity{:name "Tom" :components {:age {:domain :age :data 31}
;                                 :height {:domain :height :data 60}}}

;The problem with this organization is that we have redundant information.
;We could represent entities a bit flatter....
;#entity{:name "Tom" 
;        :age 31
;        :height 60}
;Still use components, but entities are just flat associations...
;All entities implicitly have :name or :id component.
;Still get O(1) access...
;What does that do for components? 
;The entity's component domains are encoded implicitly in the keys. 
;I think it's "nice" to have implicit entity specifications...

;Allows us to extend the notion of "entity" any any arbitrary associative
;structure.


(defprotocol IEntity 
  "A protocol for defining fundamental operations on entities."
  (entity-name [e] "Get the unique name of entity e")
  (entity-components [e] "Get the unique components that define entity e"))

;We define a useful container for entities.  We use this as an initial and 
;intermediate form for querying and computation, but the entity is "really" 
;stored across entries in the entity store's component tables.  Still, 
;it's useful to have a logical association of an entity to its components, 
;particularly when creating entities, or when inspecting them.  We use this 
;to get a reified form of the entity as needed.
(defrecord entity [name components]
  IEntity 
  (entity-name [e] name)
  (entity-components [e] components)) 

;is there a reason for this? 
;maybe you cut down on the number of ways an entity can be defined for 
;now...
;only maps and records...

;(defn get-key [plist k]
;  (loop [xs plist]
;    (when (seq xs)
;      (if (= (first xs) k)
;        [k (second xs)]
;        (recur (rest (rest plist)))))))
;
;(defn get-keys [plist]
;  (map first (partition 2 plist)))
;
;(defn get-vals [plist]
;  (map second (partition 2 plist)))
;
;(defn add-property [plist k v]
;  (reduce conj plist [k v]))

(extend-protocol IEntity
  nil 
  (entity-name [n] nil)
  (entity-components [n] nil)

  clojure.lang.PersistentHashMap
  (entity-name [m] (get m :name))
  (entity-components [m] (cond (contains? m :components) (get m :components)
                               (contains? m :domains) (get m :domains)
                               :else (dissoc m :name))))
  
(defn conj-component
  "Conjoings the component to components in ent ."
  [ent component]
  (assoc-in ent [:components (:domain component)] component)) 

(defn build-entity
  "Assembles an entity record from one or more component records."
  [name components] 
    (->entity name 
              (reduce (fn [acc {:keys [domain] :as component}] 
                        (assoc acc domain component)) {} components)))

(defn keyval->component
  "Converts key/value pairs into components.  Allows a simple shorthand
   for composing entities, in that entity components can be contained in 
   a map."
  ([k v]
    (cond (= (type v) component)  v
          (and (map? v) 
               (contains? v :domain) 
               (contains? v :components))
          (->component (:domain v) (:components v))
          :else (->component k v)))
  ([keyval] (keyval->component (first keyval) (fnext keyval))))          

(defn entity-from-map [m]
  "Allows shorthand definition of entities from simple map structures.
   Keys in the map correspond to component domains of the entity."
  (let [entname (get m :name)]
    (->entity entname 
              (reduce #(assoc %1 (:domain %2) %2) {}
                      (map keyval->component (dissoc m :name))))))
                  
(defn entity->components
  "Retrieve the component data associated with an entity."
  [ent] (vals (:components ent)))

(defn entity->domains
  "Retrieve the domains that the entity is a member of, where 
   domains are the names of components."
  [ent] (keys (:components ent)))

(defn entity->records
  "Converts the entity into a sequene of records, which can be easily added to
   an entity store.  This is the entity in its non-reified form, distributed
   across one or more components."
  [ent]
  (let [id (:name ent)]
    (map (fn [{:keys [domain data]}] [id domain data]) 
         (entity->components ent))))

(defn merge-entity
  "Similar to the merge operation on maps, only for entities.  When entities 
   are merged, the components of the first entity are conjoined with the second, 
   and a 'merged' entity is produced. The merged entity has components across 
   the union of domains from both entities.  The component data in each domain 
   corresponds to the last component data found.  This means entity2's data will 
   be returned in cases where the domains overlap."
  [e1 e2] (build-entity (str (:name e1) (:name e2))
                         (concat (entity->components e1)
                                 (entity->components e2))))
(defn ent-seq? [entcoll]
  (and (not (empty? entcoll))
       (or  (coll? (first entcoll))  
         (= (type (first entcoll)) entity))))
            

(defn merge-entities 
  "Merges multiple entities.  For overlapping domains, the right-most, or last 
   entity's data is returned in the merge."
  [entcoll] 
  (if (ent-seq? entcoll)
    (reduce merge-entity entcoll)
    (throw (Exception. "Expected a collection of entities."))))
                    
(defn get-info
  "Get a quick summary of the entity, i.e. its components..."
  [ent]
  [(:name ent) (into [] (keys (:components ent)))])
  
;protocol-derived functionality 

(defn add-field
  "Deprecated. 
   Adds a component entry to db for entity id, derived from a map containing 
   domain and data keys."
  [db id {:keys [domain data] :as component}] 
  (add-record db id domain data)) 

(defn reduce-records
  "Mechanism for updating the entity store.  
   Fold function f::store -> [id component data] -> store 
   over a sequence of records, with store as its initial value."
  [store f recs] 
  (reduce (fn [store rec] (f store rec)) store recs))

(defn drop-records 
  "Fold a sequence of [id component data] using drop-record to return 
   an updated entitystore."
  [store drops]
  (reduce-records store (fn [s [id c & more]] (drop-record s id c)) drops))

(defn add-records 
  "Fold a sequence of [id component data] using add-record to return 
   an updated entitystore."
  [store adds]
  (reduce-records store (fn [s [id c data]] (add-record s id c data)) adds))

(defn add-parameter
  "Add a simple name and value to a dedicated parameters table.  We treat this 
   like any other component, except we provide explicit functionality to access
   it....for semantic purposes.  Parameters usually have a special meaning."
  [store name val]
  (add-record store name :parameters val))
  
(defn drop-parameter
  "Add a simple name and value to a dedicated parameters table.  We treat this 
   like any other component, except we provide explicit functionality to access
   it....for semantic purposes.  Parameters usually have a special meaning."
  [store name]
  (drop-record store name :parameters))

(defn add-entity 
  "Associate component data with id.  Records are {:component data} or 
  [[component data]] form.  Alternately, add a pre-built entity record."
  ([db id records] (reduce #(add-field %1 id %2) db records))
  ([db ^entity ent] (add-records db (entity->records ent))))

(defn add-entities
  "Register multiple entity records at once..." 
  [db entities] (reduce add-entity db entities))

(defn drop-entity
  "drop component data associated with id, and id from entities in db." 
  [db id]
  (reduce #(drop-record %1 id %2) db (get-parts db id)))

(defn entity-count
  "Return the count of unique entity ids in the entitystore."
  [db] (count (get-entities db)))

(defn entity-data
  "Query the entitystore for the components associated with entity id."
  [db id]
  (reduce #(assoc %1 %2 (->component %2 (get-record db id %2)))
    {} (get-parts db id)))

(defn entities-in
  "Returns a sequence of entity ids that are members of a domain.
   Specifically, each entity has component data in the domain."
  [db domain]
  (keys (get-component db domain)))

(defn get-entity
  "Reify an entity record for entity id from an entitystore."
  [db id]
  (if-let [data (entity-data db id)]
    (->entity id data)
    nil))

(defn get-entities 
  "Reify multiple entity records from ids from entitystore."
  [db ids]
  (map #(get-entity db %) ids))

(defn entity-seq
  "Return a lazy sequence of reified entity records for each entity in the 
   entitystore."
  [db] 
  (for [id (get-keys db)]
    (get-entity db id)))

(defn entity-union
  "Returns the logical union of entities across one or more domains, 
   retuning a set of entity ids, in which each entity is a member of 
   one or more domains."
  [db domains]
  (->> domains 
    (map #((comp set entities-in) db %)) 
    (reduce clojure.set/union))) 
    
(defn entity-intersection
  "Returns the logical intersection of entities across one or more domains, 
   retuning a set of entity ids, in which each entity is a member of 
   all domains."
  [db domains]
  (->> domains 
    (map #((comp set entities-in) db %)) 
    (reduce clojure.set/intersection)))

(defn key->symbol [k]
  (symbol (subs (str k) 1)))

(defn kvps->binds [m]
  (loop [acc nil
         ks (keys m)]
    (if (seq ks) 
      (recur (apply conj acc (let [k (first ks)]
                               `(~(get m k) ~(key->symbol k))))
             (rest ks))
      acc)))

(defn entity-binds [e]
  (kvps->binds (merge (:components e) {:name (:name e)})))
                 
(defmacro with-entity
 "Macro to allow binding of specific components"
 [ent & body]
    `(let [~@(entity-binds (eval ent))]
      ~@body))      

(defn default
  "If x is nil, returns y.  Used for implementing default values."
  [& [x y & rest]]
  (if x x y))

;I'd like to have a high-level abstraction for querying entities...
;If we treat the entity store as a database, our components are tables.
;Rows/records are the entities in the component's table.


;where should be a function on the entity....not the component.
;if select does its job, the resulting set will be a sequence of reified 
;entities 
  
;When we select entities from an entity store, our building blocks are 
;components.  Components are simple 2-column tables of [entityID data].
;Any predicates used for filtering must be phrased in terms of component
;and entity relationships. 

;Example query -> find the entity nearest to a coordinate, whose name is 
;NOT "Bob".
;(defn get-nearest-visible [id store]
;  (let [fromcoords (->> (get-entity store id) :components :coords)]
;    (->> store
;      (select-entities :from [named visible coordinates] ;intersection.
;                       :where #(and visible
;                                    (not= named "Bob")
;                                    (in-range? coordinates fromcoords))
;                       :order-by (fn [e1 e2] 
;                                   (euclidean coordinates fromcoords))))))

;in peter's work, and in a real db, from tells us which tables to join.
;where applies a predicate to each row in the joined table, indicating if
;the record should be excluded. 
;order-by indicates how to order of the final resulting recordset.

;the entitystore is a simplified datastore.  We can use relational semantics 
;for selection, projection, etc, since we have primary keys in every table. 
;The simplification is that each table is really just a 2-column recordset, 
;where the first field is always the entity's id, or the primary key. 
;The second field can be any kind of data (likely nested, possibly atomic).
;The simplified records are represented as a key-value pair. 
;So the entity store is a simple key-value store. 


;Porting the SQL-like language in Peter Seibel's excellent Practical Common Lisp
(defn select-entities
  "Acts like a SQL select, in which components are analogous to single-column
   tables of data.  Filters the results by where predicate, optionally sorts 
   query if order-by is supplied. Values are automatically distinct due to the 
   nature of the entity store (e.g. component records are unique relative to the 
   entity).  Returns a (sub)set of the component data."  
  [store & {:keys [from join-by where order-by] :or {from (get-domains store)
                                                join-by :intersection
                                                where nil
                                                order-by nil}}]
  (let [joinfunc (cond (= join-by :intersection) entity-intersection 
                       (= join-by :union)  entity-union
                       :else join-by)]                       
  (reduce (fn [acc f] (if (nil? f) acc (f acc))) 
      (get-entities store (joinfunc store (if (coll? from) from [from])))
      [(when where 
         (fn [es] (filter where es))) 
       (when order-by
         (fn [es] (sort-by order-by es)))])))

(defn select-store [store & {:keys [from join-by where order-by] 
                             :or {from (get-domains store)
                                  join-by :intersection
                                  where nil
                                  order-by nil}}]
  (->> (select-entities  store 
         {:from from :join-by join-by :where where :order-by order-by})
       (add-entities emptystore)))
                    

;(defn entities-resembling
;  "Using the components in entity e as a template, selects entities with 
;   similar domain membership (shares the same components, but not necessarily 
;   identical component values)."
;  [db e]
;  (select-entities db :from ()

;(defn entities-identical
;  [db e])
  

;a factory for defining component boilerplate for us. 
(defmacro defcomponent
  "Macro to define a new component (for use in specifying entity templates and 
   general convenience).  Defined components get a namespace-local constructor 
   prefixed with ->  Allows definition of complex constructors for the generic 
   ->component record type.  Args are evaluated in the context of body when 
   a defined component is evaluated, which allows for parameterizing of 
   components.

   Usage:  
  (defcomponent basicstats [{:keys [health agility strength] :as stats}] stats)"
  
  [name args body]
  `(defn ~(symbol (str "->" name)) ~args 
         (->component ~(keyword name) ~body)))

;It'd also be nice to easily define functions that dispatch on components...

(defn binding->component [expr] 
   (if (keyword? (first expr)) 
     `(~'keyval->component ~(first expr) ~(second expr)) 
     (let [[expr1 expr2] expr]
       (list (symbol (str '-> (str expr1))) 
             expr2))))

(defmacro emit-entity-builder [args cs]
  `(fn [~'id ~@(distinct (remove #{'id} args))]    
     (~'build-entity ~'id 
       [~@(map binding->component (partition 2 cs))])))

(defn spec-merger [specs]
  (fn [id] 
    (reduce #(apply conj %1 %2) []
      (:components (merge-entities 
                     (map (fn [s] (if (fn? s) (s id) s)) specs))))))

;specs are either [symbol]
;or [(entity-builder args)]
;where symbol is a function of one arg, ID, 
;entity-builder is a function of more than one arg, to be threaded an ID...
;
(defmacro emit-complex-entity-builder [args specs cs]
  `(let [specbuilder#  (~'spec-merger
                         (list ~@(for [s specs] 
                                   (if (coll? s) `(~'fn [~'id]  ~s) `(list ~s ~'id)))))]                             
     (fn [~'id ~@(distinct (remove #{'id} args))]
       (let [components# (list ~@(map binding->component (partition 2 cs)))]
         (~'build-entity ~'id
             (specbuilder# ~'id))))))
;           (concat (specbuilder# ~'id) components#))))))

;macro to define functions for building stock templates for entities
;allows us to define namespaced functions to build default entities easily.
;I could probably borrow from CLOS here....but I'm not there yet...it'd be 
;nice to have specs belong to pseudo classes and inheritance, but it's 
;really not "that" necessary....all they're doing is defining a set of 
;components for us.  It might behoove us to push this off into a pure data 
;representation as well...Actually, the constructor built by defspec actually
;"looks" like a record anyway...
(defmacro entity-spec
  "Helper macro for composing anonymous entity-building functions.  We can 
   inline these, or formally define them as with defspec.
   
   Formal components declared by defcomponent must be declared ahead of time,
   but they may be parameterized in the body using arguments supplied by args.
   Component parameterization is not mutually recursive, thus there is currently 
   no shared lexical environment between components.
     
   Inlined or ad-hoc components  can be evaluated as bindings of the form 
   [:component-name component-data] , where component-name is a keyword.
   
   Usage, with defined components and one inline component called playertag:

   (entity-spec [id h] 
      [basicstats {:health h :agility 30 :strength 30}
       offense 10
       visage (str The remnant of a lost age, standing alone against evil)
       coords {:x 0 :y 0}
       :playertag :player1]
       )   
   Yields a lambda, (fn [id] ...) which will create entities with the
   specified components.  If no arguments are supplied, a single id arg 
   will be inserted."
  ([args specs components]
;    (let [cs (concat (reduce #(apply conj %1 %2) [] 
;                 (:components (merge-entities 
;                                (map #(let [s (eval %)]
;                                        (if (fn? s) 
;                                          (s (str (gensym)))
;                                          s)) specs)))) 
;                     components)]
;       `(~'emit-entity-builder ~args ~cs)))
       `(~'emit-complex-entity-builder ~args ~specs ~components))
  ([args components]
    (let [args (distinct (remove #{'id} args))]
      `(~'emit-entity-builder ~args ~components))))

(defmacro defspec
  "Allows composition of a set of components into an entity template.  Creates 
   a function in the current namespace, prefixed with 'build-', taking at least
   one argument - id -  that allows for declaration of entities based on the 
   specification.  id will always be the first argument, but callers may 
   declare more arguments that will become part of lexical environment of 
   the entity building function.  This allows parameterization of entity 
   specifications, where component expressions can be further parameterized if 
   desired. 
   
   Formal components declared by defcomponent must be declared ahead of time,
   but they may be parameterized in the body using arguments supplied by args.
   Component parameterization is not mutually recursive, thus there is currently 
   no shared lexical environment between components.
     
   Inlined or ad-hoc components  can be evaluated as bindings of the form 
   [:component-name component-data] , where component-name is a keyword.
   
   Usage, with defined components and one inline component called playertag:

   (defspec player [id] 
      [basicstats {:health 30 :agility 30 :strength 30}
       offense 10
       visage (str The remnant of a lost age, standing alone against evil)
       coords {:x 0 :y 0}
       :playertag :player1]
       )   
   Yields a function (build-player id) which will create player entities.
   If no arguments are supplied, a single id arg will be inserted.
             
   Alternately, new specs can be derived from existing specs.  If a vector of 
   specs is supplied prior to the components, then the specs will be evald, 
   their components merged, as per merge-entity.  Components with identical 
   domains will retain the last value in the final spec, which is more or 
   less how inheritance typically works.  If the spec is an anonymous spec, 
   as defined by entity-spec, then it will work as well.  This should allow 
   variable means to compose entities, as well as overriding pieces of 
   entity construction.

   An example of another entity leveraging the player spec:  
   (defspec computer-player [aitype name]       
     [player] 
     [:playertag :computer
      :ai aitype])
   This yields a function, (build-computer-player id aitype name) that 
   produces parameterized computer players."   
  ([name args specs components]
    `(def ~name (entity-spec ~args ~specs ~components)))
  ([name args components]
    `(def ~name (entity-spec ~args ~components)))
  ([name components]
    `(def ~name (entity-spec [] ~components))))


;(defmacro defspec
;  "Allows composition of a set of components into an entity template.  Creates 
;   a function in the current namespace, prefixed with 'build-', taking at least
;   one argument - id -  that allows for declaration of entities based on the 
;   specification.  id will always be the first argument, but callers may 
;   declare more arguments that will become part of lexical environment of 
;   the entity building function.  This allows parameterization of entity 
;   specifications, where component expressions can be further parameterized if 
;   desired. 
;   
;   Formal components declared by defcomponent must be declared ahead of time,
;   but they may be parameterized in the body using arguments supplied by args.
;   Component parameterization is not mutually recursive, thus there is currently 
;   no shared lexical environment between components.
;     
;   Inlined or ad-hoc components  can be evaluated as bindings of the form 
;   [:component-name component-data] , where component-name is a keyword.
;   
;   Usage, with defined components and one inline component called playertag:
;
;   (defspec player [id] 
;      [basicstats {:health 30 :agility 30 :strength 30}
;       offense 10
;       visage (str The remnant of a lost age, standing alone against evil)
;       coords {:x 0 :y 0}
;       :playertag :player1]
;       )   
;   Yields a function (build-player id) which will create player entities.
;   If no arguments are supplied, a single id arg will be inserted.
;             
;   Alternately, new specs can be derived from existing specs.  If a vector of 
;   specs is supplied prior to the components, then the specs will be evald, 
;   their components merged, as per merge-entity.  Components with identical 
;   domains will retain the last value in the final spec, which is more or 
;   less how inheritance typically works.  If the spec is an anonymous spec, 
;   as defined by entity-spec, then it will work as well.  This should allow 
;   variable means to compose entities, as well as overriding pieces of 
;   entity construction.
;
;   An example of another entity leveraging the player spec:  
;   (defspec computer-player [aitype name]       
;     [player] 
;     [:playertag :computer
;      :ai aitype])
;   This yields a function, (build-computer-player id aitype name) that 
;   produces parameterized computer players."   
;  ([name args specs components]
;    `(defspec ~name ~args
;       ~(concat (reduce #(apply conj %1 %2) [] (:components (merge-entities 
;                                (map #((eval %) (str (quote name))) specs)))) 
;                  components)))
;  ([name args components]
;    (let [args (distinct (remove #{'id} args))]
;      `(defn ~name [~'id ~@args]    
;         (build-entity ~'id 
;           [~@(map (fn [expr] 
;                     (if (keyword? (first expr)) 
;                       `(~'keyval->component ~(first expr) ~(second expr)) 
;                       (let [[expr1 expr2] expr]
;                         (list (symbol (str '-> (str expr1))) 
;                               expr2)))) (partition 2 components))])))))


