(ns entitysystem.core
  (:use [DEVS.schedule]))

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
  (has-entity? [db id] "existence check for entities")
  (get-keys [db] "returns a set of all entity keys")
  (get-parts [db id] "derive components id contains")
  (add-record [db id component data] "Associate a record of data /w entity id")
  (drop-record [db id component] "Drop record for id from component")
  (get-record [db id component] "Fetch the component record for ent id")
  (get-entities [db] "Return a seq of [entityid #{components..}]")
  (get-component [db component] "Return the data for component"))

;  (get-entity [db id] "Return a collection of components associated with ID")

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
  (get-component [db component] (get components component)))
  

(def emptybase (->EntityStore {} {}))

(defrecord component [domain data]) 
(defrecord entity [name components]) 


(defn conj-component [ent component]
  (assoc-in ent [:components (:domain component)] component)) 

(defn defentity [name & components]
  (->entity name (reduce (fn [acc [k v]] (assoc acc k (->component k v))) {} 
                               (partition  2 components))))

(defn build-entity [name components] 
    (->entity name 
              (reduce (fn [acc {:keys [domain] :as component}] 
                        (assoc acc domain component)) {} components))) 
;protocol-derived functionality 

(defn add-field 
  [db id {:keys [domain data] :as component}] 
  (add-record db id domain data)) 

(defn reduce-records
  "Mechanism for updating the entity store.  
   Fold function f::store -> [id component data] -> store 
   over a sequence of records, with store as its initial value."
  [store f recs] 
  (reduce (fn [store rec] (apply f store rec)) store recs))

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

(defn add-entity 
  "Associate component data with id.  Records are {:component data} or 
  [[component data]] form.  Alternately, add a pre-built entity record."
  ([db id records] (reduce #(add-field %1 id %2) db records))
  ([db ^entity ent] (add-entity db (:name ent) (:data ent))))  

(defn add-entities
  "Add multiple entities at once..." 
  [db entities] (reduce add-entity db entities))

(defn drop-entity
  "drop component data associated with id, and id from entities in db." 
  [db id]
  (reduce #(drop-record %1 id %2) db (get-parts db id)))


(defn entity-count [db] (count (get-entities db)))
(defn entity-data
  "Convert an entity into a map of components"
  [db id]
  (reduce #(assoc %1 %2 (->component %2 (get-record db id %2)))
    {} (get-parts db id)))

(defn get-entity [db id]
  (if-let [data (entity-data db id)]
    (->entity id data)
    nil))

(defn entity-seq [db] 
  (for [id (get-keys db)]
    (get-entity db id)))

;some testing functions....
;define a little dumbset of entities.

;a factory for defining component boilerplate for us. 
(defmacro defcomponent [name args body]
  `(defn ~(symbol (str "->" name)) ~args 
         (->component ~(keyword name) ~body)))

;macro to define functions for building stock templates for entities
;allows us to define namespaced functions to build default entities easily.
;I could probably borrow from CLOS here....but I'm not there yet...it'd be 
;nice to have specs belong to pseudo classes and inheritance, but it's 
;really not "that" necessary....all they're doing is defining a set of 
;components for us.  It might behoove us to push this off into a pure data 
;representation as well...

(defmacro defspec
  "Allows composition of a set of components into an entity template.  Creates 
   a function in the current namespace, prefixed with 'spec', taking one arg 
   - id -  that allows for declaration of entities based on the specification."
  [name components & inlinedcomponents]
    `(defn ~(symbol (str "spec-" name)) ~'[id]    
           (build-entity ~'id [~@(concat
                                   (map (fn [[expr1 expr2]]
                                          (list (symbol (str '-> (str expr1))) 
                                                expr2))
                                        (partition 2 components)) 
                                   inlinedcomponents)])))

;The entity store provides an abstract database for storing entities, as they
;manifest across component data.  Specifications allow us to define common or 
;default combinations of components, to produce entity factories. 

