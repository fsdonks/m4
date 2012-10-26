;deprecated
(ns CBA.component
  (:use [DEVS.Schedule]))

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
unique data (which reinforces our desire to maintain orthogonal domains).

...many more comments to come")

(defn- find-name [obj]
  (if (:name obj) (:name obj) 
    (if (:id obj) (:id obj) nil)))

(defn- dumb-dispatch 
  ([msg] (println msg))
  ([] (println "Nothing sent")))

(defprotocol IComponent
  "-Currently not used-
   IComponent protocol is a common interface for things that represent 
   a domain and domain-specific data."  
  (get-domain [c])
  (get-data [c]))

(defprotocol IEntity
  "IEntity protocol is a common interface for things that represent 
   a mapping of name->IComponents."
  (get-name [e]) 
  (get-state [e]))
;(def defaultEntity {:get-name find-name})

;component container has one or more components.  Entities and enviroments 
;both host components, and implement this protocol.
(defprotocol IComponents
  "IComponents is a protocol for accessing an abstract sequence of IComponent
   data, hosted by some component container - usually the data bound to an
   IEntity."
  (list-components [cc])
  (components->seq [cc]) 
  (get-host [cc]))
;(def defaultComponents {:list-components :components :get-host (fn [cc] cc)})                    

(defprotocol ISystem
  "ISystem is a protocol for querying and updating aspects of a System."
  (list-domains [s])
  (update [s msg env]))
;(def defaultSystem {:list-domains :domains :update dumb-dispatch})

(defprotocol IEnvironment 
  "IEnvironment is a protocol for querying and updating aspects of an 
   environment."
  (change-state [e s]))
;(def defaultEnvironment {:change-state #((e %) s)})

(defrecord Environment [entities domains systems dispatchf data]
  IEnvironment 
    (change-state [env s] #((env %) s))
  ISystem 
    (list-domains [s] (:domains s))
    (update [s msg env] ((:dispatchf s) msg env))
  IComponents 
    (list-components [cc] (:domains cc)) 
    ;(components->seq [cc] (mapcat list-components (:entities cc))
    (get-host [cc] cc))                         

(defn make-env [] (Environment. {} #{} [] dumb-dispatch {}))

(defrecord SubSystem [domains dispatchf] 
  ISystem 
    (list-domains [sys] (:domains sys))
    (update [sys msg env] ((:dispatchf sys) msg env)))

(defn make-subsystem [domainlist dispatchf] 
  (SubSystem. (into #{} domainlist) dispatchf))

(defrecord Entity [id components] 
  IComponents 
   (list-components [cc] (:components cc))
   (get-host [cc] cc)
  IEntity
   (get-name [e] (:id e))
   (get-state [e] (:components e)))

(defn get-domain* [component]
  (first component))

(defn get-data* [component] 
  (second component))

(defn make-entity
  ([id] (Entity. id {}))
;  ([id cname c] (Entity. id (conj {} [cname c]))) 
  ([id cpairs] (Entity. id (reduce conj {} cpairs))))

(defn add-component [e c]
  (merge e {:components (assoc (:components e) (get-domain* c) (get-data* c))})) 

(defn add-components [e coll]
  (reduce add-component e coll)) 

(defn add-entity [env e]
  (let [cs (list-components e)
        domains (into (:domains env) (keys cs))
        entities (assoc (:entities env) (get-name e) e)]
    (merge env {:domains domains :entities entities})))
(defn add-entities [env es] (reduce add-entity env es)) 

(defn list-entities [env]
  (map #(let [ent (% 1)
              cs (keys (list-components ent))]
               [(get-name ent)  cs]) (:entities env)))

(defn list-systems [env] (:systems env))
     
(defn get-entitynames [env]
  (map #(get-name (% 1)) (:entities env)))

;;simple component definitions...
(defn time-component [tlast tnext] 
  [:time [tlast tnext]])

(defn comm-component 
  ([] [:comm {:channels #{}}])
  ([channelcoll] [:comm {:channels (if (set? channelcoll) 
                                     channelcoll (set channelcoll))}]))
    
(defn listen-component [evtstream] 
  [:listen (fn [msg] (next-event evtstream))])

(defn talk-component [evtstream] 
  [:talk (fn [msg] (put-event evtstream msg))])

(defn tags-component [tags]
  [:tagged (into #{} tags)])

(defn tag-entity
  ([ent] (add-component ent (tags-component nil)))
  ([ent tags] (add-component ent (tags-component tags)))) 

(defn get-entity [env ename] (get (:entities env) ename))

(defn schedule-component
  ([] [:schedule (make-schedule)])
  ([sched] [:schedule sched]))

(defn make-talker [e] 
  (let [s (:schedule (list-components e))
        talkc (talk-component s) 
        listenc (listen-component s)]
    (add-components e [talkc listenc])))

(defn typeclassifier
  ([o cname] (type o))
  ([o cname f] (f o)))

(defmulti has? typeclassifier) ;(fn [o cname] (type o))) 

(defmethod has? Entity [e cname] (contains? (list-components e) cname))
(defmethod has? Environment [env cname] 
  (or (contains? (:domains env) cname)
      (contains? (:entities env) cname)))

;(defmethod print-method CBA.component.Environment [env,w]
;  (print-method (quote Env) w))

(defn new-entity 
  ([name components] (-> (make-entity name)
                         (add-components components)))
  ([env name components] (add-entity env (new-entity name components))))

;testing 
;build an empty environment....
(def clock (-> (make-entity "clock")
               (add-components [(time-component 0 1) (schedule-component)])
               (make-talker)))

(def alarm (-> (make-entity "alarm")
               (add-components [(time-component 0 0) (schedule-component)])
               (make-talker)))

(def clockworld (-> (make-env) (add-entities [clock alarm])))


               
                 




