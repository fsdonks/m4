;;A separate namespace for testing and prototyping behaviors in 
;;clojure, using a functional style instead of imperative.
(ns marathon.data.behaviorbase)

;;what is the minimal amount of information necessary for a
;;behavior tree?

;;our btree will likely be a dag...
;;we can use our graph lib to derive behaviors..good for structural
;;property testing.

;;Also, break up behaviors into small, reusable components.
;;Taking a functional approach, a behavior has some "state",
;;determining whether it's active.

;;behaviors are either running, succeeded, failed.

(def behaviors nil)
(defprotocol IBehaviorTree
  (behave [b ctx]))
(defrecord bnode [type status f data]
  IBehaviorTree
  (behave [b ctx] (f ctx)))

(defn beval [b ctx]
  (cond (satisfies? IBehaviorTree b) (behave b ctx)
        (fn? b) (b ctx)))

;;note, behaviors are perfect candidates for zippers...
(defn ->leaf [f]    (->bnode  :leaf nil  (fn [ctx]  (f ctx)) nil))
(defn ->pred [pred] (->bnode  :pred nil  (fn [ctx] (if (pred ctx) [:success ctx] [:fail ctx])) nil))
(defn ->and  [xs]
  (->bnode  :and nil
     (fn [ctx]
      (reduce (fn [acc child]
                (let [[res ctx] (beval child (second acc))]
                  (case res
                    :run       (reduced [:run ctx])
                    :success   [:success ctx]
                    :fail      (reduced [:fail ctx])))) [:success ctx] xs))
     xs))

(defn ->or  [xs]
  (->bnode  :or nil 
     (fn [b ctx]
       (reduce (fn [acc child]
                 (let [[res ctx] (beval child acc)]
                   (case res
                     :run       (reduced [:run ctx])
                     :success   (reduced [:success ctx])
                     :fail      [:fail ctx]))) ctx xs))
     xs))

(defn ->not [b]
  (->bnode  :not nil
      (fn [b ctx] (let [[res ctx] (beval b ctx)]
                   (case res
                     :run [:run ctx]
                     :success [:fail ctx]
                     :fail [:success ctx])))
      b))

;;if a behavior fails, we return fail to the parent.
;;we can represent a running behavior as a zipper....
;;alternatively, we can just reval the behavior every time (not bad).
(defn ->alter  [f] (->bnode :alter nil (fn [ctx] [:success (f ctx)]) nil))
(defn ->elapse [interval]                            
    (->alter #(update-in % [:time] + interval)))

(defn succeed [b]
  (fn [ctx] [:success (second (beval b ctx))]))
(defn fail [b]
  (fn [ctx] [:fail (second (beval b ctx))]))

;;a behavior that waits until the time is less than 10.
(defn ->wait-until [pred]
  (->bnode  :wait-until nil 
          (fn [ctx] (if (pred ctx) [:success ctx] [:run ctx]))    nil))

;;do we allow internal failure to signal external failure?
(defn ->while [pred b]
  (->bnode :while nil 
           (fn [ctx] (if (pred ctx) 
                         (beval b ctx)
                         [:fail ctx])) 
           b))
          
(defn ->elapse-until [t interval]
  (->while #(< (:time %) t)
            (->elapse interval)))

(defn ->do [f] 
  (fn [ctx] [:success (do (f ctx) ctx)]))

(def bt (->and [(->wait-until #(= (:time %) 12))
                (->alter #(assoc % :count 1))]))

(def testctx {:time 10})

;;a simple behavior tree...
;; (->and [(->wait-until #(= (:time %) 10))
;;         (->alter #(assoc % :count 1))])

;;let's say the entity has some notion of state then.
;;how long it's been there, etc.
;;The next behavior change happens in 20. 
;;We can determine how we'll modify the time the entity spent in a
;;behavior...

(def bigctx {:time 0 :ages   {:a 0 :b 0 :c 0}
                     :states {:a :default :b :default :c :default}})

(defn no-messages? [ctx] (not (contains?  ctx :messages)))
(def  await-messages
  (->while no-messages?
           (->and [(->do #(println "waiting for messages at " (:time %)))
                   (->elapse 1)])))

(defn update-entity [id msg ctx]
  (let [b (get-in ctx [:behavior id])]
    (beval (assoc ctx :id id :msg msg) 
           b)))

(defn dispatch-messages [ctx]
  (if-let [messages (get ctx :messages)]  
    (let [t (:time ctx)
          dispatches (atom [])
          remaining
                  (reduce-kv (fn [acc tm  msg]
                               (if (<= tm t)
                                 (do (swap! dispatches conj msg)
                                     (dissoc acc tm))
                                 acc))
                             messages messages)]
      (reduce (fn [ctx [ent msg]]
                (update-entity ent msg ctx))              
              (if (empty? remaining) 
                (dissoc ctx :messages)
                (assoc ctx :messages remaining)))))
  ctx)

;;entities age themselves...
(defn update-entity [id]
  (fn [ctx] 
    (let [t (get ctx :time)
          last-update (get-in ctx [:ages id]) 
          delta       (- t last-update)]
      (if (zero? delta)  ;;we're up-to-date...

;;Say we have an entity with a clock behavior..
;;It just elapses time until forever...
;;While it's elapsing, it also updates other entities in the context.
;;These updates take the form of applying a behavior to the entities, 
;;providing, as part of the context, how much time has elapsed for 
;;the entity.
;;this is crucial if we have a situation where the entity's notion 
;;of time is happening eventfully; we have concurrent entity "lives"
;;in play.
       
 
 
 
 

 

;;can we describe a simple behavior?
;;an alarm...
;;the behavior context is the current time, and a duration.

