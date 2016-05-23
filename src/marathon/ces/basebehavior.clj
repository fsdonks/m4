(ns marathon.ces.basebehavior
  (:require [spork.ai.core :as ai]
            [spork.ai.behavior :refer :all]
            [spork.entitysystem.store :refer :all]
            [spork.sim.simcontext :as sim]))

(defrecord behaviorenv [entity behavior current-messages new-messages ctx current-message
                        tupdate deltat statedata]
  ai/IEntityMessaging
  (entity-messages- [e id] current-messages)
  (push-message-    [e from to msg] ;should probably guard against posing as another entity
    (let [t        (.valAt ^clojure.lang.ILookup  msg :t)
          _        (ai/debug [:add-new-messages-to new-messages])
          additional-messages (spork.ai.behavior/swap!! (or new-messages  (atom []))
                                        (fn [^clojure.lang.IPersistentCollection xs]
                                          (.cons  xs
                                             (.assoc ^clojure.lang.Associative msg :from from))))]                            
      (behaviorenv. entity
                    behavior
                    current-messages
                    additional-messages
                    ctx
                    current-message
                    tupdate
                    deltat
                    statedata
                    )))
  ai/IEntityStorage ;we could just have commit-entity- return something we can append...another idea.
  (commit-entity- [env]
    (let [ctx      (ai/deref! ctx)
          ent      (-> (ai/deref! entity) (assoc  :statedata statedata :last-update tupdate))
         ; existing-messages (atom (:messages ent))          
          id       (:name ent)
;          _       (ai/debug  [:committing ent])
;          _       (ai/debug  [:new-messages new-messages])
          ]
      (reduce
       (fn [acc m]
         (do 
          ;(println [:pushing m :in acc])
          (sim/trigger-event m acc)))
       (mergee ctx (:name ent) ent)
       new-messages))))

(def args  (atom  nil))

;;global var...
(def default-behavior (atom nil))
(def behaviors (atom nil))

;;note: if we change over to a set of coroutines running the ECS,
;;we can just put the message on their channel and let the coro
;;handle updates.

;;We have a way to send messages now....dispatch is handled through
;;send-message, which uses step-entity! to handle the message.


;;we could go ahead and extend-protocol to simcontext.

(defn ->benv [ctx e msg default]
    (let [^clojure.lang.ILookup  e  (if (map? e) e (get-entity ctx e))
         ent    (atom e)
         beh   (.valAt e :behavior default)
         beh   (cond (identical? beh :default)
                       (do  (swap! ent assoc :behavior default)
                            default)
                       (nil? beh) (throw (Exception. "No behavior defined..."))
                       :else  beh)
         tupdate (:t msg)
        ;;load a behavior context for the entity to behave in.
        ;;Note: if we're using stateful object-like entities,
        ;;they'll maintain a stateful behavior environment.
        benv (marathon.ces.basebehavior.behaviorenv.  ent
             beh ;right now there's only one behavior :default
             [msg]
             nil
             (atom ctx)
             msg     
             tupdate ;current time.
             (if-let [tprev (:last-update e)] ;deltat
               (- tupdate tprev)
               0)
             (:statedata e))        
          _ (reset! args benv)]
      benv))

;;immediate steps happen with no time-delta.
;;like ai/step-entity!, we should find a way to reuse it.
(defn step-entity!
  ([ctx e msg default]
   (let [^behaviorenv benv (->benv ctx e msg default)
        _    (ai/debug [:stepping (:name e) :last-update (:last-update e)  msg])]
    (-> (beval (.behavior benv) benv)
        (return!)
        (ai/commit-entity-))))
  ([ctx e msg] (step-entity! ctx e msg @default-behavior)))

