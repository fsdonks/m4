;;A namespace for simple entity behaviors.
;;We'll define core behaviors here.
(ns marathon.ces.behavior
  (:require [spork.ai.core :as ai :refer
             [deref! fget fassoc  push-message- map->entityctx debug ->msg]]
            [spork.ai.behavior :as behavior
             :refer [beval
                     success?
                     success
                     run
                     fail
                     behave
                     ->seq
                     ->elapse
                     ->not
                     ->do
                     ->alter
                     ->elapse-until
                     ->leaf
                     ->wait-until
                     ->if
                     ->and
                     ->and!
                     ->pred
                     ->or
                     ->bnode
                     ->while
                     ->reduce
                     always-succeed
                     always-fail
                     bind!
                     bind!!
                     merge!
                     merge!!
                     push!
                     return!
                     val!
                     befn
                     ] :as b]
            [spork.util.general     :as gen]        
            [spork.data.priorityq   :as pq]
            [clojure.core.reducers  :as r]
            [spork.entitysystem.store :as store :refer :all :exclude [default]]
            [spork.sim.simcontext :as sim]
            ))


;;an alternative idea here...
;;use a closure to do all this stuff, and reify to give us implementations
;;for the object.  We can also just us a mutable hashmap behind the
;;scene if we want to...at some point, it's probably better to have
;;the shared-nothing approach and just leave entities in their
;;own mutable cells, isolated from other state.  We can
;;still maintain persistent history.  Everything becomes a lookup though;
;;we have to find the current value of the entity at time t;
;;More to think of here..

;;New
;;Environment for evaluating entity behaviors, adapted for use with the simcontext.
;;If we provide an address, the entity is pushed there.  So, we can have nested
;;updates inside associative structures.
(defrecord behaviorenv [entity behavior current-messages new-messages ctx current-message]
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
                    )))
  ai/IEntityStorage ;we could just have commit-entity- return something we can append...another idea.
  (commit-entity- [env]
    (let [ctx      (ai/deref! ctx)
          ent      (ai/deref! entity)
         ; existing-messages (atom (:messages ent))          
          id  (:name ent)
          _   (ai/debug  [:committing ent])
          _   (ai/debug  [:new-messages new-messages])
          ]
      (reduce
       (fn [acc m]
         (do 
          ;(println [:pushing m :in acc])
          (sim/trigger-event m acc)))
       (mergee ctx (:name ent) ent)
       new-messages))))

;;__Utility functions__
;;Entity step operations...
(defn progress-cycle [x width]
  (if (>= x width)
    0
    (unchecked-inc x)))

(defn deployment-over?
  [y]
  (or (>= y (* 15 31))
      (and (>= y 30) (<= (rand) 0.01))))


(defn deployed? [e] (identical? (:state e) :deploying))

(defn should-deploy? [t tmax]
  (and (>= t 365)
       (<= (rand) (* 0.005 (/ (double t) tmax)))))
(defn should-reset? [t tmax] (>= t tmax))

;;__Behavior Definitions__
;;Note: there are only a couple of small places where the behaviors need to
;;interface with the store.  We "could" abstract that away even further and
;;eliminate the store dependency, but for now it's fine.  For instance,
;;we could capture the changes as instructions, and queue them up for
;;integration via a higher-level system.
(defn rconcat
  ([& colls]
   (reify clojure.core.protocols/CollReduce
     (coll-reduce [this f1]
       (let [c1   (first colls)
             init (reduce (fn [acc x] (reduced x)) (r/take 1 c1))
             a0   (reduce f1 init (r/drop 1 c1))]
         (if (reduced? a0) @a0
             (reduce (fn [acc coll]
                       (reduce (fn [acc x]
                                 (f1 acc x)) acc coll)) a0 (r/drop 1 colls)))))
     (coll-reduce [this f init]
       (reduce (fn [acc coll]
                (reduce (fn [acc x]
                          (f acc x)) acc coll)) init colls))
     clojure.lang.ISeq
     (seq [this] (seq (into [] (r/mapcat identity colls) )))
     )))


;;__behaviors__

;;logs the state of the entity.
(befn notify-change {:keys [entity] :as ctx}
 (do (debug 
       (let [{:keys [state wait-time name t]} (deref! entity)
             ]
         (str "<"  t "> " "Entity " name
              " is now in state " state
              " for " wait-time " steps")))
     (success ctx)))

;;if we have a message, and the message indicates
;;a time delta, we should wait the amount of time
;;the delta indicates.  Waiting induces a change in the
;;remaining wait time, as well as a chang
(befn wait-in-state ^behaviorenv [entity current-message ctx]
  (let [;_ (println [:wait-in-state entity msg])
        msg    current-message
        t     (fget msg :t)
        delta (- t (fget (deref! entity) :t))]
    (when-let [duration (fget  (deref! entity) :wait-time)]
      (if (<= delta duration) ;time remains or is zero.
         ;(println [:entity-waited duration :remaining (- duration delta)])
        (merge!!  entity {:wait-time (- duration delta)
                          :t t}) ;;update the time.
        (do ;can't wait out entire time in this state.
          (merge!! entity {:wait-time 0
                           :t (- t duration)}) ;;still not up-to-date
           ;;have we handled the message?
           ;;what if time remains? this is akin to roll-over behavior.
           ;;we'll register that time is left over. We can determine what
           ;;to do in the next evaluation.  For now, we defer it.
          (bind!! {:current-message (.assoc ^clojure.lang.Associative msg :delta (- delta duration))}
                 )

          )))))
                
;;choose-state only cares about the entity.
(befn choose-state  ^behaviorenv [entity]
      (let [nxt (gen/case-identical? (:state (deref! entity))        
                    :dwelling  :deploying
                    :deploying :dwelling
                    :dwelling
                    )]       
        (push! entity :state nxt)))

(befn choose-time ^behaviorenv [entity]
      (let [twait
            (gen/case-identical? (:state (deref! entity))
              :dwelling  (unchecked-add (int 365)  (rand-int 730))
              :deploying (unchecked-add (int 230)  (rand-int 40)))]
        (push! entity :wait-time twait)))

(defn up-to-date? [e ctx] (== (:t e) (:t ctx)))

;;This will become an API call...
;;instead of associng, we can invoke the protocol.
(befn schedule-update  ^behaviorenv {:keys [entity ctx new-messages] :as benv}
      (let [st       (deref! entity)
            nm       (:name st)
            duration (:wait-time st)
            tnow     (:t (deref! ctx))
            tfut     (+ tnow duration)
            _        (debug 4 [:entity nm :scheduled :update tfut])
            ;_ (when new-messages (println [:existing :new-messages new-messages]))
            ]        
        (success (push-message- benv nm nm (->msg nm nm tfut :update)))))

(definline move [ctx]
  `(->and! [choose-state
            choose-time
            notify-change
            schedule-update] ~ctx))

;;pick an initial move, log the spawn.
(befn spawn ^behaviorenv {:keys [entity] :as ctx}
      (when (identical? (:state (deref! entity)) :spawning)
        (->and! [(->do (fn [_] (debug (str  "Entity " (:name (deref! entity)) " spawned"))))
                 move]
                ctx)))
       
;;the entity will see if a message has been sent
;;externally, and then compare this with its current internal
;;knowledge of messages that are happening concurrently.
(befn check-messages ^behaviorenv [entity current-messages ctx]
  (let [old-msgs     (fget (deref! entity) :messages)]
    (when-let [msgs  (pq/chunk-peek! old-msgs)]
      (let [new-msgs (rconcat (r/map val  msgs) current-messages)
            _        (b/swap!! entity (fn [^clojure.lang.Associative m]
                                        (.assoc m :messages
                                                (pq/chunk-pop! old-msgs msgs)
                                                )))]
            (bind!! {:current-messages new-msgs})))))

;;we need the ability to loop here, to repeatedly
;;evaluate a behavior until a condition changes.
;;->while is nice, or ->until
;;We'd like to continue evaluating the behavior (looping)
;;until some predicate is tripped.  Another way to do
;;this is to send a message....If there's time remaining,
;;we tell ourselves to keep moving, and move again prior to
;;committing.
(befn advance ^behaviorenv [entity ctx]
      (if (not (identical? (:state (deref! entity)) :spawn))
        (->and [wait-in-state move])
        spawn))
;;this is a dumb static message handler.
;;It's a simple little interpreter that
;;dispatches based on the message information.
;;Should result in something that's beval compatible.
;;we can probably override this easily enough.
;;#Optimize:  We're bottlnecking here, creating lots of
;;maps....

;;Where does this live?
;;From an OOP perspective, every actor has a mailbox and a message handler.
;;

;;type sig:: msg -> benv/Associative -> benv/Associative
;;this gets called a lot.
(defn message-handler [msg ^behaviorenv benv]
  (let [entity           (.entity benv)
        current-messages (.current-messages benv)
        ctx              (.ctx benv)]
    (do (println (str [(:name (deref! entity)) :handling msg]))
      (beval 
       (gen/case-identical? (:msg msg)
           ;;generic update function.  Temporally dependent.
           :update (if (== (:t (deref! entity)) (:t (deref! ctx)))
                     (do (success benv)) ;entity is current
                     (->and [(fn [^clojure.lang.Associative ctx] (success (.assoc ctx :current-message msg)))                           
                             advance
                             ]))
           :spawn  (->and [(push! entity :state :spawning)                        
                           spawn]
                          )
           ;;allow the entity to change its behavior.
           :become (push! entity :behavior (:data msg))
           :do     (->do (:data msg))
           (do (println (str [:ignoring :unknown-message-type (:msg msg) :in  msg]))
               (success benv)))
       benv))))

;;message handling is currently baked into the behavior.
;;We should parameterize it.

;;handle the current batch of messages that are pending for the
;;entity.  We currently define a default behavior.
(befn handle-messages ^behaviorenv {:keys [entity current-messages ctx] :as benv}
      (when current-messages
        (reduce (fn [acc msg]                  
                  (do ;(debug [:handling msg])
                    (message-handler msg (val! acc))))
                (success benv)
                current-messages)))

;;Basic entity behavior is to respond to new external
;;stimuli, and then try to move out.

;;This is a pretty typical prototype for entities to follow.
(befn default ^behaviorenv [entity]
      (->or [(->and [check-messages
                     handle-messages])             
             advance]))

(def args  (atom  nil))

;;note: if we change over to a set of coroutines running the ECS,
;;we can just put the message on their channel and let the coro
;;handle updates.

;;like ai/step-entity!, we should find a way to reuse it.
(defn step-entity! [ctx e msg]
  (let [^clojure.lang.ILookup  ent (atom (if (map? e) e (get-entity ctx e)))
        beh   (.valAt e :behavior default)
        beh   (if (identical? beh :default)
                  (do  (swap! ent assoc :behavior default)
                       default)
                  beh)
        benv (marathon.ces.behavior.behaviorenv.  ent
             beh ;right now there's only one behavior :default
             [msg]
             nil
             (atom ctx)
             msg)
        _    (println [:stepping (:name e) msg])
        _ (reset! args benv)]
    (-> (beval beh benv)
        (return!)
        (ai/commit-entity-))))

(comment 

;;[change width to time-in-state or something if possible...]
;;[we're using width here...]

;;We'll change this to look for an assigned deployment,
;;vs finding one.

;Primitive behaviors allow us to compose bottom->up.
;;where deploying is this...
(comment 
(befn deploying
      "Defines a deploying entity"
      [ctx entity]
      (let [[x y] (:position entity)]
        (when (should-deploy? x (:width board)) ;in big need of changing.
          (let [destination (quilsample.board/random-region)]
            (bind! {:board
                    (store/deploy-entity! board (:name entity) (:state entity) x y
                                          :location (:location entity)
                                          :destination destination)})))))



;;[we're using width here...]
(befn resetting
      "Entity behavior when reset."
      [ctx entity]
      (let [[x y] (:position entity)]
        (when (or (should-reset?  x  (:width board))                  
                  (and (deployed?   entity)
                       (deployment-over? y)))
          (bind! {:board (ctx/reset-entity! ctx entity)}))))

)

;;[depends on store]


;;goal here is to get the unit to accumulate dwell.
;;time, specifically deltat, must be given.

;;note that the bound entity isn't changing...
;;we might want a reference after all..
;;or we use the entity-handle to vary the board. Alter the entity, broadcast,
;;then commit the entity afterward.
(comment

  (befn  moving
  "Behavior that defines an entity's movement"
  [ctx entity]            
  (let [{:keys [name policy]} entity
        [x   y]   position
        [dx  dy]  velocity
        new-ctx (-> ctx 
                    )]
    (bind! {:ctx new-ctx})))

  )

;;Only Movement....
;;===================

;;How would the behavior tree look rooted at update-state-beh, so that 
;;we get an equivalent behavior tree to a unit that ONLY knows how to 
;;move (i.e. follow a script).

;;we need to implement movement-beh 
;;so, the movement behavior depends on several subgoals...
;;To get an entity to move, 
;;We need to know where it's moving to, and make that part of the context
;;We (may) need to know where it currently is....
;;We (may) need to know what it's supposed to after it gets there
;;(default is to wait)
;;We (may) need to know how long it's supposed to wait (if it's
;;waiting).

;;So, at the high level, we have a simple behavior that checks to see
;;if it can move, finds where to move to, starts the process of
;;moving (maybe instantaneous), and waits...
;;Maybe our implied behavior is really [move wait]
;;move == 
;; [should-move? get-next-location move-next-location]
;;move(transport-time)== 
;;  [should-move? get-next-location (move-next-location transport-time)]
  
;;We should consider move if our time in state has expired, or 
;;if we have a next-location planned.
(befn should-move? [bb next-position statedata] 
  (or next-position
      (zero? (remaining statedata))
      (spawning? bb)))

;;simple predicates, semi-monadic interface....
;; (?  should-move? [*env* *statedata*]
;;         (or (:next-position *env*)
;;             (zero? (remaining *statedata*))))
;; (!  record-move  (put! :moved true))       
                  
;(def record-move (->alter #(set-bb % :moved true)))
(befn record-move []
      (bind! {:moved true}))

;;after updating the unit bound to :entity in our context, 
;;we commit it into the supplystore.  This is probably 
;;slow....we may want to define a mutable version, 
;;or detect if mutation is allowed for a faster update
;;path.  For instance, on first encountering the unit,
;;we establish a mutable cell to its location and use that 
;;during the update process.
;(def commit-entity (->alter merge-updates))

;;I like looking at unit updates like transactional semantics.
;;We probably want to pull out the unit and establish a mutable
;;context if we find out we have to change it.  For instance, 
;;we can just bind the mutable cell to the :entity in the context,
;;and then detect if mutation (or even simple change) has occurred.
;;Another way to do this is to just check identity (since we're using 
;;immmutable objects) and 
;;Commmit our changes iff we recorded a change to the unit (we 
;;may not during the course of the update)
;; (def commit-if-changed 
;;   (->and [(->pred :changed)
;;            commit-unit]))

(defmacro get-else [m k v]
  `(if-let [res# (get ~m ~k)]
     res#
     ~v))

;;Given that we have the context for a move in place, 
;;we want to move as directed by the context.  If there 
;;is a wait time associated with the place we're moving 
;;to, we will add the wait-time to the context.  That way,
;;downstream behaviors can pick up on the wait-time, and 
;;apply it.
(defn apply-move [ctx] 
    (if-let [nextpos (next-position ctx)] ;we must have a position computed, else we fail.                                       
      (let [t        (get-bb ctx :tupdate) ;(tupdate ctx)
            u        (entity  ctx)
            frompos  (get     u      :positionpolicy)
            wt       (get-wait-time  u  nextpos ctx)]
        (success
         (if (= frompos nextpos)  
           ctx ;do nothing, no move has taken place.          
           (let [newstate (get-state u nextpos)
                 ;;#Todo change change-state into a fixed-arity
                 ;;function, this will probably slow us down due to arrayseqs.
                 new-sd   (fsm/change-state (statedata ctx) newstate wt)
                 new-u    (traverse-unit u t frompos nextpos)]
             (->> (merge-bb ctx ;update the context with information derived
                                        ;from moving
                            {:entity       new-u
                             :old-position frompos ;record information 
                             :new-state    newstate
                             :statedata    new-sd
                             :new-duration wt})
                  (u/unit-moved-event! new-u nextpos)
                  )))))
      (fail ctx)))

;;#Todo change change-state into a fixed-arity
          ;;function, this will probably slow us down due to arrayseqs.
(befn apply-state [statedata new-duration]
      (when-let [new-state (get-bb ctx :new-state)]
        ;;update the context with information derived from moving
        (bind! {:statedata (fsm/change-state statedata new-state  new-duration)})))


;; (defn apply-state [ctx] 
;;   (if-let [new-state (get-bb ctx :new-state)]
;;     (let [
;;           new-sd   (fsm/change-state (statedata ctx) new-state (get-bb ctx :new-duration))]
;;       (success 
;;        (merge-bb ctx ;update the context with information derived
;;                                         ;from moving
;;                  {:statedata    new-sd})))
;;     (fail ctx)))

;;consume all the ambient changes in the blackboard, such as the
;;statedata we've built up along the way, and pack it back into the 
;;unit for storage until the next update.
;; (defn update-entity [ctx]
;;   (if-let [

(comment 
(def apply-changes (->and [apply-move 
                           apply-state                                                    
                           ]))
)

(befn apply-changes []
      (->and [apply-move
              apply-state]))

;; ;;apply-state? should update the entity's state, change the duration
;; ;;to be the current wait-time, etc.
;; (defn apply-state [ctx] 
;;   (if-let [nextpos (next-position ctx)] ;we must have a position computed, else we fail.                                       
;;     (let [new-state (get-bb :new-state)]
;;         (success
;;          (merge-bb ctx ;update the context with information derived
;;                                         ;from moving
;;                    {:entity       (traverse-unit u t frompos nextpos)
;;                     :old-position frompos ;record information 
;;                     :new-state    newstate})))
;;     (fail ctx)))

(comment 
(def set-next-position  
  (->alter #(let [e (entity %)
                  p (get-next-position e  (:positionpolicy e))]
              (merge-bb %  {:next-position  p
                            :wait-time     (get-wait-time e p %)}))))
)

(befn set-next-position [ctx entity]
      (let [e entity
            p (get-next-position e (:positionpolicy e))]
        (bind! {:next-position p
                :wait-time (get-wait-time e p ctx)})))


;; (def find-move  (->and [should-move? 
;;                         set-next-position ;;the problem here is that
;;                         ;;we don't see what's being changed....the
;;                         ;;context is changing, but where? At least
;;                         ;;it's not side-effecting, but can we keep
;;                         ;;track of our changes better?
;;                         ]))

(befn find-move []
      (->and [should-move?
              set-next-position]))


;;We know how to wait.  If there is an established wait-time, we
;;request an update after the time has elapsed using update-after.
(defn wait [ctx]
  (if-let [wt (wait-time ctx)] ;;if we have an established wait time...    
    (->> (if (zero? wt) 
           ctx ;skip the wait, instantaneous.  No need to request an
               ;update.
           (log! (str "waiting for " wt) (update-after  wt ctx)))           
         (success))
    (fail      ctx)))
    
;;Movement is pretty straightforward: find a place to go, determine 
;;any changes necessary to "get" there, apply the changes, wait 
;;at the location until a specified time.
(def moving-beh 
  (->and [find-move
          apply-changes
          wait]))


(comment
;;Composed behaviors
;;==================
;;how can we handle messages?
;;Do we use a message handling behavior?
;;Is there a consistent behavior, regardless of the message?

;;can we make beval work on behaviors?
(befn try-deploying
      [entity]
      (let [y (second (:position entity))]     
        (if  (deployment-over? y)                         
          resetting
          moving)))

;;One way to define dwell...
;;Each of these is a contextual function that operates on
;;the bulletin board, and maps ctx->ctx.

;;->or is our logical version of cond...
;;if checks behaviors in order, until one succeeds, or all fail.
(def try-dwelling
  (behavior/->or [deploying ;this hoses us...we end up "deploying" twice.
                  resetting
                  moving]))
)





)
