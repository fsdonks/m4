;;A namespace for simple entity behaviors.  Trying to decouple our
;;stub behaviors from the drawing/entityboard implementation.
(ns quilsample.behavior
  (:require [spork.ai.behavior :as behavior
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
                     ->pred
                     ->or
                     ->bnode
                     ->while
                     always-succeed
                     always-fail
                     bind!
                     befn]]))

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
(befn should-move? [ctx bb entity] 
  (->pred #(or (get ctx :next-position)
               (zero? (remaining (get bb :statedata {})))
               (spawning? bb))))

;;simple predicates, semi-monadic interface....
;; (?  should-move? [*env* *statedata*]
;;         (or (:next-position *env*)
;;             (zero? (remaining *statedata*))))
;; (!  record-move  (put! :moved true))       
                  
(def record-move (->alter #(set-bb % :moved true)))

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

(defn apply-state [ctx] 
  (if-let [new-state (get-bb ctx :new-state)]
    (let [;;#Todo change change-state into a fixed-arity
          ;;function, this will probably slow us down due to arrayseqs.
          new-sd   (fsm/change-state (statedata ctx) new-state (get-bb ctx :new-duration))]
      (success 
       (merge-bb ctx ;update the context with information derived
                                        ;from moving
                 {:statedata    new-sd})))
      (fail ctx)))

;;consume all the ambient changes in the blackboard, such as the
;;statedata we've built up along the way, and pack it back into the 
;;unit for storage until the next update.
;; (defn update-entity [ctx]
;;   (if-let [
  
(def apply-changes (->and [apply-move 
                           apply-state                                                    
                           ]))

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

(def set-next-position  
  (->alter #(let [e (entity %)
                  p (get-next-position e  (:positionpolicy e))]
              (merge-bb %  {:next-position  p
                            :wait-time     (get-wait-time e p %)}))))

(def find-move  (->and [should-move? 
                        set-next-position ;;the problem here is that
                        ;;we don't see what's being changed....the
                        ;;context is changing, but where? At least
                        ;;it's not side-effecting, but can we keep
                        ;;track of our changes better?
                        ]))

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
