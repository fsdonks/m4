;;A library for defining entity behaviors.  Legacy implementation 
;;only covered unit entities, and used a Finite State Machine (FSM).
;;New implementation should be more general, and follows the Behavior 
;;Tree design as presented by A.J. Champagnard.
;;Revised notes:
;;Under the functional paradigm, behaviors are first-class state
;;transition functions.  They compose other first-class state
;;transitions functions to affect a change of state on the simstate.

;;We focus on associating the state transition in the unit/entity
;;behavior.  The other "system" behaviors act a coarse LOD, and really
;;only have one thing they're modifying, so they tend to be a little
;;simpler, BUT the concept is identical (as are the signatures).
;;The marathon engine, event-step marathon, is just a (sequential)
;;composition of these implicit states (i.e. managing supply, managing
;;demand, blah....)

;;If we can hone in on a protocol for all states/behaviors, then
;;we have the same notion of atomic and composite behaviors.

;;States are simple behaviors, they do not overtly determine
;;what the transition is.
;;State transitions are captured explicitly via a graph in the FSM
;;or implicitly via allowing the states to call eachother directly.

;;The legacy scheme mixes these together....our states are all
;;functions that transform units, they accomplish system-wide
;;state transitions by firing events, and updating other parts of
;;the system.  In some cases, we allow the state to invoke another
;;immediately, affecting an instantaneous transfer.

;;Most states are driven by the unit's policy....the policy provides
;;a state-transition graph of some of the states, along with wait
;;times.

;;Some state transitions are not encoded in policy, but affected
;;via function calls that codify the implicit transitions.

;;There is a main "Driver" or loop that prosecutes state transitions
;;until the state machine reaches a (currently) halting state.  This
;;is typically indicated by running out of states to transition to,
;;or running out of time (having time remaining in the current state).


;;It probably behooves us to formalize the implicit conventions above.
;;Some basic principles:
;;  We have one or more states (functions) that serve to transition
;;  an entity relative to the current context.
;;  State transitions happen relative to time....
;;  State transitions may happen instantaneously, i.e. blip states.
;;  State transitions also happen relative to the whole context,
;;  since information is propogated about the entity's changes...
;;  When an entity has time remaining in a state, we simply update
;;  the entity with said state.
;;   [In behavior trees, this is known as being "done" in the current
;;   behavior"]
;;  States may have pre and post conditions (i.e. on-enter and on-exit
;;  conditions).
;;  Updates, over a period of time, may trigger moving to a new state
;;  (or behavior).
;;  External forces can cause the state to become invalidated, or
;;  there may be a directed state transition that occurs....

;;How can we formulate the building blocks for these concerns, so that
;;it becomes easy to define isolated states, their effects on the
;;entity in question and the broader simcontext, and the ordering
;;of states?

;;One observation is that the only difference between state machines
;;and behavior trees is the ordering of the state change mechanism.
;;FSMs are defined by nodes in the graph (the states), and the arcs
;;that connect them (arc weights include some notion of transition
;;action, i.e. "wait 10 seconds" or "go immediately" or "trigger this
;;event then change".

;;Behavior trees impose an ordering by embedding states in, at a
;;minumum, a tree structure.  The current state exists as a leaf node
;;somewhere in this structure.  State changes happen as the
;;current behavior completes, any sibling behaviors are visited in
;;an depth-first in-order traversal.

;;BTs are pretty damn convenient from an FP perspective (nice and
;;composeable)....is there anything that we can't compose with states
;;though?  
(ns marathon.data.behavior
  (:require [marathon.data.protocols :as protocols]
            [marathon.data.fsm :as fsm]
            [marathon.sim [core :as core]
                          [unit :as u]
                          [demand :as d]
                          [supply :as supply]
                          ]            
            [spork.sim.simcontext :as sim]))

(defrecord behaviorstore [name behaviors])

;;#Pending  - Import Behavior Implementations From marathon.sim.legacy

;;_Begin Porting Legacy Behavior_
;;THe strategy here is to pull in what we originally had, translate it
;;to clojure, and then see if any patterns jump out.  We know we're
;;working with a finite state machine at the moment.  We may find it
;;easier to go with behavior trees or something more compositional
;;later;

;;For the time being, we have our good ole' case statement that
;;switches between states.  So our behavior is really a limited
;;interpreter.

;;THe task at hand is to port the functions used by said interpreter,
;;because they will probably be used by anything we use (even if we go
;;the non-fsm route).

;;some stubs for necessary functions....at the moment they aren't clear.
(declare update-behavior init-unit-behavior change-state)

;;Legacy Implementation
;;======================

;;In an effort to simplify the original implementation for entity
;;behaviors, I am porting them to composeable behavior trees.
;;The current task is to determine how to map the existing FSM
;;into a set of behaviors.

;;The API for the legacy implementation is focused on two functions:
;;update      :: unitdata -> deltat -> unitdata
;;changeState :: unitdata -> tostate -> deltat -> *duration ->
;;              *followingstate -> unitdata

;;Both functions incorporate the notion of possible time delta;  
;;Both functions also return the updated unit.

;;update is the more standard usage, as it's purely a function of
;;time.  The 90% use-case is that the simulation is ticking, prompting
;;time changes.  We actually have several concurrent simulations (unit
;;entities) that are flowing through the supply.  The simulation clock
;;is serving as a central point for coordinating time changes.
;;As the clock ticks, we advance time.  Units are scheduled for
;;updating at specific intervals (typically at their request).
;;The update consists of translating a deltat into an effect on
;;the unit (and transitively the broader simulation writ large).
;;So, we have a somewhat complex unit behavior codified as an FSM
;;that follows a script (a rotational policy), and advances the unit's
;;position over multiple time-slices (deltat's).  Note that the
;;time-slices are not arbitrary....for instance the units will request
;;updates for themselves at eventful points; say according to a
;;policy, their next move should be to go transition to a new location
;;and wait for 100 units of time.  They make the state change, i.e.
;;record the new location, record the fact that they are waiting for
;;100, and request an update at 100 steps after the current time.
;;Thus, in the ideal case, the entity will automatically follow its
;;built-in behavior, which is to move periodically from place to place
;;as a function of its current policy position and the time in that
;;position, according to a scripted policy it's following.
;;This is a happy-medium for re-usable, data-driven behavior, in that
;;the entity is just a policy-interpreter with a swath of pre-defined
;;behavior.

;;The other 10% use case is when we need to directly intervene in
;;an entity's life outside of the passage of time; specifically
;;an external event "causes" the entity to change its state,
;;typically to a state determined a-priori.  ChangeState gives us
;;access to the entity's internal mechanisms to accomplish this,
;;perhaps via an event or an arbitrary (formerly) method call.
;;In the case of unit entities, we see this happening when there
;;are unscheduled deployments, or unplanned withdraws from a demand,
;;etc.  Basically, we deviate from the current script, and drop the
;;entity somewhere else in the script.  So long as we stay "somewhere"
;;in the script, the entity can pick up and continue following along
;;without caring why, or caring where it came from.

;;In fact, ChangeState is a primitive call for other states to use,
;;and serves as what would be more commonly found as the familiar,
;;"onEnter, onUpdate, onExit" set of methods in the State design
;;pattern.  Other states, like MoveState, are typical targets for
;;ChangeState, since 99% of the entity's behavior is following a
;;script that tells it where to move and for how long.

;;If we're thinking of moving to a distributed environment,
;;We can (probably should?) unify the interface as having each entity
;;exist as a formal process that receives messages.  In this case,
;;even time sliced updates are messages.  Currently it's a technical
;;distinction.

;;The centers of gravity for a useful entity (something that moves of
;;its own volition, enough for a supply simulation), is to have the
;;behavior implement (update), and understand how to read from a
;;script (i.e. the unit's policy).

;;We can generalize and implement an "onEvent" or similar function
;;later, behind which we can wrap our changestate ops.

;;Also note that many of the states operate not only on the
;;unit entity, but on the simulation context; in many cases,
;;information is propogated via the event system (typically
;;instantaneous events, mostly for logging, ui stuff).

;;So, we are likely to include, either directly or in a
;;contextual binding, the current simulation context, when
;;we go to update one or more units.

;;Transitiong To Behavior Trees
;;=============================
;;Changing from the current policy-driven FSM, we need to start at the
;;choke-points so-to-speak, and then grow from there.  The nice thing
;;about behaviors is that they compose nicely....so you can build much
;;more complex behaviors from smaller, simpler behaviors.  We should
;;also be able to analyze, and possibly "compile" behavior trees at
;;some point.  For now though, we'll focus on building at least two
;;fundamental behaviors.

;;The philosophy behind our behavior tree is that the tree contains
;;all the possible actions a unit entity could take, inlcuding the
;;sequences and conditions (and joint conditions) therein.  The tree
;;should structure the logic of our entity's behavior such that its
;;depth-first traversal embodies the "hierarchy" of concerns for the
;;entity.  Typically, the AI folks will dictate that this is analagous
;;to the hierarchy of needs;  higher-priority needs occur earlier in
;;the tree;  With our naive tree, we always check from the root and
;;traverse until a stopping criterion is met;  this means we can
;;accomplish the same reactive "philosophy" of the existing FSM;
;;we should be able to make simple changes to the state of our
;;entity, and the behavior tree - upon re-evaluation - should be
;;able to suss out what it should be doing.


;;One strategy could be to isolate the state-independent parts of the 
;;existing logic and identify them as behaviors; in other words, 
;;remove all state-changes.  We then have the idiom of defining 
;;small, self-contained behaviors, and where we previously "wanted"
;;a state change, we realize a sequence of the simple behavior, 
;;and the state change.

;;This doesn't refactor anything, but it does allow us to translate
;;fairly directly to a behavior tree representation.

;;For instance, the control flow of the Moving state would establish
;;the context of a move based on policy, and then evaluate an
;;instantaneous call to ChangeState afterwards.  We can view that 
;;as a behavior like the following: 
;;               Moving         
;;[SetNextMove SetWaitTime LogMove WaitInNextState]

;;So one strategy is to just break out all of the implicit unit entity
;;state changes we're performing and them build our behaviors out 
;;of them.  As we go along, we can re-use previous actions, or 
;;where apprioriate, entire behaviors.

;;We may give some thoughts to extensions for our naive behavior tree
;;as well: 
;;  Behavior zippers (so we can remember the path we followed to our
;;                    current child)
;;  Specific types that denote success, running, failure


;;Implementation
;;==============

;;For now, we'll let the behavior tree assume it has everything it
;;needs in its context. 
;;The context is a simple map; we may move to a record type as an
;;optimization later, particularly if there are well-known fields 
;;that we'll be accessing frequently.  The context acts as a
;;"blackboard" for the nodes in the behavior tree to work with.

;;Note-> there are opportunities for using STM and exploiting
;;parallelism here; if we implement a parallel node, we may 
;;enjoy the benefits of "fast" entity updates.  On the other hand, 
;;since supply updating takes the preponderance of our time, 
;;we can still get a lot of bang-for-the-buck by updating individual
;;units in parallel batches.  Parallelizing the behavior tree may 
;;not be all that necessary.

;;Note: if we do implement a parallel node (even if executed
;;serially), we can have competing concerns executed in parallel 
;;(i.e. listen for messages, and also update over time slices).

;;Behavior Tree Core, temporarily copied from marathon.data.behaviorbase
(def behaviors nil)
(defprotocol IBehaviorTree
  (behave [b ctx]))
  

(defrecord bnode [type status f data]
  IBehaviorTree
  (behave [b ctx] (f ctx))
  ;; clojure.lang.IFn 
  ;; (invoke [obj arg] (f arg))
  )

(defn behavior? [obj] (satisfies? IBehaviorTree obj))

(defn beval [b ctx]
  (cond (behavior? b) (behave b ctx)
        (fn? b) (b ctx)))

;;convenience? macros...at least it standardizes success and failure,
;;provides an API for communicating results.
(defmacro success [expr]
  `(vector :success ~expr))
(defmacro fail [expr]
  `(vector :fail ~expr))
(defmacro run [expr]
  `(vector :run ~expr))

;;note, behaviors are perfect candidates for zippers...
(defn ->leaf [f]    (->bnode  :leaf nil  (fn [ctx]  (f ctx)) nil))
(defn ->pred [pred] (->bnode  :pred nil  (fn [ctx] (if (pred ctx) (success ctx) (fail ctx))) nil))
(defn ->and  [xs]
  (->bnode  :and nil
     (fn [ctx]
      (reduce (fn [acc child]
                (let [[res ctx] (beval child (second acc))]
                  (case res
                    :run       (reduced (run ctx))
                    :success   (success ctx)
                    :fail      (reduced [:fail ctx])))) (success ctx) xs))
     xs))

(defn ->or  [xs]
  (->bnode  :or nil 
     (fn [ctx]
       (reduce (fn [acc child]
                 (let [[res ctx] (beval child acc)]
                   (case res
                     :run       (reduced (run ctx))
                     :success   (reduced (success ctx))
                     :fail      (fail ctx)))) ctx xs))
     xs))

(defn ->not [b]
  (->bnode  :not nil
      (fn [ctx] (let [[res ctx] (beval b ctx)]
                   (case res
                     :run     (run     ctx)
                     :success (fail    ctx)
                     :fail    (success ctx))))
      b))

;;if a behavior fails, we return fail to the parent.
;;we can represent a running behavior as a zipper....
;;alternatively, we can just reval the behavior every time (not bad).
(defn ->alter  [f] (->bnode :alter nil (fn [ctx] (success (f ctx))) nil))
(defn ->elapse [interval]                            
    (->alter #(update-in % [:time] + interval)))

;;always force success
(defn always-succeed [b]
  (fn [ctx] (success (second (beval b ctx)))))
;;always force failure
(defn always-fail [b]
  (fn [ctx] (fail (second (beval b ctx)))))



;;a behavior that waits until the time is less than 10.
(defn ->wait-until [pred]
  (->bnode  :wait-until nil 
          (fn [ctx] (if (pred ctx) (success ctx) (run ctx)))    nil))

;;do we allow internal failure to signal external failure?
(defn ->while [pred b]
  (->bnode :while nil   
           (fn [ctx] (if (pred ctx) 
                       (beval b ctx)
                       (fail ctx))) 
           b))
          
(defn ->elapse-until [t interval]
  (->while #(< (:time %) t)
            (->elapse interval)))

(defn ->do [f] 
  (fn [ctx] (success (do (f ctx) ctx))))



;;Accessors
;;=========

;;Accessors for our behavior context.
;;We don't "have" to do this, but I wanted to lift up
;;from raw lookups and formalize the interface.  we seem to be using
;;these alot, enough to warrant a new idiom possibly.

;;Get an item from the blackboard, which is stored in the simstate of 
;;the context, under :state 
(defn get-bb   
  ([ctx k]      (get (core/get-blackboard ctx) k))
  ([ctx k else] (get (core/get-blackboard ctx) k)))
(defn set-bb   [ctx k v]  (core/set-blackboard ctx (assoc (core/get-blackboard ctx) k v)))
(defn merge-bb [ctx m]    (core/set-blackboard ctx (merge (core/get-blackboard ctx) m)))

;;we could use a macro here....maybe later..

(defn entity        [m] (get-bb m :entity))
(defn from-position [m] (get-bb m :from-position))
(defn next-position [m] (get-bb m :next-position))
(defn tupdate       [m] (get-bb m :tupdate))
(defn deltat        [m] (get-bb m :deltat))
(defn wait-time     [m] (get-bb m :wait-time))
(defn statedata     [m] (get-bb m :statedata))

(defn eget [m k]
  (if-let  [res (get m k)]
    res
    (throw (Exception. (str "Expected to find key " k " in " m)))))

;;aux function
(defn remaining [statedata]
  (- (eget statedata :duration) 
     (eget statedata :timeinstate)))

;; Private Function getState(unit As TimeStep_UnitData, position As String) As String
;; getState = unit.policy.getState(position)
;; End Function

;;#TODO   See if we can encode or derive a more meaningful semantics
;;from the indices currently associated with the states...for
;;instance, :deployable randomly came back with 7 as a state, we
;;either don't want this or we want to have it mean something.
(defn get-state [unit position] 
  (let [s (protocols/get-state (:policy unit) position)]
    (if (number? s)  :dwelling s)))
    

;; 'Tom Change 24 May 2011
;; Private Function getNextPosition(unit As TimeStep_UnitData) As String
;; getNextPosition = unit.policy.nextposition(unit.PositionPolicy)
;; End Function

(defn get-next-position [unit position] (protocols/next-position (:policy unit) position))

;; Private Function getWaitTime(unit As TimeStep_UnitData, position As String, Optional deltat As Single) As Single
;; Dim nextposition As String

;; With unit
;;     nextposition = .policy.nextposition(position)
;;     getWaitTime = .policy.TransferTime(position, nextposition)
;;     getWaitTime = getWaitTime - (deltat - .StateData.remaining) '<- this gets the "next" transfer.
;; End With

;; End Function

(defn get-wait-time
  ([unit frompos topos ctx]
     (let [wt (protocols/transfer-time (:policy unit) frompos topos)
           deltat (or  (deltat ctx) 0) ;allow the ctx to override us...
           ]
       (- wt (remaining (statedata ctx)))))
  ([unit position ctx]
   (let [np (get-next-position unit position)
         wt (protocols/transfer-time (:policy unit) position np)
         deltat (or  (deltat ctx) 0)]
     (- wt (remaining (statedata ctx)))))
  ([position ctx] (get-wait-time (entity ctx) position ctx))
  ([ctx] (wait-time ctx)))

;;todo# move to generic statedata library.
;;this should be lifted out
;;This just keeps track of where we are, how long we're waiting, and 
;;where we expect to be after.
(defn change-statedata [sdata tostate duration followingstate]
  (-> statedata 
      (assoc :tostate  tostate)
      (assoc :duration duration)
      (assoc :timeinstate 0)
      (assoc :followingstate followingstate)))

;;auxillary function to help us lock down things like unit updates,
;;things that should never fail.  Note, we lose some of the context 
;;if we're not tracing the behavior evaluation live; that still needs
;;to be implemented.
(defn error-on-fail [bres]
  (if (identical? (first bres) :success) bres 
      (throw (Exception. (str "behavior returned failure...")))))    

;;Basic API
;;=========
;;The rest of the simulation still relies on our pre-existing API, 
;;namely that we have "change-state", and "update"


;;note that change-state already exists in marathon.sim.unit/change-state, 
;;we're merely providing an interface to the unit's behavior for it.
;;Also note that change-state is only called (currently) from
;;marathon.sim.demand (for abrupt withdraws), and marathon.sim.supply
;;(for deployments).

(declare change-state-beh update-state-beh update-state 
         roll-forward-beh)
;;API
;;===

;;These are the entry points that will be called from the outside.
;;Under the legacy implementation, they delegated to a hard coded
;;finite state machine that interpreted rotational policy to infer
;;state transitions.  The general mechanism is to augment the
;;simulation context.  We may want to define a single function
;;load-context and unload-context the clears up any augmented
;;contextual items we put in.  That, or manage the simulation
;;context separate from the behavior context.  For now, managing
;;the simcontext along with the behavior context (treating it
;;as a huge blackboard) seems like the simplest thing to do.

;;Similarly, we'll have update take the context last.
;;update will depend on change-state-beh, but not change-state.
;;change-state is a higher-level api for changing things.
(defn update  [unit deltat ctx]  
  (->> (merge-bb ctx {:tupdate (sim/current-time ctx)
                                    :entity  unit
                                    :deltat  deltat})
       (roll-forward-beh) ;update the unit according to the change in time.
       (error-on-fail)    ;unit updates should never fail.
       (second  ;result is in terms of [:success|:fail ctx], pull out
                                        ;the ctx
        )))

;;This implementation takes the context last.
(defn change-state [unit to-state deltat ctx]
 (beval (->and [#(update unit deltat %) ;ensure the unit is up-to-date
                  change-state-beh])    ;execute the change
        (merge-bb ctx {:tupdate (sim/current-time ctx)
                       :entity   unit
                       :deltat   deltat})))

;;we can probably define a macro for contextual behaviors that
;;define preconditions for executing, i.e. ensuring that we have
;;an entity, we have a deltat, etc. in the context, and bind to them
;;if we do.  If we don't have them, then we throw an error because our
;;expectations failed...(todo)



;;note - another way to handle this is to record dirty entities
;;and update them appropriately.

;;note-we have a wait time in the context, under :wait-time
;;updates an entity after a specified duration, relative to the 
;;current simulation time + duration.
(defn update-after 
  ([entity duration ctx]
     (core/request-update (+ (sim/current-time ctx) duration) 
                          (:name entity)
                          :supplyupdate
                          ctx))
  ([duration ctx] (update-after (entity ctx) duration ctx)))

;;auxillary function that helps us wrap updates to the unit.
(defn traverse-unit [u t from to]   
  (-> u 
      (assoc :positionpolicy to)
      (u/add-traversal t from to)))

;;this is kinda weak, we used to use it to determine when not to
;;perform updates via the global state, but it's probably less
;;important now...
(defn special-state? [s] (or (identical? s :spawning)
                             (identical? s :abrupt-withdraw)))

(defn just-spawned?  [ctx] (==  (:spawntime (entity ctx))    (core/get-time ctx)))
(defn state-expired? [ctx] (<=  (remaining (statedata  ctx)) (deltat ctx)))

(defn to-position?   [to   ctx] (identical? (next-position ctx) to))
(defn from-position? [from ctx] (identical? (from-position ctx) from))
;;Behaviors
;;=========
;;our idioms for defining behaviors will be to unpack 
;;vars we're expecting from the context.  typically we'll 
;;just be passing around the simulation context, perhaps 
;;with some supplementary keys.
(def change-state-beh 
  (fn [{:keys [entity deltat statedata newstate duration followingstate] 
        :or   {deltat 0 duration 0 } :as ctx}]
    (let [followingstate (or followingstate newstate)
          newdata (change-statedata statedata newstate duration followingstate)
          ctx     (set-bb ctx :statedata newdata)]
      (cond
        ;state change inducing a wait until update.
        (pos? duration)  (success (update-after  entity duration ctx))
        ;immediate change, force an update.
        (zero? duration) (success (update-state-beh ctx)) 
        :else (throw   (Exception. "Error, cannot have negative duration!"))))))

;;implement update-state-beh
(def change-and-update (->and [change-state-beh update-state-beh]))

;;we want to update the unit to its current point in time.  Basically, 
;;we are folding over the behavior tree, updating along the way by 
;;modifying the context.  One of the bits of context we're modifying 
;;is the current deltat; assumably, some behaviors are predicated on 
;;having a positive deltat, others are instantaneous and thus expect 
;;deltat = 0 in the context.  Note, this is predicated on the
;;assumption that we can eventually pass time in some behavior....

(defmacro if-y [expr & else]
  `(if (= (clojure.string/upper-case (read)) "Y")
     ~expr 
     ~@else))
;;Note -> we could represent roll-forward-beh as something more
;;btree-ish, specifically as a combination of a repeat node and 
;;some conditions (namely the condition that deltat <= remaining)
;;Might have a smaller behavior that advances the next-smallest 
;;time slice.  TODO# refactor using behaviortree nodes.
(def roll-forward-beh 
  (fn [ctx]
    (let [deltat (get-bb ctx :deltat 0)]
      (loop [dt  deltat
             ctx ctx]
        (let [sd (statedata ctx)              
              _ (println [:sd sd])
              timeleft (remaining sd)
              _ (println [:rolling :dt dt :remaining timeleft ]) ]
          (if-y 
            (if (<= dt timeleft)           
              (beval update-state-beh ctx)
              (let [[stat nxt] (beval update-state-beh (set-bb ctx :deltat dt))]
                (if (= stat :success) 
                  (recur  (- dt timeleft) ;advance time be decreasing delta
                          nxt)
                  [stat nxt])))
            ctx))))))
  
;;What then, is the update-state-beh definition? 
;;This "was" our central dispatch for states, i.e., based on 
;;the current state, we would lookup the appropriate handler and 
;;call it as the update function.

;;It seems to me, there will be no "central dispatch" necessarily, 
;;but these branches (the states) become leaves (or subtrees) to be 
;;re-used with other behaviors.

;;either that, or the update-state-beh is the hierarchical 
;;composition of the entire tree, specifically it encodes everything 
;;explicitly (including state transitions).

;;The later definition seems more consistent with the behavior tree
;;approach.  We want the whole enchilada to be updated every time
;;step.


;;We assume the the entity-behavior is stored in the context....
;;so, update-state-beh is just using the behavior associated with the
;;entity...
(declare default-behavior) ;;we need to make a default root behavior..

;;Heh, that's pretty weak, but it's technically correct
;;All we do is provide a reference to the time the update, note we can
;;easily override the default behavior by providing a root-behavior in
;;the context.  This is pretty cool for things like overriding
;;behaviors if we have temporary effects.
(defn update-state-beh [{:keys [root-behavior] :as ctx
                         :or   {root-behavior default-behavior}}]
  (beval root-behavior ctx))

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
(def should-move?
  (->pred #(or (get % :next-position)
               (zero? 
                (remaining (get-bb % :statedata {}))))))

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
(def commit-unit
  (->alter 
   (fn [ctx] 
     (sim/merge-updates 
      {:supply (supply/add-unit (entity ctx) 
                                (core/get-supplystore ctx))}
      ctx))))

;;I like looking at unit updates like transactional semantics.
;;We probably want to pull out the unit and establish a mutable
;;context if we find out we have to change it.  For instance, 
;;we can just bind the mutable cell to the :entity in the context,
;;and then detect if mutation (or even simple change) has occurred.
;;Another way to do this is to just check identity (since we're using 
;;immmutable objects) and 
;;Commmit our changes iff we recorded a change to the unit (we 
;;may not during the course of the update)
(def commit-if-changed 
  (->and [(->pred :changed)
           commit-unit]))

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
      (let [t        (tupdate ctx)
            u        (entity  ctx)
            frompos  (get     u      :positionpolicy)
            wt       (get-wait-time  u  nextpos ctx)]
        (success
         (if (= frompos nextpos)  
           ctx ;do nothing, no move has taken place.          
           (let [newstate (get-state u nextpos)
                 ;;#Todo change change-state into a fixed-arity
                 ;;function, this will probably slow us down due to arrayseqs.
                 new-sd   (fsm/change-state (statedata ctx) newstate wt)]
             (merge-bb ctx ;update the context with information derived
                                        ;from moving
                {:entity       (traverse-unit u t frompos nextpos)
                 :old-position frompos ;record information 
                 :new-state    newstate
                 :statedata    new-sd
                 :new-duration wt})))))
      (fail ctx)))

(defn apply-state [ctx] 
  (if-let [new-state (get-bb ctx :new-state)]
    (let [;;#Todo change change-state into a fixed-arity
          ;;function, this will probably slow us down due to arrayseqs.
          new-sd   (fsm/change-state (statedata ctx) new-state (get-bb :new-duration))]
      (success 
       (merge-bb ctx ;update the context with information derived
                                        ;from moving
                 {:statedata    new-sd}))
      (fail ctx))))

;;consume all the ambient changes in the blackboard, such as the
;;statedata we've built up along the way, and pack it back into the 
;;unit for storage until the next update.
;; (defn update-entity [ctx]
;;   (if-let [
  

(def apply-changes (->or [apply-move 
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
           ctx ;skip the wait, instantaneous.  No need to request an update.
           (update-after  wt ctx))
         (success)
         (core/debug-print (str "waiting for " wt)))
    (fail      ctx)))
    
;;Movement is pretty straightforward: find a place to go, determine 
;;any changes necessary to "get" there, apply the changes, wait 
;;at the location until a specified time.
(def moving-beh 
  (->and [find-move
          apply-changes
          wait]))

;; ;;a better moving-beh would be...
;; (def moving-beh2 
;;   (->or  [(->while should-move?
;;                     [find-move
;;                      apply-changes])
;;           wait]))

(defn pass 
  [msg ctx]  
  (->> (success ctx)
       (core/debug-print [:passing msg])))
(defn age [ctx] 
  (pass :age-stub ctx))

;;State-dependent functions, the building blocks of our state machine.
;;states are identical to leaf behaviors, with 
;;the possibility for some states to invoke transitions.
;;we'll continue to port them.
(def default-states 
  {:global          age
   :reset           age
   :bogging         age
   :dwelling        age 
   :moving          age
   :start-cycle     age
   :end-cycle       age
   :overlapping     age
   :demobilizing    age
   :policy-change   age
   :recovering      age
   :recovered       age 
   :nothing         age
   :spawning        #(pass :spawning %2)
   :abrupt-withdraw #(pass :abrupt-withdraw %2)})

;;perform a simple update via the entity's FSM.
(defn update-current-state [states ctx] 
  (->> (if-let [f (get states (get (statedata ctx) :curstate))]
         (f ctx)
         ctx)
       ((get states :global identity))))

;;similar to moving behavior, we have a stationary behavior...
;;If we're stationary, we're not moving, but staying in the same 
;;state, and updating statistics as a function of (usually time) 
;;based on the state we're in.
(def stationary-beh 
  (->or [update-current-state ;perform any state-specific stat updates...
         apply-changes        ;typical sweep of changes, typically
                              ;statistical updates
         ]))         


;;This is actually pretty cool, and might be a nice catch-all
;;behavior...
;;We try to compute changes, apply the changes, then wait until 
;;the next known change...
;;Known changes occur when we're told about them...i.e. 
;;when time elapses, when an external state change happens, 
;;etc.  It's ALWAYS externally driven by the caller.


(def default-behavior moving-beh) 


;; Private Function FinishCycle(unit As TimeStep_UnitData, frompos As String, topos As String) As TimeStep_UnitData
;; Set FinishCycle = unit

;; If NewCycle(unit, frompos, topos) Then
;;     If Not JustSpawned(unit) Then
;;         Set FinishCycle = StartCycle_State(EndCycle_State(unit, 0), 0) 'wrap up the cycle.
;;     End If
;; End If

;; End Function



;; 'TODO -> reduce this down to one logical condition man....
;; 'TOM Change 6 June -> pointed disengagement toward demand name, via unit.LocationName
;; Private Sub checkOverlap(unit As TimeStep_UnitData, frompos As String, nextpos As String)

;; If nextpos = "Overlapping" Then
;;     'Decoupled*
;;     'unit.parent.parent.demandManager.disengage unit, unit.LocationName, True
;;     MarathonOpDemand.disengage simstate.demandstore, unit, unit.LocationName, simstate.context, True
;; ElseIf frompos = "Overlapping" Then
;;     'Decoupled*
;;     'unit.parent.parent.demandManager.disengage unit, unit.LocationName, False
;;     MarathonOpDemand.disengage simstate.demandstore, unit, unit.LocationName, simstate.context, False
;; End If

;; End Sub

;;this is really a behavior, modified from the old state.  called from overlapping_state.
;;used to be called check-overlap
(defn disengage [ctx]
  (let [unit (entity ctx)]
    (cond (to-position? :overlapping ctx)
            (success (d/disengage (core/get-demandstore ctx) unit (:locationname unit) ctx true))
          (from-position? :overlapping ctx)
            (success (d/disengage (core/get-demandstore ctx) unit (:locationname unit) ctx false))
            :else (fail ctx))))

;; Private Sub checkDeployable(unit As TimeStep_UnitData, frompos As String, nextpos As String)
;; With unit
;;     If .policy.Deployable(frompos) <> .policy.Deployable(nextpos) Then 'came home from deployment, "may" back in Reset
;;         'Decoupled*
;;         '.parent.UpdateDeployStatus unit
;;         MarathonOpSupply.UpdateDeployStatus simstate.supplystore, unit, , , simstate.context
;;     End If
;; End With
;; End Sub

(defn check-deployable [])

;;Testing...

(comment 

(require '[marathon.sim.testing :as test])
(def u   (val (first (core/units test/demandctx))))
(def s1  (assoc fsm/blank-data :duration 0)) ;motivate a change.

;;note u and statedata are decoupled....
(def testctx 
  (-> test/demandctx
      (merge-bb {:entity    u 
                 :statedata s1})))

(defn b!  [b ctx]  (first (beval b ctx)))
(defn b!! [b ctx]  (second (beval b ctx)))

)

;; 'TODO -> these should be functions of policy, not parameters.
;; Private Function exceedsCycle(NewCycle As Long, unit As TimeStep_UnitData) As Boolean
;; Dim upperbound As Long
;; exceedsCycle = NewCycle > unit.policy.cyclelength
;; End Function

;; Private Function Deployable(position As String, policy As TimeStep_Policy) As Boolean
;; Deployable = policy.isDeployable(policy.GetCycleTime(position))
;; End Function

;; 'TODO -> Get this back to working as a function of policy.
;; 'TOM Change 3 Jan 2011 -> when conditions are met, units will start new cycles, update their cycles collection
;; 'This is a boolean filter, based on POLICY, that determines if the unit's state change merits a new cycle
;; Private Function NewCycle(unit As TimeStep_UnitData, frompos As String, topos As String) As Boolean
;; NewCycle = (topos = unit.policy.startstate)
;; End Function

;; 'This is a very general and powerful mechanism to capture state changes in the unit.
;; Private Function StateExpired(unit As TimeStep_UnitData, deltat As Single) As Boolean
;; If unit.StateData.remaining <= deltat Then StateExpired = True
;; End Function



;; Private Function JustSpawned(unit As TimeStep_UnitData) As Boolean
;; 'Decoupled*
;; 'JustSpawned = (unit.spawnTime = parent.getTime)
;; JustSpawned = (unit.spawnTime = SimLib.getTime(simstate.context))
;; End Function





;; 'State handler for generic updates that occur regardless of the state.
;; 'These are specific to the unit data structure, not any particular state.
;; Private Function Global_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData
;; Dim nextposition As String
;; Dim nextstate As String
;; Dim change As Boolean

;; change = False
;; 'bring the unit up to current time
;; Set unit = ageUnit(unit, deltat)

;; If StateExpired(unit, deltat) Then 'time in current state has expired
;;     nextposition = getNextPosition(unit) 'what is the next position according to policy?
;;     Set Global_State = Moving_State(unit, 0, nextposition) 'TOM Note 29 Mar 2011 -> this now reflects an instant move.
;; End If

;; Set Global_State = unit
;; End Function



;; 'Tom Change 1 July 2011
;; Private Function ageUnit(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData
;; With unit
;;     If deltat > 0 Then
;;         .cycletime = .cycletime + deltat 'units will always increase cycletime
;;         .StateData.timeinstate = .StateData.timeinstate + deltat 'we increase state time here, but we haven't
;;         'by changing the timeinstate, we've made the delta 0
;;         deltat = 0 'mutate the deltat variable
;;         'however, rollforward should be doing this for us by default.
;;     End If
;; End With
;; Set ageUnit = unit
;; End Function



;; 'State to control how a unit acts when it spawns.
;; Private Function Spawning_State(unit As TimeStep_UnitData, deltat As Single, Optional topos As String, Optional cycletime As Single) As TimeStep_UnitData
;; Dim newduration As Single
;; Dim offset As Single
;; Dim timeinstate As Single
;; Dim timeremaining As Single
;; Dim nextstate As String
;; 'added for overuse of .gettime
;; Dim tnow As Single

;; tnow = SimLib.getTime(simstate.context)

;; With unit
;;     If topos = vbNullString And .PositionPolicy = vbNullString Then 'no target position to spawn at, must derive
;;         .PositionPolicy = .policy.getPosition(.cycletime) '.parent.parent.PolicyManager.DeriveLocationID(.cycletime, . component)
;;         topos = .PositionPolicy '.parent.parent.policyManager.LocationIndex(.location)
;;         'Decoupled*
;;         '.location = .parent.parent.policyManager.locationID(topos)
;;         .location = MarathonOpPolicy.locationID(topos, simstate.policystore)
;;     Else 'target position, derive index
;;         topos = .PositionPolicy
;;         'Decoupled*
;;         '.location = .parent.parent.policyManager.locationID(topos)
;;         .location = MarathonOpPolicy.locationID(topos, simstate.policystore) 'TODO <----this is redundant, see if we can eliminate it.
;;     End If
;;     'Decoupled*
;;     '.spawnTime = .parent.getTime
;;     .spawnTime = tnow
;;     'hack...
;;     If .cycletime >= 0 Then
;;         timeinstate = .cycletime - .policy.GetCycleTime(.PositionPolicy) 'derived time in state upon spawning.
;;         'Decoupled*
;;         'unit.InitCycles parent.getTime
;;         unit.InitCycles tnow
;;         unit.AddDwell .cycletime
;;     Else
;;         unit.AddBOG timeinstate
;;         Err.Raise 101, , "Negative cycle times are not handled..."
;;     End If
;;     timeremaining = .policy.TransferTime(.PositionPolicy, .policy.nextposition(.PositionPolicy)) 'time remaining in this state
;;     newduration = timeremaining - timeinstate
    
;;     If .cycletime > 0 Then
;;         'Decoupled*
;; '       .DateToReset = DateAdd("d", -.cycletime, .parent.parent.parameters.startdate)
;;         .DateToReset = DateAdd("d", -.cycletime, simstate.parameters.startdate)
;;         nextstate = "Dwelling"
;;     ElseIf .cycletime = 0 Then
;;         'TOM note double check this.  make it point to current date.
;;         'Decoupled*
;;         '.DateToReset = .parent.parent.parameters.startdate
;;         .DateToReset = simstate.parameters.startdate
;;         nextstate = "Dwelling"
;;     Else 'account for deployed/bogging
;;         newduration = .cycletime
;;         nextstate = "Bogging"
;;     End If

;;     'initialize cycle from policy
;; End With

;; Set Spawning_State = ChangeState(unit, nextstate, 0, newduration)

;; 'TOM Change -> Changed from "Initialized" to "Spawning"
;; With unit
;;     'Decoupled*
;;     '.parent.LogMove .spawnTime, "Spawning", .PositionPolicy, unit, newduration
;;     MarathonOpSupply.LogMove .spawnTime, "Spawning", .PositionPolicy, unit, newduration, simstate.context
;; End With

;; End Function




;; Private Function Bogging_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData

;; If deltat > 0 Then
;;     With unit
;;         .AddBOG deltat
;;         '.DeployTimeRemaining = .DeployTimeRemaining - deltat
;;     End With
;; End If

;; Set Bogging_State = unit
;; End Function







;; 'Function to handle the occurence of an early withdraw from a deployment.
;; 'when a demand deactivates, what happens to the unit?
;; 'The behavior will be guided by (the unit's) policy.
;; 'The default behavior is that a unit will check its policy to see if it CAN deploy.
;; 'If policy says it's okay, the unit will return to the point time of its current lifecycle.
;; 'We can parameterize the penalty it takes to get back into lifecycle from deployment.
;;     'A usual penalty is a move to "90 days of recovery"
;; 'Note, we can also specify if the unit is instantly available to local demands.
;; Private Function AbruptWithdraw_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData

;; Dim bogremaining As Single

;; If deltat > 0 Then MarathonOpUnit.AddBOG unit, deltat
;; 'Consult policy to determine if entry back into available / ready pool is feasible.
;; 'TOM note 18 july 2012 -> this is erroneous.  We were check overlap....that's not the definition of
;; 'a unit's capacity to re-enter the available pool.
;; 'bogremaining = unit.CurrentCycle.bogbudget - unit.policy.overlap

;; 'Recovery should now be an option by default, not specifically dictated by
;; 'policy.

;; 'On second thought, this is sound.  If the unit is already in overlap, it's in a terminal state..
;; 'For followon eligibility, it means another unit would immediately be overlapping this one anyway,
;; 'and the demand would not be considered filled....It does nothing to alleviate the demand pressure,
;; 'which is the intent of followon deployments.  Conversely, if overlap is 0, as in typical surge
;; 'periods, then units will always followon.  I take back my earlier assessment, this is accurate.
;; bogremaining = unit.CurrentCycle.bogbudget - unit.policy.overlap

;; If bogremaining <= 0 Then 'makes no sense for the unit to continue BOGGING, send it home.
;;     Set AbruptWithdraw_State = Reset_State(unit, deltat)
;; Else 'unit has some feasible bogtime left, we can possibly have it followon or extend its bog...
;;     'A follow-on is when a unit can immediately move to fill an unfilled demand from the same
;;     'group of demands.  In otherwords, its able to locally fill in.
;;         'This allows us to refer to forcelists as discrete chunks of data, group them together,
;;         'and allow forces to flow from one to the next naturally.
    
;;     'Decoupled*
;; '   parent.addFollowOn unit 'register the unit as a possible followOn
;;     MarathonOpSupply.addFollowOn simstate.supplystore, unit, simstate.context 'register the unit as a possible followOn
    
;;     Set AbruptWithdraw_State = FollowOn_State(unit, 0) 'this puts us in position to followon.
;; End If

;; End Function




;; 'way to get the unit back to reset.
;; Private Function Reset_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData
;; unit.followoncode = vbNullString
;; Set Reset_State = Moving_State(unit, deltat, unit.policy.startstate)
;; End Function
;; 'Follow-on state is an absorbing state, where the unit waits until a changestate sends it elsewhere.
;; 'The only feasible state transfers are to a reentry state, where the unit re-enters the arforgen pool
;; 'in a dynamically determined position, or the unit goes to another demand compatible with the
;; 'followon code.
;; Private Function FollowOn_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData

;; Set FollowOn_State = ageUnit(unit, deltat)

;; End Function









;; 'A state to handle reentry into the available pool....
;; Private Function ReEntry_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData

;; Dim newpolicy As IRotationPolicy
;; Dim wasDeployable As Boolean
;; Dim isDeployable As Boolean
;; Dim newduration As Single
;; Dim offset As Single
;; Dim timeinstate As Single
;; Dim timeremaining As Single
;; Dim nextstate As String
;; Dim newcycletime As Single
;; Dim cycletimeA As Single
;; Dim policynameA As String
;; Dim policynameB As String
;; Dim CycleProportionA As Single
;; Dim CycleTimeB As Single
;; Dim PositionA As String
;; Dim PositionB As String

;; With unit
    
;;     'TOM Change 20 April 2012
;;     'wasDeployable = .policy.isDeployable(.cycletime)
;;     wasDeployable = .policy.isDeployable(.cycletime)

;;     'HACK to account for negative cycle times.
;;     If .cycletime < 0 Then Err.Raise 101, , "Cycle Time Should not be negative!"
    
;;     If .PositionPolicy <> "Deployed" And .PositionPolicy <> "Overlapping" Then  'we can safely change the unit's policy.
       
;;         isDeployable = .policy.isDeployable(.cycletime)
;;         PositionA = .PositionPolicy
;;         If .PositionPolicy <> "Deployed" Then
;;             If .PositionPolicy = "Overlapping" Then Err.Raise 101, , "Forgot about overlap.."
;;             PositionB = .policy.getPosition(.cycletime) 'dwelling units can change positions.
;;         Else
;;             Err.Raise 101, , "Cannot be deployed!"
;;         End If
        
;;         'position policy is the SAME as positionB
;;         .PositionPolicy = PositionB
        
;;         timeremaining = .policy.TransferTime(PositionB, .policy.nextposition(PositionB)) 'time remaining in this state
        
;;         'How long will the unit have been in this state?
;;             'Since it's a policy change....do we zero it out?
;;             'Or do we assume that the unit has been in the state the exact amount of time required?
;;         'We assume that the unit has been in the state the exact amount of time required.
;;         'We also assume that the unit is not entering another cycle, merely extending or truncating.
;;             'Its current cycle is modified.
;;             'Does not get a cycle completion out of it.
            
;;         If .cycletime >= 0 Then
;;             If PositionB <> "Deployed" Then
;;                 timeinstate = .cycletime - .policy.GetCycleTime(PositionB) 'derived time in state upon spawning.
;;             Else
;;                 Err.Raise 101, , "Error, should not be deployed"
;;             End If
;;         End If
        
;;         newduration = timeremaining - timeinstate
;;         nextstate = getState(unit, PositionB)
        
;;         'TOM change 23 july 2011
;;         'Decoupled*
;;         'parent.LogPosition parent.getTime, PositionA, PositionB, unit
;;         MarathonOpSupply.LogPosition SimLib.getTime(simstate.context), PositionA, PositionB, unit, , simstate.context
        
;;         If nextstate = "Dwelling" Or nextstate = "DeMobilizing" _
;;                 Or nextstate = Recovering Or nextstate = Recovered Then
;;             'parent.LogMove parent.getTime, .LocationName, .PositionPolicy, unit
;;             .changeLocation .PositionPolicy, simstate.context
;;             '.LocationName = .PositionPolicy
;;         End If
        
;;         'Decoupled*
;;         '.parent.UpdateDeployStatus unit
;;         MarathonOpSupply.UpdateDeployStatus simstate.supplystore, unit, , , simstate.context

;;         'Decouple*
;;         'parent.parent.trigger supplyUpdate, .name, .name, "Unit ReEntering with " & .CurrentCycle.bogbudget & " BOGBudget & " & .name
;;         SimLib.triggerEvent supplyUpdate, .name, .name, "Unit ReEntering with " & .CurrentCycle.bogbudget & " BOGBudget & " & .name, , simstate.context
;;         Set ReEntry_State = ChangeState(unit, nextstate, 0, newduration)
;;     Else
;;         Err.Raise 101, , "Cannot handle during deployment or overlap"
;;     End If
;; End With
;; End Function







;; '''TOM Change 1 June 2011 -> Renamed moving state to Repositioning_State.  Location changes (moves) cannot happen without a
;; '''change of position.  This is convention.  Change of position implies a state transition.
;; ''Private Function Repositioning_State(unit As TimeStep_UnitData, deltat As Single, _
;; ''                                Optional nextpos As String, Optional waitDuration As Single) As TimeStep_UnitData
;; ''
;; ''Dim frompos As String
;; ''Dim logtime As Single
;; ''Dim newstate As String
;; ''
;; ''Set Moving_State = unit
;; ''If nextpos = vbnullstring Then nextpos = getNextPosition(unit)
;; ''With unit
;; ''    frompos = .PositionPolicy
;; ''    If frompos <> nextpos Then
;; ''        logtime = .parent.gettime
;; ''        .PositionPolicy = nextpos 'Move the unit .
;; ''        .CurrentCycle.addTraversal logtime, frompos, nextpos
;; ''
;; ''        checkOverlap unit, frompos, nextpos
;; ''        checkDeployable unit, frompos, nextpos
;; ''
;; ''        Set unit = FinishCycle(unit, frompos, nextpos)
;; ''        If waitDuration = 0 Then waitDuration = getWaitTime(unit, nextpos) '- deltat
;; ''        .parent.LogMove logtime, frompos, nextpos, unit, waitDuration  'record the move
;; ''    End If
;; ''
;; ''    newstate = getState(unit, nextpos) 'derive the new state associated with the location
;; ''    Set Moving_State = ChangeState(unit, newstate, waitDuration)  'changestates
;; ''End With
;; ''
;; ''
;; ''End Function





;; 'TOM Change 1 June 2011 -> Added a subordinate routine to deal with changes in the spatial
;; 'location of a unit, i.e. "moves".
;; 'By convention, every call to moving state will also call update_location, to ensure the unit's
;; 'location is kept up to date.
;;     'Location changes are always precipitated by position updates.  A unit cannot change its non-temporal
;;     'state without realizing a positional change.
;; Private Function Moving_State(unit As TimeStep_UnitData, deltat As Single, _
;;                                 Optional nextpos As String, Optional waitDuration As Single) As TimeStep_UnitData
;; Dim frompos As String
;; Dim logtime As Single
;; Dim newstate As String

;; Set Moving_State = unit
;; If nextpos = vbNullString Then nextpos = getNextPosition(unit)
;; With unit
;;     frompos = .PositionPolicy
;;     newstate = getState(unit, nextpos) 'derive the new state associated with the location
    
;;     If frompos <> nextpos Then
;;         'Decouple
;;         logtime = SimLib.getTime(simstate.context)
        
;;         'TOM Change 24 July -> moved down....
;;         .PositionPolicy = nextpos 'Move the unit .
;;         .CurrentCycle.addTraversal logtime, frompos, nextpos
        
;;         'TOM note 6 June 2011 -> this both cause side-effects....
;;         checkOverlap unit, frompos, nextpos
;;         checkDeployable unit, frompos, nextpos
        
;;         'This is where mutation occurs.  PolicyChange could happen here.
;;         'If there is a policy change, we need to record the intermediate location change as well.
;;         'We come in from overlapping to reset.  The positionpolicy is updated to reset.
;;         'We go into finishcycle, with a pending policy change on the policy stack.
;;         'The policy change is applied, to the current position and cycletime....
;;             'which are both 0, and don't produce a neglible side effect.
;;         'The policy change then changes the location from
;;         Set unit = FinishCycle(unit, frompos, nextpos)
        
        
;;         'TOM change 21 july 2011
;;         If waitDuration = 0 And unit.StateData.remaining = 0 Then
;;             waitDuration = getWaitTime(unit, nextpos) '- deltat
;;         Else
;;             waitDuration = unit.StateData.remaining
;;         End If
        
;;         'TOM Change 6 June 2011
;;         If nextpos <> .PositionPolicy Then nextpos = .PositionPolicy
;;         'Decoupled*
;;         '.parent.LogPosition logtime, frompos, nextpos, unit, waitDuration  'record the move
;;         MarathonOpSupply.LogPosition logtime, frompos, nextpos, unit, waitDuration, simstate.context 'record the move
;;     End If

;;     'TOM Change 6 June 2011 -> HACK
;;     'We want overlapping units to remain associated with their locations....
;;     'Tom Change 19 April 2012
;;     If newstate = "Dwelling" Or newstate = "DeMobilizing" Or newstate = "Recovering" Then .changeLocation nextpos, simstate.context  '.LocationName = nextpos
;;     Set Moving_State = ChangeState(unit, newstate, deltat, waitDuration) 'changestates
;; End With

;; End Function




;; 'TOM Change 13 Jul 2011
;; 'Needed to implement the transition from one policy to another.  I chose to add a state to handle just this.
;; 'Visual analysis showed that PolicyChange looks a lot like Spawn, in that when a unit changes policies, it must change
;; 'a lot of its internal state to follow the new policy.  The result of the policy change is:
;; '   1: The unit's cycle time is normalized, and then transformed into the relevant cycletime in the new policy.
;; '   2: The unit's position "may" change to reflect its position in the new policy.
;; '   3: The unit's location "may" change to reflect its location in the new policy.
;; 'TOM Change 20 April:
;; '   4: The unit's BOGBudget "may" change to reflect either increased, or decreased, BOGBudget.
;; 'TOM Change 24 April:
;; '   5: The unit's BOGBudget and MAXBOG may only change (increase) as the result of a policy change.
;; '   6: Policy changes can NOT happen during terminal states:
;; '       [Deployed {Bogging, Overlapping}, Demobilizing]


;; 'If NOT deployed (bogging, overlapping) or in a terminal state (demobilizing), then entities can change policy immediately.
;; 'Otherwise, units change policy upon next reset (change is deferred).

;; 'Assumes that the new policy is already set for the unit (i.e. the unitdata is pointing toward the new policy).
;; 'Ideally, an outside agent will have modified the unit's policy, and subsequently told it to changestates to a policy-change
;; 'state.

;; 'Net effect is that policy changes to the same policy are idempotent.
;; 'State to control how a unit acts when it changes policy.
;; 'Note -> we extract the next policy from the unitdata's policy stack.

;; 'TOM note -> figure out how to change this for the deployed population...they have negative cycle
;; 'times.

;; 'Note -> this assumes we have compatible policies, or at least policies that have a cyclical
;; 'rotational lifecycle.
;; Function PolicyChange_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData

;; Dim newpolicy As IRotationPolicy

;; Dim wasDeployable As Boolean
;; Dim isDeployable As Boolean

;; Dim newduration As Single
;; Dim offset As Single
;; 'tom change
;; Dim tnow As Single
;; Dim timeinstate As Single
;; Dim timeremaining As Single
;; Dim nextstate As String

;; Dim newcycletime As Single

;; Dim cycletimeA As Single
;; Dim policynameA As String
;; Dim policynameB As String
;; Dim CycleProportionA As Single
;; Dim CycleTimeB As Single
;; Dim PositionA As String
;; Dim PositionB As String


;; tnow = SimLib.getTime(simstate.context)

;; With unit

;;     If .policyStack.count = 0 Then Err.Raise 101, , "no new policies on stack!"
               
;;     'fetch the new policy defined by the IRotationPolicy.
;;     'Note, for composite policies, the newpolicy defers to the active policy.
;;     'The active policy should have been modified for us.  That's a weak spot...
;;     Set newpolicy = .policyStack(.policyStack.count)
    
;;     If newpolicy.bogbudget = 0 Then Err.Raise 101, , "No bog budget!"
;;     policynameA = .policy.AtomicName 'points to the active atomic policy
;;     policynameB = newpolicy.AtomicName 'points to the active atomic policy
    
;;     'TOM Change 20 April -> We need to separate the unit's experienced
;;     'cycle length vs the NOMINAL cycle duration, which exists in
;;     'POLICY SPACE.  In composite rotational policies, the NOMINAL cycle duration
;;     'changes when Atomic policies change.  Specificallly, we map the unit's position
;;     'or coordinates in the current atomic policy to coordinates in the new policy.
;;     'The unit's actual experienced lifecycle, i.e. its cycletime property, is not
;;     'an accurate mapping between policies.  The implicit assumption is that when
;;     'mapping from one policy to another, if the policies have differing cycle lengths
;;     'then there is a discount or exchange rate between the policies, such that time
;;     'spent in one policy is NOT equal to time spent in another.  However, our
;;     'unit's cyclelength property is not subject to this, since it technically
;;     'exists OUTSIDE of the policy view of time.  The cyclelength property reflects the
;;     'actual time a unit has spent, under ANY policy, until it has reset or started a
;;     'new cycle.
    
;;     'Prior to 19 April 2012, The unit's ability to deploy, via the CanDeploy method,
;;     'depended on it's position in the current policy as a function of the cyclelength property.
;;     'We should prefer the duration of the current cycle record, which is an accurate reflection
;;     'of the relative time in the unit's current policy.
;;     cycletimeA = .cycletime
    
;;     'TOM Change 20 April 2012
;;     'cycletimeA = .CurrentCycle.duration

;;     PositionA = .PositionPolicy
;;     'TOM Change 20 April 2012
;;     'wasDeployable = .policy.isDeployable(.cycletime)
;;     wasDeployable = .policy.isDeployable(.cycletime)

    
;;     'HACK to account for negative cycle times.
;;     If .cycletime < 0 Then Err.Raise 101, , "Cycle Time Should not be negative!"
    
;;     CycleProportionA = .cycletime / .policy.cyclelength
;;     'TOM change 23 April 2012 -> No longer allow units that are De-mobilizing to enter into available pool.
    
;;     If CycleProportionA <= 1# And PositionA <> "Deployed" And _
;;         PositionA <> "Overlapping" And PositionA <> "DeMobilizing" Then   'we can safely change the unit's policy.

;;         'Tom Change -> fixed to integers.
;;         CycleTimeB = Fix(CycleProportionA * newpolicy.cyclelength)
;;         If CycleTimeB > newpolicy.cyclelength Then Err.Raise 101, , "Cyclelength is too long!"
        
;;         isDeployable = newpolicy.isDeployable(CycleTimeB)
        
;;         If .PositionPolicy <> "Deployed" Then
;;             If .PositionPolicy = "Overlapping" Then Err.Raise 101, , "Forgot about overlap.."
;;             PositionB = newpolicy.getPosition(CycleTimeB) 'dwelling units can change positions.
;;         Else
;;             PositionB = .PositionPolicy 'deployed units remain deployed.
;;         End If
        
;;         'Tom Note 12 July 2012
;;         'This actually updates the unit's policy....
;;         'What are the consequences?
;;         'We 're setting its IRotation policy to the new policy....
;;         'We should be setting it to the root policy....
;;         Set .policy = newpolicy
;;         'position policy is the SAME as positionB
;;         .PositionPolicy = PositionB
        
;;         timeremaining = .policy.TransferTime(PositionB, .policy.nextposition(PositionB)) 'time remaining in this state
        
;;         'How long will the unit have been in this state?
;;             'Since it's a policy change....do we zero it out?
;;             'Or do we assume that the unit has been in the state the exact amount of time required?
;;         'We assume that the unit has been in the state the exact amount of time required.
;;         'We also assume that the unit is not entering another cycle, merely extending or truncating.
;;             'Its current cycle is modified.
;;             'Does not get a cycle completion out of it.
;;         .cycletime = CycleTimeB
       
;;         If .cycletime >= 0 Then
;;             If PositionB <> "Deployed" Then
;;                 timeinstate = .cycletime - .policy.GetCycleTime(PositionB) 'derived time in state upon spawning.
                
;;                 'Decoupled*
;; '                unit.ChangeCycle parent.getTime
;;                 unit.ChangeCycle tnow
;;                 unit.ModifyCycle newpolicy
                
;;             Else 'how long have we been deployed?  Assume it's BOG for now....
;;                 timeinstate = .CurrentCycle.bog  'TOM Note This may change.
;;             End If
;;         Else
;;             unit.AddBOG timeinstate
;;             Err.Raise 101, , "Negative cycle times are not handled..."
;;         End If
        
;;         newduration = timeremaining - timeinstate
;;         nextstate = getState(unit, PositionB)
        
;;         'TOM change 23 july 2011
;;         If PositionA <> PositionB Then
;;             'Decoupled*
;; '           parent.LogPosition parent.getTime, PositionA, PositionB, unit
;;             MarathonOpSupply.LogPosition tnow, PositionA, PositionB, unit, , simstate.context

;;             If nextstate = "Dwelling" Or nextstate = "DeMobilizing" _
;;                     Or nextstate = Recovering Or nextstate = Recovered Then
;;                 'parent.LogMove parent.getTime, .LocationName, .PositionPolicy, unit
;;                 .changeLocation .PositionPolicy, simstate.context
;;                 '.LocationName = .PositionPolicy
;;             End If
;;         End If
        
        
;;         'If wasDeployable <> isDeployable Then .parent.UpdateDeployStatus unit
;;         'Decoupled*
;;         '.parent.UpdateDeployStatus unit
;;         MarathonOpSupply.UpdateDeployStatus simstate.supplystore, unit, , , simstate.context
        
;;         'Adopt Policy B.
;;         'Policy A ->
;;         '    Find relative CT = ct/CLengthA
;;         'Policy B ->
;;         '    Find relative positionB = pos(RelativeCT * CLengthB)
;;         'Movingstate from PositionA to relative PositionB.
;;         'Update with delta0.
;;         'TOM Change 2 Sep -> moved this north so that we can use the policy stack as a flag in unit's
;;         'ChangeCycle logic.  Check for sideeffects
;;         .policyStack.Remove 1
        
;;         'Decoupled*
;; '        parent.parent.trigger UnitChangedPolicy, .name, .policy.AtomicName, "Unit " & .name & " changed policies: " & _
;; '            policynameA & ":" & cycletimeA & "->" & policynameB & ":" & CycleTimeB
;;         SimLib.triggerEvent UnitChangedPolicy, .name, .policy.AtomicName, "Unit " & .name & " changed policies: " & _
;;             policynameA & ":" & cycletimeA & "->" & policynameB & ":" & CycleTimeB, , simstate.context

        
;;         'parent.parent.trigger SupplyUpdate, .name, .name, "Policy Change Caused Supply Update"
;;         Set PolicyChange_State = ChangeState(unit, nextstate, 0, newduration)
;;         'Decoupled*
;; '        parent.parent.trigger supplyUpdate, .name, .name, "Policy Change Caused Supply Update for unit " & .name
;;         SimLib.triggerEvent supplyUpdate, .name, .name, "Policy Change Caused Supply Update for unit " & .name, , simstate.context

;;         'NOTE -> I may need to consider changing location here.....
;;     Else 'The unit's cycle cannot project onto another cycle.  We need to defer policy change until reset.
;;         'leave the policy on the stack.  Catch it during reset.
;;         ''TOM change 2 Sep 2011 -> we modify the cyclerecord to reflect changes in expectations...
;;         'This is not a replacement...
        
;;         'TOM Change 23 April 2012 -> We do NOT modify the cycle.....screws with expectations?
;;         '.ModifyCycle newpolicy
        
;;         'Decoupled*
;; '        parent.parent.trigger AwaitingPolicyChange, .name, .policy.AtomicName, "Unit " & _
;; '                .name & " in position " & .PositionPolicy & " is waiting until reset to change policies"
;;         SimLib.triggerEvent AwaitingPolicyChange, .name, .policy.AtomicName, "Unit " & _
;;                 .name & " in position " & .PositionPolicy & " is waiting until reset to change policies", , simstate.context
;;         Set unit = RevertState(unit)
;;         'We updated the unit in the process
;;         'Decoupled*
;;         'parent.parent.trigger supplyUpdate, .name, .name, "Policy Change Attempt Caused Supply Update for unit " & .name
;;         SimLib.triggerEvent supplyUpdate, .name, .name, "Policy Change Attempt Caused Supply Update for unit " & .name, , simstate.context
;;     End If
;; End With
;; End Function




;; 'Units dwelling will accumulate dwell over time.
;; Private Function Dwelling_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData
;; If deltat > 0 Then unit.AddDwell deltat
;; Set Dwelling_State = unit
;; End Function





;; 'TOM Change 23 April 2012 -> Check to see if we should even try to recover....
;; 'Criteria: if cycletime + recoverytime > cycleduration then 'skip recovery....
;; 'go straight to recovered.  This will push us into whatever the post recovery state was...
;; Public Function Recovering_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData

;; Dim nm As String
;; If canRecover(unit) Then
;;     If deltat > 0 Then unit.AddDwell deltat
;;     Set Recovering_State = unit
;; Else
;;     nm = unit.name
;;     'decoupled*
;;     'parent.parent.trigger supplyUpdate, nm, nm, "Unit " & nm & " Skipping Recovery with " & unit.CurrentCycle.bogbudget & " BOGBudget"
;;     SimLib.triggerEvent supplyUpdate, nm, nm, "Unit " & nm & " Skipping Recovery with " & unit.CurrentCycle.bogbudget & " BOGBudget", , simstate.context
;;     'zero out the unit's ability to BOG, preventing recovery and re-Entry.
;;     unit.CurrentCycle.bogbudget = 0
;;     Set Recovering_State = Moving_State(unit, deltat) 'advance the unit forward in time.
;; End If

;; End Function






;; 'TOM Change 23 April 2012
;; 'Auxillary function to help us determine whether a unit should try to recover....
;; Private Function canRecover(unit As TimeStep_UnitData) As Boolean
;; Dim rtime As Single
;; Dim demobtime As Single 'Tom Change 18 July 2012

;; canRecover = False

;; With unit
;;     'Tom change 18 July 2012 ->
;;         'Allowing general recovery as a default behavior, although policies can change it...
;;             'I.e. RC14 remob is an example.
;;         'If the unit's policy doesn't have a recovery state, we set an abitrary recoverytime.
;;         'Derived from Parameters("DefaultRecoveryTime")
        
;;     'Tom Change 23 April 2012
;;     If .policy.PositionGraph.nodeExists(recovery) Then
;;         rtime = .policy.TransferTime(recovery, Recovered)
;;     Else 'Decoupled*
;;         'rtime = parent.parent.parameters.getKey("DefaultRecoveryTime")
;;         rtime = simstate.parameters.getKey("DefaultRecoveryTime")
;;         'Tom note -> I think this is a moot point, because our criteria for recovering is to not
;;         'exceed the expected duration of a cycle.  If we're already exceeding it, we don't have to
;;         'check demob.
;;         'if the policy includes demob, we need to account for demob time as well.
;; '        If .policy.PositionGraph.nodeExists(demobilization) Then
;; '            rtime = .policy.TransferTime(demobilization, .policy.nextposition(demobilization))
;; '        End If
;;     End If
    
;;     If .CurrentCycle.bogbudget > 0 And .cycletime + rtime < .CurrentCycle.DurationExpected Then
;;         canRecover = True
;;     End If
;; End With

;; End Function





;; 'A state in which the unit will attempt to re-enter the available pool, if it has enough BOG time remaining.
;; Public Function Recovered_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData
;; Set Recovered_State = unit
;; Dim entryPosition As String
;; Dim timeremaining As Single
;; Dim timeinstate As Single
;; Dim newduration As Single

;; With unit
;;     'Tom Change 23 April 2012
;;     If .CurrentCycle.bogbudget > 0 Then
;;         If .cycletime < .CurrentCycle.DurationExpected Then
;;             Set unit = ReEntry_State(unit, deltat) 'We try to re-enter the available pool.
;;         End If
;;     Else
;;         Set unit = Moving_State(unit, deltat)
;;     End If
;; End With
       
;; End Function







;; 'Units starting cycles will go through a series of procedures.
;; Private Function StartCycle_State(unit As TimeStep_UnitData, deltat As Single, Optional tnow As Single) As TimeStep_UnitData
;; 'Possibly log this as an event?
;; If IsMissing(tnow) Then tnow = SimLib.getTime(simstate.context)
;; With unit
;;     .cycletime = 0 'reset the time in cycle
;;     'Decoupled*
;; '    .DateToReset = .parent.getTime 'reset the datetoreset
;;     .DateToReset = tnow 'reset the datetoreset
;;     'TOM change 19 Jul 2011
;;     'Check to see if we have a pending policychange.
;;     If .policyStack.count > 0 Then
;;         Set unit = PolicyChange_State(unit, deltat)
;;     End If
;; End With

;; Set StartCycle_State = unit
;; End Function






;; 'Units ending cycles will record their last cycle locally.
;; Private Function EndCycle_State(unit As TimeStep_UnitData, deltat As Single, Optional tnow As Single) As TimeStep_UnitData
;; If IsMissing(tnow) Then tnow = SimLib.getTime(simstate.context)
;; With unit
;;     'Set cycledict = New Dictionary
;;     'Decouple
;;     .CurrentCycle.tfinal = tnow
;;     'cycledict.add "Cycle", .CurrentCycle
;;     'Decoupled*
;;     SimLib.triggerEvent CycleCompleted, .name, simstate.supplystore.name, "Completed A Cycle", .CurrentCycle, simstate.context 'cycledict
;;     .RecordCycle (.CurrentCycle.tfinal)
;; End With

;; 'Set cycledict = Nothing

;; Set EndCycle_State = unit
;; End Function









;; 'Units in overlap accumulate dwell. They also may do other things.
;; Private Function Overlapping_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData

;; MarathonOpUnit.AddBOG unit, deltat

;; Set Overlapping_State = unit
;; End Function





;; 'Units in overlap accumulate dwell. They also may do other things.
;; Private Function DeMobilizing_State(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData

;; unit.addMOB deltat
;; Set DeMobilizing_State = unit

;; End Function





;; Private Sub Class_Terminate()
;; 'Set parent = Nothing
;; End Sub

;; Private Property Let IUnitBehavior_name(ByVal RHS As String)
;; name = RHS
;; End Property
;; Private Property Get IUnitBehavior_name() As String
;; IUnitBehavior_name = name
;; End Property
;; Private Sub IUnitBehavior_init(state As TimeStep_SimState, Optional basicbehavior As IUnitBehavior)
;; Set simstate = state
;; End Sub
;; Private Function IUnitBehavior_ChangeState(unit As TimeStep_UnitData, tostate As String, deltat As Single, Optional duration As Single, Optional followingstate As String) As TimeStep_UnitData
;; Set IUnitBehavior_ChangeState = ChangeState(unit, tostate, deltat, duration, followingstate)
;; End Function
;; Private Property Set IUnitBehavior_simstate(ByVal RHS As TimeStep_SimState)
;; Set simstate = RHS
;; End Property

;; Private Property Get IUnitBehavior_simstate() As TimeStep_SimState
;; Set IUnitBehavior_simstate = simstate
;; End Property

;; Private Function IUnitBehavior_update(tElapsed As Single, unit As TimeStep_UnitData) As TimeStep_UnitData
;; Set IUnitBehavior_update = update(tElapsed, unit)
;; End Function


;;OBE
;;=================

;;I don't think we ever used this.

;; 'Tom change 25 july 2011
;; 'Ability to revert back to previous state, timeinstate, duration, etc.
;; Private Function RevertState(unit As TimeStep_UnitData) As TimeStep_UnitData

;; unit.StateData.RevertState
;; Set RevertState = unit

;; End Function


(comment 
;;the current design maintains a reference to the mutable simstate.
;;we can copy that for now by allowing a dynamic var...
;;going to cheat for now...allow behaviors to have access to core data.
(def ^:dynamic *simstate* nil)


;;Interface defining unit behaviors.
;;AC behavior, RC behavior, etc. all implement these things.

;;well, behaviors have names....
;;They also have a simstate reference...

;;let's assume we can minimally represent a behavior as a
;;name and a set of states.  Given that, we can use the
;;behavior to determine what happens to a unit on updating,
;;using the context of a simstate. and any other pertinent
;;information.  Note, the IUnitBehavior protocol may be gratiutous
;;at this point. We'll see.
(defrecord behavior [name states]
  protocols/IUnitBehavior
  (behavior-name [b] name)
  (init-behavior [b state] (init-unit-behavior b state *simstate*))
  (update [b deltat unit]  (update-behavior b deltat unit *simstate*))
  (change-state [b unit to-state deltat duration following-state]
    (change-state b unit to-state deltat duration following-state *simstate*)))

;;TODO# define an empty-behavior
(declare roll-forward-beh)

(defn old-update-behavior [b deltat unit simstate]
  (roll-forward-beh b (merge simstate {:entity unit :deltat deltat})))
)



;;What happens when we change a state?
;;Some states are just blips (0 duration) .
;;Some states are absorbing states (infinite duration) .
;;Unless otherwise specified (by a duration),e duration is assumed infinite.
;; Public Function ChangeState(unit As TimeStep_UnitData, tostate As String, deltat As Single, Optional duration As Single, _
;;                                         Optional followingstate As String) As TimeStep_UnitData

;; 'TOM change 1 July 2011

;; If deltat > 0 Then Set unit = update(deltat, unit)

;; With unit
;;     .StateData.ChangeState tostate, duration, followingstate, True 'allows instant state changes.
;;     If duration > 0 Then
;;         'If duration <> .StateData.inf Then
;;             'Decoupled*
;;             MarathonOpUnit.requestUnitUpdate SimLib.getTime(simstate.context) + duration, unit, simstate.context
;;         'End If
;;     End If
;;     Set ChangeState = UpdateState(unit, 0) 'State Changes are instantaneous.  If the current state is instantaneous, then
;; End With

;; End Function


;;hrm...change-state is less important than before; it used to be the
;;foundation of the state machine, and states used it to establish
;;transitions.  change-state had 2 primary roles: establish the
;;context of the transition, and record the transition.  Recording the
;;transition is as simple as updating the entity's statedata
;;(duration, current state, time-in-state, time-remaining).
;;Establishing the context of the transition (previous state, current
;;state).  Change-state also acted on the default that if no time was
;;specified, we'd have an infinite state.  In the context of
;;behaviors, this may be obsolete....or it decomposed into a smaller
;;behavior.  change-state is part of the high-level API as well...it's
;;fundamental to modifying units.

;;change-state already ported as change-state-beh.
;;we may want to refine that name to just be "change" or "change behavior"


;; 'This function allows us to roll through multiple updates, if delta T is too large.
;; 'We could set deltaT to 1095 and get an entire cycle in theory.
;; Private Function RollForward(unit As TimeStep_UnitData, deltat As Single) As TimeStep_UnitData

;; Dim remaining As Single
;; Dim deltaNext As Single

;; If deltat <= unit.StateData.remaining Then 'will the change cause us to be in the same state?
;;     Set RollForward = UpdateState(unit, deltat)
;; Else
;;     While unit.StateData.remaining < deltat 'if the change means we're exceeding our time remaining...
;;         deltaNext = unit.StateData.remaining 'this entire logic branch prevents us from overreaching time in state.
;;         deltat = deltat - deltaNext
;;         Set RollForward = UpdateState(unit, deltaNext)
;;     Wend
;; End If

;; End Function



;; Private Function specialstate(instate As String) As Boolean
;; specialstate = instate = "Spawning" Or instate = "AbruptWithdraw"
;; End Function
