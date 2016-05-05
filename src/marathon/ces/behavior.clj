;;A namespace for simple entity behaviors.
;;We'll define core behaviors here.
(ns marathon.ces.behavior
  (:require [spork.ai.core :as ai :refer
             [deref! fget fassoc  push-message- map->entityctx debug ->msg]]
            [spork.ai.behavior 
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
            [marathon.data [fsm :as fsm]
                           [protocols :as protocols]
             ]
            [marathon.ces [basebehavior :as base :refer :all]
                          [core :as core]
                          [unit :as u]
                          [supply :as supply]
                          [demand :as d]
             ]
            
            [spork.util.general     :as gen]        
            [spork.data.priorityq   :as pq]
            [clojure.core.reducers  :as r]
            [spork.entitysystem.store :as store :refer :all :exclude [default]]
            [spork.sim.simcontext :as sim]
            )
  (:import [marathon.ces.basebehavior behaviorenv]))

;;__utils__
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

(defn ->seq2
  "Defines a sequential node, more or less the bread-and-butter of behavior tree architecture.
   A sequential node will traverse xs, in order, only short circuiting if a node is running.  
   After the reduction is complete, the value of the sequence is successful."
  [xs]
  (b/->bnode  :seq2 nil
     (fn seq2 [ctx]
       (reduce (fn seqf [acc child]
                 (try
                   (do 
                     (println (type acc)
                       (b/with-result [[res ctx] (beval child (val! acc))]
                         (spork.util.general/case-identical? res
                                                             :run       (reduced (b/run ctx))
                                                             :success   (b/success ctx)
                                                             :fail      (b/fail ctx)))))
                   (catch Exception e (println {:child child :acc acc :e e} )))) (b/success ctx) xs))
     xs))


(defmacro if-y [expr & else]
  `(if (= (clojure.string/upper-case (read)) "Y")
     ~expr 
     ~@else))

(defmacro log! [msg ctx]
  `(do (println ~msg)
       ~ctx))

(defn echo [msg]
  (fn [ctx] (do (println msg) (success ctx))))

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


;;__Utility functions__
;;Entity step operations...
(defn progress-cycle [x width]
  (if (>= x width)
    0
    (unchecked-inc x)))

;;testing function...
(defn deployment-over?
  [y]
  (or (>= y (* 15 31))
      (and (>= y 30) (<= (rand) 0.01))))

;;testing function...
(defn should-deploy? [t tmax]
  (and (>= t 365)
       (<= (rand) (* 0.005 (/ (double t) tmax)))))

(defn deployed? [e] (identical? (:state e) :deploying))
(defn should-reset? [t tmax] (>= t tmax))
(defn spawning?     [^marathon.data.fsm.statedata statedata]
  (identical?  (.curstate statedata) :spawning))

;;aux functions will most likely be plentiful.  We specifically
;;have a host of helper functions for unit-specific entity behaviors.
;;Most of them deal with how the units read their policies and stuff.
;;__Aux Functions__

;;#TODO   See if we can encode or derive a more meaningful semantics
;;from the indices currently associated with the states...for
;;instance, :deployable randomly came back with 7 as a state, we
;;either don't want this or we want to have it mean something.
;;Note: these are specific to unit, so could probably go into the unit
;;namespace; save on real estate.
(defn get-state [unit position] 
  (let [s (protocols/get-state (:policy unit) position)]
    (if (number? s)  :dwelling s)))
(defn get-next-position [unit position]
  (protocols/next-position (:policy unit) position))

(defn policy-wait-time [policy statedata position deltat]
  (let [frompos  (protocols/next-position policy position)
        topos    (protocols/next-position policy frompos)
        t (protocols/transfer-time policy frompos topos)
        _ (println [frompos topos t])
        ]
    (- t (- deltat (fsm/remaining statedata)))))

;;Could be a cleaner way to unpack our data, but this is it for now...
;;need to fix this...let's see where we use it.
(defn get-wait-time
  ([unit frompos topos {:keys [deltat statedata] :as benv}]
     (let [wt (protocols/transfer-time (:policy unit) frompos topos)
           deltat (or  deltat 0) ;allow the ctx to override us...
           ]
       (- wt (fsm/remaining statedata))))
  ([unit position {:keys [deltat statedata] :as benv}]
   (policy-wait-time (:policy unit) statedata position (or deltat 0)))
  ([position {:keys [entity] :as benv}] (get-wait-time @entity position benv))
  ([{:keys [wait-time] :as benv}] wait-time))

;;Basic API
;;=========
;;The rest of the simulation still relies on our pre-existing API, 
;;namely that we have "change-state", and "update"

;;note that change-state already exists in marathon.sim.unit/change-state, 
;;we're merely providing an interface to the unit's behavior for it.
;;Also note that change-state is only called (currently) from
;;marathon.sim.demand (for abrupt withdraws), and marathon.sim.supply
;;(for deployments).

;;might ditch these....
(declare change-state-beh update-state-beh update-state 
         roll-forward-beh
         check-overlap
         check-deployable
         finish-cycle
         spawning-beh
;         age-unit
         moving-beh
         )



        

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

;;__update-entity!__
;;Similarly, we'll have update take the context last.
;;update will depend on change-state-beh, but not change-state.
;;change-state is a higher-level api for changing things.
;;Note: this is covered by step-entity!  We need to
;;include the roll-forward-beh though, to ensure we're
;;consistent.

;;we can wrap these up and just pass a generic message for the
;;behavior to interpret.
;;change-state becomes
;;load-entity
;;add-message {:to-state to-state :deltat deltat :ctx ctx}


;;Move this out to marathon.ces.unit?
;;auxillary function that helps us wrap updates to the unit.
(defn traverse-unit [u t from to]   
  (-> u 
      (assoc :positionpolicy  to)
      (u/add-traversal t from to)))

;;this is kinda weak, we used to use it to determine when not to
;;perform updates via the global state, but it's probably less
;;important now...we can actually codify this structurally
;;in the behavior tree now...
;;special states just diverted the fsm update function to
;;a different path (bypassing the global state, i.e. not
;;aging/advancing).  Where we had direct method calls to
;;other state handler functions, we can now just directly
;;encode the transition in the tree...
(defn special-state? [s] (or (identical? s :spawning)
                             (identical? s :abrupt-withdraw)))

(defn just-spawned?
  "Determines if the entity recently spawned, indicated by a default
   negative spawn time or a spawntime in the present."
  [{:keys [entity ctx] :as benv}]
  (let [st (:spawntime @entity)]
    (or (neg? st)
        (==  st    (core/get-time @ctx)))))

(defn state-expired? [{:keys [deltat statedata] :as benv}] 
  (let [r (fsm/remaining statedata)
        dt (or deltat 0)
        ]
    (<=  r dt)))
       
;;debatable utility...
;;Not sure where we're using these guys....
(defn to-position?   [to   benv]  (identical? (:next-position benv)  to))
(defn from-position? [from  benv] (identical? (:from-position benv) from))

;;Capturing change information in a structure, rather than passing it
;;around willy-nilly in the environment.  If we have a pending
;;change, there will be changeinfo.  This only applies for instantaneous
;;changes....That way, we can communicate our state updates serially
;;by adding (and removing) changeinfo.
(comment 
(defrecord changeinfo [newstate duration followingstate])
)

;;Behaviors
;;=========

;;A lot of these behaviors operate on the concept of a blackboard.
;;The behavior environment, defined in marathon.ces.basebehavior,
;;is a map of lexical bindings that we use to evaluate the consequences
;;of a unit's behavior.  Certain behaviors may place or remove things
;;from the blackboard to communicate information with other behaviors
;;"down the line".  We can couple behaviors directly using the behavior
;;tree, or allow them to be indirectly coupled using the blackboard
;;as a form of simple event communication.  Many behaviors, like
;;update-after, and roll-forward-beh, will actually "consume"
;;items in the environment, like time.  It will be common to see
;;an ephemeral, or a transactional semantics with the behaviors.

(befn +nothing-state+ [entity deltat ctx]
     (->do (fn [_] (log! (str (:name @entity) " is doing nothing for " deltat) ctx)
             )))

;;note-we have a wait time in the context, under :wait-time
;;updates an entity after a specified duration, relative to the 
;;current simulation time + duration.
(befn update-after  ^behaviorenv [entity wait-time tupdate ctx]
   (when wait-time
     (->alter
      #(let [tfut (+ tupdate wait-time) 
             e                       (:name @entity)
             _    (println [e :requesting-update :at tfut])]
         (swap! ctx (fn [ctx] 
                      (core/request-update tfut
                                           e
                                           :supplyupdate
                                           ctx)))
         (dissoc % :wait-time) ;remove the wait-time from further consideration...
         ))))

;;our idioms for defining behaviors will be to unpack 
;;vars we're expecting from the context.  typically we'll 
;;just be passing around the simulation context, perhaps 
;;with some supplementary keys.
;;Let's think about what it means to change state....
;;Are we in fact changing the root of the behavior?
;;This is where the transition from FSM to behavior tree
;;comes in....
(befn change-state-beh!  {:keys [entity ctx statedata state-change deltat] 
                         :or   {deltat 0} :as benv}
     (when state-change
       (let [{:keys [newstate duration followingstate timeinstate]} state-change
             followingstate (or followingstate newstate)
             ;;we change statedata here...
             wt (- duration timeinstate)
             _ (println [:changing-state state-change :wait-time wt])
             newdata (assoc (fsm/change-statedata statedata newstate duration followingstate)
                            :timeinstate timeinstate)
             benv    (merge (dissoc benv :state-change) {:statedata newdata
                                                         :duration duration
                                                         :timeinstate timeinstate
                                                         :wait-time wt})
             _       (swap! entity #(assoc % :state newstate )) ;;update the entity state, currently redundant.
             ]
                                        ;state change inducing a wait until update.
;         (if (not (neg? duration))
;           (->> (if (pos? duration)
;                  (val (update-after benv))                  
;                  benv)                
         (beval update-state-beh benv))))
(def change-state-beh (->seq [(echo :<change-state-beh>)
                              change-state-beh!]))

           ;;supposed to immediately jump to the next beh 
;           (throw   (Exception. "Error, cannot have negative duration!"))))))


 
;;Maybe we never have a change-state, we just set the new state of the unit
;;and update its behavior....

;;implement update-state-beh
;;basically, all this does is modify the state data in the fsm (reading from
;;the behavior context), set the time, etc, and then process an update.
;;So, the update-state-beh is really the key here; it's (currently) just a
;;case statement, ala the FSM design.  Could we capture this in the tree
;;structure ala Btrees?

;;change-state-beh will fail if there's no changeinfo....cool.
(def change-and-update
  (->and [change-state-beh
          update-state-beh]))

;;we want to update the unit to its current point in time.  Basically, 
;;we are folding over the behavior tree, updating along the way by 
;;modifying the context.  One of the bits of context we're modifying 
;;is the current deltat; assumably, some behaviors are predicated on 
;;having a positive deltat, others are instantaneous and thus expect 
;;deltat = 0 in the context.  Note, this is predicated on the
;;assumption that we can eventually pass time in some behavior....

;;note: we have the current behavior in the behavior environment.
;;we can use that as our stack.

;;if we were "really" smart about it, we could hash the
;;deltas that would be applied from updating a unit as a
;;function of time and then just pull out the requisite
;;statistical chunks and add it to the unit to update...
;;However, this would be less useful than it sounds...

;;Note -> we could represent roll-forward-beh as something more
;;btree-ish, specifically as a combination of a repeat node and 
;;some conditions (namely the condition that deltat <= remaining)
;;Might have a smaller behavior that advances the next-smallest 
;;time slice.  TODO# refactor using behaviortree nodes.
;;One issue is how we measure time.  Currently, deltat is our
;;main metric for measuring time to advance.  Most of the legacy 
;;FSM implementation went off of absolute time, and measured 
;;deltas internally.
(befn roll-forward-beh {:keys [deltat] :as benv}
  (when (pos? deltat)
    (loop [dt   deltat
           benv benv]
      (let [sd (:statedata    benv)            
            timeleft    (fsm/remaining sd)
            _  (println [:sd sd])
            _  (println [:rolling :dt dt :remaining timeleft])
            ]
        (if-y 
         (if (<= dt timeleft)
           (do (println [:dt<=timeleft])
               (beval update-state-beh (assoc   (log! [:updating-for dt] benv) :deltat dt)))
           (let [residual   (max (- dt timeleft) 0)
                 res        (beval update-state-beh (assoc benv  :deltat timeleft))]
             (if (success? res) 
               (recur  residual ;advance time be decreasing delta
                       (val! res))
               res)))
         nil)))))

;;roll-forward is a while behavior...
;;(->while (> deltat 

;;I think deltat is okay....
;;We're saying "take enough steps until this much time has elapsed." 
;;One way we can do that is to add the disparity between tlastupdate 
;;and tnow to deltat.
  
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

;;We can go with the halo bitmask behavior approach...
;;Being in a state precludes certain behaviors from running...
;;Rather than selecting a specific behavior, we just outlaw
;;known behaviors as a function of the state.

;;This broadens the utility of the behavior tree.
;;Another approach is we just check inside the behavior for
;;state...so....that's also doable.


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
(befn should-move? ^behaviorenv {:keys [next-position statedata] :as benv}
      (do (println [:should?
                    {:next-position next-position
                     :remaining (fsm/remaining statedata)
                     :spawning? (spawning? statedata)
                     :wait-time (:wait-time benv)}])
          (when (or next-position
                    (zero? (fsm/remaining statedata)) ;;time is up...
                    (spawning? statedata))
            (success benv))))

;;This may not matter...                  
(befn record-move {:as benv} (bind!! {:moved true}))

;;after updating the unit bound to :entity in our context, 
;;we commit it into the supplystore.  This is probably 
;;slow....we may want to define a mutable version, 
;;or detect if mutation is allowed for a faster update
;;path.  For instance, on first encountering the unit,
;;we establish a mutable cell to its location and use that 
;;during the update process.
;(def commit-entity (->alter commit-entity!))

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

;;We probably need to account for location component
;;changes as well, since these will be tied to position...
;;Or, we tie in the physical position changes with the
;;abstract position changes...

;;Should we decouple state changes from movement?  Does movement
;;correspond with state change?  No...it does not.

;;Given that we have the context for a move in place, 
;;we want to move as directed by the context.  If there 
;;is a wait time associated with the place we're moving 
;;to, we will add the wait-time to the context.  That way,
;;downstream behaviors can pick up on the wait-time, and 
;;apply it.
(befn move->statechange ^behaviorenv {:keys [entity next-position tupdate statedata ctx] :as benv}
    (when-let [nextpos next-position] ;we must have a position computed, else we fail.                                       
      (let [t        tupdate
            u        @entity 
            frompos  (get     u      :positionpolicy) ;;look up where we're coming from.
            wt       (or (:wait-time benv) (get-wait-time  u  nextpos benv))  ;;how long will we be waiting?            
            ]
        (if (= frompos nextpos)  ;;if we're already there...
          (do (println [:no-movement frompos nextpos (type benv)])
              (success (dissoc benv :next-position))) ;do nothing, no move has taken place.  No change in position.
          (let [ ;_        (println [:apply-move frompos nextpos wt (select-keys benv [:next-position :tupdate :statedata :wait-time])])
                _        (println [:moving frompos nextpos])
                newstate (get-state u nextpos)
;                new-sd   (fsm/change-state statedata newstate wt)
                state-change {:newstate       newstate
                              :duration       wt
                              :followingstate nil
                              :timeinstate 0
                              }
                _        (reset! entity  (traverse-unit u t frompos nextpos)) ;update the entity atom
                _        (reset! ctx (u/unit-moved-event! @entity nextpos @ctx)) ;ugly, fire off a move event.
                from-loc (:locationname u)
                to-loc   (when (clojure.set/intersection
                                #{"Dwelling" "DeMobilizing" "Recovering"
                                  :deployable :dwelling} newstate)
                           nextpos
                          )
                ]
            (bind!!  ;update the context with information derived
                                        ;from moving
             {:position-change {:from-position frompos ;record information
                                :to-position   nextpos}
              :state-change  state-change
              :location-change (when (not (identical? from-loc to-loc))
                                          {:from-location  from-loc
                                           :to-location    to-loc})
              :wait-time     nil
              :next-position nil}
             ))
          ))))

;;Dont' think we need this...
;;more expressive...
;; (befn apply-state {:keys [new-state new-duration statedata] :as benv}
;;       (when new-state
;;         (do (println [:applying-state!])
;;             (bind!! ;update the context with information derived
;;                                         ;from moving
;;              {:statedata  (fsm/change-state statedata  new-state new-duration)}))))


;;This hooks us up with a next-position and a wait-time
;;going forward.
(befn find-move ^behaviorenv {:keys [entity next-position wait-time] :as benv}      
  (let [e  @entity
        currentpos (:positionpolicy e)
        p  (or next-position
               (do (println [:computing-position])
                   (get-next-position e  currentpos)))
        wt (or wait-time
               (do (println [:computing-wait])
                   (get-wait-time @entity (:positionpolicy e) benv)))
        _ (println [:found-move {:next-position p :wait-time wt}])]
    (bind!! {:next-position  p
             :wait-time      wt
             }  ;;have a move scheduled...
            )))

;;We know how to wait.  If there is an established wait-time, we
;;request an update after the time has elapsed using update-after.
(befn wait ^behaviorenv {:keys [wait-time] :as benv}          
  (when-let [wt wait-time] ;;if we have an established wait time...    
    (if (zero? wt)
      (do
        (println [:instantly-updating])
                 update-state-beh) ;skip the wait, instantaneous.  No need to request an
      ;update.
      (log! (str "waiting for " wt)
            (update-after  benv)))))

;;[looks a lot like my naive test behavior]

      
;;Units starting cycles will go through a series of procedures.
;;Possibly log this as an event?
(befn start-cycle {:keys [entity deltat tupdate] :as benv}
   (let [unit   @entity
         pstack (:policystack unit)]
     (do  (swap! entity #(merge % {:cycletime 0
                                   :date-to-reset tupdate}))
          (if (pos? (count pstack))
            (bind!! {:next-policy (first pstack)
                     :policy-change true})
            benv))))    

;;Units ending cycles will record their last cycle locally.  We broadcast
;;the change...Maybe we should just queue this as a message instead..
(befn end-cycle {:keys [entity ctx tupdate] :as benv}
  (let [cyc (assoc (:currentcycle @entity) :tfinal tupdate)
        _  (swap! entity (fn [unit]
                           (->  unit
                                (assoc :currentcycle cyc)
                                (u/recordcycle tupdate))))
        _  (swap! ctx (fn [ctx] (sim/trigger-event :CycleCompleted
                                                   (:name @entity)
                                                   :SupplyStore
                                                   "Completed A Cycle" ctx)))]
    (success benv)))

;;dunno, just making this up at the moment until I can find a
;;definition of new-cycle.  This might change since we have local
;;demand effects that can cause units to stop cycling.
(defn new-cycle? [unit frompos topos]
  (identical? (protocols/end-state (:policy unit)) topos))

(befn finish-cycle ^behaviorenv {:keys [entity from-position to-position] :as benv}
      (when (and (not (just-spawned? benv))
                 (new-cycle? @entity from-position to-position))
        (->> benv
             (start-cycle)
             (end-cycle))))

;;this is really a behavior, modified from the old state.  called from overlapping_state.
;;used to be called check-overlap
(befn disengage {:keys [entity to-position from-position ctx] :as benv}
      (let [res   (cond (identical? to-position   :overlapping)   true
                        (identical? from-position :overlapping)   false
                        :else :none)]
        (when (not (identical? res :none)) ;ugh?
          (do (swap! ctx ;;update the context...
                     #(d/disengage (core/get-demandstore %) @entity (:locationname @entity) % res))
              (success benv)))))

(def check-overlap disengage)

(befn check-deployable ^behaviorenv {:keys [entity position-change ctx] :as benv}
   (when position-change
     (let [{:keys [from-position to-position]} position-change
           u @entity
           p (:policy u)
           _ (println [:checking-deployable  :from from-position :to to-position])]
       (when   (not= (protocols/deployable-at? p from-position)
                     (protocols/deployable-at? p to-position))
         (do (println [:deployable-changed! from-position to-position])
             (swap! ctx #(supply/update-deploy-status u nil nil %))
             (success benv))))))

(def change-position
  (->seq [check-overlap
          check-deployable
          finish-cycle
          (->alter  #(dissoc % :position-change))]))

;;if there's a location change queued, we see it in the env.
(befn change-location {:keys [entity location-change ctx] :as benv}
   (when location-change
     (let [{:keys [from-location to-location]} location-change]
       (let [_ (println [:location-change location-change])            
             _  (swap! ctx    #(u/unit-moved-event! @entity to-location %))
             _  (reset! entity (u/push-location @entity to-location))] 
         ;;we need to trigger a location change on the unit...
         (success (dissoc benv :location-change))))))


;;with a wait-time and a next-position secured,
;;we can now move.  Movement may compute a statechange
;;in the process.
(def execute-move
  (->seq [(echo :<move->statechange>)
          move->statechange
          (echo :<change-position>)
          change-position
          (echo :<change-location>)
          change-location
          change-state-beh
          (echo :waiting)
          wait
          ]))
;;Movement is pretty straightforward: find a place to go, determine 
;;any changes necessary to "get" there, apply the changes, wait 
;;at the location until a specified time.
(def moving-beh 
  (->and [(echo :moving-beh)
          should-move? ;if there is a next position or our time in state expired.
          find-move    ;determine the wait-time, and possibly the next-position to wait at.         
          (echo :execute-move)
          execute-move
          ]))

(defn pass 
  [msg ctx]  
  (->> (success ctx)
       (core/debug-print [:passing msg])))

;;State handler for generic updates that occur regardless of the state.
;;These are specific to the unit data structure, not any particular state.
;;Should we keep a timestamp with the unit? That way we can keep track
;;of how fresh it is.
(befn age-unit ^behaviorenv {:keys [deltat statedata entity ctx] :as benv}
      (let [dt (or deltat 0)]
        (if (zero? dt)
            (success benv) ;done aging.
            (let [
                                        ;_  (log!   [:aging dt] benv)
                  _  (swap! entity #(u/add-duration  % dt)) ;;update the entity atom
                  _ (println [:aging-unit deltat :cycletime (:cycletime @entity)]) 
                  ]
              (bind!! {:deltat 0 ;is this the sole consumer of time? 
                       :statedata (fsm/add-duration statedata dt)})))))
 
;;We're going to copy this a bunch of times...
(befn dwelling-beh ^behaviorenv {:keys [entity deltat] :as benv}
      (do (swap! entity  #(u/add-dwell % deltat))
          (success benv)))

(befn bogging-beh ^behaviorenv {:keys [entity deltat] :as benv}
      (do (swap! entity  #(u/add-bog % deltat))
          (success benv)))


(befn special-state {:keys [entity statedata] :as benv}
      (let [s (:state entity)]
        (case s
          :spawning spawning-beh
          :abrupt-withdraw (echo :abrupt-withdraw) ;abrupt-withdraw-beh
          (fail benv))))

(befn do-current-state {:keys [entity statedata] :as benv}
      (echo :do-current-state)
      )

;;the entity will see if a message has been sent
;;externally, and then compare this with its current internal
;;knowledge of messages that are happening concurrently.
(befn check-messages ^behaviorenv {:keys [entity current-messages ctx] :as c}
  (when-let [old-msgs     (fget (deref! entity) :messages)] ;we have messages
    (when-let [msgs  (pq/chunk-peek! old-msgs)]
      (let [new-msgs (rconcat (r/map val  msgs) current-messages)
            _        (b/swap!! entity (fn [^clojure.lang.Associative m]
                                        (.assoc m :messages
                                                (pq/chunk-pop! old-msgs msgs)
                                                )))]
        (bind!! {:current-messages new-msgs})))
;    (success c)
    ))
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

;;so now we can handle changing state and friends.
;;we can define a response-map, ala compojure and friends.


;;type sig:: msg -> benv/Associative -> benv/Associative
;;this gets called a lot.
(defn message-handler [msg ^behaviorenv benv]
  (let [entity           (.entity benv)
        current-messages (.current-messages benv)
        ctx              (.ctx benv)]
    (do (ai/debug (println (str [(:name (deref! entity)) :handling msg])))
      (beval 
       (case (:msg msg)
           ;;generic update function.  Temporally dependent.
           :update (if (== (get (deref! entity) :last-update -1) (.tupdate benv))
                     (do (success benv)) ;entity is current
                     (->and [(echo :update)
                             (fn [^clojure.lang.Associative ctx]
                               (success (.assoc ctx :current-message msg
                                                   )
                                        ))
                             ;(.behavior benv)
                             ;advance ;;we don't do anything
                             ;(echo :blah)
                             ]))
           :spawn  (->and [(echo :spawn)
                           (push! entity :state :spawning)                        
                           spawning-beh]
                          )
           ;;allow the entity to change its behavior.
           :become (push! entity :behavior (:data msg))
           :do     (->do (:data msg))
           :echo   (->do  (fn [_] (println (:data msg))))
           (do ;(println (str [:ignoring :unknown-message-type (:msg msg) :in  msg]))
               (sim/trigger-event msg @ctx) ;toss it over the fence
               ;(throw (Exception. (str [:unknown-message-type (:msg msg) :in  msg])))
               (success benv)
               ))
       benv))))

;;we'd probably like to encapsulate this in a component that can be seen as a "mini system"
;;basically, it'd be a simple record, or a function, that exposes a message-handling
;;interface (could even be a generic fn that eats packets).  For now, we'll work
;;inside the behavior context.  Note, the entity is a form of continuation....at
;;least the message-handling portion of it is.

;;message handling is currently baked into the behavior.
;;We should parameterize it.

;;handle the current batch of messages that are pending for the
;;entity.  We currently define a default behavior.
(befn handle-messages ^behaviorenv {:keys [entity current-messages ctx] :as benv}
      (when current-messages
        (reduce (fn [acc msg]                  
                  (do (debug [:handling msg])
                    (message-handler msg (val! acc))))
                (success benv)
                current-messages)))

(defmacro ->as [nm & body]
  `(->seq [(echo ~nm)
           ~@body]))

(def global-state
  (->seq [(echo :aging)
          age-unit          
          (echo :aged)
          moving-beh]))

(def update-state-beh
  (->seq [(echo :<update-state-beh>)
          (->or [(->and [(echo :check-messages)
                         check-messages
                         handle-messages])
                 (echo :no-messages)])
          (->or [special-state
                 (->and [do-current-state
                         (echo :global-state)
                         (fn [ctx]
                           (if-y 
                            global-state
                            (fail ctx)))
                          
                         ])
                 (echo :up-to-date)])]))

;;So, this precludes is from having shared behaviors
;;across states.  If we go the state-based route, we
;;end up partitioning our behavior and having
;;to duplicate it internally. 


;;states are identical to leaf behaviors, with 
;;the possibility for some states to invoke transitions.
;;we'll continue to port them.
(comment

  ;;these are state masks, effectively....
  
(def default-states 
  {:global          age-unit
   :reset            #(pass :spawning %)  
   :bogging          bogging-beh   ;simple updates f(t)
   :dwelling         dwelling-beh  ;simple updates f(t)
   ;Currently, we encode multiple states in the policy graph.  We may
   ;want to re-evaluate that...right now, we have multiple
   ;combinations of states to handle...that all correspond to dwelling.
   #{:dwelling}      dwelling-beh
   #{:deployable}    dwelling-beh
   #{:deployable :dwelling}  dwelling-beh
   :moving           moving-beh
   :start-cycle      #(pass :start-cycle     %)
   :end-cycle        #(pass :end-cycle       %)
   :overlapping      #(pass :overlapping     %)
   :demobilizing     #(pass :demobilizing    %)
   :policy-change    #(pass :policy-change   %)
   :recovering       #(pass :recovering      %)
   :recovered        #(pass :recovered       %) 
   :nothing          #(pass :nothing         %)
   :spawning         spawning-beh
   :abrupt-withdraw  #(pass :abrupt-withdraw %)
;   #{:deployable :dwelling} #(pass :deployable-dwelling)
   })

)

(comment
;;this is significantly different than the fsm approach we used before...
;;now, we handle each case as a specific branch of the tree...
;;note: we can parameterize this and build the tree dynamically to
;;define higher-order behaviors.

(defmacro try-get [m k & else]
  `(if-let [res# (get ~m ~k)]
     res# 
     ~@else))
  
;;perform a simple update via the entity's FSM.
(defn update-current-state 
  ([states ctx] 
     (let [st (get (statedata ctx) :curstate)
           _ (log! st ctx)]
       (if-let [f (try-get states st +nothing-state+)]
         (do (log! [:updating-in st] ctx)
             (f ctx))
         (do (log! [:unknown-state st] ctx)
             (fail ctx)))))
  ([ctx] (update-current-state default-states ctx)))

;;this is another btree node:
;;(->case) or (->states ) 

)

;;similar to moving behavior, we have a stationary behavior...
;;If we're stationary, we're not moving, but staying in the same 
;;state, and updating statistics as a function of (usually time) 
;;based on the state we're in.

;; (def stationary-beh 
;;   (->or [update-current-state ;perform any state-specific stat updates...
;;          apply-changes        ;typical sweep of changes, typically
;;                               ;statistical updates
;;          ]))         

;;Our default spawning behavior is to use cycle to indicate.
;;There will be times we alter the methods in which a unit 
;;spawns (initializes itself in the simulation).  It'd be nice
;;to break this out at some point, for now, we just let it go.

;;we can break the spawning behavior up into smaller tasks...
;;Find out where we're supposed to be. Do we have initial conditions? 
;;Initial conditions are currently derived from cycletime and policy. 
;;For instance, we narrowly assume that every unit exists somewhere in 
;;on a cycle at t=0, rather than setting them in arbitray deployments
;;to begin with.  This is limiting, we should be able to define
;;all kinds of initial conditions to perform upon spawn (like set
;;location, cycletime, etc.)  For now, we just replicate the
;;cycletime-derived automated initial conditions logic.

;;Given a cycletime, where should we be according to policy?
;;Behavior to control how a unit acts when it spawns.
;;We're trying to account for the unit's initial state...
;;We move from spawning to the initial location.
;;We account for having been at the initial location for 
;;timeinstate days (currently tied to cycletime - timetoposition).
;;So, what we really want to do is update the unit initially, possibly 
;;with a negative time, and advance it forward to time 0 via the
;;deltat being the timeinstate.
(befn spawning-beh ^behaviorenv {:keys [to-position cycletime tupdate statedata entity ctx]
                                 :as  benv}
  (when (spawning? statedata)     
    (let [ent @entity
          {:keys [ positionpolicy policy]} ent
          {:keys [curstate prevstate nextstate timeinstate 
                  timeinstateprior duration durationprior 
                  statestart statehistory]} statedata
          cycletime (or cycletime (:cycletime ent))
          topos     (if  (not (or to-position positionpolicy))
                         (protocols/get-position (u/get-policy ent) cycletime)
                         positionpolicy)
          timeinstate   (- cycletime 
                           (protocols/get-cycle-time policy
                                                     positionpolicy))
          timeremaining (protocols/transfer-time policy
                                                 positionpolicy 
                                                 (protocols/next-position policy positionpolicy))
          newduration   (- timeremaining timeinstate)
          nextstate     (protocols/get-state policy positionpolicy)
          spawned-unit  (-> ent (u/initCycles tupdate) (u/add-dwell cycletime) (assoc :last-update tupdate)) ;;may not want to do this..
          _             (reset! entity spawned-unit)
          state-change {:newstate       nextstate
                        :duration       timeremaining
                        :followingstate nil
                        :timeinstate timeinstate
                        }
          _             (println [:nextstate nextstate :state-change state-change :current-state (:state ent)])
          
          ]
      (->>  (assoc benv :state-change state-change
                   :location-change {:from-location "Spawning"
                                     :to-location   topos}
                   :next-position   topos ;queue up a move...
                   ;:wait-time newduration ;;change-state handles this for us...
                   )
            (log!  (core/msg "Spawning unit " (select-keys (u/summary spawned-unit) [:name :positionstate :positionpolicy :cycletime])))
;;           MarathonOpSupply.LogMove .spawnTime, "Spawning", .PositionPolicy, unit, newduration, state.context
            (beval (->seq [(echo :change-state)
                           change-state-beh
                           (fn [benv]
                             (do (reset! ctx 
                                         (supply/log-move! tupdate :spawning (:positionpolicy @entity) @entity @ctx))
                                 (success benv)))]
                   ))))))
                   


;;This is actually pretty cool, and might be a nice catch-all
;;behavior...
;;We try to compute changes, apply the changes, then wait until 
;;the next known change...
;;Known changes occur when we're told about them...i.e. 
;;when time elapses, when an external state change happens, 
;;etc.  It's ALWAYS externally driven by the caller.
(comment 
(def default-behavior 
  (->seq [spawning-beh ;make sure we're alive 
          moving-beh   ;if were' alive move
          update-current-state ;update once we're done moving
          age-unit ;if we're not moving, age in place.
          ]))
)

(comment 
;;default behavior is this...
(->or [[:messaging (->and [check-messages ;;state-specific, and one-off externally driven behavior.
                           handle-messages])]
       spawning       ;;try to spawn the unit if possible.
       exiting        ;;if the unit is dead, kill it.
       age-unit       ;;get the unit up to date, following its current behavior.       
       moving         ;;if we're not able to move, we can accumulate statistics...       
       [:cycling
         (->or  [deploying   (->or [bogging ;;accumulate bog
                                    overlapping ;;change state to overlap, accumulat bog probably..
                                    abrupt-withdraw ;;unit is abruptly removed.
                                    ])
                 disengaging (->or [recovering  ;;try to recover
                                    redeploying ;;try to redeploy to become available...                                  
                                    ])
                 resetting   ;go to reset...                            
                 dwelling    [add-dwell]])]])

[resetting [finish-cycle
            moving]]
)


;;__Behavior Definitions__
;;Note: there are only a couple of small places where the behaviors need to
;;interface with the store.  We "could" abstract that away even further and
;;eliminate the store dependency, but for now it's fine.  For instance,
;;we could capture the changes as instructions, and queue them up for
;;integration via a higher-level system.


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
                          :tupdate t}) ;;update the time.
        (do ;can't wait out entire time in this state.
          (merge!! entity {:wait-time 0
                           :tupdate (- t duration)}) ;;still not up-to-date
           ;;have we handled the message?
           ;;what if time remains? this is akin to roll-over behavior.
           ;;we'll register that time is left over. We can determine what
           ;;to do in the next evaluation.  For now, we defer it.
          (bind!! {:current-message (.assoc ^clojure.lang.Associative msg :delta (- delta duration))}
                 )
          )))))

;;choose-state only cares about the entity.
;;normally, we determine the entity's state based on its
;;policy.  I guess it really doesn't matter too much...
;;states correspond to primitive behaviors, really
;;they just update primitive statistics or whatnot.
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

(defn up-to-date? [e ctx] (== (:tupdate e) (:tupdate ctx)))

;;This will become an API call...
;;instead of associng, we can invoke the protocol.
(befn schedule-update  ^behaviorenv {:keys [entity ctx new-messages] :as benv}
      (let [st       (deref! entity)
            nm       (:name st)
            duration (:wait-time st)
            tnow     (:tupdate (deref! ctx))
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


;;Basic entity behavior is to respond to new external
;;stimuli, and then try to move out.
(defn recovered?       [u] false)
(defn deployment-over? [u] false)

;(defn find-move [ctx] nil)
;;Typically, there will be no external stimuli in the
;;form of messages, so the entity will derive its operation
;;based on its current surroundings.  Note: we can
;;still model the entity as an FSM if the state is
;;set appropriately...in that case these behaviors
;;are dispatched via a case statement; everything
;;still works nicely.
(befn try-dwell ^behaviorenv {:keys [entity] :as benv}
      (when (or (deployment-over? @entity)
                (recovered? @entity))
        [find-move
         advance]))

;;wire in functionality here for a unit to invoke its own
;;deployment order...
;;From here on, the system will append a deployment order to
;;the unit, and send the unit a message to update.
;;The unit will handle the message by appending a
;;deployment order to its state and invoking an update.
;;This way, we handle messages first, which preps the
;;behavior environment to respond to stimulii (like
;;the presence of a deploy order)
(defn deploy-to [o benv]
  ;;stub
  (success benv))

(befn try-deploy ^behaviorenv {:keys [entity] :as benv}
      (when-let [o (:deploy-order @entity)]
        (deploy-to o)))

;;This is a pretty typical prototype for entities to follow.
(befn default ^behaviorenv [entity]
      (->or [(->and [check-messages
                     handle-messages])  ;check for non-default events.
             try-deploy  ;initiate a new deployment, if we have orders.
             try-dwell   ;go back to dwelling, if our deployment is over.
             advance     ;advance in policy, time, statistics, etc.
             ]))

;;This is kind of weak, but I don't have a better solution at the moment...
(do (println [:setting-defaults])
    (reset! base/default-behavior +nothing-state+))

(comment 

;;if we're supposed to deploy, we should have orders for the pending
;;deployment...

;;Orders tell us where to go, when, and (possibly) for how long.
;;Units can thus interpret orders (rather than having
;;the deployment system handle everything in a monolothic
;;fashion.  Fills => orders.  

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


(comment ;OBE

(defn update-unit 
  "Computes a new simulation context given a specific unit to update, 
   an elapsed time, and an optional time of update.  tupdate is inferred
   to be the current simulation time if none is supplied."
  ([unit deltat ctx]        
     (update-unit unit deltat (sim/current-time ctx) ctx))
  ([unit deltat tupdate ctx]
     (->>  ctx  
           (load-entity! unit deltat tupdate)
           (roll-forward-beh) ;update the unit according to the change in
                                        ;time.
           (error-on-fail)    ;unit updates should never fail.       
           (second  ;result is in terms of [:success|:fail ctx], pull out
                                        ;the ctx
            )
           (commit-entity!)
;           (clear-bb)
           )))


;;We'll replace these; for now the units will automatically
;;try to update themselves if possible.

;;Debatable...just invokes roll-forward-beh; I think we can ensure that
;;roll-forward is always invoked first...

;;Re-evaluate the need for this....can we synchronize from outside?
;;ideally, we just keep track of the unit's last update....
(defn synch 
  "Utility function.  Synchronize the unit to the current simulation time.  
   If the last update occured before the current time, we roll the unit forward 
   by the delta between the last update and the current time."
  [unit ctx]
  (let [tprev (or (sim/last-update (:name unit) ctx) 0)
        tnow  (sim/current-time ctx)]
    (if (= tprev tnow)
      (log! (str "unit " (:name unit) "is up to date") ctx)
      (log! (str "Synchronizing unit " (:name unit) " from " tprev " to " tnow)
            (update-unit unit (- tnow tprev) tprev ctx)))))

;;Synchronizes the unit to the current time, then applies a time 
;;delta, then processes/records the unit's time of update.
(defn update 
  "Entry point for computing behavior-based unit updates.  Fundamental 
   API function for processing unit entities.  Synchronizes the unit to 
   the current simulation time, then computes the new simulation context 
   resulting from the entity behavior over an elapsed deltat (from current
   simulation time)."
  [unit deltat ctx]
  (let [nm (get unit :name)]
    (->> (synch unit ctx)
         (update-unit unit deltat)
         (u/unit-update! nm (core/msg "Updated " nm)))))




)
