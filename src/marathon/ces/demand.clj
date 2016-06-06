;;The demand system provides functions for scheduling demands, as well as 
;;defining the order in which demands are filled.  The namespace contains 
;;primitive functions for operating on demandstores, as well as demand-related
;;notifications and a high-level demand management API.  
;Functions for creating, initializing, resetting, and updating state related to
;the demand simulation are also found here.
(ns marathon.ces.demand
  (:require  [marathon.demand [demanddata :as d]
                              [demandstore :as dstore]
                              ]
             [marathon.ces    [core :as core] 
                              [supply :as supply] 
                              [policy :as policy]
                              [unit :as u]]
             [spork.entitysystem.store :as store
              :refer [gete assoce mergee assoc-ine updatee get-entity add-entity drop-entity
                      update-ine update-entity get-ine] ]
             [spork.sim       [simcontext :as sim]]
             [spork.ai.core :refer [debug]]
             [spork.util      [tags :as tag] [general :as gen] [temporal :as temporal]]))

;;##Primitive Demand and DemandStore Operations

(defn can-simulate? [demandstore]
  (> (count (tag/get-tags (:tags demandstore) :enabled)) 0))

(defn add-fillable [fillrule demandstore]
  (assert (not (contains? (-> demandstore :fillables) fillrule))  
          "Tried to add fillrule multiple times")
  (gen/deep-update demandstore [:fillables] conj fillrule))

(defn remove-fillable [fillrule demandstore]
  (assert (contains? (-> demandstore :fillables) fillrule)  
          "Tried to remove non-existent fillrule")
  (gen/deep-update demandstore [:fillables] disj fillrule))

;;I think we can alter this to push to the entitystore instead of
;;the demandmap....
;;All the calls to add-demand need to be changed though...
;;the demandstore is now the ctx itself...
;;it's actually okay if we can tree the entitystore as
;;a demandstore - this works out okay with the existing api.
;;Actually, get-demandstore is just identity then....
;;Another option, which may seem wierd, is that we add a
;;reference to the parent context (Although we may
;;screw up gc)....
;;So, if the demandstore's parent is the entitystore,
;;if we get the demandstore, and the dstore has a ctx
;;reference, we can just shim demandstore operations to
;;that....

;TOM ADDED 30 MAy 2013 
(defn add-demand [demandstore demand]
  (gen/deep-assoc demandstore [:demandmap (:name demand)] demand))

(defn manage-changed-demands [day state] ;REDUNDANT
  (gen/deep-assoc state [:demand-store :changed] {}))

(defn clear-changes [demandstore] (assoc demandstore :changed {}))

(defn register-change [demandstore demandname]
  (if (contains? (:changed demandstore) demandname)
    demandstore 
    (gen/deep-assoc demandstore [:changed demandname]  0)))

(defn active-demand? [demandstore demandname]
  (contains? (get demandstore :activedemands) demandname))

;TOM Note 20 May 2013 -> need to abstract fillrule, etc. behind a function, 
;preferably one that uses keywords.
;inject appropriate tags into the GenericTags
(defn tag-demand 
  ([demand demandstore extras]
     (->> (tag/multi-tag (:tags demandstore) 
                         (:name demand) 
                         (into   [(core/msg "FILLRULE_" (:src demand)) ;USE KEYWORD
                                  (core/msg "PRIORITY_" (:priority demand)) ;USE KEYWORD
                                  :enabled]
                                  extras))
          (assoc demandstore :tags)))
  ([demand demandstore] (tag-demand demand demandstore nil)))

(defn tag-demand-sink [demandstore sink]
  (gen/deep-update demandstore [:tags] tag/tag-subject  :Sinks sink))

(defn get-demand-sinks [demandstore] 
  (tag/get-subjects (:tags demandstore) :Sinks))

(defn clear-demands [demandstore] ;REDUNDANT?
  (merge demandstore
         {:demandmap {}
          :tlastdeactivation nil
          :unfilledq {}
          :activations {}
          :active-demands {}
          :fillables {}
          :tags (tag/add-tag tag/empty-tags :Sinks)}))

;;__TODO__ Possibly move unit-related functionality to supply/unit simulations..

(defn set-followon [unit code] (assoc unit :followoncode code))
(defn ghost? [unit] (= (:src unit) "Ghost"))
(defn ungrouped? [g] (= g "UnGrouped"))
(defn unit-count [d] (count (:units-assigned d)))
(defn demand-filled? [d] (= (count (:units-assigned d)) (:quantity d)))
(defn empty-demand? [d] (zero? (unit-count d)))
(defn priority-key [demand] [(:name demand) (:priority demand)]) 
          
;We can break the fill into a couple of simple queries...
;
;we've got a category of demand...
;This is a priorityq of unfilled demands.
;We traverse the priorityq in priority order, trying to fill.
;If we can't fill, or only partially fill the next demand, we stop traversing.
  ;enforces the hierchical fill constraints. 
;so...filling a category implies traversing a sorted map of unfilled demands. 
;note -> we can modify the sorting key to include followon information, to make 
;things homogenous, just adding a special comparison function. 

(defn unfilled-categories [demandstore] 
  (keys (:unfilledq demandstore)))

(defn unfilled-demandnames [category demandstore]
  (get-in demandstore [:unfilledq category]))

(defn unfilled-demands
  ([category demandstore ctx]
    (into (sorted-map)
          (for [[k nm] (unfilled-demandnames category demandstore)]
            [k (store/get-entity ctx nm)])))
  ([category demandstore]
   (if-let [ctx (:ctx (meta demandstore))]
     (unfilled-demands category demandstore ctx)   
     (throw (Exception. (str [:no-context-in-meta]))))))


;;Right now, we're storing demands in their entirety in the demandmap...
;;We really just want to store the entity's name...
;;Optionally, we can elevate this to a core function, using the
;;store, and from there, we have access to the demand from a sole source...
;;I think that's the best thing to do...
(defn get-demand [demandstore name]
  (store/get-entity (:ctx (meta demandstore)) name))
;  (get-in demandstore [:demandmap name]))


;Simple api function to group active demands from the store by their src. 
(defn demands-by-src [store src] (get-in store [:unfilledq src]))
 
;1) Tom Note 20 May 2013 -> It would be nice to have a function or macro for 
;   defining nested updates like this, as it will probably happen quite a bit.
(defn remove-demand [demandstore demandname]
  (if (contains? (:demandmap demandstore) demandname)
    (let [{:keys [activations deactivations demandmap]} demandstore
          demand (get demandmap demandname)
          dname  (:name demand)
          tstart (:startday demand)
          tfinal (+ tstart (:duration demand))]
      (-> demandstore                                                        ;1)
        (gen/deep-update [:demandmap] dissoc    dname)
        (gen/deep-update [:activations tstart]   dissoc dname)
        (gen/deep-update [:deactivations tfinal] dissoc dname)))
    demandstore)) 

;procedure that allows us to process a set of tags indicating associated demands
;that should be disabled.  if removal is true, the demands will be 
;removed from memory as well. in cases where there are a lot of demands, this 
;may be preferable.
(defn scope-demand [demandstore disable-tags & {:keys [removal]}]
  (let [tags    (:tags demandstore)
        f       (if removal #(remove-demand %1 %2) (fn [m k] m))]     
    (reduce (fn [store demand-name] 
              (let [demands (:demandmap store)]
                (if (contains? demands demand-name)
                  (f (core/disable store demand-name) demand-name) store)))
      demandstore (mapcat (partial tag/get-subjects tags) disable-tags))))


;Simple wrapper for demand update requests.  
(defn request-demand-update! [t demandname ctx]
  (sim/request-update t demandname :demand-update ctx))

;;#Demand Notifications
(defn request-fill! [demandstore category d ctx]
  (sim/trigger-event :RequestFill (:name demandstore) (:name demandstore)
     (str "Highest priority demand in Category " category " is " (:name d) 
          " with priority " (:priority d)) nil ctx))

(defn trying-to-fill! [demandstore category ctx] 
  (sim/trigger-event :RequestFill (:name demandstore) (:name demandstore) 
     (str "Trying to Fill Demand Category " category) nil ctx))

(defn fill-demand! [demandstore demandname ctx]
   (sim/trigger-event :FillDemand (:name demandstore) (:name demandstore)
      (str "Sourced Demand " demandname) nil ctx ))

(defn can-fill-demand! [demandstore demandname ctx]
  (sim/trigger-event :CanFillDemand (:name demandstore) 
       demandname (str "Completely Filled " demandname) nil ctx))

(defn demand-fill-changed! [demandstore demand ctx]
  (sim/trigger-event :DemandFillChanged (:name demandstore) (:name demand) 
     (str "The fill for " (:name demand) " changed.") demand ctx))

(defn sourced-demand! [demandstore demand ctx]
  (sim/trigger-event :FillDemand (:name demandstore) (:name demandstore) 
     (str "Sourced Demand " (:name demand)) nil ctx))

(defn activating-demand! [demandstore demand t ctx]
  (let [demandname (:name demand)]
    (sim/trigger-event :ActivateDemand (:name demandstore)  demandname
                       (str "Activating demand " demandname " on day " t) nil ctx)))

(defn deactivating-demand! [demandstore demand t ctx]
  (let [dname (:name demand)]
    (sim/trigger-event :DeActivateDemand (:name demandstore) dname
       (str "DeActivating demand " dname " on day " t) dname ctx)))

;Look into unifying this with deactivating-unfilled....seems redundant.
(defn deactivating-empty-demand! [demand t ctx]
  (sim/trigger-event :DeActivateDemand :DemandStore (:name demand)  
       (str "Demand " (:name demand) " Deactivated on day " t
            " with nothing deployed ") nil ctx))    

(defn deactivating-unfilled! [demandstore demandname ctx] 
  (sim/trigger-event :DeActivateDemand (:name demandstore) demandname 
       (str "Demand " demandname " was deactivated unfilled") nil ctx))

(defn removing-unfilled! [demandstore demandname ctx] 
  (sim/trigger-event :FillDemand (:name demandstore) demandname
     (str "Removing demand " demandname " from the unfilled Q") nil ctx))

(defn adding-unfilled! [demandstore demandname ctx] 
  (sim/trigger-event :RequestFill (:name demandstore) demandname  ;WRONG                   
     (str "Adding demand " demandname " to the unfilled Q") nil ctx))

(defn ghost-returned! [demand unitname ctx]
  (sim/trigger-event :GhostReturned (:src demand) unitname 
     (str "Ghost for src " (:src demand) " left deployment.") nil ctx))

(defn sending-home! [unitname ctx] 
  (sim/trigger-event :supply-update :DemandStore unitname 
     (str "Send Home Caused SupplyUpdate for " unitname) nil ctx))

(defn disengaging! [demand unitname ctx]
  (sim/trigger-event :DisengageUnit :DemandStore unitname 
     (str "Disengaging unit" unitname " from de-activated demand" 
        (:name demand)) nil ctx))

(defn disengaging-home! [demandstore demand unit ctx]
  (sim/trigger-event :DisengageUnit (:name demandstore)  (:name unit) ;WRONG
     (str "Sending unit" (:name unit) 
          "home from demand" (:name demand)) unit ctx))

(defn overlapping! [demandstore demand unit ctx]
  (sim/trigger-event :overlapping-unit (:name demandstore) (:name unit)
        (str "Overlapping unit" (:name unit) " in demand" 
             (:name demand)) unit ctx))

(defn registering-demand! [demand ctx]
  (sim/trigger-event :added-demand "DemandStore" "DemandStore" 
       (core/msg "Added Demand " (:name demand)) nil ctx))

;;We can revisit this in the entitystore context in the future...
;;Can probably store this in a flatter context.
;;#Demand Registration and Scheduling
(defn get-activations   [dstore t]
  (let [dmap (:demandmap dstore)]
    (set (filter dmap (get-in dstore [:activations t] nil)))))
(defn set-activations   [dstore t m] (gen/deep-assoc dstore [:activations t] m)) ;;requires a double assoc.

(defn get-deactivations [dstore t]
  (let [dmap (:demandmap dstore)]
    (set (filter dmap  (get-in dstore         [:deactivations t] nil)))))

(defn set-deactivations [dstore t m] (gen/deep-assoc dstore [:deactivations t] m))

;TOM note 27 Mar 2011 ->  I'd like to factor these two methods out into a single 
;function, discriminating based on a parameter, rather than having two entire 
;methods.
;Register demand activation for a given day, given demand.
;TOM Change 7 Dec 2010
(defn add-activation [t demandname dstore]
  (let [actives (get-activations dstore t)]
    (set-activations dstore t (conj actives demandname))))

;Register demand deactviation for a given day, given demand.
;1)Tom Note 20 May 2013 -> Our merge-entity function looks alot like entity 
;  updates in the component-based model.  Might be easy to port...
(defn add-deactivation [t demandname demandstore]
  (let [inactives (get-deactivations demandstore t)
        tlast     (max (:tlastdeactivation demandstore) t)]   
    (-> (assoc demandstore :tlastdeactivation tlast)
        (set-deactivations t (conj inactives demandname)))))

;Schedule activation and deactivation for demand. -> Looks fixed.
(defn schedule-demand [demand demandstore ctx]
  (let [{:keys [startday name duration]} demand
        endday (+ startday duration)
        demandname (:name demand)]
    (->> ctx
         (request-demand-update! startday demandname)
         (request-demand-update! endday demandname)
         (core/merge-entity 
          {:DemandStore
           (->>  demandstore
                 (add-activation   startday name)
                 (add-deactivation endday name))}))))

;;Maybe we make this reaallllly simple and focused.  WE only use this 
;;when it makes sense for bulk updates.  There should be a few
;;hotspots where we're doing bulk updates (particularly updating
;;supply).

;;Can we efficiently register demands using this api?  Let alone 
;;anything else....


;;Another option....we have a dynamic var: *mutables* 
;;From we assoc all these guys on there...
;;Inside a transaction, when we create a mutable, we just 
;;assoc it to the existing mutables.
;;Then define functions that, when acquiring resources, prefer 
;;to use the mutable version first.
;;If the mutable version exists, operations like repacking are 
;;not necessary.
;;This brings us back to splicing alternate code paths.....where 
;;we have a mutable and an imperative version that are largely
;;identical.  Some of these are mitigated by unifying assoc 
;;and friends...

;;efficiently register demands...This is a trial run of the new cells 
;;API, to see what practical problems arise when we actually try to
;;use it!

;;At the very least, having an interface to call out high level 
;;mutation and push it to specific functions is nice....better than 
;;what was there...
;(comment) 

(defn register-demand! [ctx demand demands dstore pstore]
  (assert false "Only use persistent version for now pls..")
  (let [demand   (core/ensure-name demand demands)
        dname    (core/entity-name   demand) ;;replace with entity-name                      
        newstore (tag-demand demand (add-demand dstore demand))
        _        (policy/register-location dname pstore)]
    (->> (store/add-entity ctx demand)
         (registering-demand! demand)     ;;doesn't care.         
         ;;this should still be fast.  Alternately just modify locs directly...
         (schedule-demand demand newstore))))  

;;Note -> there's a setup here for bad things to happen.  I forgot
;;that the reduction function I was using, while happily
;;side-effecting as an optimization, was actually tossing out 
;;(i.e. not aggregating) the activation and deactivations for 
;;the demands.  Good news was it wasn't a bug in the cellular 
;;stuff, it was a bug in my logic...I was retaining persistent 
;;information in dstore.  So, the net effect is that if you 
;;want to take advantage of fine-grained mutation, you have 
;;to call out where the mutation lies.  This could lead to some 
;;problems and oversight if we're not careful....
(defn register-demands!
  ([register-f xs ctx]
    (assert false "Only use persistent version for now pls..")
     ;;Cleaner representation, allow multiple cells to be defined in a
     ;;single binding.
     ;; (core/with-cells [{locations     [:state :policystore :locationmap]
     ;;                    demandtags    [:state :demandstore :tags]  
     ;;                    demands       [:state :demandstore :demandmap]
     ;;                    activations   [:state :demandstore :activations] ;forgot this guy, has
     ;;                    deactivations [:state :demandstore :deactivations]
     ;;                    :as txn}    ctx]
     ;;   (update-txn!
     ;;    (core/with-transient-cells [locations demandtags demands activations deactivations]
     ;;      (reduce  (fn [acc demand]                     
     ;;                 (-> (register-demand! acc demand demands (core/get-demandstore acc) (core/get-policystore acc))
     ;;                     (register-f  demand))) txn xs))))
     )
  ([xs ctx] (register-demands! (fn [ctx d] ctx) xs ctx))

  )

;; (defn register-demands! 
;;   ([register-f xs ctx]
;;      ;;Cleaner representation, allow multiple cells to be defined in a
;;      ;;single binding.
;;      (core/with-cells [{locations     [:state :policystore :locationmap]
;;                         demandtags    [:state :demandstore :tags]  
;;                         demands       [:state :demandstore :demandmap]
;;                         activations   [:state :demandstore :activations] ;forgot this guy, has
;;                         deactivations [:state :demandstore :deactivations]
;;                         :as txn}    ctx]
;;        (update-txn!
;;         (core/with-transient-cells [locations demandtags demands activations deactivations]
;;           (let [pstore    (core/get-policystore txn) ;has mutable cells inside txn
;;                 dstore    (core/get-demandstore txn) ;has mutable cells inside txn                
;;                 ]
;;           (reduce  (fn [acc demand]                     
;;                      (-> (register-demand! acc demand demands (core/get-demandstore acc) (core/get-policystore acc))
;;                          (register-f  demand))) txn xs))))))
;;   ([xs ctx] (register-demands! (fn [ctx d] ctx) xs ctx)))

;;Non-mutable version...
(defn register-demand 
  ([demand demandstore policystore ctx]
     (let [dname    (:name demand)
           newstore (tag-demand demand (add-demand demandstore demand))]
       (->> (store/add-entity ctx (:name demand) demand)
            (registering-demand! demand)
            (core/merge-entity {:PolicyStore (policy/register-location dname policystore)})
            (schedule-demand demand newstore))))
  ([demand ctx] (register-demand demand (core/get-demandstore ctx)
                                 (core/get-policystore ctx) ctx)))

;;persistent version is back for now...
(defn register-demands
  [xs ctx] (reduce (fn [acc d] (register-demand d acc) ) ctx xs))

;;cop-out, see if it's faster...
(comment
  (defn register-demand  [demand ctx] (register-demands! [demand] ctx))
)




;; ;;Alternate formulation
;; (deftransaction register-demands! {dstore    [:state :demandstore]
;;                                    pstore    [:state :policystore]
;;                                    locations {pstore [:locationmap]}
;;                                    demandtags {dstore [:tags]}
;;                                    demands    {dstore [:demands]}}
;;   ...) 


;; ;;even better....
;; (transaction
;;  register-demands! [{:resources [dstore pstore locations demandtags demands] 
;;                      :or {dstore [:state :demandstore]
;;                           pstore [:state :policystore]
;;                           locations {pstore [:locationmap]}
;;                           demandtags {dstore [:tags]}
;;                           demands    {dstore [:demands]}} :as resources}]
;;  ....do work 
;;  (update-resources!))



  
                                   
                                               
                                   
        

;;another way to look at this guy is that we're modifying disparate
;;domains via a transaction.  Specifically, we're working on multiple
;;things simultaneously.

;;Maybe we create a transactional system that understands how to
;;provide mutable references for processing "inside" the transaction.
;;This is like STM....transactions expect to have access to 
;;resources so they can do their jobs.  So we can provide a 
;;transactional context, to decouple the "storage" of stuff from 
;;the processing of stuff.  This is close to how the entity system 
;;decouples component data from how that data is operated on in 
;;systems.

;;In an ideal world, we'd really just love to have a flat map of 
;;resources, or bindings, that form the local transactional context.
;;Given that, we can drastically simplify the operations...i.e. 
;;a transactional function operates on a map and returns a map.
;;What can transactions do? 
;;  Some times we want to add resources to the transaction, i.e.
;;  establish a handle to them for bulk-loading or more efficient 
;;  processing (via mutation).  Maybe we just don't want to pay 
;;  the lookup cost over and over again.  
;;  These resources should be shared within a transaction....;
;;  In other words, we allow transactions to a) bind resources 
;;  to the transactional context, b) fetch resources from 
;;  the transactional context, and c) commit resources to the 
;;  transactional context. 

;;  Under this paradigm, we spend time defining "where" resources 
;;  are, and "how" to acquire them -> the current example is 
;;  using the cells methodolgy to establish resource paths in 
;;  nested maps.  This is a clean divide between acquiring and 
;;  freeing resources...

;;  Transactions can "assume" they have access to everything 
;;  they need, along with facilities for altering the transactional 
;;  context.  When this assumption is violated, we can bomb out 
;;  with an error.  This is akin to a transactional contract.

;;#simple contract, but gets us away from simple, focused computation.
;;  Thus, transactional functions operate without any consideration 
;;  of how the resources are managed.  They retain the pure functional
;;  aspect of the current design.  We know what the "input" contract
;;  is explicitly:  transactional functions request specific resources 
;;  from the context, and if those do not exist, they bomb.

;;  What about the output contract?  What should a transactional 
;;  function "return"?  Perhaps a sequence of effects, if any?  
;;  At the very least, it needs to return the transactional 
;;  context.  This lets us define a monadic setting for operating 
;;  on this stuff.  It also keeps the interface uniform, akin to 
;;  the state monad.  Transactors take a context, and other args, 
;;  and return the context.  This is pretty consistent with the 
;;  majority of our functions that operate on simstate, but 
;;  small auxillary functions are - currently - at a loss.
;;  The aux functions like those in sim.demand, expect to 
;;  operate on demandstore's for the most part, or policystores.
;;  We can (and do) pack that into the transactional context.
;;  However, 

                
  

;;bulk loading functions, experimental.
;;If we could pass in the demandstore as an atomic reference, 
;;that would suffice....then we update the accumulated 
;;reference at the end...
;; (defn register-demands [demands demandstore policystore ctx]
;;   (let [dstore (core/->cell demandstore)
;;         pstore (core/->cell policystore)]
;;     (reduce (fn [acc demand]
;;               (let [dname    (:name demand)
;;                     newstore (tag-demand demand (add-demand dstore demand))]
;;                 (->> (registering-demand! demand ctx)         
;;                      (core/merge-entity {:policystore (policy/register-location dname policystore)})
;;                      (schedule-demand demand newstore)))) 

;utility function....
(defn pop-priority-map [m]
  (if (empty? m) m (dissoc m (first (keys m)))))


(defn drop-unfilled-demand [demandstore demand  ctx]
  (let [unfilled (:unfilledq   demandstore)
        src      (:src demand)
        fill-key (priority-key demand)]
        ;;basically - drop-unfilled-demand
    (if (contains? unfilled src) ;either filled or deactivated
      (let [demandq      (get unfilled src)
            nextq        (dissoc demandq fill-key)
            nextunfilled (if (zero? (count nextq))
                           (dissoc unfilled src) 
                           (assoc  unfilled src nextq))]
        (->> (removing-unfilled! demandstore (:name demand) ctx)
             (core/merge-entity {:DemandStore (assoc demandstore :unfilledq nextunfilled)})))              
      (deactivating-unfilled! demandstore (:name demand) ctx))     ;notification
    ))

;;Register the unfilled demand entity and update the demandstore's unfilled.  We
;;just track the name of the entity.
(defn add-unfilled-demand [demandstore demand ctx]
  (let [unfilled  (:unfilledq   demandstore)
        src       (:src demand)
        demandq   (or (get unfilled src) (sorted-map))
        fill-key  (priority-key demand)
        _ (debug [:adding fill-key :to src ;:/ demandq
                  ])
        ]  
    (if (contains? demandq fill-key) ctx ;pass-through
        (->> (core/merge-entity ;add to unfilled 
              {:DemandStore 
               (gen/deep-assoc demandstore [:unfilledq src] 
                               (assoc demandq fill-key (:name demand)))} ctx) ;WRONG?
             (adding-unfilled! demandstore (:name demand))))))

;;#Managing the Fill Status of Changing Demands
;;Unfilled demands exist in a priority queue, UnfilledQ, ordered by the demand's 
;;priority field value - typically an absolute, static ordering, set at 
;;construction.  
;;UnfilledQ partitions the set of active demands that are unfilled.
;;When we go to look for demands that need filling, we traverse keys in 
;;priority order.  
;;We use a priority queue to effeciently respond to changes in a demand's fill 
;;status, and to only try to fill demands when we need to.  Older algorithms 
;;naively polled the demands, and led to quadratic complexity.  In this case, 
;;the priority queue lets us implement a push algorithm, and if we're filling 
;;hierarchically, we can terminate the fill process early if we fail to satisfy
;;a demand.  
 
;;Basic demand events are Activation, Deactivation, Fill, Unfill.
;;Activation and unfill events manifest in demandnames being added to the 
;;unfilledQ. Deactivation and filled events manifest in demandnames being 
;;removed from the unfilledQ. We want to effeciently find out if 
;;there are unfilled demands,  what the most important unfilled demand is, 
;;and what happens when we fill the demand (take the demand off the q or not?)
;;Using the fill queue, the update-fill function provides a general hub to 
;;enforce these invariants.  You'll see it get used a bit when we make changes
;;to a demand, either filling, activating, etc.  It applies the appropriate 
;;processing to keep a demand's fill status consistent.
;;TODO# Simplify this guy?  Maybe break up the detection phase by
;;activated/deactivated....
(defn update-fill
  "Derives a demand's fill status based on its current data.  Satisfied demands 
   are removed from the unfilled queue, unsatisfied demands are kept or added to
   the unfilled queue.  Deactivating unfilled demands are detected as well.
   Propogates notifications for each special case."
  [demandstore demandname ctx]
  (let [demand   (store/get-entity ctx demandname)]
    (cond  (nil? (:src demand)) (throw (Exception. (str "NO SRC for demand" demandname)))
           (nil? demandname)    (throw (Exception. (str "Empty demand name! " demandname)))
           :else
           (let [ _ (debug [:updfill demandname :required (d/required demand)])]
             ;;The demand is inactive or has no fill, either way we should remove it from fill consideration.
             (if (or (zero? (d/required demand))  ;;if we move to components, active-demand? doesn't need the store..
                     (not (:active demand))) ;demand is filled, remove it
               (drop-unfilled-demand demandstore demand ctx)        
               ;;basically - add-unfilled-demand
                                        ;demand is unfilled, make sure it's added
               (add-unfilled-demand demandstore demand ctx))))))

(comment ;;older version..
(defn update-fill
  "Derives a demand's fill status based on its current data.  Satisfied demands 
   are removed from the unfilled queue, unsatisfied demands are kept or added to
   the unfilled queue.  Deactivating unfilled demands are detected as well.
   Propogates notifications for each special case."
  [demandstore demandname ctx]
  (let [demand   (get-in demandstore [:demandmap demandname])   ;     (store/get-entity ctx demandname) ;;this is get-demand...
        fill-key (priority-key demand)
        unfilled (:unfilledq   demandstore)   ;;do we need the store for this?  Can the unfilledq be an entity?
        ]
    (assert (not (nil? (:src demand))) (str "NO SRC for demand" demandname))
    (assert (not (nil? demandname)) "Empty demand name!")
    (let [required (d/required demand)
          src      (:src demand)
          _ (debug [:updfill demandname :required required])]
      ;;basically - drop-unfilled-demand
      (if (or (zero? required)  ;;if we move to components, active-demand? doesn't need the store..
              (not (active-demand? demandstore demandname))) ;demand is filled, remove it  
        (if (contains? unfilled src) ;either filled or deactivated
          (let [demandq      (dissoc (get unfilled src) fill-key)
                nextunfilled (if (zero? (count demandq)) 
                                 (dissoc unfilled src) 
                                 (assoc  unfilled src demandq))]
            (->> (removing-unfilled! demandstore demandname ctx)
                 (core/merge-entity {:DemandStore (assoc demandstore :unfilledq nextunfilled)})))              
          (deactivating-unfilled! demandstore demandname ctx))     ;notification

        ;;basically - add-unfilled-demand
        ;demand is unfilled, make sure it's added
        (let [demandq (get unfilled src (sorted-map))]  
          (if (contains? demandq fill-key) ctx ;pass-through
            (->> (core/merge-entity ;add to unfilled 
                   {:DemandStore 
                    (gen/deep-assoc demandstore [:unfilledq src] 
                         (assoc demandq fill-key demand))} ctx) ;WRONG?
                 (adding-unfilled! demandstore demandname)))))))) 
)

;;##Describing Categories of Demand To Inform Demand Fill Rules
;;Demands have typically been binned into gross categories, based on the type 
;;of capability required to meet a demand.  Additional complexities arose as
;;subcategories - such as the notion of follow-on demands - became a necessity.
;;This resulted in a multi-phase fill process: where we initially selected 
;;demands to fill based on a simple priority - supplied by the user - we quickly
;;found a set of conditional or meta priorities that required "trying to fill"
;;a class of demands - follow-on eligible demands - with supply first.  This 
;;mapped to the need to utilize pre-deployed supply to avoid incidental waste of 
;;resources, when pre-deployed supply - supply already local to the demand - 
;;could be flowed directly to compatible concurrent local demands. 
;;The need to utilize a special class of supply resulted in a duplication
;;of the fill logic, where the first "phase" tried to exhaust all follow-on 
;;supply, with the second or general phase determining demand priority under 
;;a simple priority scheme.   

;;#Categories are a Little Language for Relating Supply to Demand
;;As the potential for additional "special cases" of fill arose, it became 
;;obvious that a general mechanism for unambiguously describing cases would 
;;simplify the filling logic, and allow for future rule expansions.
;;After a lengthy thinking cycle, I resolved to define a useful little language
;;for describing categories, that is, elements of demand and the contextual 
;;information for how they should be filled.  Categories are encoded as simple 
;;clojure data structures, and interpreted by both the demand system and the 
;;supply system to affect queries.  Thus, they act as a cross-domain 
;;protocol for matching and ordering entities.

;;This should eliminate a slew of duplication and complexity from the legacy 
;;implementation, since we can use the category of fill to select eligible 
;;demands, and to inform suitability of supply.  

;;#Category Examples

;;In the simple case, when the category is a string, or a key, we act like 
;;normal filling..
;;__(fill-category "SRC2")__ 
;;=> find all eligible demands for SRC2.

;;In a complex case, when the category is a sequence, we parse it according to 
;;the following:
;;Pairs define a more constrained category, which is interpreted as a category 
;;of SRC and a grouping.  

;;__(fill-category ["SRC2" "Group1"])__ 
;;=> find the eligible demands for SRC2, where the demand group is group1.

;;If the category rule is a pair, and the second element is also a sequence, 
;;the sequence is parsed as a simple set filter.  The second element then forms
;;a union query that can incorporate multiple demand groups into the criteria.

;;__(fill-category ["SRC2" ["Group1" "Group2"]])__ 
;;=> find the eligible demands for SRC2, where the demand group is either Group1 
;;or Group2.

;;Detailed categories come in the forms of maps, which are interpreted as a 
;;category pair, and a set of filters by default.  We can use tags to create 
;;general filters as well, which should let us create a unique set of queries.
;;A trivial (and useful) extension will be to allow arbitrary tags to be 
;;included.  For instance, the map associated with the :supply-tags key in the 
;;example could result in an additional tag query to be executed against 
;;candidate supply.

;;(fill-category {:demand {:src "SRC1" :demandgroup "Group2"}    
;;                :supply-tags {:just-in-time :state-side}})  

;;Categories serve to link supply AND demand, since categories are parsed by 
;;ISupplier functions into supply ordering queries.
;;For example,  from the supply-side, we determine which supply is eligible to 
;;fill an ["SRC1" "Group2"] category of demand:  
;;By default, the query should look for supply matching "SRC1", and all feasible
;;substitutes for "SRC1", where the constraint that the supply followon code 
;;also equals "Group2".

;;Categories can be interpreted an arbitrary number of ways, but the preceding
;;conventions should cover both the 90% use cases, for simple SRC matches, and 
;;almost unlimited extensibility via encoding categories as maps, to be 
;;interpreted by an ISupplier.  This also simplifies the demand fill logic, 
;;since we can implement the same "phased" or hierarchical fill process by 
;;encoding the information in the category being filled.  The solution to the 
;;existing "follow-on" fill still provides a phased fill approach, but it uses
;;the same demand ordering function (rather than duplicating and slightly 
;;modifying).  This makes it possible to define a plethora of filling schemes
;;by either querying or building demand categories, and composing eligibilty 
;;functions in a desired order.

;;#Demand Category Methods
;;Categories are used by two new functions: __find-eligible-demands__ , 
;;defined here, and __find-supply__,  defined in marathon.sim.fill.

;;placeholder for allowing out of order definitions, defined later.
(declare find-eligible-demands)

;;Note -> __find-eligible-demands__ is...in a general sense, just a select-where 
;;query executed against the demandstore...        
(defmulti category->demand-rule core/category-type)

;;Interprets a category as a rule that finds demands based on an src.
;;Note -> we should probably decouple the get-in part of the definition, and 
;;hide it behind a protocol function, like get-demands.  
(defmethod category->demand-rule :simple [src]
  (fn [store ctx] (get-in store [:unfilledq src]))) ;priority queue of demands.

;;Interprets a category as a composite rule that matches demands based on an 
;;src, and filters based on a specified demand-group.
;;[src demandgroup|#{group1 group2 groupn}|{group1 _ group2 _ ... groupn _}]  
(defmethod category->demand-rule :src-and-group [[src groups]]
  (let [eligible? (core/make-member-pred groups)] ;make a group filter
    (fn [store ctx]
      (->> (seq (find-eligible-demands store src ctx)) ;INEFFICIENT
        (filter (fn [[pk d]] (eligible? (:demandgroup d))))
        (into (sorted-map)))))) ;returns priority-queue of demands.

;;matches {:keys [src group]}, will likely extend to allow tags...
;;Currently, provides a unified interface for rules, so we can just use simple 
;;maps. Should probably migrate other calls to this simple format.
(defmethod category->demand-rule :rule-map [category]
  (let [has-every-key? (comp every? #(contains? category %))]
    (cond (has-every-key? [:src :groups]) 
             (category->demand-rule ((juxt :src :groups) category))  
          (has-every-key? [:src]) 
             (category->demand-rule (:src category))                  
          :else ;Future extensions.  Might allow arbitrary predicates and tags. 
             (throw (Exception. "Not implemented!")))))      

;;#Finding Demands by Category
;;High-level API to find a set of eligible demands that match a potentially 
;;complex category.  Uses the category->demand-rule interpreter to parse the 
;;rule into a query function, then applies the query to the store.

;;Tom update 19 May 2016 -> used to return [[pk demand]] map, but
;;now it implictly returns [[pk demandname]] map, so we need to add a layer
;;to get the actual demands...
(defn find-eligible-demands 
  "Given a demand store, and a valid categorization of the demand, interprets
   the category into a into a demand selection function, then applies the query
   to the store."
  [store category ctx]
  (->> ((category->demand-rule category) store ctx)
         (map (fn [[pk demand]] ;;if we only have demand names, we coerce them to demands.
                [pk (if (map? demand) demand (store/get-entity ctx demand))]))))

;;##Demand Scheduling
;;In addition to serving demands for filling, scheduling demands for activation 
;;and deactivation is a primary service of the demand system.  The following 
;;functions define operations that pertain to the timing and updating of 
;;demands. 

;;#Demand Activation
;;Over time, we maintain a set of active demands which will help us only fill 
;;active demands. How do demands get added to the set? Upon initialization, we 
;;schedule their activation day and deactivation day (start + duration). During
;;the course of the simulation, we have a listener that checks to see if the 
;;current day is a day of interest, specifically if it's an activation day or
;;a deactivation day. 

(defn activations? [t demandstore] 
  (contains? (:activations demandstore) t))
(defn deactivations? [t demandstore] 
  (contains? (:deactivations demandstore) t))

;(defn  get-activations  [demandstore t] (get (:activations demandstore) t))
;(defn  get-deactivations [demandstore t] (get (:deactivations demandstore) t))

;;activate-demand => add the demand to the active set.
;;update the fill for the active demand
;; more specifically - list the newly activated demand as unfilled.
;;   --given the demand's category, update the unfilled queue...
;;   --alternative, sort demands as needed when filling.

;;note: we have some simple component-level queries that pop out here...
;;active-demands == entities with a demand component, and an active component...
;;inactive-demands == entities with a demand component, not in active component...
;;most of the time, we only care about active...

;;unfilled-demands == active demands with an unfilled component.

;;Rather than mutating a structure in a nested object,
;;we can see activating a demand as marking the demand active,
;;and computing the requirement for the demand.
;;Note: we can update the unfilledq at each step, after activating
;;or deactivating demands...
;;If we activate demands, the unfilledq is dirty.
;;We need to reorder the queue(s).

;;Really, we have one or more lists of fills based on the category of
;;demand...
(defn activate-demand
  "Shifts demand named dname to the active set of demands, updates its fill status, and 
   notifies any interested parties."
  [demandstore t d ctx]
  (let [dname (:name d) 
        store (-> (gen/deep-assoc demandstore [:activedemands dname] dname)
                  (register-change dname))]               
    (->> (assoc d :active true)
         (store/add-entity ctx)
         (activating-demand! store d t)
         (update-fill store dname)
          )))

;;We should make activate-demands a bulk operation...
;;and phrase activate-demand in terms of a singleton.
(defn activate-demands
  "For the set of demands registered with the context's demand store, relative 
   to time t, any demands scheduled to start at t are activated."
  [t ctx]
  (let [demandstore (core/get-demandstore ctx)]
    (reduce (fn [ctx dname]
              (let [store (core/get-demandstore ctx)]
                (activate-demand store t
                                 (store/get-entity ctx dname) ;(get-demand store dname)
                                 ctx))) 
            ctx 
            (get-activations demandstore t))))



;;#Shifting Elements of Supply To and From Demand
;;As supply is selected to fill demand, the supply is actively assigned to a 
;;specific demand.  From this status, we may see supply stay there indefinitely,
;;shift from an actively assigned state to an overlapping state, or completely 
;;disengage from the demand and return to the global supply.  The following 
;;functions implement these scenarios.

(defn withdraw-unit
  "Auxillary function that dissociates an element of supply, unit, from a 
   related element of demand, demand.  Typically used when a demand deactivation
   prematurely sends a unit home.  Shifts the unit entity into a withdraw state,
   and notifies systems of the entity's abrupt withdraw."
  [unit demand ctx]
  (let [demandgroup (:demandgroup demand)
        unitname    (:name unit)
        _ (debug [:withdraw-unit unitname])
        ]
	  (cond 
	    (and demandgroup (not= "" demandgroup) (not (ungrouped? demandgroup)))
            (do  (debug :abw1)
                 (let [ctx (store/assoce ctx unitname :followoncode  demandgroup)
                       _ (debug [:pre-abw unitname (store/gete ctx unitname :last-update)])]
                   (u/change-state (store/get-entity ctx unitname) :abrupt-withdraw 0 0 ctx)))
            (not (ghost? unit))
            (do  (debug :abw2)
                 (u/change-state (store/get-entity ctx unitname) :abrupt-withdraw 0 0 ctx))
	    :else (->> (if (ghost? unit) (ghost-returned! demand unitname ctx) ctx)  
                       (u/change-state (store/get-entity ctx unitname) :Reset 0 nil)))))                     


;;We can swap out with-draw-unit with something else...
;;Since we don't need to alter the unit's state, it's not an immediate withdraw,
;;Specifically, we have location-based-behavior telling us when to go home.
(defn send-home
  "Implements the state changes required to deactivate a demand, namely, to send
   a unit back to reset. The cases it covers are times when a demand is 
   deactivated, and units are not expecting to overlap. If units are 
   overlapping at a newly-inactive demand, then they get sent home
   simultaneously."
  [t demand unit ctx] 
  (let [unitname    (:name unit)
        startloc    (:locationname unit)
        demandgroup (:demandgroup demand)
        _ (debug [:demand/send-home (:name unit) :t t])
        
        ]    
    (->> (u/unit-update unit ctx)      ;;we don't want to get caught in an update cycle.
         (withdraw-unit unit demand) 
         (disengaging! demand unitname)))) 



;;#Notes on calling change-state for the unit-level system
;there WILL be things happening to the ctx, possible mutations and such, that 
;we may need to carry forward (although we can discipline ourselves for now).

;change-state will have to return the unit that changed, as well as updated 
;supply, demand, etc, even context.  so change-state will have big changes...
;We need a way to dispatch based on the changes.
;Again, event handling could work....
;change-state is a high-level simulation transition function for either the 
;unit simulation, or the supply simulation.


;;#Disengagement
;;Disengagement is the primitive process for shifting a unit of supply's role 
;;from actually filling a demand, into an optional overlapping status.  
;;Overlapping supply is still associated with the demand, i.e. proximate and 
;;in use, but it is not ready to return to the global supply.

;TEMPORARY private helper function.....until I figure out a cleaner solution.
;broke out uber function into a smaller pairing of disengagement functions, to 
;handle specific pieces of the contextual change.
(defn- disengage-unit [demand demandstore unit ctx & {:keys [overlap]}]
  (if overlap
    (let [;_ (println [:pre demand])
          demand (d/send-overlap demand unit)
          ;_ (println [:overlapping demand])
          ]
      (->> (store/add-entity ctx demand )
          (overlapping! demandstore demand unit)
          ))
    (->> (send-home (sim/current-time ctx) demand unit ctx)
         (disengaging-home! demandstore demand unit))))

;This is also called independently from Overlapping_State.....
;Remove a unit from the demand.  Have the demand update its fill status.
;Move the unit from the assigned units, to Overlapping Units.
(defn disengage
  "Shifts the unit from being actively assigned to the demand, to passively 
   overlapping at the demand.  Updates the demand's fill status."
 ([demandstore unit demandname ctx overlap]
  (let [demand    (store/get-entity ctx  demandname)
        nextstore (register-change demandstore demandname)
        ctx       (disengage-unit demand demandstore unit ctx :overlap overlap)]  
   ; (if (zero? (d/required demand))
      (update-fill nextstore demandname ctx)  ;;always check...
;      (update-fill demandname (:unfilledq demandstore) demandstore ctx)
  ;    ctx)
  ))
 ([demandstore unit demandname ctx] ;hack....
  (disengage demandstore unit demandname ctx false)))

;; sword of verboseness +1 
(defn remove-unit-from-demand
  "Shifts the unit from being actively assigned to the demand, to passively 
   overlapping at the demand.  Updates the demand's fill status.  Does not 
   update the unit."
 [demandstore unit demandname ctx]
 (let [demand    (d/send-home (get-in demandstore [:demandmap demandname])
                              unit)
       nextstore (register-change demandstore demandname)                   
       ;;ctx       (disengage-unit demand demandstore unit ctx :overlap overlap)       
       ]
   (->> (store/add-entity ctx demand)
        (update-fill  demandstore demandname))))

(defn send-home-units
  "Sends home all units from a demand, essentially freeing consumed resources.  
   Only called against active demands as part of the deactivation process.  If
   demand is empty, a notification is sent, otherwise the demand is cleared of 
   units."
  [demand t ctx]
  (if (empty-demand? demand)
    (do (debug [:empty-demand! (:name demand)])
        (deactivating-empty-demand! demand t ctx))
    (let [us      (vec (concat (keys   (:units-assigned    demand))
                               (keys   (:units-overlapping demand))))
          _       (debug [:sending-home us :from (:name demand) :t t])
          sent    (atom #{})
          nextctx (reduce (fn [acc nm]
                           (do (if (contains? @sent nm)
                                 (throw (Exception. (str [:already-sent-home nm])))
                                 (do (swap! sent conj nm)
                                     (send-home t demand (store/get-entity acc nm) acc)))))
                         ctx us)]         
      (-> nextctx
          (update-entity
           :DemandStore       
           #(gen/deep-assoc % [:demandmap (:name demand)]
                            (assoc demand :units-assigned {}
                                          :units-overlapping {})))))))


;;==todo== stop storing activedemand information in demandstore/activedemands
;;lift it out to a component instead....causing some problems and inefficiencies.
;;#Demand DeActivation
(defn deactivate-demand
  "Frees resources associated with a demand, sending home any units proximate 
   to the demand.  Removes the demand from the active set."
  [demandstore t d ctx]
  (assert (active-demand? demandstore (:name d))
    (throw (Exception. (str "DeActivating an inactive demand: " (:name d))))) 
  (let [store (-> (gen/deep-update demandstore [:activedemands] dissoc (:name d))
                  (register-change (:name d)))
;        _ ;(println [:demand d])
        _ (debug [:deactivating (:name d) :assigned  (keys (:units-assigned d))
                  :overlapping            (keys (:units-overlapping d))])
        d (dissoc d :active)]
    (->> (-> ctx
             (store/add-entity d)
             (store/add-entity store))                      
         (deactivating-demand! store d t)
         (send-home-units d t)
         (update-fill store (:name d)))))    

(defn deactivate-demands
  "Deactivates any demands - in the demandstore of the context - scheduled to 
   end at time t."
  [t ctx]
  (let [demandstore (core/get-demandstore ctx)]
    (reduce (fn [ctx dname] 
              (let [store (core/get-demandstore ctx)                    
                    d     (store/get-entity ctx dname);(get-demand store dname)
                    ]
                (deactivate-demand store t                                  
                                   d
                                   ctx))) 
            ctx 
            (get-deactivations demandstore t))))

;;#Analysis...
(defn profile 
  "Builds a temporal profile of all the demands in the store."
  [store]
  (temporal/active-intervals 
   (temporal/activity-profile (vals (:demandmap store)) :start-func :startday :duration-func :duration)))

;;##Demand Management
(defn manage-demands
  "High level demand management API.  The primary system service used by the 
   simulation engine.  Processes the activation and deactivation of demands 
   at time t, which adds demands to the active set, queuing them for filling,
   or removes active demands, freeing supply resources in the process."
  [t ctx]
  (->> ctx
    (activate-demands t)
    (deactivate-demands t))) 
