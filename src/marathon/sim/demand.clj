;;The demand system provides functions for scheduling demands, as well as 
;;defining the order in which demands are filled.  The namespace contains 
;;primitive functions for operating on demandstores, as well as demand-related
;;notifications and a high-level demand management API.  
;Functions for creating, initializing, resetting, and updating state related to
;the demand simulation are also found here.
(ns marathon.sim.demand
  (:require  [marathon.demand [demanddata :as d]
                              [demandstore :as store]]
             [marathon.sim    [core :as core] 
                              [supply :as supply] 
                              [policy :as policy]
                              [unit :as u]]           
             [spork.sim       [simcontext :as sim]]
             [spork.util      [tags :as tag] [general :as gen]]))

;;##Primitive Demand and DemandStore Operations

(defn can-simulate? [demandstore]
  (> (count (tag/get-subjects (:tags demandstore) :enabled)) 0))

(defn add-fillable [fillrule demandstore]
  (assert (not (contains? (-> demandstore :fillables) fillrule))  
          "Tried to add fillrule multiple times")
  (gen/deep-update demandstore [:fillables] conj fillrule))

(defn remove-fillable [fillrule demandstore]
  (assert (contains? (-> demandstore :fillables) fillrule)  
          "Tried to remove non-existent fillrule")
  (gen/deep-update demandstore [:fillables] disj fillrule))

;TOM ADDED 30 MAy 2013 
(defn add-demand [demandstore demand]
  (gen/deep-assoc demandstore [:demand-map (:name demand)] demand))

(defn manage-changed-demands [day state] ;REDUNDANT
  (gen/deep-assoc state [:demand-store :changed] {}))

(defn clear-changes [demandstore] (assoc demandstore :changed {}))

(defn register-change [demandstore demandname]
  (if (contains? (:changed demandstore) demandname)
    demandstore 
    (gen/deep-assoc demandstore [:changed demandname]  0)))

;TOM Note 20 May 2013 -> need to abstract fillrule, etc. behind a function, 
;preferably one that uses keywords.
;inject appropriate tags into the GenericTags
(defn tag-demand 
  ([demand demandstore extras]
     (->> (tag/multi-tag (:tags demandstore) (:name demand) 
                         (into   [(str "FILLRULE_" (:primaryunit demand)) ;USE KEYWORD
                                  (str "PRIORITY_" (:priority demand)) ;USE KEYWORD
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
          :tags (tag/tag-subject (tag/empty-tags) :Sinks)}))

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
(defn unfilled-demands [category demandstore] 
  (get-in demandstore [:unfilledq category]))
(defn get-demand [demandstore name] (get-in demandstore [:demandmap name]))
;Simple api function to group active demands from the store by their src. 
(defn demands-by-src [store src] (get-in store [:unfilledq src]))
 

;1) Tom Note 20 May 2013 -> It would be nice to have a function or macro for 
;   defining nested updates like this, as it will probably happen quite a bit.
(defn remove-demand [demandstore demandname]
  (if (contains? (:demand-map demandstore) demandname)
    (let [{:keys [activations deactivations demand-map]} demandstore
          demand (get demand-map demandname)
          dname  (:name demand)
          tstart (:startday demand)
          tfinal (+ tstart (:duration demand))]
      (-> demandstore                                                        ;1)
        (gen/deep-update [:demand-map] dissoc    dname)
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
              (let [demands (:demand-map store)]
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
       demandname (str "Filled " demandname) nil ctx))

(defn demand-fill-changed! [demandstore demand ctx]
  (sim/trigger-event :DemandFillChanged (:name demandstore) (:name demand) 
     (str "The fill for " (:name demand) " changed.") demand ctx))

(defn sourced-demand! [demandstore demand ctx]
  (sim/trigger-event :FillDemand (:name demandstore) (:name demandstore) 
     (str "Sourced Demand " (:name demand)) nil ctx))

(defn activating-demand! [demandstore demand t ctx]
  (sim/trigger-event :ActivateDemand (:name demandstore) (:name demand)
     (str "Activating demand " (:name demand) " on day " t) nil ctx)) 

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
       (str "Added Demand " (:name demand)) nil ctx))

;;#Demand Registration and Scheduling
(defn get-activations   [dstore t]   (get-in dstore [:activations t] #{}))
(defn set-activations   [dstore t m] (gen/deep-assoc dstore [:activations t] m)) ;;requires a double assoc.
(defn get-deactivations [dstore t]   (get-in dstore   [:deactivations t] #{}))
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
;1)Tom Note 20 May 2013 -> Our merge-updates function looks alot like entity 
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
         (sim/merge-updates 
          {:demandstore
           (->>  demandstore
                 (add-activation   startday name )
                 (add-deactivation endday name))}))))

(defn register-demand [demand demandstore policystore ctx]
  (let [dname    (:name demand)
        newstore (tag-demand demand (add-demand demandstore demand))]
    (->> (registering-demand! demand ctx)         
         (sim/merge-updates {:policystore (policy/register-location dname policystore)})
         (schedule-demand demand newstore)))) 

;;bulk loading functions, experimental.
;;If we could pass in the demandstore as an atomic reference, 
;;that would suffice....then we update the accumulated 
;;reference at the end...
;; (defn register-demands [demands demandstore policystore ctx]
;;   (let [dstore (atom demandstore)
;;         pstore (atom policystore)]
;;     (reduce (fn [acc demand]
;;               (let [dname    (:name demand)
;;                     newstore (tag-demand demand (add-demand dstore demand))]
;;                 (->> (registering-demand! demand ctx)         
;;                      (sim/merge-updates {:policystore (policy/register-location dname policystore)})
;;                      (schedule-demand demand newstore)))) 

;utility function....
(defn pop-priority-map [m]
  (if (empty? m) m (dissoc m (first (keys m)))))


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

(defn update-fill
  "Derives a demand's fill status based on its current data.  Satisfied demands 
   are removed from the unfilled queue, unsatisfied demands are kept or added to
   the unfilled queue.  Deactivating unfilled demands are detected as well.
   Propogates notifications for each special case."
  [demandstore demandname ctx]
  (let [demand (get-in demandstore [:demandmap demandname])
        fill-key (priority-key demand)
        unfilled (:unfilledq demandstore)]
    (assert (not (nil? (:src demand))) (str "NO SRC for demand" demandname))
    (assert (not (nil? demandname)) "Empty demand name!")
    (let [required (:required demand)
          src      (:src demand)]
      (if (= required 0) ;demand is filled, remove it
        (if (contains? unfilled src) ;either filled or deactivated
          (let [demandq (dissoc (get unfilled src) fill-key)
                nextunfilled (if (= 0 (count demandq)) 
                                 (dissoc unfilled src) 
                                 (assoc unfilled src demandq))]
            (->> (removing-unfilled! demandstore demandname ctx)
                 (sim/merge-updates 
                   {:demandstore (assoc demandstore :unfilled nextunfilled)})))              
          (deactivating-unfilled! demandstore demandname ctx))     ;notification
        ;demand is unfilled, make sure it's added
        (let [demandq (get unfilled src (sorted-map))]  
          (if (contains? demandq fill-key) ctx ;pass-through
            (->> (sim/merge-updates ;add to unfilled 
                   {:demandstore (gen/deep-assoc demandstore [:unfilled src] 
                       (assoc demandq fill-key demand))} ctx) ;WRONG?
                 (adding-unfilled! demandstore demandname)))))))) 

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
  (fn [store] (get-in store [:unfilledq src]))) ;priority queue of demands.

;;Interprets a category as a composite rule that matches demands based on an 
;;src, and filters based on a specified demand-group.
;;[src demandgroup|#{group1 group2 groupn}|{group1 _ group2 _ ... groupn _}]  
(defmethod category->demand-rule :src-and-group [[src groups]]
  (let [eligible? (core/make-member-pred groups)] ;make a group filter
    (fn [store]
      (->> (seq (find-eligible-demands store src)) ;INEFFICIENT
        (filter (fn [[pk d]] (eligible? (:demand-group d))))
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

(defn find-eligible-demands 
  "Given a demand store, and a valid categorization of the demand, interprets
   the category into a into a demand selection function, then applies the query
   to the store."
  [store category]
  ((category->demand-rule category) store))


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

(defn  get-activations  [demandstore t] (get (:activations demandstore) t))
(defn  get-deactivations [demandstore t] (get (:deactivations demandstore) t))

(defn activate-demand
  "Shifts demand d to the active set of demands, updates its fill status, and 
   notifies any interested parties."
  [demandstore t d ctx]
  (let [store (-> (gen/deep-assoc demandstore [:activedemands (:name d)] d)
                  (register-change (:name d)))]               
    (->> (activating-demand! store d t ctx)
      (update-fill store (:name d)))))    

(defn activate-demands
  "For the set of demands registered with the context's demand store, relative 
   to time t, any demands scheduled to start at t are activated."
  [t ctx]
  (let [demandstore (core/get-demandstore ctx)]
	  (reduce (fn [ctx dname] 
	            (let [store (core/get-demandstore ctx)]
	              (activate-demand store t (get-demand store dname) ctx))) 
	          ctx 
	          (get-activations demandstore))))

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
        unitname    (:name unit)]
	  (cond 
	    (ungrouped? demandgroup) 
	      (->> (core/update-unit (set-followon unit demandgroup) ctx)          
	           (u/change-state unitname :AbruptWithdraw 0 nil))                      
	    (not (ghost? unit))
	      (u/change-state unitname :AbruptWithdraw 0 nil ctx)                   
	    :else (->> (if (ghost? unit) (ghost-returned! demand unitname ctx) ctx)  
	            (u/change-state unitname :Reset 0 nil)))))                     

(defn send-home
  "Implements the state changes required to deactivate a demand, namely, to send
   a unit back to reset. The cases it covers are times when a demand is 
   deactivated, and units are not expecting to overlap. If units are 
   overlapping at a newly-inactive demand, then they get sent home
   simultaneously."
  [t demand unit ctx] 
  (let [unitname  (:name unit)
        startloc  (:locationname unit)
        unit      (u/update unit (- t (sim/last-update unitname ctx))) ;WRONG?
        demandgroup (:demandgroup demand)]
    (->> (sim/trigger-event :supply-update :DemandStore unitname ;WRONG
              (str "Send Home Caused SupplyUpdate for " unitname) ctx) ;WRONG
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
(defn- disengage-unit [demand demandstore unit ctx & [overlap]]
  (if overlap 
    (->> (overlapping! demandstore demand unit ctx)
         (d/send-overlap demand unit))
    (->> (send-home (sim/current-time ctx) demand unit ctx)
         (disengaging-home! demandstore demand unit))))

;This is also called independently from Overlapping_State.....
;Remove a unit from the demand.  Have the demand update its fill status.
;Move the unit from the assigned units, to Overlapping Units.
(defn disengage
  "Shifts the unit from being actively assigned to the demand, to passively 
   overlapping at the demand.  Updates the demand's fill status."
  [demandstore unit demandname ctx & [overlap]]
  (let [demand    (get-in demandstore [:demandmap demandname])
        nextstore (register-change demandstore demandname)
        ctx       (disengage-unit demand demandstore unit ctx overlap)]  
    (if (zero? (:required demand)) 
      (update-fill demandname (:unfilledq demandstore) demandstore ctx)
      ctx)))

(defn send-home-units
  "Sends home all units from a demand, essentially freeing consumed resources.  
   Only called against active demands as part of the deactivation process.  If
   demand is empty, a notification is sent, otherwise the demand is cleared of 
   units."
  [demand t ctx]
  (if (empty-demand? demand)
    (deactivating-empty-demand! demand t ctx)
    (->> (:units-assigned demand) ;otherwise send every unit home.
      (reduce (fn [ctx u] (send-home t demand u ctx)) ctx)
      (sim/merge-updates
        {:demandstore 
         (gen/deep-assoc (core/get-demandstore ctx) [:demandmap (:name demand)]
                   (assoc demand :units-assigned {}))}))))

;;#Demand DeActivation

(defn deactivate-demand
  "Frees resources associated with a demand, sending home any units proximate 
   to the demand.  Removes the demand from the active set."
  [demandstore t d ctx]
  (assert (contains? (:activedemands demandstore) (:name d)) 
    (throw (Exception. (str "DeActivating an inactive demand: " (:name d))))) 
  (let [store (-> (gen/deep-update demandstore [:activedemands] dissoc (:name d))
                  (register-change (:name d)))]
    (->> (deactivating-demand! store d t ctx)
         (send-home-units d t)
         (update-fill store (:name d)))))    

(defn deactivate-demands
  "Deactivates any demands - in the demandstore of the context - scheduled to 
   end at time t."
  [t ctx]
  (let [demandstore (core/get-demandstore ctx)]
    (reduce (fn [ctx dname] 
              (let [store (core/get-demandstore ctx)]
                (deactivate-demand store t (get-demand store dname) ctx))) 
            ctx 
            (get-deactivations demandstore))))

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
