(ns marathon.sim.demand
  (:require [marathon.demand [demanddata :as d]
                             [demandstore :as store]]
            [marathon.sim [supply :as supply]]
            [sim [simcontext :as sim]]
            [util [tags :as tag]]))

;This is the companion module to the TimeStep_ManagerOfDemand class.
;The module contains library functions for the demand simulation in Marathon.  Functions for
;creating, initializing, resetting, and updating state related to the Demand simulation are
;found here.

;The high-level entry point is ManageDemands, which is invoked from the EventStep_Marathon
;method in the TimeStep_Engine class.



;(defn new-demand [name tstart duration overlap primary-unit quantity priority demandstore 
;                  policystore ctx operation vignette source-first]
;
;>>>>>>>>>>>>>>>>>Missing Functionality>>>>>>>>>>>>>>>>>>>>


;<<<<<<<<<<<<<<<<<Missing Functionality<<<<<<<<<<<<<<<<<<<<
(defn can-simulate? [demandstore]
  (> (count (tag/get-subjects (:tags demandstore) :enabled)) 0))

;register-demand is a good example...
;we want it to return a list of updates....
;like...(merge-updates {:demand-store ... :policy-store ...})
;Actually, we can have an event handler that does this....
;or we have an integrating function that weaves stuff across...
;what happends when there's a set of updates, along with events? 
;The events provide yet another form of update to the state as a whole.
;typically the context will provide the focus point for both...
;maybe have, as part of the API, the ability to merge updates with the context,
;as well as trigger events...
;that's it.

(defn register-demand [demand demandstore policystore ctx]
  (let [dname  (:name demand)
        ctx    (sim/trigger-event :added-demand "DemandStore" "DemandStore" 
                                  (str "Added Demand " demand.name) ctx)]
    (->> ctx 
     (sim/merge-updates 
       {:demand-store (tag-demand demand 
                        (schedule-demand demand 
                       (assoc-in demandstore [:demand-map dname] demand) ctx))
        :policy-store (register-location dname policy-store)})))) 

(defn add-fillable [fillrule demandstore]
  (assert (not (contains? (-> demandstore :fillables) fillrule))  
          "Tried to add fillrule multiple times")
  (update-in demandstore [:fillables] conj fillrule))

(defn remove-fillable [fillrule demandstore]
  (assert (contains? (-> demandstore :fillables) fillrule)  
          "Tried to remove non-existent fillrule")
  (update-in demandstore [:fillables] disj fillrule))

;Note -> we're just passing around a big fat map, we'll use destructuring in the 
;signatures to pull the args out from it...the signature of each func is 
;state->state

;Perform a prioritized fill of demands, heirarchically filling demands using followon
;supply, then using the rest of the supply.
(defn fill-demands [t state]
  (->> state 
    (fill-follow-ons t)
    (supply/release-max-utilizers)
    (fill-normal-demands t)))

;TOM Change 21 Sep 2011 -> this is the new first pass we engage in the fill process.
;The intent is to ensure that we bifurcate our fill process, forcing the utilization of follow-on
;units.  We split the fill process into 2 phases...
;   Phase 1:  Find out which demands in the UnfilledQ(FillRule) are eligible for Follow-On supply.
;             If a demand is eligible for follow-on supply (it has a corresponding Group in FollowOns in
;             supplymanager.
;             Add the demand to a new priority queue for the fillrule (lexically scoped).
;             Basically, we;re getting a subset of eligible demands (by fillrule) from the existing unfilledQ for
;             the fillrule.
;
;             Pop ALL Demands off the new priorityQ for the fillrule, trying to fill them using supply from the
;             followonbuckets relative to the applicable demand group. (basically a heapsorted traversal)
;             This may result in some demands being filled.
;               If a demand is filled, we update its fill status (mutate the unfilledQ) just as in the normal fill
;                routine.
;               If it;s not filled, we leave it alone.
;                       *Update -> thre problem is, our fill function, if allowed to make ghosts, will
;                                  KEEP trying to fill, and effectively short circuit our other supply.
;                                  we need to prevent the fill function from making ghosts in phase1!
;             The biggest difference is that we DO NOT stop, or short-circuit the fill process if we don;t find
;             follow-on supply.  We give every eligible demand a look.
;             After phase 1 is complete, all demands that were eligible for follow-ons will have recieved follow-on
;             supply.  Any demands completely filled by follow-ons will have been eliminated from further consideration.
;             Demands with requirements remaining are still in the unfilledQ for our normal, ARFORGEN-based fill.
;   Phase 2:  This is the normal, ARFORGEN-based fill routine.  Anything left in the unfilledQ is processed as we
;             did before.  This time, we should only be looking at ARFORGEN supply (since our follow-on supply was
;             utilized in the Phase 1).

(defn fill-follow-ons 
  [t {:keys [fillstore supplystore parameters demandstore policystore ctx] :as state}]
)

Dim i As Long
;TOM Change 6 Dec 2010 -Added several variables, used for dictionary access
Dim demandname As String
;TODO -> change this.  I don;t think we need it.
Dim DemandCategory As GenericSortedDictionary
Dim Categoryname
Dim canFill As Boolean
Dim demand As TimeStep_DemandData
Dim followonbuckets As Dictionary

Static startfill As Long
Static stopfill As Long

;Check
Set followonbuckets = supplystore.followonbuckets

With demandstore
    ;For each independent set of prioritized demands (remember, we partition based on substitution/SRC keys)
    If followonbuckets.count > 0 Then ;we can try to process follow-on supply...
        For Each Categoryname In .UnfilledQ ;UnfilledQ is a global, want to replace this with a function parameter...
                ;get eligible demands from the unfilled priorityQ....
                ;This is a subset of demands that "may" have eligible supply in the followonBuckets.
                ;Note -> followonBuckets are keyed by {DemandGroup, {FillRule, Supply}}
                ;So if we have eligible demands, we tap into the followonBuckets by the demand;s demandgroup property.
                ;point to the priorityQ of unfilled, homogeneous demands
                Set DemandCategory = _
                    getFollowOnDemands(demandstore, followonbuckets, .UnfilledQ(Categoryname).data)

                ;going to traverse ALL eligible demands to ensure we exhaust our follow-ons.
                ;Note, this is different than our typical supply traversal, where we stop pulling supply after the highest priority
                ;demand goes unfilled.  In here, we ensure that all demands with the potential for
                While DemandCategory.count > 0
                    canFill = False
                    ;try to fill the topmost demand
                    demandname = DemandCategory.removeNext ;values stored in pq are the names of demands
                    If demandname = vbNullString Then Err.Raise 101, , "Demand has no name"
                    Set demand = .demandmap(demandname)

                    ;we CAN short-circuit the follow-on fill if there are no more units for the specified follow-ons in the
                    ;demand group
                    If followonbuckets.exists(demand.demandgroup) Then
                        startfill = demand.unitsAssigned.count
                        ;We want to change the sourcing to use supply from the follow-on buckets....
                        ;we pass the scoped-down set of eligible Supply (from followonbuckets) to the sourcing routine...
                        ;this lets us use the same fillrules, with a different set of supply.

                        ;Decoupling -> need to change this....either passing in Fillmanager as a parameter...or something
                        ;else...
                        ;TODO -> remove the DEF b.s.
                        canFill = MarathonOpFill.sourceDemand(supplystore, parameters, fillstore, ctx, policystore, t, demand, "DEF", followonbuckets(demand.demandgroup), onlyUseFollowons)
                        stopfill = demand.unitsAssigned.count

                        If stopfill <> startfill Then
                            SimLib.triggerEvent DemandFillChanged, demandstore.name, demand.name, "The fill for " & demand.name & " changed.", demand, ctx
                           ;If we fill a demand, we update the unfilledQ.
                            ;UpdateFill demandname, UnfilledQ
                            
                            ;TOM Change 20 Aug 2012...wasn;t recording properly.
                            registerChange demandstore, demand.name
                        End If

                        If canFill Then ;changed sourceDemand to a function returning a boolean, which we use
                            ;Decoupled
                            SimLib.triggerEvent FillDemand, demandstore.name, demandstore.name, _
                                                    "Sourced Demand " & demandname, , ctx
                            ;Decoupled
                            SimLib.triggerEvent CanFillDemand, demandstore.name, demandname, _
                                                    "Filled " & demandname, , ctx
                            UpdateFill demandname, .UnfilledQ, demandstore, ctx
                        Else
                            ;the demand is still on the unfilled queue, we only "tried" to fill it. No state changed .
                            ;Debug.Print "could not fill"
                        End If
                    End If
                Wend ;keep popping off all eligible demands
        Next Categoryname
    End If
End With

End Sub



  
;TOM Note 13 Mar -> we perform some major effeciency improvements.  The most obvious is representing demands as
;objects.  Rather than having a demand for 5 srcs result in 5 physical entries, we just have one demand and keep
;track of its fill.  Major reduction in overhead (and searching).
;TOM Change 6 Dec 2010
;Going to redirect this sub to incorporate unfilledQ. The idea is to look for the highest priority demand,
;by SRC . Specifically, we will be redirecting the portion of the routine that finds "a demand" to be filled.
;In the previous algorithm, we just traversed thousands of demands until we found one with a status of False,
;then tried to fill it.
;In this new algorithm, instead of a loop over each demand in the demanddata array,
;we consult our bookeeping:
;For each independent set of prioritized demands (remember, we partition based on substitutionjSRC keys)
;We use our UnfilledQ to quickly find unfilled demands.
;UnfilledQ keeps demands in priority order for us, priority is stored in DEFDemand(i) .priority
;Note - we can change priority dynamically, interesting possibilities later ....
;We only fill while we have feasible supply left.
;We check deployables to efficiently find feasible supply.
;Not currently kept in priority order .... could be converted easily enough though.
;If we fill a demand, we take it off the queue.
;If we fail to fill a demand, we have no feasible supply, thus we leave it on the queue, stop filling,
;and proceed to the next independent set.
;After all independent sets have been processed, we;re done.
;TOM Change 3 Mar 2011 renamed from DEFSourcing
Public Sub FillNormalDemands(t As Single, fillstore As TimeStep_ManagerOfFill, supplystore As TimeStep_ManagerOfSupply, _
                                parameters As TimeStep_Parameters, demandstore As TimeStep_ManagerOfDemand, _
                                    policystore As TimeStep_ManagerOfPolicy, ctx As TimeStep_SimContext)

;Dim demand As Long
Dim i As Long
;TOM Change 6 Dec 2010 -Added several variables, used for dictionary access
Dim demandname As String
Dim DemandCategory As GenericSortedDictionary
Dim Categoryname
Dim canFill As Boolean
Dim demand As TimeStep_DemandData
Static startfill As Long
Static stopfill As Long
Dim msg As String

;what we're really doing is prioritizing demands...
;trying to fill said demands....
(defn fill-demands [t {:keys [fillstore supplystore parameters demandstore policystore ctx] :as state}])
(defn fill-category [t category unfilledq {:keys [fillstore supplystore parameters demandstore policystore ctx] :as state}]
    (loop [demandq (get category unfilledq)
           demandname (first demandq)
           
  
         

;For each independent set of prioritized demands (remember, we partition based on substitution/SRC keys)
;TOM Change 27 Mar 2011 -> added a mutable filter called fillables, which records demands with known supply.
;This allows the stockwatcher to maintain a list of fillable demands.  We then proceed with the previous
;logic of filling each demand.
;Note -> as of 27 Mar 2011, the fill routine is only putting out a single path (the shortest path).
;This happens in sourcedemand.
With demandstore
    For Each Categoryname In .UnfilledQ ;UnfilledQ
            ;Decoupled
            SimLib.triggerEvent RequestFill, .name, .name, "Trying to Fill Demand Category " & Categoryname, , ctx
            ;We use our UnfilledQ to quickly find unfilled demands.
            Set DemandCategory = .UnfilledQ(Categoryname) ;point to the priorityQ of unfilled, homogeneous demands
            For i = 1 To DemandCategory.count
                canFill = False
                ;try to fill the topmost demand
                demandname = CStr(DemandCategory.nextval) ;values stored in pq are the names of demands
                If demandname = vbNullString Then Err.Raise 101, , "Demand has no name"

                msg = "Highest priority demand in Category " & Categoryname & " is " & demandname & " with priority " _
                        & DemandCategory.HighestPriority
                ;Decoupled
                SimLib.triggerEvent RequestFill, .name, .name, msg, , ctx

                ;We;re going to be passing entire blocks of demand, vs. little atomic components of demand.  This will be
                ;loads more effecient.  That way, the supply manager just fills as much as it can, and replies if the demand
                ;was filled.  IF yes, then we can continue filling this category of demands.
                ;We only fill while we have feasible supply left.
                ;TOM Note -> Source Demand is actually trying to source the demand, although it looks like
                ;a simple boolean check, it;s actually mutating stuff.
                ;If parent.SupplyManager.sourceDemand(day, DemandMap(demandname), "DEF") Then ;changed sourceDemand to a function returning a boolean, which we use
                ;TOM change 22 Mar 2011 -> demandsourcing is now done through fillmanager.
                Set demand = .demandmap(demandname)
                startfill = demand.unitsAssigned.count
                ;Tom Note 27 Mar 2012 - DEF is vestigial....should be excised.
                ;Decoupling again....push fillmanager as fill function.
                canFill = sourceDemand(supplystore, parameters, fillstore, ctx, policystore, t, demand, "DEF", , useEveryone)
                stopfill = demand.unitsAssigned.count
                ;Decoupled
                If stopfill <> startfill Then
                    SimLib.triggerEvent DemandFillChanged, demandstore.name, demand.name, "The fill for " & demand.name & " changed.", demand, ctx
                    ;TOM Change 20 Aug 2012...wasn;t recording properly.
                    registerChange demandstore, demand.name
                End If
                
                If canFill Then ;changed sourceDemand to a function returning a boolean, which we use
                        msg = "Sourced Demand " & demandname
                        ;Decoupled
                        SimLib.triggerEvent FillDemand, demandstore.name, demandstore.name, msg, , ctx
                    ;If we fill a demand, we update the unfilledQ.
                    UpdateFill demandname, .UnfilledQ, demandstore, ctx
                    ;Decouple
                    SimLib.triggerEvent CanFillDemand, demandstore.name, demandname, "Filled " & demandname, , ctx
                Else
                    Exit For ;If we fail to fill a demand, we have no feasible supply, thus we leave it on the queue, stop filling.
                ;Note, the demand is still on the queue, we only "tried" to fill it. No state changed .
                End If
            Next i
            ;Proceed to the next independent set.
        ;End If
    Next Categoryname
End With

;After all categories processed, we;re done. Remaining code is legacy, has been moved, etc.
End Sub

;Return a list of demands that are currently eligible for follow-on
;The returned dictionary is a nested dictionary, nested by Group (FillRule demands)
;This is a subset of the original unfilledQ.

;add the demand if its follow-on group is present.
;note, we don;t know a-priori if the demand will be filled...we;re merely noting that the demand "may" have
;follow-on units, reached through potentially complicated substitution rules, that can be utilized.  we;re just
;scoping the set of demands.
(defn get-followon-demands [demandstore followon-buckets unfilled-demands]
  (->> (map #(get demandstore %) unfilled-demands)
       (filter #(contains? followon-buckets (:demandgroup %)))
       (reduce (fn [m x] (assoc m [(:name x) (:priority x)] (:name x))) {})))   

;follow-on supply is a function of fillrule, demandgroup, followonbuckets

;procedure that allows us to, using the fillgraph, derive a set of tags whose associated demands
;should be disabled.  if removal is true, the demands will be removed from memory as well.
;in cases where there are a lot of demands, this may be preferable.
(defn scope-demand [demandstore disable-tags & {:keys [removal]}]
  (let [tags    (:tags demandstore)
        f       (if removal #(remove-demand %1 %2) (fn [m k] m))]     
    (reduce (fn [store demand-name] 
              let [demands (:demand-map store)]
              (if (contains? demands demand-name)
                (f (disable store demand-name) demand-name) store))
      demandstore (mapcat (partial tag/get-subjects tags) disable-tags))))

(defn remove-demand [demandstore demandname]
  (if (contains? (:demand-map demandstore) demandname)
    (let [{:keys [activations deactivations demand-map]} demandstore
          demand (get demand-map demandname)
          dname  (:name demand)
          tstart  (:startday demand)
          tfinal (+ tstart (:duration demand))]
      (-> demandstore 
        (update-in [:demand-map] dissoc demand-map demand-name)
        (update-in [:activations] update-in activations [tstart] dissoc dname)
        (update-in [:deactivations] update-in deactivations [tfinal] dissoc dname)))
    demandstore)) 

(defn is-enabled [demandstore demandname] 
  (tag/has-tag (:tags demandstore) :enabled demandname))
(defn enable [demandstore demandname]
  (update-in demandstore [:tags] tag/add-tag :enabled demandname))
(defn disable [demandstore demandname]
  (update-in demandstore [:tags] tag/remove-tag :enabled demandname))

;Simple wrapper for demand update requests.
(defn request-demand-update [t demandname ctx]
  (sim/request-update t demandname :demand-update ctx))

;TOM note 27 Mar 2011 ->  I;d like to factor these two methods out into a single function,
;discriminating based on a parameter, rather than having two entire methods.
;Register demand activation for a given day, given demand.
;TOM Change 7 Dec 2010
(defn add-activation [t demandname demandstore ctx]
  (let [actives (get-in demandstore [:activations t] #{})]
    (->> (request-demand-update t demandname ctx) ;FIXED - fix side effect
      (sim/merge-updates {:demandstore (assoc-in demandstore [:activations t] ;UNPRETTY 
                                                 (conj actives demandname))}))))

;Register demand deactviation for a given day, given demand.
(defn add-deactivation [t demandname demandstore ctx]
  (let [inactives (get-in demandstore [:deactivations t] #{})
        tlast     (max (:tlastdeactivation demandstore) t)]
    (->> (request-demand-update t demandname ctx) ;FIXED - fix side effect
      (sim/merge-updates ;UNPRETTY
        {:demandstore (-> (assoc demandstore :tlastdeactivation tlast)
                          (assoc-in [:deactivations t] (conj actives demandname)))}))))

;Schedule activation and deactivation for demand. -> Looks fixed.
(defn schedule-demand [demand demandstore ctx]
  (let [{:keys [startday demand-name duration]} demand]
    (->> (add-activation startday demand-name demandstore ctx)
         (add-deactviation (+ startday duration) demand-name demandstore))))

;TOM Change 6 Dec 2010
;Introducing this sub, to be inserted into the mainmodel loop.
;Its purpose is to maintain a running list of demands (ActiveDemands dictionary), which will help us only fill
;active demands. How do demands get added to the list? Upon initialization, we schedule their activation day
;and deactivation day (start + duration). During the course of the mainmodel loop, we have a listener that
;checks to see if the current day is a day of interest, specifically if it;s an activation day.
;Note that this idea, partitioning our days into "days of interest", while localized to handle demand
;activations and deactivations, could easily be extended to a general "days of interest" manager, with
;subscribed handlers and subroutines to run. In fact, this is exactly what an event step simulation does.
;We;re just copping techniques and modifying them for use in a timestep sim to make it more efficient.
(defn manage-demands [t {:keys [demandstore context] :as state}]
  (let [{:keys [activations 
)

Public Sub ManageDemands(t As Single, state As TimeStep_SimState)
Dim DemandKey
Dim demand As TimeStep_DemandData
Dim ptr As Dictionary
Dim nm
Dim msg As String
Dim demandstore As TimeStep_ManagerOfDemand

Set demandstore = state.demandstore

With demandstore
    If .activations.exists(t) Then
        Set ptr = .activations(t)
        For Each DemandKey In ptr
            Set demand = .demandmap(DemandKey)
            With demand
                If .status = True Then
                    Err.Raise 101, , "Activating an already Active demand, impossible"
                End If
                ;todo -> remove this
                .status = True ;vestigial
                ;If demandtraffic Then
                    msg = "Activating demand " & DemandKey & " on day " & t
                    ;Decoupled
                    SimLib.triggerEvent ActivateDemand, demandstore.name, demand.name, msg, , state.context
                ;End If
                demandstore.activeDemands.add CStr(DemandKey), demand
                ;addEligible demand
                UpdateFill CStr(DemandKey), demandstore.UnfilledQ, demandstore, state.context
                registerChange demandstore, demand.name
            End With
        Next DemandKey
    End If
    ;TOM change 7 Dec 2010 -> added a hook in here to send home deployed/overlapping units, want deactivation to force
    ;them to go to reset.
    If .deactivations.exists(t) Then
        Set ptr = .deactivations(t)
        For Each DemandKey In ptr
            Set demand = .demandmap(DemandKey)
            With demand
                .status = False

                msg = "DeActivating demand " & DemandKey & " on day " & t
                ;Decouple
                 SimLib.triggerEvent DeActivateDemand, demandstore.name, .name, msg, .name, state.context


                If demandstore.activeDemands.exists(CStr(DemandKey)) = False Then
                    Err.Raise 101, , "deactivating something that does not exist ... "
                End If

                demandstore.activeDemands.Remove CStr(DemandKey)
                ;removeEligible demand

                For Each nm In .unitsAssigned ;send all units home.
                    SendHome t, demand, CStr(nm), state.context ;<----- This is the hook
                Next nm
                UpdateFill CStr(DemandKey), demandstore.UnfilledQ, demandstore, state.context ;remove demand from fill consideration
                 ;Tom change 20 aug 2012 -> force the system to sample demands.
                registerChange demandstore, demand.name
            End With
        Next DemandKey
    End If
End With

End Sub


(defn manage-changed-demands [day state] ;REDUNDANT
  (assoc-in state [:demand-store :changed] {}))

;PURE FUNCTIONS :: 'a -> demandstore 
(defn clear-changes [demandstore] (assoc demand-store :changed {}))

(defn register-change [demandstore demandname]
  (if (contains? (:changed demandstore) demandname)
    demandstore 
    (assoc-in demandstore [:changed] demandname 0)))

;inject appropriate tags into the GenericTags
(defn tag-demand [demand demandstore & {:keys [extras]}]
  (update-in demandstore [:tags] tags/multi-tag 
     (concat [(str "FILLRULE_" (:primaryunit demand)) ;USE KEYWORD
              (str "PRIORITY_" (:priority demand)) ;USE KEYWORD
              :enabled] 
             extras)))

(defn tag-demand-sink [demandstore sink]
  (update-in demandstore [:tags] tag/add-tag  :Sinks sink))

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
          :tags (tag/add-tag (tag/empty-tags) :Sinks)}))

;TEMPORARY...
;move this to supply or unit....

;This is a temporary HACK! Reaches too deeply into the state? 
;returns a context.
(defn update-unit [u ctx]
  (assoc-in ctx [:state :supplystore :unitmap (:name u)] u))
(defn set-followon [unit code] (assoc unit :followoncode code))
(defn ghost? [unit] (= (:src unit) "Ghost"))
(defn ungrouped? [g] (= g "UnGrouped"))
(defn empty-demand? [d] (zero? (count (:units-assigned d))))
;END TEMPORARY 

; ::unit->string->context->context 
(defn withdraw-unit [unit demandgroup ctx]
  (cond 
    (ungrouped? demandgroup) 
      (-> (update-unit (set-followon unit demandgroup) ctx) ;assuming update-unit returns a context.      
          (u/change-state :AbruptWithdraw 0 nil ctx)) ;ASSUMES change-state returns a context... 
    (not (ghost? unit)) (u/change-state unit :AbruptWithdraw 0 nil ctx) ;ASSUMES change-state returns a context...
    :else (->> (if (ghost? unit) ;POSSIBLY WRONG
                 (trigger-event :GhostReturned (:src demand) unitname 
                    "Ghost for src " (:src demand) " left deployment." ctx)
                 ctx)  
            (u/change-state unit :Reset 0 nil)))) ;ASSUMES change-state returns a context...

;TOM Change 7 Dec 2010 -> removed deactivation code from sourcedemand, placed it in a separate sub.
;This sub implements the state changes required to deactivate a demand, namely, to send a unit back to reset.
;The cases it covers are times when a demand is deactivated, and units are not expecting to overlap.
;If units are overlapping at a newly-inactive demand, then they get sent home simultaneously
;For this reason, we take the abs(unit) to get both overlapping and bogging units.
;Alex;s convention for overlapping units was a negative value for unitsdeployed.
;TODO -> Remove convention of negative cycle time = BOG, transition to stateful information contained in
;unit data, or derived from Policy data.
;TOM Change 14 Mar 2011 <- provide the ability to send home all units or one unit...default is all units
; :: float -> demand -> string -> context -> context
(defn send-home [t demand unitname ctx] 
  (if (empty-demand? demand) 
    (sim/trigger-event :DeActivateDemand :DemandStore (:name demand)  
       (str "Demand " (:name demand) " Deactivated on day " t
            " with nothing deployed ") ctx) ;RIGHT, just trigger and return ctx.
    (let [old-unit  (get (:units-assigned demand) unitname)
          startloc  (:locationname unit)
          unit (u/update old-unit (- t (sim/last-update unitname ctx))) ;WRONG?
          demandgroup (:demandgroup demand)]
      (->> (sim/trigger-event :supply-update :DemandStore unitname ;WRONG
              (str "Send Home Caused SupplyUpdate for " unitname) ctx) ;WRONG
        (withdraw-unit old-unit demandgroup) 
        (sim/trigger-event :DisengageUnit :DemandStore unitname 
         (str "Disengaging unit" unitname 
              " from de-activated demand" (:name demand))))))) 


;there WILL be things happening to the ctx, possible mutations and such, that 
;we may need to carry forward (although we can discipline ourselves for now).

;change-state will have to return the unit that changed, as well as updated 
;supply, demand, etc, even context.  so change-state will have big changes...
;We need a way to dispatch based on the changes.
;Again, event handling could work....

 
;TEMPORARY.....until I figure out a better, cleaner solution.
;broke out uber function into a smaller pairing of disengagement functions, to 
;handle specific pieces of the contextual change.
(defn- disengage-unit [demand demandstore unit ctx & [overlap]]
  (if overlap 
    (->> (sim/trigger-event :overlapping-unit (:name demandstore) (:name unit) ;WRONG
            (str "Overlapping unit" (:name unit) " in demand" (:name demand)) unit ctx) ;WRONG
         (d/send-overlap demand unit))
    (->> (send-home (sim/current-time ctx) demand unit ctx)
      (sim/trigger-event :disengage-unit (:name demandstore)   (:name unit) ;WRONG
           (str "Sending unit" (:name unit) "home from demand" (:name demand) unit ctx))))) ;WRONG

;RE-LOOK...what are the returns?  what's being operated on?  
;This is also called independently from Overlapping_State.....
;Remove a unit from the demand.  Have the demand update its fill status.
;move the unit from the assigned units, to overlappingunits.
(defn disengage [demandstore unit demandname ctx & [overlap]]
  (let [demand    (get-in demandstore [:demandmap demandname])
        nextstore (register-change demandstore demand-name)
        ctx       (disengage-unit demand demandstore unit ctx overlap)]  
    (if (zero? (:required demand)) ;WRONG 
      (update-fill demand-name (:unfilledq demandstore) demandstore ctx) ;WRONG
;    demandstore)));WRONG
      ctx)))

;Tom change 6 Dec 2010
;Sub to register or deregister Demands from the UnfilledQ
;UnfilledQ partitions the set of demands that are unfilled
;When we go to look for demands that need filling, we traverse the keys linearly.
;Only keys that exist will be filled ...
;Currently, we have no implemented way of prioritizing SRC fills over eachother, but it also should not matter
;due to the independence of the SRC populations.
;Unfilled demands exist in a priority queue, based on the demand;s priority, this is data driven/positional.
;The basic idea is this:
;Upon initialization of the demand, we tag a field in the demand and associate it with priority. This
;will, by default, be the positional location of the demand in the data (i.e. it;s row index). The idea
;is that the user will put data in priority order. Note - with the advent of "priority" associated with
;the demand, we can always just read this in as another parameter in the data. Then position won;t matter,
;only data.
;Basic demand events are Activation, Deactivation, Fill, Unfill.
;Marathon3 currently models these events by filling an array, aDEFDemandData, with a boatload of heterogeneous
;DemandData containers, which contain info on status. Basically, if the demand is active, its status should be
;true. Our goal is to move this information from a polling environment, where we have to traverse the entire
;array in O(N) worst case time, every day, to a pushing environment. In a push environment, we check for the
;existence of unfilled demands very effeciently, 0(1) constant time, and upon discovering the existence, we
;retrieve the highest priority demand in 0(1) time.
;Activation and unfill events manifest in demandnames being added to the unfilledQ.
;Deactivation and filled events manifest in demandnames being removed from the unfilledQ.
;The bottom line is that we want to quickly find out if A. there are unfilled demands, B. what the most important
;unfilled demand is, C. what happens when we fill the demand (take the demand off the q or not?)
;aDEFDemandData is the indicator we use to capture active demands, it;s the only thing that changes state ..
;One more note, this assumes an atomic model of filling demands -> we split demands up into individual units,
;which we fill. Rather than keeping track of the quantity filled at each demand to determine overall fill,
;we only have a binary filled/unfilled in the atomic model. This means we don;t ever check the condition
;that a demand might have gained a unit, via deployment, and still remain unfilled. It;s impossible.

;NEED TO RETHINK THIS GUY, WHAT ARE THE RETURN vals?  Mixing notification and such...
(defn update-fill [demandname unfilled demandstore ctx]
  (let [demand (get-in demandstore [:demandmap demandname])]
    (assert (not (nil? (:src demand))) (str "NO SRC for demand" demandname))
    (assert (not (nil? demandname)) "Empty demand name!")
    (let [required (:required demand)
          src      (:src demand)]
      (if (= required 0) ;demand is filled, remove it
        (if (contains? unfilled src) ;either filled or deactivated
          (let [demandq (dissoc (get unfilled src) demandname)
                nextfilled (if (= 0 (count demandq)) 
                             (dissoc unfilled src) 
                             (assoc unfilled src demandq))]
            (->> (sim/trigger-event :FillDemand (:name demandstore) demandname
                  (str "Removing demand " demandname " from the unfilled Q" nil ctx))
              (sim/merge-updates {:demandstore (assoc demandstore :unfilled nextfilled)})))              
          (trigger-event :DeActivateDemand (:name demandstore) demandname 
             (str "Demand " demandname " was deactivated unfilled") ctx)) ;have a deactivation
        (let [demandq (get unfilled src (empty-sorted-dictionary))] ;demand is unfilled, add it
          (if (not (contains? demandq demandname))
            (do (assoc demandq [demandname (:priority demand)] demand) ;WRONG
              (sim/trigger-event :RequestFill (:name demandstore) demandname  ;WRONG                   
                 (str "Adding demand " demandname " to the unfilled Q") nil ctx)))))))) 



;>>>>>>>>>>>>>>>>>>Deferred>>>>>>>>>>>>>>>>>>>>>>
;We'll port this when we  come to it....not sure we need it...

;Public Sub rescheduleDemands(demandstore As TimeStep_ManagerOfDemand, ctx As TimeStep_SimContext)
;Static demand As TimeStep_DemandData
;Static itm
;With demandstore
;    For Each itm In .demandmap
;        Set demand = (.demandmap(itm))
;        scheduleDemand demand, demandstore, ctx
;        demand.reset
;    Next itm
;End With
;
;End Sub


;Public Sub fromExcel(state As TimeStep_SimState)
;
;Dim factory As TimeStep_EntityFactory
;Set factory = state.EntityFactory
;
;factory.DemandsFromTable getTable("DemandRecords"), state.demandstore
;;factory.AddDemandFromExcel strtcell.Worksheet, Me

;End Sub


;Public Sub NewDemand(name As String, tstart As Single, duration As Single, overlap As Single, _
;                            primaryunit As String, quantity As Long, priority As Single, demandstore As TimeStep_ManagerOfDemand, _
;                              policystore As TimeStep_ManagerOfSupply, ctx As TimeStep_SimContext, _
;                                Optional operation As String, Optional vignette As String, Optional sourcefirst As String)
;Dim dem As TimeStep_DemandData
;
;Set dem = createDemand(name, tstart, duration, overlap, primaryunit, quantity, priority, _
;                            demandstore.demandmap.count + 1, operation, vignette, sourcefirst)
;registerDemand dem, demandstore, policystore, ctx
;
;End Sub
;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<


