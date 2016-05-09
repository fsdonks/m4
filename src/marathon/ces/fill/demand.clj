;;Functions for coordinating the filling of demands.  Specifically, we deal 
;;with the querying and selection of demands.

(ns marathon.ces.fill.demand
  (:require [marathon.ces    [core :as core] 
                             [supply :as supply]
                             [demand :as dem]
                             [fill :as fill]]           
            [spork.sim       [simcontext :as sim]]))

;Since we allowed descriptions of categories to be more robust, we now abstract
;out the - potentially complex - category entirely.  This should allow us to 
;fill using 99% of the same logic.
;What we were doing using fill-followons and fill-demands is now done in 
;fill-category. In this case, we supply a more robust description of the 
;category of demand we're trying to fill. To restrict filling of demands that 
;can take advantage of existing followon supply, we add the followon-keys to the 
;category.  This ensures that find-eligible-demands will interpret the 
;[category followon-keys] to mean that only demands in a demand-group contained by 
;followon-keys will work.  Note: we should extend this to arbitrary tags, since 
;demandgroup is a hardcoded property of the demand data.  Not a big deal now, 
;and easy to extend later.

;For each independent set of prioritized demands (remember, we partition based 
;on substitution/SRC keys) we can use this for both the original fill-followons 
;and the fill-normal demands. The difference is the stop-early? parameter.  If 
;it's true, the fill will be equivalent to the original normal hierarchical 
;demand fill. If stop-early? is falsey, then we scan the entire set of demands, 
;trying to fill from a set of supply.

;1.  The result of trying to fill a demand should be a map with context
;    we can be more flexible here, maybe pass info on the success of the fill.
;2.  Incorporate fill results.
;3.  If we fail to fill a demand, we have no feasible supply, thus we leave it 
;    on the queue, and stop filling. Note, the demand is still on the queue, we
;    only "tried" to fill it. No state changed .
(defn fill-category [demandstore category ctx & {:keys [stop-early?] 
                                                 :or   {stop-early? true}}]
  ;We use our UnfilledQ to quickly find unfilled demands. 
  (loop [pending   (dem/find-eligible-demands demandstore category)   
         ctx       (dem/trying-to-fill! demandstore category ctx)]
    (if (empty? pending) ctx ;no demands to fill!      
      (let [demand      (val (first pending))                    
            demandname  (:name demand)           ;try to fill the topmost demand
            ctx         (dem/request-fill! demandstore category demand ctx)           
            [fill-status fill-ctx]  (fill/satisfy-demand demand category ctx);1)            
            can-fill?   (= fill-status :filled) 
            next-ctx    (if (= fill-status :unfilled) fill-ctx 
                          (->> fill-ctx 
                               (dem/demand-fill-changed! demandstore demand) ;2)
                               (core/merge-entity               ;UGLY 
                                 {:DemandStore 
                                  (dem/register-change demandstore demandname)})))
             ]
        (if (and stop-early? (not can-fill?)) ;stop trying if we're told to...
          next-ctx                                                           ;3)
          ;otherwise, continue filling!
          (recur (dem/pop-priority-map pending) ;advance to the next unfilled demand
                 (->> (dem/sourced-demand! demandstore demand next-ctx);notification 
                   (dem/update-fill      demandstore demandname)  ;update unfilledQ.
                   (dem/can-fill-demand! demandstore demandname))))))));notification

;NOTE...since categories are independent, we could use a parallel reducer here..
;filling all unfilled demands can be phrased in terms of fill-category...

;;#High Level Demand Fill

;;Higher-order function for filling demands.
(defn fill-demands-with [f ctx]
  (reduce (fn [acc c]
            (do ;(println [:filling (type acc) c])
                (f (core/get-demandstore acc) c acc)))
      ctx (dem/unfilled-categories (core/get-demandstore ctx))))

;;Implements the default hierarchal, stop-early fill scheme.
(defn fill-hierarchically [ctx] (fill-demands-with fill-category ctx))

;;Implements the try-to-fill-all-demands, using only follow-on-supply scheme.
(defn fill-followons [ctx]
  (if-let [groups (core/get-followon-keys ctx)] 
    (fill-demands-with
     (fn [store category ctx] 
       (fill-category store [category groups] ctx :stop-early false)) ctx)
    ctx))

;Note -> we're just passing around a big fat map, we'll use destructuring in the 
;signatures to pull the args out from it...the signature of each func is 
;state->state

;;__fill-demands__ is a high-level hook for the engine API that takes the 
;;place of what used to be a demand-manager object, with an method of the same 
;;name.  It exists here, due to the need to resolve cyclic dependencies, but 
;;the organization is subpar.  We probably need to move it elsewhere, ideally 
;;in a high-level, centralized protocol for "marathon".

;;TOM Note 20 May 2013 -> the t arg may not be necessary, since we can derive it
;;from context.  
(defn fill-demands
  "Default fill order for demands.  Performs a prioritized fill of demands, 
   hierarchically filling demands using followon supply, then using the rest of 
   the supply."
  [t ctx]
  (->> ctx
    (fill-followons)
;    (supply/release-max-utilizers) ;DECOUPLE, eliminate supply dependency...
    (fill-hierarchically)))

;;Well, we can go the dbag route on this....
;;Fill-demands could be executed much more poorly than it is.
;;Specifically, every time we have a change in supply, we try
;;to fill demand brute-force like.
;;Go through all the deployers, find 
