;;Deploying entities requires operating on multiple systems.  This namespace
;;builds on the services provided by supply, demand, and policy, to coordinate 
;;changes to the simulation context necessary to physically allocate supply to 
;;demand - or to execute deployments.  
;;Primarily used by __marathon.sim.fill__ .
(ns marathon.sim.deployment
  (:require [marathon.demand [demanddata :as d]]
            [marathon.supply [unitdata :as udata]]
            [marathon.sim    [core :as core] [demand :as dem] 
                             [policy :as policy] [supply :as supply] 
                             [unit :as u]]
            [marathon.data   [protocols :as protocols]]
            [spork.sim       [simcontext :as sim]]
            [spork.util      [tags :as tag]]))

;;#Functions for Deploying Supply
(defn get-max-bog [unit policystore]
  (let [bog-remaining (udata/unit-bog-budget unit)
        p             (policy/get-policy    (-> unit :policy :name) policystore)]
    (- bog-remaining  (protocols/overlap p))))

(defn check-followon-deployer! [followon? unitname demand t ctx]
  (let [supply (core/get-supplystore ctx)
        unit   (supply/get-unit ctx unitname)]
        (if followon? 
          (supply/record-followon supply unit demand ctx)
          (u/re-deploy-unit unit t (:deployment-index unit) ctx))))

(defn check-first-deployer!   [store unitname ctx]
  (let [unit (supply/get-unit store unitname)]  
    (if (supply/first-deployment? unit store)
      (->> (core/set-supplystore ctx (supply/tag-as-deployed unit store))
           (supply/first-deployment! store unit)
           (supply/adjust-max-utilization! store unit)))))

;Critical function.

;;TODO# fix bog arg here, we may not need it.  Also drop the followon?
;;arg, at least make it non-variadic..
(defn deploy-unit
  "Deploys a unit entity, registered in supply, at time t, to demand.  The 
   expected length of stay, bog, will determine when the next update is 
   scheduled for the unit.  Propogates logging information about the context 
   of the deployment."
  ([ctx unit t demand bog filldata deploydate  followon?]
    (assert  (u/valid-deployer? unit) 
             "Unit is not a valid deployer! Must have bogbudget > 0, 
     cycletime in deployable window, or be eligible or a followon  deployment")
    (core/with-simstate [[supplystore parameters policystore demandstore fillstore] ctx]
      (let [fillcount     (count (:fills fillstore))
            bog           (get-max-bog unit policystore) 
            unitname      (:name unit)
            demandname    (:name demand)
            from-location (:locationname unit) ;may be extraneous
            from-position (:position-policy unit);
            to-location   demandname
            to-position   :deployed
            unit          (-> unit ;MOVE THIS TO A SEPARATE FUNCTION? 
                              (assoc :position-policy to-position) 
                              (assoc :dwell-time-when-deployed (udata/get-dwell unit)))
            demand        (d/assign demand unit) ;need to update this in ctx..          
            supplystore   (assoc supplystore :tags  (supply/drop-fence (:tags supplystore)
                                                                       (:name unit)))] 
        (->> (sim/merge-updates {:demandstore (dem/add-demand demandstore demand)
                                 :supplystore supplystore} ctx)
                                        ;( (fn [x] (do  (println :blah) x)) )
             (u/change-location unit (:name demand)) ;unit -> str ->ctx
                                        ;-> ctx....
             (check-followon-deployer! followon?  unitname demand t)
             ))))
  ([ctx unit t demand bog filldata deploydate]
    (deploy-unit  ctx unit t demand bog filldata deploydate (core/followon? unit))))

(comment   
           
           
           (u/deploy-unit unit t (supply/get-next-deploymentid supplystore))
           (check-first-deployer! supplystore unitname) ;THIS MAY BE OBVIATED.
           (supply/update-deployability unit false false) 
           (supply/log-deployment! t from-location demand unit fillcount filldata 
                                   deploydate (policy/find-period t policystore))
           (supply/supply-update! supplystore unit nil))
