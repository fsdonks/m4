;;A namespace for unimplemented functionality.  Also a 
;;litmus test for identifying pending work.
(ns marathon.ces.missing)

;;#Missing Functions#
(defn assign-behavior [unit behaviorname] 
  (throw (Exception. "assign-behavior not implemented.")))

(defn choose-policy   [policy-id component parameters policystore src]
   (throw (Exception. "choose-policy not implemented.")))

;;__TODO__ Implement subscribe-unit-to-policy.  Used to be associated with the policy object, 
;;now we have the policy store maintaining a table of unit id's to policies.
(defn subscribe-unit-to-policy [u p policystore ctx]
    (throw (Exception. "subscribe-unit-to-policy not implemented")))

;;__TODO__ Implement get-subscribers.  Used to be associated with the policy object, 
;;now we have the policy store maintaining a table of unit id's to policies.
(defn get-subscribers [policy-name policystore]
  (throw (Exception. "get-subscribers not implemented")))


;;From marathon.sim.engine
;;These functions are currently missing, possibly due to deprecation.
;--------Missing Functionality>>>>>>>>>>>>>>>>>>
(comment  ;stub is somewhat useless at the moment.
	(stub "guess-last-day"
	   (defn guess-last-day [state & [last-day]]))
	;If .interactive Then MarathonEngine.notifyUI DumbUI, .context 
	;let folks know if we have a UI
	(stub "notify-watches, check MarathonEngine"
	   (defn notify-watches [state]))
	(stub "initialize-ouput not implemented, reference MarathonSteup" 
	   (defn initialize-output [state path]))
	(stub "can-simulate? not implemented, check MarathonEngine"
	   (defn can-simulate? [state]))
	(stub "keep-simulating? not implemented, check MarathonEngine"
	   (defn keep-simulating? [state]))
	(stub "log-status not yet implemented"  
	    (defn log-status [msg]))
)
;<<<<<<<<Missing Functionality----------------

;marathon.sim.demand
;;Undefined as of yet...
;(defn new-demand [name tstart duration overlap primary-unit quantity 
;      priority demandstore policystore ctx operation vignette source-first]
;
