;;A namespace for unimplemented functionality.  Also a 
;;litmus test.
(ns marathon.sim.missing)

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