;;A namespace for unimplemented functionality.  Also a 
;;litmus test.
(ns marathon.sim.missing)

(defn assign-behavior [unit behaviorname] 
  (throw (Exception. "assign-behavior not implemented.")))

(defn choose-policy [policy-id component parameters policystore src]
   (throw (Exception. "choose-policy not implemented.")))