(ns marathon.test)

;basic marathon simulation....
;supply...
  ;set of units
  ;force generation function....
;demand...
  ;set of demands
  ;demand activation function
;policy...
  ;set of active force generation policies 
  ;set of active force management (fill) policies

;our job is to integrate
  ;supply (units)
    ;register and deregister units of supply
  ;demand (activities)
    ;activate/deactivate demands
  ;policy
    ;force management (fill policy: suitability of fill, priority of demand)
      ;involves substitution, suitability function...
      ;generally, fill highest priority demands with most suitable units-greedy.
    ;force generation (supply policy; available supply, readiness of supply)

;this integration is Marathon.....some dynamic process that can take initial 
;supply, demand, policy states, and compute a history.

;We maintain a notion of time to order our history, and to control state 
;transition.

  ;Some state transitions may be simultaneous (occur on the same time period).
    ;Multiple events happen on the same day.
(comment 
(require '(DEVS [Schedule :as agenda]))
(use 'marathon.time)
(require '(DEVS [sim :as sim]))
(defrecord mstate [supply demand policy events])
(def blankstate (mstate. {} {} {} agenda/emptyq))
)









