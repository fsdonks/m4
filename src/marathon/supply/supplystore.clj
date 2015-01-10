;;A data store for unit entities and the supply system.
(ns marathon.supply.supplystore
  (:require [spork.util [tags :as tag]])
  (:use     [spork.util.record :only [defrecord+]]))

(defrecord+ supplystore 
  [[name :SupplyStore]
   srcs-in-scope ;Set of unique SRCs in scope.
   deployable-buckets ;{category entity}, indicates entities that can fill demand.
   unitmap        ;{entity-name unitdata}, map of unique unit entities.
   unit-behaviors ;map of named unit behaviors.  may move this out...
   unit-updates ;set of eventful unit-days....might be able to handle this outside.
                ;this was listed as a tag structure earlier...not certain..
   [tags tag/empty-tags];set of supply tags...should move to a global tag-store.
   has-ghosts ;boolean flag to determine if the supply can generate ghosts..might change.
   follow-ons]) ;map of entity-names to units that are in follow-on status.

(def empty-supplystore (make-supplystore))
