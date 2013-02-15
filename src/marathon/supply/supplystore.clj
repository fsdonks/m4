(ns marathon.supply.supplystore
  (:require [util [tags :as tag]])
  (:use [util.record :only [defrecord+]]))

(defrecord+ supplystore 
  [[name "SupplyStore"]
   srcs-in-scope ;Set of unique SRCs in scope.
   deployable-buckets ;Map of entities to supply categories that can fill demands.
   followon-buckets ;Map of entities to supply categories, eligible for follow-on.
   unitmap ;Map of entities to unitdata\
   unit-behaviors ;map of named unit behaviors.  may move this out...
   unit-updates ;set of eventful unit-days....might be able to handle this outside.
                ;this was listed as a tag structure earlier...not certain..
   [tags tag/empty-tags];a generic set of tags...need to probably move this out to an external 
        ;tag-store.
   has-ghosts ;boolean flag to determine if the supply can generate ghosts..might change.
   follow-ons]) ;map of entity-names to units that are in follow-on status.



;------>Note 
;When filling demands, if there are eligible follow-on units, we exhaust them FIRST.
;    This means redirecting the initial fill for a demand toward the 
;    followonbucket dictionary...Most of the fill logic is identical.  
;    We're just redirecting the source of fill to a different set of units.
;        During this follow-on specific pass, we exhaust the follow-on units 
;        first. Since follow-on units can only be used within their demandgroup, 
;        and in contiguous demands, they form an independent fill performed 
;        outside of the normal, AFORGEN-driven fills.
;                        
;We partition these separately to ensure that follow-ons are exhausted first, prior to fill via
;normal supply.

;-----------Pending-------------
;Need to initialize this guy with default unit behaviors....right now, the
;behaviors don't exist...

;Set UnitBehaviors = New Dictionary
;Set behavior = New TimeStep_UnitBehaviorBase
;behavior.name = "Legacy"
;
;UnitBehaviors.add "Default", behavior
;UnitBehaviors.add "Legacy", behavior