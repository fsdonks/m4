;A collection of shared functions, might turn into protocols someday...
;I started this due to the fact that several functions - primarily accessors, 
;were bubbling up in each of the domain-specific modules.  As a result, we'll 
;just shove them in here.  If we don't, we'll end up with circular dependencies.
;Nobody wants that...
(ns marathon.sim.core)

(defn get-demandstore [ctx]   (get-in ctx [:state :demandstore]))
(defn update-unit [u ctx]
  (assoc-in ctx [:state :supplystore :unitmap (:name u)] u))

;Generic tag-related....These apply equally to supplystore, demandstore, etc.
;The interface is identical.  Only the interpretation of the tags is different.
(defn is-enabled [demandstore demandname] 
  (tag/has-tag? (:tags demandstore) :enabled demandname))
(defn enable [demandstore demandname]
  (update-in demandstore [:tags] tag/tag-subject :enabled demandname))
(defn disable [demandstore demandname]
  (update-in demandstore [:tags] tag/untag-subject :enabled demandname))