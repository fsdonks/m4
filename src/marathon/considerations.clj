;;A simple namepace for collecting design considerations and
;;pending tasks.
(ns marathon.considerations)

;;Legacy Scattered Files
;;======================
;;--Mostly Done
;;Drop legacy files.  There are plenty in marathon.project and
;;marathon.processing that are not doing anything for us.

;;Migrate marathon.data.period into spork.data.period, update
;;dependencies to it.  Policy namespaces currently depend on it.

;;Mutable Implementations
;;=======================
;;--Pending
;;Implement mutable versions for the core simulation structures,
;;possibly branching off entirely ala the network flow lib.  Use
;;test basis for verification.

;;Changes/Deltas
;;==============
;;Instantiate first-class support for change.
;;We currently have some disparities in how we
;;manage change, specifically entities that
;;have been altered since the last change computation.
;;We register changes in a couple of places, like
;;ces.demand, and implicitly via listeners (telemetry)
;;in marathon.observers

;;It'd be nice to unify this - one way or another - into
;;a consistent object that can lazily describe
;;changes, and efficiently record updates.
;;For instance, when we access the entity store, we can alter
;;single component entries, sometimes more.

;;Tagging => Fact Database + Logic Programming?
;;=============================================
;;The legacy implementation used a system of generic tags to
;;encode metadata about certain objects...
;;In this new system, we don't really need the tags....but we
;;will need a way to encode extensible metadata for the objects.
;;Perhaps a better way to accomplish the tagging is to just
;;go full-bore with a fact database + inference engine.
;;That way, we can simply assert facts (or rescind them)
;;as the state changes, and declare relationships over time.

;;Compelling use cases for this include:
;; Querying non-standard relationships about
;; entities....things like command, proximity, affinity,
;; and a host of other properties that can be codified
;; via triples and inference rules...
;; This would primarily benefit extending the scripting of
;; suitability functions and constraints...

;;We could replace the entirety of the fill logic with
;;a constraint satisfaction program using either core.logic
;;or cloco (based on the java Choco library).

;;Not sure what the performance implications are
;;as of yet....this would certainly be preferable
;;to manually "filling" via one set of rules, then
;;breaking the rules when things suit us...

;;Wow...this opens up a slew of interesting possibilities...
;;we can apply rules to the simulation itself, to determine
;;for instance, if a unit deployed to a demand, it must have
;;gone through training at a prior point...

