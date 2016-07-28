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
;;The primary basis for this would be to migrate the entitystore
;;to a mutable entitystore implementation (likely based on
;;concurrent hashmaps), we could event retain an
;;append-log for diffing if we want to.  Everything else
;;would flow from this.

;;Serialization/DeSerialization
;;=============================
;;We currently serialize histories as an init context
;;plus a sequence of [t patch] differentials that
;;describe the internal differences in the simulation
;;state (namely the entity store).

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

;;Another option is to setup a time component...
;;Whenever entities change, or we want to record an
;;association of the entity with a given point in time,
;;we can conj the time component onto the entity.
;;The downside is, we'd end up with a boatload of components.
;;An alternative would be to store the times in an array, or
;;an arraylist, and copy as necessary.

;;This makes sense....it kind of gives us a way to index
;;entities by txn...getting into datomic territory a little
;;bit though.  txns kind of reference changes already...

;;Tagging => (+ Fact Database  Logic Programming) ? 
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


;;Potential Bugs - Pending Verification
;;=====================================
;;Overlap bug - legacy implementation had a problem with unit
;;overlap + early disengagement causing invalid deployability.
;;stemmed from inconsistent deployability criteria (should
;;be unit's bogbudget > policy's overlap.  Believe the
;;port eliminated this inconsistency, but it needs to be
;;verified.


;;Data Validation Needs
;;=====================
;;Having done a couple of runs and pushed the scripting
;;and data around, it's pretty easy to mess up transitioning
;;between SRM and ARFORGEN policies.
;;We need to include some invariants and exceptions to
;;safeguard the data validation prior to running.
;;One example: if you're using a Rotational demand
;;category, the start/endstate shouldn't matter, where
;;if you're using SRM, they definitely matter.



