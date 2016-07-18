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

