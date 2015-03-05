;;A simple namepace for collecting design considerations and
;;pending tasks.
(ns marathon.considerations)

;;Drop legacy files.  There are plenty in marathon.project and
;;marathon.processing that are not doing anything for us.

;;Migrate marathon.data.period into spork.data.period, update
;;dependencies to it.  Policy namespaces currently depend on it.

;;Implement mutable versions for the core simulation structures,
;;possibly branching off entirely ala the network flow lib.  Use
;;test basis for verification.
