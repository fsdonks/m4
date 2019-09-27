;;A namespace for defining typical experimental designs
;;e.g. supply variation, demand variation, maybe both.
(ns marathon.analysis.experiment
  (:require [marathon.analysis :as a]
            [spork.util.table :as tbl]))


;;We'll start with a hacky, very specific form of
;;supply experimentation motivated by generating
;;output for the an existing model.

;;Specifically, we'll perform a series of capacity
;;analysis runs (unaudited) and collect expected
;;fill for each run.

;;In general, we want a function to take some input,
;;and generate design points.

;;Then we want another function that will experiment
;;with the given design.

;;Finally, a function that collates the results
;;of the experiments.

;;This is the basica map/reduce paradigm.
;;We project an input onto multiple datums,
;;then map some processing onto those,
;;then reduce the processed results.

;;In our case, we'll generate multiple
;;M4 projects from an initial project.
;;We'll split the project by SRC.
;;For each SRC, we'll generate multiple projects as well,
;;based on a design function.
;;The design function will generate variable supply
;;from the root.
;;We'll run capacity analyses on each design,
;;generating an expected fill percentage.
;;We may even want to generate a fill distribution/curve vs.
;;a single point.

;;Then we'll reduce the results into a table of
;;SRC, AC, RC, NG, Fill

;;Note: we already have a basic implementation for DOE from
;;TMAS if we want to leverage it.

;;This will be somewhat dumb (intentionally).
(defn bounds->experiment [l r]
  (range l r))

;;we can use scoping information to split up the project into
;;work chunks.  If there are substitutions, we have to have strongly
;;connected components together.  Our default supply variation
;;assumptions preclude this (via independence) though.
(defn split-project [p]
  (let [ctx   (a/load-context p)
        srcs  (spork.entitysystem.store/gete ctx :parameters :SRCs-In-Scope)]))

;;could be nice to define a multi-project, that is,
;;a project that yields subprojects.
(defn root-project->src-project [tbls]
  (let [srcs (into #{} (comp (mapcat tbl/table-records)
                             (map :SRC))
                   (select-keys [:DemandRecords :SupplyRecords] tbls))]
    )
