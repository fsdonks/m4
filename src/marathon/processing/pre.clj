;pre-processing routines for marathon.
;Common tasks include sanity-checks like compiling data, splitting up 
;runs, building fill-graphs, etc.  Rather than performing run-time checks,
;we can hook into a project prior to running and make sure it's kosher.

;We'll also stick in run-setup here....things like modifying an existing
;project parametrically, applying general transforms to the data rather
;than going the manual route...
(ns marathon.processing.pre
  (:require [marathon [project :as prj]]
            [spork.util [table :as tbl]]))

(defn test-project
  "Runs each test from tests against the project.  Reports failure context
   for tests returning a result other than true."
  [tests p])
    
(defn scope-project
  "Checks the scope of data in a project.
   Returns a sequence of scoping issues if any are found, as well as a set
   of data about records that are out of scope."
  [p])

(defn filter-project 
  [filterf p & {:keys [tablenames]}]
  "Applies a filter to the project tables, where only records matching the 
   filter are retained.")

(defn partition-project 
  [partitionf p & {:keys [tablenames]}]
  "Partitions the Marathon project tables into independent data sets.
   This is useful for splitting runs up and farming out work.  The child
   runs will continue to exist under a single set of data, possibly a much
   smaller set of run-time data to work with.")

(defn merge-projects [ps]
  "Merges one or more marathon projects.  Tables from each project are 
   merged together.  Tables with the same name across projects are 
   merged.  After merging."
  )

  

