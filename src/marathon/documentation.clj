;;This is a build script that we can run from the REPL.
;;It assumes we have marginalia installed, and the lein-marginalia plugin.
;;This makes it easy to cook off a couple of different documentation builds, 
;;from a full-on bible that describes every system in the package, to more 
;;targeted builds.  It'd be really nice if I could sort the packages
;;topologically.
(ns marathon.documentation
  (:require [clojure.java [shell :as shell]]))

;;In the near future, I'd like to have a namespace scraper, but 
;;for now, we'll just do this manually.

;;#Documentation - i.e. this file#
(def doc "marathon.documentation")

;;#The standard prelude for all marathon docs.#
;;Basically a bumper sticker namespace with summary info.
(def prelude ["marathon.prelude"])

(defn expand-paths [root xs] (map #(str root \. %) xs))
(defn path->files [p]
  (str "/src/marathon/" (clojure.string/replace p \. \/) ".clj"))

;;#Discrete Event Simulation Library.#
(def simulation-lib
   ["sim.simcontext"
    "sim.pure.network"
    "sim.updates"
    "sim.agenda"
    "sim.data"])

;;#Graph Operations#
;;Currently using cljgraph, another library I built.

;;#Stochastic Demand PreProcessing Libraries#
(def stochastic-demand
  (into (expand-paths 
          "marathon.processing.helmet" 
          ["core"
           "collision"
           "split"])
          ["util.sampling"
           "util.stats"]))
  
;;#Aggregate and primitive data used by Marathon#
(def marathon-data 
  ["marathon.data.simstate"
   "marathon.data.protocols" ;sketchy.
   "marathon.fill.filldata"
   "marathon.fill.fillstore"
   "marathon.demand.demanddata"
   "marathon.demand.demandstore"
   "marathon.supply.unitdata"
   "marathon.supply.supplystore"
   "marathon.policy.policydata"
   "marathon.policy.policystore"
   "marathon.data.behavior" ;sketchy, possibly deprecated.
   "marathon.data.cycle"
   "marathon.data.period"   ;Note, this is duplicated in marathon.sim.policy
   "marathon.data.output"])

;;#High Level Simulation Functions in Marathon#
(def marathon-sim
  (expand-paths "marathon.sim" 
    ["core" 
     "engine"
     "fill"
     "demand"
     "supply"
     "policy"
     "policyio"]))

;;#Processing Tasks# 
(def processing 
  (expand-paths "marathon.processing" 
    ["deployments"
     "highwater"
     "forgereader"
     "excel"]))
  
;;#GUI/Processing#
(def user-interface 
  ["marathon.core"])

;;#Marathon Project Definition and Management# 
(def marathon-project 
  ["marathon.project"
   "marathon.project.excel"
   "marathon.project.projectviews"])

(defn build-config
  "Specify different build configurations for various levels of documentation."
  [xs]
   (flatten (into [doc prelude] xs)))

(def build-everything 
  (build-config [user-interface 
                 marathon-project
                 marathon-sim 
                 stochastic-demand
                 marathon-data
                 processing 
                 simulation-lib]))

(def simulation-only 
  (build-config [marathon-sim marathon-data simulation-lib]))

(defn marge-command [xs]
  (into ["lein.bat" "marg"]  
        (clojure.string/join \space (map path->files xs))))
(defn build-docs
  "Spits a set of file paths to marginalia for documentation."
  [files]
  (clojure.java.shell/sh 
    (marge-command files))) 
    
  


