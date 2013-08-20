;;##Note for Folks Viewing From a Browser
;;If you're reading this, and you want to enable the syntax highlighting for
;;the source code, you need to make sure javascript, or for Internet Explorer,
;;ActiveX content is enabled.  The syntax highlighter is a small script that
;;runs in the browser and colorizes the text.  The PDF version does not suffer
;;this problem.

;;#Documentation Overview
;;Marathon documentation serves dual purposes: it's a build script that we can 
;;run from the REPL to generate various pieces of documentation, and it 
;;provides a topical overview of the different namespaces the support Marathon.  

;;It assumes we have marginalia installed, and the lein-marginalia plugin.
;;This makes it easy to cook off a couple of different documentation builds, 
;;from a full-on bible that describes every system in the package, to more 
;;targeted builds.  

;;The documentation tool, marginalia, produces one or more formatted html files
;;that aim for a simple form of literate programming.  The idea is that author
;;commentary appears on the left margin, while the actual source code appears 
;;on the right, so that the commentary is taken in context with the code.  

;;By default, every build of the documentation will include this file, to serve
;;as a topical roadmap, and a prelude for Marathon.  The prelude is intended 
;;to provide a brief, high-level overview of the "what" and "why" of Marathon.

;;Deeper discourse will appear in later chapters.  The general aim is to provide
;;the highest level concepts early in any documentation, and then descend 
;;to the more granular implementation details.  

;;I apologize up-front for the current elegance, or lack thereof, of much of 
;;the prose.  The current versions of the source code and the comments were 
;;very recently lifted from a large legacy code-base.  Apparently, the demons 
;;from Marathon's previous environment, the padded walls of VBA, echoed through
;;the 40K lines of code, hundreds of classes, and 30K lines of comments gathered
;;over 2 to 3 years.  One reader of an earlier, somewhat "raw" draft likened 
;;the document to "a man's journal of his own descent into madness."    

;;Such criticism is welcome, and I have since struggled to avoid offending
;;readers' sensibilities. I am still porting both the source code, and 
;;refining the prose.  My hope is to asymptotically approach the quality and 
;;clarity of Don Knuth's writings, but that particular mountain top is quite 
;;distant.  

;;As the documentation matures, I will try to take advantage of the 
;;formatting options available, to help distinguish between my interspersed 
;;commentary, and commentary attached to specific source code.  

;;Currently, the juxtaposition of commentary with code (to the right) implies a
;;direct relation to the code.  

;;The code you will see throughout is from a Lisp dialect called Clojure: 
;;http://clojure.org/        

;;Feel free to contact me at __thomas.spoon@us.army.mil__      
;;__-Tom Spoon__

(ns marathon.documentation
  (:require [clojure.java [shell :as shell]]))

;;#Documentation v0.1 - i.e. this file
(def documentation "marathon.documentation")

;;#The standard prelude for all marathon docs.
;;Basically a bumper sticker namespace with summary info.
(def prelude ["marathon.prelude"])

(defn expand-paths [root xs] (map #(str root \. %) xs))
(defn path->file [p]
  (str "src/" (clojure.string/replace p \. \/) ".clj"))

;;#Discrete Event Simulation Library.
(def simulation-lib
   ["sim.simcontext"
    "sim.pure.network"
    "sim.updates"
    "sim.agenda"
    "sim.data"])

;;#Graph Operations
;;In time, I will link to __cljgraph__, another library I built.

;;#Stochastic Demand PreProcessing Libraries#
(def stochastic-demand
  (into (expand-paths 
          "marathon.processing.helmet" 
          ["core" "collision" "split"])
          ["util.sampling"
           "util.stats"]))
  
;;#Aggregate and primitive data used by Marathon
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

;;#High Level Simulation Functions in Marathon
(def marathon-sim
  (expand-paths "marathon.sim" 
    ["core" 
     "engine"
     "fill"
     "deployment"
     "demand"
     "supply"
     "policy"
     "policyio"]))

;;#Processing Tasks
(def processing 
  (expand-paths "marathon.processing" 
    ["deployments"
     "highwater"
     "forgereader"
     "excel"]))
  
;;#The Main User-Facing Entry Point
(def user-interface 
  ["marathon.core"])

;;#Marathon Project Definition and Management 
(def marathon-project 
  ["marathon.project"
   "marathon.project.excel"
   "marathon.project.projectviews"])

;;The rest are internal functions that build topical subsets of the Marathon 
;;documentation, or push out an entire compendium.

(defn build-config
  "Specify different build configurations for various levels of documentation."
  [xs]
   (flatten (into [documentation prelude] xs)))

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
  (into ["lein.bat" "margplus"]  
        (map path->file xs)))

(defn build-docs
  "Spits a set of file paths to marginalia for documentation."
  [files]
  (apply clojure.java.shell/sh 
    (marge-command files))) 