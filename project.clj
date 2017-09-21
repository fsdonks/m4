(defproject marathon "4.1.1-SNAPSHOT"
  :description "An Integrated Suite of Rotational Analysis Tools."
  :dependencies [[org.clojure/clojure "1.8.0"]
;                 [org.clojure.contrib/standalone "1.3.0-alpha4"]
                 [spork "0.2.0.6-SNAPSHOT"]
                 [proc  "0.2.3-SNAPSHOT"] ;;post processing.
                 ;;external libs
;                 [com.taoensso/nippy "2.11.0-RC1"] ;temporarily added to tes serialization.
                 ;;temporarily added to explore possible uses of inference...
                 [datascript "0.15.0"] 
                 [org.clojure/core.logic "0.8.10"]
                 [joinr/nightclub "0.0.1-SNAPSHOT"]
                 [alembic "0.3.2"]
                 ]
  :jvm-opts ^:replace ["-Xmx4g" #_"-Xmx1000m" "-XX:NewSize=200m"]
  :source-paths ["src" "../spork/src" "../nightclub/src" "../proc/src"]
  :profiles {:uberjar {:aot  [marathon.main]
                       :main  marathon.main
                       :jvm-opts ^:replace ["-Xmx1000m" "-XX:NewSize=200m" "-server"]
                       }
             :aot {:main  marathon.main
                   :aot [marathon.sampledata.branches
                         marathon.data.protocols
                         marathon.policy.policydata
                         marathon.data.period
                         marathon.data.store
                         marathon.data.cycle
                         marathon.ces.core
                         marathon.ces.unit
                         marathon.demand.demanddata
                         marathon.supply.unitdata
                         marathon.ces.policyops
                         marathon.ces.policy
                         marathon.ces.supply
                         marathon.ces.demand
                         marathon.ces.deployment
                         marathon.project
                         marathon.processing.pre ;?
                         marathon.schemas
                         marathon.processing.stoke.core
                         marathon.processing.stoke.io
                         marathon.processing.stoke.scraper
                         marathon.processing.stoke.app
                         marathon.processing.helmet.split
                         marathon.processing.helmet.collision
                         marathon.processing.helmet.core
                         marathon.processing.stoke.sensitivity
                         marathon.ces.fill.fillgraph
                         marathon.ces.query
                         marathon.fill.filldata
                         marathon.ces.fill
                         marathon.ces.fill.demand
                         ;marathon.sampledata.srm
                         marathon.project.excel
                         marathon.ces.policyio
                         marathon.ces.sampledata
                         marathon.ces.fill.scope
                         marathon.ces.behavior
                         marathon.ces.entityfactory
                         marathon.ces.setup
                         marathon.project.linked
                         marathon.observers
                         marathon.ces.engine
                         marathon.analysis
                         marathon.processing.highwater ;;lift this up...
                         ;marathon.visuals.core ;;nothing in it.
                         marathon.analysis.requirements
                         marathon.processing.fillrates
                         marathon.processing.post
                         marathon.visuals.patches
                         marathon.run
                         marathon.demo
                         marathon.core
                         marathon.documentation
                         marathon.ces.testing
                         ;marathon.ces.behaviortest ;;probably obe.
                         ;marathon.processing.deployvectors ;;probably obe.
                         ;marathon.gui ;?
                         ;marathon.sim.suppliers
                         ;marathon.processing.forgereader ;compile error
                         ;marathon.processing.surgereader ;compile error
                         marathon.visuals.styling
                         ;marathon.processing.dwell ;;nothing here, compile error
                         ;marathon.processing.stoke.evolutionary
                         ;marathon.processing.stoke.optimal
                         ;marathon.util
                         ;marathon.ces.debug
                         ;marathon.project.views.core
                         ;marathon.processing.pedigree
                         marathon.assessment
                         marathon.vnv
                         ;marathon.project.views.deployments ;;obe
                         ;marathon.serial ;;obe, functionality in spork.util.serial
                         ;marathon.output.outputstore ;;obe
                         ;marathon.sampledata.core ;;obe
                         ;marathon.startup
                         ;spork.marathon.sim.simlib
                         ;marathon.sim.policycreation
                         ;marathon.processing.stoke.testdata
                         ;marathon.analysis.dummydata
                         ;marathon.processing.dynamic
                         ;marathon.prelude
                         ;marathon.port.data.policycomposite
                         ;marathon.ces.categories ;;?
                         marathon.main
                         ;marathon.sim.legacy
                         ;marathon.processing.sampledata
                         ;marathon.processing.capacity
                         ;marathon.considerations
                         ;marathon.ces.obe
                         ;marathon.visuals.template
                         ]}
             :publish [:uberjar
                       {:aot [spork.util.reducers
                              spork.cljgui.components.PaintPanel
                              spork.cljgui.components.swing
                              spork.util.table
                              spork.util.metaprogramming                              
                              marathon.ces.core
                              marathon.serial 
                              marathon.core]}]}
  :plugins [
            [lein-capsule "0.2.1"]]
  ;;; Capsule plugin configuration section, optional
  :capsule {:application {;; Optional, corresponds 1:1 to Application-Name manifest entry, check https://github.com/puniverse/capsule#application-id for defaults 
                          :name "marathon" 
                          ;; Optional, corresponds 1:1 to Application-Version manifest entry, check https://github.com/puniverse/capsule#application-id for defaults 
                          :version "4.1.1"} 

            :types {
     ;; Optional, can override anything, will trigger building a thin capsule
                    :fat {
                          :name "fat-capsule.jar"
                          }}
            :execution {:runtime {:jvm-args ["-Xmx4g"]}
                        :boot    {:main-class  "marathon.main"}}}
  )
