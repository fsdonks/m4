(defproject marathon "4.2.4-SNAPSHOT"
  :description "An Integrated Suite of Rotational Analysis Tools."
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [spork "0.2.1.6-SNAPSHOT"
                  :exclusions [org.clojure/tools.reader]]
                 ;;schemas / specs
                 [marathon-schemas "4.1.8-SNAPSHOT"
                  :exclusions [spork]]
                 ;;post processing.
                 [proc  "0.2.9-SNAPSHOT"
                  :exclusions [spork]]
                 [stoke "0.1.1-SNAPSHOT"
                  :exclusions [spork]]
                 [helmet "0.1.1-SNAPSHOT"
                  :exclusions [spork]]
                 [demand_builder "0.1.1-SNAPSHOT"
                  :exclusions [spork]]]
  :jvm-opts ^:replace ["-Xmx4g" "-XX:NewSize=200m"]
  :source-paths ["src"]
  :profiles {:dev {:source-paths [;"../spork/src" "../nightclub/src"
                                  ; "../proc/src"
                                  ;"../marathon-schemas/src"
                                  ]}
             :uberjar {;:aot  [marathon.main]
                       :aot [marathon.main];~aot-order
                       :main  marathon.main
                       :jvm-opts ^:replace ["-Xmx1000m" "-XX:NewSize=200m" "-server"]
                       :plugins [[lein-capsule "0.2.1"]]
                       }
             :uberjar-all {;:aot  [marathon.main]
                           :aot [marathon.main marathon.core];~aot-order
                           :main  marathon.main
                           :jvm-opts ^:replace ["-Xmx1000m" "-XX:NewSize=200m" "-server"]
                           :plugins [[lein-capsule "0.2.1"]]
                           }}
  )
