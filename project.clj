;;Go to marathon.core to update +version+ as well!
(defproject marathon "4.2.19-SNAPSHOT"
  :description "An Integrated Suite of Rotational Analysis Tools."
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [spork "0.2.1.8-SNAPSHOT"
                  :exclusions [org.clojure/tools.reader]]
                 ;;schemas / specs
                 [marathon-schemas "4.1.11-SNAPSHOT"
                  :exclusions [spork]]
                 ;;post processing.
                 [proc  "0.3.5-SNAPSHOT"
                  :exclusions [spork]]
                 [stoke "0.1.1-SNAPSHOT"
                  :exclusions [spork]]
                 [helmet "0.1.1-SNAPSHOT"
                  :exclusions [spork]]
                 [demand_builder "0.1.4-SNAPSHOT"
                  :exclusions [spork]]
                 [com.cnuernber/ham-fisted "1.003"]
                 [io.github.tonsky/clj-reload "0.4.1"]
                 [djblue/portal "0.53.0"]]
  :jvm-opts ^:replace ["-Xmx4g" "-XX:NewSize=200m"]
  :source-paths ["src"]
  :profiles {:dev {:source-paths ["src" "../spork/src/" "../proc/src/"]}
             :large {:jvm-opts ^:replace ["-Xmx700g" "-Xms100g"
                                          #_#_"-XX:NewSize=100g"
                                          "-XX:TLABSize=500m"]
                     :source-paths ["src" "../spork/src/"]}
             :aot {:aot [marathon.analysis.random]}}
  :plugins [[reifyhealth/lein-git-down "0.4.1"]]
  :middleware [lein-git-down.plugin/inject-properties]
  :repositories [["public-github" {:url "git://github.com"}]]
  :git-down {proc  {:coordinates  fsdonks/proc}
             demand_builder  {:coordinates  fsdonks/demand_builder}
             marathon-schemas {:coordinates fsdonks/marathon-schemas}})
