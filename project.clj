;;Go to marathon.core to update +version+ as well!
(defproject marathon "4.2.14-SNAPSHOT"
  :description "An Integrated Suite of Rotational Analysis Tools."
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [spork "0.2.1.8-SNAPSHOT"
                  :exclusions [org.clojure/tools.reader]]
                 ;;schemas / specs
                 [marathon-schemas "4.1.8-SNAPSHOT"
                  :exclusions [spork]]
                 ;;post processing.
                 [proc  "0.3.4-SNAPSHOT"
                  :exclusions [spork]]
                 [stoke "0.1.1-SNAPSHOT"
                  :exclusions [spork]]
                 [helmet "0.1.1-SNAPSHOT"
                  :exclusions [spork]]
                 [demand_builder "0.1.3-SNAPSHOT"
                  :exclusions [spork]]
                 [com.cnuernber/ham-fisted "1.003"]
                 [io.github.tonsky/clj-reload "0.4.1"]
                 [djblue/portal "0.52.2"]]
  :jvm-opts ^:replace ["-Xmx4g" "-XX:NewSize=200m"]
  :source-paths ["src"]
  :profiles {:dev {:source-paths ["src" "../spork/src/"]}
             :large {:jvm-opts ^:replace ["-Xmx700g" "-Xms100g"
                                          #_#_"-XX:NewSize=100g"
                                          "-XX:TLABSize=500m"]
                     :source-paths ["src" "../spork/src/"]}} 
  :plugins [[reifyhealth/lein-git-down "0.4.1"]]
  :middleware [lein-git-down.plugin/inject-properties]
  :repositories [["public-github" {:url "git://github.com"}]]
  :git-down {proc  {:coordinates  fsdonks/proc}
             demand_builder  {:coordinates  fsdonks/demand_builder}})
