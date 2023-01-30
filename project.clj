(defproject marathon "4.2.6-SNAPSHOT"
  :description "An Integrated Suite of Rotational Analysis Tools."
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [spork "0.2.1.6-SNAPSHOT"
                  :exclusions [org.clojure/tools.reader]]
                 ;;schemas / specs
                 [marathon-schemas "4.1.8-SNAPSHOT"
                  :exclusions [spork]]
                 ;;post processing.
                 [proc  "0.3.3-SNAPSHOT"
                  :exclusions [spork]]
                 [stoke "0.1.1-SNAPSHOT"
                  :exclusions [spork]]
                 [helmet "0.1.1-SNAPSHOT"
                  :exclusions [spork]]
                 [demand_builder "0.1.1-SNAPSHOT"
                  :exclusions [spork]]]
  :jvm-opts ^:replace ["-Xmx4g" "-XX:NewSize=200m"]
  :source-paths ["src"]
  :plugins [[reifyhealth/lein-git-down "0.4.1"]]
  :middleware [lein-git-down.plugin/inject-properties]
  :repositories [["public-github" {:url "git://github.com"}]]
  :git-down {proc  {:coordinates  fsdonks/proc}})
