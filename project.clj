(defproject marathon "4.0.8-SNAPSHOT"
  :description "An Integrated Suite of Rotational Analysis Tools."
  :dependencies [[org.clojure/clojure "1.7.0"]
;                 [org.clojure.contrib/standalone "1.3.0-alpha4"]
                 [spork "0.1.9.6-SNAPSHOT"]]
  :jvm-opts ^:replace ["-Xmx1000m" "-XX:NewSize=200m"]
  ;:main marathon.core
  )
