(defproject marathon "4.0.8-SNAPSHOT"
  :description "An Integrated Suite of Rotational Analysis Tools."
  :dependencies [[org.clojure/clojure "1.7.0"]
;                 [org.clojure.contrib/standalone "1.3.0-alpha4"]
                 [spork "0.1.9.7-SNAPSHOT"]
                 [piccolotest  "0.1.0-SNAPSHOT"]
                 [com.taoensso/nippy "2.11.0-RC1"] ;temporarily added to tes serialization.
                 ]
  :jvm-opts ^:replace ["-Xmx1000m" "-XX:NewSize=200m"]
  ;:main marathon.core
  )
