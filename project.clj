(defproject marathon "4.0.9-SNAPSHOT"
  :description "An Integrated Suite of Rotational Analysis Tools."
  :dependencies [[org.clojure/clojure "1.8.0"]
;                 [org.clojure.contrib/standalone "1.3.0-alpha4"]
                 [spork "0.2.0.3-SNAPSHOT"]
                 [proc         "0.2.2-SNAPSHOT"] ;;post processing.
                 ;;external libs
                 [com.taoensso/nippy "2.11.0-RC1"] ;temporarily added to tes serialization.
                 ;;temporarily added to explore possible uses of inference...
                 [datascript "0.15.0"] 
                 [org.clojure/core.logic "0.8.10"]
                 [joinr/swingrepl "1.4.2-SNAPSHOT"]
                 ]
  :jvm-opts ^:replace ["-Xmx4g" #_"-Xmx1000m" "-XX:NewSize=200m"]
  :profiles {:uberjar {:aot  [marathon.main]
                       :main  marathon.main
                       :jvm-opts ^:replace ["-Xmx1000m" "-XX:NewSize=200m" "-server"]
                       }
             :publish [:uberjar
                       {:aot [spork.util.reducers
                              spork.cljgui.components.PaintPanel
                              spork.cljgui.components.swing
                              spork.util.table
                              spork.util.metaprogramming                              
                              marathon.ces.core
                              marathon.serial 
                              marathon.core]}]})
