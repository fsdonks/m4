(require 'clojure.edn)
(def aot-order (let [f (clojure.java.io/file "order.edn")]
                 (if (.exists f)
                   (clojure.edn/read-string (slurp "order.edn"))
                   '[marathon.main])))
(def version "4.1.3")
(def capsule-name "marathon")
(def capsule-jar (str  capsule-name "-" version ".jar"))

;;project definition...
(defproject marathon "4.1.3-SNAPSHOT"
  :description "An Integrated Suite of Rotational Analysis Tools."
  :dependencies [[org.clojure/clojure "1.8.0"]
;                 [org.clojure.contrib/standalone "1.3.0-alpha4"]
                 [spork "0.2.0.7-SNAPSHOT"]
                 [proc  "0.2.3-SNAPSHOT"] ;;post processing.
                 ;;external libs
;                 [com.taoensso/nippy "2.11.0-RC1"] ;temporarily added to tes serialization.
                 ;;temporarily added to explore possible uses of inference...
                 [datascript "0.15.0"] 
                 [org.clojure/core.logic "0.8.10"]
                 [joinr/nightclub "0.0.1-SNAPSHOT"]
                 [alembic "0.3.2"]
                 [eigenhombre/splasher "0.0.2"] ;;splash screen lib
                 ]
  :jvm-opts ^:replace ["-Xmx4g" #_"-Xmx1000m" "-XX:NewSize=200m"]
  :source-paths ["src" "../spork/src" "../nightclub/src" "../proc/src"]
  :profiles {:uberjar {;:aot  [marathon.main]
                       :aot [marathon.main];~aot-order
                       :main  marathon.main
                       :jvm-opts ^:replace ["-Xmx1000m" "-XX:NewSize=200m" "-server"]
                       }}
  :plugins [[lein-capsule "0.2.1"]]
  ;;; Capsule plugin configuration section, optional
  :capsule {:application {:name    ~capsule-name
                          :version ~version} 
            :types {:fat {:name   ~capsule-jar}}
            :execution {:runtime {:jvm-args ["-Xmx4g"]}
                        :boot    {:main-class  "marathon.main"}}}
  )
