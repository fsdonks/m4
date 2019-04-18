(require 'clojure.edn)
(def aot-order (let [f (clojure.java.io/file "order.edn")]
                 (if (.exists f)
                   (clojure.edn/read-string (slurp "order.edn"))
                   '[marathon.main])))
(def version "4.1.7")
(def capsule-name "marathon")
(def capsule-jar (str  capsule-name "-" version ".jar"))

;;project definition...
(defproject marathon "4.1.7-SNAPSHOT"
  :description "An Integrated Suite of Rotational Analysis Tools."
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [spork "0.2.1.1-SNAPSHOT"]
                 ;;schemas / specs
                 [marathon-schemas "4.1.6-SNAPSHOT"
                  :exclusions [spork]]
                 ;;post processing.
                 [proc  "0.2.7-SNAPSHOT"
                  :exclusions [spork]]
                 [stoke "0.1.0-SNAPSHOT"
                  :exclusions [spork]]
                 [helmet "0.1.0-SNAPSHOT"
                  :exclusions [spork]]
                 [demand_builder "0.1.0-SNAPSHOT"
                  :exclusions [spork]]
                 ;;external libs
                 [joinr/nightclub "0.0.1-SNAPSHOT"]
                 [eigenhombre/splasher "0.0.2"] ;;splash screen lib
                 ]
  :jvm-opts ^:replace ["-Xmx4g" #_"-Xmx1000m" "-XX:NewSize=200m"]
  :source-paths ["src" "../spork/src" "../nightclub/src" "../proc/src"
                 ]
  :profiles {:uberjar {;:aot  [marathon.main]
                       :aot [marathon.main];~aot-order
                       :main  marathon.main
                       :jvm-opts ^:replace ["-Xmx1000m" "-XX:NewSize=200m" "-server"]
                       :plugins [[lein-capsule "0.2.1"]]
                       }}
  ;:plugins [[lein-capsule "0.2.1"]]
  ;;; Capsule plugin configuration section, optional
  :capsule {:application {:name    ~capsule-name
                          :version ~version} 
            :types {:fat {:name   ~capsule-jar}}
            :execution {:runtime {:jvm-args ["-Xmx4g"]}
                        :boot    {:main-class  "marathon.main"}}}
  )
