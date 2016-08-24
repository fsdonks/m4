;;Shim class for running marathon without
;;aot compilation issues.
;;entrypoint for marathon gui.
(ns marathon.main
  (:gen-class :main true))

(defn -main [& args]
  (require 'clojure.set)
  (require 'marathon.core)
  (binding [*ns* *ns*]
    (in-ns 'marathon.core)
    ((resolve 'marathon.core/hub) :exit? true)))
