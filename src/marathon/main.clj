;;Shim class for running marathon without
;;aot compilation issues.
;;entrypoint for marathon gui.
(ns marathon.main
  (:gen-class :main true))

;;This is the main entry point for marathon.
;;It's a good example of a shim-class, and
;;requires some arcane features to get things
;;working, since we're creating repls on other
;;threads.
(defn -main [& args]
  ;;clojure.set isn't imported by default, causing errors when
  ;;aot-compiling in some places.
  (require 'clojure.set)
  ;;rather than :require it in the ns-decl, we load it
  ;;at runtime.
  (require 'marathon.core)
  ;;if we don't use this, i.e. establish a root binding
  ;;for the *ns* var, we can't use in-ns later....
  ;;which leads to compile-time and run-time errors..
  (binding [*ns* *ns*]
    (in-ns 'marathon.core)
    ;;if we don't use resolve, then we get compile-time aot
    ;;dependency on marathon.core.  This allows us to shim the
    ;;class.
    ((resolve 'marathon.core/hub) :exit? true)))
      
      
