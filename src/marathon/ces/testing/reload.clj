;;custom ns for reloading marathon.ces.testing.
(ns marathon.ces.testing.reload
  (:require [clj-reload.core :as reload]))


(in-ns 'clj-reload.util)

;;enable record syntax to work as clojure does.  there is no hook
;;at the moment in upstream, for now we monkey patch.
(defn read-form [reader]
  (binding [;*read-eval* false
            *reader-resolver* dummy-resolver]
    (read reader-opts reader)))

(in-ns 'marathon.ces.testing.reload)


(defn reload []
  "Reload all changed files on the classpath.  Compatible with e.g. core.async."
  (when (empty? (-> @@#'clj-reload.core/*state :dirs))
    (reload/init {:dirs ["src"]}))
  (clj-reload.core/reload))
