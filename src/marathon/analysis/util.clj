(ns marathon.analysis.util
  (:require [clojure.core.async :as a]))

(defn pmap!
  ([n f xs]
   (let [output-chan (a/chan)]
     (a/pipeline-blocking n
                          output-chan
                          (map f)
                          (a/to-chan xs))
     (a/<!! (a/into [] output-chan))))
  ([f xs] (pmap! (.availableProcessors (Runtime/getRuntime)) f  xs)))
