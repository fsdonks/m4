(ns marathon.startup)
 (defn flatten-requires [[req & xs]]
    (flatten 
     (for [reqs xs]
       (case (count reqs)
         1   (first reqs)
         (let [[root & decls] reqs]
           (or (seq (for [d (take-while #(not= % :refer) decls)
                     :when (coll? d)]
                      (symbol (str (name root) "." (name (first d))))))
               root))))))
  (defmacro timed-require [xs]
    (let [reqs (flatten-requires xs)]
      `(do ~@(for [r reqs]
               `(do (println (quote ~r))
                    (time (require (quote ~r) :reload)))))))
                                  
(comment
  (:require [spork.util.table]            
             [marathon.ces
              [core     :as core]
              [engine   :as engine]
              [setup    :as setup]
              ]             
             [clojure.core.reducers :as r]
             [spork.util.reducers]
             [clojure.pprint :refer [pprint]]
             [marathon [project  :as proj]]
             [marathon.project [linked :as linked]
              [excel  :as xl]]
             [spork.entitysystem
              [diff    :as diff]
              [store   :as store]]
             [spork.sim.simcontext     :as sim]
             [marathon
              [observers :as obs]
              [serial    :as ser]
              [util      :as util]]
             )

  (:require [spork.util [table :as tbl]
                        [io :as io]]
            [marathon.processing.helmet [core :as helm]]
            [marathon.processing.stoke [core :as stoke]
                                       [io :as stokeio]
                                        ;[scraper :as scraper]
             ]
            [marathon.demo :as demo]
            [clojure       [pprint :as pprint]
                           [set :as set]]
            [spork.cljgui.components [swing :as gui]]
            [spork         [mvc :as mvc]]
            [spork.events  [observe :as obs]
             [native :as swing-events]]
            [piccolotest.repl :as repl]
            [spork.util.mailbox]
            [marathon.processing.post]
            [marathon.project])
  
  )
