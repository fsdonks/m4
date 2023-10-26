(require 'marathon.analysis.random)

(in-ns 'spork.ai.behaviorcontext)
(defn step-entity!
  ([ctx e msg default]
   ;;TODO Check this for performance hits...
   (binding [spork.ai.core/*debug* (or spork.ai.core/*debug*  (= (:name e) *observed*))]
     (let [^behaviorenv benv (->benv ctx e msg default)
           _    (ai/debug  [:<<<<<< :STEPPING (:name e) :last-update (:last-update e)  msg :>>>>>>])
           ;_    (reset! last-benv benv)
           ]
           (-> (b/beval (.behavior benv) benv)
               (b/return!)
               (ai/commit-entity-)
               )
           )))
  ([ctx e msg] (step-entity! ctx e msg @default-behavior)))

(in-ns 'marathon.analysis.random)
(def path "~/repos/notional/base-testdata-v7.xlsx")
(def phases [["comp" 1 821] ["phase-1" 822 967]])


(defn runit [n]
  (binding [*noisy*   0
            *threads* n #_(cp/threadpool n)]
    (run path
      :reps 1
      :phases phases
      :lower 0 :upper 1.5
      :compo-lengths default-compo-lengths)))

(do
  (def src "09537RB00")
  (def big (-> (a/load-project path)
               e/split-project
               (get "09537RB00")))

  (defn big-run []
    (try-fill big src 110 phases))

  (defn simple-fill
  ([proj src idx phases]
   (loop [n *retries*]
     (let [_   (when (and *noisy*
                          (not (zero? *noisy*))
                          (or (= *noisy* 1.0)
                              (< (rand) *noisy*)))
                 (util/log [:trying src :level idx]))
           res (try (let [seed (:rep-seed proj)]
                      (do (-> (rand-proj proj)
                              a/load-project
                              a/as-stream
                              count)
                          [{:total-quantity 1}]))
                    ;;If we are distributed (like with pmap!), the error won't
                    ;;throw on the host computer,  so we catch the
                    ;;exception and print it.
                    (catch Exception e (.getMessage e))
                    )]
       ;;Should be a sequence of records, but will be a string if it
       ;;was an error
       (cond 
         (string? res) (if (pos? n)
                  (do (util/log {:retrying n :src src :idx idx})
                      (recur (dec n)))
                  (let [err {:error (str "unable to compute fill " src)
                             :src   src
                             :idx   idx
                             :reason res}
                         
                        _    (util/log err)]
                    err))
         :else res)))))

  (defn simple-run []
    (simple-fill big src 110 phases))


  (defn n-runs [n & {:keys [f] :or {f big-run}}]
    (reduce (fn [acc xs]  (reduce + acc (map :total-quantity xs))) 0 (hf/pmap (fn [_] (f)) (range n))))

  ;;slightly more efficient.
  (defn n-futs [n & {:keys [f] :or {f big-run}}]
    (reduce (fn [acc xs]  (reduce + acc (map :total-quantity @xs))) 0 (repeatedly n  #(future (f)))))

  ;;look at implementing transducer/reducer variants of simulation streams...right now they are all
  ;;seq based by default.  allocating.
  )
