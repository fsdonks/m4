;;primary testbed for exploring benefits of
;;mutable operations and parallelism.
(ns marathon.ces.testing.scaling
  (:require  [clojure.test :as t]
             [marathon.analysis
              [random :as r]
              [experiment :as e]
              [util :as util]]
             [marathon.analysis :as a]
             [marathon.ces.core :as core]
             [ham-fisted.api :as hf]))

;;temporary monkey patch...eliminate resetting atom to eliminate
;;contention.  Optionally: make that thread-local or some other lock-free
;;design.
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
(in-ns 'marathon.ces.testing.scaling)

(def path "~/repos/notional/base-testdata-v7.xlsx")
(def phases [["comp" 1 821] ["phase-1" 822 967]])


(defn runit [n]
  (binding [r/*noisy*   0
            r/*threads* n]
    (r/run path
      :reps 1
      :phases phases
      :lower 0 :upper 1.5
      :compo-lengths r/default-compo-lengths)))

(def src "09537RB00")
(def big (-> (a/load-project path)
             e/split-project
             (get "09537RB00")))

(defn big-run [] (r/try-fill big src 110 phases))

(defn simple-fill
  ([proj src idx phases]
   (loop [n r/*retries*]
     (let [_   (when (and r/*noisy*
                          (not (zero? r/*noisy*))
                          (or (= r/*noisy* 1.0)
                              (< (rand) r/*noisy*)))
                 (util/log [:trying src :level idx]))
           res (try (let [seed (:rep-seed proj)]
                      (do (-> (r/rand-proj proj)
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

(defn mutable-fill
  ([proj src idx phases]
   (loop [n r/*retries*]
     (let [_   (when (and r/*noisy*
                          (not (zero? r/*noisy*))
                          (or (= r/*noisy* 1.0)
                              (< (rand) r/*noisy*)))
                 (util/log [:trying src :level idx]))
           res (try (let [seed (:rep-seed proj)]
                      (do (-> (r/rand-proj proj)
                              a/load-project
                              a/as-context
                              core/mutate-ctx
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

  (defn mutable-run []
    (mutable-fill big src 110 phases))


  (defn n-runs [n & {:keys [f] :or {f big-run}}]
    (reduce (fn [acc xs]  (reduce + acc (map :total-quantity xs))) 0 (hf/pmap (fn [_] (f)) (range n))))

  ;;slightly more efficient.
  (defn n-futs [n & {:keys [f] :or {f big-run}}]
    (reduce (fn [acc xs]  (reduce + acc (map :total-quantity @xs))) 0 (repeatedly n  #(future (f)))))

  ;;look at implementing transducer/reducer variants of simulation streams...right now they are all
  ;;seq based by default.  allocating.
