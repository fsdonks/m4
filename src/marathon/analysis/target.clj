(ns marathon.analysis.target
  (:require [marathon.analysis :as a]
            [marathon.analysis.util :as util]
            [marathon.ces [core :as c] [query :as query]]
            [spork.util [table :as tbl]
             [io :as io]
             [general :as gen]]))

;;Added data for shave charts and availability...

;;this is cribbed almost directly...
(def zero-fills
  {:TotalRequired 0
   :TotalFilled 0

   :ACFilled    0
   :RCFilled    0
   :NGFilled    0

   :Overlapping 0
   :Deployed 0
   :Unfilled 0
   })

(defn frame->fills [[t ctx]]
  (->>
   (for [[src ds] (group-by :SRC (util/demand-trends-exhaustive t ctx))]

     [src
      (reduce (fn [acc {:keys [TotalRequired TotalFilled Overlapping ACFilled RCFilled NGFilled
                               Deployed] :as r}]
                (-> acc
                    (update :TotalRequired      #(+ % TotalRequired))
                    (update :TotalFilled        #(+ % TotalFilled))
                    (update :ACFilled           #(+ % ACFilled))
                    (update :RCFilled           #(+ % RCFilled))
                    (update :NGFilled           #(+ % NGFilled))
                    (update :Overlapping        #(+ % Overlapping))
                    (update :Deployed           #(+ % Deployed))
                    (update :Unfilled           #(+ % (- TotalRequired Deployed)))
                    ))
              zero-fills
              ds)])
   (into {})))

(defn frame->target-trends [[t ctx]]
  (let [fills (frame->fills [t ctx])]
    (for [[src us] (->> ctx c/units (group-by :src))]
      (let [{:keys [deployable modernizing not-ready deployed]} (group-by util/state-key us)
            availables (group-by :component deployable)
            {:strs [AC RC NG]}  availables
            {:keys [c1 c2 c3 c4] :or {c1 0 c2 0 c3 0 c4 0 c5 0}}
            (->> us (map util/readiness) frequencies)]
        (merge
         {:t t
          :period (c/current-period ctx)
          :src  src
          :not_ready     (count not-ready)
          :deployed      (count deployed)
          :deployable    (+ (count AC) (count RC) (count NG))
          :deployable_ac (count AC)
          :deployable_rc (count RC)
          :deployable_ng (count NG)
          :C1 c1
          :C2 c2
          :C3 c3
          :C4 c4}
         (get fills src zero-fills))))))

;;these are really fields for the shave chart and other stuff...
(def target-fields
  [:t :period :src :not_ready :deployed
   :deployable :deployable_ac :deployable_rc :deployable_ng ;;availables...
   :C1 :C2 :C3 :C4
   :TotalRequired
   :TotalFilled
   :ACFilled
   :NGFilled
   :RCFilled
   :Overlapping
   :Deployed
   :Unfilled
   ])

;;ugh, this is an ugly, TEMPORARY, copy-paste job.  We should be using the generic
;;stuff in other namespaces, but I'm pressed for time...
(defn lerps [rs]
  (let [final (atom nil)]
    (concat
     (for [[xs ys] (partition 2 1 rs)]
       (do (reset! final [ys 1])
           [xs (- (:t (first ys))
                  (:t (first xs)))]))
     (lazy-seq (vector @final)))))

(defn interpolate [rs]
  (apply concat
         (for [[xs dt] (lerps rs)]
           (if (= dt 1)
             xs
             (let [t0 (:t (first xs))]
               (apply concat
                      (for [n (range dt)]
                        (map (fn [r] (assoc r :t (+ t0 n))) xs))))))))

(defn history->target-trends [h tgt]
  (-> (map frame->target-trends h)
      interpolate
      (tbl/records->file tgt :field-order target-fields)))
