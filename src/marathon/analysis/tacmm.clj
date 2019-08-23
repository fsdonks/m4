;;Namespace for TACMM related analytic functions and prototyping.
;;Documentation of business rule changes as well.
;;Possibly ephemeral.
(ns marathon.analysis.tacmm
  (:require [marathon     [analysis :as a]]
            [marathon.ces [core :as c] [query :as query]]
            [spork.entitysystem [store :as store]]
            [spork.util [io :as io] [table :as tbl]]))

;;Introducing Modernization
;;=========================

;;So we have a new requirement to add an addtionaly dimension of state
;;and sorting criterion: modernization.

;;We have multiple mod-levels (assume n, although we know of 3).
;;Units have an associated mod level, which is orthogonal to
;;SRC (although it may correlate).

;;Mod levels affect us in multiple ways:

;;Demands may prefer units of a particuar capability (SRC) AND have
;;some mod preference (or filter).

;;Supply may have an associated mod level.

;;When filling demand, we need to express sourcefirst criteria that
;;includes modernization as a consideration.

;;There's a loose notion of substitution, although here we subordinate
;;mod to capability and have a more general notion of mod distance.

;;Mod distance is defined as the absolute value of the difference
;;between the mod of the demand and the mod of the supply.

;;We generally try to minimize mod distance, and use the highest mod
;;levels after that.  So a loose sorting rule would be:
;;[min-mod-distance max-mod-level ...]

;;So, assuming we have static conditions (mod levels and preferences
;;are set in the initial conditions and don't change), then we can
;;express these rules in terms of sourcing criteria and demand types.

;;The problem we currently run into with such a scheme is that there
;;are some pre-baked assumptions regarding demand categorization,
;;source-first criteria, etc.  We had some special rules.

;;So, one goal is to generalize the process of defining filtering
;;and sorting criterion, and allowing users to express these
;;rules via input.  We're mostly there....

;;Our goal is to have something like this..

;;start with a sample context.

(comment
(def path (io/file-path "../notional/base-testdata-v8.xlsx"))
(def modpath (io/file-path "../notional/mod-testdata.xlsx"))
)

;;a dumb pre-processor to modernize the
;;demand mod levels consistently.
;;Assume 1/2 are 1 1/2 are 2.
(defn mod-supply [ctx]
  (->> (c/unit-names ctx)
       (reduce (fn [acc id]
                 (let [unit-index (store/gete acc id :unit-index)]
                   (store/assoce acc id :mod
                                 (if (odd? unit-index)
                                   2 3)))) ctx)))

;;Similarly modernized unit entity mod levels.
;;Assume 1/2 are 2, 1/2 are 3.
;;This is equivalent to having "MOD1" or "MOD2"
;;in the SourceFirst criteria.

(defn mod-demand  [ctx]
  (->> (c/demand-names ctx)
       (reduce (fn [acc id]
                 (let [n (-> (clojure.string/split id #"_")
                             first
                             clojure.edn/read-string)
                       mod-level (if (odd? n)
                                   1 2)]
                   (store/mergee acc id {:mod          mod-level
                                         :source-first (str "MOD" mod-level)})))
               ctx)))


(def mod-demand
  "Type	Enabled	Priority	Quantity	DemandIndex	StartDay	Duration	Overlap	SRC	SourceFirst	DemandGroup	Vignette	Operation	Category	Title 10_32	OITitle
DemandRecord	TRUE	1	4	1	822	273	45	01205K000	AC_First	Hinny	Pearl	Kersten	Modernization	10	OI18")

#_(def ctx (-> (a/load-context modpath)
             mod-supply
             #_mod-demand))

;;we now have a modernized supply and demand.
;;This is artificial, but it works (assuming we
;;can inject the mod information into the inputs).

;;given a demand, we assoc a mod level with it.  Assume this is a given.
#_(def d (store/get-entity ctx "2_Bob_01205K000_[1...3651]"))


;;explicit queries, exercising the functionality.
#_(def mod2-units
  (->> ctx
       (marathon.ces.query/find-supply {:where   #(= (:mod %) 2)
                                        :order-by  marathon.ces.query/MOD2
                                        :collect-by [:name :mod :cycletime]})))

;;(["28_01205K000_NG" 2 1622] ["10_01205K000_AC" 2 895])

#_(def mod3-units
  (->> ctx
       (marathon.ces.query/find-supply {:where    #(= (:mod %) 3)
                                        :order-by  marathon.ces.query/MOD3
                                        :collect-by [:name :mod :cycletime]})))

;; (["29_01205K000_NG" 3 1723]
;;  ["11_01205K000_AC" 3 995]
;;  ["27_01205K000_NG" 3 1520]
;;  ["9_01205K000_AC" 3 796])

;;This is more typical of how things actually work.
;;We project a demand onto a rule, and then use that rule
;;to inform how to find supply.
#_(def feasibles (->> ctx
                    (marathon.ces.query/find-supply (marathon.ces.fill/demand->rule d))
                    (map (comp (juxt :name :mod marathon.ces.unit/normalized-dwell) second))))

;; #_(["28_01205K000_NG" 2 0.888767]
;;    ["10_01205K000_AC" 2 0.817351]
;;    ["29_01205K000_NG" 3 0.944109]
;;    ["11_01205K000_AC" 3 0.908675]
;;    ["27_01205K000_NG" 3 0.832876]
;;    ["9_01205K000_AC" 3 0.72694])

(comment 
(def dmod (->> ctx c/demands (some (fn [{:keys [source-first] :as r}]
                                     (when (= source-first "MOD1-Target-AC")
                                       r)))))
(def modrule (marathon.ces.fill/demand->rule dmod))
(def modw     (:where modrule identity))

(def mod-ac-feasibles (->> ctx
                        (marathon.ces.query/find-supply
                           modrule)
                        (map (comp (juxt :name :mod marathon.ces.unit/normalized-dwell) second))))

)
;; (["2_01205K000_AC" 2 0.09041]
;;  ["4_01205K000_AC" 2 0.272146]
;;  ["6_01205K000_AC" 2 0.453881]
;;  ["8_01205K000_AC" 2 0.635616]
;;  ["10_01205K000_AC" 2 0.817351]
;;  ["1_01205K000_AC" 3 0.0]
;;  ["3_01205K000_AC" 3 0.181735]
;;  ["5_01205K000_AC" 3 0.36347]
;;  ["7_01205K000_AC" 3 0.545205]
;;  ["9_01205K000_AC" 3 0.72694]
;;  ["11_01205K000_AC" 3 0.908675])

#_(def mod-any-feasibles
  (->> ctx
       (marathon.ces.query/find-supply
        (assoc modrule :where nil))
       (map (comp (juxt :name :mod marathon.ces.unit/normalized-dwell) second))))

;; (["2_01205K000_AC" 2 0.09041]
;;  ["4_01205K000_AC" 2 0.272146]
;;  ["6_01205K000_AC" 2 0.453881]
;;  ["8_01205K000_AC" 2 0.635616]
;;  ["10_01205K000_AC" 2 0.817351]
;;  ["28_01205K000_NG" 2 0.888767]
;;  ["1_01205K000_AC" 3 0.0]
;;  ["3_01205K000_AC" 3 0.181735]
;;  ["5_01205K000_AC" 3 0.36347]
;;  ["7_01205K000_AC" 3 0.545205]
;;  ["9_01205K000_AC" 3 0.72694]
;;  ["11_01205K000_AC" 3 0.908675]
;;  ["27_01205K000_NG" 3 0.832876]
;;  ["29_01205K000_NG" 3 0.944109])


;;let's define some custom output for our tacmm listener.


#_(def acu (->> ctx c/units (some #(when (= (:component %) "AC") %)) :name))

;;This is a dumb way to collect state, but effective and simple relative
;;to clojure semantics.  The "better" way is to define frame emitters
;;similar to marathon.ces.analysis

(defn frame->state-samples [[t ctx]]
  (for [{:keys [name src locationname positionpolicy state mod]} (c/units ctx)]
    (let [readiness (or (first (clojure.set/intersection
                                #{:c1 :c2 :c3 :c4 :c5}
                                state))
                        (if (get state :modernizing) :c4))]
      {:t t
       :src  src
       :name name
       :locationname locationname
       :positionpolicy positionpolicy
       :state state
       :readiness readiness
       :position (if (state :modernizing) "modernizing" positionpolicy)
       :mod   mod})))

(defn history->state [h tgt]
  (let [flds [:t :src :name :locationname :positionpolicy
              :state :readiness :position :mod]]
    (-> (mapcat frame->state-samples h)
        (tbl/records->file tgt :field-order flds))))

(defn tacmm-key [u]
  (let [s (:state u)]
    (cond (s :deployable)   :deployable
          (s :modernizing)  :modernizing
          (s :dwelling)     :not-ready
          (s :bogging)      :deployed
          (s :overlapping)  :deployed
          (s :demobilizing) :not-ready
          (= s :waiting)    :deployed ;;NonBOG corner case.
          :else (throw (ex-info (str "unknown state!" s)
                                {:in (:state u) :name (:name u)})))))

(defn c-rating [s]
  (first (clojure.set/intersection
          #{:c1 :c2 :c3 :c4}
          (if (coll? s) (set s) #{s}))))

(defn readiness [u]
  (let [state     (:state u)
        statedata (:statedata u)
        c         (c-rating state)]
    (cond (or (state :modernizing) (state :waiting))
          (-> statedata :nextstate c-rating)
          c c
          :else
          (or (c-rating (:prevstate statedata))
              (let [p  (:policy u)
                    ct (-> u :currentcycle :dwell)]
                (->> (marathon.data.protocols/get-position p ct)
                     (marathon.data.protocols/state-at     p)
                     c-rating))
              (throw (ex-info "can't find c-rating"
                              {:in (select-keys u [:name :state :statedata])}))))))

;;add mod levels in here...
(def tacmm-fields
  [:days :src :not_ready :deployed :modernizing
   :deployable_tacmm3 :deployable_tacmm2 :deployable_tacmm1
   :C1 :C2 :C3 :C4
   :supply_tacmm1 :supply_tacmm2 :supply_tacmm3
   :TotalRequired  :TotalRequired_tacmm1  :TotalRequired_tacmm2  :TotalRequired_tacmm3
   :TotalFilled
   :Overlapping    :Overlapping_tacmm1  :Overlapping_tacmm2  :Overlapping_tacmm3
   :Deployed       :Deployed_tacmm1  :Deployed_tacmm2  :Deployed_tacmm3
   :Sub_1_1 :Sub_1_2 :Sub_1_3
   :Sub_2_1 :Sub_2_2 :Sub_2_3
   :Sub_3_1 :Sub_3_2 :Sub_3_3
   :Unfilled  :Unfilled_tacmm1  :Unfilled_tacmm2  :Unfilled_tacmm3 
   ])


(def mods {1 :tacmm1
           2 :tacmm2
           3 :tacmm3})

(defn mod-level [m]
  (when m
    (get mods (long m))))

(defn mod-key [u]
  (or (mod-level (:mod u))
      (do (println ((juxt :mod :name) u))
          (throw (ex-info "unknown mod level!" {:in ((juxt :mod :name) u)})))))

(defn mod-supply [us]
  (let [{:keys [tacmm1 tacmm2 tacmm3]}
        (group-by mod-key us)]
    {:tacmm1 (count tacmm1)
     :tacmm2 (count tacmm2)
     :tacmm3 (count tacmm3)}))

(def empty-subs
  {:Sub_1_1 0
   :Sub_1_2 0
   :Sub_1_3 0
   :Sub_2_2 0
   :Sub_2_1 0
   :Sub_2_3 0
   :Sub_3_3 0
   :Sub_3_1 0
   :Sub_3_2 0})

(defn subfills [mod-lev t1 t2 t3]
  (case mod-lev
    1 (merge empty-subs
             {:Sub_1_1 t1
              :Sub_1_2 t2
              :Sub_1_3 t3})
    2 (merge empty-subs
             {:Sub_2_1 t1
              :Sub_2_2 t2
              :Sub_2_3 t3})
    3 (merge empty-subs
             {:Sub_3_1 t1
              :Sub_3_2 t2
              :Sub_3_3 t3})))

;;Deployed_tacmm1	Deployed_tacmm2	Deployed_tacmm3
(defn mod-fills
  "Compute a map of {ACFilled RCFilled NGFilled OtherFilled
   ACOverlapping RCOverlapping NGOverlapping OtherOverlapping}
   from a demand."
  [ctx d units-assigned units-overlapping]
  (let [mod-lev   (or (some-> d :mod long)
                      (throw (ex-info "Demand has no mod associated with it!"
                                      {:in d})))
        mod-fills (->> units-assigned
                         (keys)
                         (map (fn [nm]
                                (or (mod-level (store/gete ctx nm :mod))
                                    (throw (ex-info "unknown mod-level"
                                              {:in nm :mod (store/gete ctx nm :mod)})))))
                         (frequencies))
        {:keys [tacmm1 tacmm2 tacmm3]
         :or   {tacmm1 0 tacmm2 0 tacmm3 0}} mod-fills
        t1filled tacmm1
        t2filled tacmm2
        t3filled tacmm3
        mod-overlaps  (->> units-overlapping
                             (keys)
                             (map (fn [nm]
                                    (or (mod-level (store/gete ctx nm :mod))
                                        (throw (ex-info "unknown mod-level"
                                                        {:in nm :mod (store/gete ctx nm :mod)})))))
                             (frequencies))
        {:keys [tacmm1 tacmm2 tacmm3]
         :or   {tacmm1 0 tacmm2 0 tacmm3 0}} mod-overlaps
        t1overlap tacmm1
        t2overlap tacmm2
        t3overlap tacmm3]
    (merge
     {:Mod               mod-lev
;;     :Filled_tacmm1    (+ t1filled t1overlap)
;;     :Filled_tacmm2    (+ t2filled t2overlap)
;;     :Filled_tacmm3    (+ t3filled t3overlap)
      :Deployed_tacmm1     t1filled
      :Deployed_tacmm2     t2filled
      :Deployed_tacmm3     t3filled
      :Overlapping_tacmm1     t1overlap
      :Overlapping_tacmm2     t2overlap
      :Overlapping_tacmm3     t3overlap}
     (subfills mod-lev t1filled t2filled t3filled))))

;;cribbed from marathon.analysis.  We're goin the dumb, brute-force
;;sampling instead of the smart, sparse approach.
(defn demand-trends
  ([t ctx]
   (let [qtr     (a/as-quarter t) ;;1-based quarters.
         changes (store/gete ctx :demand-watch :demands) ;;map of {demand-name ..}
         finals  (store/gete ctx :DemandStore  :finals)
         actives (store/gete ctx :DemandStore  :activedemands)
         finals? (finals t)]
     (when true #_(or (seq changes)
                      finals?
                      (= (sim/get-final-time ctx) t))
       (->> actives
            (keys)
            (map #(store/get-entity ctx %))
            (map  (fn [{:keys [category demandgroup operation vignette Command] :as d}]
                    (let [assigned     (:units-assigned    d)
                          overlapping  (:units-overlapping d)
                          ua           (count              assigned)
                          uo           (count              overlapping)
                          ]
                      (merge
                       {:t             t
                        :Quarter       qtr
                        :SRC           (:src      d)
                        :TotalRequired (:quantity d)
                        :TotalFilled   (+ uo ua)
                        :Overlapping   uo
                        :Deployed      ua
                        :DemandName    (:name d)
                        :Vignette      vignette
                        :DemandGroup   demandgroup
                        :deltaT       (when (and finals? (= t (a/tfinal d))) 1)}
                       (a/compo-fills ctx assigned overlapping)
                       (mod-fills ctx d assigned overlapping)
                       ))))))))
  ([ctx] (demand-trends (spork.sim.core/get-time ctx) ctx)))

(def requireds {1  :TotalRequired_tacmm1
                2  :TotalRequired_tacmm2
                3  :TotalRequired_tacmm3})

(def unfilleds {1  :Unfilled_tacmm1
                2  :Unfilled_tacmm2
                3  :Unfilled_tacmm3})

(def zero-fills
  {:TotalRequired 0 :TotalRequired_tacmm1 0 :TotalRequired_tacmm2 0 :TotalRequired_tacmm3 0
   :TotalFilled 0
   :Overlapping 0   :Overlapping_tacmm1 0 :Overlapping_tacmm2 0 :Overlapping_tacmm3 0
   :Deployed 0      :Deployed_tacmm1 0 :Deployed_tacmm2 0 :Deployed_tacmm3 0
   :Sub_1_1 0 :Sub_1_2 0, :Sub_1_3 0,
   :Sub_2_1 0, :Sub_2_2 0  :Sub_2_3 0,
   :Sub_3_1 0, :Sub_3_2 0, :Sub_3_3 0
   :Unfilled 0 :Unfilled_tacmm1 0 :Unfilled_tacmm2 0 :Unfilled_tacmm3 0
   })

(defn frame->fills [[t ctx]]
  (->>
   (for [[src ds] (group-by :SRC (demand-trends t ctx))]

     [src
      (reduce (fn [acc {:keys [Mod TotalRequired TotalFilled Overlapping
                               Deployed Deployed_tacmm1 Deployed_tacmm2
                               Deployed_tacmm3 Overlapping_tacmm1
                               Overlapping_tacmm2 Overlapping_tacmm3
                               Sub_1_1 Sub_1_2 Sub_1_3
                               Sub_2_1 Sub_2_2 Sub_2_3
                               Sub_3_1 Sub_3_2 Sub_3_3] :as r}]
                (let [m (or (-> Mod requireds)
                            (throw (ex-info "unknown mod level!"
                                            {:in r :Mod Mod :acc acc})))]
                  (-> acc
                      (update :TotalRequired     #(+ % TotalRequired))
                      (update (-> Mod requireds) #(+ % TotalRequired))
                      (update :TotalFilled        #(+ % TotalFilled))
                      (update :Overlapping        #(+ % Overlapping))
                      (update :Overlapping_tacmm1 #(+ % Overlapping_tacmm1))
                      (update :Overlapping_tacmm2 #(+ % Overlapping_tacmm2))
                      (update :Overlapping_tacmm3 #(+ % Overlapping_tacmm3))
                      (update :Deployed           #(+ % Deployed))
                      ;;These will be obfuscated if we don't pull out subs.
                      (update :Deployed_tacmm1    #(+ % Deployed_tacmm1))
                      (update :Deployed_tacmm2    #(+ % Deployed_tacmm2))
                      (update :Deployed_tacmm3    #(+ % Deployed_tacmm3))
                      (update :Sub_1_1            #(+ % Sub_1_1))
                      (update :Sub_1_2            #(+ % Sub_1_2))
                      (update :Sub_1_3            #(+ % Sub_1_3))
                      (update :Sub_2_1            #(+ % Sub_2_1))
                      (update :Sub_2_2            #(+ % Sub_2_2))
                      (update :Sub_2_3            #(+ % Sub_2_3))
                      (update :Sub_3_1            #(+ % Sub_3_1))
                      (update :Sub_3_2            #(+ % Sub_3_2))
                      (update :Sub_3_3            #(+ % Sub_3_3))
                      (update :Unfilled           #(+ % (- TotalRequired Deployed)))
                      (update (-> Mod unfilleds)  #(+ % (- TotalRequired Deployed)))
                      )))
              zero-fills
              ds)])
   (into {})))

#_(defn frame->fills [[t ctx]]
  (->>
   (for [[src ds] (group-by :SRC (demand-trends t ctx))]

     [src
      (reduce (fn [acc {:keys [TotalRequired TotalFilled Overlapping Deployed] :as r}]
                (-> acc
                    (update :TotalRequired #(+ % TotalRequired))
                    (update :TotalFilled   #(+ % TotalFilled))
                    (update :Overlapping   #(+ % Overlapping))
                    (update :Deployed      #(+ % Deployed))))
              {:TotalRequired 0 :TotalFilled 0 :Overlapping 0 :Deployed 0}
              ds)])
   (into {})))

(defn frame->tacmm-trends [[t ctx]]
  (let [fills (frame->fills [t ctx])]
    (for [[src us] (->> ctx c/units (group-by :src))]
      (let [{:keys [deployable modernizing not-ready deployed]}
            (group-by tacmm-key us)
            mods (group-by (comp long :mod) deployable)
            {:keys [c1 c2 c3 c4] :or {c1 0 c2 0 c3 0 c4 0 c5 0}}
            (->> us (map readiness) frequencies)
            {:keys [tacmm1 tacmm2 tacmm3]} (mod-supply us)]
        (merge
         {:days t
          :src  src
          :not_ready   (count not-ready)
          :deployed    (count deployed)
          :modernizing (count modernizing)
          :deployable_tacmm3 (count (get mods 3 []))
          :deployable_tacmm2 (count (get mods 2 []))
          :deployable_tacmm1 (count (get mods 1 []))
          :C1 c1
          :C2 c2
          :C3 c3
          :C4 c4
          :supply_tacmm1 tacmm1
          :supply_tacmm2 tacmm2
          :supply_tacmm3 tacmm3}
         (get fills src zero-fills))))))

(defn lerps [rs]
  (let [final (atom nil)]
    (concat
     (for [[xs ys] (partition 2 1 rs)]
       (do (reset! final [ys 1])
           [xs (- (:days (first ys))
                  (:days (first xs)))]))
     (lazy-seq (vector @final)))))

(defn interpolate [rs]
  (apply concat
    (for [[xs dt] (lerps rs)]
      (if (= dt 1)
        xs
        (let [t0 (:days (first xs))]
          (apply concat
            (for [n (range dt)]
              (map (fn [r] (assoc r :days (+ t0 n))) xs))))))))

(defn history->tacmm-trends [h tgt]
  (-> (map frame->tacmm-trends h)
      interpolate
      (tbl/records->file tgt :field-order tacmm-fields)))

;;days src tacmm c-rating quantity

(def trkey (juxt :src mod-key readiness))
(defn tacmm-readiness [t ctx]
  (for [[[src m c] us] (group-by trkey (c/units ctx))]
    {:days t
     :src  src
     :tacmm m
     :c-rating c
     :quantity (count us)}))

(defn frame->tacmm-readiness [[t ctx]]
  (tacmm-readiness t ctx))

(defn history->tacmm-trends [h tgt]
  (-> (map frame->tacmm-readiness h)
      interpolate
      (tbl/records->file tgt :field-order [:days :src :tacmm :c-rating :quantity])))

(comment
  #_(def p "~/Documents/tacmm/august/run_base.xlsx")
  (def p "~/workspacenew/notional/mod-testdata-v2.xlsx")
  (def ctx (a/load-context p))

  #_(history->state (a/as-stream ctx) "mods.txt")
  (history->tacmm-trends (a/as-stream ctx) (io/file-path "~/workspacenew/notional/tacmm.txt"))
  )
