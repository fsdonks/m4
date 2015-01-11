(ns marathon.sim.testing
  (:require [marathon.sim.missing] 
            [marathon.sim [engine :refer :all]]
            [marathon.sim.fill.demand   :as fill]
            [marathon.sim [core :as core]
                          [supply :as supply]
                          [demand :as demand]
                          [unit :as unit]
                          [policy :as policy]
                          [policyio :as policyio]
                          [sampledata :as sd]
                          [entityfactory :as ent]
                          [setup :as setup]
                          [engine :as engine]]                        
            [marathon.data [simstate :as simstate]
                           [protocols :as generic]]
            [spork.sim     [simcontext :as sim]]
            [spork.util.reducers]
            [spork.sketch :as sketch]
            [clojure.core [reducers :as r]]
            [clojure.test :as test :refer :all]))

;;Testing for the core Engine Infrastructure
;;==========================================

;;There is no guard against negative times....we may want to enforce 
;;that.
(def ^:constant +end-time+ 1000)

;;note, even the empty sim has time 0 on the clock.  Perhaps we should
;;alter that....
(def primed-sim
  (->> (initialize-sim core/emptysim +end-time+)
       (sim/add-times [44 100 203 55])))

;;#Tests for basic simulation engine invariants.
(deftest basic-engine-testing
  (is (keep-simulating?  core/emptysim)
      "We should be able to simulate, since there is time, 
       and thus events on the clock now.")
  (is (not (can-simulate? core/emptysim))     
      "No supply or demand should indicate as false for now.")
  (is (zero?(sim/current-time core/emptysim)) 
      "empty simulations have no time")
  (is (not  (sim/has-time-remaining? (sim/advance-time core/emptysim))) 
      "nothing scheduled, should be no more work.")
)


;;#Tests for minimal simulation context invariants.
(deftest primed-engine-testing 
  (is (keep-simulating? primed-sim)
      "We should be able to simulate, since there is time, 
       and thus events on the clock now.")
  (is (= (sim/get-next-time primed-sim) 1)
      "Initialized simulation should have a start time of one....")
  (is (= (sim/get-final-time primed-sim) +end-time+ ) "upper bound should be the final time.")
  (is (sim/has-time-remaining? primed-sim) "we have time scheduled")
)


;;#Event propogation tests.
(defn push-message! [ctx edata name]
  (let [s    (:state ctx)
        msgs (get-in s [:state :messages] [])]
    (->> (assoc-in s [:state :messages] (conj msgs [name edata]))
        (assoc ctx :state))))

(def listener-ctx (assoc core/emptysim :propogator 
                     (:propogator (sim/make-debug-context 
                                   :debug-handler push-message!))))

(deftest event-propogation-testing
  (is (= (:messages (sim/get-state (sim/trigger-event :hello :dee :dumb "test!" nil listener-ctx)))
         [[:debugger #spork.sim.simcontext.packet{:t 0, :type :hello, :from :dee, :to :dumb, :msg "test!", :data nil}]])
      "Should have one message logged."))
 
;;Mocking up a sample run....
;;When we go to "run" marathon, we're really loading data from a
;;project.  The easiest way to do that is to provide marathon an API
;;for instantiating a project from tables.  Since we have canonical
;;references for project data, it's pretty easy to do this...  
(def testctx  (assoc-in core/emptysim [:state :parameters :SRCs-In-Scope] {"SRC1" true "SRC2" true "SRC3" true}))
(def debugctx (assoc-in core/debugsim [:state :parameters :SRCs-In-Scope] {"SRC1" true "SRC2" true "SRC3" true}))

(def demand-records    (sd/get-sample-records :DemandRecords))
(def ds       (ent/demands-from-records demand-records testctx))
(def first-demand (first ds))
(def tstart   (:startday first-demand))
(def tfinal   (+ tstart (:duration first-demand)))
(def res      (demand/register-demand  first-demand testctx))
(def dstore   (core/get-demandstore res))
(deftest scheduled-demand-correctly 
  (is (= ((juxt  :startday :duration) first-demand)
         [ 901 1080])
      "Sampledata should not change.  Naming should be deterministic.")
  (is (= (first (demand/get-activations dstore tstart))
         (:name first-demand))
      "Demand should register as an activation on startday.")
  (is (= (first (demand/get-deactivations dstore tfinal)) (:name first-demand)) 
        "Demand should be scheduled for deactivation")
  (is (zero? (sim/get-time res)) "Simulation time should still be at zero.")
  (is (== (sim/get-next-time res) tstart) "Next event should be demand activation")
  (is (== (sim/get-next-time (sim/advance-time res)) tfinal) "Next event should be demand activation"))
(def earliest (reduce min (map :startday ds)))
(def latest   (reduce max (map #(+ (:startday %) (:duration %)) ds)))

(def multiple-demands (demand/register-demands! ds testctx))

(def m-dstore (core/get-demandstore multiple-demands))
(def times    (map sim/get-time (take-while spork.sim.agenda/still-time? (iterate sim/advance-time multiple-demands))))
(def known-events   (core/events multiple-demands))
(def expected-events (list {:time 0, :type :time} {:time 1, :type :time} {:time 91, :type :time} {:time 181, :type :time} 
            {:time 271, :type :time} {:time 361, :type :time} {:time 451, :type :time} {:time 467, :type :time} 
            {:time 481, :type :time} {:time 523, :type :time} {:time 541, :type :time} {:time 554, :type :time} 
            {:time 563, :type :time} {:time 595, :type :time} {:time 618, :type :time} {:time 631, :type :time} 
            {:time 666, :type :time} {:time 721, :type :time} {:time 778, :type :time} {:time 811, :type :time} 
            {:time 901, :type :time} {:time 963, :type :time} {:time 991, :type :time} {:time 1048, :type :time} 
            {:time 1051, :type :time} {:time 1081, :type :time} {:time 1261, :type :time} {:time 1330, :type :time} 
            {:time 1351, :type :time} {:time 1441, :type :time} {:time 1531, :type :time} {:time 1621, :type :time} 
            {:time 1711, :type :time} {:time 1801, :type :time} {:time 1981, :type :time} {:time 2071, :type :time} 
            {:time 2095, :type :time} {:time 2341, :type :time} {:time 2521, :type :time}))

(def activations481 (demand/get-activations m-dstore 481))

;;TODO# modify test clause to work around structural equality problem.
;;I had to put this dude in, for some reason the structural equality
;;checks are not working inside the test clauses.  This does...
(defn same? [& colls]
  (loop [cs colls
         acc true]
    (if (every? seq cs)
      (let [x (ffirst cs)]
        (if (every? #(= (first %) x)  cs)
          (recur (filter identity (map rest cs))
                 acc)
          false))
      (if (some seq cs)
        false
        acc))))
;  (every? identity (map = colls)))

(deftest scheduled-demands-correctly 
  (is (= times
         '(0 1 91 181 271 361 451 467 481 523 541 554 563 595 618 631 666 721 
             778 811 901 963 991 1048 1051 1081 1261 1330 1351 1441 1531 1621 1711 1801 1981 2071 2095 2341 2521))
      "Scheduled times from sampledata should be consistent, in sorted order.")
  (is (= known-events expected-events)           
      "The only events scheduled should be time changes.")
  (is (same? (take 2 activations481)
             ["1_R29_SRC3[481...554]" "1_A11_SRC2[481...554]"])
      "Should have actives on 481...")
  (is (re-find #"1_Vig-ANON-.*[481...554]" (nth activations481 2))
      "The third active on 481 should be an anonymously named vignette with a  number in the name.")
  (is (some (fn [d] (= d (:name first-demand))) (demand/get-activations m-dstore tstart))
      "Demand should register as an activation on startday.")
  (is (zero? (sim/get-time multiple-demands)) "Simulation time should still be at zero.")
  (is (== (sim/get-next-time multiple-demands) earliest) "Next event should be demand activation")
  (is (= (last times) (:tlastdeactivation m-dstore))
      "Last event should be a deactivation time.")) 


;;we can't build supply without policy....initializing supply with
;;an understanding of policy...
(def pstore            (setup/default-policystore))

;;our canonical test data...
(def test-dstore m-dstore)
(def loadedctx (core/merge-updates {:policystore  pstore :demandstore test-dstore} testctx))

;;#unit processing#
;;build a supply store...
(def supply-records    (sd/get-sample-records :SupplyRecords))
(def sstore            (core/get-supplystore loadedctx))
(def us                (ent/units-from-records supply-records sstore pstore))
  
;;processing units, adding stuff.
;;Note, this is taking about a second for processing 30000 units.
;;Most of that bottleneck is due to not using transients and doing
;;bulk updates where possible.  
(def loadedctx        (ent/process-units us loadedctx))


;;TODO# add tests for mutable version of process-units!

(def test-fillstore   (setup/default-fillstore))
(def loadedctx        (core/set-fillstore loadedctx test-fillstore))


;;#Testing on Entire Default Context

;;An entire context loaded from the default project.
;;Includes scoping information, supply, demand, policy, etc.  This
;;will be the new hub for regression testing.
(def defaultctx       (setup/default-simstate core/debugsim))

(defn nonzero-srcs [tbl]
  (into #{} (r/map :SRC (r/filter #(and (:Enabled %)(pos? (:Quantity %))) tbl ))))

(def nonzero-supply-srcs  (nonzero-srcs (setup/get-table :SupplyRecords)))
(def nonzero-demand-srcs  (nonzero-srcs (setup/get-table :DemandRecords)))
(def expected-srcs        (clojure.set/union nonzero-supply-srcs nonzero-demand-srcs))

(deftest correct-context
  (is (= nonzero-supply-srcs
         (set (map :src (vals (core/units defaultctx)))))
      "Should have all the supply loaded.")
  (is (= nonzero-demand-srcs
         (set (map :src (vals (core/demands defaultctx)))))
      "Should have all the demand loaded."))

;;can we run a demand simulation?
(deftest demand-activations
  (is (empty? (keys  (:activedemands (core/get-demandstore (demand/activate-demands 0 defaultctx)))))
      "Should have no demands active at t 0")
  (is (same? '("2_R1_SRC3[1...91]" "2_R2_SRC3[1...2521]" "1_R3_SRC3[1...2521]" "1_R25_SRC3[1...541]")
             (keys  (:activedemands (core/get-demandstore (demand/activate-demands 1 defaultctx)))))
      "Should have four demands active at t 1"))

(defn demand-step
  "Stripped down demand simulation."
  [day ctx]
  (->> ctx 
    (engine/begin-day day)         ;Trigger beginning-of-day logic and notifications.
;    (manage-supply day)     ;Update unit positions and policies.
;    (manage-policies day)   ;Apply policy changes, possibly affecting supply.
    (demand/manage-demands day)    ;Activate/DeActiveate demands, handle affected units.      
;    (fill-demands day)      ;Try to fill unfilled demands in priority order. 
;    (manage-followons day)  ;Resets unused units from follow-on status. 
    (engine/end-day day)           ;End of day logic and notifications.
    (demand/manage-changed-demands day)));Clear set of changed demands
                                        ;in demandstore.

(defn ->simreducer [stepf init]  
  (r/take-while identity (r/iterate (fn [ctx] 
                                      (when  (engine/keep-simulating? ctx)
                                        (sim/advance-time
                                         (stepf (sim/get-time ctx) ctx))))
                                    init)))
;;A wrapper for an abstract simulation.  Can produce a sequence of
;;simulation states; reducible.
(defn ->simulator [stepf seed]
  (let [simred (->simreducer stepf seed)]
    (reify     
      clojure.lang.Seqable 
      (seq [this]  
        (take-while identity (iterate (fn [ctx] 
                                        (when  (engine/keep-simulating? ctx)
                                          (sim/advance-time
                                           (stepf (sim/get-time ctx) ctx))))
                                      seed)))
      clojure.core.protocols/CollReduce
      (coll-reduce [this f1]   (reduce f1 simred))
      (coll-reduce [_ f1 init] (reduce f1 init simred)))))

(def demand-sim (->simulator demand-step defaultctx))

(defn actives [rctx]
  (into [] (r/map (fn [ctx] [(sim/get-time ctx) (:activedemands (core/get-demandstore ctx))])  rctx)))

(def demandctx         (demand-step 1 defaultctx))
(def unfilled-ds  (keys (val (first (:unfilledq (core/get-demandstore demandctx))))))

(deftest unfilled-demands
  (is  (same? unfilled-ds
              '(["1_R25_SRC3[1...541]" 1] ["1_R3_SRC3[1...2521]" 1] ["2_R1_SRC3[1...91]" 2] ["2_R2_SRC3[1...2521]" 2]))
      "Should have the same order and set of demands unfilled on day 1."))


(def deployables  (filter unit/in-deployable-window? (vals  (core/units defaultctx))))
(defn deployable-units [ctx] (filter unit/can-deploy?(vals  (core/units defaultctx))))
(def deploynames  (map :name deployables))



;;fill queries...
(def fillrules (map marathon.sim.fill/derive-supply-rule (vals (core/demands defaultctx)) (core/get-fillstore defaultctx)))

;; ([:fillrule "SRC3"] [:fillrule "SRC3"] [:fillrule "SRC2"] [:fillrule "SRC1"] [:fillrule "SRC3"] [:fillrule "SRC3"] [:fillrule "SRC1"] [:fillrule "SRC3"] [:fillrule "SRC2"])

;;These should become basic supply queries.  We can change the
;;implementation strategy (likely to tags/components) after the
;;fact...Also, they're primitive supply for our more general fill
;;query language...
(defn find-deployable-supply  [supply src] (keys (get (supply/get-buckets supply) src)))
(def  src->fillrule (memoize (fn [src] 
                               (marathon.sim.fill.fillgraph/sink-label src))))

;;Provides an ordered vector of suitable supply buckets to look.
(defn src->srcs [srcmap src] 
  (->> (for [[rule cost] (get srcmap (src->fillrule src))]
             [(marathon.sim.fill.fillgraph/source-root rule) cost]) 
       (sort-by second)
       (mapv first)))

(def srcs->prefs (memoize (fn [srcs]   
                              (into {} (map-indexed (fn [idx src] [src idx]) srcs)))))
(defn src->prefs [src srcmap]  (srcs->prefs (src->srcs srcmap src)))

(defmacro change-if [default test & body]
  `(if ~test
     ~@body
     ~default))


;;Reducer/seq that provides an abstraction layer for implementing 
;;queries over deployable supply.  I really wish I had more time 
;;to hack out a better macro for the reducers, but this works for now.
(defn ->deployers [supply & {:keys [cat src unit] :or {cat  identity 
                                                       src  identity 
                                                       unit identity}}]
  (let [catfilter cat
        srcfilter src 
        unitfilter unit]
    (reify     
      clojure.lang.Seqable 
      (seq [this]  
        (for [[cat srcs]    (:deployable-buckets supply)
              [src units]   srcs
              [nm u]        units
              :when (and (catfilter cat) (srcfilter src) (unitfilter u))]
          [[cat src] u]))
      clojure.core.protocols/CollReduce
      (coll-reduce [this f1]        
        (reduce-kv (fn [acc cat srcs]
                     (change-if acc (catfilter cat)
                                (reduce-kv (fn [acc src units]
                                             (change-if acc (srcfilter src)
                                                        (reduce-kv (fn [acc nm unit]
                                                                     (change-if units (unitfilter unit)
                                                                                (f1 acc [[cat src] unit]))) acc units)))
                                           acc srcs))) (f1) (:deployable-buckets supply)))
      (coll-reduce [_ f1 init]      
        (reduce-kv (fn [acc cat srcs]
                     (change-if acc (catfilter cat)
                                (reduce-kv (fn [acc src units]
                                             (change-if acc (srcfilter src)
                                                        (reduce-kv (fn [acc nm unit]
                                                                     (change-if units (unitfilter unit)
                                                                                (f1 acc [[cat src] unit]))) acc units)))
                                           acc srcs))) init (:deployable-buckets supply))))))
(defn is? 
  ([x y] (or (identical? x y) (= x y)))
  ([x] (fn [y] (is? x y))))

;;#TODO supplement this with supply queries, so we can change the
;;sort-order, etc.  allow caller to provide custom sort
;;function...this is pretty huge.  From that, we can build all kinds
;;of queries.
(defn find-feasible-supply 
  ([supply srcmap category src]
     (if (is? category :any) 
       (->deployers supply :src (is? src))
       (let [prefs (src->prefs  src srcmap)]
         (->>  (->deployers supply :src #(contains? prefs %))
               (into [])
               (sort-by prefs)))))
  ([supply srcmap src] (find-feasible-supply supply srcmap :generic src))
  ([ctx src] (find-feasible-supply (core/get-supplystore ctx) (:fillmap (core/get-fillstore ctx)) src)))

(deftest unit-queries 
  (is (same? deploynames 
             '("11_SRC3_NG" "17_SRC3_NG" "25_SRC3_AC" "28_SRC3_AC" "12_SRC3_NG" 
               "22_SRC3_AC" "24_SRC3_AC" "23_SRC3_AC" "10_SRC3_NG"))
      "Should have 5 units deployable"))

