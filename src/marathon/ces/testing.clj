(ns marathon.ces.testing
  (:require [marathon.ces.missing] 
            [marathon.ces [engine :refer :all]]
            [marathon.ces [fill  :as fill]]
            [marathon.ces [core :as core]
                          [supply :as supply]
                          [demand :as demand]
                          [unit :as unit]
                          [policy :as policy]
                          [policyio :as policyio]
                          [sampledata :as sd]
                          [entityfactory :as ent]
                          [setup :as setup]
                          [engine :as engine]
                          [query :as query]
                          [deployment :as deployment]]                        
            [marathon.data [simstate :as simstate]
                           [protocols :as generic]]
            [marathon.demand [demanddata :as dem]]
            [spork.sim     [simcontext :as sim]]
            [spork.entitysystem.store :as store]
            [spork.util [reducers]
                        [tags :as tags]]
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
  (let [s (spork.sim.pure.network/get-state ctx)
        sn (store/updatee
            (spork.sim.pure.network/get-state ctx) :state :messages conj  [name edata])]
    (spork.sim.pure.network/set-state ctx sn)))

(def listener-ctx
  (->   core/emptysim
        (assoc  :propogator 
                (:propogator (sim/make-debug-context 
                              :debug-handler push-message!)))
        (store/add-entity :state {:messages []})))

(deftest event-propogation-testing
  (is (=  (store/gete (sim/trigger-event :hello :dee :dumb "test!" nil listener-ctx) :state :messages)
         [[:debugger #spork.sim.simcontext.packet{:t 0, :type :hello, :from :dee, :to :dumb, :msg "test!", :data nil}]])
      "Should have one message logged."))
 
;;Mocking up a sample run....
;;When we go to "run" marathon, we're really loading data from a
;;project.  The easiest way to do that is to provide marathon an API
;;for instantiating a project from tables.  Since we have canonical
;;references for project data, it's pretty easy to do this...  
(def testctx  (store/assoc-ine core/emptysim [:parameters :SRCs-In-Scope] {"SRC1" true "SRC2" true "SRC3" true}))
(def debugctx (store/assoc-ine core/debugsim [:parameters :SRCs-In-Scope] {"SRC1" true "SRC2" true "SRC3" true}))

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

(def multiple-demands (demand/register-demands ds testctx))

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
         (set (map :src (core/units defaultctx))))
      "Should have all the supply loaded.")
  (is (= nonzero-demand-srcs
         (set (map :src  (core/demands defaultctx))))
      "Should have all the demand loaded."))

;;can we run a demand simulation?
(deftest demand-activations
  (is (empty? (keys  (:activedemands (core/get-demandstore (demand/activate-demands 0 defaultctx)))))
      "Should have no demands active at t 0")
  (is (same?  (sort ["2_R1_SRC3[1...91]" "2_R2_SRC3[1...2521]" "1_R3_SRC3[1...2521]" "1_R25_SRC3[1...541]"])
             (sort (keys  (:activedemands (core/get-demandstore (demand/activate-demands 1 defaultctx))))))
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

;;Note:
;;simpler solution is to NOT maintain deployable buckets; Rather let
;;demand sort supply as needed....

(def deployables  (filter unit/can-deploy? (core/units defaultctx)))
(defn deployable-units [ctx] (filter unit/can-deploy?   (core/units ctx)))
(def deploynames  (map :name deployables))

;;Every demand has a corresponding supply rule that indicates its preference for unit
;;types among other things.
;;fill queries...
(def fillrules (map fill/derive-supply-rule (core/demands defaultctx))); (core/get-fillstore defaultctx)))

;;We've got a couple of issues with categories atm.
;;We need to ensure that the demand categories are consistent (i.e. difference between Foundational and Foundation...)
;;probably could use better pre-processing checks when we're reading in data.

;; ([:fillrule "SRC3"] [:fillrule "SRC3"] [:fillrule "SRC2"] [:fillrule "SRC1"] [:fillrule "SRC3"] [:fillrule "SRC3"] [:fillrule "SRC1"] [:fillrule "SRC3"] [:fillrule "SRC2"])

;;These are analogous to entity queries btw...

;;These are using query/find-feasible-supply to examine all the
;;units in the deployable-buckets when they draw supply.  So,
;;if we put them in there, it's our bad.

;;using features from sim.query, we build some supply queries:
(def odd-units (->> defaultctx
                    (query/find-supply {:where #(odd? (:cycletime %)) 
                                        :order-by query/uniform 
                                        :collect-by [:name :cycletime]})))

(def even-units (->> defaultctx 
                     (query/find-supply {:where #(even? (:cycletime %)) 
                                         :order-by query/uniform 
                                         :collect-by [:name :cycletime]})))

;;These queries should not change from the sampledata.
;;Can we define more general supply orderings?...
(deftest unit-queries 
  (is (same? deploynames 
             '("29_SRC3_NG" "36_SRC3_AC" "23_SRC3_NG" "11_SRC2_AC" "2_SRC1_NG" "24_SRC3_NG" "22_SRC3_NG" "40_SRC3_AC"
               "34_SRC3_AC" "37_SRC3_AC" "8_SRC2_NG" "35_SRC3_AC"))
      "Should have 12 units deployable")
  (is (same? odd-units
      '(["24_SRC3_NG" 1601]
        ["8_SRC2_NG" 1825]
        ["23_SRC3_NG" 1399]
        ["29_SRC3_NG" 1385]
        ["22_SRC3_NG" 1217]
        ["40_SRC3_AC" 365])))
  (is (same? even-units
      '(["2_SRC1_NG" 1520]
        ["11_SRC2_AC" 912]
        ["37_SRC3_AC" 704]
        ["36_SRC3_AC" 522]
        ["35_SRC3_AC" 366]
        ["34_SRC3_AC" 230]))))

(def supplytags (store/gete defaultctx :SupplyStore :tags))
(def odd-tags
  (map (fn [[id _]] (tags/subject->tags supplytags id)) odd-units))
;;As a brief interlude; we'd like to check the tags to ensure they're
;;properly tagged.
(deftest tag-queries 
  (is (same? odd-tags
             '(#{:SOURCE_SRC3 :COMPO_NG :BEHAVIOR_:default :POLICY_RCOpSus :enabled :TITLE_no-description}
               #{:COMPO_NG :SOURCE_SRC2 :POLICY_RC15 :BEHAVIOR_:default :enabled :TITLE_no-description}
               #{:SOURCE_SRC3 :COMPO_NG :BEHAVIOR_:default :POLICY_RCOpSus :enabled :TITLE_no-description}
               #{:SOURCE_SRC3 :COMPO_NG :BEHAVIOR_:default :POLICY_RCOpSus :enabled :TITLE_no-description}
               #{:SOURCE_SRC3 :COMPO_NG :BEHAVIOR_:default :POLICY_RCOpSus :enabled :TITLE_no-description}
               #{:COMPO_AC :SOURCE_SRC3 :POLICY_FFGACRoto :BEHAVIOR_:default :enabled :TITLE_no-description}))))

;;Try filling a demand.
;;We can query a demand and try to find feasible supply for it.
;;Given a generic demand, we want to see if:
;1)It's a member of a category (there exists a category constraint)
;2)It's got an SRC preference associated with it (should).
;3)It has any tags that may be useful in finding suitable supply.

;;Maybe it would be nice to see if there are any rules associated with
;;the demand?  then we just look up the rule and query by it....

;;So rules have that general information stored...
;;{:src ... :cat ... :where .... :order-by ...} 

;;Given the following holds....
;;marathon.ces.testing> (query/match-supply (:src d) :name defaultctx)
;;("40_SRC3_AC" "37_SRC3_AC" "36_SRC3_AC" "35_SRC3_AC" "34_SRC3_AC" "29_SRC3_NG" "24_SRC3_NG" "23_SRC3_NG" "22_SRC3_NG" "2_SRC1_NG")

;;We should then be able to fill the supply.
;;We need a function that takes these names and turns them into fill
;;promises...


;;fill demand d => 
;;n <- how many units are needed for d
;;r <- d's rule for matching supply 
;;xs <- units that match r in supply
;;us <- take n xs

;;It might be better to have a non-zero notion of distance be the general
;;scheme for matching units.  Then, we can specificy unit distance in terms of
;;distance, sort, and work on it appropriately.  It's just a matter of changing
;;distance functions then...
(def unfilled  (marathon.ces.demand/unfilled-demands "SRC3" (core/get-demandstore demandctx)))
(def d         (first (vals unfilled)))
(def suitables (query/match-supply {:src (:src d) :order-by query/uniform} :name  demandctx))
(def needed    (dem/required   d))
(def selected  (take 2 suitables))



(defn ascending?  [xs] (reduce (fn [l r] (if (<= l r) r (reduced nil))) xs))
(defn descending? [xs] (reduce (fn [l r] (if (>= l r) r (reduced nil))) xs))

;;find supply according to the strictest criteria, in this case,
;;supply that has the same category as demand; note: there should be
;;none, since the referenced demand actually has a category:
;;We find that there are no SRC 2 elements available to fill
;;in this population.  Further, the fills should be
;;sorted in order of suitability, preference, etc.  So, the unit
;;with the same type of supply should edge higher on the sorting
;;function.
(def strict-fills (fill/find-supply demandctx (fill/demand->rule d)))

;;we should find plentiful supply if we make the demand rule more
;;relaxed by removing the category.  This way, we look at all
;;categories of supply.  Also, fill/find-supply takes into account the
;;source-first data associated with the demand, and uses that to
;;order the fills.  Since the referenced demand has an "AC" sourcing
;;priority, AC entities should end up toward the front of the list.
;;We're using an inclusive rather than exclusive default order, so we
;;use the compo preference to improve order, rather than completely
;;excluding things of said compo.  We'll look into making the query
;;system more sophisticated (probably at read-time when we load
;;demands).  For instance, we may want the :source-first val for a
;;demand to be a user-defined function.
(def relaxed-fills (fill/find-supply demandctx (dissoc (fill/demand->rule d) :cat :src)))
;;aux function to help testing...
(defn sort-names [xs]
  (sort-by (fn [s] (clojure.edn/read-string
                    (first (clojure.string/split s #"_")))) xs))
(def any-supply
  (->> relaxed-fills
       (map (comp :name :source))
       sort-names))

(deftest demandmatching 
  (is (same? '("2_SRC1_NG" "8_SRC2_NG" "11_SRC2_AC" "22_SRC3_NG"
               "23_SRC3_NG" "24_SRC3_NG" "29_SRC3_NG" "34_SRC3_AC" "35_SRC3_AC"
               "36_SRC3_AC" "37_SRC3_AC" "40_SRC3_AC")
             any-supply)
      "The relaxed fills actually have two more elements of supply - SRC 2 - since the 
       default preference for SRC 3 only allows substitution for SRC 1.  Thus, we 
       expand the set of compatible SRCs to include all buckets of supply, leading us 
       to include SRC2, which adds two deployable elements to our set.")
  (is (ascending? (map :priority (vals unfilled)))
      "Priorities of unfilled demand should be sorted in ascending order, i.e. low to hi")
  (is (same? suitables 
             '("24_SRC3_NG" "23_SRC3_NG" "29_SRC3_NG" "22_SRC3_NG" "37_SRC3_AC"
               "36_SRC3_AC" "35_SRC3_AC" "40_SRC3_AC" "34_SRC3_AC" "2_SRC1_NG"))
      "The feasible supply names that match the first demand should be consistent.  Since SRC1 is a lower
       order of supply via its substitution weight, it should end up last, even though the unit's cycle 
       time is actually pretty good.")
  (is (same? (map (comp :name :source) (fill/find-supply demandctx {:src "SRC3" :order-by query/uniform}))
             suitables)
      "fill/find-supply should be synonymous with match-supply")
  (is (== needed 2)
      "First demand should require 2 units")
  (is (same? selected  '("24_SRC3_NG" "23_SRC3_NG"))
      "Suitability of units is dictated by the ordering.  Best first. In this case, we expect 
       the units with the greatest cycle times [most deployable] and of like type to be most 
       suited for deployment."))

;;Can we fill a demand at this point?
;;We have a) an entity that's designated as filling a demand.
;;b) the demand to fill.
;;If we have a match, we

;;deploy the entity,
;;change location,
;;change position in policy,
;;change state/status,
;;ask for an update time.

;;register the entity with the demand,
;;update the demand fill,
;;possibly remove the demand from the unfilled.

;;If we don't care about caching anything, if we only
;;care about finding and searching for unfilled,
;;then the process simplifies to:
;;  Find the highest priority unfilled demand (sort the demand (already done))
;;  Find available supply to fill the demand.
;;  Deploy allocated supply to the demand.
;;  Update the demand's fill (either filled or not).
;;  Continue filling until the highest priority demand
;;  cannot be filled.

(def the-deployers  
  (for [nm selected]
    (store/get-entity defaultctx nm)))

(def deployedctx    (deployment/deploy-units defaultctx the-deployers d))
(def deployed-units (store/get-entities deployedctx selected))
;;we have now deployed units and updated their state to a bare minimum
;;to indicate they should be deploying.
;;The trick now is to indicate that the entities should be updating.
;;In other words, we want to send them messages, albeit without
;;causing a side-effect (for this implementation).  So, we can
;;accomplish the same effect if we leave a message in a place
;;the entity is sure to find it, and have some assurance that
;;the message will be processed according to the entity's logic
;;at an appropriate point in the pending computation.
;;Under the mutable VBA implementation, we'd normally just
;;immediately dispatch the event and trigger a state change.
;;In this case, the process is spread out.  The initial
;;deployment triggers additional data (a notification is
;;appended to the entity's messages).  We need to guarantee
;;that the entity will have an opportunity to process the
;;message (at least abstractly) before the day is out.


;;how do we turn suitable supply into a set of fills? 
;;Suitable supply is represented as a sequence of names of units...
;;Hmmm...can these imply promises? 
;;What if we don't have enough supply?
;;[For now, if we don't have enough supply, we have to define ways to find
;; more supply, name supply generators]

;;At some point, we have a name of units that actually exist.
;;Maybe we partition off our jit supply into other areas...
;;In the original marathon, we encoded the ghost as a special case.
;;Whenever we ran out of units, we'd make it.  It was always last.

;;In fmca, we have to be able to generate units, sometimes we prefer
;;to generate units over using existing units actually. 

;;Another way to do this is to create a proxy unit....
;;the proxy is something like "JIT_SRCTYPE"
;;Rather than simply a unit name, i.e. a single unit, we have a unit 
;;generator that can provide multiple units of the same
;;characteristic...

;;So if we have 
;;["A", "B", "C"], those are equivalent to 
;;[["A" 1], ["B" 1], ["C" 1]], i.e. a single entity that exists in the
;;supply.

;;if we have ["A", ["JIT_B", 20], "C"], then we have the ability to
;;generate/fill up to 21 units before we use C.

;;Just like we did before, the ghost was a proxy unit.
;;This time, we have proxies in th supply for our jittable stuff.

;;That allows us to keep them present in the supply.

;;The implication is that there are 2 different paths for filling
;;using a regular unit and a JIT unit.

;;No matter what, we go down the line, asking the supplier to 
;;fill the quantity asked for.

;;If we follow that abstraction, then single units will 
;;merely return a promised fill and the amount.
;;So singletons work...
;;JIT units, or units representing populations, also work 
;;to fill supply; they return a promised fill and the amount 
;;remaining.

;;We just keep going until all the fills are done, or we 
;;have no supply left.

;;This lets us arbitrarily order where the JITers come in 
;;based on the attributes of the unit, i.e. tags, src, etc.

;;So, we think of each unit as an individual supplier.

;;On a really grand scale...
;;We can think of each unit as a supplystore.
;;In some cases, we may want that....so we can break apart units.
;;The supplystore is responsible for tracking units....

;;A generator is a facade around one or more class of units.
;;The supplystore is effectively a generator that 
;;produces units based on the existing supply.

;;Note: going this route, we can also treat demands as generators..
;;specifically, they "can" show up as suppliers.  This is good....


;;Right now....let's just get names of units, and then figure out
;;how to fill using the unit....
(comment
  ;; (deployment/deploy-unit t/demandctx (:source t/fzero) (sim/get-time t/demandctx) t/d 10 t/fzero (core/interval->date (sim/get-time t/demandctx) (core/followon? (:source t/fzero))))
  

(def fzero  (first relaxed-fills))
(def u0     (:source fzero))
(def depres
  (deployment/deploy-unit
   demandctx  u0 (sim/get-time demandctx)
   d 10 fzero (core/interval->date (sim/get-time demandctx)
   (core/followon? u0))))
)
