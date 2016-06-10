;;This is the main source for marathon simulation testing.
;;Basically, all of the functionality, from loading a
;;project, instantiating the simulation, and
;;computing various stages of the simulation, are
;;provided here.
;;It also serves as an experimentation platform,
;;since a lot of the repl-based design-in-the-small
;;surfaces becomes formalized as regression tests
;;here.  We basically build up marathon, get it to
;;compute runs, inspect properties of the runs, and
;;from there derive invariables that must hold.
;;Some tests are defined apriori, but up to now,
;;the vast majority are defined as a consequence of
;;working with actual data.
(ns marathon.ces.testing
  (:require [marathon.ces [engine  :as engine  :refer :all]]
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
                          [query :as query]
                          [deployment :as deployment]]
            [marathon.ces.fill [demand :as filld]]
            [marathon.data [simstate :as simstate]
                           [protocols :as generic]]
            [marathon.demand [demanddata :as dem]]
            [marathon.project [linked :as linked]
             [excel :as xl]]
            [marathon.project :as proj]
            [spork.sim     [simcontext :as sim]]
            [spork.entitysystem.store :as store]
            [spork.util [reducers]
                        [tags :as tags]]
            [spork.sketch :as sketch]
            [clojure.core [reducers :as r]]
            [clojure.test :as test :refer :all]
            [marathon.analysis :as analysis]))

;;Some of our output is pretty 'uge, and trying to
;;print it all out kills emacs.
;;So we'll restrict our error messages for now...


(set! *print-level* 5)
(set! *print-length* 100)

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
;;We take a pretty granular view of events in Marathon, specifically, we schedule system-wide
;;updates at specific times.  Rather than queing many small, atomic-scale updates, we batch
;;updates by day and process them in bulk.  If a syste has no update, then nothing is done.
;;So, currently, the only event in Marathon are :time events, that serve as a clock.
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

(deftest scheduled-demands-correctly
  (is (= times
         '(0 1 91 181 271 361 451 467 481 523 541 554 563 595 618 631 666 721 
             778 811 901 963 991 1048 1051 1081 1261 1330 1351 1441 1531 1621 1711 1801 1981 2071 2095 2341 2521))
      "Scheduled times from sampledata should be consistent, in sorted order.")
  (is (= known-events expected-events)           
      "The only events scheduled should be time changes.")
  (is (same? (take 2 (sort activations481))
             ["1_A11_SRC2[481...554]" "1_R29_SRC3[481...554]"])
      "Should have actives on 481...")
  (is (re-find #"1_Vig-ANON-.*[481...554]" (nth (sort activations481) 2))
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

(def demand-sim (analysis/->simulator demand-step defaultctx))
(def the-unit    "4_SRC1_AC")

(defn disable-except [u ctx]
  (reduce (fn [c x]
            (if (not= (:name x) u)
              (store/assoce c (:name x) :disabled true)
              c))              
          ctx  (core/units ctx)))

(def disabled (disable-except the-unit defaultctx))
    
(defn supply-step
    [day ctx]
  (->> ctx 
    (engine/begin-day day)         ;Trigger beginning-of-day logic and notifications.
    (supply/manage-supply day)     ;Update unit positions and policies.
;    (manage-policies day)   ;Apply policy changes, possibly affecting supply.
;    (demand/manage-demands day)    ;Activate/DeActiveate demands, handle affected units.      
;    (fill-demands day)      ;Try to fill unfilled demands in priority order. 
;    (manage-followons day)  ;Resets unused units from follow-on status. 
    (engine/end-day day)           ;End of day logic and notifications.
;    (demand/manage-changed-demands day)));Clear set of changed demands
                                        ;in demandstore.
    ))

(def small-ctx (disable-except "2_SRC1_NG" defaultctx))

(def supply-sim (analysis/->simulator supply-step defaultctx))
(def ss (analysis/->simulator supply-step (disable-except the-unit defaultctx)))

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
(def  deployables  (filter unit/can-deploy? (core/units defaultctx)))
(defn deployable-units [ctx] (filter unit/can-deploy?   (core/units ctx)))
(def  deploynames  (map :name deployables))

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
             '("29_SRC3_NG" "23_SRC3_NG" "11_SRC2_AC" "38_SRC3_AC" "2_SRC1_NG" "24_SRC3_NG" "22_SRC3_NG"
               "37_SRC3_AC" "8_SRC2_NG" "41_SRC3_AC"))
      "Should have 12 units deployable")
  (is (same? odd-units
      '(["24_SRC3_NG" 1601]
        ["8_SRC2_NG" 1825]
        ["23_SRC3_NG" 1399]
        ["29_SRC3_NG" 1385]
        ["22_SRC3_NG" 1217])
      ))
  (is (same? even-units
      '(["38_SRC3_AC" 616]
        ["2_SRC1_NG" 1520]
        ["11_SRC2_AC" 912]
        ["41_SRC3_AC" 608]
        ["37_SRC3_AC" 470]))))

(def supplytags (store/gete defaultctx :SupplyStore :tags))
(def odd-tags
  (map (fn [[id _]] (tags/subject->tags supplytags id)) odd-units))
;;As a brief interlude; we'd like to check the tags to ensure they're
;;properly tagged.
;; (deftest tag-queries 
;;   (is (same? (odd-tags
;;              '(#{:SOURCE_SRC3 :COMPO_NG :BEHAVIOR_:default :POLICY_RCOpSus :enabled :TITLE_no-description}
;;                #{:COMPO_NG :SOURCE_SRC2 :POLICY_RC15 :BEHAVIOR_:default :enabled :TITLE_no-description}
;;                #{:SOURCE_SRC3 :COMPO_NG :BEHAVIOR_:default :POLICY_RCOpSus :enabled :TITLE_no-description}
;;                #{:SOURCE_SRC3 :COMPO_NG :BEHAVIOR_:default :POLICY_RCOpSus :enabled :TITLE_no-description}
;;                #{:SOURCE_SRC3 :COMPO_NG :BEHAVIOR_:default :POLICY_RCOpSus :enabled :TITLE_no-description}
;;                #{:COMPO_AC :SOURCE_SRC3 :POLICY_FFGACRoto :BEHAVIOR_:default :enabled :TITLE_no-description})))))

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
  (is (same? '("2_SRC1_NG" "8_SRC2_NG" "11_SRC2_AC" "22_SRC3_NG" "23_SRC3_NG"
               "24_SRC3_NG" "29_SRC3_NG" "37_SRC3_AC" "38_SRC3_AC" "41_SRC3_AC")             
             any-supply)
      "The relaxed fills actually have two more elements of supply - SRC 2 - since the 
       default preference for SRC 3 only allows substitution for SRC 1.  Thus, we 
       expand the set of compatible SRCs to include all buckets of supply, leading us 
       to include SRC2, which adds two deployable elements to our set.")
  (is (ascending? (map :priority (vals unfilled)))
      "Priorities of unfilled demand should be sorted in ascending order, i.e. low to hi")
  (is (same? suitables 
             '("24_SRC3_NG" "38_SRC3_AC" "41_SRC3_AC" "23_SRC3_NG" "29_SRC3_NG" "22_SRC3_NG" "37_SRC3_AC" "2_SRC1_NG")
             )
      "The feasible supply names that match the first demand should be consistent.  Since SRC1 is a lower
       order of supply via its substitution weight, it should end up last, even though the unit's cycle 
       time is actually pretty good.")
  (is (same? (map (comp :name :source) (fill/find-supply demandctx {:src "SRC3" :order-by query/uniform}))
             suitables)
      "fill/find-supply should be synonymous with match-supply")
  (is (== needed 2)
      "First demand should require 2 units")
  (is (same? selected  '("24_SRC3_NG" "38_SRC3_AC"))
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

;;pending tests
(def deployedctx    (deployment/deploy-units defaultctx the-deployers d))
(def deployed-units (store/get-entities deployedctx selected))

(defn simple-step [day ctx]
  (->> ctx
      (engine/begin-day       day)
      (supply/manage-supply   day)
      (demand/manage-demands  day)
      (policy/manage-policies day)
      (engine/end-day day)))

;;A basic supply/demand context, initialized for the first day
;;of simulation.
(def zero-ctx
  (->> defaultctx
       (engine/sim-step 0)
       ;;We should have several demands activated,
       ;;as well as some supply to fill with.
       (engine/begin-day 1)
       (supply/manage-supply   1)
       (demand/manage-demands  1)
       (policy/manage-policies 1)))

;(def zero-ctx (engine/sim-step 0 defaultctx))
(def filled  (filld/fill-demands 1 zero-ctx))
(def zctx (->> defaultctx
               (engine/sim-step 0)
               (sim/advance-time)))
(def zctx1
  (let [day (sim/get-time zctx)]
    (->> zctx
         (engine/begin-day        day)  ;Trigger beginning-of-day logic and notifications.
         (supply/manage-supply    day)  ;Update unit positions and policies.
         (policy/manage-policies  day)  ;Apply policy changes, possibly affecting supply.
         (demand/manage-demands   day)  ;Activate/DeActiveate demands, handle affected units.
         (filld/fill-demands      day)  ;Try to fill unfilled demands in priority order.         
         (supply/manage-followons day)  ;Resets unused units from follow-on status. 
         (engine/end-day day)           ;End of day logic and notifications.
         (demand/manage-changed-demands day))));Clear set of changed demands in demandstore.

;               (engine/sim-step 1)))

;;we should have no unfilled demands at the end of day 1.

(def ds1 (core/get-demandstore zctx1))
(def activedemands1  (map (partial demand/get-demand ds1) (keys (:activedemands ds1))))
 
(deftest day1fill
  (is (empty? (demand/unfilled-demands "SRC3" (core/get-demandstore zctx1)))
      "Should have successfully filled all the demands on day 1")
  (is (empty? (demand/unfilled-categories (core/get-demandstore zctx1)))
      "There should be no unfilled categories..."))

(defn debugging-on
  ([t f]
   (let [debug? (cond (number? t)    #(= t (core/get-time %))
                      (set?    t)    #(t   (core/get-time  %))
                      (fn?     t)    #(t (core/get-time %))
                      :else (throw (Exception. (str [:unknown-debug-param t]))))]
     (fn [t ctx] (if (debug? ctx)
                 (core/debugging
                  (binding [spork.ai.core/*debug* true]
                    (f t ctx)))
                 (f t ctx)))))
  ([t] (debugging-on t engine/sim-step)))

;;I'd like to debug on a certain day, i.e. add information only at a certain point in time.
;;Fortunately, we can do this pretty easily by defining a trace function for our pipeline,
;;turning on debugging information for a narrow range of updates...
(comment 
(def h91
  (analysis/->history 91  (debugging-on 91)
             defaultctx))

(def ctx91 (get h91 91))
(def s91 (core/get-supplystore (get h91 91)))

(def h181
  (analysis/->history 181  (debugging-on 181)
             ctx91))

;(binding [unit/*uic* "2_SRC1_NG"]
(def h271 
  (analysis/->history 271  (debugging-on (fn [_] true))
             (get h181 181)))

(def h1000
                        (analysis/->history 1000 engine/sim-step
                                   defaultctx))

)

(deftest vanilla-sim
  (let [h2521  (analysis/->history 2521 engine/sim-step 
                          defaultctx)]
    ))
    
;;__Scoping tests__
;;This set of data has no demand for SRC4, and no supply for
;;SRC1,2, or 3.  Further, there are no substitutions.  We
;;should detect this when we scope.
(def brokenctx 
  (setup/simstate-from  sd/broken-supply-tables core/debugsim))

(deftest scoping
  (is (= {"SRC4" "No Demand", "SRC3" "No Supply", "SRC1" "No Supply", "SRC2" "No Supply"}
         (:SRCs-Out-Of-Scope (core/get-parameters brokenctx)))
      "We should have nothing at all in scope for the broken supply."))

;;testing followon demands...
(def followonctx
  (->> (setup/simstate-from  sd/followon-tables core/debugsim)
       (sim/add-time 1)))

(comment 
(def followonfill
  (let [f467 
        (get (analysis/->history 467 engine/sim-step             
                        followonctx) 467)
        day (sim/get-time f467)]
    (->> f467
         (engine/begin-day        day)  ;Trigger beginning-of-day logic and notifications.
         (supply/manage-supply    day)  ;Update unit positions and policies.
         (policy/manage-policies  day)  ;Apply policy changes, possibly affecting supply.
         (demand/manage-demands   day)  ;Activate/DeActiveate demands, handle affected units.         
         (filld/fill-demands      day)  ;Try to fill unfilled demands in priority order.
         (supply/manage-followons day)  ;Resets unused units from follow-on status. 
         (engine/end-day day)           ;End of day logic and notifications.
         (demand/manage-changed-demands day))))
)

;;we need to verify that overlap is working...
;;we should be able to see overlapping relations...

;;Should get through 
(deftest followontest
  (let [ft  (analysis/->history 1100
                   ;(debugging-on #{451
                   ;               467
                   ;               523
                   ;               563
                   ;               595
                   ;               963
                   ;               1051})
                 engine/sim-step
                 followonctx)]
    ))


;; (defn ->fills [h]
;;     (let [ks    (sort (keys h))
;;         pairs (partition 2 1 ks)]
;;     (into (vec (apply concat
;;                     (->> pairs
;;                          (mapcat (fn [[l r]]
;;                                    (let [ctx (get h l)
;;                                          demands (store/gete ctx :DemandStore :activedemands)
;;                                          ]                                     
;;                                      ;;sample in between...
;;                                      (for [t (range l r)]
;;                                        (core/locations t ctx))))))))
;;           (core/locations (last ks) (get h (last ks))))))
  
;;srm-specific demand and supply.
;;We should be able to load up our srm tables and get a context.

(comment
  (def srmctx
      (->> (setup/simstate-from  sd/srm-tables core/debugsim)
           (sim/add-time 1)))

  (def binderctx
    (->> (setup/simstate-from  sd/srm-tables core/debugsim)
         (sim/add-time 1)
         (core/solo-src "Binder")))
  
  (def bindertest
    (->  (analysis/->history 1 ;(debugging-on #{451
                  ;               467
                   ;              523
                   ;              563
                   ;              595
                   ;              963
                   ;              1051})
                 engine/sim-step
                 binderctx)
         (get 1)))
  
  (def srm1
    (->  (analysis/->history 1 ;(debugging-on #{451
                  ;               467
                   ;              523
                   ;              563
                   ;              595
                   ;              963
                   ;              1051})
                 engine/sim-step
                 srmctx)
       (get 1)))

(def srm1nofill
  (->> srmctx
         (engine/begin-day        0)  ;Trigger beginning-of-day logic and notifications.
         (supply/manage-supply    0)  ;Update unit positions and policies.
         (policy/manage-policies  0)  ;Apply policy changes, possibly affecting supply.
         (demand/manage-demands   0)  ;Activate/DeActiveate demands, handle affected units.         
         (filld/fill-demands      0)  ;Try to fill unfilled demands in priority order.
         (supply/manage-followons 0)  ;Resets unused units from follow-on status. 
         (engine/end-day 0)           ;End of day logic and notifications.
         (sim/advance-time)           
         (engine/begin-day        1)  ;Trigger beginning-of-day logic and notifications.
         (supply/manage-supply    1)  ;Update unit positions and policies.
         (demand/manage-demands   1)  ;Activate/DeActiveate demands, handle affected units.         
         ))

(def srmd (store/get-entity srm1nofill "1_Al's Game_Binder[1...5001]"))

(def srm-deployed
  (deployment/deploy-units srm1nofill
     (store/get-entity srm1nofill "3_Binder_AC")  srmd))

(deftest srm-deployments
  (is (not (get (store/get-ine srm-deployed [:SupplyStore :deployable-buckets "SRM" "Binder"]) "3_Binder_AC"))
      "Should no longer be deployable"))

(core/debugging!
 (def srm-deployed
   (deployment/deploy-unit srm1nofill (store/get-entity srm1nofill "3_Binder_AC") 1  srmd)))

(def simple-srm
  (analysis/->history 100 ;(debugging-on #{451
                  ;               467
                   ;              523
                   ;              563
                   ;              595
                   ;              963
                   ;              1051})
                 simple-step
                 srmctx))
(def srm1
  (->  (analysis/->history 1 ;(debugging-on #{451
                  ;               467
                   ;              523
                   ;              563
                   ;              595
                   ;              963
                   ;              1051})
                 engine/sim-step
                 srmctx)
       (get 1)))

(def srmtest
  (analysis/->history 5001 ;(debugging-on #{451
                  ;               467
                   ;              523
                   ;              563
                   ;              595
                   ;              963
                   ;              1051})
                 engine/sim-step
                 srmctx))
;;Our problem now is getting unts registered as deployable in srm...
;;Rather than registering themselves as merely deployable,
;;they can register as srm-deployable.
;;Since they use a different behavior, we can override
;;the deployable behavior, or we can account for the fact that
;;units in general register their deployability with
;;a category in a registry...
;;if no category is defined, they go with default (as is current).
;;that way, the behavior can alter the visibility of supply for
;;certain demands..

;;Solution to this problem was to denote srm-behavior as
;;foisting upon unit entities a component that makes their
;;default-bucket "SRM"

(def srm93 (get srmtest 93))

)


(defn srm-stream [& {:keys [tmax] :or {tmax 5001}}]
  (let [srmctx
          (->> (setup/simstate-from  sd/srm-tables core/debugsim)
               (sim/add-time 1))]
    (analysis/->history-stream tmax
               engine/sim-step
               srmctx)))

;;This is a simple test function that
;;gives us a robust srm history.
(defn srm-hist [& {:keys [tmax] :or {tmax 5001}}]
  (into {} (srm-stream :tmax tmax)))

;;We'd like to take a simulation history, and glean from it
;;location data.  
(deftest srm-test
  (srm-hist)
      )

;;We should then be able to spawn all the entities.
;;Entities should schedule supply updates and move as normal.

;;We can load stuff from Excel now...

;;We need to add more backends for other types of projects...
;;later...

;;we'll need to update this later...
(def ep "C:\\Users\\tspoon\\Documents\\srm\\notionalbase.xlsx")
;;Project loading tests...
(defn excel-ctx  [p]
  (->>  (setup/simstate-from 
         (:tables (proj/load-project p))
         core/debugsim)
        (sim/add-time 1)))

(defn excel-stream [& {:keys [tmax] :or {tmax 5001}}]
  (analysis/->history-stream tmax
                      engine/sim-step
                      (excel-ctx ep)))

(defn excel-hist [& {:keys [tmax] :or {tmax 5001}}]
  (into {} (excel-stream :tmax tmax)))
