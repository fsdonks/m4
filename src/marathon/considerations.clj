;;A simple namepace for collecting design considerations and
;;pending tasks.
(ns marathon.considerations)

;;Counter Intuitive / Sensitive Behaviors
;;=======================================
;;Order of supply now matters (technically).

;;Before the MARV effort, we didn't have a platform neutral
;;total ordering of supply, due the implementation-specific
;;hash functions and storage used in different implementations
;;of M3 and M4...

;;This led to inconsistent behavior between the platforms
;;in regards to selecting units for fill, where - if
;;two units were effectively equal - the unit that just
;;happend to be "ordered" (read stored) first would get
;;picked.  This meant that the implementation of the
;;associative structure containing the entities had
;;an undefined, seemingly arbitrary control over the
;;fill order for such cases.

;;To rectify this - and for the sake of comparative
;;verification - we added a "unit-index" key to
;;each entity, that derived from the order in which
;;the entity was created from source data (supply records).
;;This serves as the final, lowest form of sorting
;;criteria for any fill comparison.  Thus, units with
;;lower indices (appearing "earlier" in the supply records)
;;will have an arbitrary advantage in filling demands
;;iff all other criteria are equal (rare, but possible).

;;This cured one source of inconsistency and helped
;;aid in comparative verification of the platforms...

;;The side-effect - which just reared its head during
;;requirements analysis in M4 - is that the order of
;;supply can cause counterintuitive behavior when
;;computing requirements from a capacity run.  In other
;;words, supply order is now an independent variable
;;that can contribute to changes in history!

;;I noticed the phenomena when tracking down a bug
;;related to a new policy, and comparing output with
;;legacy m3 and m4.  During testing the M4, I
;;alternately had supply records (from RA) where
;;the components were in order of NG, RC, AC, vs.
;;the typical (AC NG RC).
;;In this case, only AC was being grown, against an
;;initial supply of NG and RC.

;;During testing, RA converged on 611 for the step
;;size.  I checked capacity analyses with identical
;;supply in each and found the output to be consistent
;;using marathon.vnv.  After some wrangling, though,
;;I must have mixed up the order, and ended up
;;with an insufficient supply in M3, but a sufficient
;;supply in M4.  I even generated the explicit supply
;;records according to the cycle-times that M4
;;was using to ensure consistent supply.

;;Finally, I tracked down the case to RA being "wrong,"
;;even though M4 says it converged with 0 misses....
;;Testing in the REPL showed that, indeed, with the
;;- otherwise identical - supply of [9 NG 63 RC 611 AC]
;;pulled in from the requirements.txt output, if
;;fed to a stand alone capacity analysis, we'd get
;;1 missed demand, despite having converged!?!

;;Testing showed that, running with [611 AC 9 NG 63 RC]
;;indeed created zero misses; thus - at some point -
;;the deployments were biased "enough" solely by the
;;lower unit-index values of the NG/RC entities, that
;;we ended up with at least one instance where a
;;non-AC unit was used, thus deviating history from
;;the RA run, and leaving us with a miss!  In actuality,
;;the miss was incredibly small, BUT, it caused an
;;infeasibility in practice.

;;Solution: I'm looking at different options here....
;;Seems the simplest is to:
;;a) sort the output of
;;   requirements.txt to ensure that it's consistent
;;   with the supply records built for the run,
;;   (RA fills in missing records by proportion order,
;;   which goes AC, NG, RC in the GhostProportionsAggregate
;;   input).
;;b) Possibly sort all supplyrecords by compo before doing
;;   any run.  This is how data is "typically" formatted....
;;   and sorting would eliminate the unit-index as a confounding
;;   source between runs.
;;c) Accept that supply order matters and inform the user?
;;   Are there cases where it's semantically useful to have
;;   earlier supply be preferred in case of a tie?
;;   That's the current behavior.  It was originally for
;;   consistency, but may have analytic uses....
;;d) Use a consistent random-seed based off hashing the
;;   supply records to provide a PRNG to determine ties?
;;   This would preserve determinism, but could be hard
;;   to get right for multiple platforms....

;;We use option d) to implement a consistent total ordering,
;;using a the PRNG in clojure (java) using the same
;;initial random seed.  Order of supply still matters
;;(technically), but the effects are likely imperceptible
;;with this randomization.

;;Legacy Scattered Files
;;======================
;;--Mostly Done
;;Drop legacy files.  There are plenty in marathon.project and
;;marathon.processing that are not doing anything for us.

;;Migrate marathon.data.period into spork.data.period, update
;;dependencies to it.  Policy namespaces currently depend on it.

;;Mutable Implementations
;;=======================
;;--Pending
;;Implement mutable versions for the core simulation structures,
;;possibly branching off entirely ala the network flow lib.  Use
;;test basis for verification.
;;The primary basis for this would be to migrate the entitystore
;;to a mutable entitystore implementation (likely based on
;;concurrent hashmaps), we could event retain an
;;append-log for diffing if we want to.  Everything else
;;would flow from this.

;;Serialization/DeSerialization
;;=============================
;;We currently serialize histories as an init context
;;plus a sequence of [t patch] differentials that
;;describe the internal differences in the simulation
;;state (namely the entity store).

;;Changes/Deltas
;;==============
;;Instantiate first-class support for change.
;;We currently have some disparities in how we
;;manage change, specifically entities that
;;have been altered since the last change computation.
;;We register changes in a couple of places, like
;;ces.demand, and implicitly via listeners (telemetry)
;;in marathon.observers

;;It'd be nice to unify this - one way or another - into
;;a consistent object that can lazily describe
;;changes, and efficiently record updates.
;;For instance, when we access the entity store, we can alter
;;single component entries, sometimes more.

;;Another option is to setup a time component...
;;Whenever entities change, or we want to record an
;;association of the entity with a given point in time,
;;we can conj the time component onto the entity.
;;The downside is, we'd end up with a boatload of components.
;;An alternative would be to store the times in an array, or
;;an arraylist, and copy as necessary.

;;This makes sense....it kind of gives us a way to index
;;entities by txn...getting into datomic territory a little
;;bit though.  txns kind of reference changes already...

;;Tagging => (+ Fact Database  Logic Programming) ? 
;;=============================================
;;The legacy implementation used a system of generic tags to
;;encode metadata about certain objects...
;;In this new system, we don't really need the tags....but we
;;will need a way to encode extensible metadata for the objects.
;;Perhaps a better way to accomplish the tagging is to just
;;go full-bore with a fact database + inference engine.
;;That way, we can simply assert facts (or rescind them)
;;as the state changes, and declare relationships over time.

;;Compelling use cases for this include:
;; Querying non-standard relationships about
;; entities....things like command, proximity, affinity,
;; and a host of other properties that can be codified
;; via triples and inference rules...
;; This would primarily benefit extending the scripting of
;; suitability functions and constraints...

;;We could replace the entirety of the fill logic with
;;a constraint satisfaction program using either core.logic
;;or cloco (based on the java Choco library).

;;Not sure what the performance implications are
;;as of yet....this would certainly be preferable
;;to manually "filling" via one set of rules, then
;;breaking the rules when things suit us...

;;Wow...this opens up a slew of interesting possibilities...
;;we can apply rules to the simulation itself, to determine
;;for instance, if a unit deployed to a demand, it must have
;;gone through training at a prior point...


;;Potential Bugs - Pending Verification
;;=====================================
;;Overlap bug - legacy implementation had a problem with unit
;;overlap + early disengagement causing invalid deployability.
;;stemmed from inconsistent deployability criteria (should
;;be unit's bogbudget > policy's overlap.  Believe the
;;port eliminated this inconsistency, but it needs to be
;;verified.

;;Data Validation Needs
;;=====================
;;Having done a couple of runs and pushed the scripting
;;and data around, it's pretty easy to mess up transitioning
;;between SRM and ARFORGEN policies.
;;We need to include some invariants and exceptions to
;;safeguard the data validation prior to running.
;;One example: if you're using a Rotational demand
;;category, the start/endstate shouldn't matter, where
;;if you're using SRM, they definitely matter.

;;Additional Policy/Demand Validation For SRM Policies
;;====================================================
;;It'd be a "good thing" to be able to verify ahead of time
;;if we have demands that can never have their preconditions
;;met based on the policies of units.  For instance, we
;;had a M_NDA_C1 transition that never could actually
;;happen.  So, we need to shore this up and tell the user
;;ahead of time if there's a problem.

;;Note: shouldn't be an issue with older policies.



;;Finite Cycle Length Goal (Comparing Infinite / Finite
;;Cyclelength Policies Consistently)
;;=====================================================

;;This is a little simulation to help examine the consequences of
;;adopting the FCLG(finite cycle-length goal) proposal for solving
;;comparisons between infinite and finite cycle lengths.

;;So, the default mechanism for comparing entities, at least
;;the most prevalent, is normalized dwell.  Normalized dwell
;;arose as an attempt to make the sourcing of units "fair"
;;regardless of policy cycle.  Unlike the discrete prioritized
;;fills by compo-pool from older days, which implicitly biased
;;fills by compo (i.e. drain ac available first, then rc avail, etc.)
;;we can get a fairer sharing by projecting each entity onto a
;;normalized cycle length.  This provides a measure of progress
;;in the cycle as a function of cycletime (nominally a measure of
;;dwell prior to deployment).  This gave us a more balanced measure
;;that cut across components.  Note: if policies under comparison
;;have identical cycle lengths, we effectively get a measure of
;;dwell, just normalized by the cyclelength....

;;We run into problems when we try to compare "infinite" cyclelength
;;policies with finite policies.  The problem is that the proxy
;;for infiniteness is just a massively large integer.  When
;;comparing against finite cyclelengths, this has the effect
;;of causing us to have a massiv denominator in the normalizing
;;computation, leading to a weighting where finite policies
;;almost always implicitly "Win" when compared to infinite ones.
;;The only advantage infinite policies offer here is to provide
;;perhaps earlier deployable times.  The net effect is that
;;buckets of supply with heterogenous mixtures of finite and
;;"infinite" policies will end up filling from the finites
;;first, even if it may not make sense.

;;One proposed solution (A) is to - during comparison - derive a constant
;;to use as a proxy for the infinite lengths.  The original idea
;;was to choose a constant from the set of supply under consideration
;;for sorting, that is, find the max or min cycle-length from the
;;the entities and use that to compute normalized dwells as needed
;;iff comparing infinite to finite policies.

;;We'll call this constant the Finite Cycle Length Goal (FCLG).

;;The FCLG is great - we want immutable, consistent things
;;when trying to derive consistent comparisons.  Yet,
;;solution A has problems, namely that it's dependent on a
;;likely variable input - the population of entities being
;;sorted at a given point in time - which could create
;;unintentional biases and seemingly variable comparisons.
;;If the batch has a single outlier, the FCLG now either
;;penalizes (or benefits) the copmutations for all
;;finite-to-infinite comparisons.  If the FCLG changes
;;due to the entry (or exit) of a single entity in the
;;next fill, we could have the opposite effect.  These
;;swings could happen throughout.  They make the skin
;;crawl, since the order of fill is now a variable,
;;dependent on time and composition, rather than an
;;constant thing.  If possible, we'd like to tie
;;our order to something fixed.

;;It turns out we already do this in a sense, when
;;computing initial conditions for entities following
;;said infinite policies.  We have to make an assumption
;;about "where" they are in cycle time at the beginning
;;of the simulation.  The arrived-upon rule is to detect
;;effectively infinite cycle lengths (currently lengths over
;;30 years), and project them along a 3-year, or 1095 day
;;cyclelength instead.  This keeps the initial conditions
;;from surging into astronomical regions (say entities
;;have billions of years of dwell).  The 3-year mark
;;came from a business rule during the inception of
;;the infinite length policies....The constant
;;marathon.ces.entityfactory/+default-cyclelength+
;;defines this.

;;So, the counter-proposal is simple: We re-use the
;;bridge between infinite/finite we already established
;;(for initial conditions), and use the same policy
;;constant.  The effect is consistent: when comparing
;;infinite to infinite, we replace one constant (99999999)
;;with /+default-cyclelength+.  The computed proportions
;;are just scaled in an affine? manner, and the ordering
;;should be identical to the original scheme.
;;When comparing finite->infinite, the rather than having
;;a biased result against our formerly gigantic demoniator
;;that effectively zeroed out all of our infinite-length
;;policies, we have a consistent measure of progress
;;relative to a 3-year lifecycle, and the ability to
;;make meaningful inferences about "normalized dwell"
;;between finite and infinite policies.

;;The following simulation generates random populations
;;and computes their fill ordering using the old
;;and new schemes.
(comment
  (def inf 9999999)
  (def fclg 1095)
  (def policies
     (->>  [{:policy-name :max
             :cycle-length inf}
            {:policy-name :near-max
             :cycle-length inf}
            {:policy-name :ac12
             :cycle-length (* 365 3)}
            {:policy-name :rc15
             :cycle-length (* 365 6)}]
           (mapv #(assoc % :fclg
                         (if (= (:cycle-length %) inf)
                           fclg
                           (:cycle-length %)))))
    )

  (defn random-unit [id pol]
    (let [cl (:cycle-length pol)
          cl (if (== cl inf) fclg cl)]
      (merge pol {:id id
                :ct (rand-int cl)
                }
           )))

  (defn random-population [n]
    (for [i (range n)]
      (random-unit i (rand-nth policies))))

  (defn normalized-dwell [x]
    (/ (:ct x)
       (:cycle-length x)))

  (defn fclg-dwell [x]
    (/ (:ct x)
       (:fclg x)))

  (defn test [& {:keys [n] :or {n 10}}]
    (let [xs (random-population n)
          fs (map #(assoc % :dwell (double (fclg-dwell %))) xs)
          norms (map #(assoc % :dwell (double (normalized-dwell %))) xs)]
      {:norms (sort-by (comp - :dwell) norms)
       :fs    (sort-by (comp - :dwell) fs) }))

  (require '[incanter [core :as i] [charts :as c]])
  (defn view-test [& {:keys [n] :or {n 10}}]
    (let [{:keys [norms fs]} (test :n n)]
      (i/view (c/scatter-plot (range (count norms)) (map :dwell norms)
                              :title "Normalized Dwell Using CycleLength"
                              :x-label "Ranked units"
                              :y-label "Normalized Dwell"))
      (i/view (c/scatter-plot (range (count fs)) (map :dwell fs)
                              :title "Normalized Dwell Using Finite CycleLength Goal"
                              :x-label "Ranked units"
                              :y-label "Normalized Dwell"))
      ))
)


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


