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

(def path (io/file-path "../notional/base-testdata-v8.xlsx"))
(def modpath (io/file-path "../notional/mod-testdata.xlsx"))

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

(def ctx (-> (a/load-context modpath)
             mod-supply
             #_mod-demand))

;;we now have a modernized supply and demand.
;;This is artificial, but it works (assuming we
;;can inject the mod information into the inputs).

;;given a demand, we assoc a mod level with it.  Assume this is a given.
(def d (store/get-entity ctx "2_Bob_01205K000_[1...3651]"))


;;explicit queries, exercising the functionality.
(def mod2-units
  (->> ctx
       (marathon.ces.query/find-supply {:where   #(= (:mod %) 2)
                                        :order-by  marathon.ces.query/MOD2
                                        :collect-by [:name :mod :cycletime]})))

;;(["28_01205K000_NG" 2 1622] ["10_01205K000_AC" 2 895])

(def mod3-units
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
(def feasibles (->> ctx
                    (marathon.ces.query/find-supply (marathon.ces.fill/demand->rule d))
                    (map (comp (juxt :name :mod marathon.ces.unit/normalized-dwell) second))))

;; #_(["28_01205K000_NG" 2 0.888767]
;;    ["10_01205K000_AC" 2 0.817351]
;;    ["29_01205K000_NG" 3 0.944109]
;;    ["11_01205K000_AC" 3 0.908675]
;;    ["27_01205K000_NG" 3 0.832876]
;;    ["9_01205K000_AC" 3 0.72694])

(def dmod (->> ctx c/demands (some (fn [{:keys [source-first] :as r}]
                                     (when (= source-first "MOD1-Target-AC")
                                       r)))))
(def modrule (marathon.ces.fill/demand->rule dmod))
(def modw     (:where modrule identity))

(def mod-feasibles (->> ctx
                        (marathon.ces.query/find-supply
                           (assoc modrule :where (fn [u] (= (:component u) "AC"))))
                        (map (comp (juxt :name :mod marathon.ces.unit/normalized-dwell) second))))


