(ns marathon.ces.testing.async
   (:require [marathon.ces [engine   :as engine  :refer :all]]
            [marathon.ces [fill     :as fill]]
            [marathon.ces [core     :as core]
                          [supply   :as supply]
                          [demand   :as demand]
                          [unit     :as unit]
                          [policy   :as policy]
                          [policyio :as policyio]
                          [policyops :as policyops]
                          [sampledata :as sd]
                          [entityfactory :as ent]
                          [setup :as setup]
                          [query :as query]
             [deployment :as deployment]]
            [marathon.ces.fill [demand :as filld]]
            [marathon.data   [protocols :as generic]]
            [marathon.demand [demanddata :as dem]]
            [marathon.project [linked :as linked]
             [excel :as xl]]
            [marathon.project :as proj]
            [spork.sim     [simcontext :as sim]
                           [history :as history]]
            [spork.entitysystem
             [store :as store]
              [diff :as entdiff]]
            [spork.util [reducers] [diff :as diff]
             [tags :as tags]
             [table :as tbl]]
            [spork.sketch :as sketch]
            [clojure.core [reducers :as r]]
            [clojure.test :as test :refer :all]
            [marathon [analysis :as analysis]
             [observers :as obs]]
            [marathon.analysis.tacmm.demo :as tacmm]
            [marathon.analysis.requirements :as req]
            [marathon.analysis.random :as random]
            [marathon.spec :as spec]
            [clojure.java.io :as java.io]))

;;Forward station testing
(defn get-demands
[[t ctx :as frame]]
  (let [actives (store/gete ctx :DemandStore  :activedemands)]
    (->> actives
         (keys)
         (map #(store/get-entity ctx %)))))

(defn forward-unit?  "Is a unit forward stationed?"
  [u]
  (= (:aligned u) :forward))

(defn get-units
  [ctx d]
  (let [overlappers (keys (:units-overlapping d))
        assigned (keys (:units-assigned d))]
    (concat overlappers assigned)))

(defn units-forward? "Are all units forward-stationed that are
  assigned or overlapping in a demand?"
  [ctx d]
    (every? (fn [entity-name] (forward-unit? (store/get-entity ctx
                                                               entity-name)))
            (get-units ctx d)))

(defn forward-demand? "Is a demand a forward stationed demand?"
  [d]
  (= (:region d) :forward))

(defn forward-in-demands?  "Check that all of the forward stationed
  demands are only filled by forward stationed units."
  [[t ctx :as frame]]
  (let [forward-demands (filter forward-demand? (get-demands frame))]
    (every? (partial units-forward? ctx) forward-demands)))

  (defn count-forwards  "Count the number of forward-staioned units
  that are deployed."
  [[t ctx :as frame]]
  (let [forward-demands (filter forward-demand? (get-demands frame))]
    (reduce + (map (fn [d] (count (get-units ctx d))) forward-demands))))

(defn units-in-demands
  [[t ctx :as frame]]
  (let [demands (get-demands frame)]
    (mapcat (partial get-units ctx) demands)))
        
(defn get-col-index
  "Given a marathon project, name of a table in :tables, and the name
  of a field in the record, returns the column index of that field in
  the spork table."
  [proj tbl-key rec-key]
  (let [col-index (.indexOf (get-in proj [:tables tbl-key :fields]) rec-key)
        _ (assert (not (neg? col-index))
                  (str [:column-doesnt-exist! rec-key]))]
    col-index
    ))

(defn record-assoc
  "Assoc a value onto a record within a marathon project by specifying
  the table keyword, the index of the record to update the value in,
  the key to add to the record, and the value.
  The key must already exist in the table so that this is a valid
  table operation. Returns the proj with the update value."
  [proj tbl-key rec-index rec-key value]
  (let [col-index (get-col-index proj tbl-key rec-key)]
    (assoc-in proj [:tables
                    tbl-key
                    :columns
                    col-index
                    rec-index] value)))

(defn copy-row
  "Copy the nth row in a table and add it to the end of the table."
  [table n]
  (let [row-values (tbl/nth-row table n)]
    (assoc table :columns (tbl/conj-row (:columns table) row-values))))
        
(defn copy-row-in
  "Copy a row within a marathon project by specifying
  the table keyword and the index of the record to copy.
  Returns the proj with the updated value."
  [proj tbl-key rec-index]
  (update-in proj
             [:tables
              tbl-key]
             copy-row rec-index))           

(defn update-params
  "Given a marathon project, update a value in the parameters table
  with a map of field name (keyword or string) to value."
  [proj update-map]
  (update-in proj [:tables :Parameters] tacmm/xform-records
          #(tacmm/merge-parameters % update-map)))
  
(defn fence-project
  "Return a project that is prepped to fun the fencing test where we
  check that only forward stationed units fill forward stationed
  demands, but those units can fill other demands if no forward
  stationed units are available, and those units go back to filling
  forward stationed demands after filling other demands and they don't
  fill something else."
  [project-pass]
  (-> project-pass
      (update-params {:DefaultACPolicy "MaxUtilization"})
      (copy-row-in :DemandRecords 0)
      ;;start day of second record=631 and category=Rotational,
      ;;duration=5, remove alignment, quantity = 31
      ;;verify that 11 AC fill this demand
      (record-assoc :DemandRecords 1 :StartDay 631)
      (record-assoc :DemandRecords 1 :Category "Rotational")
      (record-assoc :DemandRecords 1 :Duration 5)
      (record-assoc :DemandRecords 1 :Tags "")
      (record-assoc :DemandRecords 1 :Quantity 31)
      
      
      ;;last record duration=5, start-day=636, quantity = 32
      (record-assoc :DemandRecords 2 :StartDay 636)
      (record-assoc :DemandRecords 2 :Duration 5)
      (record-assoc :DemandRecords 2 :Quantity 32)
      (record-assoc :DemandRecords 2 :DemandIndex 2)))

(defn grow-forward
  "Edit project pass so that requirements analysis has a feasible
  solution and doesn't run forever when forward bin size is less than
  the demand."
  [project-pass]
  (-> project-pass
      (record-assoc :DemandRecords 0 :Quantity 1)
      (record-assoc :DemandRecords 1 :Quantity 1)
      (record-assoc :SupplyRecords 0 :Enabled false)
      ))

(defn single-record
  "Check if multiple SRC, component records exist. If so, throw
  exception.  Otherwise, return the first record."
  [recs]
  (if (> (count recs) 1)
    (throw (Exception. (str "More than one record exists!")))
    (first recs)))

(defn supply-for
  "Given a spork.util.table of supply records, return the Quantity of
  the first supply record found for the SRC and Component specified."
  [table src compo]
  (let [;;create records from the table
        recs (reduce conj [] table)]
    (->> recs
         (filter (fn [{:keys [SRC Component]}] (and (= SRC src)
                                                    (= Component
                                                       compo))))
         (single-record)
         (:Quantity))))
            
(deftest forward-only
  (let [;;load a requirements project so that we can test requirements
        ;;analysis later.
        project-fail (analysis/load-requirements-project (clojure.java.io/resource
                                             "forward-stationing.xlsx"))
        stream-fail (analysis/as-stream project-fail)                        
        ;;Fix our category on the first demand record so that it only
        ;;accepts units that are forward stationed.
        project-pass (record-assoc project-fail :DemandRecords
                                   0 :Category "Forward")
        stream-pass (analysis/as-stream project-pass)
        follow-on-fail (fence-project project-pass)
        follow-stream (analysis/as-stream follow-on-fail)
        follow-on-proj (record-assoc  follow-on-fail
                                        :DemandRecords 1
                                        :DemandGroup "Bacon")
        follow-on-fixed (analysis/as-stream follow-on-proj)
        ;;We can never grow enough AC supply if non are
        ;;forward-stationed (like in project-pass)
        ;;probably want to assert
        
        ;;for requirements analysis, need to assert that the forward
        ;;stationed supply bin is >= the forward stationed demand so
        ;;that we don't have an issue like previous.
        ;;-----
        forward-growth (grow-forward project-pass)
        ;;we should have supply grow to cover the regular demands in
        ;;addition to the forward stationed demands.
        forward-and-regular (record-assoc (grow-forward
                                           follow-on-proj)
                                          :DemandRecords 2 :Tags "")
        ac-growth (supply-for (req/requirements-from-proj
                               forward-and-regular)
                              "01205K000" "AC")
        ]
    (is (nil? (spec/validate-project follow-on-fixed))
        "Check if this project passes spec project validation.  An
  error would indicate that the spec failed.")
    (is (not (every? forward-in-demands? stream-fail)) "Just to check that
our test fails properly, our first demand has a :region :forward but a
category of NonBOG so it will accept a non-forward-stationed unit.")
    (is (every? forward-in-demands? stream-pass) "Check that
  non forward-stationed units never fill demands.")
    (is (not (every? forward-in-demands? follow-stream))
        "If a demandgroup goes from non-aligned to aligned, we could
        have units non-aligned units filling the aligned demands with followon.")
    (is (every? forward-in-demands? follow-on-fixed) "Forward stationed demands
  are only filled by forward stationed units in a more complicated
  case.")
    (is (every? (fn [[t ctx :as frame]]
                  (if (and (> t 0) (or (< t 631) (> t 635)))
                    ;;only need to check when forward stationed
                    ;;demands are active
                    (= 3 (count-forwards frame))
                    true))
                ;;no demands are filled on the last day
                (butlast follow-on-fixed))
        "The three forward stationed units on max utilization should
  always be filling the forward stationed demand."
        )
    (is (every? (fn [[t ctx :as frame]]
                  (if (and (> t 630) (< t 636))
                    ;;only need to check when forward stationed
                    ;;demands are active
                      (every? (set (units-in-demands frame))
                              ["3_01205K000_AC"
                               "2_01205K000_AC"
                               "1_01205K000_AC"])
                    true))
                follow-on-fixed)
        "The three forward stationed units on max utilization fill the
  non-forward stationed demands if there aren't any forward stationed
  demands to fill."
        )
    (is (= (supply-for (req/requirements-from-proj forward-growth)
                       "01205K000" "AC")
           2)
        "With three forward stationed units and two forward stationed
demands, do we grow two units?")
    (is (and (> ac-growth 11) (<= ac-growth 32)) "AC should grow
beyond the 3 forward stationed units and existing 11 ac supply in
order to meet the max of demand of 32 units for a
non-forward-stationed demand.")
    ))

;;When defining forward-stationed demands, we should check/assert that
;;the demands have a :region :forward, a :Category Forward, so one
;;of those shouldn't exist without the other.
;;In most cases, we probably also want a forward stationed supply and
;;we probably want forward stationed stuff to be highest priority, but
;;this might not always be the case.


;;_____________________________________________________
;;marathon.analysis.random tests.
(def previous-results
  (java.io/resource "runamc-testdata_results_before-m4-merge.txt"))
(def new-results-book
  (java.io/resource "runamc-testdata.xlsx"))

(defn make-new-results
  [proj]
  ;;the old results were created with one thread to make sure that
  ;;each inventory level for an src has the same seed given that
  ;;the default seed was used in both cases.
  (binding [random/*threads* 1]
    (let [p (analysis/load-project proj)
          phases [["comp" 1 821] ["phase-1" 822 967]]]
      (random/rand-runs-ac-rc 5 ;;min-distance
                              0.5 ;;lower-rc
                              0.7 ;;upper-rc
                              (random/add-transform p random/adjust-cannibals
                                                    []) :reps 2 :phases phases
                              :lower 0 :upper 0.1
                              :compo-lengths random/default-compo-lengths
                              ))))
        
(defn set-tab-delim-tolerance
  "results.txt is still reading the rep-seed as scientific even with a
  no scientific parse mode.  Not sure why, but for now, this will make
  the old and new rep seeds equal."
  [{:keys [rep-seed] :as r}]
  (assoc r :rep-seed (long (/ rep-seed 1000000))))

(defn compare-rand-recs
  "For two compare two sequences of results, we first round the rep
  seed to something that will match and then return the records
  for comparison."
  [results]
  (->> 
   results
   (map (fn [r] (set-tab-delim-tolerance r)))))

(deftest runamc-merge-check
  (let [old-results (into [] (tbl/tabdelimited->records
                              (slurp previous-results)
                              :parsemode :no-science))
        new-results (make-new-results new-results-book)]
    (is (apply = (map compare-rand-recs [old-results new-results]))
        "Make sure that the results are the same from when we ran them
in the run-amc repo before we moved and refactored the code to
marathon.analysis.random and after we made that move.")))
