(ns marathon.ces.policyio
  (:require [marathon.ces [policyops :as policyops]
                          [policy :as pol]]
            [marathon.data [protocols :as core]
                           [period :as per]
                           [store :as simstate]]
            [spork.util [table :as tbl]]))

;Data related to the policystore, is actually quite disparate.  There are
;multiple data sources that must be read to compose and initialize a policystore 
;that has information on simulation periods, rotational policies, and 
;substitution relations.  As such, policyIO has enough code to justify breaking
;it out into a separate module, for organizational sanity.

;The functions in this module concern reading data regarding policies and 
;converting it into internal data structures.  Many of the records and other 
;data forms Marathon uses serve as a specification or a list of instructions 
;that are intended to be parsed to build policy structures.  The following 
;functions support the transformation of records to various policystore related
;structures, such as relations, atomic rotational policies, composite rotational
;policies, tables of policy records, etc.  Most of these functions will be 
;wrapped in higher level constructors, like tablesToPolicyStore, or 
;policyStoreFromExcel, which glue together the lower level IO and parsing 
;functions for the end user.  Additional parsing functions, such as JSON and 
;Clojure data readers/writers, will also crop up here as needed.

(def deployable-templates
  (atom #{ ;core/MaxUtilization core/NearMaxUtilization core/FFGMission
          :MaxUtilization :NearMaxUtilization :FFGMission}))

;Flag for policies that don't need have their deployable ranges set.  
;Only applies to MaxUtilization.
(defn deployable-set? [template-name]
  (contains? @deployable-templates template-name)) 

;LEGACY issue.
;this is a little weak, we could use a map here instead of a list of values.
(defn record->relation [inrec] 
  ((juxt :Relation :Donor :Recepient :Cost) inrec))

;;TODO -- Defer policy loading so that these are delayed.  Or make
;policy generation faster (we're bottlenecking there a bit).  We don't
;use all the policies, so we simply defer them until necessary, and
;throw in a check for realized? when we go to lookup a policy.
;(_   _    _  PolicyName Template MaxDwell MinDwell MaxBOG StartDeployable StopDeployable Overlap Recovery BOGBudget Deltas  _)
(defn record->policy 
  [{:keys [Template PolicyName MaxBOG MaxDwell MinDwell Overlap 
           StartDeployable StopDeployable Deltas]}]
  (let [deltas (if (= Deltas "{}") {}
                   (clojure.edn/read-string
                    (clojure.string/replace Deltas
                                            "\\" "")))
        ]
    (-> (if (= Template "Ghost") 
          (policyops/register-ghost-template PolicyName MaxBOG  :overlap Overlap)
          ;;we'd like to delay this if possible...          
          (policyops/register-template Template MaxDwell MinDwell MaxBOG 
                                 StartDeployable StopDeployable
                                 :overlap Overlap 
                                 :deltas  deltas
                                 :deployable-set? (deployable-set? Template)))
        (assoc :name PolicyName))))           

(def reltbl {"SUB" :sub
             "EQUIVALENCE" :equivalence})
(defn rel->key [r]
  (get reltbl (clojure.string/upper-case (clojure.string/trim r))))
    
;generate a sequence of relations from the table records
(defn table->relations [t]
  (->> (tbl/table-records t)
       (filter (fn [r] (:Enabled r)))
       (map (fn [r] (if-let [rel (rel->key (:Relation r))]
                      (assoc r :Relation rel)
                      (throw (Exception. (str "unknown relation: " (:Relation r)))))))                                  
       (map record->relation)))

;generate a collection of atomic policies from the table records
(defn table->policies [t] (map table->policies (tbl/table-records t)))
       
;generate a dictionary of atomic policies from a table, where the keys are
;policy names.  Enforces unique policy names.
(defn table->policy-map [t]
  (reduce (fn [m r]
            (let [p (record->policy r)]
              (assoc m (:name p) p)))
          {} (tbl/table-records t))) 

(def literals #{"#" "[" "(" "{"})
(defn data-literal?  [s] (literals (subs s 0 1)))
(defn implicit-seq? [^String s] (.contains s ","))
(defn read-composition [x]
  (cond (data-literal? x) (clojure.edn/read-string x)
        (implicit-seq? x) (mapv str (clojure.edn/read-string (str "[" x "]")))
        :else x))

(defn ensure-vec [x]
  (if (coll? x) x
      [x]))

;MAY BE OBSOLETE...
;Reads an expression from a record
;with keys (CompositeName Policy), 
;vals [somestring, {policy dictionary}/or [policy list]]
;We use our evaluator to transform the policy string into a policy dictionary.
;Returns a pair of [rulename, {policy dict}], or [rulename [policy sequence]]
(defn record->composition [r] 
  [(:CompositeName r) (clojure.edn/read-string (:Composition r))])

;Evaluates a record as into a key-val pair that describes a rule.
;Keys in the dictionary correspond to the name of the rule, and vals correspond 
;to a map of period names to policy names/ policies.
(defn add-composition [acc r]
  (if (contains? r :Composition)
    (conj acc (record->composition r))
    (let [{:keys [CompositeName CompositionType]} r
          ;;For legacy support, if composition type is
          ;;absent, implies it's a periodic
          CompositionType (or CompositionType "Periodic")]
      (case CompositionType 
        "Periodic" (update-in acc [CompositeName] assoc (:Period r) (:Policy r))
        "Sequential" (do (assert (not (contains? acc CompositeName)))
                         (assoc-in acc [CompositeName]
                             (ensure-vec (read-composition (:Policy r)))))
        (throw (Exception. (str ["Error parsing composition policy!" 
                                 r])))))))

;Generates a map of composition rules from a table.
(defn table->compositions [t]
  (reduce add-composition {} (tbl/table-records t)))

;Create a policystore from a set of tables.
;much more robust, uses the generictable interface to simplify loading.
(defn tables->policystore 
  ([relation-table period-table atomic-table composite-table]
     (->> simstate/empty-policystore
          (pol/add-relations (table->relations relation-table))
          (pol/add-periods   (map per/record->period (tbl/table-records period-table)))
          (pol/add-dependent-policies (table->policy-map atomic-table) 
                                      (table->compositions composite-table))))
  ([{:keys [RelationRecords PeriodRecords PolicyRecords CompositePolicyRecords]}]
     (tables->policystore RelationRecords PeriodRecords PolicyRecords CompositePolicyRecords)))

;;testing
(comment

(require '[marathon.sim.sampledata :as sd])
;; (def atomics     (sd/get-sample-records :PolicyRecords))
;; (def composites  (sd/get-sample-records :CompositeRecords))

(def atomics     (get sd/sample-tables :PolicyRecords))
(def composites  (get sd/sample-tables :CompositePolicyRecords))
(def rels        (get sd/sample-tables :RelationRecords))
(def pers        (get sd/sample-tables :PeriodRecords))
(def pstore      (tables->policystore rels pers atomics composites))

)
