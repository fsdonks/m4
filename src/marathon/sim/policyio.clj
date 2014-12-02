(ns marathon.sim.policyio
  (:require [marathon.sim [policyops :as policyops]
                          [policy :as pol]]
            [marathon.data [protocols :as core]
                           [period :as per]]
            [marathon.policy [policystore :as pstore]]
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

;(_   _    _  PolicyName Template MaxDwell MinDwell MaxBOG StartDeployable StopDeployable Overlap Recovery BOGBudget Deltas  _)
(defn record->policy 
  [{:keys [Template PolicyName MaxBOG MaxDwell MinDwell Overlap 
           StartDeployable StopDeployable Deltas]}]
  (let [deltas (or (clojure.edn/read-string Deltas) {})]
    (-> (if (= Template "Ghost") 
          (policyops/register-ghost-template PolicyName MaxBOG  :overlap Overlap)
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
    (let [{:keys [CompositeName CompositionType]} r]
      (case CompositionType 
        "Periodic" (update-in acc [CompositeName] assoc (:Period r) (:Policy r))
        "Sequential" (do (assert (not (contains? acc CompositeName)))
                       (assoc-in acc [CompositeName] (read-string (:Policy r))))
        (throw (Exception. "Error parsing composition policy!" 
                           CompositeName))))))

;Generates a map of composition rules from a table.
(defn table->compositions [t]
  (reduce add-composition {} (tbl/table-records t)))

;Create a policystore from a set of tables.
;much more robust, uses the generictable interface to simplify loading.
(defn tables->policystore 
  ([relation-table period-table atomic-table composite-table]
     (->> pstore/empty-policystore
          (pol/add-relations (table->relations relation-table))
          (pol/add-periods   (map per/record->period (tbl/table-records period-table)))
          (pol/add-dependent-policies (table->policy-map atomic-table) 
                                      (table->compositions composite-table))))
  ([{:keys [RelationRecords PeriodRecords PolicyRecords CompositePolicyRecords]}]
     (tables->policystore RelationRecords PeriodRecords PolicyRecords CompositePolicyRecords)))

;;this is currently too slow because we're generating a shitload of 
;;intermediate colls, we're also evaling first.  I shifted to
;;memoizing; along with lazy loading of policies, that might be a good
;;option.

;; (comment 
;; (defn 
;;   get-position!  [p cycletime] 
;;   (loop [pos startstate
;;          t   0]
;;     (if-let [nxt (first (graph/sinks positiongraph pos))]
;;       (let [tnxt (+ t   (long (graph/arc-weight positiongraph pos nxt)))]
;;         (if (>= tnxt cycletime) pos
;;             (recur nxt tnxt)))
;;       (throw (Exception. "Cycletime exceeds policy!")))))
;; (defn 
;;   (set-deployable!   [p tstart tfinal] (-> p 
;;                                            (core/insert-modifier tstart {:name :deployable})
;;                                            (core/insert-modifier tfinal {:name :non-deployable})
;;                                            (core/mark-deployable-region))))
;; (defn insert-modifier 
;;   ([policy cycletime {:keys [name weight] :or {name :modified weight 0}}]
;;      (let [x     (get-position policy cycletime)
;;            nxt   (next-position policy x)      
;;            pg    (get-position-graph policy)
;;            tprev (-> (graph/depth-first-search pg (start-state policy) x {:weightf graph/arc-weight})
;;                      (get :distance)
;;                      (get x))
;;            offset (- cycletime tprev)
;;            dnxt   (- (graph/arc-weight pg x nxt) offset)]                          
;;        (set-position-graph policy
;;             (-> pg 
;;                 (graph/disj-arc x nxt)
;;                 (graph/add-arcs [[x name offset]
;;                                   [name [x name] weight]
;;                                   [[x name] nxt dnxt]])))))
;;   ([policy cycletime] (insert-modifier policy cycletime {})))

;; (defn mark-deployable-region 
;;   "Adds modifiers to each node in the position graph of a policy between :deployable and :non-deployable nodes, 
;;    indicating the position is an eligible deployable state."
;;   [policy] 
;;   (let [pg (get-position-graph policy)]
;;     (if-let [path (graph/first-path (graph/depth-first-search pg :deployable :non-deployable))]    
;;       (->> (update-nodes pg (drop 1 (butlast path)) #(toggle-tag % :deployable))
;;            (set-position-graph policy))
;;       (throw (Exception. (str "No deployable range found between in " policy))))))

;;)

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
