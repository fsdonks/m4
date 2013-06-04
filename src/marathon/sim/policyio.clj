(ns marathon.sim.policyio
  (:require [marathon.sim [policyops :as policyops]
                          [policy :as pol]]
            [marathon.data [protocols :as core]
                           [period :as per]]
            [marathon.policy [policystore :as pstore]]
            [util [table :as tbl]]))

;-------------TODO------------ 
;Port periodlib into marathon.data.period,  specifically for table->periods
;-------------TODO------------ 

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


(def *deployable-templates* 
  (atom #{core/MaxUtilization core/NearMaxUtilization core/FFGMission
          :MaxUtilization :NearMaxUtilization :FFGMission}))

;Flag for policies that don't need have their deployable ranges set.  
;Only applies to MaxUtilization.
(defn deployable-set? [template-name]
  (contains? @*deployable-templates* template-name)) 

;LEGACY issue.
;this is a little weak, we could use a map here instead of a list of values.
(defn record->relation [inrec] 
  ((juxt [:Relation :Donor :Recepient :Cost]) inrec))

;(_   _    _  PolicyName Template MaxDwell MinDwell MaxBOG StartDeployable StopDeployable Overlap Recovery BOGBudget Deltas  _)
(defn record->policy 
  [{:keys [Template PolicyName MaxBOG MaxDwell MinDwell Overlap 
           StartDeployable StopDeployable Deltas]}]
  (let [deltas (or (read-string Deltas) {})]
    (-> (if (= Template "Ghost") 
          (policyops/register-ghost-template PolicyName MaxBOG Overlap)
          (policyops/register-template Template MaxDwell MinDwell MaxBOG 
                                 StartDeployable StopDeployable Overlap deltas 
                                 (deployable-set? Template)))
        (assoc :name PolicyName))))           

;generate a sequence of relations from the table records
(defn table->relations [t]
  (->> (tbl/table->records t)
       (filter (fn [r] (:Enabled r)))
       (map record->relation)))

;generate a collection of atomic policies from the table records
(defn table->policies [t] (map table->policies (table->records t)))
       
;generate a dictionary of atomic policies from a table, where the keys are
;policy names.  Enforces unique policy names.
(defn table->policy-map [t]
  (->> (tbl/table->records t)
       (map record->policy))) 

;MAY BE OBSOLETE...
;Reads an expression from a record
;with keys (CompositeName Policy), 
;vals [somestring, {policy dictionary}/or [policy list]]
;We use our evaluator to transform the policy string into a policy dictionary.
;Returns a pair of [rulename, {policy dict}], or [rulename [policy sequence]]
(defn record->composition [r] 
  [(:CompositeName r) (read-string (:Composition r))])

;Evaluates a record as into a key-val pair that describes a rule.
;Keys in the dictionary correspond to the name of the rule, and vals correspond 
;to a map of period names to policy names/ policies.
(defn add-composition [m r]
  (if (contains? r :Composition)
    (conj acc (record->composition r))
    (let [{:keys [CompositeName CompositionType] r}]
      (case CompositionType 
        "Periodic" (update-in acc [CompositeName] assoc (:Period r) (:Policy r))
        "Sequential" (do (assert (not (contains? acc rulename)))
                       (assoc-in acc [CompositeName] (read-string (:Policy r))))
        (throw (Exception. "Error parsing composition policy!" 
                           CompositeName))))))

;Generates a map of composition rules from a table.
(defn table->compositions [t]
  (reduce add-composition {} (tbl/table->records t)))

;Create a policystore from a set of tables.
;much more robust, uses the generictable interface to simplify loading.
(defn tables->policystore 
  [relation-table period-table atomic-table composite-table]
  (->> pstore/empty-policystore
       (pol/add-relations (table->relations relation-table))
       (pol/add-periods (per/table->periods period-table))
       (pol/add-dependent-policies (table->policymap atomic-table) 
                                   (table->compositions composite-table))))       

;-----------DEFERRED--------------------

;Returns a policystore object initialized from default tables in Excel.  
;Mostly for compatibility.

;Public Function policyStoreFromExcel() As TimeStep_ManagerOfPolicy
;Set policyStoreFromExcel = tablesToPolicyStore(getTable("RelationRecords"), _
;                             getTable("PeriodRecords"), _
;                             getTable("PolicyRecords"), _
;                             getTable("CompositePolicyRecords"))
;End Function
;Public Function policyStoreFromFiles() As TimeStep_ManagerOfPolicy
;Dim tbl
;Dim tblist As Collection
;Dim p As String
;p = ActiveWorkbook.path & "\"
;
;Set tblist = list("RelationRecords", "PeriodRecords", "PolicyRecords", 
;                  "CompositePolicyRecords")
;For Each tbl In tblist
;    TableLib.saveTable getTable(CStr(tbl)), p & "\" & CStr(tbl) & ".json"
;Next tbl
;                             
;Set policyStoreFromFiles = 
;       tablesToPolicyStore(getTable(p & "RelationRecords" & ".json"), _
;                           getTable(p & "PeriodRecords" & ".json"), _
;                           getTable(p & "PolicyRecords" & ".json"), _
;                           getTable(p & "CompositePolicyRecords" & ".json"))
;
;End Function

;Public Function folderToPolicyStore(folderpath As String) 
;   As TimeStep_ManagerOfPolicy
;Dim tbl
;Dim tblist As Collection
;Dim p As String
;p = folderpath
;
;Set tblist = list("RelationRecords", "PeriodRecords", "PolicyRecords", 
;                                     "CompositePolicyRecords")
;Set folderToPolicyStore = 
;    tablesToPolicyStore(getTable(p & "RelationRecords" & ".json"),
;                        getTable(p & "PeriodRecords" & ".json"), _
;                        getTable(p & "PolicyRecords" & ".json"), _
;                        getTable(p & "CompositePolicyRecords" & ".json"))
;                             
;End Function