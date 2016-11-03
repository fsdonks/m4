;;Functions for instantiating simstate from a variety of serialized
;;formats.  Also functions for providing sample or test data in the 
;;form of a fully prepped simstate.
(ns marathon.ces.setup
  (:require [marathon.ces.sampledata :as sd]
            [marathon.ces [core :as core]
                          [demand :as demand]
                          [supply :as supply]
                          [policy :as policy]
                          [policyops :as policyops]
                          [policyio :as policyio]
                          [entityfactory :as ent]]
            [marathon.ces.fill [fillgraph :as fillgraph]
                               [scope :as scope]]
            [marathon.fill [fillstore :as fillstore]]
            [spork.util [tags :as tags]
                        [table :as tbl]]))

;;We need to establish a workflow here...
;;Typical workflow for analysts is to work
;;Excel, build the project, then
;;pull in tables from there.

;;Could use a core API...
;;as-tables...
;;This is where the marathon project
;;format could come in handy.
;;Tables is just a map of table-name
;;to table...so...a database....
;;We can have multiple versions of
;;this..

;;A central resource for getting tables.  
(def ^:dynamic *tables*  sd/sample-tables)
(defn alt-name [x]
  (cond (keyword? x) (subs (str x) 1)
        (string? x)  (keyword x)))

;;This is our central point for acquiring data...
(defn get-table 
  ([name]      (get-table name *tables*))
  ([name tbls] (or (get tbls name) (get tbls (alt-name name)))))

(defn as-records [t]
  (cond (tbl/tabular? t) (tbl/table-records t)
        (and (seq? t) (map? (first t)))  t ;;infers that the table is a sequence of recordss...
        :else (throw (Exception. (str [:not-table-or-record-seq t])))))

;;We should expand this to allow us to view tables
;;as record
(defn get-records
  ([name]      (as-records (get-table name)))
  ([name tbls] (as-records (get-table name tbls))))

;;Automates the building of a default behavior manager, linking it to a supply store.

;; Public Function defaultBehaviorManager(state As TimeStep_SimState, Optional bm As TimeStep_ManagerOfBehavior) As TimeStep_ManagerOfBehavior
;; If bm Is Nothing Then Set bm = New TimeStep_ManagerOfBehavior
;; bm.initBehaviors state
;; Set defaultBehaviorManager = bm
;; End Function


;;Function to read in data from the existing table of [ParameterName
;;Value] records.  Produces a map of parameters with the param names keyworded.
(defn table->parameters [paramtbl]
  (persistent!
   (reduce (fn [acc {:keys [ParameterName Value]}]
             (assoc! acc (keyword ParameterName) Value)) 
           (transient {}) paramtbl)))

;;Reads SRC tags from a table, either returning a new set of tags, or adding them to existing tags.
(defn get-src-tags [tagtbl & {:keys [init-tags] :or {init-tags tags/empty-tags}}]
  (reduce (fn [acc {:keys [SRC Tag]}]
            (tags/tag-subject acc Tag SRC))
          init-tags tagtbl))

;;A simple function to automate buildings a map of parameters from a
;;table of parameters [ParameterName Value]+ and a table of SRC Tags,
;;[SRC Tag]+.
(defn tables->parameters [paramtbl srctagtbl]
  (let [ps (table->parameters paramtbl)]
    (->> (get ps :src-tags tags/empty-tags) 
         (get-src-tags srctagtbl :init-tags)
         (assoc ps :src-tags))))

;;#TODO -> ensure numeric conversions for numeric params, like
;;LastDayDefault and friends.
;;Creates a default set of parameters derived from a paramters table and an SRCTag table.
(defn default-parameters [] 
  (tables->parameters (get-table :Parameters) (get-table :SRCTagRecords)))


;;#TODO decomplect the need for a simulation context here due to 
;;policy/initialize-policystore.

;;creates a default policy store.  We should relook this to see if we
;;really need an event context...All we're using the context for is 
;;scheduling policy updates for the periods in question.  It seems
;;like we may be able to separate that out and decomplect it.
;;Ideally, we should be returning just the policy store that's been
;;built, and initializing it (scheduling policy changes in the
;;periods) at a later time....
(defn default-policystore [] ;[ctx]
  (-> (policyio/tables->policystore (get-table :RelationRecords)
                                    (get-table :PeriodRecords)
                                    (get-table :PolicyRecords)
                                    (get-table :CompositePolicyRecords))))

;;Creates a fill store, which provides information for fill
;;preferences as well as scoping information.
(defn default-fillstore []
  (let [rawgraph (fillgraph/tables->fillgraph 
                  (get-table :SupplyRecords)
                  (get-table :DemandRecords)
                  (get-table :RelationRecords))
        fg (fillgraph/reduced-graph rawgraph)
        fm (fillgraph/fill-map fg)]
    (fillstore/make-fillstore :fillgraph fg :fillmap fm :rawfillgraph rawgraph)))

;;Return a scoped set of supply and demand, based on the information in the fillgraph of the local
;;fillstore.

;;TODO# This guy is not applying scoping rules, and leaving us to
;;exclude every demand.
(defn default-scoped-state [ctx] (scope/apply-scope ctx))

;;Creates a default supply.  The default is to derive from Excel worksheets.
(defn default-supply [ctx & {:keys  [records]}]
  (let [records (or records (get-records :SupplyRecords))
        sstore (core/get-supplystore ctx)
        pstore (core/get-policystore ctx)]
    (ent/process-units (ent/units-from-records records sstore pstore) ctx)))

;;Creates a default demand.  The default is to derive from Excel worksheets.
(defn default-demand [ctx & {:keys  [records]}]                            
  (let [records (or records (get-records :DemandRecords))
        ds  (ent/demands-from-records records ctx)]
    ;(demand/register-demands! ds ctx) ;;outed for arch changes...
    (demand/register-demands ds ctx)
    ))  

;;TODO parameterize this to work off data, rather than the default
;;records we have baked in at the moment....
(defn default-simstate 
  ([ctx]
      (-> ctx 
          (core/set-parameters  (default-parameters))
          (core/set-policystore (default-policystore))
          (core/set-fillstore   (default-fillstore))
          (default-scoped-state)         
          (default-supply)
          (default-demand)))
  ([] (default-simstate core/emptysim)))

;;Create a simstate from a different set of tables than the default.
(defn simstate-from
  ([tables ctx]
   (binding [*tables* tables]
     (default-simstate ctx)))
  ([tables] (simstate-from tables core/emptysim)))




          
          
         
         
         
      
