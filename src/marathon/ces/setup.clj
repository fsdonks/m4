;;Functions for instantiating simstate from a variety of serialized
;;formats.  Also functions for providing sample or test data in the 
;;form of a fully prepped simstate.
(ns marathon.ces.setup
  (:require [marathon.ces.sampledata :as sd]
            [marathon.ces [core :as core]
                          [demand :as demand]
                          [supply :as supply]
                          [policy :as policy]
                          [policyio :as policyio]
                          [entityfactory :as ent]]
            [marathon.ces.fill [fillgraph :as fillgraph]
                               [scope :as scope]]
            [marathon.fill [fillstore :as fillstore]]
            [spork.util [tags :as tags]
                        [table :as tbl]]))

;;A central resource for getting tables.  
(def ^:dynamic *tables*  sd/sample-tables)
(defn alt-name [x]
  (cond (keyword? x) (subs (str x) 1)
        (string? x)  (keyword x)))

;;This is our central point for acquiring data...
(defn get-table 
  ([name]      (get-table name *tables*))
  ([name tbls] (or (get tbls name) (get tbls (alt-name name)))))

(defn get-records
  ([name]      (tbl/record-seq (get-table name)))
  ([name tbls] (tbl/record-seq (get-table name tbls))))

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


;;---------------------------------------------------


;;    Public Function SupplyfromExcel(policystore As TimeStep_ManagerOfPolicy, parameters As TimeStep_Parameters, behaviors As TimeStep_ManagerOfBehavior, _
;;                                        ctx As TimeStep_SimContext, Optional ensureghost As Boolean) As TimeStep_ManagerOfSupply
;;    Dim tbl As GenericTable
;;    Dim gunit As TimeStep_UnitData
  
;;    Set SupplyfromExcel = New TimeStep_ManagerOfSupply
;;   'TODO -> turn this into a function.
;;    UnitsFromSheet "SupplyRecords", SupplyfromExcel, behaviors, parameters, policystore, ctx
  
;;    If ensureghost Then
;;        If Not SupplyfromExcel.hasGhosts Then
;;            Set gunit = createUnit("Auto", "Ghost", "Anything", "Ghost", 0, "Auto", parameters, policystore)
;;            Set gunit = associateUnit(gunit, SupplyfromExcel, ctx)
;;            registerUnit SupplyfromExcel, behaviors, gunit, True, ctx
;;            Debug.Print "Asked to do requirements analysis without a ghost, " & _
;;                "added Default ghost unit to unitmap in supplymanager."
;;        End If
;;    End If
  
;;    End Function


;;    Public Sub fromExcel(supplystore As TimeStep_ManagerOfSupply, policystore As TimeStep_ManagerOfPolicy, _
;;                            parameters As TimeStep_Parameters, behaviors As TimeStep_ManagerOfBehavior, _
;;                                ctx As TimeStep_SimContext, Optional ensureghost As Boolean)
  
;;    Dim gunit As TimeStep_UnitData
  
;;    UnitsFromSheet "SupplyRecords", supplystore, behaviors, parameters, policystore, ctx
  
;;    If ensureghost Then
;;        If Not supplystore.hasGhosts Then
;;            Set gunit = createUnit("Auto", "Ghost", "Anything", "Ghost", 0, "Auto", parameters, policystore)
;;           'Decoupled
;;            Set gunit = associateUnit(gunit, supplystore, ctx)
;;           'decoupled
;;            Set supplystore = registerUnit(supplystore, behaviors, gunit, True, ctx)
;;            Debug.Print "Asked to do requirements analysis without a ghost, " & _
;;                "added Default ghost unit to unitmap in supplymanager."
;;        End If
;;    End If
  
;;    End Sub

;; --------------------------------------------------------------------------------

;;    Public Sub UnitsFromSheet(sheetname As String, supplystore As TimeStep_ManagerOfSupply, behaviors As TimeStep_ManagerOfBehavior, _
;;                                parameters As TimeStep_Parameters, policystore As TimeStep_ManagerOfPolicy, _
;;                                    ctx As TimeStep_SimContext)
;;    Dim tbl As GenericTable
  
;;    Set tbl = New GenericTable
;;    tbl.FromSheet Worksheets(sheetname)
  
;;    MarathonOpFactory.unitsFromTable tbl, supplystore, behaviors, parameters, policystore, ctx
 
;;    End Sub 

;; 'Tom Change 26 Oct 2012 -> extracted from simstate creation.  This is a side-effecting function.  forRequirements should
;; 'be pulled out...
;; 'Given a simulation state, prepare the output store.
;; Public Function initializeOutput(simstate As TimeStep_SimState, Optional path As String, _
;;                                     Optional forRequirements As Boolean) As TimeStep_SimState
;; If path = vbNullString Then path = ActiveWorkbook.path & "\"
;; With simstate
;;     'profile "OutputSetup"
;;     'TOM Change 7 Jun 2011 -> moved to end to ensure locations are known prior to initialization.
;;     .outputstore.init path, , .policystore.locations, forRequirements
;;     'profile "OutputSetup"
;; End With
;; 
;; Set initializeOutput = simstate
;; 
;; End Function

;; Public Function defaultRequirementState() As TimeStep_SimState
;; Set defaultRequirementState = defaultSimState(True)
;; End Function








;; 'For right now, we're just going to load csv's into excel tables....
;; 'We assume that the project points us to the paths needed to get at our csvs.
;; 'From there, we load the files into the active tables.
;; 'Then call initialize engine from excel.
;; 'Should be easy enough...

;; Public Sub Initialize_Engine_From_Project(project As TimeStep_Project, _
;;                                             Optional forRequirements As Boolean)
;; 
;; Err.Raise 101, , "Needs updating! "
;; ''
;; ''Project.load 'should pull in all the csv's we need.
;; ''
;; ''parameters.Workstate = Initializing
;; '''Tom Change
;; ''logStatus "Entering Marathon"
;; ''
;; ''profile "ParameterSetup"
;; ''parameters.fromExcel
;; ''profile "ParameterSetup"
;; ''
;; ''profile "PolicySetup"
;; ''policymanager.fromExcel
;; ''profile "PolicySetup"
;; ''
;; ''profile "SupplySetup"
;; ''SupplyManager.fromExcel Worksheets("SupplyAggregate").Range("SupplyStart"), forRequirements
;; ''profile "SupplySetup"
;; ''
;; ''profile "DemandSetup"
;; ''DemandManager.fromExcel Worksheets("DemandRecords").Range("DemandStart")
;; ''profile "DemandSetup"
;; ''
;; ''profile "FillSetup"
;; ''fillmanager.fromExcel
;; ''profile "FillSetup"
;; ''
;; ''profile "OutputSetup"
;; '''TOM Change 7 Jun 2011 -> moved to end to ensure locations are known prior to initialization.
;; ''outputmanager.init ActiveWorkbook.path & "\", , policymanager.locations
;; ''profile "OutputSetup"
;; ''
;; ''If forRequirements Then
;; ''    NoSupplyWarning = True
;; ''    'SupplyManager.UnitsFromDictionary unitrecords
;; ''End If
;; 
;; End Sub


;; 'try to keep pre-calculated policy, parameters, etc. in the system.  we just want to flush out the
;; 'dynamic stuff and pull in more information.
;; 'If requirements is true, we will look for supply in a GeneratedSupply worksheet.
;; Public Sub Reset_Engine_FromExcel(Optional Requirements As Boolean, Optional unitrecords As Dictionary)
;; 
;; 
;; Err.Raise 101, , "Needs updating!"
;; ''truncateTime = Requirements
;; ''
;; ''
;; ''reset 'this will reschedule demands
;; ''
;; ''parameters.Workstate = Initializing
;; '''Tom Change
;; ''logStatus "Resetting Marathon"
;; ''
;; ''If Requirements Then
;; ''    NoSupplyWarning = True
;; ''    If Not (unitrecords Is Nothing) Then
;; ''        SupplyManager.UnitsFromDictionary unitrecords
;; ''    Else
;; ''        'Tom Change 16 April 2012
;; ''        'SupplyManager.UnitsFromSheet "GeneratedSupply"
;; ''        SupplyManager.UnitsFromDictionary getTable(ActiveWorkbook.path & "\GeneratedSupply.csv").toGenericRecords
;; ''    End If
;; ''Else
;; ''    SupplyManager.fromExcel Worksheets("SupplyAggregate").Range("SupplyStart")
;; ''End If
;; ''
;; ''fillmanager.fromExcel
;; 
;; End Sub











;;##Nice to have / Future 


;; 
;; 'To facilitate independent runs, we need a function that can take several source tables, determine
;; 'dependencies between them, and split the tables into N groups of subtables...
;; 
;; 'Note -> the fill manager already does most of this, using tables...
;; 'If we use the fill manager's initialization logic, we bake supply, demand, policy, to get a set of
;; 'dependent data.
;; 'Once the dependencies are known, we separate the data by equivalence classes.
;; 
;; 'computeDependencies does a quick scan of supply, demand, and relations (substitutions) to determine
;; 'which SRCs must be run together.
;; Public Function computeDependencies(supplytbl As GenericTable, _
;;                                     demandtbl As GenericTable, _
;;                                     relationtbl As GenericTable) As Dictionary
;; Dim state As TimeStep_SimState
;; Dim fgraph As TimeStep_FillGraph
;; Dim res As Dictionary
;; 
;; Set fgraph = MarathonOpFill.FillGraphFromTables(New TimeStep_FillGraph, supplytbl, _
;;                                                    demandtbl, _
;;                                                    relationtbl)
;;                                                    
;; Set computeDependencies = MarathonOpFill.findIslands(fgraph.reducedgraph)
;; End Function




;;useful, but never used as I recall...

;; 'If we have a function, f, we can take an initial set of data for supply, demand, and relations,
;; 'and split one humongous run into N smaller runs.  Note, this is pre-cooking the input data,
;; 'not compiling and allocating everything.
;; Public Function splitRunData(Optional supplytbl As GenericTable, _
;;                              Optional demandtbl As GenericTable, _
;;                              Optional relationtbl As GenericTable) As Dictionary
;;                              
;; Dim deps As Dictionary
;; Dim srcs As Dictionary
;; Dim group
;; If nil(supplytbl) Then Set supplytbl = getTable("SupplyRecords")
;; If nil(demandtbl) Then Set demandtbl = getTable("DemandRecords")
;; If nil(relationtbl) Then Set relationtbl = getTable("RelationRecords")
;; Dim datasets As Dictionary
;; Dim filt As RecordFilter
;; Dim dataset As Dictionary
;; Dim tbls As Dictionary
;; Set tbls = newdict("supply", supplytbl, "demand", demandtbl, "relations", relationtbl)
;; 
;; Set deps = computeDependencies(supplytbl, demandtbl, relationtbl)
;; 'For each equivalence class, split the data.
;; Set datasets = New Dictionary
;; For Each group In deps("Dependencies")
;;     'Should be class_1, clas_2, etc.
;;     Set srcs = deps("Dependencies").item(group)
;;     Set filt = dependencyFilter(srcs)
;;     datasets.add group, getDataSet(tbls, filt)
;; Next group
;; 
;; Set splitRunData = datasets
;; Set datasets = Nothing
;;     
;; End Function


;;##Already Have?

;; 'TOM added 5 Nov 2012 -> Returns a filter that will return true for any SRCs in the set of
;; 'dependent SRCs.  Applied to the "SRC" field of a record.  Used to extract subsets of
;; 'dependent data.
;; Public Function dependencyFilter(dependentSRCs As Dictionary) As RecordFilter
;; Dim src
;; Dim vals As Collection
;; Set vals = New Collection
;; For Each src In dependentSRCs
;;     vals.add CStr(src)
;; Next src
;; 
;; Set dependencyFilter = makeRecordFilter(newdict("SRC", vals), orfilter)
;; End Function


;; 'Extract subtables from each tbl where the SRC field is a member of dependencies.
;; 'Results in a map of tables that are subsets of the originals
;; Public Function getDataSet(tables As Dictionary, filt As RecordFilter) As Dictionary
;; Dim nm
;; Dim tbl As GenericTable
;; 
;; Set getDataSet = New Dictionary
;; For Each nm In tables
;;     Set tbl = tables(nm)
;; '    getDataSet.add nm, TableLib.filterRecords(tbl, filt)
;;     getDataSet.add nm, TableLib.selectWhere(tbl, filt)
;; Next nm
;; 
;; End Function

;; 'Tom added 6 Nov 2012
;; 'Serializes the tables in a data set into a directory structure.
;; 'Specifically, splits the supply,demand,relation bits into separate folders...
;; Public Sub spitDataSet(dataset As Dictionary, Optional path As String)
;; If path = vbNullString Then path = ActiveWorkbook.path & "\dataset\"
;; mapToFolders path, dataset, True
;; End Sub
;; 
;; 'Tom added 7 Nov 2012
;; 'Given a dataset of one or more runs.....
;; 'Slurps the dataset into a dictionary, then processes the dictionary from there...
;; Public Function slurpDataSet(Optional path As String) As Dictionary
;; If path = vbNullString Then path = ActiveWorkbook.path & "\dataset\"
;; Set slurpDataSet = folderToMap(path, True)
;; End Function




;;##Likely Obsolete 



;; 'Timestep_simulation from given parameters.
;; 'Right now, we read parameters/demand from Excel Host
;; Public Sub Initialize_Engine_From_Excel(Optional state As TimeStep_SimState, Optional forRequirements As Boolean)
;; 
;; If state Is Nothing Then Set state = makeSimState()
;; With state
;;     'tom change 10 SEP, needed a place to put this, might move it.
;;     Set .behaviormanager = defaultBehaviorManager(state, .behaviormanager)
;;     'moved from class_initialize
;;     .supplystore.SupplyTraffic = True
;;     .demandstore.demandtraffic = True
;;     .policystore.PolicyTraffic = True
;;     .parameters.Workstate = Initializing
;;     'Tom Change
;;     logStatus "Entering Marathon"
;;     
;;     
;;     'profile "ParameterSetup"
;;     'TESTING.
;;     '.parameters.fromExcel
;;     Set .parameters = defaultParameters()
;;     'profile "ParameterSetup"
;;     
;;     'Allow requirements analysis to shorten sim time to only active demands.
;;     If forRequirements Then
;;         .truncateTime = True
;;     End If
;; 
;;     'initialize the policy store
;;     Set .policystore = defaultPolicyStore(.context)
;;     
;;     'Early scoping allows us to identify SRCs that are islands, and identify them.  Additionally,
;;     'if we label them as out of scope, we don't waste time loading them only to scope them out after
;;     'the fact.  Scoping basically sets up a filter for us.
;;     If .earlyscoping Then
;;         'Create a fillstore from supply, demand, policy, parameters - derived from state.
;;         Set .fillstore = defaultFillStore(state)
;;         'scope the simulation state.  Mutation.
;;         Set state = defaultScopedState(state)
;;     End If
;;     
;;     'profile "SupplySetup"
;;     Set .supplystore = defaultSupply(state, forRequirements)
;;     '.supplystore.fromExcel Worksheets("SupplyAggregate").Range("SupplyStart"), forRequirements
;;     'profile "SupplySetup"
;;     
;;     'profile "DemandSetup"
;;     Set .demandstore = defaultDemand(state)
;;     '.demandstore.fromExcel Worksheets("DemandRecords").Range("DemandStart")
;;     'profile "DemandSetup"
;;     
;;     
;;     'TOM Change 26 OCT yanked, output initialization is handled separately now.
;; ''    'profile "OutputSetup"
;; ''    'TOM Change 7 Jun 2011 -> moved to end to ensure locations are known prior to initialization.
;; ''    .outputstore.init ActiveWorkbook.path & "\", , .policystore.locations, forRequirements
;; ''    'profile "OutputSetup"
;;     
;;     If forRequirements Then
;;         .NoSupplyWarning = True
;;         'SupplyManager.UnitsFromDictionary unitrecords
;;     End If
;; End With
;; 
;; End Sub




;; 'A function that derives a simState, relying on defaults where required objects are not provided.
;; 'We allow the custom parts to be defined using a little language.
;; 'Basically, the idea is to allow simple scripting, which can be called from an external file, or
;; 'passed from the repl.
;; 
;; 'Forms in our language...
;; 
;; 'Supply
;; '  InitialUnits
;; 'Demand
;; '  InitialDemand
;; 'Policy
;; '  InitialRelations.
;; 
;; Public Function stateWith(Optional baseState As TimeStep_SimState, Optional customParts As Dictionary) As TimeStep_SimState
;; 
;; Dim prt
;; 
;; If customParts Is Nothing Then
;;    Set stateWith = defaultSimState()
;; Else
;;     Err.Raise 101, , "Not Implemented!"
;; '    For Each prt In customParts
;; '        Select Case CStr(prt)
;; '            Case "Supply"
;; '            Case "Demand"
;; '            Case "Policy"
;; End If
;; 
;; End Function



          
          
         
         
         
      
