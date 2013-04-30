(ns marathon.sim.policyio)

'Data related to the policystore or TimeStep_ManagerOfPolicy, is actually quite disparate.  There are
'multiple data sources that must be read to compose and initialize a policystore that has information on
'simulation periods, rotational policies, and substitution relations.  As such, policyIO has enough code
'to justify breaking it out into a separate module, for organizational sanity.

'The functions in this module concern reading data regarding policies and converting it into internal data
'structures.  Many of the records and other data forms Marathon uses serve as a specification or a list of
'instructions that are intended to be parsed to build policy structures.  The following functions support
'the transformation of records to various policystore related structures, such as relations, atomic
'rotational policies, composite rotational policies, tables of policy records, etc.  Most of these functions
'will be wrapped in higher level constructors, like tablesToPolicyStore, or policyStoreFromExcel, which
'glue together the lower level IO and parsing functions for the end user.  Additional parsing functions,
'such as JSON and Clojure data readers/writers, will also crop up here as needed.
Option Explicit

'(Type    Relation    Donor   Recepient   Cost    Enabled)
Public Function recordToRelation(inrec As GenericRecord) As Collection
Set recordToRelation = fieldvals(inrec, "Relation", "Donor", "Recepient", "Cost")
End Function

'TODO -> implement this bad boy!
'(_   _    _  PolicyName  Template    MaxDwell    MinDwell    MaxBOG  StartDeployable StopDeployable  Overlap     Recovery    BOGBudget   Deltas  _)
Public Function recordToPolicy(inrec As GenericRecord) As TimeStep_Policy
Dim rdict As Dictionary
Dim keylist As Collection
Dim startdeployable As Long, stopdeployable As Long, overlap As Long, recovery As Long, bogbudget As Long, deltas As Dictionary
Dim templatename As String

With inrec
    If .fields("Deltas") <> "{}" Then
        Set deltas = getDictionary(.fields("Deltas")) 'expand the deltas...
    End If
    templatename = CStr(.fields("Template"))
    If templatename = "Ghost" Then
        Set recordToPolicy = RegisterGhostTemplate(CStr(.fields("PolicyName")), CLng(.fields("MaxBOG")), CSng(.fields("Overlap")))
    Else
        Set recordToPolicy = RegisterTemplate(templatename, CLng(.fields("MaxDwell")), CLng(.fields("MinDwell")), _
                                              CLng(.fields("MaxBOG")), CLng(.fields("StartDeployable")), CLng(.fields("StopDeployable")), _
                                              CLng(.fields("Overlap")), deltas, deployableSet(templatename))
    End If
    recordToPolicy.name = CStr(.fields("PolicyName"))
End With

End Function
'Flag for policies that don't need have their deployable ranges set.  Only applies to MaxUtilization.
Public Function deployableSet(templatename As String) As Boolean
Select Case templatename
    Case MarathonEnumsAndConstants.MaxUtilization, MarathonEnumsAndConstants.NearMaxUtilization, MarathonEnumsAndConstants.FFGMission
        deployableSet = True
    Case Else
        deployableSet = False
End Select


End Function

'generate a collection of relations from the table records
Public Function tableToRelations(tbl As GenericTable) As Collection
Dim rec As GenericRecord

Set tableToRelations = New Collection

While Not tbl.EOF
    Set rec = tbl.getGenericRecord
    If rec.fields("Enabled") Then tableToRelations.add recordToRelation(rec)
    tbl.moveNext
Wend

End Function
'generate a collection of atomic policies from the table records
Public Function tableToPolicies(tbl As GenericTable) As Collection
Dim rec As GenericRecord

Set tableToPolicies = New Collection

While Not tbl.EOF
    Set rec = tbl.getGenericRecord
    tableToPolicies.add recordToPolicy(rec)
    tbl.moveNext
Wend

End Function
'generate a dictionary of atomic policies from a table, where the keys are
'policy names.  Enforces unique policy names.
Public Function tableToPolicyDict(tbl As GenericTable) As Dictionary
Dim rec As GenericRecord
Dim p As TimeStep_Policy

Set tableToPolicyDict = New Dictionary

While Not tbl.EOF
    Set rec = tbl.getGenericRecord
    If Not tableToPolicyDict.exists(rec.fields("PolicyName")) Then
        Set p = recordToPolicy(rec)
        tableToPolicyDict.add p.name, p
    End If
    tbl.moveNext
Wend

End Function


'Reads an expression from a record
'with keys (CompositeName Policy), vals [somestring, {policy dictionary}/or [policy list]]
'We use our evaluator to transform the policy string into a policy dictionary.
'Returns a pair of [rulename, {policy dict}], or [rulename [policy sequence]]
Public Function recordToComposition(inrec As GenericRecord) As Collection
Dim rulename As String
rulename = inrec.fields("CompositeName")
Set recordToComposition = list(rulename, reval(CStr(inrec.fields("Composition"))))
End Function
'Generates a dictionary of composition rules from the table.
'Keys in the dictionary correspond to the name of the rule, and vals correspond to
'a map of period names to policy names/ policies.
Public Function tableToCompositions(tbl As GenericTable) As Dictionary
Dim rec As GenericRecord
Dim rulename As String
Dim entry As Collection
Dim composition As Dictionary
Dim policysequence As Collection
Dim res As Collection
Dim tmp()
Dim subpolicies
Dim itm

Set tableToCompositions = New Dictionary

While Not tbl.EOF
    Set rec = tbl.getGenericRecord
    If rec.fields.exists("Composition") Then 'this is a table of literal compositions, rather than flat recs.
        Set res = recordToComposition(rec)
        rulename = res(1) 'the name of sequential policy.
        If tableToCompositions.exists(rulename) Then
            Err.Raise 101, , "Composition for " & rulename & "Already exists!"
        Else
            Select Case TypeName(res(2))
                Case "Collection"  'sequential policy
                    Set policysequence = res(2) 'a list of policy names.
                    tableToCompositions.add rulename, policysequence 'add the sequential policy as a valid composition rule.
                Case "Variant()"
                    Set policysequence = New Collection
                    tmp = res(2)
                    For Each itm In tmp
                        policysequence.add itm
                    Next itm
                    tableToCompositions.add rulename, policysequence 'add the sequential policy as a valid composition rule.
                Case "Dictionary"
                    Set composition = res(2)
                    tableToCompositions.add rulename, composition
                Case Else
                    Err.Raise 101, , "Unknown composite policy type!"
            End Select
        End If
    Else
        rulename = rec.fields("CompositeName")
        Select Case rec.fields("CompositionType")
            Case "Periodic"
                If tableToCompositions.exists(rulename) Then
                    Set composition = tableToCompositions.item(rulename)
                Else
                    Set composition = New Dictionary
                    tableToCompositions.add rulename, composition
                End If
                composition.add rec.fields("Period"), rec.fields("Policy")
            Case "Sequential"
                If tableToCompositions.exists(rulename) Then
                    Err.Raise 101, , "Composition " & rulename & " already exists!"
                Else
                    subpolicies = Split(CStr(rec.fields("Policy")), ",")
                    Set policysequence = New Collection
                    For Each itm In subpolicies
                        policysequence.add Trim(CStr(itm))
                    Next itm
                    tableToCompositions.add rulename, policysequence
                End If
            Case Else
                Err.Raise 101, , "Error parsing composition policy!" & rulename
            End Select
    End If
    tbl.moveNext
Wend

End Function
'Public Function compositionType(incomp As Dictionary) As PolicyType
'Select Case TypeName(incomp(2))
'End Function

'much more robust, uses the generictable interface to simplify loading.
Public Function tablesToPolicyStore(relationtable As GenericTable, periodtable As GenericTable, atomictable As GenericTable, compositeTable As GenericTable) As TimeStep_ManagerOfPolicy
Dim ps As TimeStep_ManagerOfPolicy

Set ps = New TimeStep_ManagerOfPolicy

addRelations tableToRelations(relationtable), ps
addPeriods PeriodLib.tableToPeriods(periodtable), ps
addDependentPolicies tableToPolicyDict(atomictable), tableToCompositions(compositeTable), ps

Set tablesToPolicyStore = ps
Set ps = Nothing

End Function


'Returns a policystore object initialized from default tables in Excel.  Mostly for compatibility.
Public Function policyStoreFromExcel() As TimeStep_ManagerOfPolicy
Set policyStoreFromExcel = tablesToPolicyStore(getTable("RelationRecords"), _
                             getTable("PeriodRecords"), _
                             getTable("PolicyRecords"), _
                             getTable("CompositePolicyRecords"))
End Function
Public Function policyStoreFromFiles() As TimeStep_ManagerOfPolicy
Dim tbl
Dim tblist As Collection
Dim p As String
p = ActiveWorkbook.path & "\"

Set tblist = list("RelationRecords", "PeriodRecords", "PolicyRecords", "CompositePolicyRecords")
For Each tbl In tblist
    TableLib.saveTable getTable(CStr(tbl)), p & "\" & CStr(tbl) & ".json"
Next tbl
                             
Set policyStoreFromFiles = tablesToPolicyStore(getTable(p & "RelationRecords" & ".json"), _
                             getTable(p & "PeriodRecords" & ".json"), _
                             getTable(p & "PolicyRecords" & ".json"), _
                             getTable(p & "CompositePolicyRecords" & ".json"))

End Function
Public Function folderToPolicyStore(folderpath As String) As TimeStep_ManagerOfPolicy
Dim tbl
Dim tblist As Collection
Dim p As String
p = folderpath

Set tblist = list("RelationRecords", "PeriodRecords", "PolicyRecords", "CompositePolicyRecords")
Set folderToPolicyStore = tablesToPolicyStore(getTable(p & "RelationRecords" & ".json"), _
                             getTable(p & "PeriodRecords" & ".json"), _
                             getTable(p & "PolicyRecords" & ".json"), _
                             getTable(p & "CompositePolicyRecords" & ".json"))
                             
End Function

