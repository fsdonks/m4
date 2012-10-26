(ns marathon.port.data.policystore)

Option Explicit
'This is for centralizing control over rotational policy, as well as substition policy.
'We maintain all the data structures necessary for managing this stuff.
    'Also manage all feasible locations through this object.
Public name As String
Public LocatiOnMap As Dictionary 'kvp mapping of location names to indices ... ?

'TOM Change 7 June 2011
'public positions As Dictionary 'set of all known positions. <couldn't find any refernece to this.
Public locations As Dictionary  'set of all known locations, superset of positions.

Public LocationIndex As Dictionary 'kvp mapping of location indices to names
Public policies As Dictionary 'Kvp mapping of policy names to policy objects
Public periods As Dictionary 'When we process policy, we schedule the changing of
                             'periods as well.


Private nextlocationID As Long

Public Highest As Dictionary
Public PolicyTraffic As Boolean
Public rules As Dictionary
Public rulegraph As GenericGraph
Public ptr As Dictionary
Public ruleDelim As String
Private msg As String
Private Const SubSymbol = "{>"
Private Const EquivSymbol = "="

'enumerated type to define our universe of rules.  We can always add more (and I intend to....)
Public Enum RuleType
    Equivalence
    Substitution
End Enum

Public activePeriod As GenericPeriod
Public periodchanges As Dictionary
Public memoized As Dictionary

'TOM Change 7 jun 2011
Private todayupdates As Dictionary
Private packet As TimeStep_UpdatePacket
Private policySchedule As GenericGraph

'TOM Change 12 July 2011
Public schedules As Dictionary
Public composites As Dictionary 'table of composite policies.
Public canGhost As Boolean

Public permanents As Dictionary 'set of permanent policies

Implements IVolatile


Private Sub Class_Initialize()
Dim ptr As Dictionary
Dim per As GenericPeriod

Set LocationIndex = New Dictionary
Set LocatiOnMap = New Dictionary
Set ptr = Nothing

nextlocationID = 101 'Start labeling locations at 101

Set policies = New Dictionary
name = "PolicyManager"
Set rules = New Dictionary
Set ptr = New Dictionary
rules.add "Substitutions", ptr
Set ptr = New Dictionary
rules.add "Equivalencies", ptr
Set ptr = Nothing
ruleDelim = "|"

Set periods = New Dictionary
Set per = New GenericPeriod
Set memoized = New Dictionary

'TOM Change 7 Jun 2011
'Set positions = New Dictionary
Set locations = New Dictionary

Set activePeriod = New GenericPeriod
activePeriod.Default 0, , "Initialization"

Set policySchedule = New GenericGraph
Set schedules = New Dictionary
Set permanents = New Dictionary
Set memoized = New Dictionary
Set composites = New Dictionary

End Sub


Private Sub Class_Terminate()

Set LocationIndex = Nothing
Set LocatiOnMap = Nothing

Set policies = Nothing

Set rules = Nothing

Set periods = Nothing
Set memoized = Nothing

'TOM Change 7 Jun 2011
'Set positions = Nothing
Set locations = Nothing

Set activePeriod = Nothing

Set policySchedule = Nothing
Set schedules = Nothing
Set permanents = Nothing
Set composites = Nothing
End Sub

Private Sub IVolatile_Reset()
'Set activePeriod = New GenericPeriod
'activePeriod.Default 0, , "Initialization"
'resetPolicies
 Dim ptr As Dictionary
Dim per As GenericPeriod
Dim tbl As GenericTable

Set ptr = Nothing


memoized.Clear

Set activePeriod = New GenericPeriod
activePeriod.Default 0, , "Initialization"

Set schedules = New Dictionary

'TODO Check these....
'resetPolicies
'resetPeriods

'TODO Check these....
'Set tbl = New GenericTable
'tbl.FromSheet Worksheets("CompositePolicyRecords")
'getCompositePolicies tbl

End Sub

Private Sub IVolatile_Terminate()
Class_Terminate

End Sub


