(ns marathon.port.data.supplystore)

Option Explicit
Public name As String
'Decouple
Public parent As TimeStep_Engine
'TOM Change 7 Dec 2010
'Dictionary to capture the unique set of SRCs that are in scope.
'This is initialized and maintained in Storage.Uniques, then utilized if ScopeToSupply is active.
Public SRCsinscope As Dictionary
'TOM Change
Public DeployableBuckets As Dictionary 'this is the set of all supply that are available.
                                 'as deployable supply is updated, connections to the graph are added.
                                 
'TOM Change 21 Sep 2011
Public followonbuckets As Dictionary 'this is the set of all units that are eligible for follow-ons, grouped by
'Demand Group.  Each key points to a set of keys, similar to DeployableBuckets ->
    'let followonbuckets = #{:CC3 #{SOURCE_SRC1, #{UIC1}}
'                            :MCO1 #{SOURCE_SRC2, #{UIC3, UIC10}
'                                    SOURCE_SRC3, #{UIC5, UIC29}}

'When filling demands, if there are eligible follow-on units, we exhaust them FIRST.
    'This means redirecting the initial fill for a demand toward the followonbucket dictionary...
    'Most of the fill logic is identical.  We're just redirecting the source of fill to a different set of units.
        'During this follow-on specific pass, we exhaust the follow-on units first.
        'Since follow-on units can only be used within their demandgroup, and in contiguous demands, they form
        'an independent fill performed outside of the normal, AFORGEN-driven fills.
                        
'We partition these separately to ensure that follow-ons are exhausted first, prior to fill via
'normal supply.

'TOM Change
'TOM change 18 DEC -> UnitMap is going to replace aUnitData
Public unitmap As Dictionary 'KVP mapping of unit names to indices in aUnitData
Public unitindex As Dictionary 'KVP mapping of unit indices to unit names
'the other Events unit data layer could be used to record events of interest that happen to units
'throughout the simulation
'TOM Change 18 DEC 2010
Public Forensics As Boolean
Public SupplyTraffic As Boolean
Public UnitBehaviors As Dictionary
'dictionary to catalog dates when individual units change state...this is to maximize effeciency.
'we're stepping away from updating the whole system at once.
Public UnitUpdates As GenericTags
Public Requests As Dictionary
'Tom change 15 Mar 2011
Public tags As GenericTags
Public todayupdates As Dictionary
Public profiling As GenericProfiler
Public Verbose As Boolean
Public hasGhosts As Boolean
Public followOns As Dictionary

Implements IVolatile

Private Sub Class_Initialize()
Dim behavior As IUnitBehavior

Set SRCsinscope = New Dictionary

Set unitmap = New Dictionary
Set unitindex = New Dictionary
'Purpose of this collection is to capture deployers when they become available. In an event driven
'version, we'd call UICs in the "deployable" state members of this collection. Again, it's not
'a priority queue (right now), but it is a way to paritition our search space and DRASTICALLY reduce
'the amount of work we have to keep doing every day.
'Tom Change 18 DEC 2010 -> Moved to Supply Manager Initialization
Set DeployableBuckets = New Dictionary
Set followonbuckets = New Dictionary

Set UnitBehaviors = New Dictionary
Set behavior = New TimeStep_UnitBehaviorBase
behavior.name = "Legacy"

UnitBehaviors.add "Default", behavior
UnitBehaviors.add "Legacy", behavior
''Decouple
'Set behavior.parent = Me

Set behavior = Nothing
Set tags = New GenericTags
tags.addTag "Sources"

name = "SupplyManager"
Set UnitUpdates = New GenericTags
Set Requests = New Dictionary

Set followOns = New Dictionary

End Sub


Private Sub Class_Terminate()
Set unitmap = Nothing

Set unitindex = Nothing

Set tags = Nothing

Set UnitUpdates = Nothing

Set Requests = Nothing
Set parent = Nothing

Set SRCsinscope = Nothing
Set UnitBehaviors = Nothing

Set todayupdates = Nothing
Set profiling = Nothing

End Sub

Private Sub IVolatile_Reset()

Set unitmap = Nothing
Set unitmap = New Dictionary

Set unitindex = Nothing
Set unitindex = New Dictionary

Set DeployableBuckets = Nothing
Set DeployableBuckets = New Dictionary

Set followonbuckets = Nothing
Set followonbuckets = New Dictionary

Set tags = Nothing

Set tags = New GenericTags
tags.addTag "Sources"

Set UnitUpdates = Nothing
Set UnitUpdates = New GenericTags

Set Requests = Nothing
Set Requests = New Dictionary

hasGhosts = False
Set todayupdates = Nothing
End Sub

Private Sub IVolatile_Terminate()
Set unitmap = Nothing

Set unitindex = Nothing


Set tags = Nothing

Set UnitUpdates = Nothing

Set Requests = Nothing
Set parent = Nothing

Set SRCsinscope = Nothing
Set UnitBehaviors = Nothing

Set todayupdates = Nothing
Set profiling = Nothing

Set DeployableBuckets = Nothing
Set followonbuckets = Nothing

End Sub
