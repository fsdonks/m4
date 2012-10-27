(ns marathon.port.constants)

Option Explicit

Public Enum BehaviorResponse
    active
    InActive
    Failed
    completed
End Enum

Public Enum StandardCharts
    DeploymentDotPlot
    SandChart
    DeployedPopulation
    RequirementOverTime
End Enum


Public Const UnGrouped As String = "UnGrouped"
Public Const AC12 As String = "AC12" 'Represents an AC 1:2 BOG/Dwell policy, r182 -> tr183 -> rdy365 -> av365
Public Const AC13 As String = "AC13" 'Represents an AC 1:3 BOG/Dwell policy, r182 -> tr183 -> rdy460 -> av270
Public Const RC14 As String = "RC14" 'Represents an RC 1:4 MOB/Dwell policy, r365 -> tr365 -> rdy1 365 -> rdy2365 -> av365
Public Const RC15 As String = "RC15" 'Represents an RC 1:5 MOB/Dwell policy, rst365 ->tr1 365 ->tr2 365 ->rdy1 365->rdy2 365->av 365
Public Const AC11 As String = "AC11"
Public Const RC11 As String = "RC11"
Public Const RC12 As String = "RC12"
Public Const RC14Remob As String = "RC14ReMob"

Public Const Bogging As String = "Bogging"
Public Const Dwelling As String = "Dwelling"


Public Const reset As String = "Reset"
Public Const train As String = "Train"
Public Const ready As String = "Ready"
Public Const available As String = "Available"
Public Const deployed As String = "Deployed"
Public Const Overlapping As String = "Overlapping"
Public Const mobilization As String = "Mobilization"
Public Const demobilization As String = "DeMobilization"
Public Const recovery As String = "Recovery"
Public Const Recovered As String = "Recovered"


Public Const BogDeployable = "BoggingDeployable"
Public Const DwellDeployable = "DwellingDeployable"
Public Const Deployable = "Deployable"


Public Const GhostPermanent12 = "GhostPermanent12"
Public Const GhostPermanent13 = "GhostPermanent13"
Public Const GhostTransient12 = "GhostTransient12"
Public Const GhostTransient13 = "GhostTransient13"

Public Const Recovering As String = "Recovering"


'Private Const reset = "Reset"
'Private Const train = "Train"
'Private Const ready = "Ready"
'Private Const available = "Available"
'Private Const deployed = "Deployed"
'Private Const Overlapping = "Overlapping"
'TODO -> These constants are copied into the policy object,
'solution.
'Private Const AC12 = "AC12" 'Represents an AC 1:2 BOG/Dwell
'Private Const AC13 = "AC13" 'Represents an AC 1:3 BOG/Dwell
''not a good thing. I need to fix this with
'Private Const RC14 = "RC14" 'Represents an RC 1:4 MOB/Dwell policy, r365 -> tr365 -> rdyl 365 -> rdy2365 -> av365
'Private Const RC15 = "RC15" 'Represents
'Private Const AC11 = "AC11"
'Private Const RC11 = "RC11"
'Private Const RC12 = "RC12"
'Tom Change 3 Mar 2011
'Substitutions are going to be managed as a special object...
'These are general states used to describe information about the location.
'We can actually store all kinds of details about the location in the actual
'Graph node's data. Since the structure contains variants, we could have more
'useful, descriptive
'I opted to change these to string constants, since they're more declarative and require no conversion
'These are our unit's states.
'We can add more, or even parse custom states as needed.
'Private Const Bogging = "Bogging"
'Private Const Dwelling = "Dwelling"
