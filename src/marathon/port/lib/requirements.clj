(ns marathon.port.requirements)

Option Explicit

Public Sub interactive()
Dim reqbot As Dynamic_RunManager
Set reqbot = New Dynamic_RunManager

End Sub
Public Sub fastRequirementsTest()
Dim req As Dynamic_Requirements
Dim genrec As GenericRecord


Set req = New Dynamic_Requirements

'req.IterativeConvergence
req.FastConvergence
Set req = Nothing

End Sub
Public Sub fastLoggedRequirementsTest()
Dim req As Dynamic_Requirements
Dim genrec As GenericRecord


Set req = New Dynamic_Requirements

req.FastConvergence True

End Sub

Public Sub requirementInterface()

RequirementsUI.show

End Sub
Public Sub stresstest()
Dim runmanager As Dynamic_RunManager
'Set runmanager = Nothing
Set runmanager = New Dynamic_RunManager
'link runmanager
RequirementsUI.show
RequirementsUI.link runmanager
   
runmanager.stresstest 160
    
End Sub
Public Sub reorgtst(Optional dtype As DistributorType)
Dim runmanager As Dynamic_RunManager
'Set runmanager = Nothing
Set runmanager = New Dynamic_RunManager
'link runmanager
RequirementsUI.show
RequirementsUI.link runmanager
runmanager.ReorgTest dtype
End Sub

'Public Sub slowRequirementsTest()
'Dim req As Dynamic_Requirements
'Dim genrec As GenericRecord
'
'
'Set req = New Dynamic_Requirements
'
'req.IterativeConvergence
'
'End Sub
Public Sub loggedRequirements()
Dim req As Dynamic_Requirements
Dim genrec As GenericRecord

Set req = New Dynamic_Requirements

req.IterativeConvergence True
End Sub

Public Sub chartedRequirements()
'RequirementsTest StandardCharts.DeployedPopulation, StandardCharts.DeploymentDotPlot, StandardCharts.RequirementOverTime, StandardCharts.SandChart
End Sub

Public Sub BinaryConvergence()
Dim req As Dynamic_Requirements

Set req = New Dynamic_Requirements
req.BisectionConvergence

End Sub

'Public Sub InteractiveRequirements()
'Dim req As Dynamic_Requirements
'Dim sim As TimeStep_Engine
'
'
'Set sim = New TimeStep_Engine
'sim.InteractiveSimulation
'
'Set req = New Dynamic_Requirements
'
'req.IterativeConvergence sim
'
'
'
'End Sub

