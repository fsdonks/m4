(ns marathon.port.projects)

Option Explicit

Sub pjtst()
Dim pj As MarathonProject

Set pj = New MarathonProject

End Sub


Sub pickletst()

Dim pj As MarathonProject
Dim pj2 As MarathonProject
Set pj = New MarathonProject

pj.setDefaults
pj.addPath "Tom", "C:\Tom"

Set pj2 = unpickle(New MarathonProject, pickle(pj))
quickview pj2
End Sub
'create a blank marathon project
Public Function blankProject() As MarathonProject
Set blankProject = New MarathonProject
End Function

Public Function defaultProject() As MarathonProject
Set defaultProject = blankProject()
defaultProject.setDefaults
End Function
'wipes ALL output directores in the archive path.
'if directories do not exist, creates them.
'installs default files for running Marathon (i.e. policies)
Public Sub buildDirectories(inproject As MarathonProject)
End Sub
'dump all of our working tables into a project
Public Sub saveworkingTables(Optional path As String)

Dim p As MarathonProject
Set p = New MarathonProject
p.setDefaults

If path = vbNullString Then path = getPath
With p
    dumpSheet .demands, path & "savetest\demands.json"
End With
       
End Sub
Private Sub dumpSheet(name As String, path As String)
copyTable name, path
End Sub
Public Sub saveOutputTables(Optional path As String)

End Sub
