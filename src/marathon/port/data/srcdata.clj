(ns marathon.port.data.srcdata)

'Megan Change: 14 Jul 2011
'This object template will create storage space for SRC level data.  When directed by TimeStep_OutputSummary, it
'will calculate average BOG:Dwell and percentage of missed or filled demand.

Option Explicit

Public name As String
Public dwell As Single
Public bog As Single
Public FilledDemand As Long
Public UnfilledDemand As Long
Public totdwell As Single
Public totBOG As Single
Public totFilledDemand As Long
Public totUnfilledDemand As Long

Private Sub Class_Initialize()

dwell = 0
bog = 0
FilledDemand = 0
UnfilledDemand = 0
totdwell = 0
totBOG = 0
totFilledDemand = 0
totUnfilledDemand = 0

End Sub

Public Sub AddDwell(MoreDwell As Single)

dwell = dwell + MoreDwell

End Sub

Public Sub AddBOG(MoreBOG As Single)

bog = bog + MoreBOG

End Sub

Public Sub AddFilledDemand(MoreFilledDemand As Long)

FilledDemand = FilledDemand + MoreFilledDemand

End Sub

Public Sub AddUnfilledDemand(MoreUnfilledDemand As Long)

UnfilledDemand = UnfilledDemand + MoreUnfilledDemand

End Sub

Public Function PeriodCalculations() As Variant

Dim answers As Variant
ReDim answers(1 To 3)

'TOM Change 10 Aug
If bog > 0 Then
    answers(1) = dwell / bog
Else
    'Err.Raise 101, , "BOG cannot be 0, check data or handle as exception"
    answers(1) = 0
End If

answers(2) = 0 'FilledDemand / (FilledDemand + UnfilledDemand)
answers(3) = 0 '1 - answers(2)

totdwell = totdwell + dwell
totBOG = totBOG + bog
totFilledDemand = totFilledDemand + FilledDemand
totUnfilledDemand = totUnfilledDemand + UnfilledDemand
dwell = 0
bog = 0
FilledDemand = 0
UnfilledDemand = 0

PeriodCalculations = answers

End Function

Public Function TotalCalculations() As Variant

Dim answers As Variant
ReDim answers(1 To 3)

'TOM Change 9 Aug
If totBOG > 0 Then
    answers(1) = totdwell / totBOG
Else
    'answers(1) = 0
    Err.Raise 101, , "BOG cannot be 0, check data or handle as exception"
End If
answers(2) = 0 'totFilledDemand / (totFilledDemand + totUnfilledDemand)
answers(3) = 0 '1 - answers(2)

TotalCalculations = answers

End Function
