(ns marathon.port.distributors)

Option Explicit

'This is a new discrete fill algorithm, where we trade one-time roundoff error for a
'consistent fill order.  We typically have a distribution for each SRC across components.
'One problem with this is how you handle rounding for each component.  In the early version
'of Marathon, they just rounded things out and let the chips fall.  Another way to try is to
'maintain a floating point heap for each component, and "render" down or realize an integer
'quantity as needed.  This allows you to add things piecemeal during requirements analysis,
'while respecting the proportions.  It injects some sensitivy though, and has a bit of a
'disruptive effect on the fill process, i.e. changes in data lead to slight variations in
'fill due to rounding.

'A more consistent approach is to accept the fill rounding error up front, and fill using
'discrete buckets.  This approach takes a distribution, like before, and generates k bins, for
'each component k.  The bins are arranged in sorted order, to ensure that the component with
'the largest distribution (representation) comes first in order.  From there, we multiply each
'value in the distribution by 10, and round off to 1 decimal point.  Finally, we round the
'result to a whole number.  These whole numbers become the quantities, or sizes, of our sorted
'bins.  The bins constitute a consistent fill order that we'll use when generating new units,
'and assigning them components.  Since we always start in the same order, and always fill in the
'same order, regardless of the amount of ghosts generated at each step, we'll always have consistent
'fill, which is insensitive to changes in the data.  There ARE still sensitivities, this time to
'the up-front proportion calculation, but they are made much more explicit.  Case in point,
'some components might drop out if their distribution is low enough (< 0.10).  We can catch this with
'an error to inform the user (or at least visualize it up front).
Public Function getBins(distributions As Dictionary, Optional src As String, Optional allowDrops As Boolean) As GenericRingBin
Dim Key
Dim dvalue As Single
Dim binvalue As Long
If src = vbNullString Then src = "Unknown SRC"
Set getBins = New GenericRingBin
For Each Key In sortByVal(distributions, True)  'returns a sorted list of the keys in the distributions
    dvalue = CSng(distributions(Key))
    If isNormalized(dvalue) Then
        binvalue = decimalRound(dvalue)
        If binvalue = 0 And Not allowDrops Then
            Err.Raise 101, , "Warning at SRC: " & src & "   The distribution value for key " & CStr(Key) & " : " & _
                dvalue & " is less than the rounding threshold, and will Be Dropped!  Check your Data!"
        ElseIf binvalue > 0 Then
            getBins.addBin binvalue, CStr(Key)
        End If
    Else
        Err.Raise 101, , "Distribution value " & dvalue & " is NOT normalized!  Should be between 0 and 1.0!"
    End If
Next Key
    
End Function
'Our one source of rounding error now. Note, values < 0.1 (technically the threshold is at
'0.055, will be rounded to 0.
Public Function decimalRound(invalue As Single) As Long
decimalRound = CLng(Round(invalue * 10#, 1))

End Function
Public Function isGoingtoBeDropped(n As Single) As Boolean
isGoingtoBeDropped = n < 0.1
End Function
Public Function testDistro1() As Dictionary
Set testDistro1 = newdict("AC", 0.1428, "NG", 0.4285, "RC", 0.4285)
End Function
Public Function testDistro2() As Dictionary
Set testDistro2 = newdict("AC", 0.6428, "NG", 0.1285, "RC", 0.2285)
End Function
Public Function problemDistro() As Dictionary
Set problemDistro = newdict("AC", 0.15, "RC", 0.85)
End Function
Public Function distroWithDrops() As Dictionary
Set distroWithDrops = newdict("AC", 0.6428, "NG", 0.0285, "RC", 0.3285)
End Function
Public Function isNormalized(v As Single) As Boolean
isNormalized = v >= 0 And v <= 1
End Function

Public Sub bintest()
Dim bins As GenericRingBin
Set bins = getBins(newdict("AC", 0.66, "RC", 0.2222, "NG", 0.01))
pprint bins.take(10)

pprint getBins(testDistro2).take(9)

pprint getBins(testDistro1).take(9)

End Sub
'Should warn us.
Public Sub dropTest()
pprint getBins(distroWithDrops, "SRC1").take(9)
End Sub
Public Sub problemTst()
Dim bin As GenericRingBin
Set bin = getBins(problemDistro)

pprint bin.take(13)
pprint bin.take(7)
pprint bin.take(2)
End Sub
