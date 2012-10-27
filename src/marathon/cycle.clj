(ns marathon.cycle
  (:use [util.record]))

'The CycleRecord is a simple datastructure used to record unit lifecycles, and lifecycle-specific data.
Option Explicit
Public UICname As String 'associated uic
Public src As String
Public Component As String

Public policyname As String 'associated policy
Public tstart As Single 'cycle start
Public tfinal As Single 'expected cycle end
Public duration As Single 'actual cycle length

Public availableTime As Single 'time spent in available pool
Public deployableTime As Single 'time spent deployable.

Public DurationExpected As Single 'expected cycle length
Public bog As Single 'experienced BOG (units of time, days)
Public bogbudget As Single
Public BOGExpected As Single 'expected BOG (units of time, days)
Public dwell As Single 'expected Dwell (units of time, days)
Public DwellExpected As Single 'experienced dwell
Public MOBexpected As Single 'mobilization time expected, if any.
Public mob As Single

Public deployments As Long 'count of deployments
'Public BDR As Single 'TOM Change 4 Jan 2011 -> calculated online
Public BDRExpected As Single 'expected BOG/Dwell ratio
'Public AvgDwel1 As Long 'Average dwell before deployment in the cycle, calculated
Public Traversals As Collection 'record of state traversal, good for verification


(defrecord CycleRecord [uicname src component policyname tstart tfinal duration 
                  availabletime deployabletime durationexpected bog bogbudget
                  bogexpected dwell dwellexpected mobexpected mob deployments
                  bdrexpected traversals])
(def empty-cycle
  (-> (empty-record Marathon.cycle.CycleRecord)
      (assoc-many 0 [:tstart :tfinal :duration :availableTime 
                     :deployableTime :bdrexpected :bogbudget :bog :bogexpected 
                     :dwell :dwellexpected :mob :mobexpected])
      (assoc-many [] [:traversals])))

Public Function NewCycle(t As Single, bogtime As Long, dwelltime As Long, policyduration As Long, Optional MOBtime As Single) As TimeStep_CycleRecord
Set NewCycle = New TimeStep_CycleRecord
With NewCycle
    .UICname = UICname
    .policyname = policyname 'this is a problem
    .BOGExpected = bogtime
    .bogbudget = bogtime
    .BDRExpected = 1 / ((bogtime + MOBtime) / (policyduration - (bogtime + MOBtime)))
    .DurationExpected = policyduration
    .DwellExpected = dwelltime
    If dwelltime > 1095 And MOBtime = 0 Then Err.Raise 101, , "wierd"
    .MOBexpected = MOBtime
    .tstart = t
    .tfinal = t + duration 'this is expected, may change ....
    .src = src
    .Component = Component
End With

(defn new-cycle 
  ([cyclerec t bogtime dwelltime policyduration mobtime]
    (let [totalbog (+ bogtime mobtime)
          bdr (/ 1 (/ bogtime (- policyduration totalbog)))] 
    (-> cyclerec
      (merge {:bogexpected bogtime :bogbudget bogtime :bdrexpected bdr 
              :durationexpected policyduration :dwellexpected dwelltime 
              :mobexpected mobtime :tstart t :tfinal (+ t policyduration)}))))) 

End Function
'TOM change 2 Sep 2011
'mutate the current cycle object to reflect changes in expectations
Public Function modify(bogtime As Long, dwelltime As Long, policyduration As Long, Optional MOBtime As Single) As TimeStep_CycleRecord
Set modify = Me

With modify
    .BOGExpected = bogtime
    .BDRExpected = 1 / ((bogtime + MOBtime) / (policyduration - (bogtime + MOBtime)))
    .DurationExpected = policyduration
    .DwellExpected = dwelltime
    If .DwellExpected > 1095 And MOBtime = 0 Then Err.Raise 101, , "wierd "
    .MOBexpected = MOBtime
End With

End Function
Public Sub addTraversal(t As Single, startlocation As String, endlocation As String)
    Traversals.add (t & "_" & startlocation & "_" & endlocation)
End Sub
'TOM note -> make sure this is accurate for all compos, differences between MOB, etc .....
'TOM change -> modified this to make it account for available time.
'I want BDR to be a single representative calculation, in that it accounts uniformly for
'BOG/Dwell, MOB/Dwell, and if BOG/MOB is 0, Availalable/Dwell.
Public Function BDR(Optional conventional As Boolean) As Single

If bog > 0 And dwell > 0 Then 'we have bogged.
    BDR = (bog + mob) / dwell
ElseIf dwell > 0 And availableTime > 0 Then
    BDR = availableTime / dwell
Else
    BDR = (MOBexpected + BOGExpected) / (DurationExpected - (MOBexpected + BOGExpected))
End If

If conventional Then BDR = 1 / BDR

End Function

Private Sub Class_Terminate()
Set Traversals = Nothing
End Sub
