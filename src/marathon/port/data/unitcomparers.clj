(ns marathon.port.data.unitcomparers
  (:require [spork.util [general :as gen]]))

0:   2/27/2013 5:13:35 PM
   1:  'TOM HACK 24 july 2012->
   2:  'Putting a bunch of extra logic in here to handle things...
   3:  'A -> policy matters.
   4:  '  For Future Force Generation....
   5:  '  Units in the Mission pool are highest priority to fill demands relative to their demandgroup.
   6:  '       Depending on the study, Mission Pool units may simply be off limits.
   7:  '       Set this by their demand profile.
   8:  '  Units in the Rotational pool are distributed uniformly, relative to their normalized cycle times.
   9:  '       For each unit's cycle, we project it onto a normalized space.
  10:  '           In the case of cycles with infinite length....we just use the dwell time.
  11:  '           In the case of all other policies, we represent the unit's policy coordinate as
  12:  '             the percentage completion of its current policy.
  13:  '             This eliminates differences between AC and RC policies, and uniformly distributes
  14:  '             relative readiness, rather than draining on a pool by pool basis.
  15:  '  Units in the Operational and Sustainment Pool
  16:  '       O&S units have a late deployability.  They are evaluated at 3/5 of their actual cycle completion.
  17:  
  18:  
  19:  'All other policies are identical.  In fact, we leave previous policies intact.  So that we can reproduce
  20:  'older study results.
  21:  
  22:   Option Explicit
  23:   Public Enum UnitCriteria
  24:       followon = 1
  25:       cyclelength = 2
  26:       relativeCyclelength = 3
  27:       bogbudget = 6
  28:       dwell = 5
  29:       AC = 3
  30:       RC = 4
  31:   End Enum
  32:  
  33:   Public UniformPreference As Boolean
  34:  
  35:  'this is a hack, just to get stuff working.
  36:   Public RCpresurgePreference As Boolean
  37:  
  38:   Public sortingCriteria As Collection'Dictates the criteria by which units will be compared.
  39:  
  40:  
  41:   Private demandname As String
  42:   Private followoncode As String
  43:   Private period As String
  44:   Private tags As GenericTags
  45:  
  46:   Implements IComparator


(defn unit-dwell [unit] (-> (:currentcycle unit) :dwell)) 
;;obe
(defn sort-key-ac [unit] 
  (case (:component unit)
     "AC" (unit-dwell unit)
     (/ (unit-dwell unit) 3.0)))

;;Needs to be generalized.  It's a partial application of a bias function.
;;this is really a bias toward "non-AC" units.
(defn sort-key-rc [unit]
  (if (not= (:component unit) "AC")
    (unit-dwell unit)
    (/ (unit-dwell unit) 3.0)))

 104:   Public Sub sortWith(dname As String, fcode As String, period As String, Optional supplytags As GenericTags)
 105:   demandname = dname
 106:   followoncode = fcode
 107:   period = period
 108:   Set tags = supplytags
 109:   End Sub
 110:  
 111:  'A uniform sort key allows us to order units by their relative time in cycle, which smoothes out
 112:  'differences between cycle lengths.

;;Uniform progress in a unit's life cycle.
(defn uniform-sort-key [unit]
  (let [c (:currentcycle unit)]
    (/ (cycletime unit) (:durationexpected c))))

;;figure out how to merge this later. 
(defn opsus-sort-key [unit]  (* (uniform-sort-key unit) (/ 3.0 5.0)))

;;need a defcomparer....we have something like this in util.table, and util.record.
(defmacro defcomparer
  "Defines unit comparison functions.  May be overkill."
  [name key-funcs]
  (let [cs (if (coll? key-funcs) key-funcs [key-funcs])]
    `(let [sc# (~'gen/serial-comparer ~cs)]
       (~'defn ~'name  [~'l ~'r] (sc# l r)))))

;;Uses uniform-compare 
(defcomparer uniform-compare  [uniform-sort-key])
(defcomparer ac-first         [sort-key-ac])
(defcomparer rc-first         [sort-key-rc])
(defcomparer followon-compare [(fn [l r] (if-let [l ]))])

 167:   Private Function FencedCompare(u1 As TimeStep_UnitData, u2 As TimeStep_UnitData) As Comparison
 168:   Dim l As Boolean, r As Boolean
 169:   l = MarathonOpFill.isInsideFence(u1, demandname, followoncode, tags)
 170:   r = MarathonOpFill.isInsideFence(u2, demandname, followoncode, tags)
 171:   If l And r Then
 172:       FencedCompare = equal
 173:   Else
 174:       If l And (Not r) Then
 175:           FencedCompare = greaterthan
 176:       ElseIf (Not l) And r Then
 177:           FencedCompare = lessthan
 178:       End If
 179:   End If
 180:  
 181:   End Function
(defcomparer fenced-compare 
  (fn ))                                    

(defn invert [f] (fn [l r] (f r l))) 

(defcomparer default-compare [fenced-compare followon-compare uniform-compare])

 153:   Private Function followOnCompare(u1 As TimeStep_UnitData, u2 As TimeStep_UnitData) As Comparison
 154:   If followoncode    = vbNullString Then ;This is off.  We used to have state..
 155:      followOnCompare = equal
 156:   Else
 157:       Select Case FollowOnSortKey(u1) - FollowOnSortKey(u2)
 158:           Case Is > 0
 159:               followOnCompare = greaterthan
 160:           Case Is < 0
 161:               followOnCompare = lessthan
 162:           Case Is = 0
 163:               followOnCompare = equal
 164:       End Select
 165:   End If
 166:   End Function







 182:   Private Function IComparator_compare(lhs As Variant, RHS As Variant) As Comparison
 183:   Dim u1 As TimeStep_UnitData
 184:   Dim u2 As TimeStep_UnitData
 185:   Set u1 = lhs
 186:   Set u2 = RHS
 187:  
 188:   Dim res As Comparison
 189:  
 190:   res = FencedCompare(u1, u2)
 191:   If res = equal Then res = followOnCompare(u1, u2)
 192:   If res = equal Then res = uniformCompare(u1, u2)
 193:  
 194:  'TOM Change 24 oct 2012
 195:  'Was not returning res as the value for IComparator_compare, essentially making everything
 196:  'look equal when in fact it wasn't....big bug, caught it early though.
 197:  
 198:   IComparator_compare = res
 199:  
 200:           
 201:   End Function

;TOM Change 27 SEp 2012 -> allow fencing of supply via tags...We pass 
;information to the comparer if the unit is fenced to the relative demand or 
;demand group.
;Public Function isInsideFence(uic As TimeStep_UnitData, demandname As String, 
;        followoncode As String, tags As GenericTags) As Boolean
;
;With tags
;    isInsideFence = .hasTag(uic.name, followoncode) Or 
;                    .hasTag(uic.name, demandname)
;End With
;
;End Function

(defn inside-fence? [uic demandname followoncode tags]
  )

;TOM Change 27 SEp 2012 -> allow fencing of supply via tags...We pass 
;information to the comparer if the unit is fenced to the relative demand or 
;demand group.  If a unit is fenced to a different demand or group, we return
;false.
;Public Function isOutsideFence(uic As TimeStep_UnitData, demandname As String, 
;   followoncode As String, tags As GenericTags) As Boolean
;
;With tags
;    If .hasTag("fenced", uic.name) Then
;        isOutsideFence = Not isInsideFence(uic, demandname, followoncode, tags)
;    Else
;        'by default, units are included if they have no fencing rules.
;        isOutsideFence = False 
;    End If
;End With
;End Function

(defn outside-fence? [uic demandname followoncode tags]
  )

 