;;Source data for SRM modules, policies, and color.
(ns marathon.sampledata.srm)

(def palette 
  '({:Color "DarkYellow", :R 255, :G 255, :B 0}
    {:Color "LightYellow", :R 255, :G 255, :B 153}
    {:Color "DarkGreen", :R 84, :G 130, :B 53}
    {:Color "LightGreen", :R 146, :G 208, :B 80}
    {:Color "DarkBlue", :R 0, :G 0, :B 255}
    {:Color "LightBlue", :R 0, :G 255, :B 255}))

(def nodes 
  '({:Module "Prepare",
     :Base "Yellow",
     :Intensity "Dark",
     :Color "DarkYellow",
     :Label "Building Readiness",
     :BrevityCode "PB",
     :Action "DA",
     :Rating "C3",
     :ReadinessClass "C3/C4",
     :Name "PB_C3",
     :Personnel "P2",
     :Equipment "S2",
     :EndStrengthPercentage 80,
     :Notes nil}
    {:Module "Prepare",
     :Base "Yellow",
     :Intensity "Dark",
     :Color "DarkYellow",
     :Label "Building Readiness",
     :BrevityCode "PB",
     :Action "DA",
     :Rating "C4",
     :ReadinessClass "C3/C4",
     :Name "PB_C4",
     :Personnel "P2",
     :Equipment "S2",
     :EndStrengthPercentage 80,
     :Notes nil}
    {:Module "Prepare",
     :Base "Yellow",
     :Intensity "Light",
     :Color "LightYellow",
     :Label "Transitory State",
     :BrevityCode "PT",
     :Action "DA",
     :Rating "C4",
     :ReadinessClass "C3/C4",
     :Name "PT_C4",
     :Personnel "variable",
     :Equipment "variable",
     :EndStrengthPercentage 10,
     :Notes nil}
    {:Module "Prepare",
     :Base "Yellow",
     :Intensity "Light",
     :Color "LightYellow",
     :Label "Limited Resources",
     :BrevityCode "PL",
     :Action "DA",
     :Rating "C4",
     :ReadinessClass "C3/C4",
     :Name "PL_C4",
     :Personnel "variable",
     :Equipment "variable",
     :EndStrengthPercentage 10,
     :Notes nil}
    {:Module "Ready",
     :Base "Green",
     :Intensity "Dark",
     :Color "DarkGreen",
     :Label "Ready",
     :BrevityCode "R",
     :Action "DA",
     :Rating "C1",
     :ReadinessClass "C1/C2",
     :Name "R_C1",
     :Personnel "P1",
     :Equipment "S1",
     :EndStrengthPercentage 90,
     :Notes nil}
    {:Module "Ready",
     :Base "Green",
     :Intensity "Light",
     :Color "LightGreen",
     :Label "Ready",
     :BrevityCode "R",
     :Action "DA",
     :Rating "C2",
     :ReadinessClass "C1/C2",
     :Name "R_C2",
     :Personnel "P1",
     :Equipment "S1",
     :EndStrengthPercentage 90,
     :Notes nil}
    {:Module "Mission",
     :Base "Blue",
     :Intensity "Dark",
     :Color "DarkBlue",
     :Label "Decisive Action - Prepare To Deploy Order",
     :BrevityCode "MP",
     :Action "DA",
     :Rating "C1",
     :ReadinessClass "C1/C2",
     :Name "MP_DA_C1",
     :Personnel "P1",
     :Equipment "S1",
     :EndStrengthPercentage 100,
     :Notes nil}
    {:Module "Mission",
     :Base "Blue",
     :Intensity "Light",
     :Color "LightBlue",
     :Label "NonDecisive Action - Prepare To Deploy Order",
     :BrevityCode "MP",
     :Action "A1",
     :Rating "C3",
     :ReadinessClass "C3/C4",
     :Name "MP_NDA_C3",
     :Personnel "P1",
     :Equipment "variable",
     :EndStrengthPercentage 100,
     :Notes nil}
    {:Module "Mission",
     :Base "Blue",
     :Intensity "Dark",
     :Color "DarkBlue",
     :Label "Decisive Action - Mission Allocated",
     :BrevityCode "MA",
     :Action "DA",
     :Rating "C1",
     :ReadinessClass "C1/C2",
     :Name "MA_DA_C1",
     :Personnel "P1",
     :Equipment "S1",
     :EndStrengthPercentage 100,
     :Notes nil}
    {:Module "Mission",
     :Base "Blue",
     :Intensity "Dark",
     :Color "DarkBlue",
     :Label "Decisive Action - Mission Allocated",
     :BrevityCode "MA",
     :Action "DA",
     :Rating "C2",
     :ReadinessClass "C1/C2",
     :Name "MA_DA_C2",
     :Personnel "P1",
     :Equipment "S1",
     :EndStrengthPercentage 100,
     :Notes nil}
    {:Module "Mission",
     :Base "Blue",
     :Intensity "Light",
     :Color "LightBlue",
     :Label "NonDecisive Action - Mission Allocated",
     :BrevityCode "MA",
     :Action "A1",
     :Rating "C3",
     :ReadinessClass "C3/C4",
     :Name "MA_NDA_C3",
     :Personnel "P1",
     :Equipment "variable",
     :EndStrengthPercentage 100,
     :Notes nil}
    {:Module "Mission",
     :Base "Blue",
     :Intensity "Dark",
     :Color "DarkBlue",
     :Label "Decisive Action - Assigned Forces",
     :BrevityCode "MD",
     :Action "DA",
     :Rating "C1",
     :ReadinessClass "C1/C2",
     :Name "MD_DA_C1",
     :Personnel "P1",
     :Equipment "S1",
     :EndStrengthPercentage 100,
     :Notes nil}
    {:Module "Mission",
     :Base "Blue",
     :Intensity "Dark",
     :Color "DarkBlue",
     :Label "Decisive Action - Assigned Forces",
     :BrevityCode "MD",
     :Action "DA",
     :Rating "C2",
     :ReadinessClass "C1/C2",
     :Name "MD_DA_C2",
     :Personnel "P1",
     :Equipment "S1",
     :EndStrengthPercentage 100,
     :Notes nil}
    {:Module "Mission",
     :Base "Blue",
     :Intensity "Light",
     :Color "LightBlue",
     :Label "NonDecisive Action - Assigned Forces",
     :BrevityCode "MD",
     :Action "A1",
     :Rating "C3",
     :ReadinessClass "C3/C4",
     :Name "MD_NDA_C3",
     :Personnel "P1",
     :Equipment "variable",
     :EndStrengthPercentage 100,
     :Notes "Possible?"}))

;;Routing information as of 7 May 2016....these rules may (probably will)
;;change...
;;Note: we can probably define routing information for all policies and
;;just kludge them together.  These basically form a positiongraph
;;for a policy; we just interpret the policy differently.
(def routing
  '({:Policy "SRM-AC", :From "PT_C4", :To "PB_C3", :Duration 90}
    {:Policy "SRM-AC", :From "PB_C3", :To "R_C2", :Duration 55}
    {:Policy "SRM-AC", :From "R_C2", :To "R_C1", :Duration 550}
    {:Policy "SRM-AC", :From "R_C1", :To "PB_C3", :Duration 35}
    {:Policy "SRM-AC", :From "MA_DA_C1", :To "PB_C3", :Duration 90}
    {:Policy "SRM-AC", :From "MA_DA_C2", :To "PB_C3", :Duration 90}
    {:Policy "SRM-AC", :From "MP_DA_C1", :To "R_C1", :Duration 999999}
    {:Policy "SRM-AC", :From "MA_NDA_C3", :To "PT_C4", :Duration 90}
    {:Policy "SRM-AC", :From "MD_NDA_C3", :To "PT_C4", :Duration 90}
    {:Policy "SRM-AC", :From "MP_NDA_C3", :To "PT_C4", :Duration 90}
    {:Policy "SRM-AC", :From "PB_C3", :To "MA_NDA_C3", :Duration 90}
    {:Policy "SRM-AC", :From "PB_C3", :To "MD_NDA_C3", :Duration 90}
    {:Policy "SRM-AC", :From "PB_C3", :To "MP_NDA_C3", :Duration 90}
    {:Policy "SRM-RC", :From "PT_C4", :To "PB_C4", :Duration 730}
    {:Policy "SRM-RC", :From "PB_C4", :To "PB_C3", :Duration 365}
    {:Policy "SRM-RC", :From "PB_C3", :To "R_C2", :Duration 365}
    {:Policy "SRM-RC", :From "R_C2", :To "PT_C4", :Duration 365}
    {:Policy "SRM-RC", :From "MA_DA_C1", :To "PT_C4", :Duration 90}
    {:Policy "SRM-RC", :From "MA_DA_C2", :To "PT_C4", :Duration 90}
    {:Policy "SRM-RC", :From "MA_NDA_C3", :To "PT_C4", :Duration 90}
    {:Policy "SRM-RC", :From "MD_NDA_C3", :To "PT_C4", :Duration 90}
    {:Policy "SRM-RC", :From "MP_NDA_C3", :To "PT_C4", :Duration 90}
    {:Policy "SRM-RC", :From "PB_C3", :To "MA_NDA_C3", :Duration 90}
    {:Policy "SRM-RC", :From "PB_C3", :To "MD_NDA_C3", :Duration 90}
    {:Policy "SRM-RC", :From "PB_C3", :To "MP_NDA_C3", :Duration 90}
    {:Policy "SRM-RCDivHQ", :From "PB_C4", :To "PB_C3", :Duration 730}
    {:Policy "SRM-RCDivHQ", :From "PB_C3", :To "R_C2", :Duration 365}
    {:Policy "SRM-RCDivHQ", :From "R_C2", :To "PB_C4", :Duration 365}
    {:Policy "SRM-RCDivHQ", :From "MA_DA_C1", :To "PB_C4", :Duration 90}
    {:Policy "SRM-RCDivHQ", :From "MA_DA_C2", :To "PB_C4", :Duration 90}
    {:Policy "SRM-RCDivHQ", :From "MA_NDA_C3", :To "PT_C4", :Duration 90}
    {:Policy "SRM-RCDivHQ", :From "MD_NDA_C3", :To "PT_C4", :Duration 90}
    {:Policy "SRM-RCDivHQ", :From "MP_NDA_C3", :To "PT_C4", :Duration 90}
    {:Policy "SRM-RCDivHQ", :From "PB_C3", :To "MA_NDA_C3", :Duration 90}
    {:Policy "SRM-RCDivHQ", :From "PB_C3", :To "MD_NDA_C3", :Duration 90}
    {:Policy "SRM-RCDivHQ", :From "PB_C3", :To "MP_NDA_C3", :Duration 90})
  )


;;data to support supply and demand records.


(def srm-demand-records
  "Type	Enabled	Priority	Quantity	DemandIndex	StartDay	Duration	Overlap	SRC	SourceFirst	DemandGroup	Vignette	Operation	Category	Title 10_32	OITitle	Strength	Command	Location	DemandType	Theater	RIPTOA	BOG	Length	StartState	EndState
DemandRecord	TRUE	1	1	1	1	5000	0	Binder	Uniform	A	Al's Game	Al	Rotational	10	TrapperKeeper	1	Lifeguard	KO	A	Beach	0	FALSE	9999999	MD_DA_C1	R_C2
DemandRecord	TRUE	1	1	1	1	5000	0	Binder	Uniform	A	Bill's Game	Bill	Rotational	10	TrapperKeeper	1	Lifeguard	HI	A	Beach	0	FALSE	9999999	MD_DA_C1	R_C2
DemandRecord	TRUE	1	1	1	1	730	0	Binder	AC_First	B	Chuck's Game	Chuck	Rotational	10	TrapperKeeper	1	Referee	Middle	G	Middle	0	FALSE	730	MA_NDA_C1	PT_C4
DemandRecord	TRUE	1	1	1	1	240	0	Binder	AC_First	B	Chuck's Game	Dick	Rotational	10	TrapperKeeper	1	Referee	Middle	G	Middle	0	TRUE	240	MA_NDA_C1	PT_C4
DemandRecord	TRUE	1	1	1	1	1700	30	Binder	AC_First	B	Ed's Game	Ed	Rotational	10	TrapperKeeper	1	Referee	Middle	G	Middle	30	TRUE	365	MA_NDA_C1	PT_C4
DemandRecord	TRUE	1	1	1	1	1640	30	Binder	AC_First	B	Frank's Game	Frank	Rotational	10	TrapperKeeper	1	Referee	Middle	G	Middle	30	TRUE	270	MA_NDA_C1	PB_C3
DemandRecord	TRUE	1	1	1	1	5000	0	Binder	AC_First	B	Gerry's Game	Gerry	Rotational	10	TrapperKeeper	1	Referee	CO	G	West	0	FALSE	9999999	MA_NDA_C1	PT_C4
DemandRecord	TRUE	1	1	1	1	1370	0	Binder	RC_First	B	Hank's Game	Hank	Rotational	10	TrapperKeeper	1	Referee	North	G	North	0	FALSE	365	MA_NDA_C1	PB_C3
DemandRecord	TRUE	1	1	1	93	120	0	Rubber	Uniform	A	Paul's Game	1	Rotational	10	Rubber Cup	1	Goalie	WA	A	Beach	0	FALSE	120	MD_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	244	150	0	Rubber	Uniform	A	Paul's Game	2	Rotational	10	Rubber Cup	1	Goalie	WA	A	Beach	0	FALSE	150	MD_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	275	120	0	Rubber	Uniform	A	Paul's Game	3	Rotational	10	Rubber Cup	1	Goalie	AK	A	Beach	0	FALSE	120	MD_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	489	120	0	Plastic	Uniform	A	Paul's Game	4	Rotational	10	Plastic Cup	1	Goalie	HI	A	Beach	0	FALSE	120	MD_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	578	150	0	Rubber	Uniform	A	Paul's Game	5	Rotational	10	Rubber Cup	1	Goalie	AK	A	Beach	0	FALSE	150	MD_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	640	120	0	Plastic	Uniform	A	Paul's Game	6	Rotational	10	Plastic Cup	1	Goalie	HI	A	Beach	0	FALSE	120	MD_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	823	120	0	Rubber	Uniform	A	Paul's Game	7	Rotational	10	Rubber Cup	1	Goalie	WA	A	Beach	0	FALSE	120	MD_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	974	120	0	Rubber	Uniform	A	Paul's Game	8	Rotational	10	Rubber Cup	1	Goalie	WA	A	Beach	0	FALSE	120	MD_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	1005	120	0	Rubber	Uniform	A	Paul's Game	9	Rotational	10	Rubber Cup	1	Goalie	WA	A	Beach	0	FALSE	120	MD_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	640	365	0	Rubber	Uniform	A	Charlie's Game	1	Rotational	10	Rubber Cup	1	Coach	WA	A	West	0	FALSE	365	MD_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	1370	365	0	Rubber	Uniform	A	Charlie's Game	2	Rotational	10	Rubber Cup	1	Coach	GER	A	West	0	FALSE	365	MD_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	275	270	0	Plastic	Uniform	A	Charlie's Game	3	Rotational	10	Plastic Cup	1	Coach	ITAL	A	West	0	FALSE	270	MD_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	1005	365	0	Plastic	Uniform	A	Charlie's Game	4	Rotational	10	Plastic Cup	1	Coach	ITAL	A	West	0	FALSE	365	MD_DA_C1	PT_C4
DemandRecord	TRUE	1	1	1	1	1583	0	Rubber	Uniform	B	Jim	1	Rotational	10	Rubber Cup	1	Referee	North	A	North	0	FALSE	365	MA_DA_C1	PT_C4
DemandRecord	TRUE	1	1	1	1	1611	30	Steel	Uniform	B	Ted	1	Rotational	10	Steel Cup	1	Referee	Beach	A	Beach	30	FALSE	365	MA_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	1	1552	30	Steel	Uniform	B	Bob	1	Rotational	10	Steel Cup	1	Referee	Middle	A	Middle	30	FALSE	270	MA_DA_C1	PT_C4
DemandRecord	TRUE	1	1	1	1	1552	0	Steel	Uniform	B	Joe	1	Rotational	10	Steel Cup	1	Referee	Continental	A	Continental	0	FALSE	365	MA_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	1	730	0	Plastic	Uniform	B	Maurice	1	Rotational	10	Plastic Cup	1	Referee	NAF	A	Dry	0	FALSE	365	MA_DA_C1	PB_C3
DemandRecord	TRUE	1	1	1	1	1552	0	Plastic/Steel	Uniform	B	Jeremy	1	Rotational	10	Plastic/Steel Cup	1	Referee	Continental	A	Continental	0	FALSE	365	MP_DA_C1	PT_C4
DemandRecord	TRUE	1	1	1	32	300	0	Rubber	Uniform	B	Jack	1	Rotational	10	Rubber Cup	1	Referee	GER	G	West	0	FALSE	300	MA_NDA_C1	PT_C4
DemandRecord	TRUE	1	1	1	1	1784	30	Plastic	AC_First	B	Jill	1	Rotational	10	Plastic Cup	1	Referee	Middle	G	Middle	30	TRUE	270	MA_NDA_C1	PT_C4
DemandRecord	TRUE	1	1	1	1	1642	120	Plastic	AC_First	B	Jean	1	Rotational	10	Plastic Cup	1	Referee	Middle	G	Middle	120	TRUE	365	MA_NDA_C1	PT_C4
DemandRecord	TRUE	1	1	1	1	1700	30	Rubber	AC_First	B	Phyllis	1	Rotational	10	Rubber Cup	1	Referee	Middle	G	Middle	30	TRUE	365	MA_NDA_C1	PT_C4
DemandRecord	TRUE	1	1	1	1	1460	0	Plastic	RC_First	B	Michael	1	Rotational	32	Plastic Cup	1	Referee	Beach	G	Beach	0	TRUE	365	MA_NDA_C1	PT_C4
DemandRecord	TRUE	1	1	1	1	1460	0	Plastic	RC_First	B	Dwight	1	Rotational	32	Plastic Cup	1	Referee	Special	G	Special	0	TRUE	365	MA_NDA_C1	PT_C4
DemandRecord	TRUE	1	1	1	1	365	0	Plastic	RC_First	B	Stanley	1	Rotational	32	Plastic Cup	1	Referee	Middle	G	Middle	0	TRUE	365	MA_NDA_C1	PT_C4
DemandRecord	TRUE	1	1	1	731	365	0	Plastic	Uniform	B	Meredith	1	Rotational	32	Plastic Cup	1	Referee	West	G	West	0	TRUE	365	MA_NDA_C1	PT_C4
  "
  )

(def srm-supply-records
  "Type	Enabled	Quantity	SRC	Component	OITitle	Name	Behavior	Cycletime	Policy	Tags	Spawntime	Location	Position	Original	Strength	Remarks	Command	Loc	StartState	Duration
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Goalie	KO	MD_DA_C1	99999
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Goalie	HI	MD_DA_C1	99999
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	TX	MA_NDA_C1	510
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	TX	Ready_C2	
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	KS	Ready_C2	
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	GA	Ready_C2	
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	CA	MA_NDA_C1	99999
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	NY	MA_NDA_C1	150
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	NC	PT_C4	
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	KY	MA_NDA_C1	210
SupplyRecord	TRUE	1	Binder	NG	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	PA	PB_C3	
SupplyRecord	TRUE	1	Binder	NG	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	VA	Ready_C2	
SupplyRecord	TRUE	1	Binder	NG	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	MN	PB_C3	
SupplyRecord	TRUE	1	Binder	NG	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	KS	Ready_C2	
SupplyRecord	TRUE	1	Binder	NG	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	ID	Ready_C2	
SupplyRecord	TRUE	1	Binder	NG	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	IN	MA_NDA_C1	60
SupplyRecord	TRUE	1	Binder	NG	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	CA	PB_C3	
SupplyRecord	TRUE	1	Binder	NG	TrapperKeeper	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	NY	PB_C3	
SupplyRecord	TRUE	2	Rubber	AC	Rubber Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Goalie	WA	Ready_C2	
SupplyRecord	TRUE	1	Rubber	AC	Rubber Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Goalie	AK	PB_C3	
SupplyRecord	TRUE	1	Rubber	AC	Rubber Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Coach	GER	MA_NDA_C1	120
SupplyRecord	TRUE	1	Rubber	AC	Rubber Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	TX	Ready_C2	
SupplyRecord	TRUE	1	Rubber	AC	Rubber Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	TX	Ready_C2	
SupplyRecord	TRUE	1	Rubber	AC	Rubber Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	CO	MA_NDA_C1	270
SupplyRecord	TRUE	1	Rubber	NG	Rubber Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	PA	PB_C3	
SupplyRecord	TRUE	1	Rubber	NG	Rubber Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	WA	PB_C3	
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Goalie	HI	PT_C4	
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Goalie	HI	Ready_C2	
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	CO	MA_NDA_C1	210
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	GA	MA_DA_C1	150
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	NY	MA_NDA_C1	30
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	NY	PT_C4	
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	LA	MA_NDA_C1	60
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	KY	PT_C4	
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	KY	MA_NDA_C1	270
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	KY	PB_C3	
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1	Hybrid	Coach	ITAL	Ready_C2	
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1	Hybrid	Referee	NC	Ready_C2	240
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1	Hybrid	Referee	NC	Ready_C2	
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1	Hybrid	Referee	NC	Ready_C2	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	AR	PB_C3	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	NY	Ready_C2	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	OH	PB_C3	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	OK	PB_C3	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	IN	PB_C3	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	HI	PB_C3	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	TX	PB_C3	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	CA	PB_C3	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	GA	PB_C3	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	IL	PB_C3	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	NJ	PB_C3	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	WI	PT_C4	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	LA	PT_C4	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	OR	PT_C4	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	VT	PT_C4	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	FL	MA_NDA_C1	60
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	IA	Ready_C2	
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	PA	MA_NDA_C1	240
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	VA	MA_NDA_C1	
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	TX	PB_C3	
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	TX	Ready_C2	
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	TX	MA_DA_C1	150
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	TX	PT_C4	
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	TX	Ready_C2	
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	KS	Ready_C2	
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	KS	MA_DA_C1	90
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	CO	PB_C3	
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	GA	MA_DA_C1	240
SupplyRecord	TRUE	1	Steel	NG	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	ID	Ready_C2	
SupplyRecord	TRUE	1	Steel	NG	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	IA	PB_C3	
SupplyRecord	TRUE	1	Steel	NG	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	MS	PB_C3	
SupplyRecord	TRUE	1	Steel	NG	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	TN	PB_C3	
SupplyRecord	TRUE	1	Steel	NG	Steel Cup	Auto	SRM	0	Auto	Auto	Auto	Auto	Auto	TRUE	1		Referee	NC	PT_C4	
")



