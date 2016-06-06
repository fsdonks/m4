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
  "Type	Enabled	Priority	Quantity	DemandIndex	StartDay	Duration	Overlap	SRC	SourceFirst	DemandGroup	Vignette	Operation	Category	Title 10_32	OITitle	Strength	Command	Location	DemandType	Theater	BOG	StartState	EndState	MissionLength
DemandRecord	FALSE	1	1	1	1	5000	0	Binder	Uniform	A	Al's Game	1	SRM	10	TrapperKeeper	1	Lifeguard	KO	Internal	Beach	FALSE	MD_DA_C1	R_C2	9999999
DemandRecord	FALSE	1	1	1	1	5000	0	Binder	Uniform	A	Bill's Game	2	SRM	10	TrapperKeeper	1	Lifeguard	HI	Internal	Beach	FALSE	MD_DA_C1	R_C2	9999999
DemandRecord	TRUE	1	1	1	1	730	0	Binder	AC_First	B	Chuck's Game	3	SRM	10	TrapperKeeper	1	Referee	Middle	External	Middle	FALSE	MA_NDA_C3	PT_C4	730
DemandRecord	TRUE	1	1	1	1	240	0	Binder	AC_First	B	Chuck's Game	4	SRM	10	TrapperKeeper	1	Referee	Middle	External	Middle	TRUE	MA_NDA_C3	PT_C4	240
DemandRecord	TRUE	1	1	1	1	1700	30	Binder	AC_First	B	Ed's Game	5	SRM	10	TrapperKeeper	1	Referee	Middle	External	Middle	TRUE	MA_NDA_C3	PT_C4	365
DemandRecord	TRUE	1	1	1	1	1640	30	Binder	AC_First	B	Frank's Game	6	SRM	10	TrapperKeeper	1	Referee	Middle	External	Middle	TRUE	MA_NDA_C3	PB_C3	270
DemandRecord	FALSE	1	1	1	1	5000	0	Binder	AC_First	B	Gerry's Game	7	SRM	10	TrapperKeeper	1	Referee	CO	External	West	FALSE	MA_NDA_C3	PT_C4	9999999
DemandRecord	TRUE	1	1	1	1	1370	0	Binder	RC_First	B	Hank's Game	8	SRM	10	TrapperKeeper	1	Referee	North	External	North	FALSE	MA_NDA_C3	PB_C3	365
DemandRecord	TRUE	1	1	1	93	120	0	Rubber	Uniform	A	Paul's Game	9	SRM	10	Rubber Cup	1	Goalie	WA	Internal	Beach	FALSE	MD_DA_C1	PB_C3	120
DemandRecord	TRUE	1	1	1	244	150	0	Rubber	Uniform	A	Paul's Game	10	SRM	10	Rubber Cup	1	Goalie	WA	Internal	Beach	FALSE	MD_DA_C1	PB_C3	150
DemandRecord	TRUE	1	1	1	275	120	0	Rubber	Uniform	A	Paul's Game	11	SRM	10	Rubber Cup	1	Goalie	AK	Internal	Beach	FALSE	MD_DA_C1	PB_C3	120
DemandRecord	TRUE	1	1	1	489	120	0	Plastic	Uniform	A	Paul's Game	12	SRM	10	Plastic Cup	1	Goalie	HI	Internal	Beach	FALSE	MD_DA_C1	PB_C3	120
DemandRecord	TRUE	1	1	1	578	150	0	Rubber	Uniform	A	Paul's Game	13	SRM	10	Rubber Cup	1	Goalie	AK	Internal	Beach	FALSE	MD_DA_C1	PB_C3	150
DemandRecord	TRUE	1	1	1	640	120	0	Plastic	Uniform	A	Paul's Game	14	SRM	10	Plastic Cup	1	Goalie	HI	Internal	Beach	FALSE	MD_DA_C1	PB_C3	120
DemandRecord	TRUE	1	1	1	823	120	0	Rubber	Uniform	A	Paul's Game	15	SRM	10	Rubber Cup	1	Goalie	WA	Internal	Beach	FALSE	MD_DA_C1	PB_C3	120
DemandRecord	TRUE	1	1	1	974	120	0	Rubber	Uniform	A	Paul's Game	16	SRM	10	Rubber Cup	1	Goalie	WA	Internal	Beach	FALSE	MD_DA_C1	PB_C3	120
DemandRecord	TRUE	1	1	1	1005	120	0	Rubber	Uniform	A	Paul's Game	17	SRM	10	Rubber Cup	1	Goalie	WA	Internal	Beach	FALSE	MD_DA_C1	PB_C3	120
DemandRecord	TRUE	1	1	1	640	365	0	Rubber	Uniform	A	Charlie's Game	18	SRM	10	Rubber Cup	1	Coach	WA	Internal	West	FALSE	MD_DA_C1	PB_C3	365
DemandRecord	TRUE	1	1	1	1370	365	0	Rubber	Uniform	A	Charlie's Game	19	SRM	10	Rubber Cup	1	Coach	GER	Internal	West	FALSE	MD_DA_C1	PB_C3	365
DemandRecord	TRUE	1	1	1	275	270	0	Plastic	Uniform	A	Charlie's Game	20	SRM	10	Plastic Cup	1	Coach	ITAL	Internal	West	FALSE	MD_DA_C1	PB_C3	270
DemandRecord	TRUE	1	1	1	1005	365	0	Plastic	Uniform	A	Charlie's Game	21	SRM	10	Plastic Cup	1	Coach	ITAL	Internal	West	FALSE	MD_DA_C1	PT_C4	365
DemandRecord	TRUE	1	1	1	1	1583	0	Rubber	Uniform	B	Jim	22	SRM	10	Rubber Cup	1	Referee	North	Internal	North	FALSE	MA_DA_C1	PT_C4	365
DemandRecord	TRUE	1	1	1	1	1611	30	Steel	Uniform	B	Ted	23	SRM	10	Steel Cup	1	Referee	Beach	Internal	Beach	FALSE	MA_DA_C1	PB_C3	365
DemandRecord	TRUE	1	1	1	1	1552	30	Steel	Uniform	B	Bob	24	SRM	10	Steel Cup	1	Referee	Middle	Internal	Middle	FALSE	MA_DA_C1	PT_C4	270
DemandRecord	TRUE	1	1	1	1	1552	0	Steel	Uniform	B	Joe	25	SRM	10	Steel Cup	1	Referee	Continental	Internal	Continental	FALSE	MA_DA_C1	PB_C3	365
DemandRecord	TRUE	1	1	1	1	730	0	Plastic	Uniform	B	Maurice	26	SRM	10	Plastic Cup	1	Referee	NAF	Internal	Dry	FALSE	MA_DA_C1	PB_C3	365
DemandRecord	TRUE	1	1	1	1	1552	0	Plastic/Steel	Uniform	B	Jeremy	27	SRM	10	Plastic/Steel Cup	1	Referee	Continental	Internal	Continental	FALSE	MP_DA_C1	PT_C4	365
DemandRecord	TRUE	1	1	1	32	300	0	Rubber	Uniform	B	Jack	28	SRM	10	Rubber Cup	1	Referee	GER	External	West	FALSE	MA_NDA_C3	PT_C4	300
DemandRecord	TRUE	1	1	1	1	1784	30	Plastic	AC_First	B	Jill	29	SRM	10	Plastic Cup	1	Referee	Middle	External	Middle	TRUE	MA_NDA_C3	PT_C4	270
DemandRecord	TRUE	1	1	1	1	1642	120	Plastic	AC_First	B	Jean	30	SRM	10	Plastic Cup	1	Referee	Middle	External	Middle	TRUE	MA_NDA_C3	PT_C4	365
DemandRecord	TRUE	1	1	1	1	1700	30	Rubber	AC_First	B	Phyllis	31	SRM	10	Rubber Cup	1	Referee	Middle	External	Middle	TRUE	MA_NDA_C3	PT_C4	365
DemandRecord	TRUE	1	1	1	1	1460	0	Plastic	RC_First	B	Michael	32	SRM	32	Plastic Cup	1	Referee	Beach	External	Beach	TRUE	MA_NDA_C3	PT_C4	365
DemandRecord	TRUE	1	1	1	1	1460	0	Plastic	RC_First	B	Dwight	33	SRM	32	Plastic Cup	1	Referee	Special	External	Special	TRUE	MA_NDA_C3	PT_C4	365
DemandRecord	TRUE	1	1	1	1	365	0	Plastic	RC_First	B	Stanley	34	SRM	32	Plastic Cup	1	Referee	Middle	External	Middle	TRUE	MA_NDA_C3	PT_C4	365
DemandRecord	TRUE	1	1	1	731	365	0	Plastic	Uniform	B	Meredith	35	SRM	32	Plastic Cup	1	Referee	West	External	West	TRUE	MA_NDA_C3	PT_C4	365
"
  )

(def srm-supply-records
"Type	Enabled	Quantity	SRC	Component	OITitle	Name	Behavior	CycleTime	Policy	Tags	SpawnTime	Location	Position	Original	Strength	Remarks	Command	Origin	Duration
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	1	SRMAC	Auto	0	MD_DA_C1	MD_DA_C1	TRUE	1		Goalie	KO	9999999
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	1	SRMAC	Auto	0	MD_DA_C1	MD_DA_C1	TRUE	1		Goalie	HI	9999999
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	1	SRMAC	Auto	0	MA_NDA_C3	MA_NDA_C3	TRUE	1	Altered from C1 to C3	Referee	TX	510
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	1	SRMAC	Auto	0	TX	R_C2	TRUE	1		Referee	TX	0
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	1	SRMAC	Auto	0	KS	R_C2	TRUE	1		Referee	KS	0
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	1	SRMAC	Auto	0	GA	R_C2	TRUE	1		Referee	GA	0
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	1	SRMAC	Auto	0	MA_NDA_C3	MA_NDA_C3	TRUE	1	Altered from C1 to C3	Referee	CA	9999999
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	1	SRMAC	Auto	0	MA_NDA_C3	MA_NDA_C3	TRUE	1	Altered from C1 to C3	Referee	NY	150
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	1	SRMAC	Auto	0	NC	PT_C4	TRUE	1		Referee	NC	0
SupplyRecord	TRUE	1	Binder	AC	TrapperKeeper	Auto	SRM	1	SRMAC	Auto	0	MA_NDA_C3	MA_NDA_C3	TRUE	1	Altered from C1 to C3	Referee	KY	210
SupplyRecord	TRUE	1	Binder	NG	TrapperKeeper	Auto	SRM	1	SRMRC13	Auto	0	PA	PB_C3	TRUE	1		Referee	PA	0
SupplyRecord	TRUE	1	Binder	NG	TrapperKeeper	Auto	SRM	1	SRMRC13	Auto	0	VA	R_C2	TRUE	1		Referee	VA	0
SupplyRecord	TRUE	1	Binder	NG	TrapperKeeper	Auto	SRM	1	SRMRC13	Auto	0	MN	PB_C3	TRUE	1		Referee	MN	0
SupplyRecord	TRUE	1	Binder	NG	TrapperKeeper	Auto	SRM	1	SRMRC13	Auto	0	KS	R_C2	TRUE	1		Referee	KS	0
SupplyRecord	TRUE	1	Binder	NG	TrapperKeeper	Auto	SRM	1	SRMRC13	Auto	0	ID	R_C2	TRUE	1		Referee	ID	0
SupplyRecord	TRUE	1	Binder	NG	TrapperKeeper	Auto	SRM	1	SRMRC13	Auto	0	MA_NDA_C3	MA_NDA_C3	TRUE	1	Altered from C1 to C3	Referee	IN	60
SupplyRecord	TRUE	1	Binder	NG	TrapperKeeper	Auto	SRM	1	SRMRC13	Auto	0	CA	PB_C3	TRUE	1		Referee	CA	0
SupplyRecord	TRUE	1	Binder	NG	TrapperKeeper	Auto	SRM	1	SRMRC13	Auto	0	NY	PB_C3	TRUE	1		Referee	NY	0
SupplyRecord	TRUE	2	Rubber	AC	Rubber Cup	Auto	SRM	1	SRMAC	Auto	0	WA	R_C2	TRUE	1		Goalie	WA	0
SupplyRecord	TRUE	1	Rubber	AC	Rubber Cup	Auto	SRM	1	SRMAC	Auto	0	AK	PB_C3	TRUE	1		Goalie	AK	0
SupplyRecord	TRUE	1	Rubber	AC	Rubber Cup	Auto	SRM	1	SRMAC	Auto	0	MA_NDA_C3	MA_NDA_C3	TRUE	1	Altered from C1 to C3	Coach	GER	120
SupplyRecord	TRUE	1	Rubber	AC	Rubber Cup	Auto	SRM	1	SRMAC	Auto	0	TX	R_C2	TRUE	1		Referee	TX	0
SupplyRecord	TRUE	1	Rubber	AC	Rubber Cup	Auto	SRM	1	SRMAC	Auto	0	TX	R_C2	TRUE	1		Referee	TX	0
SupplyRecord	TRUE	1	Rubber	AC	Rubber Cup	Auto	SRM	1	SRMAC	Auto	0	MA_NDA_C3	MA_NDA_C3	TRUE	1	Altered from C1 to C3	Referee	CO	270
SupplyRecord	TRUE	1	Rubber	NG	Rubber Cup	Auto	SRM	1	SRMRC	Auto	0	PA	PB_C3	TRUE	1		Referee	PA	0
SupplyRecord	TRUE	1	Rubber	NG	Rubber Cup	Auto	SRM	1	SRMRC	Auto	0	WA	PB_C3	TRUE	1		Referee	WA	0
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	1	SRMAC	Auto	0	HI	PT_C4	TRUE	1		Goalie	HI	0
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	1	SRMAC	Auto	0	HI	R_C2	TRUE	1		Goalie	HI	0
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	1	SRMAC	Auto	0	MA_NDA_C3	MA_NDA_C3	TRUE	1	Altered from C1 to C3	Referee	CO	210
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	1	SRMAC	Auto	0	MA_DA_C1	MA_DA_C1	TRUE	1		Referee	GA	150
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	1	SRMAC	Auto	0	MA_NDA_C3	MA_NDA_C3	TRUE	1	Altered from C1 to C3	Referee	NY	30
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	1	SRMAC	Auto	0	NY	PT_C4	TRUE	1		Referee	NY	0
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	1	SRMAC	Auto	0	MA_NDA_C3	MA_NDA_C3	TRUE	1	Altered from C1 to C3	Referee	LA	60
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	1	SRMAC	Auto	0	KY	PT_C4	TRUE	1		Referee	KY	0
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	1	SRMAC	Auto	0	MA_NDA_C3	MA_NDA_C3	TRUE	1	Altered from C1 to C3	Referee	KY	270
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	1	SRMAC	Auto	0	KY	PB_C3	TRUE	1		Referee	KY	0
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	1	SRMAC	Auto	0	ITAL	R_C2	TRUE	1	Hybrid	Coach	ITAL	0
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	1	SRMAC	Auto	0	NC	R_C2	TRUE	1	Hybrid	Referee	NC	240
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	1	SRMAC	Auto	0	NC	R_C2	TRUE	1	Hybrid	Referee	NC	0
SupplyRecord	TRUE	1	Plastic	AC	Plastic Cup	Auto	SRM	1	SRMAC	Auto	0	NC	R_C2	TRUE	1	Hybrid	Referee	NC	0
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	1	SRMRC	Auto	0	AR	PB_C3	TRUE	1		Referee	AR	0
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	1	SRMRC	Auto	0	NY	R_C2	TRUE	1		Referee	NY	0
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	1	SRMRC	Auto	0	OH	PB_C3	TRUE	1		Referee	OH	0
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	1	SRMRC	Auto	0	OK	PB_C3	TRUE	1		Referee	OK	0
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	1	SRMRC	Auto	0	IN	PB_C3	TRUE	1		Referee	IN	0
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	1	SRMRC	Auto	0	HI	PB_C3	TRUE	1		Referee	HI	0
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	1	SRMRC	Auto	0	TX	PB_C3	TRUE	1		Referee	TX	0
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	1	SRMRC	Auto	0	CA	PB_C3	TRUE	1		Referee	CA	0
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	1	SRMRC	Auto	0	GA	PB_C3	TRUE	1		Referee	GA	0
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	1	SRMRC	Auto	0	IL	PB_C3	TRUE	1		Referee	IL	0
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	1	SRMRC	Auto	0	NJ	PB_C3	TRUE	1		Referee	NJ	0
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	1	SRMRC	Auto	0	WI	PT_C4	TRUE	1		Referee	WI	0
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	1	SRMRC	Auto	0	LA	PT_C4	TRUE	1		Referee	LA	0
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	1	SRMRC	Auto	0	OR	PT_C4	TRUE	1		Referee	OR	0
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	1	SRMRC	Auto	0	VT	PT_C4	TRUE	1		Referee	VT	0
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	1	SRMRC	Auto	0	MA_NDA_C3	MA_NDA_C3	TRUE	1	Altered from C1 to C3	Referee	FL	60
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	1	SRMRC	Auto	0	IA	R_C2	TRUE	1		Referee	IA	0
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	1	SRMRC	Auto	0	MA_NDA_C3	MA_NDA_C3	TRUE	1	Altered from C1 to C3	Referee	PA	240
SupplyRecord	TRUE	1	Plastic	NG	Plastic Cup	Auto	SRM	1	SRMRC	Auto	0	MA_NDA_C3	MA_NDA_C3	TRUE	1	Altered from C1 to C3	Referee	VA	0
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	1	SRMAC	Auto	0	TX	PB_C3	TRUE	1		Referee	TX	0
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	1	SRMAC	Auto	0	TX	R_C2	TRUE	1		Referee	TX	0
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	1	SRMAC	Auto	0	MA_DA_C1	MA_DA_C1	TRUE	1		Referee	TX	150
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	1	SRMAC	Auto	0	TX	PT_C4	TRUE	1		Referee	TX	0
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	1	SRMAC	Auto	0	TX	R_C2	TRUE	1		Referee	TX	0
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	1	SRMAC	Auto	0	KS	R_C2	TRUE	1		Referee	KS	0
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	1	SRMAC	Auto	0	MA_DA_C1	MA_DA_C1	TRUE	1		Referee	KS	90
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	1	SRMAC	Auto	0	CO	PB_C3	TRUE	1		Referee	CO	0
SupplyRecord	TRUE	1	Steel	AC	Steel Cup	Auto	SRM	1	SRMAC	Auto	0	MA_DA_C1	MA_DA_C1	TRUE	1		Referee	GA	240
SupplyRecord	TRUE	1	Steel	NG	Steel Cup	Auto	SRM	1	SRMRC	Auto	0	ID	R_C2	TRUE	1		Referee	ID	0
SupplyRecord	TRUE	1	Steel	NG	Steel Cup	Auto	SRM	1	SRMRC	Auto	0	IA	PB_C3	TRUE	1		Referee	IA	0
SupplyRecord	TRUE	1	Steel	NG	Steel Cup	Auto	SRM	1	SRMRC	Auto	0	MS	PB_C3	TRUE	1		Referee	MS	0
SupplyRecord	TRUE	1	Steel	NG	Steel Cup	Auto	SRM	1	SRMRC	Auto	0	TN	PB_C3	TRUE	1		Referee	TN	0
SupplyRecord	TRUE	1	Steel	NG	Steel Cup	Auto	SRM	1	SRMRC	Auto	0	NC	PT_C4	TRUE	1		Referee	NC	0
"
  )
