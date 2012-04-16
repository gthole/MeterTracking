BeginPackage["MeterTracking`Content`DataPlottingUtilities`"];



ScaleDate::usage = "ScaleDate[e, d, ln] returns a radian to represent the point d in a date range e of point length ln."
DailyAverages::usage = "DailyAverages[mn, part] gives the Mean of all part in mn by day."
CircularDateListPlot::usage = "Fancy graphic to show cyclic data points."
ActivityTimeAverages
WeekDayAverages

Begin["Private`"]


Clear[$PlotMarkers];
$PlotMarkers["Year"] = {
	{"January", Pi/2, {0,0.1}},
	{"March", Pi/6, {0.25,0.1}},
	{"May", 11*Pi/6, {0.25,-0.1}},
	{"July", 3*Pi/2, {0,-0.1}},
	{"September", 7*Pi/6, {-0.25,-0.1}},
	{"November", 5*Pi/6, {-0.25,0.1}}
};
$PlotMarkers["Week"] = {
	{"Sunday", Pi/2, {0,0.1}},
	{"Monday", 3*Pi/14, {0.25,0.1}},
	{"Tuesday", 27*Pi/14, {0.25,-0.1}},
	{"Wednesday", 23*Pi/14, {0.25,-0.1}},
	{"Thursday", 19*Pi/14, {-0.25,-0.1}},
	{"Friday", 15*Pi/14, {-0.25,-0.1}},
	{"Saturday", 11*Pi/14, {-0.25,0.1}}
};

$PlotMarkers[_] := {};

ScaleDate[endpts_, date_, ln_] := With[
	{endptlng = ((370*Abs[First[DateDifference[##, "Second"]]])/371) & @@ endpts},
	(Pi/2) - (2*Pi*(First[DateDifference[First[endpts], date, "Second"]]/endptlng))
];

DailyAverages[mn_, part_] := AverageUsageBy[mn, #[[1, {1, 2, 3}]]&, part];
WeekDayAverages[mn_] := AverageUsageBy[mn, DateString[#[[1]], "DayNameShort"]&];
ActivityTimeAverages[mn_] := AverageUsageBy[
	mn,
	Which[
		#[[1,4]] === 23 || #[[1,4]] < 7,
		"Sleeping"
		,
		MatchQ[DateString[#[[1]], "DayNameShort"], "Sat"|"Sun"],
		"Active"
		,
		8 < #[[1,4]] < 18,
		"At Work"
		,
		True,
		"Active"
	]&
]

AverageUsageBy[mn_, func_, part_:3] := With[
	{qd = Append[#, func[#]]& /@ mn},
	With[
		{g = GatherBy[qd, Last]},
		{Last[First[#]], Mean[#[[All, part]]]} & /@ g
	]
]

CircularDateListPlot[argor_, endpoints_: Automatic, markertype_:None] := PolarDateListLogPlot[argor,endpoints, markertype];
PolarDateListLogPlot[argor_, endpoints_: Automatic, markertype_:None] := Block[
	{
		logfactor = (1/Min[argor[[All,2]]])*Exp[0.2*Max[argor[[All,2]]]],
		endpts = If[endpoints === Automatic, SortBy[argor[[All, 1]], AbsoluteTime][[{1, -1}]], endpoints],
		ln = Length[argor],
		mean, min, max, todayscale, arg
	},
	arg = Log[logfactor*argor[[All, 2]]];
	mean = Mean[arg];
	min = Min[arg];
	max = Max[arg];
	
	todayscale = ScaleDate[endpts, DateList[][[1;;4]], ln];
	
	Graphics[
		{
			{Gray, Thickness[0.002], Circle[{0, 0}, mean]},
			{Gray, Thickness[0.002], Dashing[0.02], Circle[{0, 0}, max]}, 
			{Gray, Thickness[0.002], Dashing[0.02], Circle[{0, 0}, min]},
			With[
				{d = max*{Cos[todayscale], Sin[todayscale]}},
				{Darker[Blue], Thickness[0.004], Line[{{0, 0}, d}], Disk[d, Scaled[0.01]]}
			],
			With[
				{d = max*{Cos[#2], Sin[#2]}},
				{Gray, Thickness[0.001], Line[{{0, 0}, d}], Text[#1, d + #3]}
			] & @@@ $PlotMarkers[markertype],
			With[
				{
					scale = ((#[[2]]-min)/(max-min)),
					ptplus = (#[[2]])*{Cos[#[[1]]], Sin[#[[1]]]},
					posQ = #[[2]] > mean
				},
				{ColorData["SolarColors"][1-scale], Thickness[0.001], Line[{{0,0}, ptplus}], Disk[ptplus, Scaled[0.006]]}
			] & /@ 
			Select[{ScaleDate[endpts, #[[1]], ln], Log[logfactor*#[[2]]]} & /@ argor, FreeQ[#, Indeterminate] &]
    	}
    ]
];

(*
CircularDateListPlot[argor_, endpoints_: Automatic, markertype_:None] := Block[
	{
		endpts = If[endpoints === Automatic, SortBy[argor[[All, 1]], AbsoluteTime][[{1, -1}]], endpoints],
		ln = Length[argor],
		mean = Mean[argor[[All, 2]]],
		min = Min[argor[[All, 2]]],
		max = Max[argor[[All, 2]]],
		todayscale
	},
	todayscale = ScaleDate[endpts, DateList[][[1;;4]], ln];
	Graphics[
		{
			{Gray, Thickness[0.004], Circle[{0, 0}, mean]},
			{Gray, Thickness[0.002], Dashing[0.02], Circle[{0, 0}, max]}, 
			{Gray, Thickness[0.002], Dashing[0.02], Circle[{0, 0}, min]},
			With[
				{d = max*{Cos[todayscale], Sin[todayscale]}},
				{ColorData["DarkRainbow"][0], Thickness[0.003], Line[{{0, 0}, d}], Disk[d, Scaled[0.004]]}
			],
			With[
				{d = max*{Cos[#2], Sin[#2]}},
				{Gray, Thickness[0.002], Line[{{0, 0}, d}], Disk[d, Scaled[0.004]], Text[#1, d + #3]}
			] & @@@ $PlotMarkers[markertype],
			With[
				{
					ptunit = mean*{Cos[#[[1]]], Sin[#[[1]]]},
					ptplus = (#[[2]])*{Cos[#[[1]]], Sin[#[[1]]]},
					posQ = #[[2]] > mean
				},
				If[
					posQ,
					{ColorData["DarkRainbow"][1], Thickness[0.001], Line[{ptunit, ptplus}], Disk[ptplus, Scaled[0.004]]},
					{ColorData["DarkRainbow"][2/7], Thickness[0.001], Line[{ptunit, ptplus}], Disk[ptplus, Scaled[0.004]]}
				]
			] & /@ 
			Select[{ScaleDate[endpts, #[[1]], ln], #[[2]]} & /@ argor, FreeQ[#, Indeterminate] &]
    	}
    ]
];
*)

End[]

EndPackage[]