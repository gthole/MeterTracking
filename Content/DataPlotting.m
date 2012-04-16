BeginPackage["MeterTracking`Content`DataPlotting`"];

Needs["MeterTracking`Content`DataPlottingUtilities`"];

WriteDataPlots::usage = "Runtime method to export data graphics to a given directory."
ShowDataPlots::usage = "Testing tool to return a specified dataplot."

Begin["Private`"]

WriteDataPlots[mn_, o_, spec_:All] := With[
	{viz = If[StringQ[spec], Cases[$visualizations, {spec,_}], Part[$visualizations,spec]]},
    With[
    	{graphic = #3[mn]},
    	Export[
    		ToFileName[{o}, #1 <> ".png"],
    		If[StringQ[#2], Labeled[graphic,Style[#2,Gray],Top], graphic],
    		"PNG"
    	]
    ]& @@@ viz
];

ShowDataPlots[mn_, spec_] :=  With[
	{viz = If[StringQ[spec], Cases[$visualizations, {spec,_}], Part[$visualizations,spec]]},
    {#1, #2[mn]}& @@@ viz
];

$visualizations = {
	{
	"lastreading",
	None,
	Function[{mn}, TableForm[
		{{#[[2]], ToString[#[[3]]] <> " Kw"}, {#[[4]], ToString[#[[5]]] <> " degrees"}} &@mn[[-1]], 
		TableHeadings -> {{"Meter", "Weather"}, ({DateString[#[[1]], {"MonthName", " ", "Day", ", ", "Year"}], 
       	DateString[#[[1]], {"Hour12Short", " ", "AMPM"}]} &@mn[[-1]])}]]
    },
    {
    "Kwdatelist",
    "Kw Usage over the last 7 days and Median Use",
	Function[{mn}, 
		With[{d = Select[mn[[All, {1, 3}]], First[DateDifference[#[[1]], DateList[][[1 ;; 4]], "Day"]] < 7 &]},
			Show[
				DateListPlot[d, Joined -> True, Filling -> Axis, AspectRatio -> 1/5],
				DateListPlot[{{d[[1, 1]], Median[d[[All, 2]]]}, {d[[-1, 1]], Median[d[[All, 2]]]}}, Joined -> True, PlotStyle -> {Dashed, Opacity[0.5], Darker[Orange]}],
				ImageSize -> 720
			]
		]
	]
    },
    {
    "tempKwscatterplot",
    "Temperature to Kw Use ScatterPlot",
    Function[{mn}, 
    Block[
		{pts, fit1, fit2, fit3, x},
		pts = mn[[All,{5,3}]];
		fit1 = Fit[pts, {1, x}, x];
		fit2 = Fit[pts, {1, x, x^2}, x];
		fit3 = Fit[pts, {1, x, x^2, x^3}, x];
		Show[
			ListPlot[pts, ColorFunction -> "DarkRainbow"],
			Plot[{fit1, fit2, fit3}, {x, Min[pts[[All, 1]]], Max[pts[[All, 1]]]}, PlotStyle -> ColorData["SolarColors"] /@ {.1, .5, .9}]
		]
	]]
	},
	{
	"weeklycircdatelistplot",
	"Recent Week - Kw Use",
	Function[{mn}, 
	With[
		{d = Select[mn[[All,{1,3}]], First[DateDifference[#[[1]], DateList[][[1 ;; 4]], "Week"]] < 1 &],
		sunday = DatePlus[DateList[][[1;;4]], -1*Mod[First[DateDifference[{2011, 12, 11}, DateList[][[1;;4]], "Day"]], 7]]
		}, 
		CircularDateListPlot[d,{sunday, DatePlus[sunday, {1,"Week"}]},"Week"]
	]
	]
	},
	{
	"yearlycircdatelistplot",
	"Recent Year - Kw Use",
	Function[{mn}, 
	With[
		{d = DailyAverages[Select[mn[[All,{1,3}]], First[DateDifference[#[[1]], DateList[][[1 ;; 4]], "Year"]] < 1 &],2],
		first = DateList[][[{1}]]
		}, 
		CircularDateListPlot[d,{first, DatePlus[first, {1,"Year"}]}, "Year"]
	]
	]
	},
	{
	"weekdayuse",
	"Averages per Weekday",
	Function[{mn},
	With[
		{d = WeekDayAverages[mn]},
		BarChart[d[[All,2]], ChartStyle -> "DarkRainbow", ChartLabels -> d[[All, 1]]]
	]
	]
	},
	{
	"activitytimeuse",
	"Averages per Activity Schedule",
	Function[{mn},
	With[
		{d = ActivityTimeAverages[mn]},
		BarChart[d[[All,2]], ChartStyle -> 24, ChartLabels -> d[[All, 1]]]
	]
	]
	},
	{
	"histogramkwuse",
	"Smooth Histogram of Kw Use",
	Function[{mn}, 
		With[{pts = mn[[All, 3]]}, 
			SmoothHistogram[pts, Filling -> Axis, 
				FillingStyle -> Lighter[Orange, 0.7], 
				PlotStyle -> {Thick, Orange}
			]
		]
	]
	}
}

End[]

EndPackage[]