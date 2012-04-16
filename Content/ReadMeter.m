BeginPackage["MeterTracking`Content`ReadMeter`"]

(* TODO: Triangle with highest point density method in findendpoint. *)
(* - Note: Centerpoint independent method.  Nice. *)
(* - Need: Sanity check for when method is invalid (on any method.) *)

(* TODO: Use ImageKeypoints to find connected circles of counters instead of ImageCorrespondingPoints. *)
(* - Tricky, but might save the subsequent ImageKeypoints calls for counters (?). *)


(* TODO: Procedural test system. *)

ReadMeter
OverlayCounterVector
OverlayDetectionPoints
CounterValue
DetectCounters
DialNumsToMeterNum
FindLineClusters

FindCounterVector
findvectorfrompoints
findcenterpoint
findendpoint
findpointgroup
$CounterDetectionKeys
Slope
MidPoint
MeanLine

Begin["`Private`"]

FindCounterVector[img_] := Block[
	{lines},
	lines = CounterImageLines[img, 0.02];

	FindVectorFromLines[lines, "Method" -> "Classic"]
];

CounterImageLines[img_, inc_] := Flatten[
		Table[
			Flatten[
				Table[
					ImageLines[
						EdgeDetect[
							MaxDetect[img, j],
							1
						],
						k, 
						Method -> "RANSAC"
					], 
					{j, .38, .45, 0.002}
				],
				1
			],
			{k, .39, .45, inc}
		],
		1
	];


OverlayCounterVector[img_] := Block[
	{vector, lines},
	vector = FindCounterVector[img];
	lines = Flatten[
		Table[
			ImageLines[
				EdgeDetect[
					MaxDetect[img, j],
					1
				],
				0.4, 
				Method -> "RANSAC"
			], 
			{j, .38, .45, 0.002}
		],
		1
	];
	Show[
		img,
		Graphics[
			Append[
				{Red, Line[#]}& /@ lines,
				{Green, Thick, Arrow[vector]}
			]
		]
	]
];

FindVectorFromLines[lines_, "Method" -> "Classic"] := Module[
	{pts = Flatten[lines, 1]},
	Which[
		Length[Tally[pts[[All,1]]]] == 2,
		key = 1,
		Length[Tally[pts[[All,2]]]] == 2,
		key = 2,
		True,
		Return[FindVectorFromLines[lines, "Method" -> "4dClusters"]]
	];
	Print["  FindVectorFromLines: Classic"];
	altkey = Mod[key, 2] + 1;
	groups = GatherBy[pts, Part[#, key]&];
	groups = Sort[groups, With[{a=altkey},(Max[#1[[All,a]]] - Min[#1[[All,a]]] > Max[#2[[All,a]]] - Min[#2[[All,a]]])]&];

	Mean /@ groups
];

FindVectorFromLines[lns_, "Method" -> "4dClusters"] := Block[
	{clustered, meanline1, meanline2, lines = lns},
	Print["  FindVectorFromLines: 4dClusters"];
	clustered = Reverse[SortBy[FindClusters[Flatten[#, 1] & /@ lines, Method -> "Agglomerate"], Length]];
	
	(* Back-up attempt. *)
	If[Length[clustered] < 2, 
		clustered = Reverse[SortBy[FindClusters[Flatten[Take[Reverse[SortBy[GatherBy[Flatten[#, 1] & /@ lines, Floor[Log[(#[[4]] - #[[2]])/(#[[3]] - #[[1]])]] &], Length]], 2], 1], Method -> "Agglomerate"], Length]];
		Print["    Agglomerate failed, tried log floor sort. ("<>ToString[Length[clustered]]<>" - "<>ToString[Length/@clustered]<>")"]
		,
		Print["    Agglomerate worked, length. ("<>ToString[Length[clustered]]<>" - "<>ToString[Length/@clustered]<>")"];
	];
	clustered = Take[clustered, 2];
	clustered = Map[Function[s, Partition[s, 2]], #] & /@ clustered;
	
	meanline1 = MeanLine[clustered[[1]]];
	meanline2 = MeanLine[clustered[[2]]];
	{MidPoint[MeanLine[{meanline1, meanline2}]], ConvergencePoint[meanline1, meanline2]}

]

FindVectorFromLines[lns_, "Method" -> "SlopeClusters"] := Module[
	{lines = lns, meanline1, meanline2, recurse = 0, $recurselimit = 2},
	While[
		recurse <= $recurselimit && Length[lines] > 2,
		recurse++;
		lines = MeanLine /@ FindLineClusters[lines]
	];

	Which[
		Length[lines]<2, Print["  Not enough clusters in ",img]; Abort[],
		Length[lines]>2, Print["  Re-clustering unsuccessful.  Using first two."]
	];
	
	meanline1 = lines[[1]];
	meanline2 = lines[[2]];
	{MidPoint[MeanLine[{meanline1, meanline2}]], ConvergencePoint[meanline1, meanline2]}
];


FindLineClusters[lines_] := Module[
	{slopes, clusters}, 
	slopes = NormalizeSteepSlope /@ Sort[Slope /@ lines];
	clusters = FindClusters[slopes, Method -> "Agglomerate"];
	SortBy[Table[Select[lines, MemberQ[clusters[[i]], NormalizeSteepSlope[Slope[#]]] &], {i, Length[clusters]}], Length]
];

NormalizeSteepSlope[sl_] := If[sl < 14, Abs[sl], sl];

MidPoint[{{x1_, y1_}, {x2_, y2_}}] := {(x1 + x2)/2, (y1 + y2)/2};

(* TODO: Fix sloppy construction. *)
ConvergencePoint[
	line1:{{x11_, y11_}, {x12_, y12_}}, 
	line2:{{x21_, y21_}, {x22_, y22_}}] := With[
	{x = Solve[Slope[line1]*(x - x11) + y11 == Slope[line2]*(x - x21) + y21,x][[1, 1, 2]]}, 
	{x, (Slope[line1]*(# - x11) + y11) &[x]}
];

(* Note: Use 10^100 for Infinity to avoid errors. *)
Slope[{{x1_, y1_}, {x2_, y2_}}] := If[x1 == x2, 10^100, (y1 - y2)/(x1 - x2)];

MeanLine[all : {{{_, _}, {_, _}}..}] := With[
	{
		x1 = Mean[all[[All, 1, 1]]],
		y1 = Mean[all[[All, 1, 2]]],
		x2 = Mean[all[[All, 2, 1]]],
		y2 = Mean[all[[All, 2, 2]]]
	},
	{{x1, y1}, {x2, y2}}
];

CounterValueFromVector[None, _] := $Failed;
CounterValueFromVector[vector_, spin_] := Block[
	{w = Normalize[vector[[2]] - vector[[1]]]},
	Times[
		If[
			spin*First[w] > 0, 
			VectorAngle[w, {0, 1}], (2*Pi) - VectorAngle[w, {0, 1}]
		],
		(10/(2*Pi))
	]
];
CounterValue[img_, spin_] := Block[
	{vector = FindCounterVector[img]},
	CounterValueFromVector[vector, spin]
];

DialNumsToMeterNum[{___, None, ___}] := $Failed;
DialNumsToMeterNum[mr_] := Block[
	{res},
	res = {};
	Table[
		Which[
			i == 1,
			PrependTo[res,mr[[-i]]]
			,
			And[
				Ceiling[mr[[-i]]] - mr[[-i]] < 0.2,
				res[[1]]/10 < 0.4
			],
			PrependTo[res,Mod[Ceiling[mr[[-i]]],10]]
			,
			And[
				mr[[-i]] - Floor[mr[[-i]]] < 0.2,
				res[[1]]/10 > 0.6
			],
			PrependTo[res, Mod[Floor[mr[[-i]]] - 1, 10]]
			,
			True,
			PrependTo[res,Floor[mr[[-i]]]]
		],
		{i, 1, 5}
	];
	ToExpression[StringJoin[ToString /@ res]]
]


Clear[ReadMeter];
Options[ReadMeter] = {};
ReadMeter[image_Image, opts:OptionsPattern[]] := Block[
	{iter = 1},
	Print["Reading Meter"];
	DialNumsToMeterNum[(iter++; CounterValue[#, (-1)^iter]) & /@ DetectCounters[image, "Method" -> "Simple"]]
];

ReadMeter[_] := $Failed;

ReadMeter[] := Block[
	{o,i,ok},
	Print["CurrentImage[] functionality buggy.  Use at own risk."];
	o = CurrentImage[];
	$ImagingDevices;
	i = CurrentImage[];
	ok = SortBy[{o, i}, ImageLevels[Binarize[#]][[2, 2]] &];
	ReadMeter[First[ok]]
];


(* TODO: Fix up. *)
DetectCounters[img_, "Method" -> "Simple"] := First[ImagePartition[ImageCrop[img, {300, 70}], {60, 65}]]

DetectCounters[image_, "Method" -> "CorrespondingPoints"] := Block[
	{timg, pts, max, min, img, dim},
  	timg = If[
    	MatchQ[OptionValue["InitialTrim"], {{_Integer, _Integer}, {_Integer, _Integer}}],
		ImageTrim[image, OptionValue["InitialTrim"]], 
		image
	];
	pts = Join @@ (First[ImageCorrespondingPoints[timg, Import[#]]]& /@ $CounterDetectionKeys);
	min = {Min[pts[[All, 1]]], Min[pts[[All, 2]]]} - {20, 20};
	max = {Max[pts[[All, 1]]], Max[pts[[All, 2]]]} + {50, 20};
	img = ImageCrop[ImageTrim[timg, {min, max}]];
	dim = ImageDimensions[img];
	First[ImagePartition[img, {First[dim]/5, Last[dim]}]]
]
		
$CounterDetectionKeys = {
   "Desktop/counters.jpg",
   "Desktop/counter23.jpg"
   };
		
		
		
		
		

(************  Old Methods  ***************)

(*



OverlayDetectionPoints[img_] := Block[
	{pts},
	pts = Join @@ (First[ImageCorrespondingPoints[img, Import[#]]] & /@ $CounterDetectionKeys);
	Show[
		img,
		Graphics[{Green, PointSize[Large], Point[#]} & /@ pts]
	]
]



*)


(*
TestPoints[a_, b_] := TrueQ[EuclideanDistance[a[[1]], b[[1]]] < 2.5*(a[[2]] + b[[2]])];


Clear[findvectorfrompoints];
Options[findvectorfrompoints] = {"UseImageCenter" -> True};
findvectorfrompoints[points_, dimensions_, OptionsPattern[]] := Module[
	{center, u, v},
	center = findcenterpoint[points, dimensions, "Method"->"Mean"];
	v = findendpoint[points, findcenterpoint[points, dimensions, "Method"->"ImageCenter"]];
	{center, v}
];

Clear[findcenterpoint];
Options[findcenterpoint] = {"Method" -> Automatic};
findcenterpoint[points_, dimensions_, OptionsPattern[]] := Block[
	{},
	Switch[
		OptionValue["Method"],
		"Mean",
			Mean /@ {points[[1]][[All,1,1]], points[[1]][[All,1,-1]]},
		(* "Edges", *)(* Larger scope problem *)
		"ImageCenter" | _,
			dimensions/2
	]
];

findendpoint[pts_,center_] := Block[
	{points, xsort, ysort, ptindx, meanuse = 8},
	points = First[pts];
	xsort = With[
		{sorted = Sort[points[[All, 1, 1]]]},
		{
			({1, 1} -> {Mean[Take[sorted, meanuse]], Abs[center[[1]] - Mean[Take[sorted, meanuse]]]}),
			({1, -1} -> {Mean[Take[sorted, -meanuse]], Abs[center[[1]] - Mean[Take[sorted, -meanuse]]]})
     	}
	];
	ysort = With[
		{sorted = Sort[points[[All, 1, -1]]]},
		{
			({-1, 1} -> {Mean[Take[sorted, meanuse]], Abs[center[[-1]] - Mean[Take[sorted, meanuse]]]}),
			({-1, -1} -> {Mean[Take[sorted, -meanuse]], Abs[center[[-1]] - Mean[Take[sorted, -meanuse]]]})
		}
	];
	ptindx = Reverse[SortBy[Flatten[{xsort, ysort}], #[[-1, -1]] &]][[1, 1]];

	Insert[
		{Reverse[SortBy[Flatten[{xsort, ysort}], #[[-1, -1]] &]][[1, 2, 1]]}, 
		Mean[Take[SortBy[points[[All, 1]], #[[(ptindx // First)*(1)]] &][[All, (ptindx // First)*(-1)]], (ptindx // Last)*meanuse]],
		(ptindx // First)*(-1)
    ]
]
  
Clear[findpointgroup];
findpointgroup[img_, num_: 1] := Block[
   {points, relations, index, g, gr, minradius = 0},
	points = DeleteDuplicates[
		Map[
			{Floor /@ #[[1]], #[[2]], #[[3]], #[[4]]} &,
			Select[
				Join @@ (Function[s, ImageKeypoints[MaxDetect[img, s], {"Position", "Scale", "Orientation", "ContrastSign"}]] /@ Range[.38, .58, 0.04]),
				(#[[4]] =!= 1 && (#[[2]]*2.5) > minradius) &
			]
		]
	];
   
	relations = {};
	If[TestPoints[#1, #2], AppendTo[relations, {#1 -> #2, #2 -> #1}]] & @@@ Tuples[points, 2];
	index = Rule @@@ Transpose[{Range[Length[points]], points}];
	relations = DeleteCases[DeleteDuplicates[Flatten[relations]], HoldPattern[x_ -> x_]] /. (Reverse /@ index);

	g = Graph[relations];
	gr = Quiet[Gather[Range[Length[points]], TrueQ[GraphDistance[g, #1, #2] < Infinity] &]];
	Take[Reverse[SortBy[gr, Length]], num] /. index
];

OverlayCounterVector[img_] := Block[
	{points, vector},
	points = findpointgroup[img, 1] // Quiet;
	vector = findvectorfrompoints[points, ImageDimensions[img]];
	Show[
		img,
		Graphics[
			Append[
				Table[{{Red, Circle[p[[1]], p[[2]]*2.5]}}, {p, points[[1]]}], 
				{Green, Thick, Arrow[vector]}
			]
		]
	]
]

CounterValue[img_, spin_] := Block[
	{points, v, w},
	points = findpointgroup[img, 1] // Quiet;
	v = findvectorfrompoints[points, ImageDimensions[img]];
	w = Normalize[v[[2]] - v[[1]]];
	If[
		spin*First[w] > 0,
		VectorAngle[w, {0, 1}],
		(2*Pi) - VectorAngle[w, {0, 1}]]*(10/(2*Pi))
]

*)

End[] (* End Private Context *)

EndPackage[]