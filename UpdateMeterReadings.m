BeginPackage["MeterTracking`UpdateMeterReadings`"];

Needs["MeterTracking`Content`ReadMeter`"];
Needs["MeterTracking`Content`DataPlotting`"];
Needs["MeterTracking`Content`WeatherUtilities`"];

(*
	ReadMeter (Done)
	GetWeather (Done)
	GeneratePowerViz (In Progress)
	RunScript (In Progress)
	Cron ()
	Webpage Template ()
*)

UpdateMeterReadings::usage = "UpdateMeterReadings[datadir, outputdir] reads a meter from images stored in datadir, and updates the tracked data."
UpdateMeterScript::usage = "UpdateMeterScript[datadir, outputdir] updates meter readings and outputs visualizations."

Begin["Private`"]

Options[UpdateMeterReadings] := {"OutputNewDataQ" -> False, "BackupQ" -> True};

UpdateMeterReadings[d_, o_, OptionsPattern[]] := Block[
	{start, datafile, img, mtr, wthr, prev, new, entry},
	
	start = DateList[][[1;;4]];
	datafile = ToFileName[{d},"meterdata.m"];
	store = ToFileName[{d, "stored"}];
	
	(* Validate directories and files. *)
	Check[
		If[!DirectoryQ[d], Abort[]];
		If[!FileExistsQ[datafile], Abort[]];
		If[!DirectoryQ[store], CreateDirectory[store]],
		Abort[]
	];
	
	(* Get image file from separate webcam process. *)
	file = FileNames["*camfile*.jpg", d];
	If[file =!= {}, file = Last[file], Abort[]];
	Print["Using file: "<>file];
	
	(* Read meter from image. *)
	img = Import[file,"JPG"];
	mtr = ReadMeter[img];
	Print["Reading: "<>ToString[mtr]];
	If[!NumberQ[mtr] || mtr < 55336, mtr = Missing["IncorrectReading"]];
	Export[
		ToFileName[{o},"meterimage.jpg"],
		ImageCrop[img, {300, 75}],
		"JPG"
	];
	Clear[img];
	
	If[(TrueQ[OptionValue["BackupQ"]] || mtr === Missing["IncorrectReading"]),
		RenameFile[
			file, 
			ToFileName[
				{d,"stored"},
				DateString[DateList[], {"Year", "Month", "Day", "Hour"}] <> ".jpg"
			]
		],
		DeleteFile[file]
	];
	
	(* Collect weather data from service. *)
	wthr = CurrentWeather[];
	If[wthr === $Failed, Abort[]];
	Print["Weather Reading: "<>ToString[wthr]];
	
	(* Import previous data file. *)
	(* TODO: Do this with streams to save memory? *)
	prev = Get[datafile];
	
	(* Create entry *)
	diff = mtr - prev[[-1,2]];
	Print["Energy diff: "<>ToString[diff]];
	
	(* Error Handling.  Insert Missing places. *)
	If[
		diff < 0 || diff > 9.5 || !FreeQ[diff, _Missing],
		diff = Missing["IncorrectReading"]
	];
	If[
		First[DateDifference[start, prev[[-1,1]], "Minute"]] > 62,
		diff = Missing["IncorrectTimeInterval"]
	];
	Print["Stored: "<>ToString[diff]];
	
	entry = Join[
		{start, mtr, diff}, 
		wthr
	];

	(* TODO: Entry validation!!! *)

	(* Append new reading. *)
	new = Append[prev, entry];
	
	(* Store and backup w/ email. *)
	Put[new, datafile];
	If[
		DateString[DateList[], {"DayName", " ", "Hour24"}] == "Sunday 23",
		SendMail[
			"To" -> "",
			"From" -> "",
			"Subject" -> "MeterBackup: "<>DateString[DateList[], {"Year", "Month", "Day"}],
			"Body" -> "Yes",
			"Server" -> "",
			"UserName" -> "",
			"Password" -> "",
			"Attachments" -> {datafile}
		]
	];
	If[
		TrueQ[OptionValue["OutputNewDataQ"]],
		new = DeleteCases[new, {__, _Missing, __}];
		new
	]
];


UpdateMeterScript[datadir_, outdir_] := Block[
	{new},
	(* Read and Update. *)
	new = UpdateMeterReadings[datadir, outdir, "OutputNewDataQ" -> True];

	(* Write visualizations to server. *)
	WriteDataPlots[new, outdir, All]
];

End[]

EndPackage[]