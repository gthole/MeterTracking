BeginPackage["MeterTracking`Content`WeatherUtilities`"];

CurrentWeather::usage = "Queries Yahoo! Weather for the temperature and conditions in a given location, with default 02122."

Begin["Private`"]


CurrentWeather[] := CurrentWeather["USMA0261"];
CurrentWeather[loc_String] := Block[
	{query},
	query = Cases[
		Quiet@Import[
  			"http://weather.yahooapis.com/forecastrss?p="<>loc<>"&u=f", 
  			"XML"
  		], 
		XMLElement[
			{"http://xml.weather.yahoo.com/ns/rss/1.0", "condition"}, 
			log_, 
			{}
		] :> log, 
		Infinity, 
		1
	];
	
	If[MatchQ[query, {{"text" -> _String, _, "temp" -> _String, _}}],
		query = First[query];
		{"text" /. query, FromDigits["temp" /. query]},
		{Missing["NotAvailable"], Missing["NotAvailable"]}
	]
];

End[]

EndPackage[]