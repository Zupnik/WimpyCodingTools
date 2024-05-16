(* ::Package:: *)

BeginPackage["WimpyCodingTools`"]

$WimpyData::usage="WimpyData will call once per session to get the data from the Wimpy API"
RouteData::usage="RouteData gets all the route data of your Wimpy tour, and with the option \"StartLocation\" you can specify where you want to start"
TourData::usage="TourData[routeData]"
WimpyTourGraphic::usage="WimpyTourGraphic[routeData]"
GetWimpyById::usage="GetWimpyById[Id]"
GetNearestWimpy::usage="GetNearestWimpy[location]"
RouteMap::usage="RouteMap[routeData]"
RouteGrid::usage="Creates a slideshow of all travel directions"


StyleOpeningTimes
StyleWimpyData

Begin["`Private`"]

$WimpyAPIUrl = "https://wimpy.uk.com/api/locations?all=true";

(* ::Text:: *)

(* Runs once per session *)
$WimpyData := Once[importWimpyData[getRawWimpyData[]]] 

$daysOfTheWeek = {"Sunday", "Monday", "Tuesday", "Wednesday", 
      "Thursday", "Friday", "Saturday"};

$dayKey = 
  Association @@ 
   MapThread[
    Rule, {Range[7], {"Sunday", "Monday", "Tuesday", "Wednesday", 
      "Thursday", "Friday", "Saturday"}
     }];

markerText[text_] := 
 Style[text, Black, Bold, FontSize -> Scaled[0.4], 
  FontFamily -> "Comic Sans MS"]

$startPin = 
 Graphics[
  GraphicsGroup[{RGBColor["#56db46"], Text[markerText["S"], {0, 3}], 
    FilledCurve[{{Line[
        Join[{{0, 0}}, ({Cos[#1], 3 + Sin[#1]} &) /@ 
          Range[-((2 \[Pi])/20), \[Pi] + (2 \[Pi])/20, \[Pi]/20], {{0,
            0}}]]}, {Line[(0.5 {Cos[#1], 6 + Sin[#1]} &) /@ 
         Range[0, 2 \[Pi], \[Pi]/20]]}}]}]]

$endPin = 
 Graphics[
  GraphicsGroup[{RGBColor["#e65e3c"], Text[markerText["E"], {0, 3}], 
    FilledCurve[{{Line[
        Join[{{0, 0}}, ({Cos[#1], 3 + Sin[#1]} &) /@ 
          Range[-((2 \[Pi])/20), \[Pi] + (2 \[Pi])/20, \[Pi]/20], {{0,
            0}}]]}, {Line[(0.5 {Cos[#1], 6 + Sin[#1]} &) /@ 
         Range[0, 2 \[Pi], \[Pi]/20]]}}]}]]

getRawWimpyData[] := $RawWimpyData = URLRead[$WimpyAPIUrl]

emptyOpeningTimes[] := <|# -> "Closed" & /@ $daysOfTheWeek|>;

importWimpyData[rawWimpyData_] := Module[
        {readLocationData = rawWimpyData, importedData},
        importedData = Association @@@ ImportString[readLocationData["Body"], "JSON"];
        Map[
			<|
				
				"Id"->#["id"],
				(* 
					This custom field gives the name of the Wimpy, as there are some in different locations,
					This custom field 175278 in one instance does not give a value, so return the city
				*)
				"Name"->With[{cust=Lookup[#["customFields"], "175278"]},
						If[MissingQ[cust], #["city"],cust]
					],
				"OpeningTimes"->openingTimes[#["hours"]],
				"GeoPosition"->makeGeoPosition[#],
				"Address"-><|
					"Street"->#["address"],
					"City"->#["city"],
					"County"->#["state"],
					"Postcode"->#["zip"]
				|>
			|>&,
			importedData
		]
    ]


split[str_String] := ToExpression /@ StringSplit[str, {":", ","}];


openingTimes[str_String]:=Map[{$dayKey[#[[1]]],TimeObject[#[[2;;3]]],TimeObject[#[[4;;5]]]}&,Partition[split[str],5]]

makeGeoPosition[x_Association]:=
Module[
	{lat,lng},
	lat = Lookup[x,"yextRoutableLat"];
	lng = Lookup[x,"yextRoutableLng"];
	GeoPosition[{lat,lng}]
]

(* Doing the Tour *)


(* Makes tooltip for when hovering over a point *)
makeToolTip[wimpyData_] := 
 Text[Column[
   Prepend[wimpyData["Address"] // Values, 
    "Wimpy " <> wimpyData["Address"]["City"]]]]


Options[RouteData]:={"WimpyData" :> $WimpyData,"StartLocation" -> None}


RouteData[opts:OptionsPattern[]] := Module[
	{wimpyData = OptionValue["WimpyData"],
		wimpyTourLength, allGeoData, wimpyTourPositions, allRoutes, allRoutes2, allGeoRoutes, allTravelDirections,
		nearest = OptionValue["StartLocation"], data
	},
	allGeoData = wimpyData[[All, "GeoPosition"]];
	{wimpyTourLength, wimpyTourPositions} = FindShortestTour[allGeoData];
	(* TODO: Better description of what this does. Makes some skeletal structure to get route layout easier *)
	allRoutes = NestList[# + 1 &, {1, 2}, Length[wimpyTourPositions] - 2];
	(* Positions of routes *)
	allRoutePositions = (wimpyTourPositions[[#]] & /@ allRoutes);
	(* allGeoRoutes = (allGeoData[[#]] & /@ allRoutePositions); *)
	
	If[
		nearest =!= None,
		(* Check to see if nearest is a location? *)
		Module[
			{cloestWimpy = GetNearestWimpy["Location"->nearest], pos },

			pos = Position[allRoutePositions,Select[allRoutePositions,(wimpyData[[#[[1]]]]["GeoPosition"] === cloestWimpy["GeoPosition"])&][[1]]][[1]];
			(* Make it so that the first value in the result will be the place closest to nearest *)
			allRoutePositions = RotateLeft[allRoutePositions, pos - 1];
		]
	];

	allTravelDirections = With[
		{wimpyFrom = wimpyData[[#[[1]]]],wimpyTo = wimpyData[[#[[2]]]]},
		<|
			"From"-> GetWimpyById[wimpyFrom["Id"]],
			"To" -> GetWimpyById[wimpyTo["Id"]],
			"TravelDirections" -> TravelDirections[{wimpyFrom["GeoPosition"],wimpyTo["GeoPosition"]}]
		|>
	] & /@ allRoutePositions;
	
	data = <|
		"Length" -> length, 
		"Tour" -> wimpyTourPositions, 
		"TravelDirections" -> allTravelDirections
	|>
]

Options[GetWimpyById]:={"WimpyData" :> $WimpyData}

GetWimpyById[Id_, opts:OptionsPattern[]]:= Module[
	{wimpyData = OptionValue["WimpyData"], cases},
	cases = Cases[wimpyData, x_Association /; x["Id"] === Id];
	(* *)
	If[cases==={}, Return[Missing[]]];
	(* Id should be unique and result should either be {} or a single value in a list *)
	cases[[1]]
]




Options[GetNearestWimpy] := {"Location" :> $GeoLocation, "WimpyData" :> $WimpyData}

GetNearestWimpy[opts:OptionsPattern[]] := Module[
	{loc = OptionValue["Location"], wimpyData = OptionValue["WimpyData"], nearest},
	
	nearest = GeoNearest[wimpyData[[All, "GeoPosition"]], loc];

	cases = Cases[wimpyData, x_Association /; x["GeoPosition"] === nearest[[1]]];

	cases[[1]]
]

(* TODO: Improve Interpretation *)
StyleOpeningTimes[times_] := TextGrid[Prepend[times,{"Day","Open","Close"}],Frame->All]
(* StyleOpeningTimes[times_] := Interpretation[TextGrid[Prepend[times,{"Day","Open","Close"}],Frame->All],times] *)

StyleWimpyData[wimpy_] := TextGrid[
{
	{"Id:",wimpy["Id"]},
	{"Name:",wimpy["Name"]},
	{"Postcode:",wimpy["Address","Postcode"]},
	{Pane[#, ImageSize -> 270] &@StyleOpeningTimes[wimpy["OpeningTimes"]],SpanFromLeft}
}
]

Options[WimpyTourGraphic] = {"CompleteRoute" -> True, "ShowSpecialMarkers" -> False}

WimpyTourGraphic[routeData_, opts:OptionsPattern[]]:=Module[
	{markers, completeRoute = OptionValue["CompleteRoute"], value},
	(* Markers for each Wimpy *)

	value = If[
		completeRoute === True,
		-1,
		-2
	];

	markers = Tooltip[
		GeoMarker[#GeoPosition, Graphics[{RGBColor["#d62e22"], Disk[]}], "Scale" -> 0.15], 
		makeToolTip[#];
	] & /@ routeData["TravelDirections"][[All, "From"]];

	If[
		OptionValue["ShowSpecialMarkers"] === True,
		markers[[1,1]] = ReplacePart[markers[[1,1]], {2 -> $startPin, 3 -> Sequence["Alignment" -> Bottom, "Scale" -> 0.15]}];
		markers[[-1,1]] = ReplacePart[markers[[-1,1]], {2 -> $endPin, 3 -> Sequence["Alignment" -> Bottom, "Scale" -> 0.15]}];
	];
	markers[[1]];
	(* Travel lines for each Wimpy *)
	travel = Style[Line[#], Thick, Black] & /@ (routeData["TravelDirections"][[All, "TravelDirections"]])[[1;;value]];
	GeoGraphics[{travel, markers}, ImageSize -> Full]
]

makeGeoMarker[wimpy_] := Tooltip[GeoMarker[wimpy["GeoPosition"], Graphics[{RGBColor["#d62e22"], Disk[]}](* , "Scale" -> 0.15 *), "Scale" -> Scaled[0.05]], makeToolTip[wimpy]]
makeTravelLine[travelDirections_] := Style[Line[travelDirections], Thick, Black]

RouteMap[routeData_]:= Module[
	{data},
	(* Sort to put GeoMarkers on top*)
	data = Sort@Flatten[
			Map[
			{
				
				makeTravelLine[#["TravelDirections"]],
				makeGeoMarker[#["From"]]
			}&
			,
			routeData["TravelDirections"]
		]
	];

	GeoGraphics[data]
]

RouteGrid[routeData_]:= Module[
	{},
	data = ParallelMap[
			{
				"OpeningTimesFrom" -> StyleWimpyData[#["From"]],
				"OpeningTimesTo" -> StyleWimpyData[#["To"]],
				"GeoGraphic"->StyleGeoGraphic[#]}&
			,
			routeData["TravelDirections"][[1;;]]
		];

	SlideView[StyleRouteData/@(Association@@@data)]
]

StyleGeoGraphic[travel_]:=Grid[{
			List@GeoGraphics[{
					makeTravelLine[travel["TravelDirections"]],
					makeGeoMarker[travel["From"]],
					makeGeoMarker[travel["To"]]
			}],
			{travel["TravelDirections"]["TravelDistance"]}
}]

StyleRouteData[assoc_]:=
	(* Headers*)
	TextGrid@{
		{"Graphic","From","To"},
		{assoc["GeoGraphic"],assoc["OpeningTimesFrom"],assoc["OpeningTimesTo"]}
	}

(* ResourceFunction["AddCodeCompletion"]["TourData"][{"Length", "Tour", "TravelDirections"}] *)

(* Assets *)

wimpyLogo = Import["/Users/anthonyz/Dropbox/projects/Wimpy2024/Images/Logo_of_Wimpy.svg", "Graphics"];

(* TODO: Don't use ImageCrop as it converts it from a Graphics into an Image *)
(*finishFlag = Import["/Users/anthonyz/Downloads/Pasted-20230905-124733.svg","Graphics"];
finishFlag=ImageCrop[RemoveBackground[finishFlag]];

startFlag=Import["/Users/anthonyz/Downloads/Pasted-20230905-132023.svg","Graphics"];
startFlag=ImageCrop[RemoveBackground[startFlag]]*)

yourNearestWimpy[]:=
Module[
    {allGeoData = $wimpyData[[All, "GeoPosition"]], nearest},
    nearest =  Nearest[allGeoData, Here];
    GeoGraphics[{GeoMarker[nearest[[1]]],GeoMarker[Here]}]
]



(* ::Subtitle:: *)
(*Scheduled Task*)


End[]

EndPackage[]
