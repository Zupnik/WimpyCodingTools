Package["WimpyCodingTools`"]

PackageExport[$WimpyData]
PackageExport[RouteData]

PackageExport[CreateRouteData]
PackageExport[TourData]
PackageExport[WimpyTourGraphic]
PackageExport[GetWimpyById]
PackageExport[GetNearestWimpy]
PackageExport[RouteGrid]
PackageExport[ImportWimpyData]

GeneralUtilities`SetUsage["$WimpyData will call once per session to get the data from the Wimpy API"]
GeneralUtilities`SetUsage["CreateRouteData gets all the route data of your Wimpy tour, and with the option \"StartLocation\" you can specify where you want to start"]
GeneralUtilities`SetUsage["TourData[routeData$] does stuff"]
GeneralUtilities`SetUsage["WimpyTourGraphic[routeData$]"]
GeneralUtilities`SetUsage["GetWimpyById[Id$] returns the Wimpy data for the given Id$"]
GeneralUtilities`SetUsage["GetNearestWimpy[location$]"]
GeneralUtilities`SetUsage["RouteGrid Creates a slideshow of all travel directions"]
GeneralUtilities`SetUsage["ImportWimpyData[x$]"]
GeneralUtilities`SetUsage["DeployWimpyCodingTools[] copies the paclet to $PacletDeploymentDirectory"]


PackageExport[StyleOpeningTimes]
PackageExport[StyleWimpyData]

$WimpyAPIUrl = "https://wimpy.uk.com/api/locations?all=true";

(* ::Text:: *)

(* Runs once per session *)
(* CONSIDER: Should this be a Dataset? *)
$WimpyData := Once[importWimpyData[getRawWimpyData[]]] 

$daysOfTheWeek = {"Sunday", "Monday", "Tuesday", "Wednesday", 
      "Thursday", "Friday", "Saturday"};

$dayKey = Association @@ MapThread[Rule, {Range[7], $daysOfTheWeek}];

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

(* Exposed function *)
ImportWimpyData[x_] := importWimpyData[x]

importWimpyData[resp_HTTPResponse] := importWimpyData[importJson[resp]]

importWimpyData[importedData_] := Module[
        {assoc},
		assoc = Association @@@ importedData;
        Map[
			<|
				
				"Id" -> Lookup[#,"id"],
				(* 
					This custom field gives the name of the Wimpy, as there are some in different locations,
					This custom field 175278 in one instance does not give a value, so return the city
				*)
				"Name"->With[{cust=Lookup[#["customFields"], "175278"]},
						If[MissingQ[cust], #["city"],cust]
					],
				"OpeningTimes" -> openingTimes[#["hours"]],
				"GeoPosition" -> makeGeoPosition[#],
				"Address" -> <|
					"Street" -> #["address"],
					"City" -> #["city"],
					"County" -> #["state"],
					"Postcode "-> #["zip"]
				|>
			|>&,
			assoc
		]
    ]

importJson[resp_HTTPResponse] := ImportString[resp["Body"], "JSON"];

split[str_String] := ToExpression /@ StringSplit[str, {":", ","}];

defaultWimpyOpeningTimes = <|# -><|"Open" -> "Closed", "Close" -> "Closed"|>|> & /@ $daysOfTheWeek

(* opening times are in the form "1:9:30:15:00,2:9:00:17:00,3:9:00:17:00,4:9:00:17:00,5:9:00:17:00,6:9:00:17:00,7:9:00:17:00"*)
openingTimes[str_String] := <|defaultWimpyOpeningTimes,Map[<|$dayKey[#[[1]]]-><|"Open"->TimeObject[#[[2;;3]]],"Close"->TimeObject[#[[4;;5]]]|>|>&,Partition[split[str],5]]|>

(* converts to the legacy format for the opening times *)
formatOpeningTimes[openingTimes_]:=List @@ AssociationMap[{#[[1]], #[[2, 1]], #[[2, 2]]} &, openingTimes]

makeGeoPosition[x_Association]:=
Module[
	{lat,lng},
	lat = Lookup[x,"yextRoutableLat"];
	lng = Lookup[x,"yextRoutableLng"];
	GeoPosition[{lat,lng}]
]

(* Doing the Tour *)


(* Makes tooltip for when hovering over a point *)
makeToolTipData[wimpyData_] := 
Text[Column[
   Prepend[wimpyData["Address"] // Values, 
    "Wimpy " <> wimpyData["Address"]["City"]]]
]

(* TODO: Have a better option for "Reverse". It would be better if they could choose to go anticlockwise or clockwise, 
	not have to look how the data is created first *)
Options[CreateRouteData] := {"WimpyData" :> $WimpyData, "StartLocation" -> None, "Reverse" -> False}

CreateRouteData[opts:OptionsPattern[]] := Module[
	{wimpyData = OptionValue["WimpyData"],
		wimpyTourLength, allGeoData, wimpyTourPositions, allRoutes, allRoutes2, allGeoRoutes, allTravelDirections,
		nearest = OptionValue["StartLocation"], data, reverse = OptionValue["Reverse"]
	},
	allGeoData = wimpyData[[All, "GeoPosition"]];
	{wimpyTourLength, wimpyTourPositions} = FindShortestTour[allGeoData];
	(* Reverse the direction of travel in case you want to do tour the other way around *)
	If[
		reverse === True,
		wimpyTourPositions = (wimpyTourPositions // Reverse)
	];

	(* TODO: Better description of what this does. Makes some skeletal structure to get route layout easier *)
	allRoutes = NestList[# + 1 &, {1, 2}, Length[wimpyTourPositions] - 2];
	(* Positions of routes *)
	allRoutePositions = (wimpyTourPositions[[#]] & /@ allRoutes);
	(* allGeoRoutes = (allGeoData[[#]] & /@ allRoutePositions); *)
	
	If[
		nearest =!= None,
		(* Check to see if nearest is a location? *)
		Module[
			{closestWimpy = GetNearestWimpy["Location" -> nearest], pos },

			pos = Position[allRoutePositions, Select[allRoutePositions, (wimpyData[[#[[1]]]]["GeoPosition"] === closestWimpy["GeoPosition"])&][[1]]][[1]];
			wimpyTourPositions = RotateLeft[wimpyTourPositions, pos - 1];
			(* Make it so that the first value in the result will be the place closest to nearest *)
			allRoutePositions = RotateLeft[allRoutePositions, pos - 1];
		]
	];

	allTravelDirections = With[
		{wimpyFrom = wimpyData[[#[[1]]]], wimpyTo = wimpyData[[#[[2]]]]},
		<|
			(* CONSIDER: This is a lazy implementation so that I don't have to go get the Id again *)
			"From"-> GetWimpyById[wimpyFrom["Id"]],
			"To" -> GetWimpyById[wimpyTo["Id"]],
			"TravelDirections" -> TravelDirections[{wimpyFrom["GeoPosition"],wimpyTo["GeoPosition"]}]
		|>
	] & /@ allRoutePositions;
	
	data = RouteData[<|
		"Length" -> wimpyTourLength, 
		"Tour" -> wimpyTourPositions, 
		"WimpysById" -> ((wimpyData[[#]]["Id"]&)/@wimpyTourPositions),
		"Routes" -> Map[wimpyData[[#]]["Id"] &, allRoutePositions, {2}],
		"TravelDirections" -> allTravelDirections
	|>]
]

(* TODO: Make route data a nice box *)
RouteData[x_]["Properties"] := x // Keys

RouteData[x_][y_] := x[y]


Options[GetWimpyById] := {"WimpyData" :> $WimpyData}

GetWimpyById[id_, opts:OptionsPattern[]]:= Module[
	{wimpyData = OptionValue["WimpyData"], cases},
	cases = Cases[wimpyData, x_Association /; x["Id"] === id];
	(* *)
	If[cases==={}, Return[Missing[]]];
	(* Id should be unique and result should either be {} or a single value in a list *)
	cases[[1]]
]

PackageExport[GetGeoData]
Options[GetGetData] := {"WimpyData" :> $WimpyData}
GetGeoData[fromId_,toId_] := Module[{wimpyData = OptionValue["WimpyData"], wimpyFrom = GetWimpyById[fromId], wimpyTo = GetWimpyById[toId], wimpyFromGeo, wimpyToGeo, td, geoPositions}, 
	{wimpyFrom, wimpyTo};
	wimpyFromGeo = wimpyFrom["GeoPosition"];
	wimpyToGeo = wimpyTo["GeoPosition"];
	td = TravelDirections[{wimpyFromGeo, wimpyToGeo}];
	geoPositions = GeoPosition /@ Flatten[td["TravelPath"][[1, 1]], 1];
	<|
		"From" -> wimpyFromGeo,
		"To" -> wimpyToGeo,
		"TravelDirections" -> td,
		"AllGeoPositions" -> GeoPosition /@ Flatten[td["TravelPath"][[1, 1]], 1],
		"AllGeoPaths" -> Table[GeoPath[{geoPositions[[i]], geoPositions[[i + 1]]}], {i, Length[geoPositions] - 1}]
	|>
	

]

Options[GetNearestWimpy] := {"Location" :> $GeoLocation, "WimpyData" :> $WimpyData}

GetNearestWimpy[opts:OptionsPattern[]] := Module[
	{loc = OptionValue["Location"], wimpyData = OptionValue["WimpyData"], nearest},
	
	nearest = GeoNearest[wimpyData[[All, "GeoPosition"]], loc];

	cases = Cases[wimpyData, x_Association /; x["GeoPosition"] === nearest[[1]]];

	cases[[1]]
]

(* TODO: Improve Interpretation *)
StyleOpeningTimes[times_] := TextGrid[Prepend[formatOpeningTimes[times],{"Day","Open","Close"}],Frame->All]
(* StyleOpeningTimes[times_] := Interpretation[TextGrid[Prepend[times,{"Day","Open","Close"}],Frame->All],times] *)

StyleWimpyData[wimpy_] := TextGrid[
	{
		{"Id:", wimpy["Id"]},
		{"Name:", wimpy["Name"]},
		{"Postcode:", wimpy["Address","Postcode"]},
		{Pane[#, ImageSize -> 270] &@StyleOpeningTimes[wimpy["OpeningTimes"]],SpanFromLeft}
	}
]

Options[WimpyTourGraphic] = {
	"CompleteRoute" -> True, 
	"ShowSpecialMarkers" -> False,
	"Dynamic" -> False
}

WimpyTourGraphic[routeData_, opts:OptionsPattern[]] := Module[
	{markers, completeRoute = OptionValue["CompleteRoute"], value, head},
	(* Markers for each Wimpy *)
	value = If[
		completeRoute === True,
		-1,
		-2
	];

	markers = Tooltip[
		GeoMarker[#GeoPosition, Graphics[{RGBColor["#d62e22"], Disk[]}], "Scale" -> Scaled[0.02]], 
		makeToolTipData[#]
	] & /@ routeData["TravelDirections"][[All, "From"]];

	(* CONSIDER: Having Options "StartPin" "EndPin" *)
	If[
		OptionValue["ShowSpecialMarkers"] === True,
		markers[[1,1]] = ReplacePart[markers[[1,1]], {2 -> $startPin, 3 -> Sequence["Alignment" -> Bottom, "Scale" -> Scaled[0.05]]}];
		markers[[-1,1]] = ReplacePart[markers[[-1,1]], {2 -> $endPin, 3 -> Sequence["Alignment" -> Bottom, "Scale" -> Scaled[0.05]]}];
	];
	
	(* Travel lines for each Wimpy *)
	travel = Style[Line[#], Thick, Black] & /@ (routeData["TravelDirections"][[All, "TravelDirections"]])[[1;;value]];
	head = If[OptionValue["Dynamic"] === True,
		DynamicGeoGraphics,
		GeoGraphics
	];

	head[{travel, markers}, ImageSize -> Full,
		Sequence @@ FilterRules[{opts}, Options[GeoGraphics]]
	]
]

(* Convinience function which will need refactoring. Makes *)
WimpyTourGraphicalInformation[routeData_, opts:OptionsPattern[]]:=1


makeGeoMarker[wimpy_] := Tooltip[GeoMarker[wimpy["GeoPosition"], Graphics[{RGBColor["#d62e22"], Disk[]}](* , "Scale" -> 0.15 *), "Scale" -> Scaled[0.05]], makeToolTipData[wimpy]]
makeTravelLine[travelDirections_] := Style[Line[travelDirections], Thick, Black]

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
	TextGrid@{
		(* Headers *)
		{"Graphic","From","To"},
		{assoc["GeoGraphic"], assoc["OpeningTimesFrom"], assoc["OpeningTimesTo"]}
	}

PackageExport[DeployWimpyCodingTools]

$PacletDeploymentDirectory = CloudObject["Wimpy/WimpyCodingTools"]
$PacletDirectory = FileNameDrop[$InputFileName]

Options[DeployWimpyCodingTools] := {OverwriteTarget -> False}
(*TODO: Fix Overwrite *)
DeployWimpyCodingTools[opts:OptionsPattern[]] := Module[
	{},
	If[
		DirectoryQ[$PacletDeploymentDirectory] && OptionValue[OverwriteTarget] === True,
		DeleteDirectory[$PacletDeploymentDirectory, DeleteContents -> True]
	];

	CopyDirectory[$PacletDirectory, $PacletDeploymentDirectory]

]
	

(* DeleteDirectory[CloudObject[
 "https://www.wolframcloud.com/obj/az/Wimpy/WimpyCodingTools"], 
 DeleteContents -> True] *)