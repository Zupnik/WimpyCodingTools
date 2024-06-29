Package["WimpyCodingTools`"]

PackageExport[ZupnikXML]
PackageExport[CreateSVGData]
CreateSVGData[routeData_]:= Module[
    {fullGraphicToProcess, graphicsToProcess, groupByStyle, rebuiltXmls, gTags, idsToAttach, obj, droppedFirstLine, fullyBuiltXML},
    graphicsToProcess = seperateGraphics[routeData];

    (* we are getting Ids of all the Names of the Wimpys *)
    idsToAttach = routeData["TravelDirections"][[All, "From", "Name"]];

    fullGraphicToProcess = Show[Sequence @@ (graphicsToProcess // Values)];

    (* We assume that the order of drawing is the same as in the Show when Exporting to SVG *)
    groupByStyle = DeleteCases[
        GroupBy[Cases[ImportString[ExportString[Show[fullGraphicToProcess], "SVG"], "XML"], XMLElement["path", ___], {0, Infinity}],
        Lookup[#[[2]], "style"]&], HoldPattern["style" -> _], {0, Infinity}
    ];
    (* Rebuild the XMLs *)
    (* rebuiltXmls = Normal[groupByStyle]/.x_XMLElement :> ExportString[x, "XML"]; *)
    
    gTags = MapThread[
        XMLElement["g", 
            {"className" -> #1}, 
            With[{paths=#2},If[#1==="geoMarker", MapThread[prependElement[{"id"->#1, "onClick" -> ZupnikXML["{handlePathClick}"]}, #2]&, {idsToAttach, Echo@paths}],paths]]
        ]&, 
        {Keys[graphicsToProcess], Normal[groupByStyle][[All,2]]}
    ];

    obj = XMLObject[
        "Document"][{XMLObject["Declaration"]["Version" -> "1.0", 
        "Encoding" -> "UTF-8"]}, 
        XMLElement[
        "svg", {"width" -> "216pt", "height" -> "420pt", 
        "viewBox" -> "0 0 216 420", "version" -> "1.1"}, gTags], {}];

    (* XML exporting doesn't support the ability to have a field value outside of a string, so we force it like so *)
    (* fullyBuiltXML = StringReplace[ExportString[obj,"XML"],"'" <> uuid <> "'" -> "{handlePathClick}"]; *)
    (* This probably doesn't work under all conditions, but will do for now *)
    fullyBuiltXML = StringReplace[ExportString[obj,"XML"],Shortest["'ZupnikXML["~~x__~~"]'"]:>x];
    (* This could be the worst thing I've ever seen. *)
    droppedFirstLine = StringJoin[Riffle[Drop[StringSplit[fullyBuiltXML, "\n"], 1], "\n"]]
    (* Thread[Keys[graphicsToProcess] -> rebuiltXmls[[All,2]]] *)
     
]

(* prependElement[id_String, XMLElement[x_, y_, z_]]:= XMLElement[x, Prepend[y, "id" -> id], z] *)

prependElement[itemsToPrepend_List, XMLElement[x_, y_, z_]]:= XMLElement[x, Fold[Prepend, y, itemsToPrepend], z]

(* A simple thing which removes all but 1 point from the GeoPosition lists, needs improvement *)
simplifyTravelDirections[travel_TravelDirectionsData]:=travel["TravelPath"]/.x_GeoPosition:>GeoPosition[x[[1]][[All,1]]]

seperateGraphics[routeData_]:=Module[
    {travelDirections, allGeoPositions},
    
    travelDirections = routeData["TravelDirections"][[All, "TravelDirections"]];
    allGeoPositions = routeData["TravelDirections"][[All, "From", "GeoPosition"]];
    (* This is also the drawing order, do not rearrange the Association *)
    <|
        "mapGraphic" -> makeMapGraphic[],
        "travelDirections" -> makeTravelDiretionsGraphics[simplifyTravelDirections/@travelDirections], 
        "geoMarker" -> makeGeoMarkerGraphics[allGeoPositions]
    |>
]


makeGeoMarkerGraphics[allGeoPositions_]:=GeoGraphics[
 	{
  		GeoMarker[#, Graphics[{RGBColor["#d62e22"], Disk[]}], 
     "Scale" -> Scaled[0.02]] & /@ allGeoPositions
  		
  	}
 	,
 	GeoBackground -> None
 ]

 makeTravelDiretionsGraphics[travelDirections_]:=GeoGraphics[
 	{
  		Style[#, Thick, Black] & /@ (travelDirections)
  		
  	}
 	,
 	GeoBackground -> None
 ]

 makeMapGraphic[]:= mapGraphic=GeoGraphics[
	{
		EdgeForm[Black],FaceForm[{Gray,Opacity[1]}],Polygon[Entity["Island", "GreatBritain"]]
	}
	,
	GeoBackground->None(*,
	GeoRange\[Rule]Quantity[2,"Miles"],
	GeoCenter\[Rule]London	city*)
	
]