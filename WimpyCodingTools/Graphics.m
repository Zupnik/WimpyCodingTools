Package["WimpyCodingTools`"]

PackageExport[CreateSVGData]
CreateSVGData[routeData_]:= Module[
    {fullGraphicToProcess, graphicsToProcess, groupByStyle, rebuiltXmls, gTags},
    graphicsToProcess = seperateGraphics[routeData];

    fullGraphicToProcess = Show[Sequence @@ (graphicsToProcess // Values)];

    (* We assume that the order of drawing is the same as in the Show when Exporting to SVG *)
    groupByStyle = DeleteCases[
        GroupBy[Cases[ImportString[ExportString[Show[fullGraphicToProcess], "SVG"], "XML"], XMLElement["path", ___], {0, Infinity}],
        Lookup[#[[2]], "style"]&], HoldPattern["style" -> _], {0, Infinity}
    ];
    (* Rebuild the XMLs *)
    (* rebuiltXmls = Normal[groupByStyle]/.x_XMLElement :> ExportString[x, "XML"]; *)
    
    gTags = MapThread[
        XMLElement["g", {"id" -> #1}, #2]&, 
        {Keys[graphicsToProcess], Normal[groupByStyle][[All,2]]}
    ];
    XMLObject[
        "Document"][{XMLObject["Declaration"]["Version" -> "1.0", 
        "Encoding" -> "UTF-8"]}, 
        XMLElement[
        "svg", {"width" -> "216pt", "height" -> "420pt", 
        "viewBox" -> "0 0 216 420", "version" -> "1.1"}, gTags], {}]
    (* Thread[Keys[graphicsToProcess] -> rebuiltXmls[[All,2]]] *)
     
]


seperateGraphics[routeData_]:=Module[
    {},
    travelDirections = routeData["TravelDirections"][[All, "TravelDirections"]];
    allGeoPositions = routeData["TravelDirections"][[All, "From", "GeoPosition"]];

    <|
        "mapGraphic" -> makeMapGraphic[],
        "travelDirections" -> makeTravelDiretionsGraphics[travelDirections], 
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
  		Style[Line[#], Thick, Black] & /@ (travelDirections)
  		
  	}
 	,
 	GeoBackground -> None
 ]

 makeMapGraphic[]:= mapGraphic=GeoGraphics[
	{
		EdgeForm[Black],FaceForm[{Gray,Opacity[1]}],Polygon[Entity["Country","UnitedKingdom"]]
		
	}
	,
	GeoBackground->None(*,
	GeoRange\[Rule]Quantity[2,"Miles"],
	GeoCenter\[Rule]London	city*)
	
]