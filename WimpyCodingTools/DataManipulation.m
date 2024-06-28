(*TODO: Think about if this needs to be in a separate package *)

Package["WimpyCodingTools`"]


prepareWimpyDataForExport[]:=Query[All, {"GeoPosition" -> First, 
   "OpeningTimes" -> 
    Query[All, {"Open" -> DateString, 
      "Close" -> DateString}]}][$WimpyData]

PackageExport[ExportWimpyDataJson]
ExportWimpyDataJson[location_]:=Export[location, prepareWimpyDataForExport[]]