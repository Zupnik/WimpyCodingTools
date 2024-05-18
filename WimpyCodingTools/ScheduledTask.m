Package["WimpyCodingTools`"]

PackageExport[WimpyDifferences]


(* WimpyDifferences::usage = "WimpyDifferences[wimpyData1, wimpyData2] returns a list of differences between the two sets of wimpy data. The differences are returned as a list of associations with the keys 'WimpysDeleted' and 'WimpysAdded'"; *)

(* Imported Symbols *)
(* WimpyCodingTools`GetWimpyById
WimpyCodingTools`$WimpyData *)

WimpyDifferences[wimpyData1_, wimpyData2_] := Module[
    {wimpyRestaraunts, wimpyOpeningTimes, wimpysOpened, wimpysClosed, wimpysChanged},
    wimpysChanged = compareWimpyRestaraunts[wimpyData1, wimpyData2];
    wimpysOpened = wimpysChanged["wimpysAdded"];
    wimpysClosed = wimpysChanged["WimpysDeleted"];
    
    (* wimpyRestaraunts = compareWimpyRestaraunts[wimpyData1, wimpyData2];
    wimpyOpeningTimes = compareOpeningTimes[wimpyData1, wimpyData2]; *)
    Grid[{
        {"Wimpys Opened!", GetWimpyById[#,"WimpyData" -> wimpyData2]["Name"]&/@wimpysOpened},
        (* Note: If a Wimpy has closed, the Id has to be taken from the old data and not the new *)
        {"Wimpys Closed!", GetWimpyById[#,"WimpyData" -> wimpyData1]["Name"]&/@wimpysClosed},
        {SlideView[compareOpeningTimes[wimpyData1, wimpyData2]], SpanFromLeft}
        }
    ]
]



(* Returns any Wimpys which have been deleted or added  *)
compareWimpyRestaraunts[wimpyData1_, wimpyData2_] := Module[
    {wimpysAdded, wimpysDeleted},
    wimpysAdded = DeleteCases[wimpyData2[[All, "Id"]], Alternatives @@ wimpyData1[[All, "Id"]]];
    wimpysDeleted = DeleteCases[wimpyData1[[All, "Id"]], Alternatives @@ wimpyData2[[All, "Id"]]];
    <|"WimpysDeleted" -> wimpysDeleted, "wimpysAdded" -> wimpysAdded|>
]

compareOpeningTimes[wimpy1_, wimpy2_] := Module[
    {wimpyBefore = wimpy1, wimpyAfter = wimpy2},
	Map[    
		With[
            {id=#},
			Module[
                {wimpy1Match, wimpy2Match},

				wimpy1Match = Select[wimpyBefore,#["Id"] === id&][[1]];
				wimpy2Match = Select[wimpyAfter,#["Id"] === id&][[1]];

				If[
					wimpy1Match["OpeningTimes"] === wimpy2Match["OpeningTimes"],
					(*No change *)
					Nothing,
					Grid[
                        {
                            {"Wimpy "<> WimpyCodingTools`GetWimpyById[id]["Name"]},
                            {
                                Dataset[Merge[{wimpy1Match["OpeningTimes"], wimpy2Match["OpeningTimes"]}, mergeOpeningTimes[#[[1]],#[[2]]]&]]
                                (* ds1 = Dataset[wimpy1Match["OpeningTimes"]],
                                ds2 = Dataset[wimpy2Match["OpeningTimes"]] *)
                            }
                        }
                    ]
				]
			]
		]
		&
		,
		Intersection[wimpy1[[All, "Id"]], wimpy2[[All, "Id"]]]
	]
]

(* mergeOpeningTimes[bef_, after_] := 1 *)

mergeOpeningTimes[time1:<|"Open" -> open1_, "Close" -> close1_|>, time:<|"Open" -> open2_, "Close" -> close2_|>] := <|"Open"->tokenizeTimeDifference[open1,open2], "Close" -> tokenizeTimeDifference[close1,close2]|>

tokenizeTimeDifference[bef_, aft_] := If[
	bef === aft,
	DateString[bef],
	WimpyToken[bef, aft]
]/. x_WimpyToken :> styleTimeChange[x]

styleTimeChange[x_WimpyToken] := Row[{Style[DateString[x[[1]]], 
   FontVariations -> {"StrikeThrough" -> True}, 
   Background -> LightRed], " ", 
  Style[DateString[x[[2]]], 
   Background -> LightGreen]}]


$WimpyChangeMonitorScheduledTask = CloudObject["Wimpy/WimpyChangeMonitorScheduledTask"];


createScheduledTask[opts:OptionsPattern[]] :=  CloudDeploy[
    runComparison[]
    ,
    $WimpyChangeMonitorScheduledTask
]


runComparison[]:=$WimpyData

