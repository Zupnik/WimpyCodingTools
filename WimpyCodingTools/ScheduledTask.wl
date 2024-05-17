BeginPackage["WimpyCodingTools`ScheduledTask`"]

Begin["`Private`"]

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

				wimpy1Match = Select[wimpyBefore,#["Id"]===id&][[1]];
				wimpy2Match = Select[wimpyAfter,#["Id"]===id&][[1]];

				If[
					wimpy1Match["OpeningTimes"] === wimpy2Match["OpeningTimes"],
					(*No change *)
					Nothing,
					Grid[
                        {
                            {"Wimpy "<>id,SpanFromLeft},
                            {"Before","After"},
                            {
                                ds1 = Dataset[wimpyBefore][Select[#Id === id&], "OpeningTimes"],
                                ds2 = Dataset[wimpyAfter][Select[#Id === id&], "OpeningTimes"]
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
	bef,
	WimpyToken[bef, aft]
]/. x_WimpyToken :> styleTimeChange[x]

styleTimeChange[x_WimpyToken] := Row[{Style[DateString[x[[1]]], 
   FontVariations -> {"StrikeThrough" -> True}, 
   Background -> LightRed], " ", 
  Style[DateString[x[[2]]], 
   Background -> LightGreen]}]

End[]

EndPackage[]
