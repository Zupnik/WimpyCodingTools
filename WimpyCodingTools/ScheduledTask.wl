BeginPackage["WimpyCodingTools`ScheduledTask`"]

Begin["`Private`"]

(* Returns any Wimpys which have been deleted or added  *)
compareWimpyRestaraunts[wimpyData1_, wimpyData2_] :=
    Module[{wimpysAdded, wimpysDeleted},
        wimpysAdded = DeleteCases[wimpyData2[[All, "Id"]], Alternatives @@ wimpyData1[[All, "Id"]]];
        wimpysDeleted = DeleteCases[wimpyData1[[All, "Id"]], Alternatives @@ wimpyData2[[All, "Id"]]];
        <|"WimpysDeleted" -> wimpysDeleted, "wimpysAdded" -> wimpysAdded|>
    ]


compareOpeningTimes[wimpy1_, wimpy2_]:= Module[{wimpyBefore = wimpy1, wimpyAfter = wimpy2},
	Map[
		With[{id=#},
			Module[{wimpy1Match, wimpy2Match},
				wimpy1Match=Select[wimpyBefore,#["Id"]==id&][[1]];
				wimpy2Match=Select[wimpyAfter,#["Id"]==id&][[1]];
				If[
					(aa=wimpy1Match["OpeningTimes"])==(bb=wimpy2Match["OpeningTimes"]),
					(*No change *)
					Nothing,
					
					Grid[{{"Wimpy "<>id,SpanFromLeft},{"Before","After"},{ds1=Dataset[bef1=wimpyBefore][Select[#Id===id&],"OpeningTimes"],ds2=Dataset[wimpyAfter][Select[#Id===id&],"OpeningTimes"]}}]
				]
			]
		]
		&
		,
		Intersection[wimpy1[[All, "Id"]], wimpy2[[All, "Id"]]]
	]
]

End[]

EndPackage[]
