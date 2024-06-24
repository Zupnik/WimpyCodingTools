Package["WimpyCodingTools`"]

PackageExport[WimpyDifferences]

GeneralUtilities`SetUsage["WimpyDifferences[wimpyData1$, wimpyData2$] returns a list of differences between the two sets of wimpy data. The differences are returned as a list of associations with the keys 'WimpysDeleted' and 'WimpysAdded'"]

PackageExport[$ReportEmail]

$ReportEmail = $CloudUserID
$ReportTemplateLocation := FileNameJoin[{PacletObject["WimpyCodingTools"]["Location"], "Resources", "WimpyReportTemplate.nb"}]


(*
    NOTES:
    To get the ScheduledTask to work in the Cloud, you need to install WimpyCodingTools. You can do this by getting the latest:
    ResourceFunction["GitHubInstall"]["Zupnik", "WimpyCodingTools"]

    Or if you aren't using a .paclet file you need to add something like this to the init.m:
    PacletDirectoryLoad[FileNameJoin[{Directory[],"/Wimpy/WimpyCodingTools/"}]];
    Needs["WimpyCodingTools`"]

*)

(* Imported Symbols *)
(* WimpyCodingTools`GetWimpyById
WimpyCodingTools`$WimpyData *)

(* CONSIDER: what if this has no value? Produces an error*)

(*  TODO: Need to look at the Holiday times field as well 
    Perhaps the holiday times should be put onto the Graphic
*)
(* NOTE: wimpyData1 should be the older data of the two, should probably rename this.    *)
WimpyDifferences[wimpyData1_, wimpyData2_] := Module[
    {wimpyRestaraunts, wimpyOpeningTimes, wimpysOpened, wimpysClosed, wimpysChanged},
    wimpysChanged = compareWimpyRestaraunts[wimpyData1, wimpyData2];
    wimpysOpened = wimpysChanged["wimpysAdded"];
    wimpysClosed = wimpysChanged["WimpysDeleted"];
    
    <|
        "WimpysClosed"->
            (<|"Id"->#,"Name"->GetWimpyById[#, "WimpyData" :> wimpyData1]["Name"]|>&/@wimpysClosed), 
        "WimpysOpened" -> 
           (<|"Id"->#, "Name"->GetWimpyById[#, "WimpyData" :> wimpyData2]["Name"]|>&/@wimpysOpened),
        "OpeningTimes" -> 
            compareOpeningTimes[wimpyData1, wimpyData2]
    |>
    (* wimpyRestaraunts = compareWimpyRestaraunts[wimpyData1, wimpyData2];
    wimpyOpeningTimes = compareOpeningTimes[wimpyData1, wimpyData2]; *)
    
    (* Grid[{
        {"Wimpys Opened!", GetWimpyById[#,"WimpyData" -> wimpyData2]["Name"]&/@wimpysOpened},
        (* Note: If a Wimpy has closed, the Id has to be taken from the old data and not the new *)
        {"Wimpys Closed!", GetWimpyById[#,"WimpyData" -> wimpyData1]["Name"]&/@wimpysClosed},
        {
            SlideView[compareOpeningTimes[wimpyData1, wimpyData2]], SpanFromLeft}
        }
    ] *)
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
                    <|
                        "WimpyId"->id, "WimpyName" -> WimpyCodingTools`GetWimpyById[id]["Name"],
                        "Changes"-> Dataset[Merge[{wimpy1Match["OpeningTimes"], wimpy2Match["OpeningTimes"]}, mergeOpeningTimes[#[[1]],#[[2]]]&]]
                    |>
					(* Grid[
                        {
                            {"Wimpy "<> WimpyCodingTools`GetWimpyById[id]["Name"]},
                            {
                                Dataset[Merge[{wimpy1Match["OpeningTimes"], wimpy2Match["OpeningTimes"]}, mergeOpeningTimes[#[[1]],#[[2]]]&]]
                                (* ds1 = Dataset[wimpy1Match["OpeningTimes"]],
                                ds2 = Dataset[wimpy2Match["OpeningTimes"]] *)
                            }
                        }
                    ] *)
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


PackageExport[$WimpyChangeMonitorScheduledTask]
$WimpyChangeMonitorScheduledTask = CloudObject["Wimpy/WimpyChangeMonitorScheduledTask"];


PackageExport[InitializeWimpyChangeMonitor]
GeneralUtilities`SetUsage["InitializeWimpyChangeMonitor deploys a CloudObject which runs a scheduled task to compare the current Wimpy data with the previous day's data. The differences are then emailed to the email address in $ReportEmail. The scheduled task runs daily by default."]


Options[InitializeWimpyChangeMonitor] = Options[iInitializeWimpyChangeMonitor] = {
    "ScheduledTask" -> $WimpyChangeMonitorScheduledTask,
    "CloudSymbol" :> "Wimpy/$WimpyData"
}

InitializeWimpyChangeMonitor[opts:OptionsPattern[]] := iInitializeWimpyChangeMonitor[opts]

iInitializeWimpyChangeMonitor[opts:OptionsPattern[]] :=  CloudDeploy[
    ScheduledTask[
        runComparison[];
        (* Set new Data*)
        CloudSymbol[OptionValue["CloudSymbol"]] = $WimpyData;
        ,
        {Tomorrow, "Daily"}
    ]
    ,
    OptionValue["ScheduledTask"]
]

runComparison[] := Module[
    {oldWimpyData, newWimpyData, co, email, runTaskQ, differences},
    email = getEmail[];
    If[FailureQ[email],
        Return[email]
    ];

    oldWimpyData = CloudSymbol["Wimpy/$WimpyData"];
    newWimpyData = $WimpyData;
    (* TODO: *)
    differences = WimpyDifferences[oldWimpyData, newWimpyData];
    runTaskQ = !AllTrue[Values[differences], {} === # &];
    
    If[
        $CloudEvaluation
        ,
        If[
            runTaskQ,
            (* Only make the report *)
            co = CloudObject["Wimpy/WimpyReport/"<>DateString[Riffle[{"Year", "Month", "Day", "Report"}, "_"]]];
            (* Using Headless mode as I'm not sure that ScheduledTask can work with FrontEnd. I've tried UsingFrontEnd with no success *)
            doc = GenerateDocument[$ReportTemplateLocation, differences, "HeadlessMode" -> True];
            
            CloudDeploy[doc, co];
            SendMail[email, co]
            ,
            SendMail[email, "No Changes to Wimpys today!"]
        ]
        ,
        (* If you want to run comparison locally *)
        GenerateDocument[$ReportTemplateLocation, differences]
    ]
]

getEmail[] := Module[
	{email},
	email = Interpreter["EmailAddress"][$ReportEmail];
	If[
        FailureQ[email] || MissingQ[email],
        Failure["InvalidEmail", <|
            "MessageTemplate" -> 
            "`email` is not a valid email. Check $ReportEmail", 
            "MessageParameters" -> <|"email" -> $ReportEmail|>|>
        ]
        ,
        Return[email]
    ];
]
