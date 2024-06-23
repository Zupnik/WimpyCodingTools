Package["WimpyCodingTools`"]

PackageExport[RegisterWimpyDataFormFunction]



$Tiers = {"S", "A", "B", "C", "D", "E", "F"}

$allWimpyMenuItems := CloudSymbol["Wimpy/$NutritionalData"][[All, "Name"]]
$allWimpyNames := $WimpyData[[All, "Name"]]

RegisterWimpyDataFormFunction[] := Module[
    {allWimpyNames, allWimpyMenuItems},

    allWimpyNames = $allWimpyNames;
    allWimpyMenuItems = $allWimpyMenuItems; 
    
    CloudDeploy[
        FormFunction[
            {
                "Date"-><|"Input" -> DateString[Today], "Interpreter"->"Date"|>,
                "Wimpy" -> allWimpyNames,
                "FoodItems" -> RepeatingElement[allWimpyMenuItems,{1,Infinity}],
                "Tier" -> $Tiers,
                "Image"-> "Image" -> None,
                "Notes" -> "String"->None
                (* CONSIDER: Having a way to remove items*)
                (* ,
                "RemovedItems" -> RepeatingElement[allWimpyMenuItems,{0,Infinity}] *)
            },
            Module[
                {data = #, image = #Image, imageSaveLocation, wimpy = #Wimpy},
                
                If[
                    FailureQ[CloudSymbol["Wimpy/WimpyTourData"]],
                    CloudSymbol["Wimpy/WimpyTourData"] = {}
                ];
                
                If[
                    image =!= None,
                    imageSaveLocation = DateString["ISODate"] <> "-" <> wimpy <> ".jpg";
                    (* Resize to something sane and export it and use the reference in the CloudSymbol *)
                    CloudExport[ImageResize[image, {Automatic, 630}], "JPEG", CloudObject[imageSaveLocation]];
                    
                    data["Image"] = CloudObject[imageSaveLocation]
                ];

                AppendTo[CloudSymbol["Wimpy/WimpyTourData"], data]
            ]&
        ],
        "Wimpy/Deployments/RegisterWimpyData"
    ]
]

(* TODO: Wimpy Info Cloud Object *)

PackageExport[TierListGenerator]


TierListGenerator[tiers_List, tierData:{__Association}] := Module[
	{blankTierList, groups, orderedTiers,sortedOrderedTiers, colorFunction = ColorData["Rainbow"],imageSize={100,100}},
	blankTierList=#->{}&/@tiers;
	groups=GroupBy[tierData,#Tier&,#[[All,"Image"]]&];
	orderedTiers=Table[tiers[[i]]->N[(Length[tiers]-i+1)/Length[tiers]],{i,Length[tiers]}];
	sortedOrderedTiers=KeySort[groups,Lookup[orderedTiers,#]&];
	
	Grid[
		Map[{
			Item[Graphics[Inset[Style[#[[1]],FontSize->50,FontColor->White]],ImageSize->imageSize],Background->ColorData["Rainbow"][Association[orderedTiers][#[[1]]]]],
			Sequence@@(Item[Image[ResourceFunction["ImageSaliencyCrop"][#],ImageSize->imageSize],ItemSize->Full]&[#]&/@#[[2]])
			}&,
		Normal[Association[blankTierList,sortedOrderedTiers]]
		],
		Dividers -> {True,All},
		Frame -> True,
		(*Frame\[Rule]{True,True,{{{1,1},{1,3}}\[Rule]True}},*)
		Spacings->{0,0},
		Background->Lighter@Black
	]
]

TierListGenerator[tierData:{__Association}] := TierListGenerator[$Tiers, tierData]

(* MakeTierListGeneratorImage[]:=1 *)