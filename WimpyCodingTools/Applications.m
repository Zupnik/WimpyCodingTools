Package["WimpyCodingTools`"]






$Tiers = {"S", "A", "B", "C", "D", "E", "F"}

$allWimpyMenuItems := CloudSymbol["Wimpy/$NutritionalData"] // Keys
$allWimpyNames := $WimpyData[[All, "Name"]]

$WimpyDataFormCloudObject = CloudObject["Wimpy/Deployments/WimpyVisitForm"]
$WimpyNutritionalFormCloudObject = CloudObject["Wimpy/Deployments/WimpyNutritionalForm"]


PackageExport[RegisterWimpyNutritionFormFunction]

RegisterWimpyNutritionFormFunction[] := Module[
    {menuItems = $allWimpyMenuItems},
    (* TODO: Add Failure check*)
    CloudDeploy[
        FormFunction[
            {
                "MenuItems" -> RepeatingElement[menuItems,{1,Infinity}]
            },
            Module[
                {meal, mealWithTotal},
                meal = menuItems[#MenuItems];
                mealWithTotal=Append[meal,<|"Total"->Normal[meal[Total]]|>];
                ExportForm[mealWithTotal,"PNG"]
            ]&
        ],
        $WimpyNutritionalFormCloudObject,
        Permissions -> "Public"
    ]
]

PackageExport[RegisterWimpyDataFormFunction]

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
                    (* TODO: Where is this even being saved?! *)
                    imageSaveLocation = DateString["ISODate"] <> "-" <> wimpy <> ".jpg";
                    (* Resize to something sane and export it and use the reference in the CloudSymbol *)
                    CloudExport[ImageResize[image, {Automatic, 630}], "JPEG", CloudObject[imageSaveLocation]];
                    
                    data["Image"] = CloudObject[imageSaveLocation]
                ];

                AppendTo[CloudSymbol["Wimpy/WimpyTourData"], data]
            ]&
        ],
        $WimpyDataFormCloudObject
    ]
]

(* TODO: Wimpy Info Cloud Object *)

PackageExport[TierListGenerator]

Options[TierListGenerator] = {
    (* "Tiers" -> $Tiers *)
    "Width" -> 6,
    "ImageSize" -> {100,100}
}

TierListGenerator[tiers_List, tierData_List,opts:OptionsPattern[]] := Module[
	{blankTierList, groups, orderedTiers,sortedOrderedTiers, colorFunction = ColorData["Rainbow"], imageSize = OptionValue["ImageSize"], width = OptionValue["Width"]},
	blankTierList=#->{}&/@tiers;
	groups = GroupBy[tierData,#Tier&,#[[All,"Image"]]&];

    (* Remove values which are missing. Probably should do something before I get to this point to prevent this, but this works. *)
    groups = DeleteCases[groups, _Missing, {0, Infinity}];

	orderedTiers = Table[tiers[[i]]->N[(Length[tiers]-i+1)/Length[tiers]],{i,Length[tiers]}];
	sortedOrderedTiers=KeySort[groups,Lookup[orderedTiers,#]&];
    (* TODO: Only works well with square images. I will need to fix this at some point. *)
	Grid[
		Map[{
			Item[Graphics[Inset[Style[#[[1]], FontSize -> 50, FontColor->White]],ImageSize -> imageSize],Background->ColorData["Rainbow"][Association[orderedTiers][#[[1]]]]],
			(* TODO: Remove ImageSaliencyCrop as its too slow *)
            Item[Grid[makeCombinedImage[#[[2]], OptionValue["Width"]],Spacings->{0,0}],Alignment->Left]
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

TierListGenerator[tierData_List, opts:OptionsPattern[]] := TierListGenerator[$Tiers, tierData, opts]

(* TODO: MOVE IMAGESIZE *)
resizeImage[image_]:=Image[image, ImageSize->{100,100}]

makeCombinedImage[images_,length_]:=Module[
    {data, resizedImages, blankImage = Image[{{RGBColor[1, 1, 1, 0]}}, ImageSize -> {100, 100}]},
    
    (* If data is empty, I just add a single blank image, which is expanded out later with Partition *)
    resizedImages = If[
        (images === List[]),
        {blankImage},
        resizeImage/@images
    ];
    (* 
        Partition the data with the desired length. 
        I've added padding to be a seethrough image of the same size so that you can fix the size of the Grid.
        There's going to be a nicer way to do this, but this works well enough for now.
    *)
    data = Partition[resizedImages, length, length, 1, blankImage];
    data
    (* ImageAssemble[data, Background -> None] *)
]

PackageExport[MakeTierListGeneratorImage]

MakeTierListGeneratorImage[]:=1


