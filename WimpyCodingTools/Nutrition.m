Package["WimpyCodingTools`"]

PackageExport[MakeNutritionalData]
PackageExport[DeployNutritionalData]

(* $PDFLocation = "https://wimpy-uk.s3-eu-west-2.amazonaws.com/static/Nutrition%20Guide.pdf"; *)
(* This is just for me *)
$PDFLocation = "/Users/anthonyz/Dropbox/projects/Wimpy2024/Coding/Food/WimpyNutritionalGuide.pdf";

$MenuDataCloudSymbol = "Wimpy/$MenuData"

MakeNutritionalData[] := Module[
    {nutritionalData, unprocessedImport, pdfLoc, split, pattern, unprocessedMenuItems, processedMenuItems},
    pdfLoc = $PDFLocation;
(* 
    If[!FileExistsQ[$PDFLocation],
        Print["Error: PDF file does not exist."];
        Failure["PDFFileNotFound", <|"Message" -> "PDF file does not exist."|>];
    ]; *)

    unprocessedImport = Import[pdfLoc, "Plaintext"];
    split = StringSplit[unprocessedImport, "\n"];
    pattern = __ ~~ WordCharacter ... ~~ StringExpression @@ Riffle[Table[NumberString, 9], " "];
    unprocessedMenuItems = Select[split, StringContainsQ[pattern]];
    processedMenuItems = Dataset[Association @@ 
        StringCases[
            name__ ~~ WordCharacter ... ~~ " " ~~ energy : NumberString ~~ 
            " " ~~ energyKcal : NumberString ~~ " " ~~ fat : NumberString ~~
            " " ~~ sats : NumberString ~~ " " ~~ carbs : NumberString ~~ 
            " " ~~ sugars : NumberString ~~ " " ~~ fibre : NumberString ~~ 
            " " ~~ prot : NumberString ~~ " " ~~ salt : NumberString :> 
            StringReplace[name, "\[Dash] " -> ""] -> <|
            "Calories" -> 
                Quantity[energyKcal // ToExpression, "DietaryCalories"], 
            "Fat" -> Quantity[fat // ToExpression, "Grams"], 
            "Saturates" -> Quantity[sats // ToExpression, "Grams"], 
            "Sugars" -> Quantity[sugars // ToExpression, "Grams"], 
            "Carbohydrates" -> Quantity[carbs // ToExpression, "Grams"], 
            "Fibre" -> Quantity[fibre // ToExpression, "Grams"], 
            "Protein" -> Quantity[prot // ToExpression, "Grams"], 
            "Salt" -> Quantity[salt // ToExpression, "Grams"]|>][
        unprocessedMenuItems]];
    (* Lazy implementation because I can't be bothered to fix the above Association *)
    processedMenuItems
]

(* 
    Symbol doesn't actually do anything other than spit out nice looking usage messages 
    TODO: Make this better.
*)
PackageExport[WimpyCodingTools]
WimpyCodingTools::coexists = "CloudObject already exists at `1`";


(* 
    Puts all the data into a package file on the cloud and then sets Cloud Symbols for that data
    Will onlye
*)
DeployNutritionalData[] := Module[{file = $NutritionalDataCloudObject},
    If[
        
        FileExistsQ[ file],
        (* Prevent overwriting unless explictly stated *)
        Message[WimpyCodingTools::coexists, file];
        Failure["CloudObjectExists", <|
            "MessageTemplate" -> 
                "CloudObject exists at `co`. Delete with the following code: `code`", 
            "MessageParameters" -> <|
                "co"-> file,
                "code" -> With[{file =  file}, Hold[DeleteFile[file]]]|>|>]
        ,
        (* Export to a Package file so it is easy to manually edit in the Cloud *)
        CloudExport[MakeNutritionalData[], {"WL", "Package"}, file];
        UpdateNutritionalDataCloudSymbol[];
        UpdateMenuCloudSymbol[];
    ]
]

PackageExport[UpdateNutritionalDataCloudSymbol]

(* TODO: Why do I do this? Seems like a waste of time. *)
UpdateNutritionalDataCloudSymbol[] := (CloudSymbol[$NutritionalDataCloudSymbol] = Import[$NutritionalDataCloudObject])

PackageExport[UpdateMenuCloudSymbol]

(* Not we need the $Menu Symbol *)
UpdateMenuCloudSymbol[]:= CloudSymbol[$MenuDataCloudSymbol] = CloudSymbol[$NutritionalDataCloudSymbol][[All, "Name"]]

PackageExport[ForceUpdateMenuAndNutritionalCloudSymbols]
ForceUpdateMenuAndNutritionalCloudSymbols[]:=(UpdateNutritionalDataCloudSymbol[];UpdateMenuCloudSymbol[])

    
