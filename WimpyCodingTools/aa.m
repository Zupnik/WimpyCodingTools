Package["WimpyCodingTools`"]

PackageScope[$WimpyTourWebsiteLocation]
PackageScope[$WimpyTourTestWebsiteLocation]
PackageScope[$WimpyTourDeploymentLocation]
PackageScope[$WimpyVisitGraphic]
PackageScope[$WimpyTourInformation]
PackageScope[$NutritionalDataCloudObject]
PackageScope[$NutritionalDataCloudSymbol]
PackageScope[$ImageSaveLocation]


PackageExport[$WimpyDataFormCloudObject]
PackageExport[$WimpyNutritionalFormCloudObject]
PackageExport[$WimpyTierListLocation]
PackageExport[$TourDataSymbol]

PackageExport[$DeployTest]
$DeployTest = True;

(* TODO: Move all constants here *)

$WimpyTourWebsiteLocation = "Wimpy2024"
$WimpyTourTestWebsiteLocation = "Wimpy2024Test"

$WimpyTourDeploymentLocation := If[$DeployTest, $WimpyTourTestWebsiteLocation, $WimpyTourWebsiteLocation]

(* Graphics/Data *)


$WimpyVisitGraphic = CloudObject["Wimpy/Deployments/WimpyVisitGraphic.svg"]
(* $WimpyTourInformation = CloudObject["Wimpy/Deployments/WimpyTourInformation.json"] *)
$WimpyTourInformation = CloudObject[$WimpyTourDeploymentLocation<>"/Data/WimpyTourInformation.json"]
$WimpyTierListLocation = CloudObject["Wimpy/Deployments/WimpyTierList.png"]
$NutritionalDataCloudObject = CloudObject["Wimpy/WimpyData/NutritionalData.m"];



(* Cloud Symbols*)
$NutritionalDataCloudSymbol = "Wimpy/$NutritionalData"
$TourDataSymbol = "Wimpy/WimpyTourData"

(* Forms *)
$WimpyDataFormCloudObject = CloudObject["Wimpy/Deployments/WimpyVisitForm"]
$WimpyNutritionalFormCloudObject = CloudObject["Wimpy/Deployments/WimpyNutritionalForm"]

$ImageSaveLocation = "Wimpy/WimpyData/Images/"

(* Resetting values*)

(*
    CloudSymbol[$TourDataSymbol] = {}
    WimpyVisitGraphic[]
*)