# WimpyCodingTools


The WimpyCodingTools are a set of tools I'm actively developing for my Wimpy Tour. For more information on the tour go to my [Linktree](https://linktr.ee/zupnik)

If you're just interested in the code.

##### Install at:

```mathematica
ResourceFunction["GitHubInstall"]["Zupnik", "WimpyCodingTools"]
```

To get started, you can see all the functions be running:

```mathematica
Needs["WimpyCodingTools`"];
Information["WimpyCodingTools`*"]
```

To get the latest data from Wimpy (Note: this only runs onces per kernel session):

```
$WimpyData
```

You can generate your route data from a start point:

```mathematica
routeData = CreateRouteData["StartLocation" -> Entity["City", {"Leeds", "Leeds", "UnitedKingdom"}]];
```

And plot using:

```mathematica
WimpyTourGraphic[routeData]
```

There are a few options for WimpyTourGraphic, and it takes all the GeoGraphics options if you want to refine the graphic a bit.

