(**

  This module contains type and constants for the Eidolon system.

  @Author  David Hoyle
  @Date    30 Aug 2014
  @Version 1.0

**)
Unit EidolonTypes;

Interface

Type
  (** An enumerate for the symbol types. **)
  TSymbolType = (tstRectangle, tstBar, tstLine, tstEllipse, tstTriangle,
    tstDiamond, tstSuperBar);

  (** An enumerate to defined the Excel Colour Indexes **)
  TColour = (
    xlcNONE,
    xlcBLACK,
    xlcBROWN,
    xlcOLIVEGREEN,
    xlcDARKGREEN,
    xlcDARKTEAL,
    xlcDARKBLUE,
    xlcINDIGO,
    xlcGRAY80,
    xlcDARKRED,
    xlcORANGE,
    xlcDARKYELLOW,
    xlcGREEN,
    xlcTEAL,
    xlcBLUE,
    xlcBLUEGRAY,
    xlcGRAY50,
    xlcRED,
    xlcLIGHTORANGE,
    xlcLIME,
    xlcSEAGREEN,
    xlcAQUA,
    xlcLIGHTBLUE,
    xlcVIOLET,
    xlcGRAY40,
    xlcPINK,
    xlcGOLD,
    xlcYELLOW,
    xlcBRIGHTGREEN,
    xlcTURQUOISE,
    xlcSKYBLUE,
    xlcPLUM,
    xlcGRAY25,
    xlcROSE,
    xlcTAN,
    xlcLIGHTYELLOW,
    xlcLIGHTGREEN,
    xlcLIGHTTURQUOISE,
    xlcPALEBLUE,
    xlcLAVENDER,
    xlcWHITE
  );

  (** An enumerate to define the line styles available in Excel. **)
  TLineStyle = (
    lsSOLID,
    lsROUNDDOT,
    lsSQUAREDOT,
    lsDASH,
    lsDASHDOT,
    lsLONGDASH,
    lsLONGDASHDOT,
    lsDASHDOTDOT
  );

  (** An enumerate for line weights. **)
  TLineWeight = (
    lw0,
    lw0_25,
    lw0_5,
    lw1,
    lw1_5,
    lw2_25,
    lw3,
    lw4_5,
    lw6,
    lwDOUBLE,
    lwDOUBLETHINTHICK,
    lwDOUBLETHICKTHIN,
    lwTRIPLETHICKBETWEENTHIN
  );

  (** An enumerate for the interior patterns. **)
  TInteriorPattern = (
    ipNONE,
    ip10PERCENT,
    ip20PERCENT,
    ip25PERCENT,
    ip30PERCENT,
    ip40PERCENT,
    ip50PERCENT,
    ip5PERCENT,
    ip60PERCENT,
    ip70PERCENT,
    ip75PERCENT,
    ip80PERCENT,
    ip90PERCENT,
    ipDARKDOWNWARDDIAGONAL,
    ipDARKHORIZONTAL,
    ipDARKUPWARDDIAGONAL,
    ipDARKVERTICAL,
    ipDASHEDDOWNWARDDIAGONAL,
    ipDASHEDHORIZONTAL,
    ipDASHEDUPWARDDIAGONAL,
    ipDASHEDVERTICAL,
    ipDIAGONALBRICK,
    ipDIVOT,
    ipDOTTEDGRID,
    ipHORIZONTALBRICK,
    ipLARGECHECKERBOARD,
    ipLARGECONFETTI,
    ipLARGEGRID,
    ipLIGHTDOWNWARDDIAGONAL,
    ipLIGHTHORIZONTAL,
    ipLIGHTUPWARDDIAGONAL,
    ipLIGHTVERTICAL,
    ipNARROWHORIZONTAL,
    ipNARROWVERTICAL,
    ipOUTLINEDDIAMOND,
    ipPLAID,
    ipSHINGLE,
    ipSMALLCHECKERBOARD,
    ipSMALLCONFETTI,
    ipSMALLGRID,
    ipSOLIDDIAMOND,
    ipSPHERE,
    ipTRELLIS,
    ipWAVE,
    ipWEAVE,
    ipWIDEDOWNWARDDIAGONAL,
    ipWIDEUPWARDDIAGONAL,
    ipZIGZAG
  );

  (** An enumerate to define the types of Triangle. **)
  TTriangleType = (ttStartAndEarly, ttStartAndLate, ttEndAndEarly, ttEndAndLate);

  (** An enumerate to define the location for Roads. **)
  TLocation = (loNone, loLeft, loRight, loBoth, loOver, loOverLeft,
    loOverRight, loUnder);

  (** An enumerate to define the orientation of the text on a schematic
      diagram object. **)
  TTextOrientation = (toHorizontal, toVertical);

  (** An enumerate to define the position of the text in a schematic diagram. **)
  TTextPosition = (tpOutside, tpInside);

  (** An enumerate to define the mearsurement percentages for the diagrams. **)
  TSetting = (seMargins, seSpacing, seCentreLine);

  (** An enumerate to define the drawing dimensions which can be changed
      by the user. **)
  TDimension = (diHeight, diWidth, diLegendWidth, diTitleHeight, diOverheadHeight,
    diDateWidth, diLegendLabel, diWeekNoWidth, diFrameGap, diQuantityHeight,
    diQuantityWidth);

  (** A list of ISO paper sizes for the time location diagrams. **)
  TPaperSize = (psA4P, psA4L, psA3P, psA3L, psA2P, psA2L, psA1P, psA1L, psA0P, psA0L);

  (** An enumerate to define the line end style. **)
  TLineEndType = (atNone, atDiamond, atOpen, atOval, atStealth, atTriangle);

  (** An enumerate to define the line end length and width. **)
  TLineEndSize = (
    asShortNarrow, asMediumNarrow, asLongNarrow,
    asShortMedium, asMediumMedium, asLongMedium,
    asShortWide,   asMediumWide,   asLongWide
  );

  (** An enumerate to define the display properties of time location symbols. **)
  TDisplayOp = (
    doRender,         // Show the Time Location Symbol on the Diagram
    doLegend,         // Show the Time Location Symbol in the Legend
    doHideText        // Hide the text on the diagram associated with the Time Location Symbol
  );
  (** A set of the above enumerate. **)
  TDisplayOps = Set Of TDisplayOp;

  (** A record to describe the dimension information for the constant array **)
  TDimensionRec = Record
    FDisplayName : String;
    FININame     : String;
    FDefaults    : Array[Low(TPaperSize)..High(TPaperSize)] Of Double;
  End;

Const
  (** A constant array of names for the enumerated symbols types. **)
  strSymbolTypes : Array[Low(TSymbolType)..High(TSymbolType)] Of String = (
    'Rectangle', 'Bar', 'Line', 'Ellipse', 'Triangle', 'Diamond', 'SuperBar'
  );

  (** A constant array providing names for the enumerated colours. **)
  strColours : Array[Low(TColour)..High(TColour)] Of String = (
    'None',
    'Black',
    'Brown',
    'OliveGreen',
    'DarkGreen',
    'DarkTeal',
    'DarkBlue',
    'Indigo',
    'Gray-80%',
    'DarkRed',
    'Orange',
    'DarkYellow',
    'Green',
    'Teal',
    'Blue',
    'Blue-Gray',
    'Gray-50%',
    'Red',
    'LightOrange',
    'Lime',
    'SeaGreen',
    'Aqua',
    'LightBlue',
    'Violet',
    'Gray-40%',
    'Pink',
    'Gold',
    'Yellow',
    'BrightGreen',
    'Turquoise',
    'SkyBlue',
    'Plum',
    'Gray-25%',
    'Rose',
    'Tan',
    'LightYellow',
    'LightGreen',
    'LightTurquoise',
    'PaleBlue',
    'Lavender',
    'White'
  );

  (** A constant array of line style names for the enumerated styles **)
  strLineStyles : Array[Low(TLineStyle)..High(TLineStyle)] Of String = (
    'Solid',
    'RoundDot',
    'SquareDot',
    'Dash',
    'DashDot',
    'LongDash',
    'LongDashDot',
    'DashDotDot'
  );

  (** A constant array of line weights names for the enumerated weights **)
  strLineWeights : Array[Low(TLineWeight)..High(TLineWeight)] Of String = (
    'None',
    '0.25',
    '0.5',
    '1',
    '1.5',
    '2.25',
    '3',
    '4.5',
    '6',
    'Double',
    'DoubleThinThick',
    'DoubleThickThin',
    'TripleThickBetweenThin'
  );

  (** A constant array of names for the enumerated interior patterns. **)
  strInteriorPatterns : Array[Low(TInteriorPattern)..High(TInteriorPattern)] Of String = (
    'None',
    '10Percent',
    '20Percent',
    '25Percent',
    '30Percent',
    '40Percent',
    '50Percent',
    '5Percent',
    '60Percent',
    '70Percent',
    '75Percent',
    '80Percent',
    '90Percent',
    'DarkDownwardDiagonal',
    'DarkHorizontal',
    'DarkUpwardDiagonal',
    'DarkVertical',
    'DashedDownwardDiagonal',
    'DashedHorizontal',
    'DashedUpwardDiagonal',
    'DashedVertical',
    'DiagonalBrick',
    'Divot',
    'DottedGrid',
    'HorizontalBrick',
    'LargeCheckerBoard',
    'LargeConfetti',
    'LargeGrid',
    'LightDownwardDiagonal',
    'LightHorizontal',
    'LightUpwardDiagonal',
    'LightVertical',
    'NarrowHorizontal',
    'NarrowVertical',
    'OutlinedDiamond',
    'Plaid',
    'Shingle',
    'SmallCheckerBoard',
    'SmallConfetti',
    'SmallGrid',
    'SolidDiamond',
    'Sphere',
    'Trellis',
    'Wave',
    'Weave',
    'WideDownwardDiagonal',
    'WideUpwardDiagonal',
    'Zigzag'
  );

  (** A constant array to defines the names of the triangle types. **)
  strTriangleTypes : Array[Low(TTriangleType)..High(TTriangleType)] Of String = (
    'StartAndEarly', 'StartAndLate', 'EndAndEarly', 'EndAndLate'
  );

  (** A constant array of string representation of the locations. **)
  Locations : Array[Low(TLocation)..High(TLocation)] Of String = (
    'None',
    'Left',
    'Right',
    'Both',
    'Over',
    'Over Left',
    'Over Right',
    'Under'
  );

  (** A constant array of RGB colours for the excel palette. **)
  Colours : Array[Low(TColour)..High(TColour)] of Integer = (
    -1,
    $000000, $003399, $003333, $003300, $663300, $800000, $993333, $333333,
    $000080, $0066FF, $008080, $008000, $808000, $FF0000, $996666, $808080,
    $0000FF, $0099FF, $00CC99, $669933, $CCCC33, $FF6633, $800080, $969696,
    $FF00FF, $00CCFF, $00FFFF, $00FF00, $FFFF00, $FFCC00, $663399, $C0C0C0,
    $CC99FF, $99CCFF, $99FFFF, $CCFFCC, $FFFFCC, $FFCC99, $FF99CC, $FFFFFF);

  (** A constant array of strings describing Line End Types. **)
  strLineEndTypes : Array[Low(TLineEndType)..High(TLineEndType)] Of String = (
    'None', 'Diamond', 'Open', 'Oval', 'Stealth', 'Triangle'
  );

  (** A constant array of strings describing Line End Sizes. **)
  strLineEndSizes : Array[Low(TLineEndSize)..High(TLineEndSize)] Of String = (
    'ShortNarrow', 'MediumNarrow', 'LongNarrow',
    'ShortMedium', 'MediumMedium', 'LongMedium',
    'ShortWide',   'MediumWide',   'LongWide'
  );

  (** A constant array of line end widths to be associated with the LineEndSizes **)
  LineEndWidths : Array[Low(TLineEndSize)..High(TLineEndSize)] Of Integer = (
    2, 2, 2, 4, 4, 4, 6, 6, 6);

  (** A constant array of line end lengths to be associated with the LineEndSizes **)
  LineEndLengths : Array[Low(TLineEndSize)..High(TLineEndSize)] Of Integer = (
    2, 4, 6, 2, 4, 6, 2, 4, 6);

  (** A constant array of dimension names for the INI file. **)
  Dimensions : Array[Low(TDimension)..High(TDimension)] Of TDimensionRec = (
                                                             //               A4P, A4L, A3P, A3L, A2P, A2L, A1P, A1L,  A0P,  A0L
    (FDisplayName: 'Height';          FININame: 'Height';         FDefaults: (297, 210, 420, 297, 594, 420, 841, 594, 1089,  841)),
    (FDisplayName: 'Width';           FININame: 'Width';          FDefaults: (210, 297, 297, 420, 420, 594, 594, 841,  841, 1189)),
    (FDisplayName: 'Legend Width';    FININame: 'LegendWidth';    FDefaults: ( 40,  40,  55,  55,  75,  75, 110, 110,  150,  150)),
    (FDisplayName: 'Title Height';    FININame: 'TitleHeight';    FDefaults: ( 40,  40,  55,  55,  75,  75, 110, 110,  150,  150)),
    (FDisplayName: 'Overhead Height'; FININame: 'OverheadHeight'; FDefaults: ( 40,  40,  55,  55,  75,  75, 110, 110,  150,  150)),
    (FDisplayName: 'Date Width';      FININame: 'DateWidth';      FDefaults: ( 25,  25,  30,  30,  40,  40,  55,  55,   75,   75)),
    (FDisplayName: 'Legend Label';    FININame: 'LegendLabel';    FDefaults: ( 15,  15,  20,  20,  30,  30,  40,  40,   52,   52)),
    (FDisplayName: 'Week No Width';   FININame: 'WeekNoWidth';    FDefaults: (  6,   8,  10,  10,  15,  15,  20,  20,   25,   25)),
    (FDisplayName: 'Frame Gap';       FININame: 'FrameGap';       FDefaults: (  6,   6,   7,   7,   8,   8,   9,   9,   10,   10)),
    (FDisplayName: 'Quantity Height'; FININame: 'QuantityHeight'; FDefaults: ( 30,  30,  40,  40,  55,  55,  75,  75,  100,  100)),
    (FDisplayName: 'Quantity Width';  FININame: 'QuantityWidth';  FDefaults: ( 30,  30,  40,  40,  55,  55,  75,  75,  100,  100))
  );

  (** A constant array of string representations of paper sizes. **)
  strPaperSizes : Array[Low(TPaperSize)..High(TPaperSize)] Of String = (
    'A4 Portrait', 
    'A4 Landscape', 
    'A3 Portrait', 
    'A3 Landscape', 
    'A2 Portrait', 
    'A2 Landscape', 
    'A1 Portrait', 
    'A1 Landscape', 
    'A0 Portrait', 
    'A0 Landscape'
  );

Implementation

End.
