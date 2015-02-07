(**

  This module contains type and constants for the Eidolon system.

  @Author  David Hoyle
  @Date    07 Feb 2015
  @Version 1.0

**)
Unit EidolonTypes;

Interface

Uses
  Graphics;

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
  TDimension = (
    diHeight,
    diWidth,
    diLegendWidth,
    diTitleHeight,
    diOverheadHeight,
    diDateWidth,
    diLegendLabel,
    diWeekNoWidth,
    diFrameGap,
    diFrameMargin,
    diPrimaryQuantityHeight,
    diSecondaryQuantityHeight,
    diTertiaryQuantityHeight,
    diPrimaryQuantityWidth,
    diSecondaryQuantityWidth,
    diTertiaryQuantityWidth
  );

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

  (** An enumerate for the Time Location boolean options. **)
  TTLSOption = (
    tlsoOverhead,
    tlsoTransposeChainage,
    tlsoLayerBySymbol,
    tlsoShowYearlyGrid,
    tlsoShowMonthlyGrid,
    tlsoShowWeeklyGrid,
    tlsoShowProgress,
    tlsoTransposeTimeScale,
    tlsoSuppressDescriptions,
    tlsoRenderSchematic,
    tlsoRenderByRoute,
    tlsoRenderUsedLegends,
    tlsoIncrementDrawingRev,
    tlsoRouteCodeOnSingleDrn,
    tlsoRenderTLInTime,
    tlsoGroupObjectsInLayers
  );

  (** A set of the above options. **)
  TTLSOptions = Set of TTLSOption;

  (** This enumerate define a list of the reference points on the time location from the
      far left to the far right. **)
  THorizontalPosition = (
    hpLeftOuterFrame,                   // Left edge of the outer frame
    hpLeftDateDivider,                  // Left Date divider between the date and the week number
    hpLeftInnerFrames,                  // Left edge of the left most inner frame
    hpRightInnerFrames,                 // Right edge of the right most inner frame
    hpRightDateDivider,                 // Right Date divider between the date and the week number
    hpTertiaryVerticalQuantityDivider,  // Left edge of the tertiary vertical Quantities area
    hpLeftOfTertiaryVQuantityFrame,     // Left edge of the tertiary vertical quantities frame
    hpRightOfTertiaryVQuantityFrame,    // Right edge of the tertiary vertical quantities frame
    hpSecondaryVerticalQuantityDivider, // Left edge of the secondary vertical Quantities area
    hpLeftOfSecondaryVQuantityFrame,    // Left edge of the secondary vertical quantities frame
    hpRightOfSecondaryVQuantityFrame,   // Right edge of the secondary vertical quantities frame
    hpPrimaryVerticalQuantityDivider,   // Left edge of the primary vertical Quantities area
    hpLeftOfPrimaryVQuantityFrame,      // Left edge of the primary vertical quantities frame
    hpRightOfPrimaryVQuantityFrame,     // Right edge of the primary vertical quantities frame
    hpLegendDivider,                    // Left edge of the Legend area
    hpLeftLegendText,                   // Left edge of the legend text
    hpLegendLabel,                      // Right edge of the legend label area
    hpRightLegendLabel,                 // Right edge of the legend label position
    hpRightLegendText,                  // Right edge of the legend text
    hpRightOuterFrame                   // Right edge of the outer frame
  );
  (** This enumerate define a list of the reference points on the time location from the
      top to the bottom. **)
  TVerticalPosition = (
    vpTopOuterFrame,                     // Top edge of the outer frame
    vpTopOfOverheadFrame,                // Top of the overhead frame
    vpBottomOfOverheadFrame,             // Bottom of the overhead frame
    vpOverheadDivider,                   // Bottom edge of the Overhead divider
    vpTopOfPrimaryHQuantityFrame,        // Top of the primary horizontal quantities frame
    vpBottomOfPrimaryHQuantityFrame,     // Bottom of the primary horizontal quantities frame
    vpBottomOfPrimaryHQuantityDivider,   // Bottom edge of the primary horizontal quantities divider
    vpTopOfSecondaryHQuantityFrame,      // Top of the secondary horizontal quantities frame
    vpBottomOfSecondaryHQuantityFrame,   // Bottom of the secondary horizontal quantities frame
    vpBottomOfSecondaryHQuantityDivider, // Bottom edge of the secondary horizontal quantities divider
    vpTopOfTertiaryHQuantityFrame,       // Top of the tertiary horizontal quantities frame
    vpBottomOfTertiaryHQuantityFrame,    // Bottom of the tertiary horizontal quantities frame
    vpBottomOfTertiaryHQuantityDivider,  // Bottom edge of the tertiary horizontal quantities divider
    vpTopInnerFrame,                     // Top edge of the Inner frames
    vpTitleDivider,                      // Top edge of the Title area
    vpTopTitleText,                      // Top of the title box text area
    vpBottomTitleText,                   // Bottom of the title box text area
    vpBottomInnerFrame,                  // Bottom edge of the Inner Frames
    vpBottomOuterFrame                   // Bottom edge of the outer frame
  );

  (** An enumerate to define the Primary, Secondray and Tertiary quantity graphs. **)
  TGraphHierarchy = (ghPrimary, ghSecondary, ghTertiary);

  (** An enumerate to define a series of options for the quantity graphs rather than
      managing numerous boolean properties. **)
  TQuantityGraphOption = (
    qgoShowHorizontalGraph,
    qgoShowHorizontalData,
    qgoStackHorizontalGraph,
    qgoShowVerticalGraph,
    qgoShowVerticalData,
    qgoStackVerticalGraph
  );

  (** A set of the quantity graph options. **)
  TGraphOptions = Set Of TQuantityGraphOption;

  (** An array of graph options for all the graph hierarchies. **)
  TQuantityGraphOptions = Array[Low(TGraphHierarchy)..High(TGraphHierarchy)] Of TGraphOptions;

  (** This is a record structure to define the items in the quantity graph options
      constant array. **)
  TQuantityGraphOptionsRec = Record
    FININame     : String;
    FDefault     : Boolean;
  End;

  (** An array of double for the intervals associated with a H or V quantities graph. **)
  TQuantityIntervals = Array[Low(TGraphHierarchy)..High(TGraphHierarchy)] Of Double;

  (** An enumerate to define the number of fonts required for TL rendering. **)
  TFontType = (
    ftLegendHeader,
    ftLegendText,
    ftShape,
    ftChainageGrid,
    ftDateHeader,
    ftDateGrid,
    ftSchematicText,
    ftQuantityText,
    ftQuantityTitleText,
    ftFrameWatermark
  );

  (** An enumerate to describe the drawing objects on a time location diagram which can
      be edited if required. **)
  TTLSDrawingObject = (
    tdoOuterFrame,
    tdoLegendDivider,
    tdoTitleDivider,
    tdoInnerFrame,
    tdoProgress,
    tdoHoliday,
    tdoYearlyGridline,
    tdoMonthlyGridline,
    tdoWeeklyGridline,
    tdoDailyGridLine,
    tdoChainageGridline,
    tdoTimeNowLine,
    tdoDateSeparator,
    tdoSchematicFrame,
    tdoSchematicCentreLine,
    tdoOverheadFrame,
    tdoQuantityFrame,
    tdoQuantityGridLines
  );

  (** A record to describe the font name and ini key. **)
  TFontTypeRec = Record
    FDisplayName : String;
    FININame     : String;
    FFontName    : String;
    FFontStyle   : TFontStyles;
    FFontSize    : Integer;
    FFontColour  : TColour;
  End;

  (** An enumerate to define the portions of Time Location Information that can be copied
      between profiles. **)
  TProfileData = (pdTLSOptions, pdDimensions, pdTitleBlock, pdMaxMinDateChaiange,
    pdHolidays, pdHolidayOptions, pdFonts, pdDrawingObjects, pdGeneralOptions,
    pdSchematicScript, pdSchematicSymbols);

  (** A set of the above Time Location Information copying portions. **)
  TProfileDatas = Set Of TProfileData;

  (** A record to describe the default properties of TLS Object Types. **)
  TTLSObjectRec = Record
    FName   : String;
    FSymbol : String;
  End;

  (** This is an enumerate to define the grouping under which the the TLSOptions are
      defined. **)
  TTLSOptionGroup = (
    tgoLayout,
    tgoGridlines,
    tgoRouteCode,
    tgoProgress,
    tgoMiscellaneous
  );

  (** This is an record to describe the attributes that are need to each TLSOption. **)
  TTLSOptionRec = Record
    strDescription : String;
    strName        : String;
    GroupID        : TTLSOptionGroup;
  End;

  (** An enumerate to describe the different variant portions of the below record. **)
  TTLSLegendInfo = (liObject, liInteger, liLegendInfo);

  (** A record to descibe the information stored in the FTimeLocationSymbols Data pointer. **)
  TTLSLegendRec = Record
    Case TTLSLegendInfo Of
      liObject :    (PObject : TObject);
      liInteger:    (iData : Cardinal);
      liLegendInfo: (iOrder : SmallInt; iDisplayOps : TDisplayOps);
  End;

  (** This record described a start and finish chainage for each frame. **)
  TFrameRecord = Record
    dblStart  : Double;
    dblFinish : Double;
  End;

  (** A type to define an array of TFrameRecords. **)
  TFrameRecordArray = Array Of TFrameRecord;

  (** A record to describe the information associated with a single quantity. **)
  TQuantity = Record
    FName       : String;
    FSheetIndex : Integer;
  End;

  (** A type to define an array of TQuantity records. **)
  TQuantityArray = Array Of TQuantity;

  (** An enumerate to define the types of graph that can be rendered. **)
  TGraphType = (gtNone, gtBar, gtLine, gtArea);

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
    //                                                                                           A4P, A4L, A3P, A3L, A2P, A2L, A1P, A1L,  A0P,  A0L
    (FDisplayName: 'Height';                    FININame: 'Height';                  FDefaults: (297, 210, 420, 297, 594, 420, 841, 594, 1089,  841)),
    (FDisplayName: 'Width';                     FININame: 'Width';                   FDefaults: (210, 297, 297, 420, 420, 594, 594, 841,  841, 1189)),
    (FDisplayName: 'Legend Width';              FININame: 'LegendWidth';             FDefaults: ( 40,  40,  55,  55,  75,  75, 110, 110,  150,  150)),
    (FDisplayName: 'Title Height';              FININame: 'TitleHeight';             FDefaults: ( 40,  40,  55,  55,  75,  75, 110, 110,  150,  150)),
    (FDisplayName: 'Overhead Height';           FININame: 'OverheadHeight';          FDefaults: ( 40,  40,  55,  55,  75,  75, 110, 110,  150,  150)),
    (FDisplayName: 'Date Width';                FININame: 'DateWidth';               FDefaults: ( 25,  25,  30,  30,  40,  40,  55,  55,   75,   75)),
    (FDisplayName: 'Legend Label';              FININame: 'LegendLabel';             FDefaults: ( 15,  15,  20,  20,  30,  30,  40,  40,   52,   52)),
    (FDisplayName: 'Week No Width';             FININame: 'WeekNoWidth';             FDefaults: (  6,   8,  10,  10,  15,  15,  20,  20,   25,   25)),
    (FDisplayName: 'Frame Gap';                 FININame: 'FrameGap';                FDefaults: (  6,   6,   7,   7,   8,   8,   9,   9,   10,   10)),
    (FDisplayName: 'Frame Margin';              FININame: 'FrameMargin';             FDefaults: (  2,   2,   3,   3,   3,   3,   4,   4,    5,    5)),
    (FDisplayName: 'Primary Quantity Height';   FININame: 'PrimaryQuantityHeight';   FDefaults: ( 30,  30,  40,  40,  55,  55,  75,  75,  100,  100)),
    (FDisplayName: 'Secondary Quantity Height'; FININame: 'SecondaryQuantityHeight'; FDefaults: ( 30,  30,  40,  40,  55,  55,  75,  75,  100,  100)),
    (FDisplayName: 'Tertiary Quantity Height';  FININame: 'TertiaryQuantityHeight';  FDefaults: ( 30,  30,  40,  40,  55,  55,  75,  75,  100,  100)),
    (FDisplayName: 'Primary Quantity Width';    FININame: 'PrimaryQuantityWidth';    FDefaults: ( 30,  30,  40,  40,  55,  55,  75,  75,  100,  100)),
    (FDisplayName: 'Secondary Quantity Width';  FININame: 'SecondaryQuantityWidth';  FDefaults: ( 30,  30,  40,  40,  55,  55,  75,  75,  100,  100)),
    (FDisplayName: 'Tertiary Quantity Width';   FININame: 'TertiaryQuantityWidth';   FDefaults: ( 30,  30,  40,  40,  55,  55,  75,  75,  100,  100))
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

  (** This is an constant array of information contain the INI name for the quantity graph
      options and there display descriptions. **)
  QuantityGraphOptions : Array[Low(TGraphHierarchy)..High(TGraphHierarchy),
    Low(TQuantityGraphOption)..High(TQuantityGraphOption)] Of
    TQuantityGraphOptionsRec = (
    (
      (FININame: 'ShowPrimaryHorizontalGraph';    FDefault: False),
      (FININame: 'ShowPrimaryHorizontalData';     FDefault: True),
      (FININame: 'StackPrimaryHorizontalGraph';   FDefault: True),
      (FININame: 'ShowPrimaryVerticalGraph';      FDefault: False),
      (FININame: 'ShowPrimaryVerticalData';       FDefault: True),
      (FININame: 'StackPrimaryVerticalGraph';     FDefault: True)
    ),
    (
      (FININame: 'ShowSecondaryHorizontalGraph';  FDefault: False),
      (FININame: 'ShowSecondaryHorizontalData';   FDefault: True),
      (FININame: 'StackSecondaryHorizontalGraph'; FDefault: True),
      (FININame: 'ShowSecondaryVerticalGraph';    FDefault: False),
      (FININame: 'ShowSecondaryVerticalData';     FDefault: True),
      (FININame: 'StackSecondaryVerticalGraph';   FDefault: True)
    ),
    (
      (FININame: 'ShowTertiaryHorizontalGraph';   FDefault: False),
      (FININame: 'ShowTertiaryHorizontalData';    FDefault: True),
      (FININame: 'StackTertiaryHorizontalGraph';  FDefault: True),
      (FININame: 'ShowTertiaryVerticalGraph';     FDefault: False),
      (FININame: 'ShowTertiaryVerticalData';      FDefault: True),
      (FININame: 'StackTertiaryVerticalGraph';    FDefault: True)
    )
  );

  (** This is a constant array of strings to represent the graph options. **)
  GraphOptions : Array[Low(TQuantityGraphOption)..High(TQuantityGraphOption)] Of
    String = (
    'Show Horizontal Graph',
    'Show Horizontal Data',
    'Stack Horizontal Graph',
    'Show Vertical Graph',
    'Show Vertical Data',
    'Stack Vertical Graph'
  );
  (** A constant array of strings representing the graph hierarchies. **)
  GraphHierarchies : Array[Low(TGraphHierarchy)..High(TGraphHierarchy)] Of String = (
    'Primary', 'Secondary', 'Tertiary');

  (** A constant array providing descriptions of the boolean options. **)
  Options : Array[Low(TTLSOption)..High(TTLSOption)] Of TTLSOptionRec = (
    ( strDescription: 'Allow room in the drawing for a overhead image.';
      strName: 'tlsoOverhead';
      GroupID: tgoLayout),
    ( strDescription: 'Transpose Chainage (High to Low instead of Low to High).';
      strName: 'tlsoTransposeChainage';
      GroupID: tgoLayout),
    ( strDescription: 'Layer diagram by symbol order in map definition.';
      strName: 'tlsoLayerBySymbol';
      GroupID: tgoMiscellaneous),
    ( strDescription: 'Show yearly grid lines';
      strName: 'tlsoShowYearlyGrid';
      GroupID: tgoGridlines),
    ( strDescription: 'Show monthly grid lines';
      strName: 'tlsoShowMonthlyGrid';
      GroupID: tgoGridlines),
    ( strDescription: 'Show weekly grid lines';
      strName: 'tlsoShowWeeklyGrid';
      GroupID: tgoGridlines),
    ( strDescription: 'Show Progress on the Time Location Symbols.';
      strName: 'tlsoShowProgress';
      GroupID: tgoProgress),
    ( strDescription: 'Transpose the Time Scale (Late to Early instead of Early to Late)';
      strName: 'tlsoTransposeTimeScale';
      GroupID: tgoLayout),
    ( strDescription: 'Suppress Descriptions on the Time Location diagrams.';
      strName: 'tlsoSuppressDescriptions';
      GroupID: tgoMiscellaneous),
    ( strDescription: 'Render Schematic Time Location Diagram in the Overhead area.';
      strName: 'tlsoRenderSchematic';
      GroupID: tgoLayout),
    ( strDescription: 'Render Time Location information by Route Code (separate diagram ' +
      'for each route).';
      strName: 'tlsoRenderByRoute';
      GroupID: tgoRouteCode),
    ( strDescription: 'Render only used Time Location Symbols in the Legend.';
      strName: 'tlsoRenderUsedLegends';
      GroupID: tgoMiscellaneous),
    ( strDescription: 'Automatically increment the drawing revision (on loading of settings).';
      strName: 'tlsoIncrementDrawingRev';
      GroupID: tgoMiscellaneous),
    ( strDescription: 'Render Route Codes on a single drawing as individual frames.';
      strName: 'tlsoRenderRouteCodesSingleDrn';
      GroupID: tgoRouteCode),
    ( strDescription: 'Render Time Location in time (Hours and Minutes) instead of days.';
      strName: 'tlsoRenderTLInHours';
      GroupID: tgoProgress),
    ( strDescription: 'Groups objects in layers on the diagram.';
      strName: 'tlsoGroupObjectsInLayers';
      GroupID: tgoLayout)
  );

  (** A constant array of names for each of the option groups. **)
  Groups : Array[Low(TTLSOptionGroup)..High(TTLSOptionGroup)] Of String = (
    'Layout Options',
    'Gridline Options',
    'Route Code Options',
    'Progress Options',
    'Miscellaneous Options'
  );

  (** A constant array to describe the font names and ini keys. **)
  FontTypes : Array[Low(TFontType)..High(TFontType)] Of TFontTypeRec = (
    (FDisplayName: 'Legend Header Font';  FININame: 'LegendHeaderFont';
      FFontName: 'Arial'; FFontStyle: []; FFontSize: 24; FFontColour: xlcBlack),
    (FDisplayName: 'Legend Text Font';  FININame: 'LegendTextFont';
      FFontName: 'Arial'; FFontStyle: []; FFontSize: 10; FFontColour: xlcBlack),
    (FDisplayName: 'Shape Font'; FININame: 'ShapeFont';
      FFontName: 'Arial'; FFontStyle: []; FFontSize: 10; FFontColour: xlcBlack),
    (FDisplayName: 'Chainage Grid Font';  FININame: 'GridFont';
      FFontName: 'Arial'; FFontStyle: []; FFontSize: 10; FFontColour: xlcBlack),
    (FDisplayName: 'Date Header Font';  FININame: 'DateHeaderFont';
      FFontName: 'Arial'; FFontStyle: [fsBold]; FFontSize: 10; FFontColour: xlcBlack),
    (FDisplayName: 'Date and Week Number Font';  FININame: 'DateFont';
      FFontName: 'Arial'; FFontStyle: []; FFontSize: 10; FFontColour: xlcBlack),
    (FDisplayName: 'Schematic Text Font';  FININame: 'SchematicTextFont';
      FFontName: 'Arial'; FFontStyle: []; FFontSize: 10; FFontColour: xlcBlack),
    (FDisplayName: 'Quantity Text Font';  FININame: 'QuantityTextFont';
      FFontName: 'Arial'; FFontStyle: []; FFontSize: 10; FFontColour: xlcBlack),
    (FDisplayName: 'Quantity Title Text Font';  FININame: 'QuantityTitleTextFont';
      FFontName: 'Arial'; FFontStyle: [fsBold]; FFontSize: 10; FFontColour: xlcBlack),
    (FDisplayName: 'Frame Watermark Font';  FININame: 'FrameWatermarkFont';
      FFontName: 'Arial'; FFontStyle: []; FFontSize: 32; FFontColour: xlcGRAY25)
  );

  (** A constant array of defaults for TLS Object Types. **)
  TLSDrawingObjects : Array[Low(TTLSDrawingObject)..High(TTLSDrawingObject)] Of TTLSObjectRec = (
    (FName: 'Outer Frame'; FSymbol: 'RECTANGLE,Black,Solid,3,None,None,None,0'),
    (FName: 'Title Divider'; FSymbol: 'LINE,Black,Solid,3'),
    (FName: 'Legend Divider'; FSymbol: 'LINE,Black,Solid,3'),
    (FName: 'Inner Frame'; FSymbol: 'RECTANGLE,Black,Solid,1.5,None,None,None,0'),
    (FName: 'Progress'; FSymbol: 'RECTANGLE,Gray-50%,Solid,0.25,Gray-25%,DarkDownwardDiagonal,White,0'),
    (FName: 'Holiday'; FSymbol: 'RECTANGLE,Gray-50%,Solid,0.25,Gray-25%,None,None,50'),
    (FName: 'Yearly Gridline'; FSymbol: 'LINE,Gray-50%,Solid,1.5'),
    (FName: 'Monthly Gridline'; FSymbol: 'LINE,Gray-50%,Solid,1'),
    (FName: 'Weekly Gridline'; FSymbol: 'LINE,Gray-50%,Solid,0.5'),
    (FName: 'Daily Gridline'; FSymbol: 'LINE,Gray-50%,Solid,0.25'),
    (FName: 'Chainage Gridline'; FSymbol: 'LINE,Gray-50%,Solid,0.5'),
    (FName: 'Time Now'; FSymbol: 'LINE,Red,Solid,2.25'),
    (FName: 'Date Separator'; FSymbol: 'LINE,Black,Solid,1'),
    (FName: 'Schematic Frame'; FSymbol: 'RECTANGLE,Black,Solid,1.5,None,None,None,0'),
    (FName: 'Schematic Centre Line'; FSymbol: 'LINE,Red,LongDashDot,0.25'),
    (FName: 'Overhead Frame'; FSymbol: 'RECTANGLE,Black,Solid,1.5,None,None,None,0'),
    (FName: 'Quantity Frame'; FSymbol: 'RECTANGLE,Black,Solid,1.5,None,None,None,0'),
    (FName: 'Quantity Gridlines'; FSymbol: 'RECTANGLE,Gray-50%,Solid,0.25,None,None,None,0')
  );

  (** A constant string to represent the holiday section of a profile in the TLI file. **)
  strHolidays = '.Holidays';
  (** A constant string to represent the Schematic Script section of a profile in the TLI
      file. **)
  strSchematicScript = '.SchematicScript';
  (** A constant string to represent the Schematic Symbols section of a profile in the
      TLI file. **)
  strSchematicSymbols = '.SchematicSymbols';
  (** A constant string to represent the Time Location Symbols section of a profile in the
      TLI file. **)
  strTimeLocationSymbols = '.TimeLocationSymbols';
  (** A constant string to represent the Title Block Objects section of a profile in the
      TLI file. **)
  strTitleBlockObjects = '.TitleBlockObjects';
  (** A constant string to represent the Horizontal Quantities section of a profile in the
      TLI file. **)
  strHorizontalQuantities = '.HorizontalQuantities';
  (** A constant string to represent the Vertical Quantities section of a profile in the
      TLI file. **)
  strVerticalQuantities = '.VerticalQuantities';
  (** A constant string to represent the Quantities Grid Intervals section of a profile in
      the TLI file. **)
  strQuantityGridIntervals = '.QuantityGridIntervals';
  (** A constant string to represent the Quantities Time Intervals section of a profile in
      the TLI file. **)
  strQuantityTimeIntervals = '.QuantityTimeIntervals';
  (** A constant string to represent the Quantities Graph Options section of a profile in
      the TLI file. **)
  strQuantityGraphOptions = '.QuantityGraphOptions';
  (** A constant array of section ends to exclude from being shown in the Profiles
      dropdown. **)
  strExcludedSections : Array[1..10] Of String = (
    strHolidays,
    strSchematicScript,
    strSchematicSymbols,
    strTimeLocationSymbols,
    strTitleBlockObjects,
    strHorizontalQuantities,
    strVerticalQuantities,
    strQuantityGridIntervals,
    strQuantityTimeIntervals,
    strQuantityGraphOptions
  );

  (** A constant array to provide string representation of the TProfileData enumerates. **)
  strProfileData : Array[Low(TProfileData)..High(TProfileData)] of String = (
    'Time Location Symbol Options',
    'Dimensions',
    'Title Block',
    'Max & Min Dates & Chaianges',
    'Holidays',
    'Holiday Options',
    'Fonts',
    'Drawing Objects',
    'General Options',
    'Schematic Script',
    'Schematic Symbols'
  );

  (** A default set of colours to be assigned to NEW graphs as defaults. **)
  DefaultColours : Array[1..36] Of TColour = (
    xlcRED,
    xlcYELLOW,
    xlcLIME,
    xlcAQUA,
    xlcBLUE,

    xlcDARKRED,
    xlcDARKYELLOW,
    xlcGREEN,
    xlcTEAL,
    xlcDARKBLUE,

    xlcBROWN,
    xlcOLIVEGREEN,
    xlcDARKGREEN,
    xlcDARKTEAL,
    xlcBLUEGRAY,

    xlcINDIGO,
    xlcORANGE,
    xlcGRAY50,
    xlcLIGHTORANGE,
    xlcSEAGREEN,
    xlcLIGHTBLUE,
    xlcVIOLET,
    xlcPINK,
    xlcGOLD,
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
    xlcLAVENDER
  );

  (** A constant array of strings to represent the graph types. **)
  GraphTypes : Array[Low(TGraphType)..High(TGraphType)] Of String = (
    'None', 'Bar', 'Line', 'Area');

Implementation

End.
