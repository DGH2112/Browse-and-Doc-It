(**

  This module contains type and constants for the Eidolon system.

  @Author  David Hoyle
  @Date    12 Mar 2017
  @Version 1.0

**)
Unit BADI.Eidolon.Types;

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
    tlsoGroupObjectsInLayers,
    tlsoHolidaysInGraphs
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
  TProfileData = (
    pdTLSOptions,
    pdDimensions,
    pdTitleBlock,
    pdMaxMinDateChaiange,
    pdHolidays,
    pdHolidayOptions,
    pdFonts,
    pdDrawingObjects,
    pdGeneralOptions,
    pdSchematicScript,
    pdSchematicSymbols,
    pdQuantityOptions,
    pdQuantityGraphData
  );

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

  (** An enumerate to define the field type for the data. **)
  TFieldType = (ftBoolean, ftByte, ftInteger, ftLong, ftCurrency, ftSingle,
    ftDouble, ftDate, ftBinary, ftText, ftLongBinary, ftMemo, ftGUID, ftBigInt,
    ftvarBinary, ftChar, ftNumeric, ftDecimal, ftFloat, ftTime, ftTimeStamp);

  (** This is an enumerate to define the two types of connection. **)
  TConnectionType = (ctPrimary, ctSecondary);

  (** An enumerate to describe which end is being parsed. **)
  TLineEnd = (leStart, leEnd);

Implementation

End.
