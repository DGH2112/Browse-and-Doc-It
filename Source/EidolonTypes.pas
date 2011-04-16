(**

  This module contains type and constants for the Eidolon system.

  @Author  David Hoyle
  @Date    16 Apr 2011
  @Version 1.0

**)
Unit EidolonTypes;

Interface

Type
    (** An enumerate for the symbol types. **)
    TSymbolType = (tstRectangle, tstBar, tstLine, tstEllipse, tstTriangle,
      tstDiamond);

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

Const
    (** A constant array of names for the enumerated symbols types. **)
    strSymbolTypes : Array[Low(TSymbolType)..High(TSymbolType)] Of String = (
      'Rectangle', 'Bar', 'Line', 'Ellipse', 'Triangle', 'Diamond'
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

Implementation

End.
