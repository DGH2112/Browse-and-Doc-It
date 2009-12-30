(**

  EidolonModule : A unit to parser Eidolon code. Please refer to the file
  "Eidolon Map File Grammar.bnf" for the complete grammar implemented.

  @Version    1.0
  @Date       30 Dec 2009
  @Author     David Hoyle

**)
Unit EidolonModule;

Interface

Uses
  SysUtils, Windows, Contnrs, Classes, BaseLanguageModule;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A XML specific implementation of comments. **)
  TEidolonComment = Class(TComment)
  Public
    Class Function CreateComment(strComment: String; iLine,
      iCol: Integer): TComment; Override;
  End;

  (** An enumerate to define the field type for the data. **)
  TFieldType = (ftBoolean, ftByte, ftInteger, ftLong, ftCurrency, ftSingle,
    ftDouble, ftDate, ftText, ftLongBinary, ftMemo);

  (** A class to represent a Database definition. **)
  TFieldDef = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FOutputName : String;
    FFieldType  : TFieldType;
    FFieldWidth : Integer;
    FPrimaryKey: Boolean;
  Public
    Constructor Create(strName : String; AScope : TScope; iLine,
      iColumn : Integer; AImageIndex : TImageIndex; AComment : TComment); Override;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property gets and sets the output name of the field.
      @precon  None.
      @postcon Gets and sets the output name of the field.
      @return  a String
    **)
    Property OutputName : String Read FOutputName Write FOutputName;
    (**
      This property gets and sets the Field Type.
      @precon  None.
      @postcon Gets and sets the Field Type.
      @return  a TFieldType
    **)
    Property FieldType : TFieldType Read FFieldType Write FFieldType;
    (**
      This property gets and sets the Field Width.
      @precon  None.
      @postcon Gets and sets the Field Width.
      @return  a Integer
    **)
    Property FieldWidth : Integer Read FFieldWidth Write FFieldWidth;
    (**
      This property gets and sets the Primary Key.
      @precon  None.
      @postcon Gets and sets the Primary Key.
      @return  a Boolean
    **)
    Property PrimaryKey : Boolean Read FPrimaryKey Write FPrimaryKey;
  End;

  (** A base class from which all the tables are derived. **)
  TBaseTable = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FFields: TLabelContainer;
  Public
    Function  AddField(Field : TFieldDef) : TFieldDef;
  End;

  (** A class to represent a TextTable definition. **)
  TTextTable = Class(TBaseTable)
    FFileName : String;
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This method gets and sets the filename of the text table.
      @precon  None.
      @postcon Gets or sets the filename of the text table.
      @return  a String
    **)
    Property FileName : String Read FFileName Write FFileName;
  End;

  (** This is a base class for all the database connections. **)
  TDBConnection = Class(TElementContainer);

  (** A class to represent a TextTable definition. **)
  TDBTable = Class(TBaseTable)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FPrimary: TLabelContainer;
    FSecondary: TLabelContainer;
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    Function AddPrimary(DBConnection : TDBConnection) : TDBConnection;
    Function AddSecondary(DBConnection : TDBConnection) : TDBConnection;
  End;

  (** This is a class to represent an Output Table. **)
  TOutputTable = Class(TDBTable)
  {$IFDEF D2005} Strict {$ENDIF} Private
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

  (** This is a class to represent an Association in a Requirements Table. **)
  TAssociation = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

  (** This is a class to represent a requirements Table. **)
  TRequirementsTable = Class(TDBTable)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FAssociations: TLabelContainer;
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    Function AddAssociation(Association : TAssociation) : TAssociation;
  End;

  (** An enumerate for the symbol types. **)
  TSymbolType = (stRectangle, stBar, stLine, stEllipse, stTriangle, stDiamond);

  (** An enumerate to defined the Excel Colour Indexes **)
  TColour = (
    xlcNONE           = -1,
    xlcBLACK          =  1,
    xlcBROWN          = 53,
    xlcOLIVEGREEN     = 52,
    xlcDARKGREEN      = 51,
    xlcDARKTEAL       = 49,
    xlcDARKBLUE       = 11,
    xlcINDIGO         = 55,
    xlcGRAY80         = 56,
    xlcDARKRED        =  9,
    xlcORANGE         = 46,
    xlcDARKYELLOW     = 12,
    xlcGREEN          = 10,
    xlcTEAL           = 14,
    xlcBLUE           =  5,
    xlcBLUEGRAY       = 47,
    xlcGRAY50         = 16,
    xlcRED            =  3,
    xlcLIGHTORANGE    = 45,
    xlcLIME           = 43,
    xlcSEAGREEN       = 50,
    xlcAQUA           = 42,
    xlcLIGHTBLUE      = 41,
    xlcVIOLET         = 13,
    xlcGRAY40         = 48,
    xlcPINK           =  7,
    xlcGOLD           = 44,
    xlcYELLOW         =  6,
    xlcBRIGHTGREEN    =  4,
    xlcTURQUOISE      =  8,
    xlcSKYBLUE        = 33,
    xlcPLUM           = 54,
    xlcGRAY25         = 15,
    xlcROSE           = 38,
    xlcTAN            = 40,
    xlcLIGHTYELLOW    = 36,
    xlcLIGHTGREEN     = 35,
    xlcLIGHTTURQUOISE = 34,
    xlcPALEBLUE       = 37,
    xlcLAVENDER       = 39,
    xlcWHITE          =  2
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

  (** A base class for all Time Location Symbols **)
  TSymbol = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FBorderColour :TColour;
    FBorderLineStyle : TLineStyle;
    FBorderWeight : TLineWeight;
    FSymbolType: TSymbolType;
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property gets and sets the symbol type.
      @precon  None.
      @postcon Gets and sets the symbol type.
      @return  a TSymbolType
    **)
    Property SymbolType : TSymbolType Read FSymbolType Write FSymbolType;
    (**
      This property gets and sets the border colour of the symbol.
      @precon  None.
      @postcon Gets and sets the border colour of the symbol.
      @return  a TColour
    **)
    Property BorderColour : TColour Read FBorderColour Write FBorderColour;
    (**
      This property gets and sets the border line style of the symbol.
      @precon  None.
      @postcon Gets and sets the border line style of the symbol.
      @return  a TLineStyle
    **)
    Property BorderLineStyle : TLineStyle Read FBorderLineStyle Write FBorderLineStyle;
    (**
      This property gets and sets the border line weight of the symbol.
      @precon  None.
      @postcon Gets and sets the border line weight of the symbol.
      @return  a TLineWeight
    **)
    Property BorderWeight : TLineWeight Read FBorderWeight Write FBorderWeight;
  End;

  (** A class to represent a TimeLocationTable definition. **)
  TTimeLocationTable = Class(TBaseTable)
    FSymbols: TLabelContainer;
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    Function AddSymbol(Symbol : TSymbol) : TSymbol;
  End;

  (** A class to represent a TextTable definition. **)
  TTextTableDef = Class(TElementContainer)
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

  (** A class to represent a Database definition. **)
  TDatabaseDef = Class(TDBConnection)
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

  (** A class to represent a connection definition. **)
  TConnectionDef = Class(TDBConnection)
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

  (** A class to represent a Table Name definition. **)
  TTableNameDef = Class(TDBConnection)
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

  (** A class to represent a LINE time location symbol **)
  TLine = Class(TSymbol);

  (** A custom class to contain the main properties of symbols with areas. **)
  TCustomFillSymbol = Class(TLine)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FInteriorColour : TColour;
    FInteriorPattern : TInteriorPattern;
    FInteriorPatternColour : TColour;
    FTransparency : Integer;
  Public
    Constructor Create(strName : String; AScope : TScope; iLine,
      iColumn : Integer; AImageIndex : TImageIndex; AComment : TComment); Override;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property gets and sets the Interior Colour of the rectangle.
      @precon  None.
      @postcon Gets and sets the Interior Colour of the rectangle.
      @return  a TColour
    **)
    Property InteriorColour : TColour Read FInteriorColour Write FInteriorColour;
    (**
      This property gets and sets the Interior Pattern of the rectangle.
      @precon  None.
      @postcon Gets and sets the Interior Pattern of the rectangle.
      @return  a TInteriorPattern
    **)
    Property InteriorPattern : TInteriorPattern Read FInteriorPattern Write FInteriorPattern;
    (**
      This property gets and sets the Interior Pattern Colour of the rectangle.
      @precon  None.
      @postcon Gets and sets the Interior Pattern Colour of the rectangle.
      @return  a TColour
    **)
    Property InteriorPatternColour : TColour Read FInteriorPatternColour Write FInteriorPatternColour;
    (**
      This property gets and sets the transparency of the rectangle.
      @precon  None.
      @postcon Gets and sets the transparency of the rectangle.
      @return  a Integer
    **)
    Property Transparency : Integer Read FTransparency Write FTransparency;
  End;

  (** A class to represent a RECTANGLE time location symbol **)
  TRectangle = Class(TCustomFillSymbol)
  {$IFDEF D2005} Strict {$ENDIF} Private
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

  (** A class to represent a BAR time location symbol **)
  TBar = Class(TCustomFillSymbol)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FBarWidth : Integer;
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property gets and sets the bar width of the bar.
      @precon  None.
      @postcon Gets and sets the bar width of the bar.
      @return  a Integer
    **)
    Property BarWidth : Integer Read FBarWidth Write FBarWidth;
  End;

  (** A class to represent a DIAMOND time location symbol **)
  TDiamond = Class(TCustomFillSymbol)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FDiamondSize : Integer;
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property gets and sets the bar width of the bar.
      @precon  None.
      @postcon Gets and sets the bar width of the bar.
      @return  a Integer
    **)
    Property DiamondSize : Integer Read FDiamondSize Write FDiamondSize;
  End;

  (** An enumerate to define the types of Triangle. **)
  TTriangleType = (ttStartAndEarly, ttStartAndLate, ttEndAndEarly, ttEndAndLate);

  (** A class to represent a DIAMOND time location symbol **)
  TTriangle = Class(TCustomFillSymbol)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FTriangleType : TTriangleType;
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property gets and sets the bar width of the bar.
      @precon  None.
      @postcon Gets and sets the bar width of the bar.
      @return  a TTriangleType
    **)
    Property TriangleType : TTriangleType Read FTriangleType Write FTriangleType;
  End;

  (** A class to represent a DIAMOND time location symbol **)
  TEllipse = Class(TCustomFillSymbol)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FEllipseSize : Integer;
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property gets and sets the bar width of the bar.
      @precon  None.
      @postcon Gets and sets the bar width of the bar.
      @return  a Integer
    **)
    Property EllipseSize : Integer Read FEllipseSize Write FEllipseSize;
  End;

  (** This is an enumerate to define the two types of connection. **)
  TConnectionType = (ctPrimary, ctSecondary);

  (** This is the main class for dealing with backus-naur grammar files. **)
  TEidolonModule = Class(TBaseLanguageModule)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FSource : String;
    FTextTableDefs: TLabelContainer;
    FDBTableDefs  : TLabelContainer;
    FTimeLocationTableDefs: TLabelContainer;
    FOutputTableDefs: TLabelContainer;
    FRequirementsTableDefs: TLabelContainer;
    { Grammar Parsers }
    Procedure Goal;
    Function  Table : Boolean;
    Function  TextTable(strName : String; StartToken : TTokenInfo) : Boolean;
    Function  DBTable(strName : String; StartToken : TTokenInfo) : Boolean;
    Function  TimeLocationTable(strName : String; StartToken : TTokenInfo) : Boolean;
    Procedure TextTableDef(TextTable : TTextTable);
    Function  FieldDef(Table : TBaseTable): Boolean;
    Procedure TypeInfo(Field : TFieldDef);
    Function  DatabaseDef(DBTable : TDBTable; ConnectionType : TConnectionType) : Boolean;
    Procedure ConnectionDef(DBTable : TDBTable; ConnectionType : TConnectionType);
    Procedure TableNameDef(DBTable : TDBTable; ConnectionType : TConnectionType);
    Function  TimeLocationDef(Table : TTimeLocationTable) : Boolean;
    Function  TLLine(strName : String; StartToken : TTokenInfo; TLT : TTimeLocationTable) : Boolean;
    Procedure BorderDef(Symbol : TSymbol);
    Procedure BorderColour(Symbol : TSymbol);
    Function  ColourName : TColour;
    Procedure BorderLineStyle(Symbol : TSymbol);
    Procedure BorderWeight(Symbol : TSymbol);
    Function  TLRectangle(strName : String; StartToken : TTokenInfo; TLT : TTimeLocationTable) : Boolean;
    Function  TLBar(strName : String; StartToken : TTokenInfo; TLT : TTimeLocationTable) : Boolean;
    Procedure InteriorDef(CustomSymbol : TCustomFillSymbol);
    Procedure Transparency(CustomSymbol : TCustomFillSymbol);
    Procedure InteriorColour(CustomSymbol : TCustomFillSymbol);
    Procedure InteriorPattern(CustomSymbol : TCustomFillSymbol);
    Procedure InteriorPatternColour(CustomSymbol : TCustomFillSymbol);
    Procedure BarWidth(Bar : TBar);
    Function  TLDiamond(strName : String; StartToken : TTokenInfo; TLT : TTimeLocationTable) : Boolean;
    Function  TLTriangle(strName : String; StartToken : TTokenInfo; TLT : TTimeLocationTable) : Boolean;
    Function  TLEllipse(strName : String; StartToken : TTokenInfo; TLT : TTimeLocationTable) : Boolean;
    Procedure DiamondSize(Diamond : TDiamond);
    Procedure TriangleType(Triangle : TTriangle);
    Procedure EllipseSize(Ellipse : TEllipse);
    Function  OutputTable(strName : String; StartToken : TTokenInfo) : Boolean;
    Function  RequirementsTable(strName : String; StartToken : TTokenInfo) : Boolean;
    Function  AssociationDef(RequirementsTable : TRequirementsTable): Boolean;
    (* Helper method to the grammar parsers *)
    Procedure TokenizeStream;
    Procedure ParseTokens;
    Procedure EatLineEnds;
    Function  CheckLiteral(strLiteral, strMethod : String) : Boolean;
    Function  CheckLineEnd(strMethod : String) : Boolean;
    Procedure EatWhitespace;
    Function  EmptyLine : Boolean;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetComment(
      CommentPosition : TCommentPosition = cpBeforeCurrentToken) : TComment;
      Override;
    procedure TidyUpEmptyElements;
    Function GetModuleName : String; Override;
  Public
    Constructor CreateParser(Source : String; strFileName : String;
      IsModified : Boolean; ModuleOptions : TModuleOptions); Override;
    Destructor Destroy; Override;
    Function KeyWords : TKeyWords; Override;
    Procedure ProcessCompilerDirective(var iSkip : Integer); Override;
    Function ReferenceSymbol(AToken : TTokenInfo) : Boolean; Override;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Const
  (** A constant array of names for the enumerated symbols types. **)
  strSymbolTypes : Array[Low(TSymbolType)..High(TSymbolType)] Of String = (
    'Rectangle', 'Bar', 'Line', 'Ellipse', 'Triangle', 'Diamond'
  );

  (** A constant array providing names for the enumerated colours. **)
  strColours : Array[Low(TColour)..High(TColour)] Of String = (
    'None',               // = -1
    '',
    'Black',              // =  1
    'White',              // =  2
    'Red',                // =  3
    'BrightGreen',        // =  4
    'Blue',               // =  5
    'Yellow',             // =  6
    'Pink',               // =  7
    'Turquoise',          // =  8
    'DarkRed',            // =  9
    'Green',              // = 10
    'DarkBlue',           // = 11
    'DarkYellow',         // = 12
    'Violet',             // = 13
    'Teal',               // = 14
    'Gray-25%',           // = 15
    'Gray-50%',           // = 16
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    '',
    'SkyBlue',            // = 33
    'LightTurquoise',     // = 34
    'LightGreen',         // = 35
    'LightYellow',        // = 36
    'PaleBlue',           // = 37
    'Rose',               // = 38
    'Lavender',           // = 39
    'Tan',                // = 40
    'LightBlue',          // = 41
    'Aqua',               // = 42
    'Lime',               // = 43
    'Gold',               // = 44
    'LightOrange',        // = 45
    'Orange',             // = 46
    'Blue-Gray',          // = 47
    'Gray-40%',           // = 48
    'DarkTeal',           // = 49
    'SeaGreen',           // = 50
    'DarkGreen',          // = 51
    'OliveGreen',         // = 52
    'Brown',              // = 53
    'Plum',               // = 54
    'Indigo',             // = 55
    'Gray-80%'            // = 56
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

  (** This is a class to represent the starting symbols for Primary and
      Secondary connections. **)
  strConnectionType : Array[Low(TConnectionType)..High(TConnectionType)] Of
    String = ('#', '@');

ResourceString
  (** A resource string for the Text Table definitions. **)
  strTextTableDefinitions = 'Text Table Definitions';

Implementation

Uses
  DGHLibrary;

resourcestring
  (** A resource string for an error message where a line end token was expected. **)
  strExpectedLineEnd = 'Expected <LineEnd> but found ''%s'' at line %d colum' +
  'n %d.';
  (** A resource string for an error message where a name is null. **)
  strTheDefintionNull = 'The defintion name can not be null at line %d column' +
  ' %d.';
  (** A resource string for an invalid data type. **)
  strInvalidDataType = 'Invalid data type ''%s'' found at line %d column %d.';
  (** A resource string for an invalid data width. **)
  strInvalidDataWidth = 'Invalid data width ''%s'' at line %d colunm %d.';
  (** A resource string for an invalid colour name. **)
  strInvalidColourName = 'Invalid colour name ''%s'' at line %d column %d.';
  (** A resource string for an invalid line style. **)
  strInvalidLineStyle = 'Invalid line style ''%s'' at line %d column %d.';
  (** A resource string for an invalid line weight. **)
  strInvalidLineWeight = 'Invalid line weight ''%s'' at line %d column %d.';
  (** A resource string for an invalid pattern. **)
  strInvalidPattern = 'Invalid pattern ''%s'' at line %d column %d.';
  (** A resource string for an invalid number. **)
  strInvalidNumber = 'Invalid number ''%s'' at line %d column %d.';
  (** A resource string for an interior colour with no pattern **)
  strYouHaveSpecifiedAColour = 'You have specified an interior colour ''%s''' +
  ' when the pattern is None';
  (** A resource string for an pattern and no colour. *)
  strYouHaveSpecifiedAPattern = 'You have specified a pattern ''%s'' but no ' +
  'interior colour.';
  (** A resource string for an invalid table type. *)
  strInvalidTableType = 'Invalid table type ''%s'' at line %d column %d.';
  (** A resource string for an null name message. **)
  strNullName = 'The name can not by null at line %d column %d.';
  (** A resource string for a dir or file that does not exist. **)
  strThisFileDirDoesNotExist = 'This file/directory ''%s'' does not exists a' +
  't line %d column %d.';
  (** A resource string for an invalid connection string. **)
  strIsNotAValidConnection = '''%s'' is not a valid connection string at lin' +
  'e %d column %d.';

Const

  (** A set of reserved words (not used in this parser.) **)
  strReservedWords : Array[0..11] Of String = (
    'bar',
    'class',
    'dbtable',
    'diamond',
    'ellipse',
    'line',
    'outputtable',
    'rectangle',
    'requirementstable',
    'texttable',
    'timelocationtable',
    'triangle'
  );

  (** This is a list of reserved, directives word and a semi colon which are
      token that can be sort as then next place to start parsing from when an
      error is  encountered. **)
  strSeekableOnErrorTokens : Array[1..2] Of String = ('<CR>', '<LF>');

(**

  This function returns a literal representation of the passed element.

  @precon  Element must be a valid instance of a TElementContainer.
  @postcon Returns a literal representation of the passed element.

  @param   Element as a TElementContainer
  @return  a String

**)
Function BuildLiteralString(Element : TElementContainer) : String;

Var
  i : Integer;

Begin
  Result := '';
  For i := 0 To Element.TokenCount - 1 Do
    Result := Result + Element.Tokens[i].Token;
End;

(**


  This method is a class method to first check the comment for being a
  documentation comment and then creating an instance of a TComment class and
  parsing the comment via the constructor.

  @precon  strComment is the full comment to be checked and parsed, iLine is
           the line number of the comment and iCol is the column number of
           the comment.

  @postcon Returns Nil if this is not a documentation comment or returns a
           valid TComment class.

  @param   strComment as a String
  @param   iLine      as an Integer
  @param   iCol       as an Integer
  @return  a TComment

**)
class function TEidolonComment.CreateComment(strComment: String; iLine,
  iCol: Integer): TComment;

begin //: @note Not currently configured or used.
  Result := Nil;
  If Length(strComment) > 0 Then
    Begin
      Case strComment[1] Of
        '/' : strComment := Copy(strComment, 2, Length(strComment) - 1);
      End;
      If Length(strComment) > 0 Then
        Begin
          If strComment[1] = '*' Then
            strComment := Copy(strComment, 2, Length(strComment) - 3);
          If strComment[1] = '/' Then
            strComment := Copy(strComment, 2, Length(strComment) - 1);
          If Length(strComment) > 0 Then
            Begin
              If strComment[1] = ':' Then
                Begin;
                  strComment := Copy(strComment, 2, Length(strComment) - 1);
                  Result := Create(strComment, iLine, iCol);
                End
              Else If strComment[1] = '*' Then
                Begin;
                  strComment := Copy(strComment, 2, Length(strComment) - 2);
                  Result := Create(strComment, iLine, iCol);
                End;
            End;
        End;
    End;
end;

{ TBaseTable }

(**

  This method adds fields to the table definition.

  @precon  Field must be a valid instance of a TFieldDef class.
  @postcon Adds fields to the table definition.

  @param   Field as a TFieldDef
  @return  a TFieldDef

**)
function TBaseTable.AddField(Field: TFieldDef): TFieldDef;
begin
  If FFields = Nil Then
    FFields := Add(TLabelContainer.Create('Fields', scNone, 0, 0, iiFieldsLabel,
      Nil)) As TLabelContainer;
  Result := FFields.Add(Field) As TFieldDef;
end;

{ TTextTable }

(**

  This method returns string representation of the text table definition.

  @precon  None.
  @postcon Returns string representation of the text table definition.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TTextTable.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;

begin
  Result := Identifier + '=Class(TextTable)';
end;

{ TDBTable }

(**

  This method adds a database connection element to the Primary section of the
  DBTable element.

  @precon  DBConnection must be a valid instance of a TDBConnection class.
  @postcon Adds a database connection element to the Primary section of
           DBTable element.

  @param   DBConnection as a TDBConnection
  @return  a TDBConnection

**)
function TDBTable.AddPrimary(DBConnection: TDBConnection): TDBConnection;
begin
  If FPrimary = Nil Then
    FPrimary := Add(TLabelContainer.Create('Primary', scNone, 0, 0,
      iiPublicTypesLabel, Nil)) As TLabelContainer;
  Result := FPrimary.Add(DBConnection) As TDBConnection;
end;

(**

  This method adds a database connection element to the Secondary section of the
  DBTable element.

  @precon  DBConnection must be a valid instance of a TDBConnection class.
  @postcon Adds a database connection element to the Secondary section of
           DBTable element.

  @param   DBConnection as a TDBConnection
  @return  a TDBConnection

**)
function TDBTable.AddSecondary(DBConnection: TDBConnection): TDBConnection;
begin
  If FSecondary = Nil Then
    FSecondary := Add(TLabelContainer.Create('Secondary', scNone, 0, 0,
      iiPublicTypesLabel, Nil)) As TLabelContainer;
  Result := FSecondary.Add(DBConnection) As TDBConnection;
end;

(**

  This method returns string representation of the database table definition.

  @precon  None.
  @postcon Returns string representation of the database table definition.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TDBTable.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;

begin
  Result := Identifier + '=Class(DBTable)';
end;

{ TTimeLocationTable }

(**

  This method adds the given Symbol to the time location table.

  @precon  Symbol must be a valid instance of a TSymbol class.
  @postcon A

  @param   Symbol as a TSymbol
  @return  a TSymbol

**)
function TTimeLocationTable.AddSymbol(Symbol: TSymbol): TSymbol;
begin
  If FSymbols = Nil Then
    FSymbols := Add(TLabelContainer.Create('Symbols', scNone, 0, 0, iiObjectsLabel,
      Nil)) As TLabelContainer;
  Result := FSymbols.Add(Symbol) As TSymbol;
end;

(**

  This method returns string representation of the time location table definition.

  @precon  None.
  @postcon Returns string representation of the time location table definition.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TTimeLocationTable.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;

begin
  Result := Identifier + '=Class(TimeLocationTable)';
end;

{ TTextTableDef }

(**

  This method returns string representation of the Text Table Definition.

  @precon  None.
  @postcon Returns string representation of the Text Table Definition.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TTextTableDef.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;

begin
  Result := BuildStringRepresentation(boolShowIdentifier, boolForDocumentation,
    '=', BrowseAndDocItOptions.MaxDocOutputWidth, ['=', ':', '\', #32],
    ['=', ':', '\', #32], []);
end;

{ TFieldDef }

(**

  This method returns string representation of the Field Definition.

  @precon  None.
  @postcon Returns string representation of the Field Definition.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TFieldDef.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;

Const
  strFieldType : Array[Low(TFieldType)..High(TFieldType)] Of String = (
    'B', 'Y', 'I', 'L', 'U', 'S', 'F', 'D', 'C', 'O', 'M');
  strPrimaryKey : Array[False..True] Of String = ('', '*');

begin
  Result := strPrimaryKey[FPrimaryKey] + Identifier + ':' +
    strFieldType[FFieldType];
  If FFieldType = ftText Then
    Result := Result + Format('(%d)', [FFieldWidth]);
  If FOutputName <> '' Then
    Result := Result + '=' + FOutputName;
end;

(**

  A constructor for the TFieldDef class.

  @precon  None.
  @postcon Defaults the field type to text with a width of 255 characters.

  @param   strName     as a String
  @param   AScope      as a TScope
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   AImageIndex as a TImageIndex
  @param   AComment    as a TComment

**)
constructor TFieldDef.Create(strName: String; AScope: TScope; iLine,
  iColumn: Integer; AImageIndex: TImageIndex; AComment: TComment);
begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FFieldType := ftText;
  FFieldWidth := 255;
  FPrimaryKey := False;
end;

{ TDatabaseDef }

(**

  This method returns string representation of the Database Definition.

  @precon  None.
  @postcon Returns string representation of the Database Definition.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TDatabaseDef.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;

begin
  Result := Identifier + '=' + BuildLiteralString(Self);
end;

{ TConnectionDef }

(**

  This method returns string representation of the Connection Definition.

  @precon  None.
  @postcon Returns string representation of the Connection Definition.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TConnectionDef.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;

begin
  Result := Identifier + '=' + BuildLiteralString(Self);
end;

{ TTableNameDef }

(**

  This method returns string representation of the Table Name Definition.

  @precon  None.
  @postcon Returns string representation of the Table Name Definition.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TTableNameDef.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;
begin
  Result := BuildStringRepresentation(boolShowIdentifier, boolForDocumentation,
    '=', BrowseAndDocItOptions.MaxDocOutputWidth, ['=', ':', '\', '.', ';', #32],
    ['=', ':', '\', '.', ';', #32], []);
end;

(**

  This method parses the Bar element of the grammar.

  @precon  StartToken must ba a valid instance of a TTokenInfo class and
           TLT must be a valid instance of a TTimeLocationTable class.
  @postcon Returns true if a BAR was parsed.

  @param   strName    as a String
  @param   StartToken as a TTokenInfo
  @param   TLT        as a TTimeLocationTable
  @return  a Boolean

**)
function TEidolonModule.TLBar(strName: String; StartToken : TTokenInfo;
  TLT : TTimeLocationTable): Boolean;

Var
  B : TBar;

begin
  Result := False;
  If CompareText(Token.Token, 'BAR') = 0 Then
    Begin
      Result := True;
      NextNonCommentToken;
      B := TLT.AddSymbol(TBar.Create(strName, scNone, StartToken.Line,
        StartToken.Column, iiPublicObject, Nil)) As TBar;
      B.SymbolType := stBar;
      EatWhitespace;
      If CheckLiteral(',', 'TLBar') Then
        Begin
          BorderDef(B);
          EatWhitespace;
          If CheckLiteral(',', 'TLBar') Then
            Begin
              InteriorDef(B);
              EatWhiteSpace;
              If CheckLiteral(',', 'TLBar') Then
                Begin
                  BarWidth(B);
                  Transparency(B);
                  CheckLineEnd('TLBar');
                End;
            End;
        End;
    End;
end;

{ TSymbol }

(**

  This method returns string representation of the basic Time Location Symbol.

  @precon  None.
  @postcon Returns string representation of the basic Time Location Symbol.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TSymbol.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;

begin
  Result := Identifier + '=' +
    strSymbolTypes[FSymbolType] + ', ' +
    strColours[FBorderColour] + ', ' +
    strLineStyles[FBorderLineStyle] + ', ' +
    strLineWeights[FBorderWeight];
end;

{ TCustomFillSymbol }

(**

  This method returns a string representation of the class.

  @precon  None.
  @postcon Returns a string representation of the class.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TCustomFillSymbol.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;

begin
  Result := Inherited AsString(boolShowIdentifier, boolForDocumentation);
  Result := Result + ', ' +
    strColours[FInteriorColour] + ', ' +
    strInteriorPatterns[FInteriorPattern] + ', ' +
    strColours[FInteriorPatternColour];
end;

(**

  A constructor for the Trectangle class.

  @precon  None.
  @postcon Initialises the transparency to -1.0, i.e. not set.

  @param   strName     as a String
  @param   AScope      as a TScope
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   AImageIndex as a TImageIndex
  @param   AComment    as a TComment

**)
constructor TCustomFillSymbol.Create(strName: String; AScope: TScope; iLine,
  iColumn: Integer; AImageIndex: TImageIndex; AComment: TComment);
begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FTransparency := -1;
end;

{ TRectangle }

(**

  This method returns a string representation of the class.

  @precon  None.
  @postcon Returns a string representation of the class

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TRectangle.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;
begin
  Result := Inherited AsString(boolShowIdentifier, boolForDocumentation);
  If Transparency >= 0.0 Then
    Result := Result + ', ' + Format('%d', [Transparency]);
end;

{ TBar }

(**

  This method returns a string representation of the class.

  @precon  None.
  @postcon Returns a string representation of the class

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TBar.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;

begin
  Result := Inherited AsString(boolShowIdentifier, boolForDocumentation);
  Result := Result + ', ' + Format('%d', [FBarWidth]);
  If Transparency >= 0.0 Then
    Result := Result + ', ' + Format('%d', [Transparency]);
end;

{ TDiamond }

(**

  This method returns a string representation of the class.

  @precon  None.
  @postcon Returns a string representation of the class

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TDiamond.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;
begin
  Result := Inherited AsString(boolShowIdentifier, boolForDocumentation);
  Result := Result + ', ' + Format('%d', [FDiamondSize]);
  If Transparency >= 0.0 Then
    Result := Result + ', ' + Format('%d', [Transparency]);
end;

{ TTriangle }

(**

  This method returns a string representation of the class.

  @precon  None.
  @postcon Returns a string representation of the class

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TTriangle.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;
begin
  Result := Inherited AsString(boolShowIdentifier, boolForDocumentation);
  Result := Result + ', ' + strTriangleTypes[FTriangleType];
  If Transparency >= 0.0 Then
    Result := Result + ', ' + Format('%d', [Transparency]);
end;

{ TEllipse }

(**

  This method returns a string representation of the class.

  @precon  None.
  @postcon Returns a string representation of the class

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TEllipse.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;
begin
  Result := Inherited AsString(boolShowIdentifier, boolForDocumentation);
  Result := Result + ', ' + Format('%d', [FEllipseSize]);
  If Transparency >= 0.0 Then
    Result := Result + ', ' + Format('%d', [Transparency]);
end;

{ TOutputTable }

(**

  This method returns a string representation of the class.

  @precon  None.
  @postcon Returns a string representation of the class

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TOutputTable.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;
begin
  Result := Identifier + '=Class(OutputTable)';
end;

{ TAssociation }

(**

  This method returns a string representation of the class.

  @precon  None.
  @postcon Returns a string representation of the class

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TAssociation.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;
var
  iToken: Integer;
begin
  Result := Identifier;
  If TokenCount > 0 Then
    Begin
      Result := Result + '=';
      For iToken := 0 To TokenCount - 1 Do
        Result := Result + Tokens[iToken].Token;
    End;
end;

{ TRequirementsTable }

(**

  This method adds an association to the requirements table.

  @precon  Association must be a valid instance of a TAssocaition.
  @postcon Adds an association to the requirements table.

  @param   Association as a TAssociation
  @return  a TAssociation

**)
function TRequirementsTable.AddAssociation(
  Association: TAssociation): TAssociation;
begin
  If FAssociations = Nil Then
    FAssociations := Add(TLabelContainer.Create('Associations', scNone, 0, 0,
      iiClassesLabel, Nil)) As TLabelContainer;
  Result := FAssociations.Add(Association) As TAssociation;
end;

(**

  This method returns a string representation of the class.

  @precon  None.
  @postcon Returns a string representation of the class

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TRequirementsTable.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;
begin
  Result := Identifier + '=Class(RequirementsTable)';
end;

(**

  This method parses the AssociationDef element of the grammar.

  @precon  RequirementsTable must be a valid instance of a TRequirementsTable
           class.
  @postcon parses the AssociationDef element of the grammar.

  @param   RequirementsTable as a TRequirementsTable
  @return  a Boolean

**)
function TEidolonModule.AssociationDef(
  RequirementsTable: TRequirementsTable): Boolean;
var
  strName: String;
  A: TAssociation;
  T : TTokenInfo;

begin
  EatWhitespace;
  Result := False;
  If Token.Token = '@' Then
    Begin
      Result := True;
      NextNonCommentToken;
      T := Token;
      strName := '';
      While Token.Token <> '=' Do
        Begin
          strName := strName + Token.Token;
          NextNonCommentToken;
        End;
      If strName <> '' Then
        Begin
          A := RequirementsTable.AddAssociation(TAssociation.Create(strName,
            scNone, T.Line, T.Column, iiPublicClass, Nil)) As TAssociation;
          If CheckLiteral('=', 'AssociationDef') Then
            Begin
              strName := '';
              While Not (Token.TokenType In [ttLineEnd]) Do
                AddToExpression(A);
              CheckLineEnd('AssociationDef');
            End;
        End Else
          ErrorAndSeekToken(strNullName, 'AssociationDef', Token.Token,
            strSeekableOnErrorTokens, stActual);

    End;
end;

(**

  This method returns a string representation of the module.

  @precon  None.
  @postcon Returns a string representation of the module.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TEidolonModule.AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String;

Begin
  Result := ChangeFileExt(Inherited AsString(boolShowIdentifier,
    boolForDocumentation), '');
End;

(**

  This method parses the BarWidth element of the grammar.

  @precon  Bar must be a valid instance of a TBar class.
  @postcon Sets the width of the bar.

  @param   Bar as a TBar

**)
procedure TEidolonModule.BarWidth(Bar: TBar);

Var
  iWidth, iErrorCode : Integer;

begin
  EatWhitespace;
  If Token.TokenType In [ttNumber] Then
    Begin
      Val(Token.Token, iWidth, iErrorCode);
      If (iErrorCode > 0) Or (Not (iWidth In [1..100]))  Then
        AddIssue(Format(strInvalidNumber, [Token.Token, Token.Line,
          Token.Column]), scNone, 'BarWidth', Token.Line, Token.Column,
          etError);
      Bar.BarWidth := iWidth;
      NextNonCommentToken;
    End Else
      ErrorAndSeekToken(strNumberExpected, 'BarWidth', Token.Token,
        strSeekableOnErrorTokens, stActual);
end;

(**

  This method parses the BorderColour element of the grammar.

  @precon  Symbol must be a valid instance of a TSymbol class.
  @postcon Assigns the border colour to the symbol.

  @param   Symbol as a TSymbol

**)
procedure TEidolonModule.BorderColour(Symbol: TSymbol);
begin
  Symbol.BorderColour := ColourName;
end;

(**

  This method parses the BorderDef element of the grammar.

  @precon  Symbol must be a valid instance of a TSymbol class.
  @postcon Parses the BorderDef element of the grammar.

  @param   Symbol as a TSymbol

**)
procedure TEidolonModule.BorderDef(Symbol: TSymbol);
begin
  EatWhitespace;
  BorderColour(Symbol);
  EatWhitespace;
  If CheckLiteral(',', 'BorderDef') Then
    Begin
      EatWhitespace;
      BorderLineStyle(Symbol);
      EatWhitespace;
      If CheckLiteral(',', 'BorderDef') Then
        Begin
          EatWhitespace;
          BorderWeight(Symbol);
        End;
    End;
end;

(**

  This method parses the BorderLineStyle element of the grammar.

  @precon  Symbol must be a valid instance of TSymbol.
  @postcon Assigns the line style to the symbol.

  @param   Symbol as a TSymbol

**)
procedure TEidolonModule.BorderLineStyle(Symbol: TSymbol);

var
  boolFound: Boolean;
  iLineStyle: TLineStyle;

begin
  Symbol.BorderLineStyle := lsSOLID;
  boolFound := False;
  For iLineStyle := Low(TLineStyle) To High(TLineStyle) Do
    If CompareText(Token.Token, strLineStyles[iLineStyle]) = 0 Then
      Begin
        Symbol.BorderLineStyle := iLineStyle;
        boolFound := True;
        Break;
      End;
  If boolFound Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken(strInvalidLineStyle, 'BorderLineStyle', Token.Token,
      strSeekableOnErrorTokens, stActual);
end;

(**

  This method parses the BorderWeight element of the grammar.

  @precon  Symbol must be a valid instance of a TSymbol class.
  @postcon Assigns the border weight to the symbol.

  @param   Symbol as a TSymbol

**)
procedure TEidolonModule.BorderWeight(Symbol: TSymbol);

var
  boolFound: Boolean;
  iLineWeight: TLineWeight;

begin
  Symbol.BorderWeight := lw0_25;
  boolFound := False;
  For iLineWeight := Low(TLineWeight) To High(TLineWeight) Do
    If CompareText(Token.Token, strLineWeights[iLineWeight]) = 0 Then
      Begin
        Symbol.BorderWeight := iLineWeight;
        boolFound := True;
        Break;
      End;
  If boolFound Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken(strInvalidLineWeight, 'BorderWeight', Token.Token,
      strSeekableOnErrorTokens, stActual);
end;

(**

  This method checks for the presents of line end characters in the token stream
  and returns true if found and moves to the next non comment token after the
  line end characters.

  @precon  None.
  @postcon Checks for the presents of line end characters in the token stream
           and returns true if found and moves to the next non comment token
           after the line end characters.

  @param   strMethod  as a String
  @return  a Boolean

**)
function TEidolonModule.CheckLineEnd(strMethod: String): Boolean;
begin
  Result := False;
  If Token.TokenType In [ttLineEnd] Then
    Begin
      Result := True;
      EatLineEnds;
    End Else
      ErrorAndSeekToken(strExpectedLineEnd, strMethod, Token.Token,
        strSeekableOnErrorTokens, stActual);
end;

(**

  This method checks for the presents of a literal in the token stream and
  returns true if found and moves to the next non comment token.

  @precon  None.
  @postcon Checks for the presents of a literal in the token stream and
           returns true if found and moves to the next non comment token.

  @param   strLiteral as a String
  @param   strMethod  as a String
  @return  a Boolean

**)
function TEidolonModule.CheckLiteral(strLiteral, strMethod: String): Boolean;
begin
  Result := False;
  If Token.Token = strLiteral Then
    Begin
      Result := True;
      NextNonCommentToken;
    End Else
      ErrorAndSeekToken(strLiteralExpected, strMethod, strLiteral,
        strSeekableOnErrorTokens, stActual);
end;

(**

  This method parses the ColourName element of the grammar.

  @precon  None.
  @postcon Returns the enumerate associated with colour at the current token
           else returns xlcNone and registers an error.

  @return  a TColour

**)
function TEidolonModule.ColourName: TColour;

var
  iColour: TColour;
  boolFound: Boolean;

begin
  Result := xlcNone;
  boolFound := False;
  For iColour := Low(TColour) To High(TColour) Do
    If CompareText(Token.Token, strColours[iColour]) = 0 Then
      Begin
        Result := iColour;
        boolFound := True;
        Break;
      End;
  If boolFound Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken(strInvalidColourName, 'ColourName', Token.Token,
      strSeekableOnErrorTokens, stActual);
end;

(**

  This method parses the ConnectionDef element of the grammar.

  @precon  Table must be a valid instance of a TBaseTable class.
  @postcon Parses the ConnectionDef element of the grammar.

  @param   DBTable        as a TDBTable
  @param   ConnectionType as a TConnectionType

**)
procedure TEidolonModule.ConnectionDef(DBTable : TDBTable; ConnectionType : TConnectionType);

Const
  strValidConnections : Array[1..5] Of String = ('', 'dbase iv;', 'foxpro 2.6;',
    'paradox 4.x;', 'text;');

Var
  C : TConnectionDef;
  strConnection: String;
  iToken: Integer;

begin
  If Token.Token = strConnectionType[ConnectionType] Then
    Begin
      NextNonCommentToken;
      If CompareText(Token.Token, 'CONNECTION') = 0 Then
        Begin
          Case ConnectionType Of
            ctPrimary:
              C := DBTable.AddPrimary(TConnectionDef.Create('Connection', scNone,
                Token.Line, Token.Column, iiPublicType, Nil)) As TConnectionDef;
          Else
            C := DBTable.AddSecondary(TConnectionDef.Create('Connection', scNone,
              Token.Line, Token.Column, iiPublicType, Nil)) As TConnectionDef;
          End;
          NextNonCommentToken;
          If CheckLiteral('=', 'ConnectionDef') Then
            Begin
              While Not (Token.TokenType In [ttLineEnd]) Do
                AddToExpression(C);
              CheckLineEnd('ConnectionDef');
              strConnection := '';
              For iToken := 0 To C.TokenCount - 1 Do
                strConnection := strConnection + C.Tokens[iToken].Token;
              If Not IsKeyWord(strConnection, strValidConnections) Then
                AddIssue(Format(strIsNotAValidConnection, [strConnection, C.Line,
                  C.Column]), scNone, 'ConnectionDef', C.Line, C.Column, etWarning);
            End;
        End Else
          RollBackToken;
    End;
end;

(**

  This is the constructor method for the TEidolonModule class.

  @precon  Source is a valid TStream descendant containing as stream of text,
           that is the contents of a source code module and Filename is the
           file name of the module being parsed and IsModified determines if
           the source code module has been modified since the last save to
           disk.
  @postcon Creates an instance of the module parser.

  @param   Source        as a String
  @param   strFileName   as a String
  @param   IsModified    as a Boolean
  @param   ModuleOptions as a TModuleOptions

**)
Constructor TEidolonModule.CreateParser(Source : String; strFileName : String;
  IsModified : Boolean; ModuleOptions : TModuleOptions);

Begin
  Inherited CreateParser(Source, strFileName, IsModified, ModuleOptions);
  FSource := Source;
  FTextTableDefs := Add(TLabelContainer.Create(strTextTableDefinitions, scNone,
    0, 0, iiPublicThreadVarsLabel, Nil)) As TLabelContainer;
  FDBTableDefs := Add(TLabelContainer.Create('Database Table Definitions', scNone,
    0, 0, iiPublicConstantsLabel, Nil)) As TLabelContainer;
  FTimeLocationTableDefs := Add(TLabelContainer.Create('Time Location Table Definitions',
    scNone, 0, 0, iiPublicVariablesLabel, Nil)) As TLabelContainer;
  FOutputTableDefs := Add(TLabelContainer.Create('Output Table Definitions', scNone,
    0, 0, iiInterfacesLabel, Nil)) As TLabelContainer;
  FRequirementsTableDefs := Add(TLabelContainer.Create('Requirements Table Definitions', scNone,
    0, 0, iiDispInterfacesLabel, Nil)) As TLabelContainer;
  AddTickCount('Start');
  CommentClass := TEidolonComment;
  TokenizeStream;
  AddTickCount('Tokenize');
  If moParse In ModuleOptions Then
    Begin
      ParseTokens;
      AddTickCount('Parse');
      Add(strErrors, iiErrorFolder, scNone, Nil);
      Add(strWarnings, iiWarningFolder, scNone, Nil);
      Add(strHints, iiHintFolder, scNone, Nil);
      Add(strDocumentationConflicts, iiDocConflictFolder, scNone, Nil);
      If FindElement(strErrors).ElementCount = 0 Then
        CheckReferences;
      AddTickCount('Refs');
      TidyUpEmptyElements;
    End;
End;

(**

  This method parses the DatabaseDef element of the grammar.

  @precon  DBTable must be a valid instance of a TDBTable class.
  @postcon Returns true of their was a database definition parsed.

  @param   DBTable        as a TDBTable
  @param   ConnectionType as a TConnectionType
  @return  a Boolean

**)
function TEidolonModule.DatabaseDef(DBTable : TDBTable; ConnectionType : TConnectionType): Boolean;

var
  D: TDatabaseDef;
  strFileName: String;
  iToken: Integer;

begin
  Result := False;
  If Token.Token = strConnectionType[ConnectionType] Then
    Begin
      NextNonCommentToken;
      If CompareText(Token.Token, 'Database') =  0 Then
        Begin
          Result := True;
          Case ConnectionType Of
            ctPrimary:
              D := DBTable.AddPrimary(TDatabaseDef.Create('Database', scNone, Token.Line,
                Token.Column, iiPublicType, Nil)) As TDatabaseDef;
          Else
            D := DBTable.AddSecondary(TDatabaseDef.Create('Database', scNone, Token.Line,
              Token.Column, iiPublicType, Nil)) As TDatabaseDef;
          End;
          NextNonCommentToken;
          If CheckLiteral('=', 'DatabaseDef') Then
            Begin
              While Not (Token.TokenType In [ttLineEnd]) Do
                AddToExpression(D);
              CheckLineEnd('DatabaseDef');
              strFileName := '';
              For iToken := 0 To D.TokenCount - 1 Do
                strFileName := strFileName + D.Tokens[iToken].Token;
              If Not FileExists(strFileName) And Not DirectoryExists(strFileName) Then
                AddIssue(Format(strThisFileDirDoesNotExist, [strFileName, D.Line,
                  D.Column]), scNone, 'DatabaseDef', D.Line, D.Column, etWarning);
            End;
        End Else
          RollBackToken;
    End;
end;

(**

  This method parses the DBTable element of the grammar.

  @precon  StartToken must be a valid instance of a TTokenInfo class.
  @postcon Parses the DBTable element of the grammar.

  @param   strName    as a String
  @param   StartToken as a TTokenInfo
  @return  a Boolean

**)
Function TEidolonModule.DBTable(strName : String; StartToken : TTokenInfo) : Boolean;
var
  DBT: TDBTable;

begin
  Result := False;
  If Token.UToken = 'DBTABLE' Then
    Begin
      Result := True;
      NextNonCommentToken;
      If strName <> '' Then
        Begin
          DBT := FDBTableDefs.Add(TDBTable.Create(strName, scNone, StartToken.Line,
            StartToken.Column, iiPublicConstant, Nil)) As TDBTable;
          If CheckLiteral(')', 'DBTable') Then
            If CheckLineEnd('DBTable') Then
              Begin
                EatWhitespace;
                If CheckLiteral('{', 'DBTable') Then
                  Begin
                    EatWhitespace;
                    If CheckLineEnd('DBTable') Then
                      Begin
                        EatWhitespace;
                        If DatabaseDef(DBT, ctPrimary) Then
                          Begin
                            EatWhitespace;
                            ConnectionDef(DBT, ctPrimary);
                            EatWhitespace;
                            TableNameDef(DBT, ctPrimary);
                          End;
                        While FieldDef(DBT) Do;
                        If CheckLiteral('}', 'DBTable') Then
                          Begin
                            EatWhitespace;
                            CheckLineEnd('DBTable');
                          End;
                      End;
                  End;
              End;
        End Else
          ErrorAndSeekToken(strTheDefintionNull, 'DBTable', Token.Token,
            strSeekableOnErrorTokens, stActual);
    End;
end;

(**


  This is a destructor for the TEidolonModule class.

  @precon  None.
  @postcon Fress the memory fo this instance.


**)
Destructor TEidolonModule.Destroy;
begin
  Inherited Destroy;
end;

(**

  This method parses the DiamondSize element of the grammar.

  @precon  Diamond must be a valid instance of a TDiamond class.
  @postcon Parses the DiamondSize element of the grammar.

  @param   Diamond as a TDiamond

**)
procedure TEidolonModule.DiamondSize(Diamond : TDiamond);

Var
  iSize, iErrorCode : Integer;

begin
  EatWhitespace;
  If Token.TokenType In [ttNumber] Then
    Begin
      Val(Token.Token, iSize, iErrorCode);
      If (iErrorCode > 0) Or (Not (iSize In [1..100]))  Then
        AddIssue(Format(strInvalidNumber, [Token.Token, Token.Line,
          Token.Column]), scNone, 'DiamondSize', Token.Line, Token.Column,
          etError);
      Diamond.DiamondSize:= iSize;
      NextNonCommentToken;
    End Else
      ErrorAndSeekToken(strNumberExpected, 'DiamondSize', Token.Token,
        strSeekableOnErrorTokens, stActual);
end;

(**

  This meothd parses the Diamond element of the grammar.

  @precon  StartToken must ba a valid instance of a TTokenInfo class and
           TLT must be a valid instance of a TTimeLocationTable class.
  @postcon Returns true if a DIAMOND was parsed.

  @param   strName    as a String
  @param   StartToken as a TTokenInfo
  @param   TLT        as a TTimeLocationTable
  @return  a Boolean

**)
function TEidolonModule.TLDiamond(strName: String; StartToken : TTokenInfo;
  TLT : TTimeLocationTable): Boolean;

Var
  D : TDiamond;

begin
  Result := False;
  If CompareText(Token.Token, 'DIAMOND') = 0 Then
    Begin
      Result := True;
      NextNonCommentToken;
      D := TLT.AddSymbol(TDiamond.Create(strName, scNone, StartToken.Line,
        StartToken.Column, iiPublicObject, Nil)) As TDiamond;
      D.SymbolType := stDiamond;
      EatWhitespace;
      If CheckLiteral(',', 'TLDiamond') Then
        Begin
          BorderDef(D);
          EatWhitespace;
          If CheckLiteral(',', 'TLDiamond') Then
            Begin
              InteriorDef(D);
              EatWhiteSpace;
              If CheckLiteral(',', 'TLDiamond') Then
                Begin
                  DiamondSize(D);
                  Transparency(D);
                  CheckLineEnd('TLDiamond');
                End;
            End;
        End;
    End;
end;

(**

  This method eats the line ends when found and puts the token at the first
  token on the next non-null line.

  @precon  None.
  @postcon Eats the line ends when found and puts the token at the first
           token on the next non-null line.

**)
procedure TEidolonModule.EatLineEnds;
begin
  While Token.TokenType In [ttLineEnd] Do
    NextNonCommentToken;
end;

(**

  This method eats the whitespace when found and puts the token at the next non
  whitespace token.

  @precon  None.
  @postcon Eats the whitespace when found and puts the token at the next non
           whitespace token.

**)
procedure TEidolonModule.EatWhitespace;
begin
  While Token.TokenType In [ttWhiteSpace] Do
    NextNonCommentToken;
end;

(**

  This method parses the EllipseSize element of the grammar.

  @precon  Ellipse must be a valid instance of a TEllipse class.
  @postcon Sets the EllipseSize property of the ellipse.

  @param   Ellipse as a TEllipse

**)
procedure TEidolonModule.EllipseSize(Ellipse: TEllipse);

Var
  iSize, iErrorCode : Integer;

begin
  EatWhitespace;
  If Token.TokenType In [ttNumber] Then
    Begin
      Val(Token.Token, iSize, iErrorCode);
      If (iErrorCode > 0) Or (Not (iSize In [1..100]))  Then
        AddIssue(Format(strInvalidNumber, [Token.Token, Token.Line,
          Token.Column]), scNone, 'EllipseSize', Token.Line, Token.Column,
          etError);
      Ellipse.EllipseSize:= iSize;
      NextNonCommentToken;
    End Else
      ErrorAndSeekToken(strNumberExpected, 'EllipseSize', Token.Token,
        strSeekableOnErrorTokens, stActual);
end;

(**

  This method eats both whitespace and line ends which make up empty lines. If
  whitespace or line ends where eaten the returns true.

  @precon  None.
  @postcon Eats both whitespace and line ends which make up empty lines. If
           whitespace or line ends where eaten the returns true.

  @return  a Boolean

**)
function TEidolonModule.EmptyLine: Boolean;
begin
  Result := False;
  While Token.TokenType In [ttLineEnd, ttWhiteSpace] Do
    Begin
      Result := True;
      NextNonCommentToken;
    End;
end;

(**

  This method parses the Ellipse element of the grammar.

  @precon  StartToken must ba a valid instance of a TTokenInfo class and
           TLT must be a valid instance of a TTimeLocationTable class.
  @postcon Returns true if a ELLIPSE was parsed.

  @param   strName    as a String
  @param   StartToken as a TTokenInfo
  @param   TLT        as a TTimeLocationTable
  @return  a Boolean

**)
function TEidolonModule.TLEllipse(strName: String; StartToken : TTokenInfo;
  TLT : TTimeLocationTable): Boolean;

Var
  E : TEllipse;

begin
  Result := False;
  If CompareText(Token.Token, 'ELLIPSE') = 0 Then
    Begin
      Result := True;
      NextNonCommentToken;
      E := TLT.AddSymbol(TEllipse.Create(strName, scNone, StartToken.Line,
        StartToken.Column, iiPublicObject, Nil)) As TEllipse;
      E.SymbolType := stEllipse;
      EatWhitespace;
      If CheckLiteral(',', 'TLEllipse') Then
        Begin
          BorderDef(E);
          EatWhitespace;
          If CheckLiteral(',', 'TLEllipse') Then
            Begin
              InteriorDef(E);
              EatWhiteSpace;
              If CheckLiteral(',', 'TLEllipse') Then
                Begin
                  EllipseSize(E);
                  Transparency(E);
                  CheckLineEnd('TLEllipse');
                End;
            End;
        End;
    End;
end;

(**

  This method parses the FieldDef element of the grammar.

  @precon  Container must be a valid instance of a TElementContainer class.
  @postcon Returns true if a field def was parsed.

  @param   Table as a TBaseTable
  @return  a Boolean

**)
function TEidolonModule.FieldDef(Table : TBaseTable): Boolean;

Var
  strName : String;
  F : TFieldDef;
  T : TTokenInfo;
  boolPrimaryKey: Boolean;

begin
  EatWhitespace;
  Result := False;
  boolPrimaryKey := False;
  If (Token.Token = '*') Or (Token.TokenType In [ttIdentifier]) Then
    Begin
      Result := True;
      If Token.Token = '*' Then
        Begin
          boolPrimaryKey := True;
          NextNonCommentToken;
        End;
      T := Nil;
      While Token.Token <> ':' Do
        Begin
          If T = Nil Then
            T := Token;
          strName := strName + Token.Token;
          NextNonCommentToken;
        End;
      If strName <> '' Then
        Begin
          F := Table.AddField(TFieldDef.Create(strName, scNone, T.Line, T.Column,
            iiPublicField, Nil));
          F.PrimaryKey := boolPrimaryKey;
          If CheckLiteral(':', 'FieldDef') Then
            Begin
              TypeInfo(F);
              If Token.Token = '=' Then
                Begin
                  NextNonCommentToken;
                  strName := '';
                  While Not (Token.TokenType In [ttLineEnd]) Do
                    Begin
                      strName := strName + Token.Token;
                      NextNonCommentToken;
                    End;
                  F.OutputName := strName;
                End;
              CheckLineEnd('FieldDef');
            End;
        End Else
          ErrorAndSeekToken(strNullName, 'FieldDef', Token.Token,
            strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method tokenises the stream of text passed to the constructor and splits
  it into tokens.

  @precon  None.
  @postcon Tokenises the stream of text passed to the constructor and splits
           it into tokens.

**)
Procedure TEidolonModule.TokenizeStream;

Type
  (** State machine for block types. **)
  TBlockType = (btNoBlock, btSingleLiteral, btDoubleLiteral, btFullComment,
    btLineComment);

Const
  (** Growth size of the token buffer. **)
  iTokenCapacity = 100;
  strSingleSymbols = [#9, #10, #13, #32, ';', '(', ')', '*', '+', ',', '='];
  (** A set of characters for single quotes **)
  strSingleQuotes = [''''];
  (** A set of characters for double quotes **)
  strDoubleQuotes = ['"'];
  (** A set of identifier characters. **)
  strIdentifiers = ['a'..'z', 'A'..'Z', '_', '-', '%', #192..#214,
    #216..#246, #248..#255];
  (** A set of number characters. **)
  strNumbers  = ['.', '0'..'9'];
  strAllChars = [#32..#255];
  (** A set of characters for general symbols **)
  strSymbols = (strAllChars - strIdentifiers - strNumbers - strSingleQuotes -
    strDoubleQuotes);

Var
  (** Token buffer. **)
  strToken : String;
  CurCharType : TBADITokenType;
  LastCharType : TBADITokenType;
  BlockType : TBlockType;
  (** Current line number **)
  iLine : Integer;
  (** Current column number **)
  iColumn : Integer;
  (** Token stream position. Fast to inc this than read the stream position. **)
  iStreamPos : Integer;
  (** Token line **)
  iTokenLine : Integer;
  (** Token column **)
  iTokenColumn : Integer;
  (** Current character position **)
  iStreamCount : Integer;
  Ch : Char;
  LastChar : Char;
  (** Token size **)
  iTokenLen : Integer;
  iChar: Integer;

  (**

    This INLINE procedure changes the whitepace tokens for more human readable
    tokens.

    @precon  strToken must be a non-null string.
    @postcon Changes the whitepace tokens for more human readable
             tokens.

    @param   strToken as a String as a reference

  **)
  Procedure ProcessWhiteSpace(var strToken : String); {$IFDEF D2005} InLine; {$ENDIF}

  Begin
    If strToken = #13 Then
      strToken := '<LF>';
    If strToken = #10 Then
      strToken := '<CR>';
  End;

Begin
  BlockType := btNoBlock;
  iStreamPos := 0;
  iTokenLine := 1;
  iTokenColumn := 1;
  CurCharType := ttUnknown;
  LastCharType := ttUnknown;
  iStreamCount := 0;
  iLine := 1;
  iColumn := 1;
  LastChar := #0;
  strToken := '';

  iTokenLen := 0;
  SetLength(strToken, iTokenCapacity);

  Try
    For iChar := 1 To Length(FSource) Do
      Begin
        Ch := FSource[iChar];
        Inc(iStreamCount);
        LastCharType := CurCharType;

        If IsInSet(ch, strWhiteSpace) Then
          CurCharType := ttWhiteSpace
        Else If isInSet(Ch, strLineEnd) Then
          CurCharType := ttLineEnd
        Else If IsInSet(ch, strSingleQuotes) Then
          CurCharType := ttSingleLiteral
        Else If IsInSet(ch, strDoubleQuotes) Then
          CurCharType := ttDoubleLiteral
        Else If IsInSet(ch, strSymbols) Then
          CurCharType := ttSymbol
        Else If IsInSet(ch, strIdentifiers) Then
          Begin
            If (LastCharType = ttNumber) And (IsInSet(Ch, ['A'..'F', 'a'..'f'])) Then
              CurCharType := ttNumber
            Else
              CurCharType := ttIdentifier;
          End
        Else If IsInSet(ch, strNumbers) Then
          Begin
            CurCharType := ttNumber;
            If LastCharType = ttIdentifier Then
              CurCharType := ttIdentifier;
          End
        Else
          CurCharType := ttUnknown;

        // Check for full block comments
        If (BlockType = btNoBlock) And (LastChar = '/') And (Ch = '*') Then
          BlockType := btFullComment;

        // Check for line comments
        If (BlockType = btNoBlock) And (LastChar = '/') And (Ch = '/') Then
          BlockType := btLineComment;

        If (LastCharType <> CurCharType) Or (IsInSet(Ch, strSingleSymbols)) Or
          (IsInSet(LastChar, strSingleSymbols)) Then
          Begin
            If ((BlockType In [btLineComment]) And (CurCharType <> ttLineEnd)) Or
              (BlockType In [btFullComment]) Then
              Begin
                Inc(iTokenLen);
                If iTokenLen > Length(strToken) Then
                  SetLength(strToken, iTokenCapacity + Length(strToken));
                strToken[iTokenLen] := Ch;
              End Else
              Begin
                SetLength(strToken, iTokenLen);
                If iTokenLen > 0 Then
                  Begin
                    If BlockType = btLineComment Then
                      LastCharType := ttLineComment;
                    ProcessWhiteSpace(strToken);
                    If LastCharType = ttIdentifier Then
                      If IsKeyWord(strToken, strReservedWords) Then
                        LastCharType := ttReservedWord;
                    AddToken(TTokenInfo.Create(strToken, iStreamPos,
                      iTokenLine, iTokenColumn, Length(strToken), LastCharType));
                  End;
               // Store Stream position, line number and column of
               // token start
               iStreamPos := iStreamCount;
               iTokenLine := iLine;
               iTokenColumn := iColumn;
               BlockType := btNoBlock;
               iTokenLen := 1;
               SetLength(strToken, iTokenCapacity);
               strToken[iTokenLen] := Ch;
              End;
          End Else
          Begin
            Inc(iTokenLen);
            If iTokenLen > Length(strToken) Then
              SetLength(strToken, iTokenCapacity + Length(strToken));
            strToken[iTokenLen] := Ch;
          End;

        // Check for the end of a block comment
        If (BlockType = btFullComment) And (LastChar = '*') And (Ch = '/') Then
          Begin
            BlockType := btNoBlock;
            CurCharType := ttBlockComment;
          End;

        // Check for single string literals
        If CurCharType = ttSingleLiteral Then
          If BlockType = btSingleLiteral Then
            BlockType := btNoBlock
          Else If BlockType = btNoBlock Then
            BlockType := btSingleLiteral;
        // Check for Double string literals
        If CurCharType = ttDoubleLiteral Then
          If BlockType = btDoubleLiteral Then
            BlockType := btNoBlock
          Else If BlockType = btNoBlock Then
            BlockType := btDoubleLiteral;

        Inc(iColumn);
        If Ch = #10 Then
          Begin
            Inc(iLine);
            iColumn := 1;
          End;
        LastChar := Ch;
      End;
      If iTokenLen > 0 Then
        Begin
          SetLength(strToken, iTokenLen);
          ProcessWhiteSpace(strToken);
          AddToken(TTokenInfo.Create(strToken, iStreamPos,
            iTokenLine, iTokenColumn, Length(strToken), LastCharType));
        End;
    AddToken(TTokenInfo.Create('<end-of-file>', iStreamPos, iTokenLine, iTokenColumn, 0,
      ttFileEnd));
  Except
    On E : Exception Do
      AddIssue(E.Message, scGlobal, 'TokenizeStream', 0, 0, etError);
  End
End;

(**

  This method parses the Transparency element of the grammar.

  @precon  CustomSymbol must be a valid instance of a TCustomFillSymbol.
  @postcon Sets the Transparency of the symbol.

  @param   CustomSymbol as a TCustomFillSymbol

**)
procedure TEidolonModule.Transparency(CustomSymbol : TCustomFillSymbol);

Var
  iTransparency : Integer;
  iErrorCode : Integer;

begin
  EatWhitespace;
  If Token.Token = ',' Then
    Begin
      NextNonCommentToken;
      EatWhitespace;
      If Token.TokenType In [ttNumber] Then
        Begin
          Val(Token.Token, iTransparency, iErrorCode);
          If (iErrorCode > 0) Or (Not (iTransparency In [0..100]))  Then
            AddIssue(Format(strInvalidNumber, [Token.Token, Token.Line,
              Token.Column]), scNone, 'Transparency', Token.Line, Token.Column,
              etError);
          CustomSymbol.Transparency := iTransparency;
          NextNonCommentToken;
          EatWhitespace;
        End Else
          ErrorAndSeekToken(strNumberExpected, 'Transparency', Token.Token,
            strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses the TriangleType element of the grammar.

  @precon  Triangle must be a valid instance of a TTriangle class.
  @postcon Parses the TriangleType element of the grammar.

  @param   Triangle as a TTriangle

**)
procedure TEidolonModule.TriangleType(Triangle: TTriangle);

var
  iTriangle: TTriangleType;
  boolFound: Boolean;

begin
  EatWhitespace;
  Triangle.TriangleType := ttStartAndEarly;
  boolFound := False;
  For iTriangle := Low(TTriangleType) To High(TTriangleType) Do
    If CompareText(Token.Token, strTriangleTypes[iTriangle]) = 0 Then
      Begin
        Triangle.TriangleType := iTriangle;
        boolFound := True;
        Break;
      End;
  If boolFound Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken(strInvalidPattern, 'TriangleType', Token.Token,
      strSeekableOnErrorTokens, stActual);
end;

(**

  This method parses the Triangle element of the grammar.

  @precon  StartToken must ba a valid instance of a TTokenInfo class and
           TLT must be a valid instance of a TTimeLocationTable class.
  @postcon Returns true if a TRIANGLE was parsed.

  @param   strName    as a String
  @param   StartToken as a TTokenInfo
  @param   TLT        as a TTimeLocationTable
  @return  a Boolean

**)
function TEidolonModule.TLTriangle(strName: String; StartToken : TTokenInfo;
  TLT : TTimeLocationTable): Boolean;

Var
  T : TTriangle;

begin
  Result := False;
  If CompareText(Token.Token, 'TRIANGLE') = 0 Then
    Begin
      Result := True;
      NextNonCommentToken;
      T := TLT.AddSymbol(TTriangle.Create(strName, scNone, StartToken.Line,
        StartToken.Column, iiPublicObject, Nil)) As TTriangle;
      T.SymbolType := stTriangle;
      EatWhitespace;
      If CheckLiteral(',', 'TLTriangle') Then
        Begin
          BorderDef(T);
          EatWhitespace;
          If CheckLiteral(',', 'TLTriangle') Then
            Begin
              InteriorDef(T);
              EatWhiteSpace;
              If CheckLiteral(',', 'TLTriangle') Then
                Begin
                  TriangleType(T);
                  Transparency(T);
                  CheckLineEnd('TLTriangle');
                End;
            End;
        End;
    End;
end;

(**

  This method parses the TypeInfo element of the grammar.

  @precon  Field must be a valid instance of a TFieldDef class.
  @postcon Parses the TypeInfo element of the grammar.

  @param   Field as a TFieldDef

**)
procedure TEidolonModule.TypeInfo(Field: TFieldDef);
var
  iWidth: Integer;
  iErrorCode: Integer;
begin
  If Token.Token = 'C' Then
    Begin
      Field.FieldType := ftText;
      NextNonCommentToken;
      If CheckLiteral('(', 'TypeInfo') Then
        If Token.TokenType In [ttNumber] Then
          Begin
            Val(Token.Token, iWidth, iErrorCode);
            If iErrorCode = 0 Then
              Begin
                Field.FieldWidth := iWidth;
                NextNonCommentToken;
              End Else
                ErrorAndSeekToken(strInvalidDataWidth, 'TypeInfo', Token.Token,
                  strSeekableOnErrorTokens, stActual);
            CheckLiteral(')', 'TypeInfo');
          End Else
            ErrorAndSeekToken(strNumberExpected, 'TypeInfo', Token.Token,
              strSeekableOnErrorTokens, stActual);
    End Else
  If Token.Token = 'B' Then
    Begin
      Field.FieldType := ftBoolean;
      NextNonCommentToken;
    End Else
  If Token.Token = 'Y' Then
    Begin
      Field.FieldType := ftByte;
      NextNonCommentToken;
    End Else
  If Token.Token = 'I' Then
    Begin
      Field.FieldType := ftInteger;
      NextNonCommentToken;
    End Else
  If Token.Token = 'L' Then
    Begin
      Field.FieldType := ftLong;
      NextNonCommentToken;
    End Else
  If Token.Token = 'S' Then
    Begin
      Field.FieldType := ftSingle;
      NextNonCommentToken;
    End Else
  If Token.Token = 'F' Then
    Begin
      Field.FieldType := ftDouble;
      NextNonCommentToken;
    End Else
  If Token.Token = 'U' Then
    Begin
      Field.FieldType := ftCurrency;
      NextNonCommentToken;
    End Else
  If Token.Token = 'D' Then
    Begin
      Field.FieldType := ftDate;
      NextNonCommentToken;
    End Else
  If Token.Token = 'O' Then
    Begin
      Field.FieldType := ftLongBinary;
      NextNonCommentToken;
    End Else
  If Token.Token = 'M' Then
    Begin
      Field.FieldType := ftMemo;
      NextNonCommentToken;
    End Else
      ErrorAndSeekToken(strInvalidDataType, 'TypeInfo', Token.Token,
        strSeekableOnErrorTokens, stActual);
end;

(**

  This is the method that should be called to parse the previously parse tokens.

  @precon  None.
  @postcon Attempts to parse the token list and check it grammatically for
           Errors while providing delcaration elements for browsing.

**)
procedure TEidolonModule.ParseTokens;
begin
  Goal;
end;

(**

  This method returns an array of key words for use in the explorer module.

  @precon  None.
  @postcon Returns an array of key words for use in the explorer module.

  @return  a TKeyWords

**)
function TEidolonModule.KeyWords: TKeyWords;

Var
  i : Integer;

begin
  SetLength(Result, Succ(High(strReservedWords)));
  For i := Low(strReservedWords) To High(strReservedWords) Do
    Result[i] := strReservedWords[i];
end;

(**

  This method parses the OutputTable element of the grammar.

  @precon  StartToken must be a valid instance of a TTokenInfo class.
  @postcon Creates and parses a OutputTable element.

  @param   strName    as a String
  @param   StartToken as a TTokenInfo
  @return  a Boolean

**)
function TEidolonModule.OutputTable(strName: String;
  StartToken: TTokenInfo): Boolean;

var
  OT: TOutputTable;

begin
  Result := False;
  If Token.UToken = 'OUTPUTTABLE' Then
    Begin
      Result := True;
      NextNonCommentToken;
      If strName <> '' Then
        Begin
          OT := FOutputTableDefs.Add(TOutputTable.Create(strName, scNone,
            StartToken.Line, StartToken.Column, iiPublicInterface, Nil)) As
            TOutputTable;
          If CheckLiteral(')', 'OutputTable') Then
            If CheckLineEnd('OutputTable') Then
              Begin
                EatWhitespace;
                If CheckLiteral('{', 'OutputTable') Then
                  Begin
                    EatWhitespace;
                    If CheckLineEnd('OutputTable') Then
                      Begin
                        EatWhitespace;
                        If DatabaseDef(OT, ctPrimary) Then
                          Begin
                            EatWhitespace;
                            ConnectionDef(OT, ctPrimary);
                            EatWhitespace;
                            TableNameDef(OT, ctPrimary);
                          End;
                        EatWhitespace;
                        If DatabaseDef(OT, ctSecondary) Then
                          Begin
                            EatWhitespace;
                            ConnectionDef(OT, ctSecondary);
                            EatWhitespace;
                            TableNameDef(OT, ctSecondary);
                          End;
                        While FieldDef(OT) Do;
                        If CheckLiteral('}', 'OutputTable') Then
                          Begin
                            EatWhitespace;
                            CheckLineEnd('OutputTable');
                          End;
                      End;
                  End;
              End;
        End Else
          ErrorAndSeekToken(strTheDefintionNull, 'OutputTable', Token.Token,
            strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses the Line element of the grammar.

  @precon  StartToken must ba a valid instance of a TTokenInfo class and
           TLT must be a valid instance of a TTimeLocationTable class.
  @postcon Returns true if a LINE symbol was parsed.

  @param   strName    as a String
  @param   StartToken as a TTokenInfo
  @param   TLT        as a TTimeLocationTable
  @return  a Boolean

**)
function TEidolonModule.TLLine(strName: String; StartToken : TTokenInfo;
  TLT : TTimeLocationTable): Boolean;

Var
  L : TLine;

begin
  Result := False;
  If CompareText(Token.Token, 'LINE') = 0 Then
    Begin
      Result := True;
      NextNonCommentToken;
      L := TLT.AddSymbol(TLine.Create(strName, scNone, StartToken.Line,
        StartToken.Column, iiPublicObject, Nil)) As TLine;
      L.SymbolType := stLine;
      EatWhitespace;
      If CheckLiteral(',', 'TLLine') Then
        Begin
          BorderDef(L);
          EatWhitespace;
          CheckLineEnd('TLLine');
        End;
    End;
end;

(**

  This method tries to get a document comment from the previous token and return
  a TComment class to the calling routine.

  @note    All comments found are automatically added to the comment collection
           for disposal when the parser is destroyed.

  @precon  None.
  @postcon Returns the comment immediately before the current token else nil.

  @param   CommentPosition as a TCommentPosition
  @return  a TComment

**)
Function TEidolonModule.GetComment(
  CommentPosition : TCommentPosition) : TComment;

Var
  T : TTokenInfo;
  iOffset : Integer;
  iToken: TTokenIndex;

Begin
  Result := Nil;
  If CommentPosition = cpBeforeCurrentToken Then
    iOffset := -1
  Else
    iOffset := -2;
  iToken := TokenIndex + iOffset;
  If iToken > -1 Then
    Begin
      While (iToken > -1) And ((Tokens[iToken] As TTokenInfo).TokenType In
        [ttLineEnd, ttLineContinuation]) Do
        Dec(iToken);
      If iToken > -1 Then
        Begin;
          T := Tokens[iToken] As TTokenInfo;
          If T.TokenType In [ttLineComment, ttBlockComment] Then
            Begin
              Result := TEidolonComment.CreateComment(T.Token, T.Line, T.Column);
              OwnedItems.Add(Result);
            End;
        End;
    End;
End;

(**

  This method returns a string representing the name of the module.
  @precon  None.
  @postcon Returns a string representing the name of the module.

  @return  a String

**)
function TEidolonModule.GetModuleName: String;
begin
  Result := ExtractFilename(FileName);
end;

(**

  This method process conditional compiler directives.

  @precon  None.
  @postcon Does nothings as conditional compilations is not supported.

  @param   iSkip as an Integer as a reference

**)
Procedure TEidolonModule.ProcessCompilerDirective(var iSkip : Integer);

Begin
  // Do nothing, i.e. Conditional Compilation is NOT supported.
End;

(**

  This method does nothing as we are not referencing symbols in XML.

  @precon  None.
  @postcon Returns false always.

  @param   AToken as a TTokenInfo
  @return  a Boolean

**)
Function TEidolonModule.ReferenceSymbol(AToken : TTokenInfo) : Boolean;

Begin
  Result := False;
End;

(**

  This method parses the RequirementsTable element of the grammar.

  @precon  StartToken must be a valid instance of a TTokenInfo class.
  @postcon Creates and parses a RequirementsTable element.

  @param   strName    as a String
  @param   StartToken as a TTokenInfo
  @return  a Boolean

**)
function TEidolonModule.RequirementsTable(strName: String;
  StartToken: TTokenInfo): Boolean;

Var
  RT : TRequirementsTable;

begin
  Result := False;
  If Token.UToken = 'REQUIREMENTSTABLE' Then
    Begin
      Result := True;
      NextNonCommentToken;
      If strName <> '' Then
        Begin
          RT := FRequirementsTableDefs.Add(TRequirementsTable.Create(strName, scNone,
            StartToken.Line, StartToken.Column, iiPublicDispInterface, Nil)) As
            TRequirementsTable;
          If CheckLiteral(')', 'RequirementsTable') Then
            If CheckLineEnd('RequirementsTable') Then
              Begin
                EatWhitespace;
                If CheckLiteral('{', 'RequirementsTable') Then
                  Begin
                    EatWhitespace;
                    If CheckLineEnd('RequirementsTable') Then
                      Begin
                        EatWhitespace;
                        If DatabaseDef(RT, ctPrimary) Then
                          Begin
                            EatWhitespace;
                            ConnectionDef(RT, ctPrimary);
                            EatWhitespace;
                            TableNameDef(RT, ctPrimary);
                          End;
                        While FieldDef(RT) Do;
                        While AssociationDef(RT) Do;
                        EatWhitespace;
                        If DatabaseDef(RT, ctSecondary) Then
                          Begin
                            EatWhitespace;
                            ConnectionDef(RT, ctSecondary);
                            EatWhitespace;
                            TableNameDef(RT, ctSecondary);
                          End;
                        While FieldDef(RT) Do;
                        While AssociationDef(RT) Do;
                        If CheckLiteral('}', 'RequirementsTable') Then
                          Begin
                            EatWhitespace;
                            CheckLineEnd('RequirementsTable');
                          End;
                      End;
                  End;
              End;
        End Else
          ErrorAndSeekToken(strTheDefintionNull, 'RequirementsTable', Token.Token,
            strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses the common start elements of the xxxxTable definitions
  and passes the found name to the actual definitions.

  @precon  None.
  @postcon Parses the common start elements of the xxxxTable definitions
           and passes the found name to the actual definitions.

  @return  a Boolean

**)
function TEidolonModule.Table: Boolean;

var
  strName: String;
  StartToken : TTokenInfo;

begin
  EatWhitespace;
  Result := False;
  strName := '';
  StartToken := Nil;
  While Token.Token <> '=' Do
    Begin
      If StartToken = Nil Then
        StartToken := Token;
      strName := strName + Token.Token;
      NextNonCommentToken;
    End;
  If Token.Token = '=' Then
    Begin
      NextNonCommentToken;
      If Token.UToken = 'CLASS' Then
        Begin
          NextNonCommentToken;
          If Token.Token = '(' Then
            Begin
              NextNonCommentToken;
              Result :=
                TextTable(strName, StartToken) Or
                DBTable(strName, StartToken) Or
                TimeLocationTable(strName, StartToken) Or
                OutputTable(strName, StartToken) Or
                RequirementsTable(strName, StartToken);
              If Not Result Then
                ErrorAndSeekToken(strInvalidTableType, 'Table', Token.Token,
                  strSeekableOnErrorTokens, stActual);
            End Else
              ErrorAndSeekToken(strLiteralExpected, 'Table', '(',
                strSeekableOnErrorTokens, stActual);

        End Else
          ErrorAndSeekToken(strReservedWordExpected, 'Table', 'CLASS',
            strSeekableOnErrorTokens, stActual);
    End Else
      ErrorAndSeekToken(strLiteralExpected, 'Table', '=',
        strSeekableOnErrorTokens, stActual);
end;

(**

  This method parses the TableNameDef element of the grammar.

  @precon  Table must be a valid instance of a TBaseTable class.
  @postcon Parses the TableNameDef element of the grammar.

  @param   DBTable        as a TDBTable
  @param   ConnectionType as a TConnectionType

**)
procedure TEidolonModule.TableNameDef(DBTable : TDBTable; ConnectionType : TConnectionType);

Var
  T : TTableNameDef;

begin
  If Token.Token = strConnectionType[ConnectionType] Then
    Begin
      NextNonCommentToken;
      If CompareText(Token.Token, 'TABLENAME') = 0 Then
        Begin
          Case ConnectionType Of
            ctPrimary:
              T := DBTable.AddPrimary(TTableNameDef.Create('TableName', scNone,
                Token.Line, Token.Column, iiPublicType, Nil)) As TTableNameDef;
          Else
            T := DBTable.AddSecondary(TTableNameDef.Create('TableName', scNone,
              Token.Line, Token.Column, iiPublicType, Nil)) As TTableNameDef;
          End;
          NextNonCommentToken;
          If CheckLiteral('=', 'TableNameDef') Then
            Begin
              While Not (Token.TokenType In [ttLineEnd]) Do
                AddToExpression(T);
              CheckLineEnd('TableNameDef')
            End;
        End;
    End;
end;

(**

  This method parses the TextTable element of the grammar.

  @precon  StartToken must be a valid instance of a TTokenInfo class.
  @postcon Parses the TextTable element of the grammar

  @param   strName    as a String
  @param   StartToken as a TTokenInfo
  @return  a Boolean

**)
Function TEidolonModule.TextTable(strName : String; StartToken : TTokenInfo) : Boolean;

Var
  TT : TTextTable;

begin
  Result := False;
  If Token.UToken = 'TEXTTABLE' Then
    Begin
      Result := True;
      NextNonCommentToken;
      If strName <> '' Then
        Begin
          TT := FTextTableDefs.Add(TTextTable.Create(strName, scNone,
            StartToken.Line, StartToken.Column, iiPublicThreadVar, Nil)) As TTextTable;
          If CheckLiteral(')', 'TextTable') Then
            If CheckLineEnd('TextTable') Then
              Begin
                EatWhitespace;
                If CheckLiteral('{', 'TextTable') Then
                  Begin
                    EatWhitespace;
                    If CheckLineEnd('TextTable') Then
                      Begin
                        EatWhiteSpace;
                        TextTableDef(TT);
                        While FieldDef(TT) Do;
                        If CheckLiteral('}', 'TextTable') Then
                          Begin
                            EatWhitespace;
                            CheckLineEnd('TextTable');
                          End;
                      End;
                  End;
              End;
        End Else
          ErrorAndSeekToken(strTheDefintionNull, 'DBTable', Token.Token,
            strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses the TextTableDef element of the grammar.

  @precon  TextTable must be a valid instance of a TTextTable class.
  @postcon Parses the TextTableDef element of the grammar.

  @param   TextTable as a TTextTable

**)
procedure TEidolonModule.TextTableDef(TextTable : TTextTable);

Var
  strFileName : String;
  TTD: TTextTableDef;

begin
  If Token.Token = '#' Then
    Begin
      NextNonCommentToken;
      If CompareText(Token.Token, 'TABLENAME') = 0 Then
        Begin
          TTD := TextTable.Add(TTextTableDef.Create('TableName', scNone, Token.Line,
            Token.Column, iiPublicRecord, Nil)) As TTextTableDef;
          NextNonCommentToken;
          If CheckLiteral('=', 'TextTableDef') Then
            Begin
              strFileName := '';
              While Not (Token.TokenType In [ttLineEnd]) Do
                AddToExpression(TTD);
              TextTable.FileName := strFileName;
              CheckLineEnd('TextTableDef');
            End;
        End Else
          RollBackToken;
    End;
end;

(**

  This method deletes any root elements which dont and items in them.

  @precon  None.
  @postcon Deletes any root elements which dont and items in them.

**)
procedure TEidolonModule.TidyUpEmptyElements;

Var
  iElement : Integer;

begin
  For iElement := ElementCount DownTo 1 Do
    If Elements[iElement].ElementCount = 0 Then
      If Elements[iElement] Is TLabelContainer Then
        DeleteElement(iElement);
end;

(**

  This method parses the TimeLocationDef elements of the grammar.

  @precon  Table must be a valid instance of a TTimeLocationTable class.
  @postcon Parses the TimeLocationDef elements of the grammar.

  @param   Table as a TTimeLocationTable
  @return  a Boolean

**)
function TEidolonModule.TimeLocationDef(Table : TTimeLocationTable): Boolean;

Var
  strName : String;
  StartToken : TTokenInfo;

begin
  Result := False;
  StartToken := Nil;
  EatWhitespace;
  If Token.Token = '&' Then
    Begin
      Result := True;
      NextNonCommentToken;
      While Token.Token <> '=' Do
        Begin
          If StartToken = Nil Then
            StartToken := Token;
          strName := strName + Token.Token;
          NextNonCommentToken;
        End;
      If strName <> '' then
        Begin
          If CheckLiteral('=', 'TimeLocationDef') Then
            Begin
              EatWhitespace;
              If TLRectangle(strName, StartToken, Table) Or
                TLBar(strName, StartToken, Table) Or
                TLLine(strName, StartToken, Table) Or
                TLTriangle(strName, StartToken, Table) Or
                TLEllipse(strName, StartToken, Table) Or
                TLDiamond(strName, StartToken, Table) Then;
            End;
        End Else
          ErrorAndSeekToken(strNullName, 'TimeLocationDef', Token.Token,
            strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses the TimeLocationTable element of the grammar.

  @precon  StartToken must be a valid instance of a TTokenInfo class.
  @postcon Parses the TimeLocationTable element of the grammar.

  @param   strName    as a String
  @param   StartToken as a TTokenInfo
  @return  a Boolean

**)
Function TEidolonModule.TimeLocationTable(strName : String; StartToken : TTokenInfo) : Boolean;

var
  TLT: TTimeLocationTable;

begin
  Result := False;
  If Token.UToken = 'TIMELOCATIONTABLE' Then
    Begin
      Result := True;
      NextNonCommentToken;
      If strName <> '' Then
        Begin
          TLT := FTimeLocationTableDefs.Add(TTimeLocationTable.Create(strName,
            scNone, StartToken.Line, StartToken.Column, iiPublicVariable,
            Nil)) As TTimeLocationTable;
          If CheckLiteral(')', 'TimeLocationTable') Then
            If CheckLineEnd('TimeLocationTable') Then
              Begin
                EatWhitespace;
                If CheckLiteral('{', 'TimeLocationTable') Then
                  Begin
                    EatWhitespace;
                    If CheckLineEnd('TimeLocationTable') Then
                      Begin
                        While FieldDef(TLT) Or EmptyLine Do;
                        While TimeLocationDef(TLT) Or EmptyLine Do;
                        If CheckLiteral('}', 'TimeLocationTable') Then
                          Begin
                            EatWhitespace;
                            CheckLineEnd('TimeLocationTable');
                          End;
                      End;
                  End;
              End;
        End Else
          ErrorAndSeekToken(strTheDefintionNull, 'TimeLocationTable', Token.Token,
            strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses the Rectangle element of the grammar.

  @precon  StartToken must ba a valid instance of a TTokenInfo class and
           TLT must be a valid instance of a TTimeLocationTable class.
  @postcon Returns true is a RECTANGLE was parsed.

  @param   strName    as a String
  @param   StartToken as a TTokenInfo
  @param   TLT        as a TTimeLocationTable
  @return  a Boolean

**)
function TEidolonModule.TLRectangle(strName: String; StartToken : TTokenInfo;
  TLT : TTimeLocationTable): Boolean;

Var
  R : TRectangle;

begin
  Result := False;
  If CompareText(Token.Token, 'RECTANGLE') = 0 Then
    Begin
      Result := True;
      NextNonCommentToken;
      R := TLT.AddSymbol(TRectangle.Create(strName, scNone, StartToken.Line,
        StartToken.Column, iiPublicObject, Nil)) As TRectangle;
      R.SymbolType := stRectangle;
      EatWhitespace;
      If CheckLiteral(',', 'TLRectangle') Then
        Begin
          BorderDef(R);
          EatWhitespace;
          If CheckLiteral(',', '') Then
            Begin
              InteriorDef(R);
              Transparency(R);
              CheckLineEnd('TLRectangle');
            End;
        End;
    End;
end;

(**

  This method is the starting position for the parsing of an Eidolon module. It
  finds the first non comment token and begins the grammar checking from their
  by deligating Syntax.

  @precon  None.
  @postcon It finds the first non comment token and begins the grammar checking
           from their by deligating Syntax.

**)
procedure TEidolonModule.Goal;

var
  C: TComment;

begin
  Line := 1;
  Column := 1;
  Try
    If TokenCount > 0 Then
      Begin
        // Find first non comment token
        While (Token.TokenType In [ttLineComment, ttBlockComment, ttLineEnd]) And
          Not EndOfTokens Do
          Begin
            If Token.TokenType In [ttLineComment, ttBlockComment] Then
              Begin
                C := TEidolonComment.CreateComment(Token.Token, Token.Line,
                  Token.Column);
                AddBodyComment(C);
                If Comment = Nil Then
                  Comment := C;
              End;
            NextToken;
          End;
        // Check for end of file else must be identifier
        While Not EndOfTokens And (EmptyLine Or Table) Do;
      End;
  Except
    On E : EParserAbort Do
      AddIssue(E.Message, scNone, 'Goal', 0, 0, etError);
  End;
end;

(**

  This method parses the InteriorColour element of the grammar.

  @precon  Rectangle must be a valid instance of TRectangle class.
  @postcon Assigns the interior colour to the rectangle.

  @param   CustomSymbol as a TCustomFillSymbol

**)
procedure TEidolonModule.InteriorColour(CustomSymbol : TCustomFillSymbol);
begin
  CustomSymbol.InteriorColour := ColourName;
end;

(**

  This method parses the InteriorDef element of the grammar.

  @precon  Rectangle must be a valid instance of a TRectangle.
  @postcon Parses the InteriorDef element of the grammar.

  @param   CustomSymbol as a TCustomFillSymbol

**)
procedure TEidolonModule.InteriorDef(CustomSymbol: TCustomFillSymbol);
begin
  EatWhitespace;
  InteriorColour(CustomSymbol);
  EatWhitespace;
  If CheckLiteral(',', 'InteriorDef') Then
    Begin
      EatWhitespace;
      InteriorPattern(CustomSymbol);
      EatWhitespace;
      If CheckLiteral(',', 'InteriorDef') Then
        Begin
          EatWhitespace;
          InteriorPatternColour(CustomSymbol);
        End;
    End;
end;

(**

  This method parses the InteriorPattern element of the grammar.

  @precon  Rectangle
  @postcon Sets the InteriorPattern of the passed symbol.

  @param   CustomSymbol as a TCustomFillSymbol

**)
procedure TEidolonModule.InteriorPattern(CustomSymbol : TCustomFillSymbol);

var
  boolFound: Boolean;
  iPattern: TInteriorPattern;
  strPattern : String;

begin
  CustomSymbol.InteriorPattern := ipNONE;
  boolFound := False;
  strPattern := '';
  While Token.TokenType In [ttNumber, ttIdentifier] Do
    Begin
      strPattern := strPattern + Token.Token;
      NextNonCommentToken;
    End;
  If strPattern = '' Then
    strPattern := Token.Token;
  For iPattern := Low(TInteriorPattern) To High(TInteriorPattern) Do
    If CompareText(strPattern, strInteriorPatterns[iPattern]) = 0 Then
      Begin
        CustomSymbol.InteriorPattern := iPattern;
        boolFound := True;
        Break;
      End;
  If Not boolFound Then
    ErrorAndSeekToken(strInvalidPattern, 'InteriorPattern', strPattern,
      strSeekableOnErrorTokens, stActual);
end;

(**

  This method parses the InteriorPatternColour element of the grammar.

  @precon  Rectangle must be a valid instance of TRectangle class.
  @postcon Assigns the interior pattern colour to the rectangle.

  @param   CustomSymbol as a TCustomFillSymbol

**)
procedure TEidolonModule.InteriorPatternColour(CustomSymbol : TCustomFillSymbol);
begin
  CustomSymbol.InteriorPatternColour := ColourName;
  If (CustomSymbol.InteriorPattern = ipNONE) And
    (CustomSymbol.InteriorPatternColour <> xlcNONE) Then
    AddIssue(Format(strYouHaveSpecifiedAColour,
      [strColours[CustomSymbol.InteriorPatternColour]]), scNone,
      'InteriorPatternColour', CustomSymbol.Line, CustomSymbol.Column, etWarning);
  If (CustomSymbol.InteriorPattern <> ipNONE) And
    (CustomSymbol.InteriorPatternColour = xlcNONE) Then
    AddIssue(Format(strYouHaveSpecifiedAPattern,
      [strInteriorPatterns[CustomSymbol.InteriorPattern]]), scNone,
      'InteriorPatternColour', CustomSymbol.Line, CustomSymbol.Column, etWarning);
end;

End.
