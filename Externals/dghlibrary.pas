(**

  This module contains numerous library functions, procedures and classes that
  can be used within many applications.

  @Version 1.0
  @Author  David Hoyle
  @Date    22 Mar 2009

**)
Unit DGHLibrary;

Interface

Uses
  SysUtils, Classes, Windows, Graphics;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A custom exception for errors getting the building number. **)
  EBuildNumberException = Class(Exception);

  (** A custom exception for converting string dates to actual dates. **)
  EDateConversionException = Class(Exception);

  (** A record to describe the degrees, minutes and seconds of a bearing. **)
  TBearing = Record
    iDegrees : Integer;
    iMinutes : Integer;
    iSeconds : Integer;
    iHundreds : Integer;
  End;

  (** This is a record that describes a vector using a distance and a
      bearing. **)
  TVector = Record
    dblDistance : Double;
    dblBearing  : Double;
  End;

  (** This is a custom Math Exception. **)
  EMathException = Class(Exception);

  (** This is a custom exception for an invalid Degree Minutes Seconds
      value. **)
  EInvalidDMSFormatException = Class(Exception);

  (** This is an enumerate which defined the types of horizontal element that
      can used in a horizontal alignment. **)
  TElementType = (etUnknown, etStraight, etCircular, etClothoid);

  (** This is an enumerate which defined the way in which the below THInfo
      record can be viewed. **)
  THInfoType = (itSetout, itMeasure, itCompare);

  (** This is a custom exception for a zero length of negative error
      condition. **)
  ELengthZeroOrNegativeException = Class(Exception);
  (** This is a custom exception for a zero length radius. **)
  EZeroRadiusException = Class(Exception);
  (** This is a custom exception for a zero length Radius * Length value. **)
  EZeroRLValueExecption = Class(Exception);
  (** This is an custom exception for a before alignment start error **)
  EBeforeAlignmentStartException = Class(Exception);
  (** This is an custom exception for an after alignment end error  **)
  EAfterAlignmentEndException = Class(Exception);

  (**

    This THInfo Structure defined the return results from several of the methods
    of the alignment classes. It is a record with different identities and can
    therefore return different information as follows:

    Setout : Bearing, Easting,  Northing;  or
    Measure: Bearing, Chainage, Offset;    or
    Compare: Bearing, Chainage, Distance;

  **)
  THInfo = Record
    dblBearing : Double;
    Case THInfoType Of
      itSetout  : (dblEasting, dblNorthing  : Double);
      itMeasure, itCompare : (dblChainage : Double;
        Case THInfoType Of
          itMeasure: (dblOffset : Double);
          itCompare: (dblDistance : Double);
    );
  End;

  (** This record defined the results of a compare method. **)
  THCompInfo = Record
    dblEasting  : Double;
    dblNorthing : Double;
    dblBearing  : Double;
    dblChainage : Double;
    dblDistance : Double;
  End;

  (** This record defined information for a vertical alignment. **)
  TVInfo = Record
    dblLevel    : Double;
    dblGradient : Double;
  End;

  (** This rcord defined information for both a vertical and horizontal
      result. **)
  TInfo = Record
    HInfo : THInfo;
    VInfo : TVInfo;
  End;

  (** This record defined information for a Horizontal element of an
      alignment **)
  THElement = Record
    dblEasting  : Double;
    dblNorthing : Double;
    dblBearing  : Double;
    dblChainage : Double;
    dblLength   : Double;
    dblRadius   : Double;
    dblRLValue    : Double;
    dblStChainage : Double;
  End;

  (** This record defines a vertical element of an alignment. **)
  TVElement = Record
    dblLevel    : Double;
    dblGradient : Double;
    dblChainage : Double;
    dblLength   : Double;
    dblRadius   : Double;
  End;

  (** This record defined the results of a measure. **)
  TMeasureInfo = Record
    dblChainage : Double;
    dblOffset   : Double;
    dblBearing  : Double;
    dblLevel    : Double;
    dblGradient : Double;
  End;

  (** This record defined the results of a compare. **)
  TCompareInfo = Record
    dblEasting   : Double;
    dblNorthing  : Double;
    dblIEasting  : Double;
    dblINorthing : Double;
    dblChainage  : Double;
    dblDistance  : Double;
    dblBearing   : Double;
    dblLevel     : Double;
    dblGradient  : Double;
  End;

  (** This class is the base class in the heiracrhy for the horizontal
      alignments classes. **)
  THBaseElement = Class
  {$IFDEF D2005} Strict {$ENDIF} Private
    FEasting  : Double;
    FNorthing : Double;
    FBearing  : Double;
    FChainage : Double;
    FLength   : Double;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetElementType : TElementType; Virtual; Abstract;
    Function GetStartPoint : THInfo; Virtual;
    Function GetEndPoint : THInfo; Virtual;
    Function GetStartChainage : Double; Virtual;
    Function GetEndChainage : Double; Virtual;
    Function GetRadius(dblChainage : Double) : Double; Virtual; Abstract;
    Function GetElementDetails : THElement; Virtual; Abstract;
  Public
    Constructor Create(dblEasting, dblNorthing, dblBearing, dblChainage,
      dblLength : Double); Virtual;
    Function Setout(dblChainage, dblOffset : Double) : THInfo; Virtual; Abstract;
    Function Measure(dblEasting, dblNorthing : Double) : THInfo; Virtual; Abstract;
    Function Compare(dblEasting, dblNorthing, dblBearing : Double) : THCompInfo;
      Virtual; Abstract;
    (**
      This property returns the Eastings value.
      @precon  None.
      @postcon Returns the Eastings value.
      @return  a Double
    **)
    Property Easting  : Double Read FEasting;
    (**
      This property returns the northing value.
      @precon  None.
      @postcon Returns the northings value.
      @return  a Double
    **)
    Property Northing : Double Read FNorthing;
    (**
      This property returns the bearing value.
      @precon  None.
      @postcon Returns the bearing value.
      @return  a Double
    **)
    Property Bearing  : Double Read FBearing;
    (**
      This property returns the chainage value.
      @precon  None.
      @postcon Returns the chainage value.
      @return  a Double
    **)
    Property Chainage : Double Read FChainage;
    (**
      This property returns the length of the element.
      @precon  None.
      @postcon Returns the length of the element.
      @return  a Double
    **)
    Property Length   : Double Read FLength;
    (**
      This property returns the type of the element.
      @precon  None.
      @postcon Returns the type of the element.
      @return  a TElementType
    **)
    Property ElementType : TElementType Read GetElementType;
    (**
      This property returns the start point of the element.
      @precon  None.
      @postcon Returns the start point of the element.
      @return  a THInfo
    **)
    Property StartPoint : THInfo Read GetStartPoint;
    (**
      This property returns the end point of the element.
      @precon  None.
      @postcon Returns the end point of the element.
      @return  a THInfo
    **)
    Property EndPoint : THInfo Read GetEndPoint;
    (**
      This property return the start chainage of the element.
      @precon  None.
      @postcon Return the start chainage of the element.
      @return  a Double
    **)
    Property StartChainage : Double Read GetStartChainage;
    (**
      This property returns the end chainage of the element.
      @precon  None.
      @postcon Returns the end chainage of the element.
      @return  a Double
    **)
    Property EndChainage : Double Read GetEndChainage;
    (**
      This property returns the radius of the element.
      @precon  None.
      @postcon Returns the radius of the element.
      @param   dblChainage as       a Double
      @return  a Double
    **)
    Property Radius[dblChainage : Double] : Double Read GetRadius;
    (**
      This property returns the elements details as a recorOrd.
      @precon  None.
      @postcon Returns the elements details as a recorOrd.
      @return  a THElement
    **)
    Property ElementDetails : THElement Read GetElementDetails;
  End;

  (** This class defined a Straight Horizontal alignment element. **)
  THStraightElement = Class(THBaseElement)
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetElementType : TElementType; Override;
    Function GetRadius(dblChainage : Double) : Double; Override;
    Function GetElementDetails : THElement; Override;
  Public
    Constructor Create(dblEasting, dblNorthing, dblBearing, dblChainage,
      dblLength : Double); Reintroduce; Overload; Virtual;
    Function Setout(dblChainage, dblOffset : Double) : THInfo; Override;
    Function Measure(dblEasting, dblNorthing : Double) : THInfo; Override;
    Function Compare(dblEasting, dblNorthing, dblBearing : Double) : THCompInfo;
      Override;
  End;

  (** This class defined a Circular Horizontal alignment element. **)
  THCircularElement = Class(THBaseElement)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FRadius : Double;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetElementType : TElementType; Override;
    Function GetRadius(dblChainage : Double) : Double; Override;
    Function GetElementDetails : THElement; Override;
  Public
    Constructor Create(dblEasting, dblNorthing, dblChainage, dblBearing,
      dblLength, dblRadius : Double; bCentre : Boolean); ReIntroduce; Overload;
      Virtual;
    Function Setout(dblChainage, dblOffset : Double) : THInfo; Override;
    Function Measure(dblEasting, dblNorthing : Double) : THInfo; Override;
    Function Compare(dblEasting, dblNorthing, dblBearing : Double) : THCompInfo;
      Override;
  End;

  (** This class defined a Clothoid Horizontal alignment element. **)
  THClothoidElement = Class(THBaseElement)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FOChainage : Double;
    FRLValue : Double;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetX(dblDistance : Double) : Double; Virtual;
    Function GetY(dblDistance : Double) : Double; Virtual;
    Function GetTheta(dblDistance : Double) : Double; Virtual;
    Function GetElementType : TElementType; Override;
    Function GetRadius(dblChainage : Double) : Double; Override;
    Function GetElementDetails : THElement; Override;
  Public
    Constructor Create(dblEasting, dblNorthing, dblChainage, dblBearing,
      dblLength, dblStChainageOrRadius, dblRLValueOrEndRadius : Double;
      boolFalse : Boolean); Reintroduce; Overload; Virtual;
    Function Setout(dblChainage, dblOffset : Double) : THInfo; Override;
    Function Measure(dblEasting, dblNorthing : Double) : THInfo; Override;
    Function Compare(dblEasting, dblNorthing, dblBearing : Double) : THCompInfo;
      Override;
  End;

  (** This class is the base class in the heiracrhy for the vertical
      alignments classes. **)
  TVBaseElement = Class
  {$IFDEF D2005} Strict {$ENDIF} Private
    FLevel    : Double;
    FGradient : Double;
    FChainage : Double;
    FLength   : Double;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetElementType : TElementType; Virtual; Abstract;
    Function GetStartPoint : TVInfo; Virtual;
    Function GetEndPoint : TVInfo; Virtual;
    Function GetStartChainage : Double; Virtual;
    Function GetEndChainage : Double; Virtual;
    Function GetRadius(dblChainage : Double) : Double; Virtual; Abstract;
    Function GetElementDetails : TVElement; Virtual; Abstract;
  Public
    Constructor Create(dblLevel, dblGradient, dblChainage, dblLength : Double);
      Virtual;
    Function Setout(dblChainage : Double) : TVInfo; Virtual; Abstract;
    (**
      This property returns the starting level of the element.
      @precon  None.
      @postcon Returns the starting level of the element.
      @return  a Double
    **)
    Property Level : Double Read FLevel;
    (**
      This property returns the starting gradient of the element.
      @precon  None.
      @postcon Returns the starting gradient of the element.
      @return  a Double
    **)
    Property Gradient : Double Read FGradient;
    (**
      This property returns the starting chainage of the element.
      @precon  None.
      @postcon Returns the starting chainage of the element.
      @return  a Double
    **)
    Property Chainage : Double Read FChainage;
    (**
      This property returns the length of the element.
      @precon  None.
      @postcon Returns the length of the element.
      @return  a Double
    **)
    Property Length : Double Read FLength;
    (**
      This property returns the elements type.
      @precon  None.
      @postcon Returns the elements type.
      @return  a TElementType
    **)
    Property ElementType : TElementType Read GetElementType;
    (**
      This property returns the start point of the element.
      @precon  None.
      @postcon Returns the start point of the element.
      @return  a TVInfo
    **)
    Property StartPoint : TVInfo Read GetStartPoint;
    (**
      This property returns the end point of the element.
      @precon  None.
      @postcon Returns the end point of the element.
      @return  a TVInfo
    **)
    Property EndPoint : TVInfo Read GetEndPoint;
    (**
      This property returns the start chainage of the element.
      @precon  None.
      @postcon Returns the start chainage of the element.
      @return  a Double
    **)
    Property StartChainage : Double Read GetStartChainage;
    (**
      This property returns the end chaninage of the element.
      @precon  None.
      @postcon Returns the end chaninage of the element.
      @return  a Double
    **)
    Property EndChainage : Double Read GetEndChainage;
    (**
      This property returns the radius of the element at a point.
      @precon  None.
      @postcon Returns the radius of the element at a point.
      @param   dblChainage as       a Double
      @return  a Double
    **)
    Property Radius[dblChainage : Double] : Double Read GetRadius;
    (**
      This property returns the elements details.
      @precon  None.
      @postcon Returns the elements details.
      @return  a TVElement
    **)
    Property ElementDetails : TVElement Read GetElementDetails;
  End;

  (** This class defined a Straight Vertical element of an alignment. **)
  TVStraightElement = Class(TVBaseElement)
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetElementType : TElementType; Override;
    Function GetRadius(dblChainage : Double) : Double; Override;
    Function GetElementDetails : TVElement; Override;
  Public
    Constructor Create(dblLevel, dblGradient, dblChainage, dblLength : Double);
      Reintroduce; Overload;
    Function Setout(dblChainage : Double) : TVInfo; Override;
  End;

  (** This class defined a Circular Vertical element of an alignment. **)
  TVCircularElement = Class(TVBaseElement)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FRadius : Double;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetElementType : TElementType; Override;
    Function GetRadius(dblChainage : Double) : Double; Override;
    Function GetElementDetails : TVElement; Override;
  Public
    Constructor Create(dblLevel, dblGradient, dblChainage, dblLength,
      dblRadius : Double); Reintroduce; Overload;
    Function Setout(dblChainage : Double) : TVInfo; Override;
  End;

  (** Forward declaration of the class type THElements **)
  THElements = Class(TList);

  (** Forward declaration of the class type TVElements **)
  TVElements = Class(TList);

  (** This class represents a collection of horizontal alignment elements. **)
  THAlignmentCollection = Class
  {$IFDEF D2005} Strict {$ENDIF} Private
    FHElements : THElements;
    FOnChange : TNotifyEvent;
    FCoordinateError: Double;
    FBearingError: Double;
    FModified: Boolean;
    procedure SetCoordinateError(const Value: Double);
    procedure SetBearingError(const Value: Double);
    procedure SetModified(const Value: Boolean);
  Public
    Constructor Create; Virtual;
    Destructor Destroy; Override;
    Function Count : Integer; Virtual;
    Function ElementType(iElement : Integer) : TElementType; Virtual;
    Function GetElementStart(iElement : Integer) : THInfo; Virtual;
    Function GetElementEnd(iElement: Integer): THInfo; Virtual;
    Function GetStartChainage(iElement : Integer) : Double; Virtual;
    Function GetEndChainage(iElement : Integer) : Double; Virtual;
    Function GetRadius(iElement : Integer;
      dblChainage : Double) : Double; Virtual;
    Procedure AddStraight(dblEasting, dblNorthing, dblBearing, dblChainage,
      dblLength : Double); Virtual;
    Procedure AddCircular(dblEasting, dblNorthing, dblBearing, dblChainage,
      dblRadius, dblLength : Double; bCentre : Boolean); Virtual;
    procedure AddClothoid(dblEasting, dblNorthing, dblBearing,
      dblOChainage, dblStChainage, dblRLValue, dblLength: Double); Virtual;
    procedure AddClothoidFalse(dblEasting, dblNorthing, dblBearing,
      dblChainage, dblLength, dblStRadius, dblEndRadius: Double); Virtual;
    Procedure UpdateStraight(iElement : Integer; dblEasting, dblNorthing,
      dblBearing, dblChainage, dblLength : Double); Virtual;
    Procedure UpdateCircular(iELement : Integer; dblEasting, dblNorthing,
      dblBearing, dblChainage, dblRadius, dblLength : Double;
      bCentre : Boolean); Virtual;
    procedure UpdateClothoid(iELement : Integer; dblEasting, dblNorthing,
      dblBearing, dblOChainage, dblStChainage, dblRLValue,
      dblLength: Double; boolFalse : Boolean); Virtual;
    Procedure LoadFromFile(strFileName : String); Virtual;
    Procedure SaveToFile(strFileName : String); Virtual;
    Function GetStraight(iElement : Integer) : THElement; Virtual;
    Function GetCircular(iElement : Integer) : THElement; Virtual;
    Function GetClothoid(iElement : Integer) : THElement; Virtual;
    Function StartChainage : Double;
    Function EndChainage : Double;
    Procedure Clear;
    Function Setout(dblChainage, dblOffset : Double) : THInfo; Virtual;
    Function Measure(dblEasting, dblNorthing : Double) : THinfo; Virtual;
    Function Compare(dblEasting, dblNorthing, dblBearing : Double) : THCompInfo;
      Virtual;
    { Properties }
    (**
      A property to provide an event for the chnage of a horizontal alignment.
      @precon  None.
      @postcon Event hook for an OnChange event handler.
      @return  a TNotifyEvent
    **)
    Property OnChange : TNotifyEvent Read FOnChange Write FOnChange;
    (**
      A notification event handler fired if the horizontal alignment changes.
      @precon  None.
      @postcon Fires an event if the alignment changes.
      @return  a Double
    **)
    Property CoordinateError : Double read FCoordinateError write SetCoordinateError;
    (**
      A property for reading the tolerance in coordinate error allowed.
      @precon  None.
      @postcon Returns a coordinate error.
      @return  a Double
    **)
    Property BearingError : Double read FBearingError write SetBearingError;
    (**
      A property to reading the tolerance in bearing error allowed
      @precon  None.
      @postcon Returns the bearing error.
      @return  a Boolean
    **)
    Property Modified: Boolean read FModified write SetModified Default False;
  End;

  (** This class represents a collection of vertical alignment elements. **)
  TVAlignmentCollection = Class
  {$IFDEF D2005} Strict {$ENDIF} Private
    FVElements : TVElements;
    FModified: Boolean;
    FOnChange: TNotifyEvent;
    FLevelError: Double;
    FGradientError: Double;
    procedure SetModified(const Value: Boolean);
    procedure SetGradientError(const Value: Double);
    procedure SetLevelError(const Value: Double);
  Public
    Constructor Create; Virtual;
    Destructor Destroy; Override;
    Function Count : Integer; Virtual;
    Function ElementType(iElement : Integer) : TElementType; Virtual;
    Function GetElementStart(iElement : Integer) : TVInfo; Virtual;
    Function GetElementEnd(iElement: Integer): TVInfo; Virtual;
    Function GetStartChainage(iElement : Integer) : Double; Virtual;
    Function GetEndChainage(iElement : Integer) : Double; Virtual;
    Function GetRadius(iElement : Integer; dblChainage : Double) : Double; Virtual;
    Procedure LoadFromFile(strFileName : String); Virtual;
    Procedure SaveToFile(strFileName : String); Virtual;
    Procedure AddStraight(dblLevel, dblGradient, dblChainage,
      dblLength : Double); Virtual;
    Procedure AddCircular(dblLevel, dblGradient, dblChainage, dblLength,
      dblRadius : Double); Virtual;
    Procedure UpdateStraight(iElement : Integer; dblLevel, dblGradient,
      dblChainage, dblLength : Double); Virtual;
    Procedure UpdateCircular(iElement : Integer; dblLevel, dblGradient,
      dblChainage, dblLength, dblRadius : Double); Virtual;
    Function GetStraight(iElement : Integer) : TVElement; Virtual;
    Function GetCircular(iElement : Integer) : TVElement; Virtual;
    Function StartChainage : Double;
    Function EndChainage : Double;
    Procedure Clear;
    Function Setout(dblChainage : Double) : TVInfo; Virtual;
    { Properties }
    (**
      A property to notify of changes made to the class.
      @precon  None.
      @postcon Fires a notification of A change
      @return  a TNotifyEvent
    **)
    Property OnChange : TNotifyEvent read FOnChange Write FOnChange;
    (**
      A property to determine if the alignment has changed.
      @precon  None.
      @postcon Returns a boolean valud indicating if the alignment is modified.
      @return  a Boolean
    **)
    Property Modified: Boolean read FModified write SetModified Default False;
    (**
      A property to determine the tolerance of an error in the level
      @precon  None.
      @postcon Returns a level error.
      @return  a Double
    **)
    Property LevelError : Double read FLevelError write SetLevelError;
    (**
      A property to determine the tolerance of an error in the gradient.
      @precon  None.
      @postcon Returns a gradient error.
      @return  a Double
    **)
    Property GradientError : Double read FGradientError write SetGradientError;
  End;

  (** This class represents a collection of both horizontal and vertical
      alignment elements. **)
  TStringAlignment = Class
  {$IFDEF D2005} Strict {$ENDIF} Private
    FHAlignment : THAlignmentCollection;
    FVAlignment : TVAlignmentCollection;
  Public
    Constructor Create; Virtual;
    Destructor Destroy; Override;
    Procedure SaveToFile(strFileName : String); Virtual;
    Procedure LoadFromFile(strFileName : String); Virtual;
    Procedure Clear;
    Function Setout(dblChainage, dblOffset : Double) : TInfo; Virtual;
    Function Measure(dblEasting, dblNorthing : Double) : TMeasureInfo;
      Virtual;
    Function Compare(dblEasting, dblNorthing, dblBearing : Double) : TCompareInfo; Virtual;
    (**
      A property to define the horizontal alignment of the string line
      @precon  None.
      @postcon Returns the horizontal alignment within the string line
      @return  a THAlignmentCollection
    **)
    Property HAlignment : THAlignmentCollection Read FHAlignment;
    (**
      A property to define the vertical alignment of the string line
      @precon  None.
      @postcon Returns the vertical alignment within the string line
      @return  a TVAlignmentCollection
    **)
    Property VAlignment : TVAlignmentCollection Read FVAlignment;
  End;

  (** This is a procedure type for handling Exception messages in ParseMacro. **)
  TExceptionProcedure = Procedure(strExceptionMsg : String) Of Object;

  (** An interface that needs to be implemented by an class passed to
      DGHCreateProcess so that messages can be handled. **)
  IDGHCreateProcessEvents = Interface
    Procedure ProcessMsgHandler(strMsg : String; var boolAbort : Boolean);
    Procedure IdleHandler;
  End;

  (** A record to describe the information required by DGHCreateProcess. **)
  TProcessInfo = Record
    boolEnabled : Boolean;
    strEXE : String;
    strParams : String;
    strDir : String;
  End;

  Function DGHCreateProcess(Process : TProcessInfo; ProcMsgHndr : IDGHCreateProcessEvents) : Integer;
  Function AdjustBearing(dblBearing : Double) : Double;
  Function BearingToString(recBearing : TBearing) : String;
  Function BinToDec(sDisplayNumber : String) : String;
  Function BinToHex(sDisplayNumber : String) : String;
  Function BinToOct(sDisplayNumber : String) : String;
  Function Capitalise(strText : String) : String;
  Function CharCount(cChar : Char; strText : String) : Integer;
  Function ConvertDate(Const strDate : String) : TDateTime;
  Function DecimalToDMS(dblBearing : Double) : TBearing;
  Function DecToBin(sDisplayNumber : String) : String;
  Function DecToHex(sDisplayNumber : String) : String;
  Function DecToOct(sDisplayNumber : String) : String;
  Function DMSToDecimal(strBearing : String) : Double;
  Function DMSAsDecToDecimal(dblBearingAsDec : Double) : Double;
  Function ExtractFileNameOnly(strFileName : String) : String;
  Function GetField(strText : String; Ch : Char; iIndex : Integer): String;
  Function GetVector(dblStE, dblStN, dblEndE, dblEndN : Double) : TVector;
  Function HexToBin(sDisplayNumber : String) : String;
  Function HexToDec(sDisplayNumber : String) : String;
  Function HexToOct(sDisplayNumber : String) : String;
  Function IsKeyWord(strWord : String; strWordList : Array Of String): Boolean;
  Procedure MakeAssociation(Const strFileExt, strFileClass, strDescription,
    strDefaultIcon : String; bAlwaysShowExt, bQuickView : Boolean);
  Procedure MakeAssociatedVerb(const strFileExt, strVerb, strVerbCaption,
    strVerbCommand : String; bUseDDE : Boolean; Const strService, strTopic,
    strMacro, strMacroNotRunning : String);
  Function OctToBin(sDisplayNumber : String) : String;
  Function OctToDec(sDisplayNumber : String) : String;
  Function OctToHex(sDisplayNumber : String) : String;
  Function PosOfNthChar(strText : String; Ch : Char; iIndex : Integer): Integer;
  Function Pow(X, Y : Real) : Real;
  Function ReduceBearing(dblBearing : Double) : Double;
  Function GetBuildNumber(strFileName : String; var iMajor, iMinor, iBugfix,
    iBuild : Integer) : String;
  Function CheckConsoleMode(hndConsole : THandle) : Boolean;
  Function GetConsoleTitle(strTitle: String) : String;
  Procedure OutputToConsole(hndConsole : THandle; Const strText : String = '';
    iTextColour : TColor = clNone; iBackColour : TColor = clNone;
    boolUpdateCursor : Boolean = True);
  Procedure OutputToConsoleLn(hndConsole : THandle; Const strText : String = '';
    iTextColour : TColor = clNone; iBackColour : TColor = clNone;
    boolUpdateCursor : Boolean = True);
  Function BuildRootKey(slParams : TStringList;
    ExceptionProc : TExceptionProcedure) : String;
  Procedure TokeniseMacro(slTokens : TStringList; Const strMacro : String);
  Function ParseMacro(Const strMacro : String; var strCommand,
    strFileName : String; strCommands : Array Of String;
    ExceptionProc : TExceptionProcedure) : Boolean;
  Function Like(strPattern, strText : String) : Boolean;
  Function CalcColour(dblValue, dblLowCriteria, dblMiddleCriteria,
    dblUpperCriteria : Double; iLowColour, iMiddleColour,
    iHighColour : TColor) : TColor;

{ -------------------------------------------------------------------------

   This module defines a function that can evaluate numeric expressions and
   return the result as a double. I understand BOMAS syntax and can handle
   numerous functions.

   Functions:
     SIN, COS, TAN, ARCSIN, ARCCOS, ARCTAN, SQR, SQRT, RAD, DEG, DMS, DEC,
     EXP, LN, INV, INT, LOG, ABS, NOT, BIN, OCT, HEX, POW, AND, OR, XOR,
     DIV, MOD, SHL, and SHR

  -------------------------------------------------------------------------- }

  Function EvaluateEquation(Const strEquation :String) : Double;

ResourceString
  (** A list of bug fix letters as a string array. **)
  strBugFix = ' abcdefghijklmnopqrstuvwxyz';

Implementation

Uses
  Math, ComObj, ShlObj, IniFiles {$IFNDEF D2005}, FileCtrl {$ENDIF};

Resourcestring
  (** A resource string to define the output format of a bearing. **)
  strBearingFormat = '%d° %2.2d'' %2.2d.%2.2d"';
  (** A resource string to say that the directory was not found. **)
  strDirectoryNotFound = 'The directory "%s" does not exist.';
  (** A resource string to say the user aborted the process. **)
  strUserAbort = 'User Abort!';
  (** A resource string to say that the EXE file was not found. **)
  strEXENotFound = 'The executable file "%s" does not exist.';

Const
  (** A constant to define the type of based that can be used with number in
      eqautions. **)
  //Bases = ['B', 'O', 'H'];
  (** The numeric number in base 2 **)
  BinNums = ['-', '0', '1'];
  (** The numeric number in base 8 **)
  OctNums = BinNums + ['2', '3', '4', '5', '6', '7'];
  (** The numeric number in base 16 **)
  HexNums = OctNums + ['8', '9', 'A', 'B', 'C', 'D', 'E', 'F'];
  (** A list of valid equation delimiters. **)
  ValidDelimiters = ['(',')','*','/','+','-'];

function PathFindOnPath(pszPath: PChar; var ppszOtherDirs: PChar): BOOL; stdcall;
  external 'shlwapi' name 'PathFindOnPathA';

(**

  This routine converts the decimal bearing into a text representation,
  i.e.

  12.3456 => 12°20'44.16"

  @precon  None.
  @postcon Converts the decimal bearing into a text representation.

  @param   dblBearing as a Double
  @return  a TBearing

**)
Function DecimalToDMS(dblBearing: Double): TBearing;

Var
  strSign : String;

begin
  If dblBearing < 0 Then
    Begin
      strSign := '-';
      dblBearing := Abs(dblBearing);
    End Else
      strSign := '';
  Result.iDegrees := Trunc(dblBearing);
  Result.iMinutes := Trunc((dblBearing - Result.iDegrees) * 60);
  Result.iSeconds := Trunc(((dblBearing - Result.iDegrees) * 60 - Result.iMinutes) * 60);
  Result.iHundreds := Trunc((((dblBearing - Result.iDegrees) * 60 - Result.iMinutes) * 60
    - Result.iSeconds) * 100);
end;

(**

  This routine converts a decimal representation of a whole circle
  bearing into a decimal of a degree.

  i.e. 12.345678 (12°34'56.78") => 12.582439

  @precon  None.
  @postcon Converts a decimal representation of a whole circle
           bearing into a decimal of a degree.

  @param   dblBearingAsDec as a Double
  @return  a Double

**)
Function DMSAsDecToDecimal(dblBearingAsDec : Double): Double;

Var
  iDegs : Integer;
  iMins : Integer;
  dblSecs : Double;
  str : String;
  iPos : Integer;
  iErrCode: Integer;

begin
  str := Format('%1.6f', [dblBearingAsDec]);
  iPos := Pos('.', str);
  Val(Copy(str, 1, iPos - 1), iDegs, iErrCode);
  Val(Copy(str, iPos + 1, 2), iMins, iErrCode);
  Val(Copy(str, iPos + 3, 4), dblSecs, iErrCode);
  dblSecs := dblSecs / 100.0;
  Result := iDegs + iMins / 60.0 + dblSecs / 3600.0;
end;

(**

  This routine converts a string representation of a degree into the
  equivalent decimal, i.e. 12°34'56.78" => 12.582439

  @precon  None.
  @postcon Converts a string representation of a degree into the equivalent
           decimal.


  @param   strBearing as a String
  @return  a Double

**)
Function DMSToDecimal(strBearing: String): Double;

Var
  iDeg : Integer;
  iMin : Integer;
  iSec : Integer;

begin
  iDeg := Pos('°', strBearing);
  iMin := Pos('''', strBearing);
  iSec := Pos('"', strBearing);
  If iDeg * iMin * iSec = 0 Then
    Raise EInvalidDMSFormatException.Create(
      '"' + strBearing + '" is in an invalid DMS format.');
  Result := StrToFloat(Copy(strBearing, 1, iDeg - 1)) +
    StrToFloat(Copy(strBearing, iDeg + 1, iMin - iDeg - 1)) / 60 +
    StrToFloat(Copy(strBearing, iMin + 1, iSec - iMin - 1)) / 3600;
end;

(**

  This routine

  @precon  None.
  @postcon Adjusts agiven bearing and makes sure its within the range 0..360.

  @param   dblBearing as a Double
  @return  a Double

**)
Function AdjustBearing(dblBearing : Double) : Double;

Begin
  While dblBearing < 0 Do
    dblBearing := dblBearing + 360;
  While dblBearing > 360 Do
    dblBearing := dblBearing - 360;
  Result := dblBearing;
End;

(**

  This routine calculates the vector between the two passed coordinates
  and returns then in a TVector structure.

  @precon  None.
  @postcon Calculates the vector between the two passed coordinates
           and returns then in a TVector structure.

  @param   dblStE  as a Double
  @param   dblStN  as a Double
  @param   dblEndE as a Double
  @param   dblEndN as a Double
  @return  a TVector

**)
Function GetVector(dblStE, dblStN, dblEndE, dblEndN : Double) : TVector;

Begin
  Result.dblDistance := Sqrt(Sqr(dblEndE - dblStE) + Sqr(dblEndN - dblStN));
  If Result.dblDistance = 0 Then
    Result.dblBearing := 0
  Else
    Try
      Result.dblBearing := RadToDeg(ArcCos((dblEndN - dblStN) / Result.dblDistance));
    Except
      Result.dblBearing := 0;
    End;
  If dblEndE - dblStE < 0 Then
    Result.dblBearing := -Result.dblBearing;
End;

(**

  This routine reduces the given bearing to the range -180..+180.

  @precon  None.
  @postcon Reduces the given bearing to the range -180..+180.

  @param   dblBearing as a Double
  @return  a Double

**)
Function ReduceBearing(dblBearing : Double) : Double;

Begin
  Result := dblBearing;
  If dblBearing > 180 Then
    Result := dblBearing - 360;
End;

(**

  This routine returns the number of occurrances of the char found in the
  string.

  @precon  None.
  @postcon Returns the number of occurrances of the char found in the string.

  @param   cChar   as a Char
  @param   strText as a String
  @return  an Integer

**)
Function CharCount(cChar : Char; strText : String) : Integer;

Var
  iCount : Integer;

Begin
  Result := 0;
  For iCount := 1 to Length(strText) Do
    If strText[iCount] = cChar Then
      Inc(Result);
End;


(**

  This routine returns the position of the Nth occurrance of the character
  in the text.

  @precon  None.
  @postcon Returns the position of the Nth occurrance of the character
           in the text.

  @param   strText as a String
  @param   Ch      as a Char
  @param   iIndex  as an Integer
  @return  an Integer

**)
Function PosOfNthChar(strText : String; Ch : Char; iIndex : Integer): Integer;

Var
  i : Integer;
  iCount : Integer;

Begin
  Result := 0;
  iCount := 0;
  For i := 1 To Length(strText) Do
    Begin
      If strText[i] = Ch Then
        Inc(iCount);
      If iIndex = iCount Then
        Begin
          Result := i;
          Exit;
        End;
    End;
End;

(**

  This function returns the contents of the specified field in the
  delimited text.

  @precon  None.
  @postcon Returns the contents of the specified field in the delimited text.

  @param   strText as a String
  @param   Ch      as a Char
  @param   iIndex  as an Integer
  @return  a String

**)
Function GetField(strText : String; Ch : Char; iIndex : Integer): String;

Var
  iNumOfFields : Integer;
  iStart, iEnd : Integer;

Begin
  iNumOfFields := CharCount(Ch, strText) + 1;
  If iIndex = 1 Then
    Begin
      If iNumOfFields > 1  Then
        Begin
          iEnd := Pos(Ch, strText);
          Result := Copy(strText, 1, iEnd - 1);
        End Else
          Result := strText;
      Exit;
    End;
  If (iIndex > 1) And (iIndex < iNumOfFields) Then
    Begin
      iStart := PosOfNthChar(strText, Ch, iIndex - 1);
      iEnd := PosOfNthChar(strText, Ch, iIndex);
      Result := Copy(strText, iStart + 1, iEnd - iStart - 1);
      Exit;
    End;
  If iIndex = iNumOfFields Then
    Begin
      iStart := PosOfNthChar(strText, Ch, iIndex - 1);
      Result := Copy(strText, iStart + 1, Length(strText) - iStart);
      Exit;
    End;
End;

(**

  This function returns the filename without the extension.

  @precon  None.
  @postcon Returns the filename without the extension.


  @param   strFileName as a String
  @return  a String

**)
Function ExtractFileNameOnly(strFileName : String) : String;

Begin
  Result := ExtractFileName(strFileName);
  Result := Copy(Result, 1, Pos('.', Result) - 1);
End;

(**

  This routine creates a file association in the registry for the given
  file extension, class and description.

  @precon  None.
  @postcon Creates a file association in the registry for the given file
           extension, class and description.

  @param   strFileExt     as a String as a constant
  @param   strFileClass   as a String as a constant
  @param   strDescription as a String as a constant
  @param   strDefaultIcon as a String as a constant
  @param   bAlwaysShowExt as a Boolean
  @param   bQuickView     as a Boolean

**)
Procedure MakeAssociation(Const strFileExt, strFileClass, strDescription,
  strDefaultIcon : String; bAlwaysShowExt, bQuickView : Boolean);

Begin
  If (Length(strFileExt) = 0) Or (strFileExt[1] <> '.') Then
    Raise Exception.Create('Invalid file extension');
  CreateRegKey(strFileExt, '', strFileClass);
  CreateRegKey(strFileClass, '', strDescription);
  If strDefaultIcon <> '' Then
    CreateRegKey(strFileClass + '\DefaultIcon', '', strDefaultIcon);
  If bAlwaysShowExt Then
    CreateRegKey(strFileClass, 'AlwaysShowExt', '');
  If bQuickView Then
    CreateRegKey(strFileClass + '\QuickView', '', '*');
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, Nil, Nil);
End;

(**

  This routine registers verb for the explorer context menu, i.e. open,
  print.

  @precon  None.
  @postcon Registers verb for the explorer context menu, i.e. open, print.


  @param   strFileExt         as a String as a constant
  @param   strVerb            as a String as a constant
  @param   strVerbCaption     as a String as a constant
  @param   strVerbCommand     as a String as a constant
  @param   bUseDDE            as a Boolean
  @param   strService         as a String  as a constant
  @param   strTopic           as a String  as a constant
  @param   strMacro           as a String  as a constant
  @param   strMacroNotRunning as a String  as a constant

**)
Procedure MakeAssociatedVerb(const strFileExt, strVerb, strVerbCaption,
  strVerbCommand : String; bUseDDE : Boolean; Const strService, strTopic,
  strMacro, strMacroNotRunning : String);

Var
  strfileClass : String;

Begin
  If (Length(strFileExt) = 0) Or (strFileExt[1] <> '.') Then
    Raise Exception.Create('Invalid file extension');
  strFileClass := GetRegStringValue(strFileExt, '');
  If strFileClass = '' Then
    Raise Exception.Create('File extension not registered.');
  CreateRegKey(strFileClass + '\shell\' + strVerb, '', strVerbCaption);
  CreateRegKey(strFileClass + '\shell\' + strVerb + '\command', '', strVerbCommand);
  If bUseDDE Then
    Begin
      CreateRegKey(strFileClass + '\shell\' + strVerb + '\ddeexec', '', strMacro);
      CreateRegKey(strFileClass + '\shell\' + strVerb + '\ddeexec\Application',
        '', strService);
      CreateRegKey(strFileClass + '\shell\' + strVerb + '\ddeexec\Topic', '',
        strTopic);
      If strMacroNotRunning <> '' Then
        CreateRegKey(strFileClass + '\shell\' + strVerb + '\ddeexec\ifexec',
          '', strMacroNotRunning);
    End;
  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, Nil, Nil);
End;

(**

  This function capitalises the give next text.

  @precon  None.
  @postcon Capitalises the give next text.

  @param   strText as a String
  @return  a String

**)
Function Capitalise(strText : String) : String;

Const
  strChars : Set Of Char = ['a'..'z', 'A'..'Z'];

Var
  i : Integer;

Begin
  Result := strText;
  If Length(Result) > 0 Then
    Result[1] := Upcase(Result[1]);
  For i := 2 To Length(Result) Do
    If Not (Result[i - 1] In strChars) Then
      Result[i]:= Upcase(Result[i]);
End;

(**

  This function returns true if the given word is in the supplied word list. It
  uses a binary search, so the word lists need to be sorted.

  @precon  strWord is the word to be searches for in the word list and
           strWordList is a static array of words in lowercase and alphabetical
           order.
  @postcon Returns true if the word is found in the list.

  @param   strWord     as a String
  @param   strWordList as an Array Of String
  @return  a Boolean

**)
function IsKeyWord(strWord : String; strWordList : Array Of String): Boolean;

Var
  l, m, r : Integer;
  str : String;

begin
  Result := False;
  str := LowerCase(strWord);
  l := Low(strWordList);
  r := High(strWordList);
  While l <= r Do
    Begin
      m := (l + r) Div 2;
      If strWordList[m] < str Then
        l := Succ(m)
      Else If strWordList[m] > str Then
        r:= Pred(m)
      Else
        Begin
          Result := True;
          Exit;
        End;
    End;
end;

(**

  This function converts a freeform text string representing dates and times
  in standard formats in to a TDateTime value.

  @precon  strDate is the string to convert into a date.
  @postcon Returns a valid TDateTime value.

  @param   strDate as a String as a Constant
  @return  a TDateTime

**)
Function ConvertDate(Const strDate : String) : TDateTime;

Type
  (** This is a record that defined the date and time for a date. **)
  TDateRec = Record
    iDay, iMonth, iYear, iHour, iMinute, iSecond, iMilli : Word;
  End;

Const
  strErrMsg = 'Can not convert the date "%s" to a valid TDateTime value.';
  Delimiters : Set Of Char = ['-', ' ', '\', '/', ':', '.'];
  Days : Array[1..7] Of String = ('fri', 'mon', 'sat', 'sun', 'thu', 'tue', 'wed');
  Months : Array[1..24] Of String = (
    'apr', 'april',
    'aug', 'august',
    'dec', 'december',
    'feb', 'february',
    'jan', 'january',
    'jul', 'july',
    'jun', 'june',
    'mar', 'march',
    'may', 'may',
    'nov', 'november',
    'oct', 'october',
    'sep', 'september'
    );
  MonthIndexes : Array[1..24] Of Word = (
    4, 4,
    8, 8,
    12, 12,
    2, 2,
    1, 1,
    7, 7,
    6, 6,
    3, 3,
    5, 5,
    11, 11,
    10, 10,
    9, 9
  );

Var
  i : Integer;
  sl : TStringList;
  strToken : String;
  iTime : Integer;
  recDate : TDateRec;
  tmp : Word;
  iIndex0, iIndex1, iIndex2 : Integer;

  (**

    This procedure adds the token to the specified string list and clears the
    token.

    @precon  StringList is the string list to add the token too and strToken is
             the token to add to the list.
    @postcon Adds the token to the specified string list and clears the
             token.

    @param   StringList as a TStringList
    @param   strToken   as a String as a reference

  **)
  Procedure AddToken(StringList : TStringList; var strToken  : String);

  Begin
    If strToken <> '' Then
      Begin
        StringList.Add(strToken);
        strToken := '';
      End;
  End;

  (**

    This procedure tries to extract the value from the indexed string list
    item into the passed variable reference. It delete is true it remove the
    item from the string list.

    @precon  iIndex is the index of the item from the string list to extract,
             iValue is a word variable to place the converted item into and
             Delete determines whether the item is removed from the string list.
    @postcon Tries to extract the value from the indexed string list item into
             the passed variable reference. It delete is true it remove the
             item from the string list.

    @param   iIndex as an Integer
    @param   iValue as a Word as a reference
    @param   Delete as a Boolean

  **)
  Procedure ProcessValue(iIndex : Integer; var iValue : Word; Delete : Boolean);

  Begin
    If iIndex > sl.Count - 1 Then Exit;
    Val(sl[iIndex], iValue, i);
    If i <> 0 Then
      Raise EDateConversionException.CreateFmt(strErrMsg, [strDate]);
    If Delete Then
      sl.Delete(iIndex);
  End;

  (**

    This procedure assigns string list indexes to the three index values
    according to the short date format and what information is supplied.

    @precon  None.
    @postcon Assigns string list indexes to the three index values
             according to the short date format and what information is
             supplied.

  **)
  Procedure AssignIndexes();

  Var
    slFormat : TStringList;
    str : String;
    j : Integer;

  Begin
    iIndex0 := 0; // Default Day / Month / Year
    iIndex1 := 1;
    iIndex2 := 2;
    slFormat := TStringList.Create;
    Try
      str := '';
      For j := 1 To Length(ShortDateFormat) Do
        If ShortDateFormat[j] In Delimiters Then
          AddToken(slFormat, str)
        Else
          str := str + ShortDateFormat[j];
      AddToken(slFormat, str);
      // Remove day of week
      For j := slFormat.Count - 1 DownTo 0 Do
        If (slFormat[j][1] In ['d', 'D']) And (Length(slFormat[j]) > 2) Then
          slFormat.Delete(j);
      For j := 0 To slFormat.Count - 1 Do
        Begin
          If slFormat[j][1] In ['d', 'D'] Then iIndex0 := j;
          If slFormat[j][1] In ['m', 'M'] Then iIndex1 := j;
          If slFormat[j][1] In ['y', 'Y'] Then iIndex2 := j;
        End;
    Finally
      slFormat.Free;
    End;
  End;

Begin
  Result := 0;
  sl := TStringList.Create;
  Try
    strToken := '';
    iTime := -1;
    For i := 1 To Length(strDate) Do
      If strDate[i] In Delimiters Then
        Begin
          AddToken(sl, strToken);
          If (strDate[i] In [':']) And (iTime = -1) Then iTime := sl.Count - 1;
        End Else
          strToken := strToken + strDate[i];
    AddToken(sl, strToken);
    FillChar(recDate, SizeOf(recDate), 0);
    // Decode time
    If iTime > -1 Then
      Begin
        ProcessValue(iTime,recDate.iHour, True);
        ProcessValue(iTime,recDate.iMinute, True);
        ProcessValue(iTime,recDate.iSecond, True);
        ProcessValue(iTime,recDate.iMilli, True);
      End;
    // Remove day value if present
    For i := sl.Count - 1 DownTo 0 Do
      If IsKeyWord(sl[i], Days) Then
        sl.Delete(i);
    // Decode date
    Case sl.Count Of
      1 :
        Begin
          DecodeDate(Now, recDate.iYear, recDate.iMonth, tmp);
          ProcessValue(0, recDate.iDay, False); // Day only
        End;
      2, 3 : // Day and Month (Year)
        Begin
          DecodeDate(Now, recDate.iYear, tmp, tmp);
          AssignIndexes;
          ProcessValue(iIndex0, recDate.iDay, False); // Get day
          If IsKeyWord(sl[iIndex1], Months) Then
            Begin
              For i := Low(Months) To High(Months) Do
                If SysUtils.AnsiCompareText(Months[i], sl[iIndex1]) = 0 Then
                  Begin
                    recDate.iMonth := MonthIndexes[i];
                    Break;
                  End;
            End Else
              ProcessValue(iIndex1, recDate.iMonth, False); // Get Month
            If sl.Count = 3 Then
              Begin
                ProcessValue(iIndex2, recDate.iYear, False); // Get Year
                If recDate.iYear < 1900 Then Inc(recDate.iYear, 2000);
              End;
        End;
    Else
      If sl.Count <> 0 Then
        Raise EDateConversionException.CreateFmt(strErrMsg, [strDate]);
    End;
    // Output result.
    With recDate Do
      Begin
        If Not (iHour In [0..23]) Then
          Raise EDateConversionException.CreateFmt(strErrMsg, [strDate]);
        If Not (iMinute In [0..59]) Then
          Raise EDateConversionException.CreateFmt(strErrMsg, [strDate]);
        If Not (iSecond In [0..59]) Then
          Raise EDateConversionException.CreateFmt(strErrMsg, [strDate]);
        Result := EncodeTime(iHour, iMinute, iSecond, iMilli);
        If iYear * iMonth * iDay <> 0 Then
          Begin
            If Not (iDay In [1..31]) Then
              Raise EDateConversionException.CreateFmt(strErrMsg, [strDate]);
            If Not (iMonth In [1..12]) Then
              Raise EDateConversionException.CreateFmt(strErrMsg, [strDate]);
            Result := Result + EncodeDate(iYear, iMonth, iDay);
          End;
      End;
  Finally
    sl.Free;
  End;
End;

(**

  This function converts a base 8 number to a base 2 (binary) number.

  @precon  The given number must be in base 8 else an exception is raised.
  @postcon The returned number is in base 2 (binary).

  @param   sDisplayNumber as a String
  @return  a String

**)
Function OctToBin(sDisplayNumber : String) : String;

Begin
  OctToBin := DecToBin(OctToDec(sDisplayNumber));
End;

(**

  This function converts a base 16 number to base 2 (binary).

  @precon  The given number must be in base 8 else an exception is raised.
  @postcon The returned number is in base 2 (binary).

  @param   sDisplayNumber as a String
  @return  a String

**)
Function HexToBin(sDisplayNumber : String) : String;

Begin
  HexToBin := DecToBin(HexToDec(sDisplayNumber));
End;

(**

  This function converts a base 10 (decimal) number to base 2 (binary).

  @precon  The number must be a decimal floating point number else a exception is raised.
  @postcon the returned number is in base 2 (binary)

  @param   sDisplayNumber as a String
  @return  a String

**)
Function DecToBin(sDisplayNumber : String) : String;

Var
  iRemainder : Integer;
  iNumber : Integer;
  sReturnString : String;

Begin
  iNumber := Trunc(StrToFloat(sDisplayNumber));
  If iNumber = 0 Then
    sReturnString := '0'
  Else
    sReturnString := '';
  While iNumber <> 0 Do
    Begin
      iRemainder := iNumber Mod 2;
      iNumber := iNumber Div 2;
      sReturnString := IntToStr(iRemainder) + sReturnString;
    End;
  DecToBin := sReturnString;
End;

(**

  This function converts a binary base 2 number to base 8.

  @precon  The given number must be in base 2 else an exception is raised.
  @postcon The returned number is in base 8.

  @param   sDisplayNumber as a String
  @return  a String

**)
Function BinToOct(sDisplayNumber : String) : String;

Begin
  BinToOct := DecToOct(BinToDec(sDisplayNumber));
End;

(**

  This function converts a base 16 number to base 8.

  @precon  The given number must be in base 16 else an exception is raised.
  @postcon The returned number is in base 8.

  @param   sDisplayNumber as a String
  @return  a String

**)
Function HexToOct(sDisplayNumber : String) : String;

Begin
  HexToOct := DecToOct(HexToDec(sDisplayNumber));
End;

(**

  This function converts a decimal base 10 number into base 8.

  @precon  The given number must be in base 10 else an exception is raised.
  @postcon The returned number is in base 8.

  @param   sDisplayNumber as a String
  @return  a String

**)
Function DecToOct(sDisplayNumber : String) : String;

Var
  iRemainder : Integer;
  iNumber : Integer;
  sReturnString : String;

Begin
  iNumber := Trunc(StrToFloat(sDisplayNumber));
  sReturnString := '';
  While iNumber <> 0 Do
    Begin
      iRemainder := iNumber Mod 8;
      iNumber := iNumber Div 8;
      sReturnString := IntToStr(iRemainder) + sReturnString;
    End;
  DecToOct := sReturnString;
End;

(**

  This function converts a binary base 2 number into a base 16 (hexidecimal) number.

  @precon  The given number must be in binary else an exception is raised.
  @postcon The returned number is on bse 16.

  @param   sDisplayNumber as a String
  @return  a String

**)
Function BinToHex(sDisplayNumber : String) : String;

Begin
  BinToHex := DecToHex(BinToDec(sDisplayNumber));
End;

(**

  This function converts as base 8 number to base 16.

  @precon  The given number must be in base 8.
  @postcon The returned number is in base 16.

  @param   sDisplayNumber as a String
  @return  a String

**)
Function OctToHex(sDisplayNumber : String) : String;

Begin
  OctToHex := DecToHex(OctToDec(sDisplayNumber));
End;

(**

  This function converts as decimal number into a hexidecimal number (base 16).

  @precon  The given number must be in base 10 else an exception is raised.
  @postcon The returned number is in base 16.

  @param   sDisplayNumber as a String
  @return  a String

**)
Function DecToHex(sDisplayNumber : String) : String;

Begin
  DecToHex := IntToHex(Trunc(StrToFloat(sDisplayNumber)),0);
End;

(**

  This function converts a binary number (base 2) into a decimal number.

  @precon  The given number must be in base 2 else an exception is raised.
  @postcon The returned number is in base 10 (decimal).

  @param   sDisplayNumber as a String
  @return  a String

**)
Function BinToDec(sDisplayNumber : String) : String;

Var
  iCount : Integer;
  iResult : Integer;
  iPower : Integer;
  sReturnString : String;
  bNegative : Boolean;

Begin
  bNegative := False;
  For iCount := 1 To Length(sDisplayNumber) Do
    If Not (sDisplayNumber[iCount] In BinNums) Then
      Raise Exception.Create('Invalid token (' + sDisplayNumber[iCount] +
        ') in binary number [' + sDisplayNumber + '].');
  sReturnString := '';
  iPower := 0;
  iResult := 0;
  For iCount := Length(sDisplayNumber) DownTo 1 Do
    Begin
      If sDisplayNumber[iCount] <> '-' Then
        Begin
          iResult := iResult + SysUtils.StrToInt(sDisplayNumber[iCount]) *
            Trunc(Power(2, iPower));
          Inc(iPower);
        End Else
          bNegative := True;
    End;
  If bNegative Then
    BinToDec := IntToStr(-iResult)
  Else
    BinToDec := IntToStr(iResult);
End;

(**

  This function converts a base 8 number into a decimal number.

  @precon  The given number must be a base 8 number else an exception is raised.
  @postcon The returned number is a base 10 (decimal) number.

  @param   sDisplayNumber as a String
  @return  a String

**)
Function OctToDec(sDisplayNumber : String) : String;

Var
  iCount : Integer;
  iResult : Integer;
  iPower : Integer;
  sReturnString : String;
  bNegative : Boolean;

Begin
  bNegative := False;
  For iCount := 1 To Length(sDisplayNumber) Do
    If Not (sDisplayNumber[iCount] In OctNums) Then
      Raise Exception.Create('Invalid token (' + sDisplayNumber[iCount] +
        ') in octal number [' + sDisplayNumber + '].');
  sReturnString := '';
  iPower := 0;
  iResult := 0;
  For iCount := Length(sDisplayNumber) DownTo 1 Do
    Begin
      If sDisplayNumber[iCount] <> '-' Then
        Begin
          iResult := iResult + SysUtils.StrToInt(sDisplayNumber[iCount]) *
            Trunc(Power(8, iPower));
          Inc(iPower);
        End Else
          bNegative := True
    End;
  If bNegative Then
    OctToDec := IntToStr(-iResult)
  Else
    OctToDec := IntToStr(iResult);
End;

(**

  This function converts a base 16 number into a decimal number.

  @precon  The given number must be a base 16 number else an exception is raised.
  @postcon The returned number is a base 10 (decimal) number.

  @param   sDisplayNumber as a String
  @return  a String

**)
Function HexToDec(sDisplayNumber : String) : String;

Var
  iCount : Integer;
  iResult : Integer;
  iPower : Integer;
  iNumber : Integer;
  sReturnString : String;
  bNegative : Boolean;

Begin
  bNegative := False;
  For iCount := 1 To Length(sDisplayNumber) Do
    If Not (sDisplayNumber[iCount] In HexNums) Then
      Raise Exception.Create('Invalid token (' + sDisplayNumber[iCount] +
        ') in hexidecimal number [' + sDisplayNumber + '].');
  sReturnString := '';
  iPower := 0;
  iResult := 0;
  For iCount := Length(sDisplayNumber) DownTo 1 Do
    Begin
      If sDisplayNumber[iCount] <> '-' Then
        Begin
          Case sDisplayNumber[iCount] Of
            '0'..'9' : iNumber := SysUtils.StrToInt(sDisplayNumber[iCount]);
            'A'..'F' : iNumber := Word(sDisplayNumber[iCount]) - Word('A') + 10;
          Else
            iNumber := 0;
          End;
          iResult := iResult + iNumber * Trunc(Power(16, iPower));
          Inc(iPower);
        End Else
          bNegative := True;
    End;
  If bNegative Then
    HexToDec := IntToStr(-iResult)
  Else
    HexToDec := IntToStr(iResult);
End;

(**

  This function raises the number x to the power of y.

  @precon  X can not be 0 else a exception is raised.
  @postcon The x to the power y is returned.

  @param   X as a Real
  @param   Y as a Real
  @return  a Real

**)
Function Pow(X, Y : Real) : Real;

Begin
  If X = 0 Then
    Begin
      Raise EMathException.Create('Invalid number passed to Power Routine.');
      Pow := 0;
      Exit;
    End;
  If X < 0 Then
    Pow := - Exp(Y * Ln(Abs(X)))
  Else
    Pow := Exp(Y * Ln(X));
End;

(**

  This routine finds the inner most pair of parenthesis and returns the
  start and end positions

  @precon  Equation is the equation to parse, PStart return the start of the inner most
           section and PEnd return the end of the inner most section.
  @postcon Returns the evaluation of the defined inner most section of the equation.

  @param   Equation as a String
  @param   PStart   as an Integer as a reference
  @param   PEnd     as an Integer as a reference

**)
Procedure FindParenthesis(Equation : String; var PStart, PEnd : Integer);

Var
  I, J : Integer;

Begin
  For I := 1 To Length(Equation) Do
    Begin
      If Equation[I] = '(' Then
        Begin
          PStart := I;
          For J := I + 1 To Length(Equation) Do
            Begin
              If Equation[J] = '(' Then
                Break;
              If Equation[J] = ')' Then
                Begin
                  PEnd := J;
                  Exit;
                End;
            End;
        End;
    End;
End;

(**

  This is the main routine for breaking down the equation in to pieces and
  evaluate those piece and substituting the result back into the eqaution
  until all expressions have been calculated.

  @precon  None.
  @postcon Returns the evaluation of the given equation else raises an error.

  @param   EquStr as a String
  @return  a String

**)
Function Evaluate(EquStr : String) : String;

Const
  Delimiters = ['*','/','+','-',','];
  Exponents = ['E','e'];

Var
  I, J, K : Integer;
  ParamStack : Array[1..52] Of Double;
  DelimiterStack : Array[1..52] Of Char;
  ParamCounter: Integer;
  DelimiterCounter : Integer;
  SearchStart : Integer;
  TempStr : String;
  ConTempStr : String;
  TempReal : Double;
  ErrorCode : Integer;

Begin
  ConTempStr := '';
  ParamCounter := 0;
  DelimiterCounter := 0;
  SearchStart := 1;
  For I := 1 To 52 Do
    Begin
      ParamStack[I] := 0;
      DelimiterStack[I] := '?';
    End;
  K := 2;
  For I := 2 To Length(EquStr) Do
    Begin
      K := I;
      If (EquStr[I] In Delimiters) And (Not (EquStr[I-1] In Exponents))
        And (Not (EquStr[I-1] In Delimiters )) Then
        Begin
          If DelimiterCounter = 49 Then
            Raise Exception.Create('Expression too long to evaluate.');
          Inc(DelimiterCounter);
          DelimiterStack[DelimiterCounter] := EquStr[I];
          TempStr := Copy(EquStr,SearchStart,I-SearchStart);
          Val(TempStr,TempReal,ErrorCode);
          If TempStr = '' Then
            TempStr := '0';
          If TempStr = 'PI' Then
            TempReal := Pi
          Else If ErrorCode <> 0 Then
            If (TempStr<> '') And (Tempstr[Length(TempStr)] = 'B') Then
              Val(BinToDec(Copy(TempStr, 1, Length(TempStr) - 1)), TempReal, Errorcode)
            Else If (TempStr<> '') And (Tempstr[Length(TempStr)] = 'O') Then
              Val(OctToDec(Copy(TempStr, 1, Length(TempStr) - 1)), TempReal, Errorcode)
            Else If (TempStr<> '') And (Tempstr[Length(TempStr)] = 'H') Then
              Val(HexToDec(Copy(TempStr, 1, Length(TempStr) - 1)), TempReal, Errorcode)
            Else
              Raise Exception.Create('Invalid Real Number.');
          Inc(ParamCounter);
          ParamStack[ParamCounter] := TempReal;
          SearchStart := I+1;
        End;
    End;
  If K = 0 Then
    I := 2
  Else
    I := K;
  TempStr := Copy(EquStr,SearchStart,I-SearchStart+1);
  Val(TempStr,TempReal,ErrorCode);
  If TempStr = 'PI' Then
    TempReal := Pi
  Else If ErrorCode <> 0 Then
    If (TempStr<> '') And (TempStr[Length(TempStr)] = 'B') Then
      Val(BinToDec(Copy(TempStr, 1, Length(TempStr) - 1)), TempReal, Errorcode)
    Else If (TempStr<> '') And (Tempstr[Length(TempStr)] = 'O') Then
      Val(OctToDec(Copy(TempStr, 1, Length(TempStr) - 1)), TempReal, Errorcode)
    Else If (TempStr<> '') And (Tempstr[Length(TempStr)] = 'H') Then
      Val(HexToDec(Copy(TempStr, 1, Length(TempStr) - 1)), TempReal, Errorcode)
    Else
      Raise Exception.Create('Invalid Real Number.');
  Inc(ParamCounter);
  ParamStack[ParamCounter] := TempReal;
  For I := 1 To DelimiterCounter Do
    While (DelimiterStack[I] = '*') Or (DelimiterStack[I] = '/') Do
      Begin
        If DelimiterStack[I] = '*' Then
          ParamStack[I] := ParamStack[I] * ParamStack[I+1]
        Else
          Begin
            If ParamStack[I+1] = 0 Then
              Raise Exception.Create('Divide by Zero.');
            ParamStack[I] := ParamStack[I] / ParamStack[I+1];
          End;
        For J := I+1 To ParamCounter Do
          ParamStack[J] := ParamStack[J+1];
        Dec(ParamCounter);
        For J := I To DelimiterCounter Do
          DelimiterStack[J] := DelimiterStack[J+1];
        Dec(DelimiterCounter);
      End;
  For I := 1 To DelimiterCounter Do
    While (DelimiterStack[I] = '+') Or (DelimiterStack[I] = '-') Do
      Begin
        If DelimiterStack[I] = '+' Then
          ParamStack[I] := ParamStack[I] + ParamStack[I+1]
        Else
          ParamStack[I] := ParamStack[I] - ParamStack[I+1];
        For J := I+1 To ParamCounter Do
            ParamStack[J] := ParamStack[J+1];
        Dec(ParamCounter);
        For J := I To DelimiterCounter Do
          DelimiterStack[J] := DelimiterStack[J+1];
        Dec(DelimiterCounter);
      End;
  If DelimiterCounter > 1 Then
    Raise Exception.Create('Error during parsing.');
  If (DelimiterCounter = 1) And (DelimiterStack[1] = ',') Then
    Begin
      Str(ParamStack[1],TempStr);
      ConTempStr := ConTempStr + TempStr + DelimiterStack[1];
      Str(ParamStack[2],TempStr);
      TempStr := ConTempStr + TempStr;
    End Else
      Str(ParamStack[1],TempStr);
  While Pos(' ',TempStr) <> 0 Do
    Delete(TempStr,Pos(' ',TempStr),1);
  Evaluate := TempStr;
End;

(**

  If a function is suspected then the routine tries to evaluate the
  function

  @precon  Funct is the name of the function to undertaken in number Number.
  @postcon The result if the function us returned else an exception is raised.

  @param   Funct  as a String
  @param   Number as a String
  @return  a Double

**)
Function GetFunction(Funct, Number : String) : Double;

Var
  RealNumber : Double;
  iErrorCode : Integer;
  X, Y : Double;
  recBearing : TBearing;

Begin
  Val(Number, RealNumber, iErrorCode);
  If iErrorCode = 0 Then
    Begin
      If Funct = 'SIN' Then
        Result := Sin(RealNumber)
      Else If Funct = 'COS' Then
        Result := Cos(RealNumber)
      Else If Funct = 'TAN' Then
        Result := Tan(realNumber)
      Else If Funct = 'ARCSIN' Then
        Result := ArcSin(RealNumber)
      Else If Funct = 'ARCCOS' Then
        Result := ArcCos(RealNumber)
      Else If Funct = 'ARCTAN' Then
        Result := ArcTan(RealNumber)
      Else If Funct = 'SQR' Then
        Result := Sqr(RealNumber)
      Else If Funct = 'SQRT' Then
        Result := Sqrt(RealNumber)
      Else If Funct = 'RAD' Then
        Result := DegToRad(RealNumber)
      Else If Funct = 'DEG' Then
        Result := RadToDeg(RealNumber)
      Else If Funct = 'DMS' Then
        Begin
          recBearing := DecimalToDMS(RealNumber);
          Val(Format('%d.%2.2d%2.2d%2.2d', [recBearing.iDegrees,
            recBearing.iMinutes, recBearing.iSeconds, recBearing.iHundreds]), Result,
            iErrorCode);
        End
      Else If Funct = 'DEC' Then
        Result := DMSAsDecToDecimal(RealNumber)
      Else If Funct = 'EXP' Then
        Result := Exp(RealNumber)
      Else If Funct = 'LN' Then
        Result := LN(RealNumber)
      Else If Funct = 'INV' Then
        Result := 1.0 / RealNumber
      Else If Funct = 'INT' Then
        Result := Int(RealNumber)
      Else If Funct = 'LOG' Then
        Result := Log10(RealNumber)
      Else If Funct = 'ABS' Then
        Result := Abs(RealNumber)
      Else If Funct = 'NOT' Then
        Result := Not Trunc(RealNumber)
      Else
        Raise Exception.CreateFmt('Invalid function [%s].', [Funct])
    End Else
      If Funct = 'POW' Then
        Begin
          Val(Copy(Number,1,Pos(',', Number) - 1), X, iErrorCode);
          Val(Copy(Number,Pos(',', Number) + 1,Length(Number) - Pos(',', Number)), Y, iErrorCode);
          Result := Pow(X,Y);
        End
      Else If Funct = 'AND' Then
        Begin
          Val(Copy(Number,1,Pos(',',Number)-1), X, iErrorCode);
          Val(Copy(Number,Pos(',',Number)+1,Length(Number)-Pos(',',Number)), Y, iErrorCode);
          Result := (Trunc(X) And Trunc(Y));
        End
      Else If Funct = 'OR' Then
        Begin
          Val(Copy(Number,1,Pos(',',Number)-1), X, iErrorCode);
          Val(Copy(Number,Pos(',',Number)+1,Length(Number)-Pos(',',Number)), Y, iErrorCode);
          Result := (Trunc(X) Or Trunc(Y));
        End
      Else If Funct = 'XOR' Then
        Begin
          Val(Copy(Number,1,Pos(',',Number)-1), X, iErrorCode);
          Val(Copy(Number,Pos(',',Number)+1,Length(Number)-Pos(',',Number)), Y, iErrorCode);
          Result := (Trunc(X) Xor Trunc(Y));
        End
      Else If Funct = 'DIV' Then
        Begin
          Val(Copy(Number,1,Pos(',',Number)-1), X, iErrorCode);
          Val(Copy(Number,Pos(',',Number)+1,Length(Number)-Pos(',',Number)), Y, iErrorCode);
          Result := (Trunc(X) Div Trunc(Y));
        End
      Else If Funct = 'MOD' Then
        Begin
          Val(Copy(Number,1,Pos(',',Number)-1), X, iErrorCode);
          Val(Copy(Number,Pos(',',Number)+1,Length(Number)-Pos(',',Number)), Y, iErrorCode);
          Result := (Trunc(X) Mod Trunc(Y));
        End
      Else If Funct = 'SHL' Then
        Begin
          Val(Copy(Number,1,Pos(',',Number)-1), X, iErrorCode);
          Val(Copy(Number,Pos(',',Number)+1,Length(Number)-Pos(',',Number)), Y, iErrorCode);
          Result := (Trunc(X) Shl Trunc(Y));
        End
      Else If Funct = 'SHR' Then
        Begin
          Val(Copy(Number,1,Pos(',',Number)-1), X, iErrorCode);
          Val(Copy(Number,Pos(',',Number)+1,Length(Number)-Pos(',',Number)), Y, iErrorCode);
          Result := (Trunc(X) Shr Trunc(Y));
        End Else
          Raise Exception.CreateFmt('Invalid function [%s].', [Funct]);
End;


(**

  This function determine whether there is a delimiter in the given string.

  @precon  None.
  @postcon Returns true if a delimiter is found.

  @param   sString as a String
  @return  a Boolean

**)
Function FindDelimiter(sString : String) : Boolean;

Var
  iCount : Integer;

Begin
  FindDelimiter := False;
  For iCount := 1 To Length(sString) Do
    If sString[iCount] In ValidDelimiters Then
      Begin
        FindDelimiter := True;
        Exit;
      End;
End;

(**

  This is the main entry point, pass a string representing an express and
  a doube value is returned.

  @precon  None.
  @postcon Evaluates the given equation and returns the result as a double.

  @param   strEquation as a String as a constant
  @return  a Double

**)
Function EvaluateEquation(Const strEquation :String) : Double;

Var
  i : Integer;
  iParNum : Integer;
  iPStart : Integer;
  iPEnd : Integer;
  iFStart : Integer;
  strCurEquation : String;
  tmpStr1 : String;
  str2 : String;
  str3 : String;
  strFunc : String;
  strFunction : String;

Begin
  strCurEquation := UpperCase(strEquation);
  If strCurEquation = '' Then
    Raise Exception.Create('Can not evaluate <null>');
  { Remove spaces }
  While Pos(' ', strCurEquation) <> 0 Do
    Delete(strCurEquation, Pos(' ', strCurEquation),1);
  { Check the Parethesis }
  iParNum := 0;
  For I := 1 To Length(strCurEquation) Do
    Case strCurEquation[I] Of
      '(' : Inc(iParNum);
      ')' : Dec(iParNum);
    End;
  If iParNum <> 0 Then
    Raise Exception.Create('Parenthesis do not match.');
  Repeat
    iPStart := 0;
    iPEnd := 0;
    tmpStr1 := '';
    str2 := '';
    str3 := '';
    FindParenthesis(strCurEquation, iPStart, iPEnd);
    If iPEnd > iPStart Then
      Begin
        (*
          Extract and evaluate the inside of the parenthesis
        *)
        For I := iPStart + 1 To iPEnd - 1 Do
          tmpStr1 := tmpStr1 + strCurEquation[I];
        Delete(strCurEquation, iPStart, iPEnd - iPStart + 1);
        str2 := Evaluate(tmpStr1);

        If ((iPStart - 1 <= Length(strCurEquation)) And (iPStart - 1 > 0)) And
          Not (strCurEquation[iPStart - 1] In ValidDelimiters) And (iPStart <> 1) Then
          Begin
            iFStart := iPStart - 1;
            strFunc := '';
            While (iFStart > 0) And Not (strCurEquation[iFStart] In ValidDelimiters) Do
              Begin
                strFunc := strCurEquation[iFStart] + strFunc;
                Dec(iFStart)
              End;
            str3 := FloatToStr(GetFunction(strFunc, str2));
            Delete(strCurEquation, iFStart + 1,(iPStart - iFStart) - 1);
            str2 := str3;
            tmpStr1 := '';
            Dec(iPStart, iPStart - iFStart - 1);
          End;
        (*
          Insert the result of the parenthesis into the original equation.
        *)
        Insert(str2, strCurEquation, iPStart);
      End;
  { Until all parenthsis are gone }
  Until(iPStart = 0) And (iPEnd = 0);
  strFunction := Copy(strCurEquation, 1, 3);
  If ((strFunction = 'BIN') Or (strFunction = 'OCT') Or (strFunction = 'HEX')) And
    Not FindDelimiter(strCurEquation) Then
    EvaluateEquation := StrToFloat(strCurEquation)
  Else
    Begin
      If Pos(',', strCurEquation) = 0 Then
        tmpStr1 := Evaluate(strCurEquation)
      Else
        tmpStr1 := '#Error. evaluating...';
      If tmpStr1[1] = '#' Then
        Raise Exception.CreateFmt(tmpStr1 + ' in expression [%s].',
          [strCurEquation])
      Else
        EvaluateEquation := StrToFloat(tmpStr1);
    End;
End;

{ -------------------------------------------------------------------------

   THBaseElement Class Methods

  ------------------------------------------------------------------------- }

(**

  This is a constructor for the THBaseElement class.

  @precon  None.
  @postcon Initialises the base element information. 

  @param   dblEasting  as a Double
  @param   dblNorthing as a Double
  @param   dblBearing  as a Double
  @param   dblChainage as a Double
  @param   dblLength   as a Double

**)
Constructor THBaseElement.Create(dblEasting, dblNorthing, dblBearing,
  dblChainage, dblLength : Double);

Begin
  FEasting  := dblEasting;
  FNorthing := dblNorthing;
  FBearing  := dblBearing;
  FChainage := dblChainage;
  FLength   := dblLength;
End;

(**

  This is a getter method for the EndChainage property.

  @precon  None.
  @postcon Returns the EndChainage of a horizontal element.

  @return  a Double

**)
function THBaseElement.GetEndChainage: Double;

begin
  Result := FChainage + FLength;
end;

(**

  This is a getter method for the EndPoint property.

  @precon  None.
  @postcon Returns the end coordinate and bearing of a horizontal element.

  @return  a THInfo

**)
function THBaseElement.GetEndPoint: THInfo;
begin
  Result := Setout(FChainage + FLength, 0);
end;

(**

  This is a getter method for the StartChainage property.

  @precon  None.
  @postcon Returns the start chainage of the horizontal element.

  @return  a Double

**)
function THBaseElement.GetStartChainage: Double;

begin
  Result := FChainage;
end;

(**

  This is a getter method for the StartPoint property.

  @precon  None.
  @postcon returns the start coordindate and bearing of the horizontal element.

  @return  a THInfo

**)
function THBaseElement.GetStartPoint: THInfo;
begin
  Result := Setout(FChainage, 0);
end;

{ -------------------------------------------------------------------------

   THStraightElement Class Methods

  ------------------------------------------------------------------------- }

(**

  This function compares a line from the given coordinate with a given bearing and finds
  the intersection point this this straight horizontal element.

  @precon  None.
  @postcon Returns a horizontal compare record containing the intersection information
           for the compare.

  @param   dblEasting  as a Double
  @param   dblNorthing as a Double
  @param   dblBearing  as a Double
  @return  a THCompInfo

**)
function THStraightElement.Compare(dblEasting, dblNorthing,
  dblBearing: Double): THCompInfo;

Var
  B : TVector;
  rec : THInfo;

begin
  B := GetVector(Easting, Northing, dblEasting, dblNorthing);
  B.dblBearing := AdjustBearing(B.dblBearing);
  Result.dblChainage := Chainage +
    Sin(DegToRad(dblBearing) - (DegToRad(B.dblBearing) + PI)) /
    Sin(DegToRad(Bearing) - DegToRad(dblBearing)) * B.dblDistance;
  Result.dblDistance := Sin(DegToRad(B.dblBearing) - DegToRad(Bearing)) /
    Sin(DegToRad(Bearing) - DegToRad(dblBearing)) * B.dblDistance;
  Result.dblBearing := Bearing;
  rec := Setout(Result.dblChainage, 0);
  Result.dblEasting := rec.dblEasting;
  Result.dblNorthing := rec.dblNorthing;
end;

(**

  This is the constructor method for the THStraightElement class.

  @precon  Requires the coordinates, chainage and bearing of the straight element.
  @postcon The element is created.

  @param   dblEasting  as a Double
  @param   dblNorthing as a Double
  @param   dblBearing  as a Double
  @param   dblChainage as a Double
  @param   dblLength   as a Double

**)
constructor THStraightElement.Create(dblEasting, dblNorthing, dblBearing,
  dblChainage, dblLength: Double);

begin
  Inherited Create(dblEasting, dblNorthing, dblBearing, dblChainage, dblLength);
  If Length <= 0 Then
    Raise ELengthZeroOrNegativeException.Create(
      'The length of an element can not be Zero or Negative.');
end;

(**

  This function returns the type of element as a straight element.

  @precon  None.
  @postcon Returns the type of element as a straight element.

  @return  a TElementType

**)
function THStraightElement.GetElementType: TElementType;

begin
  Result := etStraight;
end;

(**

  This is a getter method for the ElementDetails property.

  @precon  None.
  @postcon Returns the details the make up the element information in a horizontal element
           record.

  @return  a THElement

**)
function THStraightElement.GetElementDetails: THElement;

begin
  With Result Do
    Begin
      dblEasting := Easting;
      dblNorthing := Northing;
      dblBearing := Bearing;
      dblChainage := Chainage;
      dblLength := Length;
    End;
end;

(**

  This is a getter method for the Radius property.

  @precon  None.
  @postcon Returns a dummy number as a straight line has no radius.

  @param   dblChainage as a Double
  @return  a Double

**)
function THStraightElement.GetRadius(dblChainage: Double): Double;
begin
  Result := 999999.9000;
end;

(**

  This function measures the offset and chainage of the given coordinates to this straight
  line horizontal element.

  @precon  None.
  @postcon Returns the measure information for the chainage and offset of the given
           coordinates to the straight element.

  @param   dblEasting  as a Double
  @param   dblNorthing as a Double
  @return  a THInfo

**)
function THStraightElement.Measure(dblEasting, dblNorthing: Double): THInfo;

Var
  Vector : TVector;

begin
  Vector := GetVector(Easting, Northing, dblEasting, dblNorthing);
  Result.dblChainage := Chainage + Vector.dblDistance *
    Cos(DegToRad(Bearing - Vector.dblBearing));
  Result.dblOffset := Vector.dblDistance *
    Sin(DegToRad(Vector.dblBearing - Bearing));
  Result.dblBearing := Bearing;
end;

(**

  This function returns the coordinates and bearing of the straight line at a given
  chainage and offset.

  @precon  None.
  @postcon Returns the coordinates and bearing of the straight line at a given
           chainage and offset.

  @param   dblChainage as a Double
  @param   dblOffset   as a Double
  @return  a THInfo

**)
function THStraightElement.Setout(dblChainage, dblOffset: Double): THInfo;

Var
  Vector : TVector;

begin
  Vector := GetVector(0, Chainage, dblOffset, dblChainage);
  Result.dblEasting := Easting + Vector.dblDistance *
    Sin(DegToRad(Bearing + Vector.dblBearing));
  Result.dblNorthing := Northing + Vector.dblDistance *
    Cos(DegToRad(Bearing + Vector.dblBearing));
  Result.dblBearing := Bearing;
end;

{ -------------------------------------------------------------------------

   THCircularElement Class Methods

  ------------------------------------------------------------------------- }

(**

  This function finds the intersection of the given coordinates and bearing with this
  circular curve.

  @precon  The line must intersect with the curve else an exception is raised.
  @postcon Finds the intersections of the coordinates and bearing and returns them in a
           horizontal compare record.

  @param   dblEasting  as a Double
  @param   dblNorthing as a Double
  @param   dblBearing  as a Double
  @return  a THCompInfo

**)
function THCircularElement.Compare(dblEasting, dblNorthing,
  dblBearing: Double): THCompInfo;

Var
  dblT : Double;
  dblP : Double;
  dblS : Double;
  dblA : Double;
  dblK : Double;
//  dblFF : Double;
  dblQ : Double;
  dblE : Double;
  dblI, dblJ : Double;
  dblDist : Double;
  dblBz : Double;
  dblTZ : Double;
  dblCz : Double;
  dblTb : Double;

begin
  If FRadius > 0 then
    Begin
      dblT := Bearing + 270;
      dblP := 90;
    End Else
    Begin
      dblT := Bearing + 90;
      dblP := 270;
    End;
  dblS := Sqrt(Sqr(Easting - dblEasting) + Sqr(Northing - dblNorthing));
  If dblS <> 0 Then
    dblA := ArcCos((Northing - dblNorthing) / dblS)
  Else
    dblA := 0;
  If (Easting - dblEasting) < 0 Then
    dblA := PI * 2 - dblA;
  dblK := 1;
  dblQ := ArcSin(dblS * Sin(dblA - DegToRad(dblBearing)) / (Abs(FRadius)));
  dblE := DegToRad(dblBearing) - dblQ;

  Repeat
    dblI := Easting + Abs(FRadius) * Sin(dblE);
    dblJ := Northing + Abs(FRadius) * Cos(dblE);
    { If Dist is close to Radius this become invalid }
    If Abs(Abs(FRadius) - dblS) > 0.0001 Then
      dblDist := Sin(PI - (dblA - DegToRad(dblBearing)) -
        (DegToRad(dblBearing) - dblE)) * dblS / Sin(dblQ)
    Else
      dblDist := 0;;
    dblBz := ArcCos((dblJ - Northing) / Abs(FRadius));
    If (dblI - Easting) < 0  Then
      dblBz := PI * 2 - dblBz;
    dblTz := DegToRad(dblT);
    Repeat
      If dblTz > dblBz Then
        dblBz := dblBz + PI * 2;
    Until dblBz > dblTz;
    If FRadius < 0 Then
      dblCz := (dblTz - dblBz + PI * 2) * Abs(FRadius)
    Else
      dblCz := (dblBz - dblTz) * Abs(FRadius);
    dblTb := AdjustBearing(RadToDeg(dblBz) + dblP);
    If dblCz > (2 * PI * Abs(FRadius)) Then
      dblCz := dblCz - 2 * PI * Abs(FRadius);
    If (dblCz > 0) And (dblCz + Chainage < GetEndChainage) then
      Break;
    if dblK < 0 then
      Break;
    dblE := DegToRad(dblBearing) + dblQ + PI;
    dblK := -1;
  Until False;

  Result.dblEasting := dblI;
  Result.dblNorthing := dblJ;
  Result.dblChainage := Chainage + dblCz;
  Result.dblBearing := dblTb;
  Result.dblDistance := dblDist;
end;

(**

  This is the constructor method for the THCircularElement class.

  @precon  Requires either the centre of the circle or a point on its circumference,
           a start chainage, bearing, length and radius. Positive radii are right hand
           curves and negative radii are left hand curves.
  @postcon A circular element is created.

  @param   dblEasting  as a Double
  @param   dblNorthing as a Double
  @param   dblChainage as a Double
  @param   dblBearing  as a Double
  @param   dblLength   as a Double
  @param   dblRadius   as a Double
  @param   bCentre     as a Boolean

**)
constructor THCircularElement.Create(dblEasting, dblNorthing, dblChainage,
  dblBearing, dblLength, dblRadius: Double; bCentre: Boolean);

Var
  dblAdjust : Double;
  dblE, dblN : Double;

begin
  FRadius   := dblRadius;
  If bCentre Then
    Begin
      dblE  := dblEasting;
      dblN := dblNorthing;
    End Else
    Begin
      If FRadius < 0 Then
        dblAdjust := -90
      Else
        dblAdjust := +90;
      dblE := dblEasting + Abs(FRadius) * Sin(DegToRad(dblBearing + dblAdjust));
      dblN := dblNorthing + Abs(FRadius) * Cos(DegToRad(dblBearing + dblAdjust));
    End;
  Inherited Create(dblE, dblN, dblBearing, dblChainage, dblLength);
  If FRadius = 0 Then
    Raise EZeroRadiusException.Create(
      'A horizontal Circular must have a non-zero radius.');
  If Length <= 0 Then
    Raise ELengthZeroOrNegativeException.Create(
      'The length of an element can not be Zero or Negative.');
end;

(**

  This function returns an element type of circular.

  @precon  None.
  @postcon Returns an element type of circular.

  @return  a TElementType

**)
function THCircularElement.GetElementType: TElementType;
begin
  Result := etCircular;
end;

(**

  This is a getter method for the ElementDetails property.

  @precon  None.
  @postcon Returns the details about the elements construction.

  @return  a THElement

**)
function THCircularElement.GetElementDetails: THElement;
begin
  With Result Do
    Begin
      dblEasting := Easting;
      dblNorthing := Northing;
      dblBearing := Bearing;
      dblChainage := Chainage;
      dblLength := Length;
      dblRadius := FRadius;
    End;
end;

(**

  This is a getter method for the Radius property.

  @precon  None.
  @postcon Returns the radius of the circular curve.

  @param   dblChainage as a Double
  @return  a Double

**)
function THCircularElement.GetRadius(dblChainage: Double): Double;
begin
  Result := FRadius;
end;

(**

  This method finds the chainage and offset to this curve for the given coordinates.

  @precon  None.
  @postcon Returns the chainage and offset for the given coordinate to this curve.

  @param   dblEasting  as a Double
  @param   dblNorthing as a Double
  @return  a THInfo

**)
function THCircularElement.Measure(dblEasting, dblNorthing: Double): THInfo;

Var
  Vector : TVector;
  dblZ : Double;
  dblB : Double;

begin
  Vector := GetVector(Easting, Northing, dblEasting, dblNorthing);
  Vector.dblBearing := AdjustBearing(Vector.dblBearing); //: check calls to GetVector and see if they need AdjustBearing!
  If FRadius > 0 Then
    Begin
      dblB := Vector.dblBearing + 90;
      If dblB > 180 Then
        dblB := dblB - 360;
      While dblB < Bearing Do
        dblB := dblB + 360;
      dblZ := (dblB - Bearing) / 360 * FRadius * 2 * PI;
      If dblZ > 2 * PI * Abs(FRadius) Then
        dblZ := dblZ - 2 * PI * Abs(FRadius);
      Result.dblBearing := AdjustBearing(Vector.dblBearing + 90);
      Result.dblOffset := FRadius - Vector.dblDistance;
      Result.dblChainage := Chainage + dblZ;
    End Else
    Begin
      dblB := Vector.dblBearing - 90;
      If dblB < -180 Then
        dblB := dblB + 360;
      While dblB > Bearing Do
        dblB := dblB - 360;
      dblZ := (Bearing - dblB) / 360 * FRadius * 2 * PI;
      If dblZ > 2 * PI * Abs(FRadius) Then
        dblZ := dblZ - 2 * PI * Abs(FRadius);
      If dblZ < -2 * PI * Abs(FRadius) Then
        dblZ := dblZ + 2 * PI * Abs(FRadius);
      Result.dblBearing := AdjustBearing(Vector.dblBearing - 90);
      Result.dblOffset := Vector.dblDistance - Abs(FRadius);
      Result.dblChainage := Chainage - dblZ;
    End;
end;

(**

  This method returns the coordinates corresponding to the gven chanage and offset in this
  curve.

  @precon  None.
  @postcon Returns the coordinates corresponding to the gven chanage and offset in this
           curve.

  @param   dblChainage as a Double
  @param   dblOffset   as a Double
  @return  a THInfo

**)
function THCircularElement.Setout(dblChainage, dblOffset: Double): THInfo;

Var
  dblB : Double;
  dblR : Double;

begin
  dblB := (dblChainage - Chainage) / (2 * PI * FRadius) * 360;
  Result.dblBearing := AdjustBearing(Bearing + dblB);
  If FRadius > 0.0 Then
    Begin
      dblR := FRadius - dblOffset;
      Result.dblEasting := Easting + dblR * Sin(DegToRad(Bearing + dblB - 90));
      Result.dblNorthing := Northing + dblR * Cos(DegToRad(Bearing + dblB - 90));
    End Else
    Begin
      dblR := Abs(FRadius) + dblOffset;
      Result.dblEasting := Easting + dblR * Sin(DegToRad(Bearing + dblB + 90));
      Result.dblNorthing := Northing + dblR * Cos(DegToRad(Bearing + dblB + 90));
    End;
end;

{ -------------------------------------------------------------------------

   THClothoidElement Class Methods

  ------------------------------------------------------------------------- }

(**

  The principle of this procedure is to solve the intersection of the
  straight line and the transition curve in a local coordinate system
  around the origin of the transition curve.

  The first section of the code converts the global coordinates to the
  local coordinate system and then the solution is sort by an iteration
  using Newton-Raphson appling the straight line equation y=mx+c into
  the standard transition formulea - see the end of file for them.

  @precon  None.
  @postcon Returns the compare information record for the given intersection.

  @param   dblEasting  as a Double
  @param   dblNorthing as a Double
  @param   dblBearing  as a Double
  @return  a THCompInfo

**)
function THClothoidElement.Compare(dblEasting, dblNorthing,
  dblBearing: Double): THCompInfo;

Var
  iCount : Integer;
  dblSS, dblBS : Double;
  dblEO, dblNO : Double;
  dblM : Double;
  dblAA : Double;
  dblK : Double;
  dblC, dblC1, dblNN : Double;
  dblFC, dblFDC : Double;
  recHInfo : THInfo;

begin
  iCount := 0;
  dblSS := Sqrt(Sqr(dblEasting - Easting) + Sqr(dblNorthing - Northing));
  If dblSS <> 0 Then
    dblBS := Arccos((dblNorthing - Northing) / dblSS)
  Else
    dblBS := 0;
  If dblEasting - Easting < 0 Then
    dblBS := PI * 2 - dblBS;
  dblEO := dblSS * Sin(dblBS - DegToRad(Bearing));
  dblNO := dblSS * Cos(dblBS - DegToRad(Bearing));
  dblM := Tan(PI/2 - (DegToRad(dblBearing) - DegToRad(Bearing)));
  dblAA := dblNO - Tan(PI/2 - (DegToRad(dblBearing) - DegToRad(Bearing))) * dblEO;
  //If FRLValue < 0 Then
  //  dblM := dblM * -1;
  //dblK := Abs(FRLVAlue);
  dblK := FRLValue;
  dblC := 0;
  dblC1 := 0;
  dblNN := 0;
  Repeat
    If (dblC < 0) and (dblNN = 0) Then
      Begin
        dblM := dblM * -1;
        dblNN := 1;
      End;
    dblC := dblC1;
    dblFC := 66528 * Power(dblC, 5) * Power(dblK, 3)
      - 2661120 * dblC * Power(dblK, 5)
      - 770 * Power(dblC, 9) * dblK
      + 443520 * dblM * Power(dblC, 3) * Power(dblK, 4)
      - 7920 * dblM * Power(dblC, 7) * Power(dblK, 2)
      + 63 * dblM * Power(dblC, 11)
      + 2661120 * dblAA * Power(dblK, 5);
    dblFDC := 332640 * Power(dblC, 4) * Power(dblK, 3)
      - 2661120 * Power(dblK, 5)
      - 6930 * Power(dblC, 8) * dblK
      + 1330560 * dblM * Sqr(dblC) * Power(dblK, 4)
      - 55440 * dblM * Power(dblC, 6) * Sqr(dblK)
      + 693 * dblM * Power(dblC, 10);
    dblC1 := dblC - dblFC / dblFDC;
    Inc(iCount);
    If iCount > 1000 Then
      Raise Exception.Create('Loop Detected in Clothoid.Compare.');
  Until(Abs(dblC1 - dblC) < 0.00005);

  recHInfo := Setout(FOChainage + dblC, 0);

  dblSS := Sqrt(Sqr(recHInfo.dblEasting - dblEasting) +
    Sqr(recHInfo.dblNorthing  - dblNorthing));
  If dblSS <> 0 Then
    dblBS := ArcCos((recHInfo.dblNorthing - dblNorthing) / dblSS)
  Else
    dblBS := 0;
  If (recHInfo.dblEasting - dblEasting) < 0 Then
    dblBS := PI * 2 - dblBS;
  If Abs(dblBearing - RadToDeg(dblBS)) > 90 Then
    dblSS := dblSS * -1;

  Result.dblEasting := recHInfo.dblEasting;
  Result.dblNorthing := recHInfo.dblNorthing;
  Result.dblChainage := FOChainage + dblC;
  Result.dblBearing := recHInfo.dblBearing;
  Result.dblDistance := dblSS;
end;

(**

  This is the constructor method for the THClothoidElement class.

  @precon  Requires the original of the transition curve, its Radius * Length value,
           origin bearing, start chainage and origin chainage
  @postcon The class is created.

  @param   dblEasting            as a Double
  @param   dblNorthing           as a Double
  @param   dblChainage           as a Double
  @param   dblBearing            as a Double
  @param   dblLength             as a Double
  @param   dblStChainageOrRadius as a Double
  @param   dblRLValueOrEndRadius as a Double
  @param   boolFalse             as a Boolean

**)
constructor THClothoidElement.Create(dblEasting, dblNorthing, dblChainage,
  dblBearing, dblLength, dblStChainageOrRadius, dblRLValueOrEndRadius: Double;
  boolFalse : Boolean);

var
  dblX: Double;
  dblY: Double;
  dblTheta: Double;
  dblZ: Double;
  dblE, dblN, dblC, dblB, dblL : Double;

begin
  If Not boolFalse Then
    Begin
      dblE       := dblEasting;
      dblN       := dblNorthing;
      FOChainage := dblChainage;
      dblB       := dblBearing;
      dblL       := dblLength;
      dblC       := dblStChainageOrRadius;
      If dblL <= 0 Then
        Raise ELengthZeroOrNegativeException.Create(
          'The length of an element can not be Zero or Negative.');
      FRLValue        := dblRLValueOrEndRadius;
      If FRLValue = 0 Then
        Raise EZeroRLValueExecption.Create(
          'A horizontal clothoid must have a non-zero RL value.');
    End Else
    Begin
      dblL := dblLength;
      If dblL <= 0 Then
        Raise ELengthZeroOrNegativeException.Create(
          'The length of an element can not be Zero or Negative.');
      If dblStChainageOrRadius + dblRLValueOrEndRadius = 0 Then
        Raise EZeroRadiusException.Create(
          'A horizontal clothoid must have non-zero Start and End Radius values.');
      If dblStChainageOrRadius = 0 Then
        Begin
          (*
            This means the starting point if is the origin.
          *)
          dblE       := dblEasting;
          dblN       := dblNorthing;
          FOChainage := dblChainage;
          dblC       := dblChainage;
          dblB       := dblBearing;
          dblL       := dblLength;
          If dblL <= 0 Then
            Raise ELengthZeroOrNegativeException.Create(
              'The length of an element can not be Zero or Negative.');
          FRLValue        := dblLength * dblRLValueOrEndRadius;
          If FRLValue = 0 Then
            Raise EZeroRLValueExecption.Create(
              'A horizontal clothoid must have a non-zero RL value.');
        End
      Else If dblRLValueOrEndRadius = 0 Then
        Begin
          (*
            This means the end point is the origina and needs calcuating.
          *)
          dblC       := dblChainage;
          dblL       := dblLength;
          FOChainage := dblChainage + dblLength;
          If dblL <= 0 Then
            Raise ELengthZeroOrNegativeException.Create(
              'The length of an element can not be Zero or Negative.');
          FRLValue        := dblLength * dblStChainageOrRadius;
          If FRLValue = 0 Then
            Raise EZeroRLValueExecption.Create(
              'A horizontal clothoid must have a non-zero RL value.');
          dblX := GetX(-dblLength);
          dblY := GetY(-dblLength);
          dblTheta := GetTheta(-dblLength);
          dblB := AdjustBearing(dblBearing - RadToDeg(dblTheta));
          dblE := dblEasting - dblX * Sin(DegToRad(dblB + 90)) - dblY * Sin(DegToRad(dblB));
          dblN := dblNorthing - dblX * Cos(DegToRad(dblB + 90)) - dblY * Cos(DegToRad(dblB));
        End Else
        Begin
          dblZ := dblL * dblRLValueOrEndRadius / (dblStChainageOrRadius -
            dblRLValueOrEndRadius);
          FRLValue := Abs(dblZ) * dblStChainageOrRadius;
          dblC := dblChainage;
          FOChainage := dblChainage - dblZ;
          dblX := GetX(dblZ);
          dblY := GetY(dblZ);
          dblTheta := GetTheta(dblZ);
          dblB := AdjustBearing(dblBearing - RadToDeg(dblTheta));
          dblE := dblEasting - dblY * Sin(DegToRad(dblB)) - dblX * Sin(DegToRad(dblB + 90));
          dblN := dblNorthing - dblY * Cos(DegToRad(dblB)) - dblX * Cos(DegToRad(dblB + 90));
        End;
    End;
  Inherited Create(dblE, dblN, dblB, dblC, dblL);
end;

(**

  This function returns the type of element as Clothoid.

  @precon  None.
  @postcon Returns the type of element as Clothoid.

  @return  a TElementType

**)
function THClothoidElement.GetElementType: TElementType;
begin
  Result := etClothoid;
end;

(**

  This is a getter method for the ElementDetails property.

  @precon  None.
  @postcon Returns the horizontal elements details.

  @return  a THElement

**)
function THClothoidElement.GetElementDetails: THElement;
begin
  With Result Do
    Begin
      dblEasting := Easting;
      dblNorthing := Northing;
      dblBearing := Bearing;
      dblChainage := FOChainage;
      dblLength := Length;
      dblRLValue := FRLValue;
      dblStChainage := Chainage;
    End;
end;

(**

  This is a getter method for the Radius property.

  @precon  Requires the chainage at which the radius is needed.
  @postcon Returns the radius at the given chainage.

  @param   dblChainage as a Double
  @return  a Double

**)
function THClothoidElement.GetRadius(dblChainage: Double): Double;
begin
  If dblChainage <> FOChainage Then
    Result := FRLValue / Abs(dblChainage - FOChainage)
  Else
    Result := 999999.9000;
end;

(**

  This is a getter method for the Theta property.

  @precon  Requires the chainage at which Theta (bearing change from origin) is required
  @postcon Returns the given angle.

  @param   dblDistance as a Double
  @return  a Double

**)
function THClothoidElement.GetTheta(dblDistance : Double): Double;
begin
  Result := Sqr(dblDistance) / ( 2 * FRLValue);
  If dblDistance < 0 Then
    Result := -Result;
end;

(**

  This is a getter method for the X property.

  @precon  Requires the distance from the origin for which X (distance from the origin at
           the origin bearing) must be calculated
  @postcon The X coordinate is returned.

  @param   dblDistance as a Double
  @return  a Double

**)
function THClothoidElement.GetX(dblDistance : Double): Double;

Var
  K : Double;

begin
  K := FRLValue;
  Result := Power(dblDistance, 3) / (6 * K) -
    Power(dblDistance ,7) / (336 * Power(K, 3)) +
    Power(dblDistance, 11) / (42240 * Power(K, 5));
  If dblDistance < 0 Then
    Result := -Result;
end;

(**

  This is a getter method for the Y property.

  @precon  Requires the distance from the origin for which Y (distance from the origin at
           the origin bearing) must be calculated
  @postcon The Y coordinate is returned.

  @param   dblDistance as a Double
  @return  a Double

**)
function THClothoidElement.GetY(dblDistance : Double): Double;

Var
  K : Double;

begin
  K := FRLValue;
  Result := dblDistance - Power(dblDistance, 5) / (40 * Sqr(K)) +
    Power(dblDistance, 9) / (3456 * Power(K, 4));
end;

(**

  This method returns the chainage and offset of the given coordinate to this curve.

  @precon  None.
  @postcon Returns the chainage and offset of the given coordinate to this curve.

  @param   dblEasting  as a Double
  @param   dblNorthing as a Double
  @return  a THInfo

**)
function THClothoidElement.Measure(dblEasting, dblNorthing: Double): THInfo;

Var
  Vector : TVector;
  dblC : Double;
  HData : THInfo;
  dblDelta : Double;
  dblO : Double;
  iLoops : Integer;

begin
  dblC := FOChainage;
  iLoops := 0;
  Repeat
    HData := Setout(dblC, 0);
    Vector := GetVector(HData.dblEasting, HData.dblNorthing, dblEasting,
      dblNorthing);
    dblDelta := Sin(DegToRad(HData.dblBearing + 90 - Vector.dblBearing)) *
      Vector.dblDistance;
    dblO := Cos(DegToRad(HData.dblBearing + 90 - Vector.dblBearing)) *
      Vector.dblDistance;
    dblC := dblC + dblDelta;
    Inc(iLoops);
    If iLoops > 1000 Then
      Raise Exception.Create('Loop detected in Clothoid.Measure.');
  Until (Abs(dblDelta) < 0.00005);
  Result.dblChainage := dblC;
  Result.dblBearing := HData.dblBearing;
  If dblO > 0 Then
    Result.dblOffset := Vector.dblDistance
  Else
    Result.dblOffset := -Vector.dblDistance;
end;

(**

  This method finds the coordinates of the given chainage and offset to this curve.

  @precon  None.
  @postcon Returns the coordinates of the given chainage and offset to this curve.

  @param   dblChainage as a Double
  @param   dblOffset   as a Double
  @return  a THInfo

**)
function THClothoidElement.Setout(dblChainage, dblOffset: Double): THInfo;

Var
  dblX : Double;
  dblY : Double;
  dblTheta : Double;
  dblOBearing : Double;

begin
  dblOBearing := DegToRad(Bearing);
  dblX := GetX(dblChainage - FOChainage);
  dblY := GetY(dblChainage - FOChainage);
  dblTheta := GetTheta(dblChainage - FOChainage);
  Result.dblEasting := Easting + dblY * Sin(dblOBearing) +
    dblX * Sin(dblOBearing + PI / 2) +
    dblOffset * Sin(dblOBearing + PI / 2 + dblTheta);
  Result.dblNorthing := Northing + dblY * Cos(dblOBearing) +
    dblX * Cos(dblOBearing + PI / 2) +
    dblOffset * Cos(dblOBearing + PI / 2 + dblTheta);
  Result.dblBearing := AdjustBearing(RadToDeg(dblTheta + dblOBearing));
end;

{ -------------------------------------------------------------------------

   TVBaseElement Class Methods

 -------------------------------------------------------------------------- }

(**

  This is a constructor for the TVBaseElement class.

  @precon  None.
  @postcon Initialises the base element information.

  @param   dblLevel    as a Double
  @param   dblGradient as a Double
  @param   dblChainage as a Double
  @param   dblLength   as a Double

**)
Constructor TVBaseElement.Create(dblLevel, dblGradient, dblChainage,
  dblLength : Double);

Begin
  FLevel := dblLevel;
  FGradient := dblGradient;
  FChainage := dblChainage;
  FLength := dblLength;
End;

(**

  This is a getter method for the EndChainage property.

  @precon  None.
  @postcon Returns the end chainage of the vertical element.

  @return  a Double

**)
function TVBaseElement.GetEndChainage: Double;
begin
  Result := FChainage + FLength;
end;

(**

  This is a getter method for the EndPoint property.

  @precon  None.
  @postcon Returns the end point information for the vertical element.

  @return  a TVInfo

**)
function TVBaseElement.GetEndPoint: TVInfo;
begin
  Result := Setout(FChainage + FLength);
end;

(**

  This is a getter method for the StartChainage property.

  @precon  None.
  @postcon Returns the start chainage of the element.

  @return  a Double

**)
function TVBaseElement.GetStartChainage: Double;
begin
  Result := FChainage;
end;

(**

  This is a getter method for the StartPoint property.

  @precon  None.
  @postcon Returns the start point information of the element.

  @return  a TVInfo

**)
function TVBaseElement.GetStartPoint: TVInfo;
begin
  Result := Setout(FChainage);
end;

{ -------------------------------------------------------------------------

   TVStraightElement Class Methods

 -------------------------------------------------------------------------- }

(**

  This is the constructor method for the TVStraightElement class.

  @precon  Requires the level, gradient and chainage of the element start point.
  @postcon the element is created.

  @param   dblLevel    as a Double
  @param   dblGradient as a Double
  @param   dblChainage as a Double
  @param   dblLength   as a Double

**)
constructor TVStraightElement.Create(dblLevel, dblGradient, dblChainage,
  dblLength: Double);
begin
  Inherited Create(dblLevel, dblGradient, dblChainage, dblLength);
  If Length <= 0 Then
    Raise ELengthZeroOrNegativeException.Create(
      'The length of an element can not be Zero or Negative.');
end;

(**

  This function returns the element type as straight.

  @precon  None.
  @postcon Returns the element type as straight.

  @return  a TElementType

**)
function TVStraightElement.GetElementType: TElementType;
begin
  Result := etStraight;
end;

(**

  This is a getter method for the ElementDetails property. 

  @precon  None. 
  @postcon Returns the elements details. 

  @return  a TVElement

**)
function TVStraightElement.GetElementDetails: TVElement;
begin
  With Result Do
    Begin
      dblLevel    := Level;
      dblGradient := Gradient;
      dblChainage := Chainage;
      dblLength   := Length;
    End;
end;

(**

  This is a getter method for the Radius property.

  @precon  None.
  @postcon Returns a dummy radius for the straight curve.

  @param   dblChainage as a Double
  @return  a Double

**)
function TVStraightElement.GetRadius(dblChainage: Double): Double;
begin
  Result := 999999.9000;
end;

(**

  This method gets the level and gradient of the curve at the given chainage.

  @precon  None
  @postcon Returns the level and gradient of the curve at the given chainage.

  @param   dblChainage as a Double
  @return  a TVInfo

**)
function TVStraightElement.Setout(dblChainage: Double): TVInfo;
begin
  Result.dblLevel := Level + Gradient * (dblChainage - Chainage);
  Result.dblGradient := Gradient;
end;

{ -------------------------------------------------------------------------

   TVCircularElement Class Methods

 -------------------------------------------------------------------------- }

(**

  This is the constructor method for the TVCircularElement class.

  @precon  Requires the level, gradient, chainage, length and radius of the curve start
           point.
  @postcon The class is created.

  @param   dblLevel    as a Double
  @param   dblGradient as a Double
  @param   dblChainage as a Double
  @param   dblLength   as a Double
  @param   dblRadius   as a Double

**)
constructor TVCircularElement.Create(dblLevel, dblGradient, dblChainage,
  dblLength, dblRadius: Double);
begin
  Inherited Create(dblLevel, dblGradient, dblChainage, dblLength);
  If Length <= 0 Then
    Raise ELengthZeroOrNegativeException.Create(
      'The length of an element can not be Zero or Negative.');
  FRadius   := dblRadius;
  If FRadius = 0 Then
    Raise EZeroRadiusException.Create('The radius of a circular vertical ' +
      'curve can not be zero.');
end;

(**

  This method returns the element type as Circular.

  @precon  None.
  @postcon Returns the element type as Circular.

  @return  a TElementType

**)
function TVCircularElement.GetElementType: TElementType;
begin
  Result := etCircular;
end;

(**

  This is a getter method for the ElementDetails property. 

  @precon  None. 
  @postcon Returns the details of the elements construction. 

  @return  a TVElement

**)
function TVCircularElement.GetElementDetails: TVElement;
begin
  With Result Do
    Begin
      dblLevel    := Level;
      dblGradient := Gradient;
      dblChainage := Chainage;
      dblLength   := Length;
      dblRadius   := FRadius;
    End;
end;

(**

  This is a getter method for the Radius property.

  @precon  None.
  @postcon Returns the radius of the curve at the given point.

  @param   dblChainage as a Double
  @return  a Double

**)
function TVCircularElement.GetRadius(dblChainage: Double): Double;
begin
  Result := FRadius;
end;

(**

  This method gets the level and gradient of the curve at the given chainage.

  @precon  None
  @postcon Returns the level and gradient of the curve at the given chainage.

  @param   dblChainage as a Double
  @return  a TVInfo

**)
function TVCircularElement.Setout(dblChainage: Double): TVInfo;
begin
  Result.dblLevel := Level + Gradient * (dblChainage - Chainage) +
    Sqr(dblChainage - Chainage) / (2 * FRadius);
  Result.dblGradient := Gradient + (dblChainage - Chainage) / FRadius;
end;

{ -------------------------------------------------------------------------

   THAlignmentCollection Class Methods

  ------------------------------------------------------------------------- }

(**

  A method to add a straight element to the end of the horizontal alignment.

  @precon  Requires the information defining the straight line.
  @postcon A straight element is added to the end of the collection.

  @param   dblEasting  as a Double
  @param   dblNorthing as a Double
  @param   dblBearing  as a Double
  @param   dblChainage as a Double
  @param   dblLength   as a Double

**)
procedure THAlignmentCollection.AddStraight(dblEasting, dblNorthing,
  dblBearing, dblChainage, dblLength: Double);

Var
  FHStraight : THStraightElement;

begin
  FHStraight := THStraightElement.Create(dblEasting, dblNorthing, dblBearing,
    dblChainage, dblLength);
  FHElements.Add(FHStraight);
  Modified := True;
  If Assigned(OnChange) Then
    OnChange(Self);
end;

(**

  A method to add a circular element to the end of the horizontal alignment.

  @precon  Requires the information defining the circular element.
  @postcon A circular element is added to the end of the collection.

  @param   dblEasting  as a Double
  @param   dblNorthing as a Double
  @param   dblBearing  as a Double
  @param   dblChainage as a Double
  @param   dblRadius   as a Double
  @param   dblLength   as a Double
  @param   bCentre     as a Boolean

**)
procedure THAlignmentCollection.AddCircular(dblEasting, dblNorthing,
  dblBearing, dblChainage, dblRadius, dblLength: Double; bCentre: Boolean);

Var
  FHCircle : THCircularElement;

begin
  FHCircle := THCircularElement.Create(dblEasting, dblNorthing, dblChainage,
    dblBearing, dblLength, dblRadius, bCentre);
    FHElements.Add(FHCircle);
    Modified := True;
    If Assigned(OnChange) Then
     OnChange(Self);
end;

(**

  A method to add a clothoid element to the end of the horizontal alignment.

  @precon  Requires the information defining the clothoid element.
  @postcon A clothoid element is added to the end of the collection.

  @param   dblEasting    as a Double
  @param   dblNorthing   as a Double
  @param   dblBearing    as a Double
  @param   dblOChainage  as a Double
  @param   dblStChainage as a Double
  @param   dblRLValue    as a Double
  @param   dblLength     as a Double

**)
procedure THAlignmentCollection.AddClothoid(dblEasting, dblNorthing,
  dblBearing, dblOChainage, dblStChainage, dblRLValue, dblLength: Double);

Var
  FHClothiod : THClothoidElement;

begin
  FHClothiod := THClothoidElement.Create(dblEasting, dblNorthing, dblOChainage,
    dblBearing, dblLength, dblStChainage, dblRLValue, False);
    FHElements.Add(FHClothiod);
    Modified := True;
    If Assigned(OnChange) Then
     OnChange(Self);
end;

(**

  A method to add a clothoid element with a false origin to the end of the horizontal
  alignment.

  @precon  Requires the information defining the clothoid element.
  @postcon A clothoid element is added to the end of the collection.

  @param   dblEasting   as a Double
  @param   dblNorthing  as a Double
  @param   dblBearing   as a Double
  @param   dblChainage  as a Double
  @param   dblLength    as a Double
  @param   dblStRadius  as a Double
  @param   dblEndRadius as a Double

**)
procedure THAlignmentCollection.AddClothoidFalse(dblEasting, dblNorthing,
  dblBearing, dblChainage, dblLength, dblStRadius, dblEndRadius: Double);

Var
  FHClothiod : THClothoidElement;

begin
  FHClothiod := THClothoidElement.Create(dblEasting, dblNorthing,
    dblChainage, dblBearing, dblLength, dblStRadius, dblEndRadius, True);
  FHElements.Add(FHClothiod);
  Modified := True;
  If Assigned(OnChange) Then
    OnChange(Self);
end;

(**

  This function returns the number of horizontal elements in the collection.

  @precon  None.
  @postcon Returns the number of horizontal elements in the collection.

  @return  an Integer

**)
function THAlignmentCollection.Count: Integer;

begin
  Result := FHElements.Count;
end;

(**

  This is the constructor method for the THAlignmentCollection class.

  @precon  None.
  @postcon The horizontal alignment collection is created.

**)
constructor THAlignmentCollection.Create;
begin
  Inherited Create;
  FHElements := THElements.Create();
  FCoordinateError := 0.010;
  FBearingError := 10.0 / 3600.0;
  FModified := False;
end;

(**

  This is the destructor method for the THAlignmentCollection class.

  @precon  Frees any allocated elements and the frees itself.
  @postcon Class collection is destroyed.

**)
destructor THAlignmentCollection.Destroy;

Var
  iElement : Integer;

begin
  FHElements.Pack;
  For iElement := 0 To FHElements.Count - 1 Do
    THBaseElement(FHElements[iElement]).Free;
  FHElements.Free();
  Inherited;
end;

(**

  This method returns the type of horizontal element corresponds to the indexed item.

  @precon  The index must be a valid index between 1 and Count.
  @postcon Returns the type of horizontal element corresponds to the indexed item.

  @param   iElement as an Integer
  @return  a TElementType

**)
function THAlignmentCollection.ElementType(iElement: Integer): TElementType;

begin
  Result := THBaseElement(FHElements[iElement]).ElementType;
end;

(**

  This method returns the start information for the indexed element.

  @precon  The index must be a valid index between 1 and Count.
  @postcon Returns the start information for the indexed element.

  @param   iElement as an Integer
  @return  a THInfo

**)
function THAlignmentCollection.GetElementStart(iElement: Integer): THInfo;

begin
  Result := THBaseElement(FHElements[iElement]).StartPoint;
end;

(**

  This method returns the end information for the indexed element.

  @precon  The index must be a valid index between 1 and Count.
  @postcon Returns the end information for the indexed element.

  @param   iElement as an Integer
  @return  a THInfo

**)
function THAlignmentCollection.GetElementEnd(iElement: Integer): THInfo;

begin
  Result := THBaseElement(FHElements[iElement]).EndPoint;
end;

(**

  This function returns the end chainage for the indexed element.

  @precon  The index must be a valid index between 1 and Count.
  @postcon Returns the end chainage for the indexed element.

  @param   iElement as an Integer
  @return  a Double

**)
function THAlignmentCollection.GetEndChainage(iElement: Integer): Double;

begin
  Result := THBaseElement(FHElements[iElement]).EndChainage;
end;

(**

  This function returns the start chainage for the indexed element.

  @precon  The index must be a valid index between 1 and Count.
  @postcon Returns the start chainage for the indexed element.

  @param   iElement as an Integer
  @return  a Double

**)
function THAlignmentCollection.GetStartChainage(iElement: Integer): Double;

begin
  Result := THBaseElement(FHElements[iElement]).StartChainage;
end;

(**

  This method loads a horizontal alignment from an INI type file.

  @precon  None.
  @postcon The horizontal alignment is loaded from the given filename.

  @param   strFileName as a String

**)
procedure THAlignmentCollection.LoadFromFile(strFileName: String);

Var
  iTotalElements : Integer;
  iElement : Integer;
  iniFile: TIniFile;

begin
  iniFile := TIniFile.Create(strFileName);
  Try
    iTotalElements := iniFile.ReadInteger('GeneralInfo', 'NumOfHElements', 0);
    For iElement := 1 To iTotalElements Do
     Begin
       Case iniFile.ReadInteger(Format('HElement%d', [iElement]), 'Type', 0) Of
         1: AddStraight(
              iniFile.ReadFloat(Format('HElement%d', [iElement]), 'Easting', 0),
              iniFile.ReadFloat(Format('HElement%d', [iElement]), 'Northing', 0),
              iniFile.ReadFloat(Format('HElement%d', [iElement]), 'Bearing', 0),
              iniFile.ReadFloat(Format('HElement%d', [iElement]), 'Chainage', 0),
              iniFile.ReadFloat(Format('HElement%d', [iElement]), 'Length', 0)
            );
         2: AddCircular(
              iniFile.ReadFloat(Format('HElement%d', [iElement]), 'Easting', 0),
              iniFile.ReadFloat(Format('HElement%d', [iElement]), 'Northing', 0),
              iniFile.ReadFloat(Format('HElement%d', [iElement]), 'Bearing', 0),
              iniFile.ReadFloat(Format('HElement%d', [iElement]), 'Chainage', 0),
              iniFile.ReadFloat(Format('HElement%d', [iElement]), 'Radius', 0),
              iniFile.ReadFloat(Format('HElement%d', [iElement]), 'Length', 0),
              True
            );
         3: AddClothoid(
              iniFile.ReadFloat(Format('HElement%d', [iElement]), 'Easting', 0),
              iniFile.ReadFloat(Format('HElement%d', [iElement]), 'Northing', 0),
              iniFile.ReadFloat(Format('HElement%d', [iElement]), 'Bearing', 0),
              iniFile.ReadFloat(Format('HElement%d', [iElement]), 'Chainage', 0),
              iniFile.ReadFloat(Format('HElement%d', [iElement]), 'StChainage', 0),
              iniFile.ReadFloat(Format('HElement%d', [iElement]), 'RLValue', 0),
              iniFile.ReadFloat(Format('HElement%d', [iElement]), 'Length', 0)
            );
       Else
         Raise Exception.Create('Unknown Alignment Type Found in File.');
       End;
     End;
  Finally
    iniFile.Free;
  End;
  Modified := False;
end;

(**

  This method saves the horzontal alignment to the given filename in an INI file type.

  @precon  None.
  @postcon Saves the horzontal alignment to the given filename in an INI file type.

  @param   strFileName as a String

**)
procedure THAlignmentCollection.SaveToFile(strFileName: String);

Var
  iniFile : TIniFile;
  iElement : Integer;
  recElement : THElement;

  (**

    This procedure writes a specific item (integer) of the horizontal element to the INI
    file.

    @precon  None.
    @postcon The item (integer) is written to the INI file.

    @param   iElement as an Integer
    @param   strItem  as a String
    @param   dblValue as a Double

  **)
  Procedure WriteItem(iElement : Integer; strItem : String;
    dblValue : Double); Overload;

  Begin
    iniFile.WriteFloat(Format('HElement%d', [iElement + 1]), strItem, dblValue);
  End;

  (**

    This procedure writes a specific item (double) of the horizontal element to the INI
    file.

    @precon  None.
    @postcon The item (double) is written to the INI file.

    @param   iElement as an Integer
    @param   strItem  as a String
    @param   iValue   as an Integer

  **)
  Procedure WriteItem(iElement : Integer; strItem : String;
    iValue : Integer); Overload;

  Begin
    iniFile.WriteInteger(Format('HElement%d', [iElement + 1]), strItem, iValue);
  End;

begin
  iniFile := TIniFile.Create(strFileName);
  Try
    iniFile.WriteInteger('GeneralInfo', 'NumOfHElements', Count);
    For iElement := 0 To Count - 1 Do
      Case ElementType(iElement) Of
        etStraight:
          Begin
            recElement := THStraightElement(FHElements[iElement]).ElementDetails;
            WriteItem(iElement, 'Type', Integer(ElementType(iElement)));
            WriteItem(iElement, 'Easting', recElement.dblEasting);
            WriteItem(iElement, 'Northing', recElement.dblNorthing);
            WriteItem(iElement, 'Bearing', recElement.dblBearing);
            WriteItem(iElement, 'Chainage', recElement.dblChainage);
            WriteItem(iElement, 'Length', recElement.dblLength);
          End;
        etCircular:
          Begin
            recElement := THCircularElement(FHElements[iElement]).ElementDetails;
            WriteItem(iElement, 'Type', Integer(ElementType(iElement)));
            WriteItem(iElement, 'Easting', recElement.dblEasting);
            WriteItem(iElement, 'Northing', recElement.dblNorthing);
            WriteItem(iElement, 'Bearing', recElement.dblBearing);
            WriteItem(iElement, 'Chainage', recElement.dblChainage);
            WriteItem(iElement, 'Length', recElement.dblLength);
            WriteItem(iElement, 'Radius', recElement.dblRadius);
          End;
        etClothoid:
          Begin
            recElement := THClothoidElement(FHElements[iElement]).ElementDetails;
            WriteItem(iElement, 'Type', Integer(ElementType(iElement)));
            WriteItem(iElement, 'Easting', recElement.dblEasting);
            WriteItem(iElement, 'Northing', recElement.dblNorthing);
            WriteItem(iElement, 'Bearing', recElement.dblBearing);
            WriteItem(iElement, 'Chainage', recElement.dblChainage);
            WriteItem(iElement, 'Length', recElement.dblLength);
            WriteItem(iElement, 'RLValue', recElement.dblRLValue);
            WriteItem(iElement, 'StChainage', recElement.dblStChainage);
          End;
      End;
  Finally
    iniFile.Free;
  End;
  Modified := False;
end;

(**

  This method gets the circular element information for the given indexed element.

  @precon  The index must be a valid index between 1 and Count.
  @postcon Gets the circular element information for the given indexed element.

  @param   iElement as an Integer
  @return  a THElement

**)
function THAlignmentCollection.GetCircular(iElement: Integer): THElement;

begin
  Result := THCircularElement(FHElements[iElement]).ElementDetails;
end;

(**

  This method gets the clothoid element information for the given indexed element.

  @precon  The index must be a valid index between 1 and Count.
  @postcon Gets the clothoid element information for the given indexed element.

  @param   iElement as an Integer
  @return  a THElement

**)
function THAlignmentCollection.GetClothoid(iElement: Integer): THElement;

begin
  Result := THClothoidElement(FHElements[iElement]).ElementDetails;
end;

(**

  This method gets the straight element information for the given indexed element.

  @precon  The index must be a valid index between 1 and Count.
  @postcon Gets the straight element information for the given indexed element.

  @param   iElement as an Integer
  @return  a THElement

**)
function THAlignmentCollection.GetStraight(iElement: Integer): THElement;

begin
  Result := THStraightElement(FHElements[iElement]).ElementDetails;
end;

(**

  This method updates an indexed circular horizontal element.

  @precon  Requires the new information for the element.
  @postcon Updates the indexed element.

  @param   iELement    as an Integer
  @param   dblEasting  as a Double
  @param   dblNorthing as a Double
  @param   dblBearing  as a Double
  @param   dblChainage as a Double
  @param   dblRadius   as a Double
  @param   dblLength   as a Double
  @param   bCentre     as a Boolean

**)
procedure THAlignmentCollection.UpdateCircular(iELement : Integer; dblEasting,
  dblNorthing, dblBearing, dblChainage, dblRadius, dblLength: Double;
  bCentre: Boolean);

Var
  FOldCircle : THCircularElement;

begin
  If ElementType(iElement) <> etCircular Then
    Raise Exception.Create('This element is not a Circular element.');
  (*
    Store pointer to old version
    Create new version in its place
    Destroy old version
  *)
  FOldCircle := THCircularElement(FHElements[iElement]);
  FHElements[iElement] := THCircularElement.Create(dblEasting, dblNorthing,
    dblChainage, dblBearing, dblLength, dblRadius, bCentre);
  FOldCircle.Free;
  Modified := True;
  If Assigned(OnChange) Then
   OnChange(Self);
end;

(**
  This method updates an indexed clothoid horizontal element.

  @precon  Requires the new information for the element.
  @postcon Updates the indexed element.

  @param   iElement      as an Integer
  @param   dblEasting    as a Double
  @param   dblNorthing   as a Double
  @param   dblBearing    as a Double
  @param   dblOChainage  as a Double
  @param   dblStChainage as a Double
  @param   dblRLValue    as a Double
  @param   dblLength     as a Double
  @param   boolFalse     as a Boolean

**)
procedure THAlignmentCollection.UpdateClothoid(iElement : Integer; dblEasting,
  dblNorthing, dblBearing, dblOChainage, dblStChainage, dblRLValue,
  dblLength: Double; boolFalse : Boolean);

Var
  FOldClothoid : THClothoidElement;

begin
  If ElementType(iElement) <> etClothoid Then
    Raise Exception.Create('This element is not a Clothoid element.');
  (*
    Store pointer to old version
    Create new version in its place
    Destroy old version
  *)
  FOldClothoid := THClothoidElement(FHElements[iElement]);
  If Not boolFalse Then
    FHElements[iElement] := THClothoidElement.Create(dblEasting, dblNorthing,
      dblOChainage, dblBearing, dblLength, dblStChainage, dblRLValue, False)
  Else
    FHElements[iElement] := THClothoidElement.Create(dblEasting, dblNorthing,
      dblOChainage, dblBearing, dblLength, dblStChainage, dblRLValue, True);
  FOldClothoid.Free;
  Modified := True;
  If Assigned(OnChange) Then
   OnChange(Self);
end;

(**

  This method updates an indexed straight horizontal element.

  @precon  Requires the new information for the element.
  @postcon Updates the indexed element.

  @param   iElement    as an Integer
  @param   dblEasting  as a Double
  @param   dblNorthing as a Double
  @param   dblBearing  as a Double
  @param   dblChainage as a Double
  @param   dblLength   as a Double

**)
procedure THAlignmentCollection.UpdateStraight(iElement : Integer; dblEasting,
  dblNorthing, dblBearing, dblChainage, dblLength: Double);

Var
  FOldStraight : THStraightElement;

begin
  If ElementType(iElement) <> etStraight Then
    Raise Exception.Create('This element is not a Straight element.');
  (*
    Store pointer to old version
    Create new version in its place
    Destroy old version
  *)
  FOldStraight := THStraightElement(FHElements[iElement]);
  FHElements[iElement] := THStraightElement.Create(dblEasting, dblNorthing,
    dblBearing, dblChainage, dblLength);
  FOldStraight.Free;
  Modified := True;
  If Assigned(OnChange) Then
    OnChange(Self);
end;

(**

  This is a getter method for the Radius property.

  @precon  iElement must be a valid index for an element of the alignment.
  @postcon Returns the radius at the given chainage.

  @param   iElement    as an Integer
  @param   dblChainage as a Double
  @return  a Double

**)
function THAlignmentCollection.GetRadius(iElement: Integer;
  dblChainage: Double): Double;
begin
  Result := THBaseElement(FHElements[iElement]).Radius[dblChainage];
end;

(**

  This is a setter method for the CoordinateError property.

  @precon  None.
  @postcon Sets the Coordinate error tolerance.

  @param   Value as a Double as a constant

**)
procedure THAlignmentCollection.SetCoordinateError(const Value: Double);
begin
  If FCoordinateError <> Value Then
    FCoordinateError := Value;
end;

(**

  This is a setter method for the BearingError property.

  @precon  None.
  @postcon Sets the bearing error tolerance.

  @param   Value as a Double as a constant

**)
procedure THAlignmentCollection.SetBearingError(const Value: Double);
begin
  If FBearingError <> Value Then
    FBearingError := Value;
end;

(**

  This is a setter method for the Modified property.

  @precon  None.
  @postcon Sets whether the alignment has been modified.

  @param   Value as a Boolean as a constant

**)
procedure THAlignmentCollection.SetModified(const Value: Boolean);
begin
  FModified := Value;
end;

(**

  This method finds the coordinates of the given chainage and offset to this alignment.

  @precon  None.
  @postcon Returns the coordinates and bearing of the coordinate correpsonding to the
           given chainage and offset.

  @param   dblChainage as a Double
  @param   dblOffset   as a Double
  @return  a THInfo

**)
function THAlignmentCollection.Setout(dblChainage, dblOffset: Double): THInfo;

Var
  iElement : Integer;

begin
  Result.dblEasting := 0;
  Result.dblNorthing := 0;
  Result.dblBearing := 0;
  If Count = 0 Then
    Raise Exception.Create('There is no horizontal alignment.');
  If dblChainage < GetStartChainage(0) Then
    Raise Exception.CreateFmt('The chainage %1.4f and offset %1.4f lies ' +
      'before the alignment start.', [dblChainage, dblOffset]);
  If dblChainage > GetEndChainage(Count - 1) Then
    Raise Exception.CreateFmt('The chainage %1.4f and offset %1.4f lies ' +
      'beyond the alignment end.', [dblChainage, dblOffset]);
  For iElement := 0 To Count - 1 Do
    If (dblChainage >= GetStartChainage(iElement)) And
      (dblChainage <= GetEndChainage(iELement)) Then
      Begin
        Result := THBaseElement(FHElements[iElement]).Setout(dblChainage,
          dblOffset);
        Exit;
      End;
end;

(**

  This method finds the chainage and offset corresponding to the given coordinates.

  @precon  None.
  @postcon Returns the chainage and offset corresponding to the given coordinates.

  @param   dblEasting  as a Double
  @param   dblNorthing as a Double
  @return  a THinfo

**)
function THAlignmentCollection.Measure(dblEasting, dblNorthing: Double): THinfo;

Var
  iElement : Integer;
  HInfo : THInfo;
  Vector : TVector;

begin
  Result.dblChainage := 0;
  Result.dblOffset := 0;
  Result.dblBearing := 0;
  For iElement := 0 To Count - 1 Do
    Begin
      HInfo := THBaseElement(FHElements[iElement]).StartPoint;
      Vector := GetVector(HInfo.dblEasting, HInfo.dblNorthing, dblEasting,
        dblNorthing);
      If Cos(DegToRad(Vector.dblBearing - HInfo.dblBearing)) <= 0 Then
        Begin
          If iElement = 0 Then
            Raise EBeforeAlignmentStartException.CreateFmt(
              'These coordinates (%1.4f, %1.4f) lie before the alignment start.',
              [dblEasting, dblNorthing]);
          Result := THBaseElement(FHElements[iElement - 1]).Measure(dblEasting,
            dblNorthing);
          Exit;
        End;
    End;
  { Check last element }
  HInfo := THBaseElement(FHElements[Count - 1]).EndPoint;
  Vector := GetVector(HInfo.dblEasting, HInfo.dblNorthing, dblEasting,
    dblNorthing);
  If Cos(DegToRad(Vector.dblBearing - HInfo.dblBearing)) <= 0 Then
    Begin
      Result := THBaseElement(FHElements[Count - 1]).Measure(dblEasting,
        dblNorthing);
      Exit;
    End Else
      Raise EAfterAlignmentEndException.CreateFmt(
          'These coordinates (%1.4f, %1.4f) lie beyond the alignment end.',
          [dblEasting, dblNorthing]);
end;

(**

  This method finds the intersection of the given coordinates and bearing to the
  alignment.

  @precon  None.
  @postcon Returns the intersection of the given coordinates and bearing to the
           alignment.

  @param   dblEasting  as a Double
  @param   dblNorthing as a Double
  @param   dblBearing  as a Double
  @return  a THCompinfo

**)
function THAlignmentCollection.Compare(dblEasting, dblNorthing,
  dblBearing: Double): THCompinfo;


ResourceString
  strBeforeMsg = 'This intersection (%1.4f, %1.4f, %s) lies before the ' +
    'alignment start.';
  strAfterMsg = 'This intersection (%1.4f, %1.4f, %s) lies after the ' +
    'alignment end.';

Var
  iElement : Integer;
  V : TVector;
  B : TVector;
  recSt : THInfo;
  recEnd : THInfo;
  dblChain : Double;
  dblDist : Double;

begin
  Result.dblChainage := 0;
  Result.dblBearing := 0;
  Result.dblDistance := 0;
  For iElement := 0 To Count - 1 Do
    Begin
      recSt := GetElementStart(iElement);
      recEnd := GetElementEnd(iElement);
      dblDist := Sqrt(Sqr(recEnd.dblEasting - recSt.dblEasting) +
        Sqr(recEnd.dblNorthing - recSt.dblNorthing));
      V := GetVector(recSt.dblEasting, recSt.dblNorthing, recEnd.dblEasting,
        recEnd.dblNorthing);
      B := GetVector(recSt.dblEasting, recSt.dblNorthing, dblEasting, dblNorthing);
      V.dblBearing := AdjustBearing(V.dblBearing);
      B.dblBearing := AdjustBearing(B.dblBearing);
      dblChain := Sin(DegToRad(dblBearing) - (DegToRad(B.dblBearing) + PI)) /
        Sin(DegToRad(V.dblBearing) - DegToRad(dblBearing)) * B.dblDistance;

      If (iElement = 0) And (dblChain < 0) Then
        Raise Exception.CreateFmt(strBeforeMsg, [dblEasting, dblNorthing,
          BearingToString(DecimalToDMS(dblBearing))]);

      If dblChain <= dblDist Then
        Begin
          Result := THBaseElement(FHElements[iElement]).Compare(
            dblEasting, dblNorthing, dblBearing);
          Exit;
        End;
    End;
  Raise Exception.CreateFmt(strAfterMsg, [dblEasting, dblNorthing,
    BearingToString(DecimalToDMS(dblBearing))]);
end;

(**

  This method returns the end chainage of the alignment.

  @precon  None.
  @postcon Returns the end chainage of the alignment.

  @return  a Double

**)
function THAlignmentCollection.EndChainage: Double;
begin
  If Count <= 0 Then
    Raise Exception.Create('There is no horizontal alignment.');
  Result := THBaseElement(FHElements[Count - 1]).EndChainage;
end;

(**

  This method returns the start chainage of the alignment.

  @precon  None.
  @postcon Returns the start chainage of the alignment.

  @return  a Double

**)
function THAlignmentCollection.StartChainage: Double;
begin
  If Count <= 0 Then
    Raise Exception.Create('There is no horizontal alignment.');
  Result := THBaseElement(FHElements[0]).StartChainage;
end;

(**

  This method clears all the element from this horizontal alignment collection.

  @precon  None.
  @postcon Clears all the element from this horizontal alignment collection.

**)
procedure THAlignmentCollection.Clear;

Var
  iElement : Integer;

begin
  FHElements.Pack;
  For iElement := 0 To FHElements.Count - 1 Do
    THBaseElement(FHElements[iElement]).Free;
  FHElements.Clear;
end;

{ -------------------------------------------------------------------------

   TVAlignmentCollection Class Methods

 -------------------------------------------------------------------------- }

(**

  This method adds a vertical circular element to the collection.

  @precon  Requires the level, gradient, chainage, length and radius of the element.
  @postcon The element is added to the end of the collection.

  @param   dblLevel    as a Double
  @param   dblGradient as a Double
  @param   dblChainage as a Double
  @param   dblLength   as a Double
  @param   dblRadius   as a Double

**)
procedure TVAlignmentCollection.AddCircular(dblLevel, dblGradient,
  dblChainage, dblLength, dblRadius: Double);

Var
  FVCircular : TVCircularElement;

begin
  FVCircular := TVCircularElement.Create(dblLevel, dblGradient, dblChainage,
    dblLength, dblRadius);
  FVElements.Add(FVCircular);
  Modified := True;
  If Assigned(OnChange) Then
    OnChange(Self);
end;

(**

  This method adds a vertical straight element to the collection.

  @precon  Requires the level, gradient, chainage, and length of the element.
  @postcon The element is added to the end of the collection.

  @param   dblLevel    as a Double
  @param   dblGradient as a Double
  @param   dblChainage as a Double
  @param   dblLength   as a Double

**)
procedure TVAlignmentCollection.AddStraight(dblLevel, dblGradient,
  dblChainage, dblLength: Double);

Var
  FVStraight : TVStraightElement;

begin
  FVStraight := TVStraightElement.Create(dblLevel, dblGradient, dblChainage,
    dblLength);
  FVElements.Add(FVStraight);
  Modified := True;
  If Assigned(OnChange) Then
    OnChange(Self);
end;

(**

  This method clears all elements from the alignment collection.

  @precon  None.
  @postcon Clears all elements from the alignment collection.

**)
procedure TVAlignmentCollection.Clear;

Var
  iElement : Integer;

begin
  FVElements.Pack;
  For iElement := 0 To FVElements.Count - 1 Do
    TVBaseElement(FVElements[iElement]).Free;
  FVElements.Clear;
end;

(**

  This method returns the number of element in the collection.

  @precon  None.
  @postcon Returns the number of element in the collection.

  @return  an Integer

**)
function TVAlignmentCollection.Count: Integer;
begin
  Result := FVElements.Count;
end;

(**

  This is the constructor method for the TVAlignmentCollection class.

  @precon  None.
  @postcon The collection is created.

**)
constructor TVAlignmentCollection.Create;
begin
  Inherited Create;
  FVElements := TVElements.Create;
  FLevelError := 0.005;   // 5 mm
  FGradientError := 0.01; // 1% or 1 in 1000mm
end;

(**

  This is the destructor method for the TVAlignmentCollection class.

  @precon  None.
  @postcon The collection is destroy along with any elements that it contained.

**)
destructor TVAlignmentCollection.Destroy;

Var
  iElement : Integer;

begin
  FVElements.Pack;
  For iElement := 0 To FVElements.Count - 1 Do
    TVBaseElement(FVElements[iElement]).Free;
  FVElements.Free;
  inherited;
end;

(**

  This method returns the type of the indexed element.

  @precon  iElement must be a valid element index.
  @postcon Returns the type of the indexed element.

  @param   iElement as an Integer
  @return  a TElementType

**)
function TVAlignmentCollection.ElementType(iElement: Integer): TElementType;
begin
  Result := TVBaseElement(FVElements[iElement]).ElementType;
end;

(**

  This method returns the end chainage of the vertical alignment.

  @precon  None.
  @postcon Returns the end chainage of the vertical alignment.

  @return  a Double

**)
function TVAlignmentCollection.EndChainage: Double;
begin
  If Count <= 0 Then
    Raise Exception.Create('There is no vertical alignment.');
  Result := TVBaseElement(FVElements[Count - 1]).EndChainage;
end;

(**

  This method returns the details of the indexed circular element. 

  @precon  iElement must be a valid element index. 
  @postcon Returns the details of the indexed circular element. 

  @param   iElement as an Integer
  @return  a TVElement

**)
function TVAlignmentCollection.GetCircular(iElement: Integer): TVElement;
begin
  Result := TVCircularElement(FVElements[iElement]).ElementDetails;
end;

(**

  This method returns the level and gradient of the alignment end.

  @precon  iElement must be a valid element index.
  @postcon Returns the level and gradient of the alignment end.

  @param   iElement as an Integer
  @return  a TVInfo

**)
function TVAlignmentCollection.GetElementEnd(iElement: Integer): TVInfo;
begin
  Result := TVBaseElement(FVElements[iElement]).EndPoint;
end;

(**

  This method returns the level and gradient of the alignment start.

  @precon  iElement must be a valid element index.
  @postcon Returns the level and gradient of the alignment start.

  @param   iElement as an Integer
  @return  a TVInfo

**)
function TVAlignmentCollection.GetElementStart(iElement: Integer): TVInfo;
begin
  Result := TVBaseElement(FVElements[iElement]).StartPoint;
end;

(**

  This method returns the alignments end chainage.

  @precon  None.
  @postcon Returns the alignments end chainage.

  @param   iElement as an Integer
  @return  a Double

**)
function TVAlignmentCollection.GetEndChainage(iElement: Integer): Double;
begin
  Result := TVBaseElement(FVElements[iElement]).EndChainage;
end;

(**

  This method returns the radius of the indexed element at the given chainage.

  @precon  iElement must be a valid element index.
  @postcon Returns the radius of the indexed element at the given chainage.

  @param   iElement    as an Integer
  @param   dblChainage as a Double
  @return  a Double

**)
function TVAlignmentCollection.GetRadius(iElement: Integer;
  dblChainage: Double): Double;
begin
  Result := TVBaseElement(FVElements[iElement]).Radius[dblChainage];
end;

(**

  This method returns the alignments start chainage.

  @precon  None.
  @postcon Returns the alignments start chainage.

  @param   iElement as an Integer
  @return  a Double

**)
function TVAlignmentCollection.GetStartChainage(iElement: Integer): Double;
begin
  Result := TVBaseElement(FVElements[iElement]).StartChainage;
end;

(**

  This method returns the straight element details for the given indexed 
  element. 

  @precon  iElement must be a valid element index. 
  @postcon Returns the straight element details for the given indexed element. 

  @param   iElement as an Integer
  @return  a TVElement

**)
function TVAlignmentCollection.GetStraight(iElement: Integer): TVElement;
begin
  Result := TVStraightElement(FVElements[iElement]).ElementDetails;
end;

(**

  This method loads the vertical alignment from a given INI file.

  @precon  None.
  @postcon Loads the vertical alignment from a given INI file.

  @param   strFileName as a String

**)
procedure TVAlignmentCollection.LoadFromFile(strFileName: String);

Var
  iTotalElements : Integer;
  iElement : Integer;
  iniFile: TIniFile;

begin
  iniFile := TIniFile.Create(strFileName);
  Try
    iTotalElements := iniFile.ReadInteger('GeneralInfo', 'NumOfVElements', 0);
    For iElement := 1 To iTotalElements Do
     Begin
       Case iniFile.ReadInteger(Format('VElement%d', [iElement]), 'Type', 0) Of
         1: AddStraight(
              iniFile.ReadFloat(Format('VElement%d', [iElement]), 'Level', 0),
              iniFile.ReadFloat(Format('VElement%d', [iElement]), 'Gradient', 0),
              iniFile.ReadFloat(Format('VElement%d', [iElement]), 'Chainage', 0),
              iniFile.ReadFloat(Format('VElement%d', [iElement]), 'Length', 0)
            );
         2: AddCircular(
              iniFile.ReadFloat(Format('VElement%d', [iElement]), 'Level', 0),
              iniFile.ReadFloat(Format('VElement%d', [iElement]), 'Gradient', 0),
              iniFile.ReadFloat(Format('VElement%d', [iElement]), 'Chainage', 0),
              iniFile.ReadFloat(Format('VElement%d', [iElement]), 'Length', 0),
              iniFile.ReadFloat(Format('VElement%d', [iElement]), 'Radius', 0)
            );
       Else
         Raise Exception.Create('Unknown Alignment Type Found in File.');
       End;
     End;
  Finally
    iniFile.Free;
  End;
  Modified := False;
end;

(**

  This method save the vertical alignment to the given INI file.

  @precon  None.
  @postcon Save the vertical alignment to the given INI file.

  @param   strFileName as a String

**)
procedure TVAlignmentCollection.SaveToFile(strFileName: String);

Var
  iniFile : TIniFile;
  iElement : Integer;
  recStraight : TVElement;
  recCircular : TVElement;

  (**

    This procedure writes a double value to the INI file.

    @precon  None.
    @postcon Writes a double value to the INI file.

    @param   iElement as an Integer
    @param   strItem  as a String
    @param   dblValue as a Double

  **)
  Procedure WriteItem(iElement : Integer; strItem : String;
    dblValue : Double); Overload;

  Begin
    iniFile.WriteFloat(Format('VElement%d', [iElement + 1]), strItem, dblValue);
  End;

  (**

    This procedure writes an integer value to the INI file.

    @precon  None.
    @postcon Writes a an integer value to the INI file.

    @param   iElement as an Integer
    @param   strItem  as a String
    @param   iValue   as an Integer

  **)
  Procedure WriteItem(iElement : Integer; strItem : String;
    iValue : Integer); Overload;

  Begin
    iniFile.WriteInteger(Format('VElement%d', [iElement + 1]), strItem, iValue);
  End;

begin
  iniFile := TIniFile.Create(strFileName);
  Try
    iniFile.WriteInteger('GeneralInfo', 'NumOfVElements', Count);
    For iElement := 0 To Count - 1 Do
      Case ElementType(iElement) Of
        etStraight:
          Begin
            recStraight := TVStraightElement(FVElements[iElement]).ElementDetails;
            WriteItem(iElement, 'Type', Integer(ElementType(iElement)));
            WriteItem(iElement, 'Level', recStraight.dblLevel);
            WriteItem(iElement, 'Gradient', recStraight.dblGradient);
            WriteItem(iElement, 'Chainage', recStraight.dblChainage);
            WriteItem(iElement, 'Length', recStraight.dblLength);
          End;
        etCircular:
          Begin
            recCircular := TVCircularElement(FVElements[iElement]).ElementDetails;
            WriteItem(iElement, 'Type', Integer(ElementType(iElement)));
            WriteItem(iElement, 'Level', recCircular.dblLevel);
            WriteItem(iElement, 'Gradient', recCircular.dblGradient);
            WriteItem(iElement, 'Chainage', recCircular.dblChainage);
            WriteItem(iElement, 'Length', recCircular.dblLength);
            WriteItem(iElement, 'Radius', recCircular.dblRadius);
          End;
      End;
  Finally
    iniFile.Free;
  End;
  Modified := False;
end;

(**

  This is a setter method for the GradientError property.

  @precon  None.
  @postcon Sets the GradientError property.

  @param   Value as a Double as a constant

**)
procedure TVAlignmentCollection.SetGradientError(const Value: Double);
begin
  If FGradientError <> Value Then
    FGradientError := Value;
end;

(**

  This is a setter method for the LevelError property.

  @precon  None.
  @postcon Sets the LevelError property.

  @param   Value as a Double as a constant

**)
procedure TVAlignmentCollection.SetLevelError(const Value: Double);
begin
  If FLevelError <> Value Then
    FLevelError := Value;
end;

(**

  This is a setter method for the Modified property.

  @precon  None.
  @postcon Sets the Modified property.

  @param   Value as a Boolean as a constant

**)
procedure TVAlignmentCollection.SetModified(const Value: Boolean);
begin
  If FModified <> Value Then
    FModified := Value;
end;

(**

  This method finds the gradient and level of the given alignment chainage.

  @precon  None.
  @postcon Returns the gradient and level of the given alignment chainage

  @param   dblChainage as a Double
  @return  a TVInfo

**)
function TVAlignmentCollection.Setout(dblChainage: Double): TVInfo;

Var
  iElement : Integer;

begin
  Result.dblLevel := 0;
  Result.dblGradient := 0;
  For iElement := 0 To Count - 1 Do
    If (dblChainage >= GetStartChainage(iElement)) And
      (dblChainage <= GetEndChainage(iELement)) Then
    Begin
      Result := TVBaseElement(FVElements[iElement]).Setout(dblChainage);
      Exit;
    End;
end;

(**

  This method returns the start chainage of the alignment.

  @precon  None.
  @postcon Returns the start chainage of the alignment.

  @return  a Double

**)
function TVAlignmentCollection.StartChainage: Double;
begin
  If Count <= 0 Then
    Raise Exception.Create('There is no vertical alignment.');
  Result := TVBaseElement(FVElements[0]).StartChainage;
end;

(**

  This method updates the indexed circular vertical curve.

  @precon  Requires the new circular curves details.
  @postcon The element is updated.

  @param   iElement    as an Integer
  @param   dblLevel    as a Double
  @param   dblGradient as a Double
  @param   dblChainage as a Double
  @param   dblLength   as a Double
  @param   dblRadius   as a Double

**)
procedure TVAlignmentCollection.UpdateCircular(iElement : Integer; dblLevel,
  dblGradient, dblChainage, dblLength, dblRadius: Double);

Var
  FOldCircle : TVCircularElement;

begin
  If ElementType(iElement) <> etCircular Then
    Raise Exception.Create('This element is not a Circular element.');
  (*
    Store pointer to old version
    Create new version in its place
    Destroy old version
  *)
  FOldCircle := TVCircularElement(FVElements[iElement]);
  FVElements[iElement] := TVCircularElement.Create(dblLevel, dblGradient,
    dblChainage, dblLength, dblRadius);
  FOldCircle.Free;
  Modified := True;
  If Assigned(OnChange) Then
    OnChange(Self);
end;

(**

  This method updates the indexed straight vertical curve.

  @precon  Requires the new straight curves details.
  @postcon The element is updated.

  @param   iElement    as an Integer
  @param   dblLevel    as a Double
  @param   dblGradient as a Double
  @param   dblChainage as a Double
  @param   dblLength   as a Double

**)
procedure TVAlignmentCollection.UpdateStraight(iElement : Integer; dblLevel,
  dblGradient, dblChainage, dblLength: Double);

Var
  FOldStraight : TVStraightElement;

begin
  If ElementType(iElement) <> etStraight Then
    Raise Exception.Create('This element is not a Straight element.');
  (*
    Store pointer to old version
    Create new version in its place
    Destroy old version
  *)
  FOldStraight := TVStraightElement(FVElements[iElement]);
  FVElements[iElement] := TVStraightElement.Create(dblLevel, dblGradient,
    dblChainage, dblLength);
  FOldStraight.Free;
  Modified := True;
  If Assigned(OnChange) Then
    OnChange(Self);
end;

{ -------------------------------------------------------------------------

  TAlignment Class Methods

 -------------------------------------------------------------------------- }

(**

  This method clears both the horizontal and vertical alignment collections.

  @precon  None.
  @postcon Clears both the horizontal and vertical alignment collections.

**)
procedure TStringAlignment.Clear;
begin
  HAlignment.Clear;
  VAlignment.Clear;
end;

(**

  This is the constructor method for the TStringAlignment class.

  @precon  None.
  @postcon Both a horizontal and vertical alignments are created with in the class.

**)
constructor TStringAlignment.Create;
begin
  Inherited Create;
  FHAlignment := THAlignmentCollection.Create;
  FVAlignment := TVAlignmentCollection.Create;
end;

(**

  This is the destructor method for the TStringAlignment class.

  @precon  None.
  @postcon Both horizontal and vertical alignments are freed.

**)
destructor TStringAlignment.Destroy;
begin
  FVAlignment.Free;
  FHAlignment.Free;
  inherited;
end;

(**

  This method loads both the horizontal and vertical alignments from the given INI file.

  @precon  None.
  @postcon Loads both the horizontal and vertical alignments from the given INI file.

  @param   strFileName as a String

**)
procedure TStringAlignment.LoadFromFile(strFileName : String);
begin
  HAlignment.LoadFromFile(strFileName);
  VAlignment.LoadFromFile(strFileName);
end;

(**

  This method finds the chainage and offset of the given coordinates to the alignment.

  @precon  None.
  @postcon Returns the chainage and offset of the given coordinates to the alignment.

  @param   dblEasting  as a Double
  @param   dblNorthing as a Double
  @return  a TMeasureInfo

**)
function TStringAlignment.Measure(dblEasting, dblNorthing : Double): TMeasureInfo;

Var
  HInfo : THInfo;
  VInfo : TVInfo;

begin
  HInfo := HAlignment.Measure(dblEasting, dblNorthing);
  Result.dblChainage := HInfo.dblChainage;
  Result.dblOffset := HInfo.dblOffset;
  Result.dblBearing := HInfo.dblBearing;
  VInfo := VAlignment.Setout(HInfo.dblChainage);
  Result.dblLevel := VInfo.dblLevel;
  Result.dblGradient := VInfo.dblGradient;
end;

(**

  This method finds the intersection of the given coordinates and bearing with the
  alignment.

  @precon  None.
  @postcon Returns the intersection of the given coordinates and bearing with the
           alignment.

  @param   dblEasting  as a Double
  @param   dblNorthing as a Double
  @param   dblBearing  as a Double
  @return  a TCompareInfo

**)
function TStringAlignment.Compare(dblEasting, dblNorthing,
  dblBearing : Double): TCompareInfo;

Var
  HInfo : THCompInfo;
  VInfo : TVInfo;

begin
  HInfo := HAlignment.Compare(dblEasting, dblNorthing, dblBearing);
  Result.dblEasting := dblEasting;
  Result.dblNorthing := dblNorthing;
  Result.dblIEasting := HInfo.dblEasting;
  Result.dblINorthing := HInfo.dblNorthing;
  Result.dblChainage := HInfo.dblChainage;
  Result.dblDistance := HInfo.dblDistance;
  Result.dblBearing := HInfo.dblBearing;
  VInfo := VAlignment.Setout(HInfo.dblChainage);
  Result.dblLevel := VInfo.dblLevel;
  Result.dblGradient := VInfo.dblGradient;
end;

(**

  This method saves both the horizontal and vertical alignments to the given INI file.

  @precon  None.
  @postcon Saves both the horizontal and vertical alignments to the given INI file.


  @param   strFileName as a String

**)
procedure TStringAlignment.SaveToFile(strFileName : String);
begin
  If FileExists(strFileName) Then
    SysUtils.DeleteFile(strFileName);
  HAlignment.SaveToFile(strFileName);
  VAlignment.SaveToFile(strFileName);
end;

(**

  This method finds the coordinates and bearing of the given chainage and offset.

  @precon  None.
  @postcon Returns the coordinates and bearing of the given chainage and offset.

  @param   dblChainage as a Double
  @param   dblOffset   as a Double
  @return  a TInfo

**)
function TStringAlignment.Setout(dblChainage, dblOffset: Double): TInfo;
begin
  Result.HInfo := HAlignment.Setout(dblChainage, dblOffset);
  Result.VInfo := VAlignment.Setout(dblChainage);
end;

(**

  This method returns a string representation of the given bearing record as
  ###°##'##.##".

  @precon  None.
  @postcon Returns a string representation of the given bearing record as ###°##'##.##".

  @param   recBearing as a TBearing
  @return  a String

**)
Function BearingToString(recBearing : TBearing) : String;

Begin
  With recBearing Do
    Result := Format(strBearingFormat, [iDegrees, iMinutes, iSeconds, iHundreds]);
End;

(**

  This routine extract the build number from the EXE resources for
  display in the app title.

  @precon  None.
  @postcon Extract the build number from the EXE resources for display in the
           app title.

  @param   strFileName as a String
  @param   iMajor      as an Integer as a reference
  @param   iMinor      as an Integer as a reference
  @param   iBugfix     as an Integer as a reference
  @param   iBuild      as an Integer as a reference
  @return  a String

**)
Function GetBuildNumber(strFileName : String; var iMajor, iMinor, iBugfix,
  iBuild : Integer) : String;

Const
  strBuild = '%d.%d.%d.%d';

Var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;

Begin
  VerInfoSize := GetFileVersionInfoSize(PChar(strFileName), Dummy);
  If VerInfoSize <> 0 Then
    Begin
      GetMem(VerInfo, VerInfoSize);
      GetFileVersionInfo(PChar(strFileName), 0, VerInfoSize, VerInfo);
      VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
      with VerValue^ do
      begin
        iMajor := dwFileVersionMS shr 16;
        iMinor := dwFileVersionMS and $FFFF;
        iBugfix := dwFileVersionLS shr 16;
        iBuild := dwFileVersionLS and $FFFF;
        Result := Format(strBuild, [iMajor, iMinor, iBugfix, iBuild]);
      end;
      FreeMem(VerInfo, VerInfoSize);
    End Else
      Raise EBuildNumberException.CreateFmt(
        'The executable "%s" does not contain any version information.', [strFileName]);
End;

(**

  This method returns a string for the title of a console application containing
  the version number and build number.

  @precon  Expects a string to contains %d.%d%s for the version number followed
           by %s for the build number.
  @postcon Returns a string for the title of a console application containing
           the version number and build number.

  @param   strTitle as a String
  @return  a String

**)
Function GetConsoleTitle(strTitle: String) : String;

Var
  iMajor, iMinor, iBugfix, iBuild : Integer;
  strBuildNumber  : String;
  dtDate : TDateTime;

Begin
  strBuildNumber := GetBuildNumber(ParamStr(0), iMajor, iMinor, iBugFix, iBuild);
  Result := Format(strTitle, [iMajor, iMinor, strBugFix[iBugFix + 1],
    strBuildNumber]);
  {$IFDEF D2006}
  FileAge(ParamStr(0), dtDate);
  {$ELSE}
  dtDate := FileDateToDateTime(FileAge(ParamStr(0)));
  {$ENDIF}
  Result := Result + #13#10 +
    Format('Written by David Hoyle (c) %s', [FormatDateTime('mmm/yyyy', dtDate)]);
End;

(**

  This function returns the background colour attribute for the console
  associated with the given cl#### colour.

  Colour Matrix:
    Red(Maroon)
                Yellow(Olive)
    Lime(Green)               White(Gray)
                Aqua(Teal)
    Blue(Navy)

  @precon  CSBI must be a valid Console Screen Buffer Info structure.
  @postcon Returns the background colour attribute for the console
           associated with the given cl#### colour.

  @param   iColour as a TColor
  @param   iNone   as a TColor
  @return  an Integer

**)
Function ForeGroundColour(iColour, iNone : TColor): Integer;

Begin
  Case iColour Of
    clBlack  : Result := 0;
    clMaroon  :Result := FOREGROUND_RED;
    clGreen   :Result := FOREGROUND_GREEN;
    clOlive   :Result := FOREGROUND_RED Or FOREGROUND_GREEN;
    clNavy    :Result := FOREGROUND_BLUE;
    //clPurple  :;
    clTeal    :Result := FOREGROUND_BLUE Or FOREGROUND_GREEN;
    clGray    :Result := FOREGROUND_BLUE Or FOREGROUND_GREEN Or
                 FOREGROUND_RED;
    //clSilver  :;
    clRed     :Result := FOREGROUND_RED   Or FOREGROUND_INTENSITY;
    clLime    :Result := FOREGROUND_GREEN Or FOREGROUND_INTENSITY;
    clYellow  :Result := FOREGROUND_RED Or FOREGROUND_GREEN Or
                 FOREGROUND_INTENSITY;
    clBlue    :Result := FOREGROUND_BLUE  Or FOREGROUND_INTENSITY;
    //clFuchsia :;
    clAqua    :Result := FOREGROUND_BLUE Or FOREGROUND_GREEN  Or
                 FOREGROUND_INTENSITY;
    //clLtGray  :;
    //clDkGray  :;
    clWhite   :Result := FOREGROUND_BLUE Or FOREGROUND_GREEN Or
                 FOREGROUND_RED Or FOREGROUND_INTENSITY;
    clNone  : Result  := iNone;
  Else
    Raise Exception.Create('Invalid console colour.');
  End;
End;

(**

  This function returns the background colour attribute for the console
  associated with the given cl#### colour.

  @precon  CSBI must be a valid Console Screen Buffer Info structure.
  @postcon Returns the background colour attribute for the console
           associated with the given cl#### colour.

  @param   iColour as a TColor
  @param   iNone   as a TColor
  @return  an Integer

**)
Function BackGroundColour(iColour, iNone : TColor) : Integer;

Begin
  Case iColour Of
    clBlack  : Result := 0;
    clMaroon  :Result := BACKGROUND_RED;
    clGreen   :Result := BACKGROUND_GREEN;
    clOlive   :Result := BACKGROUND_RED Or BACKGROUND_GREEN;
    clNavy    :Result := BACKGROUND_BLUE;
    //clPurple  :;
    clTeal    :Result := BACKGROUND_BLUE Or BACKGROUND_GREEN;
    clGray    :Result := BACKGROUND_BLUE Or BACKGROUND_GREEN Or
                 BACKGROUND_RED;
    //clSilver  :;
    clRed     :Result := BACKGROUND_RED   Or BACKGROUND_INTENSITY;
    clLime    :Result := BACKGROUND_GREEN Or BACKGROUND_INTENSITY;
    clYellow  :Result := BACKGROUND_RED Or BACKGROUND_GREEN Or
                 BACKGROUND_INTENSITY;
    clBlue    :Result := BACKGROUND_BLUE  Or BACKGROUND_INTENSITY;
    //clFuchsia :;
    clAqua    :Result := BACKGROUND_BLUE Or BACKGROUND_GREEN  Or
                 BACKGROUND_INTENSITY;
    //clLtGray  :;
    //clDkGray  :;
    clWhite   :Result := BACKGROUND_BLUE Or BACKGROUND_GREEN Or
                 BACKGROUND_RED Or BACKGROUND_INTENSITY;
    clNone  : Result  := iNone;
  Else
    Raise Exception.Create('Invalid console colour.');
  End;
End;

Type
  (** An enumerate to define the current console output mode **)
  TConsoleMode = (cmUnknown, cmStandard, cmRedirected);

Var
  (** A private variable to hold the console output mode **)
  ConsoleMode : TConsoleMode;

(**

  This function returns the current mode of output of the console functions. The
  first time its gets the console mode from the Win32 API.

  @precon  hndConsole must be a valid console handle.
  @postcon Returns the current mode of output of the console functions.

  @param   hndConsole as a THandle
  @return  a Boolean   

**)
Function CheckConsoleMode(hndConsole : THandle) : Boolean;

Var
  lpMode : Cardinal;

Begin
  If ConsoleMode = cmUnknown Then
    If Not GetConsoleMode(hndConsole, lpMode) Then
      ConsoleMode := cmRedirected
    Else
      ConsoleMode := cmStandard;
  Result := ConsoleMode = cmStandard;
End;

(**

  This function outputs the given text to the console references by the given
  handle using the text and background colours provided. If xlNone is used for
  the colours then the consoles default colours are used. Adds a Carriage Return
  at the end of each line.

  @precon  hndConsole must be a valid console handle.
  @postcon Outputs the given text to the console references by the given
           handle using the text and background colours provided.

  @param   hndConsole  as a THandle
  @param   strText     as a String as a constant
  @param   iTextColour as a TColor
  @param   iBackColour as a TColor
  @param   boolUpdateCursor as a Boolean

**)
Procedure OutputToConsoleLn(hndConsole : THandle; Const strText : String = '';
  iTextColour : TColor = clNone; iBackColour : TColor = clNone;
    boolUpdateCursor : Boolean = True);

Var
  sl : TStringList;
  i : Integer;
  wChars : Cardinal;

Begin
  sl := TStringList.Create;
  Try
    sl.Text := strText;
    If sl.Text = '' Then
      sl.Text := #13#10;
    For i := 0 to sl.Count - 1 Do
      Begin
        If CheckConsoleMode(hndConsole) Then
          Begin
            OutputToConsole(hndConsole, sl[i], iTextColour, iBackColour,
              boolUpdateCursor);
            Win32Check(WriteConsole(hndConsole, PChar(#13#10), 2, wChars, Nil));
          End Else
            WriteLn(sl[i]);
      End;
  Finally
    sl.Free;
  End;
End;

(**

  This function outputs the given text to the console references by the given
  handle using the text and background colours provided. If xlNone is used for
  the colours then the consoles default colours are used. DOES NOT add a
  carraige return at the end of each line.

  @precon  hndConsole must be a valid console handle.
  @postcon Outputs the given text to the console references by the given
           handle using the text and background colours provided.

  @param   hndConsole  as a THandle
  @param   strText     as a String as a constant
  @param   iTextColour as a TColor
  @param   iBackColour as a TColor
  @param   boolUpdateCursor as a Boolean

**)
Procedure OutputToConsole(hndConsole : THandle; Const strText : String = '';
  iTextColour : TColor = clNone; iBackColour : TColor = clNone;
    boolUpdateCursor : Boolean = True);

Var
  ConsoleInfo : TConsoleScreenBufferInfo;
  wChars : DWord;
  iChar : Integer;
  Attrs : Array of Word;
  OldPos : TCoord;
  NewPos : TCoord;
  iForeAttrColour, iBackAttrColour : Integer;
  strTABText : String;

Begin
  strTabText := StringReplace(strText, #9, #175, [rfReplaceAll]);
  If CheckConsoleMode(hndConsole) Then
    Begin
      Repeat
        Win32Check(GetConsoleScreenBufferInfo(hndConsole, ConsoleInfo));
        OldPos := ConsoleInfo.dwCursorPosition;
        NewPos := OldPos;
        Win32Check(WriteConsoleOutputCharacter(hndConsole, PChar(strTABText), Length(strTABText),
          ConsoleInfo.dwCursorPosition, wChars));
        SetLength(Attrs, wChars);
        iForeAttrColour := ForeGroundColour(iTextColour, ConsoleInfo.wAttributes And $0F);
        iBackAttrColour := BackGroundColour(iBackColour, ConsoleInfo.wAttributes And $F0);
        For iChar := 0 To wChars - 1 Do
          Attrs[iChar] := iForeAttrColour Or iBackAttrColour;
        Win32Check(WriteConsoleOutputAttribute(hndConsole, Attrs,
          Length(strTABText), ConsoleInfo.dwCursorPosition, wChars));
        Delete(strTABText, 1, wChars);
        Inc(NewPos.X, wChars);
        While NewPos.X >= ConsoleInfo.dwSize.X Do
          Begin
            Inc(NewPos.Y);
            Dec(NewPos.X, ConsoleInfo.dwSize.X);
          End;
        If strTABText <> '' Then
          Begin
            Win32Check(WriteConsole(hndConsole, PChar(#13#10), 2, wChars, Nil));
            Inc(NewPos.Y);
            NewPos.X := 0;
          End;
      Until strTABText = '';
      If boolUpdateCursor Then
        Begin
          // The only time the below fails is at the end of the buffer and a new
          // line is required, hence the new line on failure.
          If Not SetConsoleCursorPosition(hndConsole, NewPos) Then
            Win32Check(WriteConsole(hndConsole, PChar(#13#10), 2, wChars, Nil));
        End Else
          Win32Check(SetConsoleCursorPosition(hndConsole, OldPos));
    End Else
      Write(strTABText);
End;

(**

  This method builds the root key INI filename for the loading and saving of
  settings from the instance handle for the module.

  @precon  slParams must be a valid instance of a TStringList class.
  @postcon Builds the root key INI filename for the loading and saving of
           settings from the instance handle for the module.

  @param   slParams      as a TStringList
  @param   ExceptionProc as a TExceptionProcedure
  @return  a String

**)
Function BuildRootKey(slParams : TStringList;
  ExceptionProc : TExceptionProcedure) : String;

ResourceString
  strExpectedSquare = 'Expected "[" at position 3 in alternate INI file parameter.';
  strExpectedClosingSquare = 'Expected a closing "]" in alternate INI file parameter.';
  strPathDoesNotExist = 'The path "%s" does not exist for the alternate INI file.';

  (**

    This function parses the alternate INI filename from the parameter.

    @precon  None.
    @postcon Parses the alternate INI filename from the parameter.

    @param   strDefaultINI as a String
    @param   strParam      as a String
    @return  a String

  **)
  Function ParseAlternateINIFile(strDefaultINI, strParam : String) : String;

  Var
    i : Integer;
    strFileName : String;

  Begin
    Result := strDefaultINI;
    i := 3;
    If strParam[i] <> '[' Then
      If Assigned(ExceptionProc) Then
        Begin
          ExceptionProc(strExpectedSquare);
          Exit;
        End;
    Inc(i);
    strFileName := '';
    While (i <= Length(strParam)) And (strParam[i] <> ']') Do
      Begin
        strFileName := strFileName + strParam[i];
        Inc(i);
        If i > Length(strParam) Then
          If Assigned(ExceptionProc) Then
            Begin
              ExceptionProc(strExpectedClosingSquare);
              Exit;
            End;
      End;
    strFileName := ExpandUNCFileName(strFileName);
    If DirectoryExists(ExtractFilePath(strFileName)) Then
      Result := strFileName
    Else
      If Assigned(ExceptionProc) Then
        ExceptionProc(Format(strPathDoesNotExist, [ExtractFilePath(strFileName)]));
  End;

var
  i: Cardinal;
  strUserName: string;
  strComputerName: string;
  strModulePathAndName : String;
  Buffer : Array[0..MAX_PATH] Of Char;
  iParam : Integer;

begin
  i := 1024;
  SetLength(strUserName, i);
  GetUserName(@strUserName[1], i);
  Win32Check(LongBool(i));
  SetLength(strUserName, i - 1);
  i := 1024;
  SetLength(strComputerName, i);
  GetComputerName(@strComputerName[1], i);
  Win32Check(LongBool(i));
  SetLength(strComputerName, i);
  GetModuleFileName(hInstance, Buffer, MAX_PATH);
  strModulePathAndName := ChangeFileExt(StrPas(Buffer), '');
  Result := Format('%s Settings for %s on %s.INI', [strModulePathAndName,
    strUserName, strComputerName]);
  If AnsiCompareText(ExtractFileExt(StrPas(Buffer)), '.exe') = 0 Then
    If slParams <> Nil Then
      For iParam := 1 To ParamCount Do
        Begin
          If Length(ParamStr(iParam)) > 0 Then
            If ParamStr(iParam)[1] In ['-', '/'] Then
              If Length(ParamStr(iParam)) > 1 Then
                If ParamStr(iParam)[2] In ['@'] Then
                  Begin
                    Result := ParseAlternateINIFile(Result, ParamStr(iParam));
                    Continue;
                  End;
          slParams.Add(ParamStr(iParam));
        End;
end;

(**

  This method tokenise a macro string into token which are placed in the passed
  string list.

  @precon  slTokens must be a valid string list instance.
  @postcon Tokenise a macro string into token which are placed in the passed
           string list.

  @param   slTokens as a TStringList
  @param   strMacro as a String as a constant

**)
Procedure TokeniseMacro(slTokens : TStringList; Const strMacro : String);

Type
  TTokenType = (ttWhiteSpace, ttIdentifier, ttNumber, ttLineEnd,
    ttStringLiteral, ttSymbol, ttUnknown);

  Const
    strWhiteSpace : Set Of Char = [#32, #9];
    strLineEnds   : Set of Char = [#10, #13];

  (**

    This function returns the token type based on the current character and the
    last chacrater type.

    @precon  None.
    @postcon Returns the token type based on the current character and the
             last chacrater type.

    @param   Ch           as a Char
    @param   LastCharType as a TTokenType
    @return  a TTokenType

  **)
  Function GetTokenType(Ch : Char; LastCharType : TTokenType) : TTokenType;

  Const
    strTokenChars : Set Of Char = ['#', '_', 'a'..'z', 'A'..'Z'];
    strNumbers    : Set Of Char = ['0'..'9'];
    strSymbols    : Set Of Char = ['&', '(', ')', '*', '+', ',', '-', '.', '/',
      ':', ';', '<', '=', '>', '@', '[', ']', '^', '{', '}'];
    strQuotes     : Set Of Char = ['"', ''''];

  Begin
    If ch In strWhiteSpace Then
      Result := ttWhiteSpace
    Else If ch In strTokenChars Then
      Result := ttIdentifier
    Else If ch In strNumbers Then
      Begin
        Result := ttNumber;
        If LastCharType = ttIdentifier Then
          Result := ttIdentifier;
      End
    Else If ch In strLineEnds Then
      Result := ttLineEnd
    Else If ch In strQuotes Then
      Result := ttStringLiteral
    Else If ch In strSymbols Then
      Result := ttSymbol
    Else
      Result := ttUnknown;
  End;

  (**

    This method determines if the token is white space and returns true is so.

    @precon  None.
    @postcon Determines if the token is white space and returns true is so.

    @param   strToken as a String
    @return  a Boolean

  **)
  Function IsTokenWhiteSpace(strToken : String) : Boolean;

  Var
    i : Integer;

  Begin
    Result := True;
    For i := 1 To Length(strToken) Do
      If Not (strToken[i] In strWhiteSpace) And Not (strToken[i] In strLineEnds)Then
        Result := False;
  End;

Type
  (** State machine for block types. **)
  TBlockType = (btNoBlock, btStringLiteral, btCompoundSymbol);

Var
  strToken : String;
  CurCharType : TTokenType;
  LastCharType : TTokenType;
  BlockType : TBlockType;
  Ch : Char;
  LastToken : TTokenType;
  i : Integer;

Begin
  BlockType := btNoBlock;
  CurCharType := ttUnknown;
  strToken := '';
  LastToken := ttUnknown;

  For i := 1 To Length(strMacro) Do
    Begin
      LastCharType := CurCharType;
      Ch := strMacro[i];
      CurCharType := GetTokenType(Ch, LastCharType);

      If (LastCharType <> CurCharType) Then
        Begin
          If ((BlockType In [btStringLiteral]) And (CurCharType <> ttLineEnd)) Or
            (BlockType In [btCompoundSymbol]) Then
            strToken := ConCat(strToken, Ch)
          Else
            Begin
              If Not IsTokenWhiteSpace(strToken) Then
                Begin
                  If ((LastToken = ttNumber) And ((strToken = '.') Or (LastCharType = ttNumber))) Or
                    ((LastToken = ttStringLiteral) And (strToken[1] = '#')) Or
                    ((LastToken = ttStringLiteral) And (LastCharType = ttStringLiteral)) Then
                    Begin
                      strToken := ConCat(strToken, Ch);
                      LastToken := LastToken;
                    End Else
                    Begin
                      slTokens.Add(strToken);
                      strToken := '';
                      LastToken := LastCharType;
                    End;
                End;
             BlockType := btNoBlock;
             strToken := ConCat(strToken, Ch);
            End;
        End Else
        If (CurCharType = ttSymbol) And Not (BlockType = btStringLiteral) Then
          Begin
            slTokens.Add(strToken);
            strToken := Ch;
            LastToken := LastCharType;
          End Else
            strToken := ConCat(strToken, Ch);

      // Check for string literals
      If CurCharType = ttStringLiteral Then
        If BlockType = btStringLiteral Then
          BlockType := btNoBlock
        Else If BlockType = btNoBlock Then
          BlockType := btStringLiteral;

      If BlockType = btCompoundSymbol Then
        BlockType := btNoBlock;

      If Ch = #10 Then
        If BlockType In [btStringLiteral] Then
          BlockType := btNoBlock;
    End;
  If strToken <> '' Then
    slTokens.Add(strToken);
End;

(**

  This method parses a macro to find the command and filename. If found
  correctly this function returns true with the command and filename in the var
  parameters.

  @precon  strCommand must be an array of lowercase commands in alphanumeric
           order.
  @postcon Parses a macro to find the command and filename. If found correctly
           this function returns true with the command and filename in the var
           parameters.

  @param   strMacro      as a String as a constant
  @param   strCommand    as a String as a reference
  @param   strFileName   as a String as a reference
  @param   strCommands   as an Array Of String
  @param   ExceptionProc as a TExceptionProcedure
  @return  a Boolean

**)
Function ParseMacro(Const strMacro : String; var strCommand,
  strFileName : String; strCommands : Array Of String;
  ExceptionProc : TExceptionProcedure) : Boolean;

ResourceString
  strExceptionMsg =
    'Error (%s),'#13#10 +
    'Parsing Macro "%s".'#13#10 +
    #13#10 +
    'Syntax:'#13#10 +
    '  [Command(Drive:\Path\FileName.Extension)]';
  strUnexpectedEndOfMacro = 'Unexpected end of macro.';
  strExpectedOpeningBracket = 'Opening square bracket expected but found "%s".';
  strNotValidCommand = '"%s" is not a valid macro command.';
  strExpectedOpeningParenthesis = 'Opening parenthesis expected after command but found "%s".';
  strExpectedClosingParenthesis = 'Closing parenthesis expected after filename but found "%s".';
  strExpectedClosingBracket = 'Closing square bracket expected but found "%s".';
  strExpectedEndOfMacro = 'End of macro expected but found "%s"';

Var
  slTokens : TStringList;
  iToken : Integer;

  (**

    This procedure increments the token position is possible else raises an
    exception.

    @precon  None.
    @postcon Increments the token position is possible else raises an
             exception.

  **)
  Procedure IncTokenPos;

  Begin
    If iToken < slTokens.Count - 1 Then
      Inc(iToken)
    Else
      Raise Exception.Create(strUnexpectedEndOfMacro);
  End;

Begin
  Result := False;
  slTokens := TStringList.Create;
  Try
    Try
      TokeniseMacro(slTokens, strMacro);
      iToken := 0;
      If slTokens[iToken] <> '[' Then
        Raise Exception.CreateFmt(strExpectedOpeningBracket, [slTokens[iToken]]);
      IncTokenPos;
      If Not IsKeyWord(slTokens[iToken], strCommands) Then
        Raise Exception.CreateFmt(strNotValidCommand, [slTokens[iToken]]);
      strCommand := slTokens[iToken];
      IncTokenPos;
      If slTokens[iToken] <> '(' Then
        Raise Exception.CreateFmt(strExpectedOpeningParenthesis, [slTokens[iToken]]);
      IncTokenPos;
      strFileName := slTokens[iToken];
      If strFileName[1] In ['"', ''''] Then
        strFileName := Copy(strFileName, 2, Length(strFileName) - 1);
      If strFileName[Length(strFileName)] In ['"', ''''] Then
        strFileName := Copy(strFileName, 1, Length(strFileName) - 1);
      IncTokenPos;
      If slTokens[iToken] <> ')' Then
        Raise Exception.CreateFmt(strExpectedClosingParenthesis, [slTokens[iToken]]);
      IncTokenPos;
      If slTokens[iToken] <> ']' Then
        Raise Exception.CreateFmt(strExpectedClosingBracket, [slTokens[iToken]]);
      If iToken > slTokens.Count - 1 Then
        Raise Exception.CreateFmt(strExpectedEndOfMacro, [slTokens[iToken]]);
      Result := True;
    Except
      On E : Exception Do
        If Assigned(ExceptionProc) Then
          ExceptionProc(Format(strExceptionMsg, [E.Message, strMacro]));
    End;
  Finally
    slTokens.Free;
  End;
End;

(**

  This function creates a process with message handlers which must be
  implemented by the passed interface in order for the calling process to
  get messages from the process console and handle idle and abort.

  @precon  ProcMsgHndr must be a valid class implementing TDGHCreateProcessEvent.
  @postcon Creates a process with message handlers which must be implemented by
           the passed interface in order for the calling process to get messages
           from the process console and handle idle and abort.

  @param   Process     as a TProcessInfo
  @param   ProcMsgHndr as an IDGHCreateProcessEvents
  @return  an Integer

**)
Function  DGHCreateProcess(Process : TProcessInfo;
  ProcMsgHndr : IDGHCreateProcessEvents) : Integer;

Var
  boolAbort: Boolean;

  (**

    This prcoedure is called periodically by the process handler in order to
    retreive console output from the running process. Output everything from
    the console (pipe the anonymous pipe) but the last line as this may not be
    a complete line of information from the console (except if boolPurge is
    true).

    @precon  slLines must be a valid instance of a TStringList class to
             accumulate the console output.
    @postcon Outputs to the IDGHCreareProcessEvent interface output information
             from the console.

    @param   slLines as a TStringList
    @param   hRead   as a THandle
    @param   Purge   as a Boolean

  **)
  Procedure ProcessOutput(slLines : TStringList; hRead : THandle;
    Purge : Boolean = False);

  Var
    iTotalBytesInPipe : Cardinal;
    iBytesRead : Cardinal;
    strOutput : String;

  Begin
    ProcMsgHndr.IdleHandler;
    If boolAbort Then
      Begin
        If Assigned(ProcMsgHndr) Then
          ProcMsgHndr.ProcessMsgHandler(strUserAbort, boolAbort);
        Exit;
      End;
    Win32Check(PeekNamedPipe(hRead, Nil, 0, Nil, @iTotalBytesInPipe, Nil));
    If iTotalBytesInPipe > 0 Then
      Begin
        SetLength(strOutput, iTotalBytesInPipe);
        ReadFile(hRead, strOutput[1], iTotalBytesInPipe, iBytesRead, Nil);
        SetLength(strOutput, iBytesRead);
        slLines.Text := Copy(slLines.Text, 1, Length(slLines.text) - 2) + strOutput;
      End;
    // Use a string list to output each line except the last as it may not
    // be complete yet.
    If Assigned(ProcMsgHndr) Then
      While slLines.Count > 1 - Integer(Purge) Do
        Begin
          ProcMsgHndr.ProcessMsgHandler(slLines[0], boolAbort);
          slLines.Delete(0);
        End;
  End;

Var
  hRead, hWrite : THandle;
  slLines : TStringList;
  SecurityAttrib : TSecurityAttributes;
  StartupInfo : TStartupInfo;
  ProcessInfo : TProcessInformation;
  iExitCode : Cardinal;
  strBuffer : Array[0..MAX_PATH] Of Char;
  OpDirs : PAnsiChar;

Begin
  Result := 0;
  boolAbort := False;
  If Process.boolEnabled Then
    Try
      If Not FileExists(Process.strEXE) Then
        Begin
          StrPCopy(strBuffer, Process.strEXE);
          OpDirs := '';
          If PathFindOnPath(strBuffer, OpDirs) Then
            Process.strEXE := StrPas(strBuffer)
          Else
            Raise Exception.CreateFmt(strEXENotFound, [Process.strEXE]);
        End;
      If Not DirectoryExists(Process.strDir) Then
        Raise Exception.CreateFmt(strDirectoryNotFound, [Process.strDir]);
      FillChar(SecurityAttrib, SizeOf(SecurityAttrib), 0);
      SecurityAttrib.nLength := SizeOf(SecurityAttrib);
      SecurityAttrib.bInheritHandle := True;
      SecurityAttrib.lpSecurityDescriptor := nil;
      Win32Check(CreatePipe(hRead, hWrite, @SecurityAttrib, 4096));
      Try
        FillChar(StartupInfo, SizeOf(TStartupInfo), 0);
        StartupInfo.cb := SizeOf(TStartupInfo);
        StartupInfo.cb          := SizeOf(StartupInfo);
        StartupInfo.dwFlags     := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
        StartupInfo.wShowWindow := SW_HIDE;
        StartupInfo.hStdOutput  := hWrite;
        StartupInfo.hStdError   := hWrite;
        Win32Check(CreateProcess(PChar(Process.strEXE),
          PChar('"' + Process.strEXE + '" ' + Process.strParams), @SecurityAttrib,
          Nil, True, CREATE_NEW_CONSOLE, Nil, PChar(Process.strDir), StartupInfo,
          ProcessInfo));
        Try
          slLines := TStringList.Create;
          Try
            While WaitforSingleObject(ProcessInfo.hProcess, 50) = WAIT_TIMEOUT Do
              Begin
                ProcessOutput(slLines, hRead);
                If boolAbort Then
                  Begin
                    TerminateProcess(ProcessInfo.hProcess, 0);
                    Break;
                  End;
              End;
            ProcessOutput(slLines, hRead, True);
          Finally
            slLines.Free;
          End;
          If GetExitCodeProcess(ProcessInfo.hProcess, iExitCode) Then
            Inc(Result, iExitCode)
        Finally
          Win32Check(CloseHandle(ProcessInfo.hThread));
          Win32Check(CloseHandle(ProcessInfo.hProcess));
        End;
      Finally
        Win32Check(CloseHandle(hWrite));
        Win32Check(CloseHandle(hRead));
      End;
    Except
      On E : Exception Do
        If Assigned(ProcMsgHndr) Then
          Begin
            ProcMsgHndr.ProcessMsgHandler(E.Message, boolAbort);
            Inc(Result);
          End;
    End;
End;

(**


  This function returns true if the pattern matches the text.

  @precon  None.
  @postcon Returns true if the pattern matches the text.


  @param   strPattern as a String
  @param   strText    as a String
  @return  a Boolean

**)
Function Like(strPattern, strText : String) : Boolean;

Type
  TMatchType = (mtStart, mtEnd);
  TMatchTypes = Set Of TMatchType;

Var
  MatchTypes : TMatchTypes;
  sl : TStringList;
  i: Integer;
  iIndex : Integer;
  iPos: Integer;

Begin
  Result := False;
  MatchTypes := [];
  If Length(strPattern) = 0 Then
    Exit;
  If strPattern[1] <> '*' Then
    Include(MatchTypes, mtStart)
  Else
    Delete(strPattern, 1, 1);
  If strPattern[Length(strPattern)] <> '*' Then
    Include(MatchTypes, mtEnd)
  Else
    Delete(strPattern, Length(strPattern), 1);
  sl := TStringList.Create;
  Try
    For i := 1 To CharCount('*', strPattern) + 1 Do
      sl.Add(lowercase(GetField(strPattern, '*', i)));
    // Check start
    iIndex := 1;
    If sl.Count > 0 Then
      If mtStart In MatchTypes Then
        If AnsiCompareText(sl[0], Copy(strText, 1, Length(sl[0]))) <> 0 Then
          Exit
        Else
          Inc(iIndex, Length(sl[0]));
    // Check in between
    For i := Integer(mtStart In MatchTypes) To sl.Count - 1 - Integer(mtEnd In MatchTypes) Do
      Begin
        iPos := Pos(sl[i], lowercase(strText));
        If (iPos = 0) Or (iPos < iIndex) Then
          Exit;
        Inc(iIndex, iPos);
        Inc(iIndex, Length(sl[i]));
      End;
    // Check end
    If sl.Count > 0 Then
      If mtEnd In MatchTypes Then
        If AnsiCompareText(sl[sl.Count - 1], Copy(strText, Length(strText) -
          Length(sl[sl.Count - 1]) + 1, Length(sl[sl.Count - 1]))) <> 0 Then
          Exit;
    Result := True;
  Finally
    sl.Free;
  End;
End;

(**

  This method interpolates a colour for the specified percentage position 
  within the colour and position information passed. 

  @precon  None. 
  @postcon Interpolates a colour for the specified percentage position within 
           the colour and position information passed.. 

  @param   dblValue          as a Double
  @param   dblLowCriteria    as a Double
  @param   dblMiddleCriteria as a Double
  @param   dblUpperCriteria  as a Double
  @param   iLowColour        as a TColor
  @param   iMiddleColour     as a TColor
  @param   iHighColour       as a TColor
  @return  a TColor

**)
Function CalcColour(dblValue, dblLowCriteria, dblMiddleCriteria,
  dblUpperCriteria : Double; iLowColour, iMiddleColour, iHighColour : TColor) : TColor;

  (**

    This function calculate the intepolation of a single colour value between 2
    colours based on value for those colour positions.

    @precon  None.
    @postcon Returns the colour which is an interpolation between the input
             colours.

    @param   iLow     as a TColor
    @param   iHigh    as a TColor
    @param   iMask    as a TColor
    @param   dblLow   as a Double
    @param   dblValue as a Double
    @param   dblHigh  as a Double
    @return  a TColor

  **)
  Function InterpolateColour(iLow, iHigh, iMask : TColor; dblLow,
    dblValue, dblHigh : Double) : TColor;

  Var
    iColourDiff : TColor;

  Begin
    iColourDiff := iHigh And iMask - iLow And iMask;
    Result := Round(iLow And iMask + iColourDiff * (dblValue - dblLow) /
      (dblHigh - dblLow)) And iMask;
  End;

  (**

    This function calculate the intepolation of a colour value between 2
    colours based on value for those colour positions.

    @precon  None.
    @postcon Returns the colour which is an interpolation between the input
             colours.

    @param   iLow     as a TColor
    @param   iHigh    as a TColor
    @param   dblLow   as a Double
    @param   dblValue as a Double
    @param   dblHigh  as a Double
    @return  a TColor  

  **)
  Function InterpolateColours(iLow, iHigh : TColor; dblLow,
    dblValue, dblHigh : Double) : TColor;

  Begin
    Result :=
      InterpolateColour(iLow, iHigh, $FF0000, dblLow, dblValue, dblHigh) +
      InterpolateColour(iLow, iHigh, $00FF00, dblLow, dblValue, dblHigh) +
      InterpolateColour(iLow, iHigh, $0000FF, dblLow, dblValue, dblHigh);
  End;

Begin
  If dblValue <= dblLowCriteria Then
    Result := iLowColour
  Else If dblValue <= dblMiddleCriteria Then
    Result := InterpolateColours(
      ColorToRGB(iLowColour),
      ColorToRGB(iMiddleColour),
      dblLowCriteria,
      dblValue,
      dblMiddleCriteria)
  Else If dblValue <= dblUpperCriteria then
    Result := InterpolateColours(
      ColorToRGB(iMiddleColour),
      ColorToRGB(iHighColour),
      dblMiddleCriteria,
      dblValue,
      dblUpperCriteria)
  Else
    Result := iHighColour;
End;

(** Initialises the console more to Unknown to force a call to the Win32 API **)
Initialization
  ConsoleMode := cmUnknown;
End.
