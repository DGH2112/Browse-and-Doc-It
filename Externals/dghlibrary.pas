(**

  This module contains numerous library functions, procedures and classes that
  can be used within many applications.

  @Version 1.0
  @Author  David Hoyle
  @Date    07 Jan 2006

**)
Unit DGHLibrary;

Interface

Uses
  SysUtils, Classes, Windows;

Type
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

  (** This record defines a vertical straight element of an alignment. **)
  TVStraight = Record
    dblLevel    : Double;
    dblGradient : Double;
    dblChainage : Double;
    dblLength   : Double;
  End;

  (** This record defines a circular straight element of an alignment. **)
  TVCircular = Record
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
  Private
    FEasting  : Double;
    FNorthing : Double;
    FBearing  : Double;
    FChainage : Double;
    FLength   : Double;
  Protected
    Function ElementType : TElementType; Virtual; Abstract;
    Function GetStartPoint : THInfo; Virtual;
    Function GetEndPoint : THInfo; Virtual;
    Function GetStartChainage : Double; Virtual;
    Function GetEndChainage : Double; Virtual;
    Function GetRadius(dblChainage : Double) : Double; Virtual; Abstract;
    Function Setout(dblChainage, dblOffset : Double) : THInfo; Virtual; Abstract;
    Function Measure(dblEasting, dblNorthing : Double) : THInfo; Virtual; Abstract;
    Function Compare(dblEasting, dblNorthing, dblBearing : Double) : THCompInfo;
      Virtual; Abstract;
  End;

  (** This class defined a Straight Horizontal alignment element. **)
  THStraightElement = Class(THBaseElement)
  Protected
    Function ElementType : TElementType; Override;
    Function GetRadius(dblChainage : Double) : Double; Override;
    Function Setout(dblChainage, dblOffset : Double) : THInfo; Override;
    Function Measure(dblEasting, dblNorthing : Double) : THInfo; Override;
    Function Compare(dblEasting, dblNorthing, dblBearing : Double) : THCompInfo;
      Override;
    Function GetElementDetails : THElement;
    Constructor Create(dblEasting, dblNorthing, dblBearing, dblChainage,
      dblLength : Double); Virtual;
  End;

  (** This class defined a Circular Horizontal alignment element. **)
  THCircularElement = Class(THBaseElement)
  Private
    FRadius : Double;
  Protected
    Function ElementType : TElementType; Override;
    Function GetRadius(dblChainage : Double) : Double; Override;
    Function Setout(dblChainage, dblOffset : Double) : THInfo; Override;
    Function Measure(dblEasting, dblNorthing : Double) : THInfo; Override;
    Function Compare(dblEasting, dblNorthing, dblBearing : Double) : THCompInfo;
      Override;
    Function GetElementDetails : THElement;
    Constructor Create(dblEasting, dblNorthing, dblChainage, dblBearing,
      dblLength, dblRadius : Double; bCentre : Boolean); Virtual;
  End;

  (** This class defined a Clothoid Horizontal alignment element. **)
  THClothoidElement = Class(THBaseElement)
  Private
    FOChainage : Double;
    FRLValue : Double;
    Function GetX(dblDistance : Double) : Double;
    Function GetY(dblDistance : Double) : Double;
    Function GetTheta(dblDistance : Double) : Double;
  Protected
    Function ElementType : TElementType; Override;
    Function Setout(dblChainage, dblOffset : Double) : THInfo; Override;
    Function Measure(dblEasting, dblNorthing : Double) : THInfo; Override;
    Function Compare(dblEasting, dblNorthing, dblBearing : Double) : THCompInfo;
      Override;
    Function GetRadius(dblChainage : Double) : Double; Override;
    Function GetElementDetails : THElement;
    Constructor Create(dblOEasting, dblONorthing, dblOChainage, dblStChainage,
      dblOBearing, dblLength, dblRLValue : Double); Virtual;
    Constructor CreateFalse(dblEasting, dblNorthing, dblChainage,
      dblBearing, dblLength, dblStRadius, dblEndRadius : Double); Virtual;
  End;

  (** This class is the base class in the heiracrhy for the vertical
      alignments classes. **)
  TVBaseElement = Class
  Private
    FLevel    : Double;
    FGradient : Double;
    FChainage : Double;
    FLength   : Double;
  Protected
    Function ElementType : TElementType; Virtual; Abstract;
    Function GetStartPoint : TVInfo; Virtual;
    Function GetEndPoint : TVInfo; Virtual;
    Function GetStartChainage : Double; Virtual;
    Function GetEndChainage : Double; Virtual;
    Function GetRadius(dblChainage : Double) : Double; Virtual; Abstract;
    Function Setout(dblChainage : Double) : TVInfo; Virtual;
      Abstract;
  End;

  (** This class defined a Straight Vertical element of an alignment. **)
  TVStraightElement = Class(TVBaseElement)
  Private
  Protected
    Constructor Create(dblLevel, dblGradient, dblChainage, dblLength : Double);
      Virtual;
    Function ElementType : TElementType; Override;
    Function GetRadius(dblChainage : Double) : Double; Override;
    Function Setout(dblChainage : Double) : TVInfo; Override;
    Function GetElementDetails : TVStraight;
  End;

  (** This class defined a Circular Vertical element of an alignment. **)
  TVCircularElement = Class(TVBaseElement)
  Private
    FRadius : Double;
  Protected
    Constructor Create(dblLevel, dblGradient, dblChainage, dblLength,
      dblRadius : Double); Virtual;
    Function ElementType : TElementType; Override;
    Function GetRadius(dblChainage : Double) : Double; Override;
    Function Setout(dblChainage : Double) : TVInfo; Override;
    Function GetElementDetails : TVCircular;
  End;

  (** Forward declaration of the class type THElements **)
  THElements = Class(TList);

  (** Forward declaration of the class type TVElements **)
  TVElements = Class(TList);

  (** This class represents a collection of horizontal alignment elements. **)
  THAlignmentCollection = Class
  Private
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
      dblLength: Double); Virtual;
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
  Private
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
    Function GetStraight(iElement : Integer) : TVStraight; Virtual;
    Function GetCircular(iElement : Integer) : TVCircular; Virtual;
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
  Private
    FHAlignment : THAlignmentCollection;
    FVAlignment : TVAlignmentCollection;
  Public
    Constructor Create; Virtual;
    Destructor Destroy; Override;
    Procedure SaveToFile(strFileName : String); Virtual;
    Procedure LoadFromFile(strFileName : String); Virtual;
    Procedure Clear;
    Function Setout(dblChainage, dblOffset : Double) : TInfo; Virtual;
    Function Measure(dblEasting, dblNorthing, dblLevel : Double) : TMeasureInfo;
      Virtual;
    Function Compare(dblEasting, dblNorthing, dblBearing,
      dblLevel : Double) : TCompareInfo; Virtual;
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

Implementation

Uses
  Math, ComObj, ShlObj, IniFiles;

Const
  (** A constant to define the type of based that can be used with number in
      eqautions. **)
  Bases = ['B', 'O', 'H'];
  (** The numeric number in base 2 **)
  BinNums = ['-', '0', '1'];
  (** The numeric number in base 8 **)
  OctNums = BinNums + ['2', '3', '4', '5', '6', '7'];
  (** The numeric number in base 16 **)
  HexNums = OctNums + ['8', '9', 'A', 'B', 'C', 'D', 'E', 'F'];
  (** A list of valid equation delimiters. **)
  ValidDelimiters = ['(',')','*','/','+','-'];

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

begin
  iDegs := Trunc(dblBearingAsDec);
  iMins := Trunc((dblBearingAsDec - iDegs) * 100);
  dblSecs := (((dblBearingAsDec - iDegs) * 100) - iMins) * 100;
  Result := iDegs + iMins / 60.0 + dblSecs / 3600;
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

  @param   strFileExt     as a String constant
  @param   strFileClass   as a String constant
  @param   strDescription as a String constant
  @param   strDefaultIcon as a String constant
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


  @param   strFileExt         as a String constant
  @param   strVerb            as a String constant
  @param   strVerbCaption     as a String constant
  @param   strVerbCommand     as a String constant
  @param   bUseDDE            as a Boolean
  @param   strService         as a String constant
  @param   strTopic           as a String constant
  @param   strMacro           as a String constant
  @param   strMacroNotRunning as a String constant

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
  l := 0;
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

  @param   strDate as a String
  @return  a TDateTime

**)
Function ConvertDate(Const strDate : String) : TDateTime;

Type
  (** This is a record that defined the date and time for a date. **)
  TDateRec = Record
    iDay, iMonth, iYear, iHour, iMinute, iSecond : Word;
  End;

Const
  strErrMsg = 'Can not convert the date "%s" to a valid TDateTime value.';
  Delimiters : Set Of Char = ['-', ' ', '\', '/', ':'];
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
    If i <> 0 Then Raise Exception.CreateFmt(strErrMsg, [strDate]);
    If Delete Then sl.Delete(iIndex);
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
    slFormat := TstringList.Create;
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
          If (strDate[i] = ':') And (iTime = -1) Then iTime := sl.Count - 1;
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
      If sl.Count <> 0 Then Raise Exception.CreateFmt(strErrMsg, [strDate]);
    End;
    // Output result.
    With recDate Do
      Begin
        If Not (iHour In [0..23]) Then Raise Exception.CreateFmt(strErrMsg, [strDate]);
        If Not (iMinute In [0..59]) Then Raise Exception.CreateFmt(strErrMsg, [strDate]);
        If Not (iSecond In [0..59]) Then Raise Exception.CreateFmt(strErrMsg, [strDate]);
        Result := EncodeTime(iHour, iMinute, iSecond, 0);
        If iYear * iMonth * iDay <> 0 Then
          Begin
            If Not (iDay In [1..31]) Then Raise Exception.CreateFmt(strErrMsg, [strDate]);
            If Not (iMonth In [1..12]) Then Raise Exception.CreateFmt(strErrMsg, [strDate]);
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
          iResult := iResult + StrToInt(sDisplayNumber[iCount]) * Trunc(Power(2, iPower));
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
          iResult := iResult + StrToInt(sDisplayNumber[iCount]) * Trunc(Power(8, iPower));
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
            '0'..'9' : iNumber := StrToInt(sDisplayNumber[iCount]);
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
        Result := Log2(RealNumber)
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
      Else If Funct = 'BIN' Then
        Result := StrToFloat(DecToBin(FloatToStr(RealNumber)))
      Else If Funct = 'OCT' Then
        Result := StrToFloat(DecToOct(FloatToStr(RealNumber)))
      Else If Funct = 'HEX' Then
        Result := StrToFloat(DecToHex(FloatToStr(RealNumber)))
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

  @param   strEquation as a String constant
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
            While (Not (strCurEquation[iFStart] In ValidDelimiters)) And (iFStart > 0) Do
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
  B := GetVector(FEasting, FNorthing, dblEasting, dblNorthing);
  B.dblBearing := AdjustBearing(B.dblBearing);
  Result.dblChainage := FChainage +
    Sin(DegToRad(dblBearing) - (DegToRad(B.dblBearing) + PI)) /
    Sin(DegToRad(FBearing) - DegToRad(dblBearing)) * B.dblDistance;
  Result.dblDistance := Sin(DegToRad(B.dblBearing) - DegToRad(FBearing)) /
    Sin(DegToRad(FBearing) - DegToRad(dblBearing)) * B.dblDistance;
  Result.dblBearing := FBearing;
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
  Inherited Create();
  FEasting  := dblEasting;
  FNorthing := dblNorthing;
  FBearing  := dblBearing;
  FChainage := dblChainage;
  FLength   := dblLength;
  If FLength <= 0 Then
    Raise ELengthZeroOrNegativeException.Create(
      'The length of an element can not be Zero or Negative.');
end;

(**

  This function returns the type of element as a straight element.

  @precon  None.
  @postcon Returns the type of element as a straight element.

  @return  a TElementType

**)
function THStraightElement.ElementType: TElementType;

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
      dblEasting := FEasting;
      dblNorthing := FNorthing;
      dblBearing := FBearing;
      dblChainage := FChainage;
      dblLength := FLength;
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
  Vector := GetVector(FEasting, FNorthing, dblEasting, dblNorthing);
  Result.dblChainage := FChainage + Vector.dblDistance *
    Cos(DegToRad(FBearing - Vector.dblBearing));
  Result.dblOffset := Vector.dblDistance *
    Sin(DegToRad(Vector.dblBearing - FBearing));
  Result.dblBearing := FBearing;
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
  Vector := GetVector(0, FChainage, dblOffset, dblChainage);
  Result.dblEasting := FEasting + Vector.dblDistance *
    Sin(DegToRad(FBearing + Vector.dblBearing));
  Result.dblNorthing := FNorthing + Vector.dblDistance *
    Cos(DegToRad(FBearing + Vector.dblBearing));
  Result.dblBearing := FBearing;
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
      dblT := FBearing + 270;
      dblP := 90;
    End Else
    Begin
      dblT := FBearing + 90;
      dblP := 270;
    End;
  dblS := Sqrt(Sqr(FEasting - dblEasting) + Sqr(FNorthing - dblNorthing));
  If dblS <> 0 Then
    dblA := ArcCos((FNorthing - dblNOrthing) / dblS)
  Else
    dblA := 0;
  If (FEasting - dblEasting) < 0 Then
    dblA := PI * 2 - dblA;
  dblK := 1;
  dblQ := ArcSin(dblS * Sin(dblA - DegToRad(dblBearing)) / (Abs(FRadius)));
  dblE := DegToRad(dblBearing) - dblQ;

  Repeat
    dblI := FEasting + Abs(FRadius) * Sin(dblE);
    dblJ := FNorthing + Abs(FRadius) * Cos(dblE);
    { If Dist is close to Radius this become invalid }
    If Abs(Abs(FRadius) - dblS) > 0.0001 Then
      dblDist := Sin(PI - (dblA - DegToRad(dblBearing)) -
        (DegToRad(dblBearing) - dblE)) * dblS / Sin(dblQ)
    Else
      dblDist := 0;;
    dblBz := ArcCos((dblJ - FNorthing) / Abs(FRadius));
    If (dblI - FEasting) < 0  Then
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
    If (dblCz > 0) And (dblCz + FChainage < GetEndChainage) then
      Break;
    if dblK < 0 then
      Break;
    dblE := DegToRad(dblBearing) + dblQ + PI;
    dblK := -1;
  Until False;

  Result.dblEasting := dblI;
  Result.dblNorthing := dblJ;
  Result.dblChainage := FChainage + dblCz;
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

begin
  Inherited Create();
  FChainage := dblChainage;
  FBearing  := dblBearing;
  FRadius   := dblRadius;
  If FRadius = 0 Then
    Raise EZeroRadiusException.Create(
      'A horizontal Circular must have a non-zero radius.');
  FLength := dblLength;
  If FLength <= 0 Then
    Raise ELengthZeroOrNegativeException.Create(
      'The length of an element can not be Zero or Negative.');
  If bCentre Then
    Begin
      FEasting  := dblEasting;
      FNorthing := dblNorthing;
    End Else
    Begin
      If FRadius < 0 Then
        dblAdjust := -90
      Else
        dblAdjust := +90;
      FEasting := dblEasting + Abs(FRadius) * Sin(DegToRad(FBearing + dblAdjust));
      FNorthing := dblNorthing + Abs(FRadius) * Cos(DegToRad(FBearing + dblAdjust));
    End;
end;

(**

  This function returns an element type of circular.

  @precon  None.
  @postcon Returns an element type of circular.

  @return  a TElementType

**)
function THCircularElement.ElementType: TElementType;
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
      dblEasting := FEasting;
      dblNorthing := FNorthing;
      dblBearing := FBearing;
      dblChainage := FChainage;
      dblLength := FLength;
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
  Vector := GetVector(FEasting, FNorthing, dblEasting, dblNorthing);
  If FRadius > 0 Then
    Begin
      dblB := Vector.dblBearing + 90;
      If dblB > 180 Then
        dblB := dblB - 360;
      While dblB < FBearing Do
        dblB := dblB + 360;
      dblZ := (dblB - FBearing) / 360 * FRadius * 2 * PI;
      If dblZ > 2 * PI * Abs(FRadius) Then
        dblZ := dblZ - 2 * PI * Abs(FRadius);
      Result.dblBearing := AdjustBearing(Vector.dblBearing + 90);
      Result.dblOffset := FRadius - Vector.dblDistance;
      Result.dblChainage := FChainage + dblZ;
    End Else
    Begin
      dblB := Vector.dblBearing - 90;
      If dblB < -180 Then
        dblB := dblB + 360;
      While dblB > FBearing Do
        dblB := dblB - 360;
      dblZ := (FBearing - dblB) / 360 * FRadius * 2 * PI;
      If dblZ > 2 * PI * Abs(FRadius) Then
        dblZ := dblZ - 2 * PI * Abs(FRadius);
      Result.dblBearing := AdjustBearing(Vector.dblBearing - 90);
      Result.dblOffset := Vector.dblDistance - Abs(FRadius);
      Result.dblChainage := FChainage - dblZ;
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
  dblB := (dblChainage - FChainage) / (2 * PI * FRadius) * 360;
  Result.dblBearing := AdjustBearing(FBearing + dblB);
  If FRadius > 0.0 Then
    Begin
      dblR := FRadius - dblOffset;
      Result.dblEasting := FEasting + dblR * Sin(DegToRad(FBearing + dblB - 90));
      Result.dblNorthing := FNorthing + dblR * Cos(DegToRad(FBearing + dblB - 90));
    End Else
    Begin
      dblR := Abs(FRadius) + dblOffset;
      Result.dblEasting := FEasting + dblR * Sin(DegToRad(FBearing + dblB + 90));
      Result.dblNorthing := FNorthing + dblR * Cos(DegToRad(FBearing + dblB + 90));
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
  dblSS := Sqrt(Sqr(dblEasting - FEasting) + Sqr(dblNorthing - FNorthing));
  If dblSS <> 0 Then
    dblBS := Arccos((dblNorthing - FNorthing) / dblSS)
  Else
    dblBS := 0;
  If dblEasting - FEasting < 0 Then
    dblBS := PI * 2 - dblBS;
  dblEO := dblSS * Sin(dblBS - DegToRad(FBearing));
  dblNO := dblSS * Cos(dblBS - DegToRad(FBearing));
  dblM := Tan(PI/2 - (DegToRad(dblBearing) - DegToRad(FBearing)));
  dblAA := dblNO - Tan(PI/2 - (DegToRad(dblBearing) - DegToRad(FBearing))) * dblEO;
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
      Raise Exception.Create('Clothoid Compare: Loop Detected Comparing.');
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
  If Cos(DegToRad(dblBearing + 90 - dblBS)) > 0 Then
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

  @param   dblOEasting   as a Double
  @param   dblONorthing  as a Double
  @param   dblOChainage  as a Double
  @param   dblStChainage as a Double
  @param   dblOBearing   as a Double
  @param   dblLength     as a Double
  @param   dblRLValue    as a Double

**)
constructor THClothoidElement.Create(dblOEasting, dblONorthing,
  dblOChainage, dblStChainage, dblOBearing, dblLength, dblRLValue: Double);
begin
  Inherited Create();
  FEasting        := dblOEasting;
  FNorthing       := dblONorthing;
  FOChainage      := dblOChainage;
  FChainage       := dblStChainage;
  FBearing        := dblOBearing;
  FLength         := dblLength;
  If FLength <= 0 Then
    Raise ELengthZeroOrNegativeException.Create(
      'The length of an element can not be Zero or Negative.');
  FRLValue        := dblRLValue;
  If FRLValue = 0 Then
    Raise EZeroRLValueExecption.Create(
      'A horizontal clothoid must have a non-zero RL value.');
end;

(**

  This is the constructor method for the THClothoidElement class.

  @precon  This constructor creates a transition element with a false origin.
  @postcon The class is created.

  @param   dblEasting   as a Double
  @param   dblNorthing  as a Double
  @param   dblChainage  as a Double
  @param   dblBearing   as a Double
  @param   dblLength    as a Double
  @param   dblStRadius  as a Double
  @param   dblEndRadius as a Double

**)
constructor THClothoidElement.CreateFalse(dblEasting, dblNorthing, dblChainage,
      dblBearing, dblLength, dblStRadius, dblEndRadius : Double);

Var
  dblZ : Double;
  dblX : Double;
  dblY : Double;
  dblTheta : Double;

begin
  Inherited Create();
  FLength := dblLength;
  If FLength <= 0 Then
    Raise ELengthZeroOrNegativeException.Create(
      'The length of an element can not be Zero or Negative.');
  If dblStRadius + dblEndRadius = 0 Then
    Raise EZeroRadiusException.Create(
      'A horizontal clothoid must have non-zero Start and End Radius values.');
  If dblStRadius = 0 Then
    Begin
      (*
        This means the starting point if is the origin.
      *)
      FEasting        := dblEasting;
      FNorthing       := dblNorthing;
      FOChainage      := dblChainage;
      FChainage       := dblChainage;
      FBearing        := dblBearing;
      FLength         := dblLength;
      If FLength <= 0 Then
        Raise ELengthZeroOrNegativeException.Create(
          'The length of an element can not be Zero or Negative.');
      FRLValue        := dblLength * dblEndRadius;
      If FRLValue = 0 Then
        Raise EZeroRLValueExecption.Create(
          'A horizontal clothoid must have a non-zero RL value.');
    End
  Else If dblEndRadius = 0 Then
    Begin
      (*
        This means the end point is the origina and needs calcuating.
      *)
      FChainage       := dblChainage;
      FLength         := dblLength;
      FOChainage      := dblChainage + dblLength;
      If FLength <= 0 Then
        Raise ELengthZeroOrNegativeException.Create(
          'The length of an element can not be Zero or Negative.');
      FRLValue        := dblLength * dblStRadius;
      If FRLValue = 0 Then
        Raise EZeroRLValueExecption.Create(
          'A horizontal clothoid must have a non-zero RL value.');
      dblX := GetX(-dblLength);
      dblY := GetY(-dblLength);
      dblTheta := GetTheta(-dblLength);
      FBearing := AdjustBearing(dblBearing + RadToDeg(dblTheta));
      FEasting := dblEasting + dblX * Sin(DegToRad(FBearing + 90)) -
        dblY * Sin(DegToRad(FBearing));
      FNorthing := dblNorthing + dblX * Cos(DegToRad(FBearing + 90)) -
        dblY * Cos(DegToRad(FBearing));
    End Else
    Begin
      dblZ := FLength * dblEndRadius / (dblStRadius - dblEndRadius);
      FRLValue := Abs(dblZ) * dblStRadius;
      FChainage := dblChainage;
      FOChainage := dblChainage - dblZ;
      dblX := GetX(dblZ);
      dblY := GetY(dblZ);
      dblTheta := GetTheta(dblZ);
      If dblZ < 0 Then
        Begin
          dblX := -dblX;
          dblTheta := -dblTheta;
        End;
      FBearing := AdjustBearing(dblBearing - RadToDeg(dblTheta));
      FEasting := dblEasting - dblY * Sin(DegToRad(FBearing)) -
        dblX * Sin(DegToRad(FBearing + 90));
      FNorthing := dblNorthing - dblY * Cos(DegToRad(FBearing)) -
        dblX * Cos(DegToRad(FBearing + 90));
    End;
end;

(**

  This function returns the type of element as Clothoid.

  @precon  None.
  @postcon Returns the type of element as Clothoid.

  @return  a TElementType

**)
function THClothoidElement.ElementType: TElementType;
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
      dblEasting := FEasting;
      dblNorthing := FNorthing;
      dblBearing := FBearing;
      dblChainage := FOChainage;
      dblLength := FLength;
      dblRLValue := FRLValue;
      dblStChainage := FChainage;
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

begin
  dblC := FOChainage;
  Repeat
    HData := Setout(dblC, 0);
    Vector := GetVector(HData.dblEasting, HData.dblNorthing, dblEasting,
      dblNorthing);
    dblDelta := Sin(DegToRad(HData.dblBearing + 90 - Vector.dblBearing)) *
      Vector.dblDistance;
    dblO := Cos(DegToRad(HData.dblBearing + 90 - Vector.dblBearing)) *
      Vector.dblDistance;
    dblC := dblC + dblDelta;
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
  dblOBearing := DegToRad(FBearing);
  dblX := GetX(dblChainage - FOChainage);
  dblY := GetY(dblChainage - FOChainage);
  dblTheta := GetTheta(dblChainage - FOChainage);
  If dblY < 0.0 Then
    Begin
      dblX := dblX * -1;
      dblTheta := dblTheta * -1;
    End;
  Result.dblEasting := FEasting + dblY * Sin(dblOBearing) +
    dblX * Sin(dblOBearing + PI / 2) +
    dblOffset * Sin(dblOBearing + PI / 2 + dblTheta);
  Result.dblNorthing := FNorthing + dblY * Cos(dblOBearing) +
    dblX * Cos(dblOBearing + PI / 2) +
    dblOffset * Cos(dblOBearing + PI / 2 + dblTheta);
  Result.dblBearing := AdjustBearing(RadToDeg(dblTheta + dblOBearing));
end;

{ -------------------------------------------------------------------------

   TVBaseElement Class Methods

 -------------------------------------------------------------------------- }

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
  Inherited Create;
  FLevel    := dblLevel;
  FGradient := dblGradient;
  FChainage := dblChainage;
  FLength   := dblLength;
  If FLength <= 0 Then
    Raise ELengthZeroOrNegativeException.Create(
      'The length of an element can not be Zero or Negative.');
end;

(**

  This function returns the element type as straight.

  @precon  None.
  @postcon Returns the element type as straight.

  @return  a TElementType

**)
function TVStraightElement.ElementType: TElementType;
begin
  Result := etStraight;
end;

(**

  This is a getter method for the ElementDetails property.

  @precon  None.
  @postcon Returns the elements details.

  @return  a TVStraight

**)
function TVStraightElement.GetElementDetails: TVStraight;
begin
  With Result Do
    Begin
      dblLevel    := FLevel;
      dblGradient := FGradient;
      dblChainage := FChainage;
      dblLength   := FLEngth;
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
  Result.dblLevel := FLevel + FGradient * (dblChainage - FChainage);
  Result.dblGradient := FGradient;
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
  Inherited Create;
  FLevel    := dblLevel;
  FGradient := dblGradient;
  FChainage := dblChainage;
  FLength   := dblLength;
  If FLength <= 0 Then
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
function TVCircularElement.ElementType: TElementType;
begin
  Result := etCircular;
end;

(**

  This is a getter method for the ElementDetails property.

  @precon  None.
  @postcon Returns the details of the elements construction.

  @return  a TVCircular

**)
function TVCircularElement.GetElementDetails: TVCircular;
begin
  With Result Do
    Begin
      dblLevel    := FLevel;
      dblGradient := FGradient;
      dblChainage := FChainage;
      dblLength   := FLEngth;
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
  Result.dblLevel := FLevel + FGradient * (dblChainage - FChainage) +
    Sqr(dblChainage - FChainage) / (2 * FRadius);
  Result.dblGradient := FGradient + (dblChainage - FChainage) / FRadius;
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
    dblStChainage, dblBearing, dblLength, dblRLValue);
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
  FHClothiod := THClothoidElement.CreateFalse(dblEasting, dblNorthing,
    dblChainage, dblBearing, dblLength, dblStRadius, dblEndRadius);
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
  FBearingError := 10 / 3600.0;
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
  Result := THBaseElement(FHElements[iElement]).GetStartPoint;
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
  Result := THBaseElement(FHElements[iElement]).GetEndPoint;
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
  Result := THBaseElement(FHElements[iElement]).GetEndChainage;
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
  Result := THBaseElement(FHElements[iElement]).GetStartChainage;
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
            recElement := THStraightElement(FHElements[iElement]).GetElementDetails;
            WriteItem(iElement, 'Type', Integer(ElementType(iElement)));
            WriteItem(iElement, 'Easting', recElement.dblEasting);
            WriteItem(iElement, 'Northing', recElement.dblNorthing);
            WriteItem(iElement, 'Bearing', recElement.dblBearing);
            WriteItem(iElement, 'Chainage', recElement.dblChainage);
            WriteItem(iElement, 'Length', recElement.dblLength);
          End;
        etCircular:
          Begin
            recElement := THCircularElement(FHElements[iElement]).GetElementDetails;
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
            recElement := THClothoidElement(FHElements[iElement]).GetElementDetails;
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
  Result := THCircularElement(FHElements[iElement]).GetElementDetails;
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
  Result := THClothoidElement(FHElements[iElement]).GetElementDetails;
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
  Result := THStraightElement(FHElements[iElement]).GetElementDetails;
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

**)
procedure THAlignmentCollection.UpdateClothoid(iElement : Integer; dblEasting,
  dblNorthing, dblBearing, dblOChainage, dblStChainage, dblRLValue,
  dblLength: Double);

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
  FHElements[iElement] := THClothoidElement.Create(dblEasting, dblNorthing,
    dblOChainage, dblStChainage, dblBearing, dblLength, dblRLValue);
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
  Result := THBaseElement(FHElements[iElement]).GetRadius(dblChainage);
end;

(**

  This is a setter method for the CoordinateError property.

  @precon  None.
  @postcon Sets the Coordinate error tolerance.

  @param   Value as a Double constant

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

  @param   Value as a Double constant

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

  @param   Value as a Boolean constant

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
    Raise Exception.CreateFmt('The chainage and offset [%12.4f, %12.4f] lies ' +
      'before the alignment start.', [dblChainage, dblOffset]);
  If dblChainage > GetEndChainage(Count - 1) Then
    Raise Exception.CreateFmt('The chainage and offset [%12.4f, %12.4f] lies ' +
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
      HInfo := THBaseElement(FHElements[iElement]).GetStartPoint;
      Vector := GetVector(HInfo.dblEasting, HInfo.dblNorthing, dblEasting,
        dblNorthing);
      If Cos(DegToRad(Vector.dblBearing - HInfo.dblBearing)) <= 0 Then
        Begin
          If iElement = 0 Then
            Raise EBeforeAlignmentStartException.CreateFmt(
              'These coordinates [%12.4f, %12.4f] lie before the alignment start.',
              [dblEasting, dblNorthing]);
          Result := THBaseElement(FHElements[iElement - 1]).Measure(dblEasting,
            dblNorthing);
          Exit;
        End;
    End;
  { Check last element }
  HInfo := THBaseElement(FHElements[Count - 1]).GetEndPoint;
  Vector := GetVector(HInfo.dblEasting, HInfo.dblNorthing, dblEasting,
    dblNorthing);
  If Cos(DegToRad(Vector.dblBearing - HInfo.dblBearing)) <= 0 Then
    Begin
      Result := THBaseElement(FHElements[Count - 1]).Measure(dblEasting,
        dblNorthing);
      Exit;
    End Else
      Raise EAfterAlignmentEndException.CreateFmt(
          'These coordinates [%12.4f, %12.4f] lie beyond the alignment end.',
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
  strBeforeMsg = 'This intersection [%12.4f, %12.4f, %15s] lies before the ' +
    'alignment start.';
  strAfterMsg = 'This intersection [%12.4f, %12.4f, %15s] lies after the ' +
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
  Result := THBaseElement(FHElements[Count - 1]).GetEndChainage;
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
  Result := THBaseElement(FHElements[0]).GetStartChainage;
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
  Result := TVBaseElement(FVElements[Count - 1]).GetEndChainage;
end;

(**

  This method returns the details of the indexed circular element.

  @precon  iElement must be a valid element index.
  @postcon Returns the details of the indexed circular element.

  @param   iElement as an Integer
  @return  a TVCircular

**)
function TVAlignmentCollection.GetCircular(iElement: Integer): TVCircular;
begin
  Result := TVCircularElement(FVElements[iElement]).GetElementDetails;
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
  Result := TVBaseElement(FVElements[iElement]).GetEndPoint;
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
  Result := TVBaseElement(FVElements[iElement]).GetStartPoint;
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
  Result := TVBaseElement(FVElements[iElement]).GetEndChainage;
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
  Result := TVBaseElement(FVElements[iElement]).GetRadius(dblChainage);
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
  Result := TVBaseElement(FVElements[iElement]).GetStartChainage;
end;

(**

  This method returns the straight element details for the given indexed element.

  @precon  iElement must be a valid element index.
  @postcon Returns the straight element details for the given indexed element.

  @param   iElement as an Integer
  @return  a TVStraight

**)
function TVAlignmentCollection.GetStraight(iElement: Integer): TVStraight;
begin
  Result := TVStraightElement(FVElements[iElement]).GetElementDetails;
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
  recStraight : TVStraight;
  recCircular : TVCircular;

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
            recStraight := TVStraightElement(FVElements[iElement]).GetElementDetails;
            WriteItem(iElement, 'Type', Integer(ElementType(iElement)));
            WriteItem(iElement, 'Level', recStraight.dblLevel);
            WriteItem(iElement, 'Gradient', recStraight.dblGradient);
            WriteItem(iElement, 'Chainage', recStraight.dblChainage);
            WriteItem(iElement, 'Length', recStraight.dblLength);
          End;
        etCircular:
          Begin
            recCircular := TVCircularElement(FVElements[iElement]).GetElementDetails;
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

  @param   Value as a Double constant

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

  @param   Value as a Double constant

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

  @param   Value as a Boolean constant

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
  Result := TVBaseElement(FVElements[0]).GetStartChainage;
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
  @param   dblLevel    as a Double
  @return  a TMeasureInfo

**)
function TStringAlignment.Measure(dblEasting, dblNorthing, dblLevel: Double): TMeasureInfo;

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
  @param   dblLevel    as a Double
  @return  a TCompareInfo

**)
function TStringAlignment.Compare(dblEasting, dblNorthing, dblBearing,
  dblLevel: Double): TCompareInfo;

Var
  HInfo : THCompInfo;
  VInfo : TVInfo;

begin
  HInfo := HAlignment.Compare(dblEasting, dblNorthing, dblBearing);
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
    Result := Format('%d°%2.2d''%2.2d.%2.2d"', [iDegrees, iMinutes, iSeconds, iHundreds]);
End;

End.
