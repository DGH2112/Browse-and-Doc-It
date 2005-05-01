(**

  This module contains numerous library functions, procedures and classes that
  can be used within many applications.

  @Version 1.0
  @Author  David Hoyle
  @Date    30 Apr 2005

**)
Unit DGHLibrary50;

Interface

Uses
  SysUtils, Classes;

Type
  (** This is a record that describes a vector using a distance and a
      bearing. **)
  TVector = Record
    dblDistance : Double;
    dblBearing  : Double;
  End;

  EMathException = Class(Exception);

  (** This is a custom exception for an invalid Degree Minutes Seconds
      value. **)
  EInvalidDMSFormatException = Class(Exception);

  TElementType = (etUnknown, etStraight, etCircular, etClothoid);

  THInfoType = (itSetout, itMeasure, itCompare);

  ELengthZeroOrNegativeException = Class(Exception);
  EZeroRadiusException = Class(Exception);
  EZeroRLValueExecption = Class(Exception);
  EBeforeAlignmentStartException = Class(Exception);
  EAfterAlignmentEndException = Class(Exception);

  (*
    THInfo Structure

    Setout : Bearing, Easting,  Northing;  or
    Measure: Bearing, Chainage, Offset;    or
    Compare: Bearing, Chainage, Distance;

  *)

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

  THCompInfo = Record
    dblEasting  : Double;
    dblNorthing : Double;
    dblBearing  : Double;
    dblChainage : Double;
    dblDistance : Double;
  End;

  TVInfo = Record
    dblLevel    : Double;
    dblGradient : Double;
  End;

  TInfo = Record
    HInfo : THInfo;
    VInfo : TVInfo;
  End;

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

  TVStraight = Record
    dblLevel    : Double;
    dblGradient : Double;
    dblChainage : Double;
    dblLength   : Double;
  End;

  TVCircular = Record
    dblLevel    : Double;
    dblGradient : Double;
    dblChainage : Double;
    dblLength   : Double;
    dblRadius   : Double;
  End;

  TMeasureInfo = Record
    dblChainage : Double;
    dblOffset   : Double;
    dblBearing  : Double;
    dblLevel    : Double;
    dblGradient : Double;
  End;

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

  THElements = Class(TList);

  TVElements = Class(TList);

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
    Property OnChange : TNotifyEvent Read FOnChange Write FOnChange;
    Property CoordinateError : Double read FCoordinateError write SetCoordinateError;
    Property BearingError : Double read FBearingError write SetBearingError;
    Property Modified: Boolean read FModified write SetModified Default False;
  End;

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
    Property OnChange : TNotifyEvent read FOnChange Write FOnChange;
    Property Modified: Boolean read FModified write SetModified Default False;
    Property LevelError : Double read FLevelError write SetLevelError;
    Property GradientError : Double read FGradientError write SetGradientError;
  End;

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
    Property HAlignment : THAlignmentCollection Read FHAlignment;
    Property VAlignment : TVAlignmentCollection Read FVAlignment;
  End;

  Function DecimalToDMS(dblBearing : Double) : String;
  Function DMSToDecimal(strBearing : String) : Double;
  Function DMSAsDecToDecimal(dblBearingAsDec : Double) : Double;
  Function AdjustBearing(dblBearing : Double) : Double;
  Function GetVector(dblStE, dblStN, dblEndE, dblEndN : Double) : TVector;
  Function ReduceBearing(dblBearing : Double) : Double;
  Function ExtractFileNameOnly(strFileName : String) : String;
  Function PosOfNthChar(Ch : Char; strText : String; iIndex : Integer): Integer;
  Function GetField(strText : String; Ch : Char; iIndex : Integer): String;
  Procedure MakeAssociation(Const strFileExt, strFileClass, strDescription,
    strDefaultIcon : String; bAlwaysShowExt, bQuickView : Boolean);
  Procedure MakeAssociatedVerb(const strFileExt, strVerb, strVerbCaption,
    strVerbCommand : String; bUseDDE : Boolean; Const strService, strTopic,
    strMacro, strMacroNotRunning : String);
  Function Capitalise(strText : String) : String;
  Function ConvertDate(Const strDate : String) : TDateTime;
  Function IsKeyWord(strWord : String; strWordList : Array Of String): Boolean;
  Function CharCount(cChar : Char; strText : String) : Integer;

  Function OctToBin(sDisplayNumber : String) : String;
  Function HexToBin(sDisplayNumber : String) : String;
  Function DecToBin(sDisplayNumber : String) : String;
  Function BinToOct(sDisplayNumber : String) : String;
  Function HexToOct(sDisplayNumber : String) : String;
  Function DecToOct(sDisplayNumber : String) : String;
  Function BinToHex(sDisplayNumber : String) : String;
  Function OctToHex(sDisplayNumber : String) : String;
  Function DecToHex(sDisplayNumber : String) : String;
  Function BinToDec(sDisplayNumber : String) : String;
  Function OctToDec(sDisplayNumber : String) : String;
  Function HexToDec(sDisplayNumber : String) : String;

Function Arcsin( X : Real ) : Real;    { Inverse Sine       }
Function Arccos( X : Real ) : Real;    { Inverse Cosine     }
Function Tan( X : Real ) : Real;       { Tan                }
Function Deg( X : Real ) : Real;       { Radians to Degrees }
Function Rad( X : Real ) : Real;       { Degrees to Radians }
Function Sub1(Z : Real) : Real;        { DMS to Decimal   }
Function DmsStr(X : Real) : String;    { Decimal to DMS as String }
Function DmsNum(X : Real) : String;    { Decimal to DMS as String Number }
Function DmsReal(X : Real) : Real;     { Decimal to DMS as Real   }
Function Squareroot(X : Real) : Real;  { Square Root Routine }
Function Pow(X, Y : Real) : Real;      { X to power of Y Routine }
Function Exponent(X : Real) : Real;    { Exponent Routine }
Function Lne(X : Real) : Real;         { Log(e) to Base (e) }
Function Inverse(X : Real) : Real;     { Inverse 1/X }
Function Log(X : Real) : Real;         { Log(10) to Base 10 }

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

(**

  This routine converts the decimal bearing into a text representation,
  i.e.

  12.3456 => 12°20'44.16"

  @precon  None.
  @postcon Converts the decimal bearing into a text representation.

  @param   dblBearing as a Double
  @return  a String

**)
Function DecimalToDMS(dblBearing: Double): String;

Var
  iDegrees : Integer;
  iMinutes : Integer;
  iSeconds : Integer;
  iHundreds : Integer;
  strSign : String;

begin
  If dblBearing < 0 Then
    Begin
      strSign := '-';
      dblBearing := Abs(dblBearing);
    End Else
      strSign := '';
  iDegrees := Trunc(dblBearing);
  iMinutes := Trunc((dblBearing - iDegrees) * 60);
  iSeconds := Trunc(((dblBearing - iDegrees) * 60 - iMinutes) * 60);
  iHundreds := Trunc((((dblBearing - iDegrees) * 60 - iMinutes) * 60 - iSeconds) * 100);
  Result := strSign + Format('%3d° %2.2d'' %2.2d.%2.2d"', [iDegrees, iMinutes,
    iSeconds, iHundreds]);
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

  @param   Ch      as a Char
  @param   strText as a String
  @param   iIndex  as an Integer
  @return  an Integer

**)
Function PosOfNthChar(Ch : Char; strText : String; iIndex : Integer): Integer;

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
      iStart := PosOfNthChar(Ch, strText, iIndex - 1);
      iEnd := PosOfNthChar(Ch, strText, iIndex);
      Result := Copy(strText, iStart + 1, iEnd - iStart - 1);
      Exit;
    End;
  If iIndex = iNumOfFields Then
    Begin
      iStart := PosOfNthChar(Ch, strText, iIndex - 1);
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
                If AnsiCompareText(Months[i], sl[iIndex1]) = 0 Then
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

  var
    iPreviousMode : Integer;

Const
  BinNums = ['-', '0', '1'];
  OctNums = BinNums + ['2', '3', '4', '5', '6', '7'];
  HexNums = OctNums + ['8', '9', 'A', 'B', 'C', 'D', 'E', 'F'];

Function OctToBin(sDisplayNumber : String) : String;

Begin
  OctToBin := DecToBin(OctToDec(sDisplayNumber));
End;

Function HexToBin(sDisplayNumber : String) : String;

Begin
  HexToBin := DecToBin(HexToDec(sDisplayNumber));
End;

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

Function BinToOct(sDisplayNumber : String) : String;

Begin
  BinToOct := DecToOct(BinToDec(sDisplayNumber));
End;

Function HexToOct(sDisplayNumber : String) : String;

Begin
  HexToOct := DecToOct(HexToDec(sDisplayNumber));
End;

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

Function BinToHex(sDisplayNumber : String) : String;

Begin
  BinToHex := DecToHex(BinToDec(sDisplayNumber));
End;

Function OctToHex(sDisplayNumber : String) : String;

Begin
  OctToHex := DecToHex(OctToDec(sDisplayNumber));
End;

Function DecToHex(sDisplayNumber : String) : String;

Begin
  DecToHex := IntToHex(Trunc(StrToFloat(sDisplayNumber)),0);
End;

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

Procedure MathError(MsgStr : String);

Begin
  Raise EMathException.Create(MsgStr);
End;

Function Arcsin( X : Real ) : Real;

Begin                          { Uses the relationship          }
  If X=-1.0 Then               { ArcSin(x) = Arctan |    x    | }
    Begin                      {                    |---------| }
      Arcsin:=-PI/2;           {                    | û(1-xý) | }
      Exit;
    End;
  If X=1.0 Then
    Begin
      Arcsin:=PI/2;
      Exit;
    End;
  If X > 1 Then
    Begin
      MathError('Invalid real number passed to Arc Sine Routine.');
      ArcSin := 0;
      Exit;
    End;
  Arcsin:=Arctan(X/Sqrt(1-Sqr(X)));
End;

Function Arccos( X : Real ) : Real;

Var
  Q : Real;

Begin                          { Uses the relationship          }
  If X=0 Then                  { ArcCos(x) = Arctan | û(1-xý) | }
    Begin                      {                    |---------| }
      ArcCos:=PI/2;            {                    |    x    | }
      Exit;
    End;
  If X > 1 Then
    Begin
      MathError('Invalid real number passed to Arc Cosine Routine.');
      ArcCos := 0;
      Exit;
    End;
  Q:=Arctan(Sqrt(1-Sqr(X))/X);
  If Q< 0.0 Then
    Q:=Q+PI;
  If (Q = 0) And (X = -1) Then
    Q := PI;
  Arccos:=Q;
End;

Function Tan( X : Real ) : Real;

Begin                                 { Uses the relationship }
  If (X = PI/2) Or (X = 3*PI/2) Then  { Tan = | Sin |         }
     Tan := 9E35                      {       |-----|         }
  Else                                {       | Cos |         }
      Tan:=Sin(X)/Cos(X);
End;

Function Rad( X : Real) : Real;
Begin
  Rad := X * PI / 180;           { Radians = Degrees * pi/ 180 }
End;

Function Deg( X : Real) : Real;
Begin
  Deg := X * 180 / PI;           { Degrees = Radians * 180 / pi}
End;

Function Sub1(Z : Real) : Real;

Var
  Z1,Z2,Z3,A : Real;

Begin                            { Converts Degrees.Minutes+Seconds }
  A:=Z+0.0000001;                {   a decimal of a degree into     }
  Z1:=Int(A);
  Z2:=Int((A-Z1)*100);
  Z3:=(((A-Z1)*100-Z2)*100);
  If (Z2>=60.0) Or (Z3>=60.0) Then
    Begin
      MathError('Invalid real number, Degrees.Minutes+Seconds.');
      Sub1 := 0;
      Exit;
    End;
  Sub1:=Z1+Z2/60+Z3/3600;
End;

{ X as a decimal of a degree }

Function Dmsstr(X : Real) : String;

Var
  B1,B2,B3 : Real;
  Dms1,Dms2,Dms3 : String[5];

Begin                       { Converts a decimal of a degree into   }
  B1:=Int(X);               {   Degrees.Minutes+Seconds as a String }
  B2:=Int((X-B1)*60);
  B3:=(((X-B1)*60-B2)*60);
  Str(B1:4:0,Dms1);
  Str(B2:2:0,Dms2);
  Str(B3:5:2,Dms3);
  Dmsstr:=Dms1+Chr(248)+Dms2+Chr(39)+Dms3+Chr(34);
End;

{ X as a decimal of a degree }

Function DmsNum(X : Real) : String;

Var
  B1,B2,B3,B : Real;
  Dms1 : String[12];

Begin                          { Converts a decimal of a degree into        }
  B1:=Int(X);                  {   Degrees.Minutes+Seconds as a real Number }
  B2:=Int((X-B1)*60);
  B3:=(((X-B1)*60-B2)*60);
  If (B2>=60.0) Or (B3>=60.0) Then
    Begin
      MathError('Invalid real number, Degrees.Minutes+Seconds.');
      DmsNum := '0';
      Exit;
    End;
  B := B1 + B2/100 + B3/10000;
  Str(B:4:6,Dms1);
  DmsNum:=Dms1;
End;

Function DmsReal(X : Real) : Real;

Var
  B1,B2,B3,B : Real;

  Begin                          { Converts a decimal of a degree into        }
  B1:=Int(X);                  {   Degrees.Minutes+Seconds as a real Number }
  B2:=Int((X-B1)*60);
  B3:=(((X-B1)*60-B2)*60);
  B := B1 + B2/100 + B3/10000;
  DmsReal := B;
End;

Function SquareRoot(X : Real) : Real;

Begin
  If X < 0 Then
    Begin
      MathError('Invalid real number passed to Square Root Routine.');
      SquareRoot := 0;
    End Else
      SquareRoot := Sqrt(X);
End;

Function Pow(X, Y : Real) : Real;

Begin
  If X = 0 Then
    Begin
      MathError('Invalid number passed to Power Routine.');
      Pow := 0;
      Exit;
    End;
  If X < 0 Then
    Pow := - Exp(Y * Ln(Abs(X)))
  Else
    Pow := Exp(Y * Ln(X));
End;

Function Exponent(X : Real) : Real;

Begin
  Exponent := Exp(X);
End;

Function Lne(X : Real) : Real;

Begin
  If X > 0 Then
    Lne := Ln(X)
  Else
    Begin
      MathError('Invalid number passed to Log(e) Routine.');
      Lne := 0;
    End;
End;

Function Inverse(X : Real) : Real;

Begin
  If X <> 0 Then
    Inverse := 1/X
  Else
    Begin
      MathError('Divide by Zero in the Inverse Routine.');
      Inverse := 0;
    End;
End;

Function Log(X : Real) : Real;         { Log(10) to Base 10 }

Begin
  If X > 0 Then
    Log := Ln(X)/Ln(10)
  Else
    Begin
      MathError('Invalid number passed to Log(10) Routine.');
      Log := 0;
    End;
End;

Const
  ValidDelimiters = ['(',')','*','/','+','-'];

{ -------------------------------------------------------------------------

   This routine finds the inner most pair of parenthesis and returns the
   start and end positions

   FindParenthesis(
     Equation    // Equation to parse as a string
     PStart      // Return the start of the inner most section as an integer
     PEnd        // Return the end of the inner most section as an integer
   );

  -------------------------------------------------------------------------- }

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

{ -------------------------------------------------------------------------

   This is the main routine for breaking down the equation in to pieces and
   evaluate those piece and substituting the result back into the eqaution
   until all expressions have been calculated.

   Evaluate(
     EquStr    // The expression to calculate
   );

  -------------------------------------------------------------------------- }

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

{ -------------------------------------------------------------------------

   If a function is suspected then the routine tries to evaluate the
   function

   GetFunction(
     Funct    // Function name as a string
     Number   // One or more comma delimited values for the function as a
              //   string
   );

  -------------------------------------------------------------------------- }

Function GetFunction(Funct, Number : String) : Double;

Var
  RealNumber : Double;
  ErrorCode : Integer;
  X, Y : Double;

Begin
  Val(Number, RealNumber, ErrorCode);
  If ErrorCode = 0 Then
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
        Result := Squareroot(RealNumber)
      Else If Funct = 'RAD' Then
        Result := Rad(RealNumber)
      Else If Funct = 'DEG' Then
        Result := Deg(RealNumber)
      Else If Funct = 'DMS' Then
        Result := DmsReal(RealNumber)
      Else If Funct = 'DEC' Then
        Result := Sub1(RealNumber)
      Else If Funct = 'EXP' Then
        Result := Exponent(RealNumber)
      Else If Funct = 'LN' Then
        Result := Lne(RealNumber)
      Else If Funct = 'INV' Then
        Result := Inverse(RealNumber)
      Else If Funct = 'INT' Then
        Result := Int(RealNumber)
      Else If Funct = 'LOG' Then
        Result := LOG(RealNumber)
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
          Val(Copy(Number,1,Pos(',', Number) - 1), X, ErrorCode);
          Val(Copy(Number,Pos(',', Number) + 1,Length(Number) - Pos(',', Number)), Y, ErrorCode);
          Result := Pow(X,Y);
        End
      Else If Funct = 'AND' Then
        Begin
          Val(Copy(Number,1,Pos(',',Number)-1),X,ErrorCode);
          Val(Copy(Number,Pos(',',Number)+1,Length(Number)-Pos(',',Number)),Y,ErrorCode);
          Result := (Trunc(X) And Trunc(Y));
        End
      Else If Funct = 'OR' Then
        Begin
          Val(Copy(Number,1,Pos(',',Number)-1),X,ErrorCode);
          Val(Copy(Number,Pos(',',Number)+1,Length(Number)-Pos(',',Number)),Y,ErrorCode);
          Result := (Trunc(X) Or Trunc(Y));
        End
      Else If Funct = 'XOR' Then
        Begin
          Val(Copy(Number,1,Pos(',',Number)-1),X,ErrorCode);
          Val(Copy(Number,Pos(',',Number)+1,Length(Number)-Pos(',',Number)),Y,ErrorCode);
          Result := (Trunc(X) Xor Trunc(Y));
        End
      Else If Funct = 'DIV' Then
        Begin
          Val(Copy(Number,1,Pos(',',Number)-1),X,ErrorCode);
          Val(Copy(Number,Pos(',',Number)+1,Length(Number)-Pos(',',Number)),Y,ErrorCode);
          Result := (Trunc(X) Div Trunc(Y));
        End
      Else If Funct = 'MOD' Then
        Begin
          Val(Copy(Number,1,Pos(',',Number)-1),X,ErrorCode);
          Val(Copy(Number,Pos(',',Number)+1,Length(Number)-Pos(',',Number)),Y,ErrorCode);
          Result := (Trunc(X) Mod Trunc(Y));
        End
      Else If Funct = 'SHL' Then
        Begin
          Val(Copy(Number,1,Pos(',',Number)-1),X,ErrorCode);
          Val(Copy(Number,Pos(',',Number)+1,Length(Number)-Pos(',',Number)),Y,ErrorCode);
          Result := (Trunc(X) Shl Trunc(Y));
        End
      Else If Funct = 'SHR' Then
        Begin
          Val(Copy(Number,1,Pos(',',Number)-1),X,ErrorCode);
          Val(Copy(Number,Pos(',',Number)+1,Length(Number)-Pos(',',Number)),Y,ErrorCode);
          Result := (Trunc(X) Shr Trunc(Y));
        End Else
          Raise Exception.CreateFmt('Invalid function [%s].', [Funct]);
End;

{ Main Equation Evaluation Engine }

Const
  Bases = ['B', 'O', 'H'];

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

{ -------------------------------------------------------------------------

   This is the main entry point, pass a string representing an express and
   a doube value is returned.

   EvauateEquation(
     EquStr   // Expression to evaluate as a string
   );         // Returns the result as a double

  -------------------------------------------------------------------------- }

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

function THBaseElement.GetEndChainage: Double;

begin
  Result := FChainage + FLength;
end;

function THBaseElement.GetEndPoint: THInfo;
begin
  Result := Setout(FChainage + FLength, 0);
end;

function THBaseElement.GetStartChainage: Double;

begin
  Result := FChainage;
end;

function THBaseElement.GetStartPoint: THInfo;
begin
  Result := Setout(FChainage, 0);
end;

{ -------------------------------------------------------------------------

   THStraightElement Class Methods

  ------------------------------------------------------------------------- }

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

function THStraightElement.ElementType: TElementType;

begin
  Result := etStraight;
end;

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

function THStraightElement.GetRadius(dblChainage: Double): Double;
begin
  Result := 999999.9000;
end;

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

function THCircularElement.ElementType: TElementType;
begin
  Result := etCircular;
end;

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

function THCircularElement.GetRadius(dblChainage: Double): Double;
begin
  Result := FRadius;
end;

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

{ -------------------------------------------------------------------------

   The principle of this procedure is to solve the intersection of the
   straight line and the transition curve in a local coordinate system
   around the origin of the transition curve.

   The first section of the code converts the global coordinates to the
   local coordinate system and then the solution is sort by an iteration
   using Newton-Raphson appling the straight line equation y=mx+c into
   the standard transition formulea - see the end of file for them.

   (
   );

  -------------------------------------------------------------------------- }
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

function THClothoidElement.ElementType: TElementType;
begin
  Result := etClothoid;
end;

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

function THClothoidElement.GetRadius(dblChainage: Double): Double;
begin
  If dblChainage <> FOChainage Then
    Result := FRLValue / Abs(dblChainage - FOChainage)
  Else
    Result := 999999.9000;
end;

function THClothoidElement.GetTheta(dblDistance : Double): Double;
begin
  Result := Sqr(dblDistance) / ( 2 * FRLValue);
end;

function THClothoidElement.GetX(dblDistance : Double): Double;

Var
  K : Double;

begin
  K := FRLValue;
  Result := Power(dblDistance, 3) / (6 * K) -
    Power(dblDistance ,7) / (336 * Power(K, 3)) +
    Power(dblDistance, 11) / (42240 * Power(K, 5));
end;

function THClothoidElement.GetY(dblDistance : Double): Double;

Var
  K : Double;

begin
  K := FRLValue;
  Result := dblDistance - Power(dblDistance, 5) / (40 * Sqr(K)) +
    Power(dblDistance, 9) / (3456 * Power(K, 4));
end;

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

function TVBaseElement.GetEndChainage: Double;
begin
  Result := FChainage + FLength;
end;

function TVBaseElement.GetEndPoint: TVInfo;
begin
  Result := Setout(FChainage + FLength);
end;

function TVBaseElement.GetStartChainage: Double;
begin
  Result := FChainage;
end;

function TVBaseElement.GetStartPoint: TVInfo;
begin
  Result := Setout(FChainage);
end;

{ -------------------------------------------------------------------------

   TVStraightElement Class Methods

 -------------------------------------------------------------------------- }

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

function TVStraightElement.ElementType: TElementType;
begin
  Result := etStraight;
end;

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

function TVStraightElement.GetRadius(dblChainage: Double): Double;
begin
  Result := 999999.9000;
end;

function TVStraightElement.Setout(dblChainage: Double): TVInfo;
begin
  Result.dblLevel := FLevel + FGradient * (dblChainage - FChainage);
  Result.dblGradient := FGradient;
end;

{ -------------------------------------------------------------------------

   TVCircularElement Class Methods

 -------------------------------------------------------------------------- }

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

function TVCircularElement.ElementType: TElementType;
begin
  Result := etCircular;
end;

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

function TVCircularElement.GetRadius(dblChainage: Double): Double;
begin
  Result := FRadius;
end;

function TVCircularElement.Setout(dblChainage: Double): TVInfo;
begin
  Result.dblLevel := FLevel + FGradient * (dblChainage - FChainage) +
    Sqr(dblChainage - FChainage) / (2 * FRadius);
  Result.dblGradient := FGradient + (dblChainage - FChainage) / FRadius;
end;

{ -------------------------------------------------------------------------

   THAlignmentCollection Class Methods

  ------------------------------------------------------------------------- }

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

function THAlignmentCollection.Count: Integer;

begin
  Result := FHElements.Count;
end;

constructor THAlignmentCollection.Create;
begin
  Inherited Create;
  FHElements := THElements.Create();
  FCoordinateError := 0.010;
  FBearingError := 10 / 3600.0;
  FModified := False;
end;

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

function THAlignmentCollection.ElementType(iElement: Integer): TElementType;

begin
  Result := THBaseElement(FHElements[iElement]).ElementType;
end;

function THAlignmentCollection.GetElementStart(iElement: Integer): THInfo;

begin
  Result := THBaseElement(FHElements[iElement]).GetStartPoint;
end;

function THAlignmentCollection.GetElementEnd(iElement: Integer): THInfo;

begin
  Result := THBaseElement(FHElements[iElement]).GetEndPoint;
end;

function THAlignmentCollection.GetEndChainage(iElement: Integer): Double;

begin
  Result := THBaseElement(FHElements[iElement]).GetEndChainage;
end;

function THAlignmentCollection.GetStartChainage(iElement: Integer): Double;

begin
  Result := THBaseElement(FHElements[iElement]).GetStartChainage;
end;

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

procedure THAlignmentCollection.SaveToFile(strFileName: String);

Var
  iniFile : TIniFile;
  iElement : Integer;
  recElement : THElement;

  Procedure WriteItem(iElement : Integer; strItem : String;
    dblValue : Double); Overload;

  Begin
    iniFile.WriteFloat(Format('HElement%d', [iElement + 1]), strItem, dblValue);
  End;

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

function THAlignmentCollection.GetCircular(iElement: Integer): THElement;

begin
  Result := THCircularElement(FHElements[iElement]).GetElementDetails;
end;

function THAlignmentCollection.GetClothoid(iElement: Integer): THElement;

begin
  Result := THClothoidElement(FHElements[iElement]).GetElementDetails;
end;

function THAlignmentCollection.GetStraight(iElement: Integer): THElement;

begin
  Result := THStraightElement(FHElements[iElement]).GetElementDetails;
end;

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

function THAlignmentCollection.GetRadius(iElement: Integer;
  dblChainage: Double): Double;
begin
  Result := THBaseElement(FHElements[iElement]).GetRadius(dblChainage);
end;

procedure THAlignmentCollection.SetCoordinateError(const Value: Double);
begin
  If FCoordinateError <> Value Then
    FCoordinateError := Value;
end;

procedure THAlignmentCollection.SetBearingError(const Value: Double);
begin
  If FBearingError <> Value Then
    FBearingError := Value;
end;

procedure THAlignmentCollection.SetModified(const Value: Boolean);
begin
  FModified := Value;
end;

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
          DecimalToDMS(dblBearing)]);

      If dblChain <= dblDist Then
        Begin
          Result := THBaseElement(FHElements[iElement]).Compare(
            dblEasting, dblNorthing, dblBearing);
          Exit;
        End;
    End;
  Raise Exception.CreateFmt(strAfterMsg, [dblEasting, dblNorthing,
    DecimalToDMS(dblBearing)]);
end;

function THAlignmentCollection.EndChainage: Double;
begin
  If Count <= 0 Then
    Raise Exception.Create('There is no horizontal alignment.');
  Result := THBaseElement(FHElements[Count - 1]).GetEndChainage;
end;

function THAlignmentCollection.StartChainage: Double;
begin
  If Count <= 0 Then
    Raise Exception.Create('There is no horizontal alignment.');
  Result := THBaseElement(FHElements[0]).GetStartChainage;
end;

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

procedure TVAlignmentCollection.Clear;

Var
  iElement : Integer;

begin
  FVElements.Pack;
  For iElement := 0 To FVElements.Count - 1 Do
    TVBaseElement(FVElements[iElement]).Free;
  FVElements.Clear;
end;

function TVAlignmentCollection.Count: Integer;
begin
  Result := FVElements.Count;
end;

constructor TVAlignmentCollection.Create;
begin
  Inherited Create;
  FVElements := TVElements.Create;
  FLevelError := 0.005;   // 5 mm
  FGradientError := 0.01; // 1% or 1 in 1000mm
end;

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

function TVAlignmentCollection.ElementType(iElement: Integer): TElementType;
begin
  Result := TVBaseElement(FVElements[iElement]).ElementType;
end;

function TVAlignmentCollection.EndChainage: Double;
begin
  If Count <= 0 Then
    Raise Exception.Create('There is no vertical alignment.');
  Result := TVBaseElement(FVElements[Count - 1]).GetEndChainage;
end;

function TVAlignmentCollection.GetCircular(iElement: Integer): TVCircular;
begin
  Result := TVCircularElement(FVElements[iElement]).GetElementDetails;
end;

function TVAlignmentCollection.GetElementEnd(iElement: Integer): TVInfo;
begin
  Result := TVBaseElement(FVElements[iElement]).GetEndPoint;
end;

function TVAlignmentCollection.GetElementStart(iElement: Integer): TVInfo;
begin
  Result := TVBaseElement(FVElements[iElement]).GetStartPoint;
end;

function TVAlignmentCollection.GetEndChainage(iElement: Integer): Double;
begin
  Result := TVBaseElement(FVElements[iElement]).GetEndChainage;
end;

function TVAlignmentCollection.GetRadius(iElement: Integer;
  dblChainage: Double): Double;
begin
  Result := TVBaseElement(FVElements[iElement]).GetRadius(dblChainage);
end;

function TVAlignmentCollection.GetStartChainage(iElement: Integer): Double;
begin
  Result := TVBaseElement(FVElements[iElement]).GetStartChainage;
end;

function TVAlignmentCollection.GetStraight(iElement: Integer): TVStraight;
begin
  Result := TVStraightElement(FVElements[iElement]).GetElementDetails;
end;

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

procedure TVAlignmentCollection.SaveToFile(strFileName: String);

Var
  iniFile : TIniFile;
  iElement : Integer;
  recStraight : TVStraight;
  recCircular : TVCircular;

  Procedure WriteItem(iElement : Integer; strItem : String;
    dblValue : Double); Overload;

  Begin
    iniFile.WriteFloat(Format('VElement%d', [iElement + 1]), strItem, dblValue);
  End;

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

procedure TVAlignmentCollection.SetGradientError(const Value: Double);
begin
  If FGradientError <> Value Then
    FGradientError := Value;
end;

procedure TVAlignmentCollection.SetLevelError(const Value: Double);
begin
  If FLevelError <> Value Then
    FLevelError := Value;
end;

procedure TVAlignmentCollection.SetModified(const Value: Boolean);
begin
  If FModified <> Value Then
    FModified := Value;
end;

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

function TVAlignmentCollection.StartChainage: Double;
begin
  If Count <= 0 Then
    Raise Exception.Create('There is no vertical alignment.');
  Result := TVBaseElement(FVElements[0]).GetStartChainage;
end;

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

procedure TStringAlignment.Clear;
begin
  HAlignment.Clear;
  VAlignment.Clear;
end;

constructor TStringAlignment.Create;
begin
  Inherited Create;
  FHAlignment := THAlignmentCollection.Create;
  FVAlignment := TVAlignmentCollection.Create;
end;

destructor TStringAlignment.Destroy;
begin
  FVAlignment.Free;
  FHAlignment.Free;
  inherited;
end;

procedure TStringAlignment.LoadFromFile(strFileName : String);
begin
  HAlignment.LoadFromFile(strFileName);
  VAlignment.LoadFromFile(strFileName);
end;

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

procedure TStringAlignment.SaveToFile(strFileName : String);
begin
  If FileExists(strFileName) Then
    DeleteFile(strFileName);
  HAlignment.SaveToFile(strFileName);
  VAlignment.SaveToFile(strFileName);
end;

function TStringAlignment.Setout(dblChainage, dblOffset: Double): TInfo;
begin
  Result.HInfo := HAlignment.Setout(dblChainage, dblOffset);
  Result.VInfo := VAlignment.Setout(dblChainage);
end;

End.
