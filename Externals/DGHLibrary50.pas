(**
  
  This module contains numerous library function and procedure that can be used
  within many applications.

  @Version 1.0
  @Author  David Hoyle
  @Date    06 Nov 2004
  
**)
Unit DGHLibrary50;

Interface

Uses
  SysUtils;

Type
  (** This is a record that describes a vector using a distance and a
      bearing. **)
  TVector = Record
    dblDistance : Double;
    dblBearing  : Double;
  End;

  (** This is a custom exception for an invalid Degree Minutes Seconds
      value. **)
  EInvalidDMSFormatException = Class(Exception);

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

Implementation

Uses
  Math, ComObj, ShlObj, Classes;

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

End.
