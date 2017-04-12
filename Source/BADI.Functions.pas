(**

  This method contains functions that are used global through out the application.

  @Version 1.0
  @Author  David Hoyle.
  @Date    12 Apr 2017

**)
Unit BADI.Functions;

Interface

Uses
  SysUtils,
  Classes,
  Dialogs,
  Controls,
  BADI.Types,
  BADI.Base.Container,
  BADI.Generic.Parameter;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** An enumerate to determine whether a file is saved or locked (read only) **)
  TStatus = (msSaved, msLocked);
  (** A set to determine the saved / locked state of a file. **)
  TStatuses = Set Of TStatus;

  Procedure DisplayException(const strMsg : String); Overload;
  Procedure DisplayException(const strMsg : String; Const Params : Array Of Const); Overload;
  Function  IsKeyWord(Const strWord : String; Const strWordList : Array Of String): Boolean;
  Function  IsInSet(Const C : Char; Const strCharSet : TSetOfAnsiChar) : Boolean; {$IFDEF D2005} InLine; {$ENDIF}
  Function  PrologCode(Const strTemplate, strMethod : String; Const iPadding : Integer) : TStringList;
  Function  EpilogCode(Const strTemplate, strMethod : String; Const iPadding : Integer) : TStringList;
  Function  OutputCommentAndTag(Const C: TBaseContainer; Const iMaxWidth: Integer;
    Const boolShowHTML: Boolean): String;
  Function  BuildLangIndepRep(Const Param: TGenericParameter): String;
  Function  BADIImageIndex(Const iBADIImageIndex : TBADIImageIndex; Const AScope : TScope) : Integer;
  Procedure BuildNumber(Var iMajor, iMinor, iBugFix, iBuild : Integer);
  Function  BuildRootKey : String;
  Function  Like(Const strPattern, strText : String) : Boolean;
  Function  ConvertDate(Const strDate : String) : TDateTime;
  Function  GetField(Const strText : String; Const Ch : Char; Const iIndex : Integer;
    Const boolIgnoreQuotes : Boolean = True): String;
  Function  CharCount(Const cChar : Char; Const strText : String;
    Const boolIgnoreQuotes : Boolean = True) : Integer;
  Procedure LoadBADIImages(Const ilScopeImages : TImageList);

Implementation

Uses
  Windows,
  BADI.Constants,
  SHFolder,
  Graphics;

(**

  This procedure displays a exception message then aborts.

  @precon  None.
  @postcon Displays a exception message then aborts.

  @param   strMsg as a String as a constant

**)
Procedure DisplayException(const strMsg : String);

Begin
  ShowMessage('Exception:'#13#10#13#10 + StrMsg);
End;

(**

  This procedure displays a formatted exception message then aborts.

  @precon  None.
  @postcon Displays a formatted exception message then aborts.

  @param   strMsg as a String as a constant
  @param   Params as an Array Of Const as a Constant

**)
Procedure DisplayException(const strMsg : String; Const Params : Array Of Const);

Begin
  ShowMessage(Format('Exception:'#13#10#13#10 + StrMsg, Params));
End;

(**

  This function returns true if the given word is in the supplied word list. It uses a binary search
  , so the word lists need to be sorted.

  @precon  strWord is the word to be searches for in the word list and strWordList is a static
           array of words in lowercase and alphabetical order.
  @postcon Returns true if the word is found in the list.

  @param   strWord     as a String as a constant
  @param   strWordList as an Array Of String as a constant
  @return  a Boolean

**)
function IsKeyWord(Const strWord : String; Const strWordList : Array Of String): Boolean;

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
          Break;
        End;
    End;
end;

(**

  This function centralises the checking of characters in set for both AnsiChars and Unicode Chars
  so that the parser tokeniser are not riddled with conditional compilation statements.

  @precon  None.
  @postcon Checks to see if the char is in the set and returns true if so.

  @param   C          as a Char as a constant
  @param   strCharSet as a TSetOfAnsiChar as a constant
  @return  a Boolean

**)
Function IsInSet(Const C : Char; Const strCharSet : TSetOfAnsiChar) : Boolean;
  {$IFDEF D2005} InLine; {$ENDIF}

Begin
  {$IFNDEF D2009}
  Result := C In strCharSet;
  {$ELSE}
  Result := CharInSet(C, strCharSet);
  {$ENDIF}
End;

(**

  This procedure returns a string list containing the prolog element of code passed in the template
  parameter.

  @precon  strTemplate must contain the macro $METHODCODE$.
  @postcon Returns a string list containing the prolog element of code passed in the template
           parameter.

  @param   strTemplate as a String as a constant
  @param   strMethod   as a String as a constant
  @param   iPadding    as an Integer as a constant
  @return  a TStringList

**)
Function PrologCode(Const strTemplate, strMethod : String; Const iPadding : Integer) : TStringList;

Var
  strPadding : String;
  iLine : Integer;
  boolFound : Boolean;

Begin
  Result := TStringList.Create;
  Result.Text := StringReplace(strTemplate, strMethodName, strMethod, [rfReplaceAll]);
  //If Not Like('*' + strMethodCode + '*', strTemplate) Then
  //  Raise Exception.Create(strMethodCode + ' Not Found in Template.');
  boolFound := False;
  While Not boolFound And (Result.Count > 0) Do
    Begin
      If CompareText(Trim(Result[Result.Count - 1]), strMethodCode) = 0 Then
        boolFound := True;
      Result.Delete(Result.Count - 1);
    End;
  strPadding := StringOfChar(#32, iPadding);
  For iLine := 0 To Result.Count - 1 Do
    Result[iLine] := strPadding + Result[iLine];
End;

(**

  This procedure returns a string list containing the epilog element of code passed in the template
  parameter.

  @precon  strTemplate must contain the macro $METHODCODE$.
  @postcon Returns a string list containing the epilog element of code passed in the template
           parameter.

  @param   strTemplate as a String as a constant
  @param   strMethod   as a String as a constant
  @param   iPadding    as an Integer as a constant
  @return  a TStringList

**)
Function EpilogCode(const strTemplate, strMethod : String; Const iPadding : Integer) : TStringList;

Var
  strPadding : String;
  iLine : Integer;
  boolFound : Boolean;

Begin
  Result := TStringList.Create;
  Result.Text := StringReplace(strTemplate, strMethodName, strMethod, [rfReplaceAll]);
  If Not Like('*' + strMethodCode + '*', strTemplate) Then
    Raise Exception.Create(strMethodCode + ' Not Found in Template.');
  boolFound := False;
  While Not boolFound And (Result.Count > 0) Do
    Begin
      If CompareText(Trim(Result[0]), strMethodCode) = 0 Then
        boolFound := True;
      Result.Delete(0);
    End;
  strPadding := StringOfChar(#32, iPadding);
  For iLine := 0 To Result.Count - 1 Do
    Result[iLine] := strPadding + Result[iLine];
End;

(**

  This function outputs the comment or tag as a string missing out HTML tags if not required and any
  trialing whitespace.

  @precon  C must eb a valid instance of a TBaseContainer.
  @postcon Outputs the comment or tag as a string missing out HTML tags if not required and any
           trialing whitespace.

  @param   C            as a TBaseContainer as a constant
  @param   iMaxWidth    as an Integer as a constant
  @param   boolShowHTML as a Boolean as a constant
  @return  a String

**)
Function OutputCommentAndTag(Const C: TBaseContainer; Const iMaxWidth: Integer;
  Const boolShowHTML: Boolean): String;

Var
  iToken: Integer;
  iLength: Integer;
  strToken: String;

Begin
  Result := '';
  iLength := 0;
  For iToken := 0 To C.TokenCount - 1 Do
    If ((C.Tokens[iToken].TokenType In [ttHTMLStartTag, ttHTMLEndTag]) And boolShowHTML) Or
      Not(C.Tokens[iToken].TokenType In [ttHTMLStartTag, ttHTMLEndTag]) Then
      Begin
        If iLength + C.Tokens[iToken].Length > iMaxWidth Then
          Begin
            Result := Result + #13#10;
            iLength := 0;
          End;
        If Not((iLength = 0) And (C.Tokens[iToken].TokenType In [ttWhiteSpace])) Then
          Begin
            If C.Tokens[iToken].Token = '#' Then
              Begin
                iLength := 0;
                Result := Result + #13#10;
              End
            Else
              Begin
                strToken := C.Tokens[iToken].Token;
                If (Length(strToken) >= 2) And (strToken[1] = '@') And (strToken[2] = '@') Then
                  strToken := Copy(strToken, 2, Length(strToken) - 1);
                Result := Result + strToken;
              End;
          End;
        Inc(iLength, C.Tokens[iToken].Length);
      End;
End;

(**

  This method builds a language independant representation of the parameter.

  @precon  None.
  @postcon Returns a string language independant representation of the parameter.

  @param   Param as a TGenericParameter as a constant
  @return  a String

**)
function BuildLangIndepRep(Const Param: TGenericParameter): String;

begin
  Result := '';
  If Param.ParamType = Nil Then
    Exit;
  If Param.ArrayOf Then
    Result := 'Array Of ';
  Result := Result + Param.ParamType.AsString(False, False);
  Case Param.ParamModifier Of
    pamVar: Result := Result + ' as a reference';
    pamConst: Result := Result + ' as a constant';
    pamOut: Result := Result + ' as an out parameter';
  End;
end;

(**

  This function returns an integer index into the image list of icons for the treeview based on the
  base image index and the scope.

  @precon  None.
  @postcon An image index is returned.

  @param   iBADIImageIndex as a TBADIImageIndex as a constant
  @param   AScope          as a TScope as a constant
  @return  an Integer

**)
Function BADIImageIndex(Const iBADIImageIndex : TBADIImageIndex; Const AScope : TScope) : Integer;

Begin
  Result := Pred(Integer(iBADIImageIndex)) *
    (Integer(High(TScope)) - Integer(Low(TScope)) + 1) +
    Integer(AScope)
End;

(**

  This is a method which obtains information about the package from is
  version information with the package resources.

  @precon  None.
  @postcon Extracts and display the applications version number present within
           the EXE file.

  @param   iMajor  as an Integer as a reference
  @param   iMinor  as an Integer as a reference
  @param   iBugFix as an Integer as a reference
  @param   iBuild  as an Integer as a reference

**)
Procedure BuildNumber(var iMajor, iMinor, iBugFix, iBuild : Integer);

Var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
  strBuffer : Array[0..MAX_PATH] Of Char;

Begin
  { Build Number }
  GetModuleFilename(hInstance, strBuffer, MAX_PATH);
  VerInfoSize := GetFileVersionInfoSize(strBuffer, Dummy);
  If VerInfoSize <> 0 Then
    Begin
      GetMem(VerInfo, VerInfoSize);
      Try
        GetFileVersionInfo(strBuffer, 0, VerInfoSize, VerInfo);
        VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
        With VerValue^ Do
          Begin
            iMajor := dwFileVersionMS shr 16;
            iMinor := dwFileVersionMS and $FFFF;
            iBugFix := dwFileVersionLS shr 16;
            iBuild := dwFileVersionLS and $FFFF;
          End;
      Finally
        FreeMem(VerInfo, VerInfoSize);
      End;
    End;
End;

(**

  This function returns the users logon name as a String.

  @precon  None.
  @postcon Returns the users logon name as a String.

  @return  a String

**)
Function UserName : String;

Var
  i : Cardinal;

Begin
  i := 1024;
  SetLength(Result, i);
  GetUserName(@Result[1], i);
  Win32Check(LongBool(i));
  SetLength(Result, i - 1);
End;

(**

  This function returns the users computer name as a String.

  @precon  None.
  @postcon Returns the users computer name as a String.

  @return  a String

**)
Function ComputerName : String;

Var
  i : Cardinal;

Begin
  i := 1024;
  SetLength(Result, i);
  GetComputerName(@Result[1], i);
  Win32Check(LongBool(i));
  SetLength(Result, i);
End;

(**

  This method builds the root key INI filename for the loading and saving of
  settings from the instance handle for the module.

  @precon  slParams must be a valid instance of a TStringList class.
  @postcon Builds the root key INI filename for the loading and saving of
           settings from the instance handle for the module.

  @return  a String

**)
Function BuildRootKey : String;

ResourceString
  strINIPattern = '%s Settings for %s on %s.INI';
  strSeasonsFall = '\Season''s Fall\';

var
  strModuleName : String;
  strINIFileName : String;
  strUserAppDataPath : String;
  strBuffer : String;
  iSize : Integer;

{$IFDEF D0007}
// Delphi 7s SHFolder.pas file is missing this constant.
Const
  SHGFP_TYPE_CURRENT = 0; { current value for user, verify it exists }
{$ENDIF}

begin
  SetLength(strBuffer, MAX_PATH);
  iSize := GetModuleFileName(hInstance, PChar(strBuffer), MAX_PATH);
  SetLength(strBuffer, iSize);
  strModuleName := strBuffer;
  strINIFileName := ChangeFileExt(ExtractFileName(strBuffer), '');
  While (Length(strIniFilename) > 0) And
    (CharInSet(strIniFileName[Length(strIniFilename)], ['0'..'9'])) Do
    strIniFileName := Copy(strIniFileName, 1, Length(strIniFileName) - 1);
  strINIFileName :=  Format(strINIPattern, [strIniFileName, UserName, ComputerName]);
  SetLength(strBuffer, MAX_PATH);
  SHGetFolderPath(0, CSIDL_APPDATA Or CSIDL_FLAG_CREATE, 0, SHGFP_TYPE_CURRENT,
    PChar(strBuffer));
  strBuffer := StrPas(PChar(strBuffer));
  strUserAppDataPath := strBuffer + strSeasonsFall;
  If Not DirectoryExists(strUserAppDataPath) Then
    ForceDirectories(strUserAppDataPath);
  Result := strUserAppDataPath + strINIFileName;
end;

(**

  This routine returns the number of occurrances of the char found in the string .

  @precon  None.
  @postcon Returns the number of occurrances of the char found in the string.

  @param   cChar            as a Char as a constant
  @param   strText          as a String as a constant
  @param   boolIgnoreQuotes as a Boolean as a constant
  @return  an Integer

**)
Function CharCount(Const cChar : Char; Const strText : String;
  Const boolIgnoreQuotes : Boolean = True) : Integer;

Var
  iCount : Integer;
  boolInQuotes : Boolean;

Begin
  Result := 0;
  boolInQuotes := False;
  For iCount := 1 to Length(strText) Do
    Begin
      If Not boolIgnoreQuotes Then
        If strText[iCount] = '"' Then
          boolInQuotes := Not boolInQuotes;
      If strText[iCount] = cChar Then
        If Not boolInQuotes Then
          Inc(Result);
    End;
End;

(**

  This routine returns the position of the Nth occurrance of the character in the text.

  @precon  None.
  @postcon Returns the position of the Nth occurrance of the character in the text.

  @param   strText          as a String
  @param   Ch               as a Char
  @param   iIndex           as an Integer
  @param   boolIgnoreQuotes as a Boolean
  @return  an Integer

**)
Function PosOfNthChar(strText : String; Ch : Char; iIndex : Integer;
  boolIgnoreQuotes : Boolean = True): Integer;

Var
  i : Integer;
  iCount : Integer;
  boolInQuotes : Boolean;

Begin
  Result := 0;
  iCount := 0;
  boolInQuotes := False;
  For i := 1 To Length(strText) Do
    Begin
      If Not boolIgnoreQuotes Then
        If strText[i] = '"' Then
          boolInQuotes := Not boolInQuotes;
      If strText[i] = Ch Then
        If Not boolInQuotes Then
          Inc(iCount);
      If iIndex = iCount Then
        Begin
          Result := i;
          Exit;
        End;
    End;
End;

(**

  This function returns the contents of the specified field in the delimited text.

  @precon  None.
  @postcon Returns the contents of the specified field in the delimited text.

  @param   strText          as a String as a constant
  @param   Ch               as a Char as a constant
  @param   iIndex           as an Integer as a constant
  @param   boolIgnoreQuotes as a Boolean as a constant
  @return  a String

**)
Function GetField(Const strText : String; Const Ch : Char; Const iIndex : Integer;
  Const boolIgnoreQuotes : Boolean = True): String;

Var
  iNumOfFields : Integer;
  iStart, iEnd : Integer;

Begin
  Result := '';
  iNumOfFields := CharCount(Ch, strText, boolIgnoreQuotes) + 1;
  If iIndex = 1 Then
    Begin
      If iNumOfFields > 1  Then
        Begin
          iEnd := PosOfNthChar(strText, Ch, 1, boolIgnoreQuotes);
          Result := Copy(strText, 1, iEnd - 1);
        End Else
          Result := strText;
    End
  Else If (iIndex > 1) And (iIndex < iNumOfFields) Then
    Begin
      iStart := PosOfNthChar(strText, Ch, iIndex - 1, boolIgnoreQuotes);
      iEnd := PosOfNthChar(strText, Ch, iIndex, boolIgnoreQuotes);
      Result := Copy(strText, iStart + 1, iEnd - iStart - 1);
    End
  Else If iIndex = iNumOfFields Then
    Begin
      iStart := PosOfNthChar(strText, Ch, iIndex - 1, boolIgnoreQuotes);
      Result := Copy(strText, iStart + 1, Length(strText) - iStart);
    End;
End;

(**

  This function returns true if the pattern matches the text.

  @precon  None.
  @postcon Returns true if the pattern matches the text.

  @param   strPattern as a String as a constant
  @param   strText    as a String as a constant
  @return  a Boolean

**)
Function Like(Const strPattern, strText : String) : Boolean;

Type
  TMatchType = (mtStart, mtEnd);
  TMatchTypes = Set Of TMatchType;

Var
  MatchTypes : TMatchTypes;
  sl : TStringList;
  i: Integer;
  //iTokenIndex : Integer;
  iStartIndex : Integer;
  iPos: Integer;
  strModPattern : String;

Begin
  Result := False;
  MatchTypes := [];
  strModPattern := strPattern;
  If Length(strModPattern) = 0 Then
    Exit;
  If strModPattern = '*' Then
    Begin
      Result := True;
      Exit;
    End;
  If strModPattern[1] <> '*' Then
    Include(MatchTypes, mtStart)
  Else
    Delete(strModPattern, 1, 1);
  If Length(strModPattern) > 0 Then
    If strModPattern[Length(strModPattern)] <> '*' Then
      Include(MatchTypes, mtEnd)
    Else
      Delete(strModPattern, Length(strModPattern), 1);
  sl := TStringList.Create;
  Try
    For i := 1 To CharCount('*', strModPattern) + 1 Do
      sl.Add(lowercase(GetField(strModPattern, '*', i)));
    // Check start
    //iTokenIndex := 1;
    iStartIndex := 1;
    If sl.Count > 0 Then
      If mtStart In MatchTypes Then
        If CompareText(sl[0], Copy(strText, 1, Length(sl[0]))) <> 0 Then
          Exit
        Else
          Inc(iStartIndex, Length(sl[0]));
    // Check in between
    For i := Integer(mtStart In MatchTypes) To sl.Count - 1 - Integer(mtEnd In MatchTypes) Do
      Begin
        iPos := Pos(sl[i], lowercase(strText));
        If (iPos = 0) Or (iPos < iStartIndex) Then
          Exit;
        //Inc(iTokenIndex, iPos);
        Inc(iStartIndex, Length(sl[i]));
      End;
    // Check end
    If sl.Count > 0 Then
      If mtEnd In MatchTypes Then
        If CompareText(sl[sl.Count - 1], Copy(strText, Length(strText) -
          Length(sl[sl.Count - 1]) + 1, Length(sl[sl.Count - 1]))) <> 0 Then
          Exit;
    Result := True;
  Finally
    sl.Free;
  End;
End;

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
  {$IFNDEF D2009}
  Delimiters : Set Of Char = ['-', ' ', '\', '/', ':', '.'];
  {$ELSE}
  Delimiters : Set Of AnsiChar = ['-', ' ', '\', '/', ':', '.'];
  {$ENDIF}
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
      Raise EBADIParserError.CreateFmt(strErrMsg, [strDate]);
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
      For j := 1 To Length({$IFDEF DXE00}FormatSettings.{$ENDIF}ShortDateFormat) Do
        {$IFNDEF D2009}
        If ShortDateFormat[j] In Delimiters Then
        {$ELSE}
        If CharInSet({$IFDEF DXE00}FormatSettings.{$ENDIF}ShortDateFormat[j], Delimiters) Then
        {$ENDIF}
          AddToken(slFormat, str)
        Else
          str := str + {$IFDEF DXE00}FormatSettings.{$ENDIF}ShortDateFormat[j];
      AddToken(slFormat, str);
      // Remove day of week
      For j := slFormat.Count - 1 DownTo 0 Do
        {$IFNDEF D2009}
        If (slFormat[j][1] In ['d', 'D']) And (Length(slFormat[j]) > 2) Then
        {$ELSE}
        If (CharInSet(slFormat[j][1], ['d', 'D'])) And (Length(slFormat[j]) > 2) Then
        {$ENDIF}
          slFormat.Delete(j);
      For j := 0 To slFormat.Count - 1 Do
        Begin
          {$IFNDEF D2009}
          If slFormat[j][1] In ['d', 'D'] Then
          {$ELSE}
          If CharInSet(slFormat[j][1], ['d', 'D']) Then
          {$ENDIF}
            iIndex0 := j;
          {$IFNDEF D2009}
          If slFormat[j][1] In ['m', 'M'] Then
          {$ELSE}
          If CharInSet(slFormat[j][1], ['m', 'M']) Then
          {$ENDIF}
            iIndex1 := j;
          {$IFNDEF D2009}
          If slFormat[j][1] In ['y', 'Y'] Then
          {$ELSE}
          If CharInSet(slFormat[j][1], ['y', 'Y']) Then
          {$ENDIF}
            iIndex2 := j;
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
      {$IFNDEF D2009}
      If strDate[i] In Delimiters Then
      {$ELSE}
      If CharInSet(strDate[i], Delimiters) Then
      {$ENDIF}
        Begin
          AddToken(sl, strToken);
          {$IFNDEF D2009}
          If (strDate[i] In [':']) And (iTime = -1) Then
          {$ELSE}
          If (CharInSet(strDate[i], [':'])) And (iTime = -1) Then
          {$ENDIF}
            iTime := sl.Count - 1;
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
                If CompareText(Months[i], sl[iIndex1]) = 0 Then
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
        Raise EBADIParserError.CreateFmt(strErrMsg, [strDate]);
    End;
    // Output result.
    With recDate Do
      Begin
        If Not (iHour In [0..23]) Then
          Raise EBADIParserError.CreateFmt(strErrMsg, [strDate]);
        If Not (iMinute In [0..59]) Then
          Raise EBADIParserError.CreateFmt(strErrMsg, [strDate]);
        If Not (iSecond In [0..59]) Then
          Raise EBADIParserError.CreateFmt(strErrMsg, [strDate]);
        Result := EncodeTime(iHour, iMinute, iSecond, iMilli);
        If iYear * iMonth * iDay <> 0 Then
          Begin
            If Not (iDay In [1..31]) Then
              Raise EBADIParserError.CreateFmt(strErrMsg, [strDate]);
            If Not (iMonth In [1..12]) Then
              Raise EBADIParserError.CreateFmt(strErrMsg, [strDate]);
            Result := Result + EncodeDate(iYear, iMonth, iDay);
          End;
      End;
  Finally
    sl.Free;
  End;
End;

(**

  This method loads the BADI images from the DLLs resources into an image list multipling the number
  of images by the number of overlay masks for scope.

  @precon  None.
  @postcon The images are loaded, one for each scope.

  @param   ilScopeImages as a TImageList as a Constant

**)
Procedure LoadBADIImages(Const ilScopeImages : TImageList);

Var
  R: TRect;
  MainImage: TBitmap;
  ScopeImage: TBitmap;
  iImage: TBADIImageIndex;
  iScope: TScope;
  x: Integer;
  y: Integer;

Begin
  R := Rect(0, 0, 11, 11);
  MainImage := TBitMap.Create;
  Try
    ScopeImage := TBitmap.Create;
    Try
      For iImage := Succ(Low(TBADIImageIndex)) To High(TBADIImageIndex) Do
        For iScope := Low(TScope) To High(TScope) Do
          Begin
              MainImage.LoadFromResourceName(hInstance, BADIImageList[iImage].FResourceName);
              ScopeImage.LoadFromResourceName(hInstance, BADIScopeList[iScope].FResourceName);
              For x := 0 To 11 Do
                For y := 0 To 11 Do
                  If ScopeImage.Canvas.Pixels[x, y] <> BADIScopeList[iScope].FMaskColour Then
                    MainImage.Canvas.Pixels[x, y] := ScopeImage.Canvas.Pixels[x, y];
              ilScopeImages.AddMasked(MainImage, BADIImageList[iImage].FMaskColour);
          End;
    Finally
      ScopeImage.Free;
    End;
  Finally
    MainImage.Free;
  End;
End;

End.
