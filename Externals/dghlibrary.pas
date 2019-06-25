(**

  This module contains numerous library functions, procedures and classes that
  can be used within many applications.

  @Version 1.0
  @Author  David Hoyle
  @Date    21 Jun 2019

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2019  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

**)
Unit DGHLibrary;

Interface

Uses
  SysUtils,
  Classes,
  Windows,
  Graphics;

{$INCLUDE '..\Source\CompilerDefinitions.inc'}

Type
  (** A custom exception for converting string dates to actual dates. **)
  EDateConversionException = Class(Exception);

  (** This is a procedure type for handling Exception messages in ParseMacro. **)
  TExceptionProcedure = Procedure(strExceptionMsg : String) Of Object;

  Function GetField(strText : String; Ch : Char; iIndex : Integer;
    boolIgnoreQuotes : Boolean = True): String;
  Function IsKeyWord(strWord : String; strWordList : Array Of String): Boolean;
  Function BuildRootKey(slParams : TStringList;
    ExceptionProc : TExceptionProcedure) : String;
  Function Like(strPattern, strText : String) : Boolean;
  Function CalcColour(dblValue, dblLowCriteria, dblMiddleCriteria,
    dblUpperCriteria : Double; iLowColour, iMiddleColour,
    iHighColour : TColor) : TColor;

Implementation

Uses
  SHFolder, IniFiles {$IFNDEF D2005}, FileCtrl {$ENDIF};

(**

  This routine returns the number of occurrances of the char found in the string
  .

  @precon  None.
  @postcon Returns the number of occurrances of the char found in the string.

  @param   cChar            as a Char
  @param   strText          as a String
  @param   boolIgnoreQuotes as a Boolean
  @return  an Integer

**)
Function CharCount(cChar : Char; strText : String;
  boolIgnoreQuotes : Boolean = True) : Integer;

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

  This routine returns the position of the Nth occurrance of the character in
  the text.

  @precon  None.
  @postcon Returns the position of the Nth occurrance of the character in the
           text.

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

  This function returns the contents of the specified field in the delimited
  text.

  @precon  None.
  @postcon Returns the contents of the specified field in the delimited text.

  @param   strText          as a String
  @param   Ch               as a Char
  @param   iIndex           as an Integer
  @param   boolIgnoreQuotes as a Boolean
  @return  a String

**)
Function GetField(strText : String; Ch : Char; iIndex : Integer;
    boolIgnoreQuotes : Boolean = True): String;

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

  This procedure searches for old INI files and moves them to a new directory.

  @precon  None.
  @postcon Searches for old INI files and moves them to a new directory.

  @param   strOldPath as a String
  @param   strNewPath as a String
  @param   strPattern as a String

**)
Procedure MoveOldINIFiles(strOldPath, strNewPath, strPattern : String);

Var
  iResult : Integer;
  recSearch : TSearchRec;

Begin
  iResult := FindFirst(strOldPath + strPattern, faAnyFile, recSearch);
  Try
    While iResult = 0 Do
      Begin
        MoveFile(PChar(strOldPath + recSearch.Name),
          PChar(strNewPath + recSearch.Name));
        iResult := FindNext(recSearch);
      End;
  Finally
    SysUtils.FindClose(recSearch);
  End;
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
  strINIPattern = '%s Settings for %s on %s.INI';
  strSeasonsFall = '\Season''s Fall\';

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
  strModuleName : String;
  strINIFileName : String;
  strUserAppDataPath : String;
  strBuffer : String;
  iParam : Integer;
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
  MoveOldINIFiles(ExtractFilePath(strModuleName), strUserAppDataPath,
    ChangeFileExt(strINIFileName, '.*'));
  If Like('*.exe', ExtractFileExt(strModuleName)) Then
    If slParams <> Nil Then
      For iParam := 1 To ParamCount Do
        Begin
          If Length(ParamStr(iParam)) > 0 Then
            {$IFNDEF D2009}
            If ParamStr(iParam)[1] In ['-', '/'] Then
            {$ELSE}
            If CharInSet(ParamStr(iParam)[1], ['-', '/']) Then
            {$ENDIF}
              If Length(ParamStr(iParam)) > 1 Then
                {$IFNDEF D2009}
                If ParamStr(iParam)[2] In ['@'] Then
                {$ELSE}
                If CharInSet(ParamStr(iParam)[2], ['@']) Then
                {$ENDIF}
                  Begin
                    Result := ParseAlternateINIFile(Result, ParamStr(iParam));
                    Continue;
                  End;
          slParams.Add(ParamStr(iParam));
        End;
end;

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
  //iTokenIndex : Integer;
  iStartIndex : Integer;
  iPos: Integer;

Begin
  Result := False;
  MatchTypes := [];
  If Length(strPattern) = 0 Then
    Exit;
  If strPattern = '*' Then
    Begin
      Result := True;
      Exit;
    End;
  If strPattern[1] <> '*' Then
    Include(MatchTypes, mtStart)
  Else
    Delete(strPattern, 1, 1);
  If Length(strPattern) > 0 Then
    If strPattern[Length(strPattern)] <> '*' Then
      Include(MatchTypes, mtEnd)
    Else
      Delete(strPattern, Length(strPattern), 1);
  sl := TStringList.Create;
  Try
    For i := 1 To CharCount('*', strPattern) + 1 Do
      sl.Add(lowercase(GetField(strPattern, '*', i)));
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

End.


