(**

  This method contains functions that are used global through out the application.

  @Version 1.0
  @Author  David Hoyle.
  @Date    04 Mar 2017

**)
Unit BADI.Functions;

Interface

Uses
  SysUtils,
  Classes,
  Dialogs,
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
  Function IsKeyWord(const strWord : String; strWordList : Array Of String): Boolean;
  Function IsInSet(C : Char; strCharSet : TSetOfAnsiChar) : Boolean; {$IFDEF D2005} InLine; {$ENDIF}
  Function PrologCode(const strTemplate, strMethod : String; iPadding : Integer) : TStringList;
  Function EpilogCode(const strTemplate, strMethod : String; iPadding : Integer) : TStringList;
  Function OutputCommentAndTag(C: TBaseContainer; iMaxWidth: Integer; boolShowHTML: Boolean): String;
  Function BuildLangIndepRep(Param: TGenericParameter): String;
  Function BADIImageIndex(iBADIImageIndex : TBADIImageIndex; Ascope : TScope) : Integer;

Implementation

Uses
  BADI.Constants,
  DGHLibrary;

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


  This function returns true if the given word is in the supplied word list.
  It uses a binary search, so the word lists need to be sorted.

  @precon  strWord is the word to be searches for in the word list and
           strWordList is a static array of words in lowercase and
           alphabetical order.
  @postcon Returns true if the word is found in the list.

  @param   strWord     as a String as a constant
  @param   strWordList as an Array Of String
  @return  a Boolean

**)
function IsKeyWord(const strWord : String; strWordList : Array Of String): Boolean;

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

  This function centralises the checking of characters in set for both AnsiChars
  and Unicode Chars so that the parser tokeniser are not riddled with
  conditional compilation statements.

  @precon  None.
  @postcon Checks to see if the char is in the set and returns true if so.

  @param   C          as a Char
  @param   strCharSet as a TSetOfAnsiChar
  @return  a Boolean

**)
Function IsInSet(C : Char; strCharSet : TSetOfAnsiChar) : Boolean; {$IFDEF D2005} InLine; {$ENDIF}

Begin
  {$IFNDEF D2009}
  Result := C In strCharSet;
  {$ELSE}
  Result := CharInSet(C, strCharSet);
  {$ENDIF}
End;

(**

  This procedure returns a string list containing the prolog element of code passed in the
  template parameter.

  @precon  strTemplate must contain the macro $METHODCODE$.
  @postcon Returns a string list containing the prolog element of code passed in the
           template parameter.

  @param   strTemplate as a String as a constant
  @param   strMethod   as a String as a constant
  @param   iPadding    as an Integer
  @return  a TStringList

**)
Function PrologCode(const strTemplate, strMethod : String; iPadding : Integer) : TStringList;

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

  This procedure returns a string list containing the epilog element of code passed in the
  template parameter.

  @precon  strTemplate must contain the macro $METHODCODE$.
  @postcon Returns a string list containing the epilog element of code passed in the
           template parameter.

  @param   strTemplate as a String as a constant
  @param   strMethod   as a String as a constant
  @param   iPadding    as an Integer
  @return  a TStringList

**)
Function EpilogCode(const strTemplate, strMethod : String; iPadding : Integer) : TStringList;

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

  This function outputs the comment or tag as a string missing out HTML tags if
  not required and any trialing whitespace.

  @precon  C must eb a valid instance of a TBaseContainer.
  @postcon Outputs the comment or tag as a string missing out HTML tags if not
           required and any trialing whitespace.

  @param   C            as a TBaseContainer
  @param   iMaxWidth    as an Integer
  @param   boolShowHTML as a Boolean
  @return  a String

**)
Function OutputCommentAndTag(C: TBaseContainer; iMaxWidth: Integer; boolShowHTML: Boolean): String;

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

  @param   Param as a TGenericParameter
  @return  a String

**)
function BuildLangIndepRep(Param: TGenericParameter): String;
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

  @param   iBADIImageIndex as a TBADIImageIndex
  @param   Ascope          as a TScope
  @return  an Integer

**)
Function BADIImageIndex(iBADIImageIndex : TBADIImageIndex; Ascope : TScope) : Integer;

Begin
  Result := Pred(Integer(iBADIImageIndex)) *
    (Integer(High(TScope)) - Integer(Low(TScope)) + 1) +
    Integer(AScope)
End;

End.
