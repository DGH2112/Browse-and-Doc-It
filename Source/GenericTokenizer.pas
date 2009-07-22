(**

  This module contains a routine for generically parsing code for use with the
  module explorer and documentation engine.

  @Author  David Hoyle
  @Date    22 Jul 2009
  @Version 1.0

**)
Unit GenericTokenizer;

Interface

{$INCLUDE 'CompilerDefinitions.inc'}

Uses
  SysUtils, Classes, BaseLanguageModule;

  Function Tokenize(strText : String; var KeyWords : TKeyWords;
    iLimit : Integer = 999999) : TStringList;

Implementation

(**

  This function returns a string list contains the tokenized representation of
  the passed string with respect to some basic object pascal grammer.

  @precon  strText si the line of text to be tokenised
  @postcon Returns a new string list of the tokenized string

  @note    The string list returnsed must be destroyed be the calling method.

  @param   strText  as a String
  @param   KeyWords as a TKeyWords as a reference
  @param   iLimit   as an Integer
  @return  a TStringList

**)
Function Tokenize(strText : String; var KeyWords : TKeyWords;
  iLimit : Integer = 999999) : TStringList;


Type
  (** State machine for block types. **)
  TBlockType = (btNoBlock, btSingleLiteral, btDoubleLiteral, btCustomUserToken,
    btXMLTag);

Const
  (** Growth size of the token buffer. **)
  iTokenCapacity = 25;

Var
  (** Token buffer. **)
  strToken : String;
  CurToken : TBADITokenType;
  LastToken : TBADITokenType;
  BlockType : TBlockType;
  (** Token size **)
  iTokenLen : Integer;
  i : Integer;
  LastChar : Char;

Begin
  Result := TStringList.Create;
  BlockType := btNoBlock;
  strToken := '';
  CurToken := ttUnknown;
  strToken := '';
  LastChar := #0;

  iTokenLen := 0;
  SetLength(strToken, iTokenCapacity);

  For i := 1 To Length(strText) Do
    Begin
      LastToken := CurToken;
      {$IFNDEF D2009}
      If strText[i] In [#32, #9] Then
      {$ELSE}
      If CharInSet(strText[i], [#32, #9]) Then
      {$ENDIF}
        CurToken := ttWhiteSpace
      {$IFNDEF D2009}
      Else If strText[i] In ['#', '_', 'a'..'z', 'A'..'Z'] Then
      {$ELSE}
      Else If CharInSet(strText[i], ['#', '_', 'a'..'z', 'A'..'Z']) Then
      {$ENDIF}
        Begin
          {$IFNDEF D2009}
          If (LastToken = ttNumber) And (strText[i] In ['A'..'F', 'a'..'f']) Then
          {$ELSE}
          If (LastToken = ttNumber) And (CharInSet(strText[i], ['A'..'F', 'a'..'f'])) Then
          {$ENDIF}
            CurToken := ttNumber
          Else
            CurToken := ttIdentifier;
        End
      {$IFNDEF D2009}
      Else If strText[i] In ['$', '0'..'9'] Then
      {$ELSE}
      Else If CharInSet(strText[i], ['$', '0'..'9']) Then
      {$ENDIF}
        Begin
          CurToken := ttNumber;
          If LastToken = ttIdentifier Then
            CurToken := ttIdentifier;
        End
      {$IFNDEF D2009}
      Else If strText[i] In [#10, #13] Then
      {$ELSE}
      Else If CharInSet(strText[i], [#10, #13]) Then
      {$ENDIF}
        CurToken := ttLineEnd
      {$IFNDEF D2009}
      Else If strText[i] In [''''] Then
      {$ELSE}
      Else If CharInSet(strText[i], ['''']) Then
      {$ENDIF}
        CurToken := ttSingleLiteral
      {$IFNDEF D2009}
      Else If strText[i] In ['"'] Then
      {$ELSE}
      Else If CharInSet(strText[i], ['"']) Then
      {$ENDIF}
        CurToken := ttDoubleLiteral
      {$IFNDEF D2009}
      Else If strText[i] In ['?'] Then
      {$ELSE}
      Else If CharInSet(strText[i], ['?']) Then
      {$ENDIF}
        CurToken := ttCustomUserToken
      {$IFNDEF D2009}
      Else If strText[i] In [#0..#255] - ['#', '_', 'a'..'z', 'A'..'Z', '$', '0'..'9'] Then
      {$ELSE}
      Else If CharInSet(strText[i], [#0..#255] - ['#', '_', 'a'..'z', 'A'..'Z', '$', '0'..'9']) Then
      {$ENDIF}
        CurToken := ttSymbol
      Else
        CurToken := ttUnknown;

      If (BlockType = btNoBlock) And (LastChar = '<') Then
        BlockType := btXMLTag;

      If (LastToken <> CurToken) Or (CurToken = ttSymbol) Then
        Begin
          If ((BlockType In [btSingleLiteral, btDoubleLiteral, btCustomUserToken,
            btXMLTag]) And
            (CurToken <> ttLineEnd)) Then
            Begin
              Inc(iTokenLen);
              If iTokenLen > Length(strToken) Then
                SetLength(strToken, iTokenCapacity + Length(strToken));
              strToken[iTokenLen] := strText[i];
            End Else
            Begin
              SetLength(strToken, iTokenLen);
              If iTokenLen > 0 Then
                Begin
                  If KeyWords <> Nil Then
                    If IsKeyWord(strToken, KeyWords) Then
                      LastToken := ttReservedWord;
                  Result.AddObject(strToken, TObject(LastToken));
                  If Result.Count >= iLimit Then
                    Begin
                      Result.Add('...');
                      Exit;
                    End;
                End;
             BlockType := btNoBlock;
             iTokenLen := 1;
             SetLength(strToken, iTokenCapacity);
             strToken[iTokenLen] := strText[i];
            End;
        End Else
        Begin
          Inc(iTokenLen);
          If iTokenLen > Length(strToken) Then
            SetLength(strToken, iTokenCapacity + Length(strToken));
          strToken[iTokenLen] := strText[i];
        End;

      If (BlockType = btXMLTag) And (strText[i] = '>') Then
        Begin
          BlockType := btNoBlock;
          CurToken := ttHTMLStartTag;
          If Length(strToken) > 1 Then
            If strToken[2] = '/' Then
              CurToken := ttHTMLEndTag
          Else
        End;

      // Check for string literals
      If CurToken = ttSingleLiteral Then
        If BlockType = btSingleLiteral Then
          BlockType := btNoBlock
        Else If BlockType = btNoBlock Then
          BlockType := btSingleLiteral;
      // Check for string literals
      If CurToken = ttDoubleLiteral Then
        If BlockType = btDoubleLiteral Then
          BlockType := btNoBlock
        Else If BlockType = btNoBlock Then
          BlockType := btDoubleLiteral;
      // Check for Custom User Token
      If CurToken = ttCustomUserToken Then
        If BlockType = btCustomUserToken Then
          BlockType := btNoBlock
        Else If BlockType = btNoBlock Then
          BlockType := btCustomUserToken;

      LastChar := strText[i];
    End;
  If iTokenLen > 0 Then
    Begin
      SetLength(strToken, iTokenLen);
      If KeyWords <> Nil Then
        If IsKeyWord(strToken, KeyWords) Then
          CurToken := ttReservedWord;
      Result.AddObject(strToken, TObject(CurToken));
    End;
End;

End.
