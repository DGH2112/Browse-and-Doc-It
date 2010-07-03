(**

  This module contains a routine for generically parsing code for use with the
  module explorer and documentation engine.

  @Author  David Hoyle
  @Date    25 Jun 2010
  @Version 1.0

**)
Unit GenericTokenizer;

Interface

{$INCLUDE 'CompilerDefinitions.inc'}

Uses
  SysUtils, Classes, BaseLanguageModule;

  Function Tokenize(strText : String; var ReservedWords, Directives : TKeyWords;
    iLimit : Integer = 999999) : TStringList;

Implementation

(**

  This function returns a string list contains the tokenized representation of
  the passed string with respect to some basic object pascal grammer.

  @precon  strText si the line of text to be tokenised
  @postcon Returns a new string list of the tokenized string

  @note    The string list returnsed must be destroyed be the calling method.

  @param   strText       as a String
  @param   ReservedWords as a TKeyWords as a reference
  @param   Directives    as a TKeyWords as a reference
  @param   iLimit        as an Integer
  @return  a TStringList

**)
Function Tokenize(strText : String; var ReservedWords, Directives : TKeyWords;
  iLimit : Integer = 999999) : TStringList;


Type
  (** State machine for block types. **)
  TBlockType = (btNoBlock, btSingleLiteral, btDoubleLiteral, btCustomUserToken,
    btXMLTag, btPascalBlockComment, btCPPBlockComment, btLineComment,
    btBraceComment);

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
      If IsInSet(strText[i], [#32, #9]) Then
        CurToken := ttWhiteSpace
      Else If IsInSet(strText[i], ['#', '_', 'a'..'z', 'A'..'Z']) Then
        Begin
          If (LastToken = ttNumber) And (IsInSet(strText[i], ['A'..'F', 'a'..'f'])) Then
            CurToken := ttNumber
          Else
            CurToken := ttIdentifier;
        End
      Else If IsInSet(strText[i], ['$', '0'..'9']) Then
        Begin
          CurToken := ttNumber;
          If LastToken = ttIdentifier Then
            CurToken := ttIdentifier;
        End
      Else If IsInSet(strText[i], [#10, #13]) Then
        CurToken := ttLineEnd
      Else If IsInSet(strText[i], ['''']) Then
        CurToken := ttSingleLiteral
      Else If IsInSet(strText[i], ['"']) Then
        CurToken := ttDoubleLiteral
      Else If IsInSet(strText[i], ['?']) Then
        CurToken := ttCustomUserToken
      Else If IsInSet(strText[i], [#0..#255] - ['#', '_', 'a'..'z', 'A'..'Z', '$', '0'..'9']) Then
        CurToken := ttSymbol
      Else
        CurToken := ttUnknown;

      If (BlockType = btNoBlock) And (LastChar = '<') Then
        BlockType := btXMLTag;

        // Check for full block comments
        If (BlockType = btNoBlock) And (LastChar = '(') And (strText[i] = '*') Then
          BlockType := btPascalBlockComment;
        If (BlockType = btNoBlock) And (LastChar = '/') And (strText[i] = '*') Then
          BlockType := btCPPBlockComment;

        // Check for line comments
        If (BlockType = btNoBlock) And (LastChar = '/') And (strText[i] = '/') Then
          BlockType := btLineComment;

      If (LastToken <> CurToken) Or (CurToken = ttSymbol) Then
        Begin
          If Not (BlockType In [btNoBlock]) And
            Not ((BlockType In [btLineComment]) And (CurToken In [ttLineEnd])) Then
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
                  If ReservedWords <> Nil Then
                    If IsKeyWord(strToken, ReservedWords) Then
                      LastToken := ttReservedWord;
                  If Directives <> Nil Then
                    If IsKeyWord(strToken, Directives) Then
                      LastToken := ttDirective;
                  If BlockType = btLineComment Then
                    LastToken := ttLineComment;
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

        // Check for the end of a block comment
        If (BlockType = btPascalBlockComment) And (LastChar = '*') And (strText[i] = ')') Then
          Begin
            BlockType := btNoBlock;
            CurToken := ttBlockComment;
          End;
        If (BlockType = btCPPBlockComment) And (LastChar = '*') And (strText[i] = '/') Then
          Begin
            BlockType := btNoBlock;
            CurToken := ttBlockComment;
          End;
        // Check for block Comments
        If (BlockType = btNoBlock) And (strText[i] = '{') Then
          Begin
            CurToken := ttBlockComment;
            BlockType := btBraceComment;
          End;
        If (BlockType = btBraceComment) And (strText[i] = '}') Then
          Begin
            CurToken := ttBlockComment;
            BlockType := btNoBlock;
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

      If strText[i] = #10 Then
        If BlockType In [btLineComment] Then
          BlockType := btNoBlock;

      LastChar := strText[i];
    End;
  If iTokenLen > 0 Then
    Begin
      SetLength(strToken, iTokenLen);
      If ReservedWords <> Nil Then
        If IsKeyWord(strToken, ReservedWords) Then
          CurToken := ttReservedWord;
      If Directives <> Nil Then
        If IsKeyWord(strToken, Directives) Then
          CurToken := ttDirective;
      Result.AddObject(strToken, TObject(CurToken));
    End;
End;

End.
