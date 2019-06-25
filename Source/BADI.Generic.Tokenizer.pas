(**

  This module contains a routine for generically parsing code for use with the
  module explorer and documentation engine.

  @Author  David Hoyle
  @Version 1.0
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
Unit BADI.Generic.Tokenizer;

Interface

{$INCLUDE 'CompilerDefinitions.inc'}

Uses
  SysUtils,
  Classes,
  BADI.Types;

Function Tokenize(const strText: String; const ReservedWords, Directives: TKeyWords;
  const iLimit: Integer = 999999): TStringList;

Implementation

Uses
  BADI.Functions;

(**

  This function returns a string list contains the tokenized representation of
  the passed string with respect to some basic object pascal grammer.

  @precon  strText si the line of text to be tokenised
  @postcon Returns a new string list of the tokenized string

  @note    The string list returnsed must be destroyed be the calling method.

  @param   strText       as a String as a constant
  @param   ReservedWords as a TKeyWords as a constant
  @param   Directives    as a TKeyWords as a constant
  @param   iLimit        as an Integer as a constant
  @return  a TStringList

**)
Function Tokenize(const strText: String; const ReservedWords, Directives: TKeyWords;
  const iLimit: Integer = 999999): TStringList;

Type
  (* * State machine for block types. * *)
  TBlockType = (btNoBlock, btSingleLiteral, btDoubleLiteral, btCustomUserToken, btXMLTag,
    btPascalBlockComment, btCPPBlockComment, btLineComment, btBraceComment);

Const
  (* * Growth size of the token buffer. * *)
  iTokenCapacity = 25;

  (**

    This function returns the token toke associated with the given character in the context of the
    previous character token type.

    @precon  None.
    @postcon Returns the token type for the given character.

    @param   C            as a Char as a constant
    @param   LastToken    as a TBADITokenType as a constant
    @param   boolInXMLTag as a Boolean as a constant
    @param   boolIsXML    as a Boolean as a constant
    @return  a TBADITokenType

  **)
  Function GetTokenType(Const C: Char; Const LastToken: TBADITokenType; Const boolInXMLTag,
    boolIsXML : Boolean): TBADITokenType; InLine;

  Begin
    Case C Of
      #32, #9: Result := ttWhiteSpace;
      '#', '_', 'a' .. 'z', 'A' .. 'Z':
        Begin
          If (LastToken = ttNumber) Then
            Case C Of
              'A' .. 'F', 'a' .. 'f': Result := ttNumber;
            Else
              Result := ttIdentifier;
            End
          Else
            Result := ttIdentifier;
        End;
      '$', '0' .. '9':
        Begin
          Result := ttNumber;
          If LastToken = ttIdentifier Then
            Result := ttIdentifier;
        End;
      #10, #13: Result := ttLineEnd;
      '''':
        If boolIsXML And Not boolInXMLTag Then
          Result := ttSymbol
        Else
          Result := ttSingleLiteral;
      '"':
        If boolIsXML And Not boolInXMLTag Then
          Result := ttSymbol
        Else
          Result := ttDoubleLiteral;
      '?': Result := ttCustomUserToken;
      #33, #37 .. #38, #40 .. #47, #58..#62, #64, #91 .. #94, #96, #123 .. #127:
        Result := ttSymbol;
    Else
      Result := ttUnknown;
    End;
  End;

  (**

    This procedure checks the current and last chacracters in the stream to determine whether they
    are the start of a comment.

    @precon  None.
    @postcon Returns whether they characters are the start of a comment block.

    @param   CurChar   as a Char as a constant
    @param   LastChar  as a Char as a constant
    @param   BlockType as a TBlockType as a reference

  **)
  Procedure CheckForCommentStarts(Const CurChar, LastChar: Char; Var BlockType: TBlockType); InLine;

  Begin
    If (BlockType = btNoBlock) And (LastChar = '(') And (CurChar = '*') Then
      BlockType := btPascalBlockComment;
    If (BlockType = btNoBlock) And (LastChar = '/') And (CurChar = '*') Then
      BlockType := btCPPBlockComment;
    If (BlockType = btNoBlock) And (LastChar = '/') And (CurChar = '/') Then
      BlockType := btLineComment;
  End;

  (**

    This procedure checks the current and last chacracters in the stream to determine whether they
    are the end of a comment.

    @precon  None.
    @postcon Returns whether they characters are the end of a comment block and the new current
             token type.

    @param   CurChar   as a Char as a constant
    @param   LastChar  as a Char as a constant
    @param   BlockType as a TBlockType as a reference
    @param   CurToken  as a TBADITokenType as a reference

  **)
  Procedure CheckForCommentEnds(Const CurChar, LastChar: Char; Var BlockType: TBlockType;
    Var CurToken: TBADITokenType); InLine;

  Begin
    If (BlockType = btPascalBlockComment) And (LastChar = '*') And (CurChar = ')') Then
      Begin
        BlockType := btNoBlock;
        CurToken := ttBlockComment;
      End;
    If (BlockType = btCPPBlockComment) And (LastChar = '*') And (CurChar = '/') Then
      Begin
        BlockType := btNoBlock;
        CurToken := ttBlockComment;
      End;
  End;

  (**

    This procedures checks to see of the current chacarter is a opening or closing brace.

    @precon  None.
    @postcon Returns the blovk type and current token type.

    @param   CurChar   as a Char as a constant
    @param   BlockType as a TBlockType as a reference
    @param   CurToken  as a TBADITokenType as a reference

  **)
  Procedure CheckForBraceComments(Const CurChar: Char; Var BlockType: TBlockType;
    Var CurToken: TBADITokenType); InLine;

  Begin
    If (BlockType = btNoBlock) And (CurChar = '{') Then
      Begin
        CurToken := ttBlockComment;
        BlockType := btBraceComment;
      End;
    If (BlockType = btBraceComment) And (CurChar = '}') Then
      Begin
        CurToken := ttBlockComment;
        BlockType := btNoBlock;
      End;
  End;

  (**

    This procedure checks the current character to determine whether they are the start of any
    string literals.

    @precon  None.
    @postcon Returns the block tpe and curent token type.

    @param   CurToken     as a TBADITokenType as a constant
    @param   BlockType    as a TBlockType as a reference
    @param   boolInXMLTag as a Boolean as a constant
    @param   boolIsXML    as a Boolean as a constant

  **)
  Procedure CheckForLiterals(Const CurToken: TBADITokenType; Var BlockType: TBlockType;
    Const boolInXMLTag, boolIsXML : Boolean); InLine;

  Begin
    // Check for string literals
    If (CurToken = ttSingleLiteral) And Not (boolIsXML Xor boolInXMLTag) Then
      If BlockType = btSingleLiteral Then
        BlockType := btNoBlock
      Else If BlockType = btNoBlock Then
        BlockType := btSingleLiteral;
    // Check for string literals
    If (CurToken = ttDoubleLiteral) And Not (boolIsXML Xor boolInXMLTag) Then
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
  End;

  (**

    This procedure adds the current character to the token buffer.

    @precon  None.
    @postcon The current chacarter is added to the token buffer.

    @param   strToken  as a String as a reference
    @param   iTokenLen as an Integer as a reference
    @param   CurChar   as a Char as a constant

  **)
  Procedure AddToToken(Var strToken: String; Var iTokenLen: Integer; Const CurChar: Char); InLine;

  Begin
    Inc(iTokenLen);
    If iTokenLen > Length(strToken) Then
      SetLength(strToken, iTokenCapacity + Length(strToken));
    strToken[iTokenLen] := CurChar;
  End;

  (**

    This procedure checks the passed token for being a reserved word or directive and updates the
    last token type accordingly.

    @precon  None.
    @postcon The last token type is updated if the token in the buffer is a reserved word or
             directive.

    @param   strToken      as a String as a constant
    @param   ReservedWords as a TKeyWords as a constant
    @param   Directives    as a TKeyWords as a constant
    @param   LastToken     as a TBADITokenType as a reference

  **)
  Procedure CheckReservedWordAndDirective(Const strToken: String; Const ReservedWords,
    Directives: TKeyWords; Var LastToken: TBADITokenType); InLine;

  Begin
    If ReservedWords <> Nil Then
      If IsKeyWord(strToken, ReservedWords) Then
        LastToken := ttReservedWord;
    If Directives <> Nil Then
      If IsKeyWord(strToken, Directives) Then
        LastToken := ttDirective;
  End;

  (**

    This procedure checks the current token for being a carriage return and if so resets the
    block type if a line.

    @precon  None.
    @postcon Resets the block type of the current chacrter is a carriage return and the block type
             is a line comment.

    @param   CurChar   as a Char as a constant
    @param   BlockType as a TBlockType as a reference

  **)
  Procedure CheckForCarriageReturn(Const CurChar: Char; Var BlockType: TBlockType); InLine;

  Begin
    If CurChar = #10 Then
      If BlockType In [btLineComment] Then
        BlockType := btNoBlock;
  End;

  (**

    This procedure checks the token buffer for XML tags.

    @precon  None.
    @postcon Updates the token type of an XML tag if this is appopriate.

    @param   strToken     as a String as a constant
    @param   LastToken    as a TBADITokenType as a reference
    @param   boolInXMLTag as a Boolean as a reference
    @param   boolIsXML    as a Boolean as a reference

  **)
  Procedure CheckForXMLTags(Const strToken : String; Var LastToken: TBADITokenType;
    var boolInXMLTag, boolIsXML : Boolean);

  Begin
    If (LastToken = ttIdentifier) And (Result.Count > 0) And (Result[Result.Count - 1] = '<') Then
      Begin
        LastToken := ttHTMLStartTag;
        boolInXMLTag := True;
        If Not boolIsXML Then
          boolIsXML := True;
      End;
    If strToken = '>' Then
      boolInXMLTag := False;
    If (LastToken = ttIdentifier) And (Result.Count > 1) And (Result[Result.Count - 2] = '<') And
      (Result[Result.Count - 1] = '/') Then
      LastToken := ttHTMLEndTag;
  End;

Var
  (* * Token buffer. * *)
  strToken: String;
  CurToken: TBADITokenType;
  LastToken: TBADITokenType;
  BlockType: TBlockType;
  (* * Token size * *)
  iTokenLen: Integer;
  i: Integer;
  LastChar: Char;
  boolIsXML : Boolean;
  boolInXMLTag : Boolean;

Begin
  Result := TStringList.Create;
  BlockType := btNoBlock;
  strToken := '';
  CurToken := ttUnknown;
  strToken := '';
  LastChar := #0;
  iTokenLen := 0;
  boolIsXML := False;
  boolInXMLTag := False;
  SetLength(strToken, iTokenCapacity);
  For i := 1 To Length(strText) Do
    Begin
      LastToken := CurToken;
      CurToken := GetTokenType(strText[i], LastToken, boolInXMLTag, boolIsXML);
      CheckForCommentStarts(strText[i], LastChar, BlockType);
      If (LastToken <> CurToken) Or (CurToken = ttSymbol) Then
        Begin
          If Not(BlockType In [btNoBlock]) And
            Not((BlockType In [btLineComment]) And (CurToken In [ttLineEnd])) Then
            AddToToken(strToken, iTokenLen, strText[i])
          Else
            Begin
              SetLength(strToken, iTokenLen);
              If iTokenLen > 0 Then
                Begin
                  CheckReservedWordAndDirective(strToken, ReservedWords, Directives, LastToken);
                  CheckForXMLTags(strToken, LastToken, boolInXMLTag, boolIsXML);
                  If BlockType = btLineComment Then
                    LastToken := ttLineComment;
                  Result.AddObject(strToken, TObject(LastToken));
                  If Result.Count >= iLimit Then
                    Begin
                      Result.AddObject('...', TObject(ttSymbol));
                      Exit;
                    End;
                End;
              BlockType := btNoBlock;
              iTokenLen := 1;
              SetLength(strToken, iTokenCapacity);
              strToken[iTokenLen] := strText[i];
            End;
        End
      Else
        AddToToken(strToken, iTokenLen, strText[i]);
      CheckForCommentEnds(strText[i], LastChar, BlockType, CurToken);
      CheckForBraceComments(strText[i], BlockType, CurToken);
      CheckForLiterals(CurToken, BlockType, boolInXMLTag, boolIsXML);
      CheckForCarriageReturn(strText[i], BlockType);
      LastChar := strText[i];
    End;
  If iTokenLen > 0 Then
    Begin
      SetLength(strToken, iTokenLen);
      CheckReservedWordAndDirective(strToken, ReservedWords, Directives, CurToken);
      Result.AddObject(strToken, TObject(CurToken));
    End;
End;

End.
