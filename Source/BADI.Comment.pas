(**

  This module contains a class which represents all comment in the Browse and Doc It system.

  @Author  David Hoyle
  @Version
  @Date    06 Jun 2020

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2020  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

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
Unit BADI.Comment;

Interface

Uses
  Classes,
  Contnrs,
  BADI.Base.Container,
  BADI.Comment.Tag,
  BADI.Types;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A class the handles and stores all the comment information **)
  TComment = Class(TBADIBaseContainer)
  Strict Private
    Type
      (** An enumerate to define the types of text block in the parser. **)
      TBlockType = (btNone, btHTML, btLink, btSingle, btDouble);
  Strict Private
    FTags        : TObjectList;
    FTagMode     : Boolean;
    FLastTag     : TTag;
    FTokenLine   : Integer;
    FTokenColumn : Integer;
  Strict Protected
    Function  GetTag(Const iTagIndex: Integer): TTag;
    Function  GetTagCount: Integer;
    Procedure ParseComment(Const strComment: String; Const iLeadingChars : Integer);
    Procedure ResetTagMode;
    Function  DetermineTokenType(Const Ch : Char;
      Const eLastToken : TBADITokenType) : TBADITokenType; InLine;
    Procedure ProcessBlocks(Const strToken : String; Const iTokenIndex : Integer;
      Var eBlockType : TBlockType; Var eCurToken : TBADITokenType); InLine;
    Procedure ProcessStringLiterals(Const eCurToken : TBADITokenType; var eBlockType : TBlockType);
      InLine;
    Procedure TrimTrailingWhitespace; InLine;
    Procedure TrimTrailingWhitespaceAndLineEnds(Const ATag : TTag); InLine;
    Procedure TrimStartingWhitespaceONLYIfItEndsWithALineEnd(Const ATag : TTag); InLine;
    Procedure TrimWhitespaceFromFixedTagsIfCommonOnALLLines(Const ATag : TTag); InLine;
  Public
    //Constructor Create(Const strName : String; Const AScope : TScope; Const iLine,
    //  iColumn : Integer); Overload; Override; 
    Constructor Create(Const srcComment: TComment); Reintroduce; Overload; Virtual;
    Constructor Create(Const strComment: String; Const iLine, iCol, iLeadingChars : Integer);
      Reintroduce; Overload; Virtual;
    Destructor Destroy; Override;
    Class Function CreateComment(Const strComment: String; Const iLine, iCol : Integer): TComment; Virtual;
    Procedure AddToken(Const strToken: String; Const iType: TBADITokenType = ttUnknown); Override;
    Procedure Assign(Const srcComment: TComment); Overload;
    Procedure Assign(Const strComment: String); Overload;
    Function  AsString(Const iMaxWidth: Integer; Const boolShowHTML: Boolean): String;
    Function  FindTag(Const strTagName: String): Integer;
    Procedure AppendComment(Const BaseCmt, Source: TComment);
    Procedure TrimWhiteSpace;
    (**
      Returns the specifically indexed tag from the comments tag collection.
      @precon  iTagIndex must eb a valid index between 0 and TagCount - 1.
      @postcon Returns the specifically indexed tag from the comments tag collection.
      @param   iTagIndex as an Integer as a Constant
      @return  a TTag
    **)
    Property Tag[Const iTagIndex: Integer]: TTag Read GetTag;
    (**
      Returns the number of tags found within the comment.
      @precon  None.
      @postcon Returns the number of tags found within the comment.
      @return  an Integer
    **)
    Property TagCount: Integer Read GetTagCount;
  End;

  (** A class type for comment parsers. **)
  TCommentClass = Class Of TComment;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  SysUtils,
  BADI.TokenInfo,
  BADI.Functions,
  BADI.Constants;

(**

  This method added a token and its type to the token list.

  @precon  strToken is a string to be added as a token and iType is the tokens type.
  @postcon Added a token and its type to the token list.

  @nometric HardCodedInteger HardCodedString

  @param   strToken as a String as a constant
  @param   iType    as a TBADITokenType as a Constant

**)
Procedure TComment.AddToken(Const strToken: String; Const iType: TBADITokenType);

Const
  strDoubleAt = '@@';
  iSecondChar = 2;

Begin
  If (strToken[1] = '@') And (Copy(strToken, 1, strDoubleAt.Length) <> strDoubleAt) Then
    Begin
      FTagMode := True;
      FLastTag := TTag.Create(
        Copy(strToken, iSecondChar, Length(strToken) - 1),
        scNone,
        FTokenLine,
        FTokenColumn + 1
      );
      FTags.Add(FLastTag);
    End
  Else If Not FTagMode Then
    Begin
      If Not((iType = ttWhiteSpace) And (TokenCount = 0)) Then
        AddToken(TTokenInfo.Create(
          strToken,
          0,
          FTokenLine,
          FTokenColumn,
          Length(strToken),
          iType
        ));
    End
  Else
    Begin
      If Not((iType = ttWhiteSpace) And (FLastTag.TokenCount = 0) And Not FLastTag.Fixed) Then
        FLastTag.AddToken(TTokenInfo.Create(
          strToken,
          0,
          FTokenLine,
          FTokenColumn,
          Length(strToken),
          iType
        ));
    End;
End;

(**

  This method appends a source comments tokens and tabs onto this comment.

  @precon  BaseCmt and Source must be valid instances of TComment.
  @postcon Appends a source comments tokens and tabs onto this comment.

  @param   BaseCmt as a TComment as a constant
  @param   Source  as a TComment as a constant

**)
Procedure TComment.AppendComment(Const BaseCmt, Source: TComment);

Var
  BC: TBADIBaseContainer;
  i: Integer;
  j: Integer;
  T: TTag;

Begin
  BC := Source;
  // Append ofg the last tag of there are tags
  If TagCount > 0 Then
    BC := BaseCmt.Tag[TagCount - 1];
  // Add space if last char of previous comment / tag is not a space
  If BC.TokenCount > 0 Then
    If Not (BC.Tokens[BC.TokenCount - 1].TokenType In [ttUnknown, ttLineEnd, ttWhiteSpace]) Then
      BC.AddToken(#32, ttWhiteSpace);
  // Append tokens to last comment / tag
  For i := 0 To Source.TokenCount - 1 Do
    BC.AppendToken(Source.Tokens[i]);
  // Append new tags / tokens
  For i := 0 To Source.TagCount - 1 Do
    Begin
      T := TTag.Create(
        Source.Tag[i].TagName,
        Source.Tag[i].Scope,
        Source.Tag[i].Line,
        Source.Tag[i].Column
      );
      FTags.Add(T);
      For j := 0 To Source.Tag[i].TokenCount - 1 Do
        T.AppendToken(Source.Tag[i].Tokens[j]);
    End;

End;

(**

  This method assigns the str passed to the end of the token list. The string
  has a pre and post fix added so that the ParseComment() method will accept it
  as a valid comment.

  @precon  strComment is a string of text to be parsed as a comment.
  @postcon Assigns the str passed to the end of the token list. The string
           has a pre and post fix added so that the ParseComment() method will
           accept it as a valid comment.

  @param   strComment as a String as a constant

**)
Procedure TComment.Assign(Const strComment: String);

Begin
  ResetTagMode;
  ParseComment(strComment, 0);
End;

(**

  This method appends all the tokens and tags from the source comment to this comment.

  @precon  srcComment is a source comment to be assign to this comment.
  @postcon Appends all the tokens and tags from the source comment to this comment.

  @param   srcComment as a TComment as a constant

**)
Procedure TComment.Assign(Const srcComment: TComment);

Var
  i, j: Integer;
  Tag : TTag;

Begin
  If Assigned(srcComment) Then
    Begin
      ClearTokens;
      FTags.Clear;
      Line := srcComment.Line;
      Column := srcComment.Column;
      Fixed := srcComment.Fixed;
      // Add tokens from one to the next.
      For i := 0 To srcComment.TokenCount - 1 Do
        AppendToken(srcComment.Tokens[i]);
      // Add tags
      For i := 0 To srcComment.TagCount - 1 Do
        Begin
          Tag := TTag.Create(
            srcComment.Tag[i].TagName,
            srcComment.Tag[i].Scope,
            srcComment.Tag[i].Line,
            srcComment.Tag[i].Column
          );
          FTags.Add(Tag);
          For j := 0 To srcComment.Tag[i].TokenCount - 1 Do
            Tag.AppendToken(srcComment.Tag[i].Tokens[j]);
        End;
    End;
End;

(**

  This method returns a string representation of the comment tokens with the specified indent and broken
  into lines by the max width parameter.

  @precon  iIndent is the indent in space required of the comment, iMaxWidth is the maximum width before
           the comment is broken onto another line and ShowHTML determines if the routine outputs the
           HTML Tags in the resulting string.
  @postcon Returns a string representation of the comment indented and broken into lines.

  @param   iMaxWidth    as an Integer as a constant
  @param   boolShowHTML as a Boolean as a constant
  @return  a String

**)
Function TComment.AsString(Const iMaxWidth: Integer; Const boolShowHTML: Boolean): String;

Begin
  Result := OutputCommentAndTag(Self, iMaxWidth, boolShowHTML, False);
End;

(**

  This is the TComment constructor. It create a token list and a tag list. Then it passes the comment to 
  the comment parser.

  @precon  strComment is a string of text to be parsed as a comment, iLine is the line number of the 
           comment and iCol is the column number of the comment.
  @postcon It create a token list and a tag list. Then it passes the comment to the comment parser.

  @param   strComment    as a String as a constant
  @param   iLine         as an Integer as a constant
  @param   iCol          as an Integer as a constant
  @param   iLeadingChars as an Integer as a constant

**)
Constructor TComment.Create(Const strComment: String; Const iLine, iCol, iLeadingChars : Integer);

Begin
  Inherited Create('', scNone, iLine, iCol);
  FLastTag := Nil;
  FTags := TObjectList.Create(True);
  FTagMode := False;
  ParseComment(strComment, iLeadingChars);
End;

(**

  This is the constructor method for the TComment class.

  @precon  None.
  @postcon Allows a comment to be constructed from another comment (clone).

  @param   srcComment as a TComment as a constant

**)
Constructor TComment.Create(Const srcComment: TComment);

Begin
  If srcComment <> Nil Then
    Inherited Create('', scNone, srcComment.Line, srcComment.Column)
  Else
    Inherited Create('', scNone, 0, 0);
  FLastTag := Nil;
  FTags := TObjectList.Create(True);
  FTagMode := False;
  Assign(srcComment);
End;

//Constructor TComment.Create(Const strName: String; Const AScope: TScope; Const iLine, iColumn: Integer);
//
//Begin
//  Inherited Create(strName, AScope, iLine, iColumn);
//End;

(**

  This is a constructor for the TComment class.

  @precon  None.
  @postcon Implements a basic comment with no start and end character removed. This method should be
           overridden by descendants to handle their different comment styles.

  @param   strComment as a String as a constant
  @param   iLine      as an Integer as a constant
  @param   iCol       as an Integer as a constant
  @return  a TComment

**)
Class Function TComment.CreateComment(Const strComment: String; Const iLine, iCol: Integer): TComment;

Begin
  Result := Create(strComment, iLine, iCol, 0);
End;

(**

  This is the TComment class`s destructor. It disposes of the token list and
  the tag list.

  @precon  None.
  @postcon Frees the classes internal lists.

**)
Destructor TComment.Destroy;

Begin
  FTags.Free;
  Inherited;
End;

(**

  This function determines the type of token being processed in the comment stream.

  @precon  None.
  @postcon Returns the token type of the current stream position.

  @nometric HardCodedString

  @param   Ch         as a Char as a constant
  @param   eLastToken as a TBADITokenType as a constant
  @return  a TBADITokenType

**)
Function TComment.DetermineTokenType(Const Ch : Char; Const eLastToken : TBADITokenType) : TBADITokenType;

Begin
  Case Ch Of
    #9, #32: Result := ttWhiteSpace;
    '''': Result := ttSingleLiteral;
    '"': Result := ttDoubleLiteral;
    '@', '_', 'a'..'z', 'A'..'Z':
      Begin
        If (eLastToken = ttNumber) Then
          Begin
           Case Ch Of
             'A'..'F', 'a'..'f': Result := ttNumber;
           Else
             Result := ttIdentifier;
           End;
          End Else
            Result := ttIdentifier;
      End;
    '0'..'9':
      Begin
        Result := ttNumber;
        If eLastToken = ttIdentifier Then
          Result := ttIdentifier;
      End;
    #10, #13: Result := ttLineEnd;
    #33, #36..#38, #40..#47, #58..#63, #91..#94, #96, #123..#128: Result := ttSymbol;
  Else
    Result := ttUnknown;
  End;
End;

(**

  This method tries to find the given tag in the tag collection. It returns
  the index, else -1.

  @precon  strTagName is the name of the tag to search for.
  @postcon Returns the tags index if found else -1.

  @param   strTagName as a String as a constant
  @return  an Integer

**)
Function TComment.FindTag(Const strTagName: String): Integer;

Var
  i: Integer;

Begin
  Result := -1;
  For i := 0 To TagCount - 1 Do
    If CompareText(Tag[i].TagName, strTagName) = 0 Then
      Begin
        Result := i;
        Break;
      End;
End;

(**

  This is a getter method for the Tag array property of the TComment class. It returns a TTag reference
  to the indexed tag item.

  @precon  iTagIndex is the index of the tag required.
  @postcon Returns an instance of the specified tag.

  @param   iTagIndex as an Integer as a constant
  @return  a TTag

**)
Function TComment.GetTag(Const iTagIndex: Integer): TTag;

Begin
  Result := (FTags[iTagIndex] As TTag);
End;

(**

  This is a getter method for the TagCount property of the TComment class.
  It return the number of tag in the list.

  @precon  None.
  @postcon Returns the number of tags in the collection.

  @return  an Integer

**)
Function TComment.GetTagCount: Integer;

Begin
  Result := FTags.Count;
End;

(**

  This method takes the given comment and parses it into tokens. It pulls out all the tags at the same 
  time. Tag should be at the end of the comment.

  @precon  strComment is a string of text to be parsed as a comment.
  @postcon Takes the given comment and parses it into tokens. It pulls out all the tags at the same time
           . Tag should be at the end of the comment.

  @param   strComment    as a String as a constant
  @param   iLeadingChars as an Integer as a constant

**)
Procedure TComment.ParseComment(Const strComment: String; Const iLeadingChars : Integer);

Const
  iTokenCapacity = 25;

Var
  i: Integer;
  CurToken: TBADITokenType;
  LastToken: TBADITokenType;
  strToken: String;
  BlockType: TBlockType;
  iTokenLen: Integer;
  LastTokenAdded: TBADITokenType;

Begin
  CurToken := ttUnknown;
  LastToken := ttUnknown;
  strToken := '';
  iTokenLen := 0;
  SetLength(strToken, iTokenCapacity);
  BlockType := btNone;
  FTokenLine := Line;
  FTokenColumn := Column + iLeadingChars;
  LastTokenAdded := ttUnknown;
  For i := 1 To Length(strComment) Do
    Begin
      LastToken := CurToken;
      CurToken := DetermineTokenType(strComment[i], LastToken);
      If ((CurToken <> LastToken) And (BlockType = btNone)) Or (strComment[i] = '<') Then
        Begin
          SetLength(strToken, iTokenLen);
          If iTokenLen > 0 Then
            Begin
              If Not (IsInSet(strToken[1], strWhiteSpace + strLineEnd)) Then
                Begin
                  AddToken(strToken, LastToken);
                  LastTokenAdded := LastToken;
                End Else
                Begin
                  If Not Assigned(FLastTag) Or (Assigned(FLastTag) And Not FLastTag.Fixed) Then
                    Begin
                      If Not (LastTokenAdded In [ttWhiteSpace, ttLineEnd]) Then
                        Begin
                          AddToken(#32, ttWhiteSpace);
                          LastTokenAdded := ttWhiteSpace;
                        End;
                    End Else
                      AddToken(strToken, LastToken);
                End;
              LastToken := CurToken;
              If Not IsInSet(strToken[1], strLineEnd) Then
                Inc(FTokenColumn, Length(strToken));
            End;
          iTokenLen := 1;
          SetLength(strToken, iTokenCapacity);
          strToken[iTokenLen] := strComment[i];
        End
      Else
        Begin
          Inc(iTokenLen);
          If iTokenLen > Length(strToken) Then
            SetLength(strToken, iTokenCapacity + Length(strToken));
          strToken[iTokenLen] := strComment[i];
        End;
      ProcessBlocks(strToken, iTokenLen, BlockType, CurToken);
      ProcessStringLiterals(CurToken, BlockType);
      If strComment[i] = #10 Then
        Begin
          FTokenColumn := 1;
          Inc(FTokenLine);
        End;
    End;
  If (iTokenLen > 0) Then
    Begin
      SetLength(strToken, iTokenLen);
      If Not(IsInSet(strToken[1], strWhiteSpace + strLineEnd)) Then
        AddToken(strToken, LastToken);
    End;
  TrimWhiteSpace;
End;

(**

  This procedure process HTML and brace comment blocks.

  @precon  None.
  @postcon The eBlockType and eCurToken are updated if HTML or brace comments are found.

  @nometric HardCodedInteger

  @param   strToken    as a String as a constant
  @param   iTokenIndex as an Integer as a constant
  @param   eBlockType  as a TBlockType as a reference
  @param   eCurToken   as a TBADITokenType as a reference

**)
Procedure TComment.ProcessBlocks(Const strToken : String; Const iTokenIndex : Integer;
  Var eBlockType : TBlockType; Var eCurToken : TBADITokenType);

Const
  iSecondChar = 2;

Begin
  If (eBlockType = btNone) And (strToken[1] = '{') Then
    eBlockType := btLink
  Else If (eBlockType = btNone) And (strToken[1] = '<') Then
    eBlockType := btHTML;
  If (eBlockType = btLink) And (strToken[iTokenIndex] = '}') Then
    Begin
      eBlockType := btNone;
      eCurToken := ttLinkTag;
    End;
  If (eBlockType = btHTML) And (strToken[iTokenIndex] = '>') Then
    Begin
      eBlockType := btNone;
      If strToken[iSecondChar] = '/' Then
        eCurToken := ttHTMLEndTag
      Else
        eCurToken := ttHTMLStartTag;
    End;
End;

(**

  This procedure processes single and double string literals in the stream.

  @precon  None.
  @postcon The eBlockType is updated depending upon whether the stream is at the start or end
           of a string literal.

  @param   eCurToken  as a TBADITokenType as a constant
  @param   eBlockType as a TBlockType as a reference

**)
Procedure TComment.ProcessStringLiterals(Const eCurToken : TBADITokenType;
  Var eBlockType : TBlockType);

Begin
  // Check for single string literals
  If eCurToken = ttSingleLiteral Then
    If eBlockType = btSingle Then
      eBlockType := btNone
    Else If eBlockType = btNone Then
      eBlockType := btSingle;
  // Check for Double string literals
  If eCurToken = ttDoubleLiteral Then
    If eBlockType = btDouble Then
      eBlockType := btNone
    Else If eBlockType = btNone Then
      eBlockType := btDouble;
End;

(**

  This method resets the comment tag mode, i.e. the comment will accept text as
  tokens and not tag tokens.

  @precon  None.
  @postcon Resets the comment tag mode, i.e. the comment will accept text as
           tokens and not tag tokens.

**)
Procedure TComment.ResetTagMode;

Begin
  FTagMode := False;
End;

(**

  This method trims starting whitespace from tags only if the line ends with a carriage return /
  line feed.

  @precon  ATAg must be a valid instance.
  @postcon The leading whitespace is removed from the tag if it ends with a CR / LF.

  @param   ATag as a TTag as a constant

**)
Procedure TComment.TrimStartingWhitespaceONLYIfItEndsWithALineEnd(Const ATag : TTag);

Var
  iPos: Integer;
  iToken : Integer;
  boolHasWS: Boolean;

Begin
  iPos := -1;
  For iToken := 0 To ATag.TokenCount - 1 Do
    If ATag.Tokens[iToken].TokenType = ttLineEnd Then
      Begin
        iPos := iToken;
        Break;
      End;
  boolHasWS := iPos > -1;
  For iToken := iPos - 1 DownTo 0 Do
    boolHasWS := boolHasWS And (ttWhiteSpace = ATag.Tokens[iToken].TokenType);
  If boolHasWS Then
    For iToken := 0 To iPos Do
      ATag.DeleteToken(0);
End;

(**

  This method removes trailing whitespace from the whole comment.

  @precon  None.
  @postcon Trailing whitespace is removed form the whole comment (including the last tag).

**)
Procedure TComment.TrimTrailingWhitespace;

Var
  iToken : Integer;

Begin
  If TokenCount > 0 Then
    Begin
      iToken := TokenCount - 1;
      While Tokens[iToken].TokenType In [ttWhiteSpace] Do
        Begin
          DeleteToken(iToken);
          Dec(iToken);
        End;
    End;
End;

(**

  This method removes both whitespave an line ends from the given Tag.

  @precon  ATag must be a valid instance.
  @postcon Whitespace and line ends are removed from the given tag.

  @param   ATag as a TTag as a constant

**)
Procedure TComment.TrimTrailingWhitespaceAndLineEnds(Const ATag : TTag);

Var
  iToken : Integer;

Begin
  iToken := ATag.TokenCount - 1;
  While (iToken >= 0) And (ATag.Tokens[iToken].TokenType In [ttWhiteSpace, ttLineEnd]) Do
    Begin
      ATag.DeleteToken(iToken);
      Dec(iToken);
    End;
End;

(**

  This method removes trailing white space tokens from the parsed comments and
  tags.

  @precon  None.
  @postcon Removes trailing white space tokens from the parsed comments and
           tags.

**)
Procedure TComment.TrimWhiteSpace;

Var
  iTag: Integer;
  ATag: TTag;

Begin
  TrimTrailingWhitespace;
  For iTag := 0 To TagCount - 1 Do
    Begin
      ATag := Tag[iTag];
      If ATag.TokenCount = 0 Then
        Continue;
      TrimTrailingWhitespaceAndLineEnds(ATag);
      TrimStartingWhitespaceONLYIfItEndsWithALineEnd(ATag);
      If ATag.Fixed Then
        TrimWhitespaceFromFixedTagsIfCommonOnALLLines(ATag);
    End;
End;

(**

  This method removes common whitespace from all the lines of the tag so that the indentation
  is maintained but the line with the least whitespace ends up with none.

  @precon  ATag must be a valid instance.
  @postcon Common whitespace is removed from the tag comment.

  @param   ATag as a TTag as a constant

**)
Procedure TComment.TrimWhitespaceFromFixedTagsIfCommonOnALLLines(Const ATag: TTag);

Const
  iLeadCountConst = 999;

Var
  iLeadCount: Integer;
  iCurCount : Integer;
  iToken: Integer;
  Token: TTokenInfo;
  boolCanCount: Boolean;

Begin
  iLeadCount := iLeadCountConst;
  iCurCount := 0;
  boolCanCount := True;
  // Count common leading whitespace tokens.
  For iToken := 0 To ATag.TokenCount - 1 Do
    Begin
      Token := ATag.Tokens[iToken];
      Case Token.TokenType Of
        ttWhiteSpace:
          If boolCanCount Then
            Inc(iCurCount, Length(Token.Token));
        ttLineEnd:
          Begin
            If iLeadCount > iCurCount Then
              iLeadCount := iCurCount;
            iCurCount := 0;
            boolCanCount := True;
          End;
      Else
        boolcanCount := False;
      End;
    End;
  If iLeadCount > iCurCount Then
    iLeadCount := iCurCount;
  // Trim leading space from lines
  iToken := 0;
  iCurCount := iLeadCount;
  If iLeadCount > 0 Then
    While iToken <= ATAg.TokenCount - 1 Do
      Begin
        Token := ATag.Tokens[iToken];
        Case Token.TokenType Of
          ttWhiteSpace:
            If iCurCount > 0 Then
              Begin
                If Length(Token.Token) <= iCurCount Then
                  Begin
                    Dec(iCurCount, Length(Token.Token));
                    ATag.DeleteToken(iToken);
                  End Else
                If Length(Token.Token) > iCurCount Then
                  Begin
                    Token.Replace(
                      Copy(Token.Token, 1 + iCurCount, Length(Token.Token) - iCurCount),
                      ttWhiteSpace);
                    Inc(iToken);
                    iCurCount := 0;
                  End;
              End Else
                Inc(iToken);
          ttLineEnd:
            Begin
              iCurCount := iLeadCount;
              Inc(iToken);
            End;
        Else
          Inc(iToken);
        End;
      End;
End;

End.
