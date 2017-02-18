(**

  This module contains a class which represents all comment in the Browse and Doc It system.

  @Author  David Hoyle
  @Version 1.0
  @Date    18 Feb 2017

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
  TComment = Class(TBaseContainer)
    {$IFDEF D2005} Strict {$ENDIF} Private
    FTags: TObjectList;
    FTagMode: Boolean;
    FLastTag: TTag;
    FTagLine: Integer;
    FTagColumn: Integer;
    {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetTag(iTagIndex: Integer): TTag;
    Function GetTagCount: Integer;
    Procedure ParseComment(const strComment: String);
    Procedure ResetTagMode;
  Public
    Constructor Create(srcComment: TComment); Overload;
    Constructor Create(const strComment: String; iLine, iCol: Integer); Overload;
    Destructor Destroy; Override;
    Class Function CreateComment(const strComment: String; iLine, iCol: Integer): TComment; Virtual;
    Procedure AddToken(const strToken: String; iType: TBADITokenType = ttUnknown); Override;
    Procedure Assign(srcComment: TComment); Overload;
    Procedure Assign(const strComment: String); Overload;
    Function AsString(iMaxWidth: Integer; boolShowHTML: Boolean): String;
    Function FindTag(const strTagName: String): Integer;
    Procedure TrimTrailingWhiteSpace;
    Procedure AppendComment(BaseCmt, Source: TComment);
    (**
      Returns the specifically indexed tag from the comments tag collection.
      @precon  iTagIndex must eb a valid index between 0 and TagCount - 1.
      @postcon Returns the specifically indexed tag from the comments tag collection.
      @param   iTagIndex as       an Integer
      @return  a TTag
    **)
    Property Tag[iTagIndex: Integer]: TTag Read GetTag;
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
  SysUtils,
  BADI.TokenInfo,
  BADI.Functions,
  BADI.Constants;

(**

  This method added a token and its type to the token list.

  @precon  strToken is a string to be added as a token and iType is the tokens
           type.
  @postcon Added a token and its type to the token list.

  @param   strToken as a String as a constant
  @param   iType    as a TBADITokenType

**)
Procedure TComment.AddToken(const strToken: String; iType: TBADITokenType);

Begin
  If (strToken[1] = '@') And (Copy(strToken, 1, 2) <> '@@') Then
    Begin
      FTagMode := True;
      FLastTag := TTag.Create(Copy(strToken, 2, Length(strToken) - 1), FTagLine,
        FTagColumn - Length(strToken));
      FTags.Add(FLastTag);
    End
  Else If Not FTagMode Then
    Begin
      If Not((iType = ttWhiteSpace) And (TokenCount = 0)) Then
        AddToken(TTokenInfo.Create(strToken, 0, 0, 0, Length(strToken), iType));
    End
  Else
    Begin
      If Not((iType = ttWhiteSpace) And (FLastTag.TokenCount = 0)) Then
        FLastTag.AddToken(TTokenInfo.Create(strToken, 0, 0, 0, Length(strToken), iType));
    End;
End;

(**

  This method appends a source comments tokens and tabs onto this comment.

  @precon  BaseCmt and Source must be valid instances of TComment.
  @postcon Appends a source comments tokens and tabs onto this comment.

  @param   BaseCmt as a TComment
  @param   Source  as a TComment

**)
Procedure TComment.AppendComment(BaseCmt, Source: TComment);

Var
  BC: TBaseContainer;
  i: Integer;
  j: Integer;

Begin
  BC := Source;
  If TagCount > 0 Then
    BC := BaseCmt.Tag[TagCount - 1];
  BC.AddToken(#32, ttWhiteSpace);
  For i := 0 To Source.TokenCount - 1 Do
    BC.AddToken(Source.Tokens[i].Token, Source.Tokens[i].TokenType);
  For i := 0 To Source.TagCount - 1 Do
    Begin
      FTagLine := Source.Tag[i].Line;
      FTagColumn := Source.Tag[i].Column + Length(Source.Tag[i].Name) + 1;
      AddToken('@' + Source.Tag[i].Name);
      For j := 0 To Source.Tag[i].TokenCount - 1 Do
        AddToken(Source.Tag[i].Tokens[j].Token, Source.Tag[i].Tokens[j].TokenType);
    End;

End;

(**

  This method appends all the tokens and tags from the source comment to this
  comment.

  @precon  srcComment is a source comment to be assign to this comment.
  @postcon Appends all the tokens and tags from the source comment to this
           comment.

  @param   srcComment as a TComment

**)
Procedure TComment.Assign(srcComment: TComment);

Var
  i, j: Integer;

Begin
  If srcComment <> Nil Then
    Begin
      ResetTagMode;
      Line := srcComment.Line;
      Column := srcComment.Column;
      // Add tokens from one to the next.
      For i := 0 To srcComment.TokenCount - 1 Do
        AddToken(srcComment.Tokens[i].Token, srcComment.Tokens[i].TokenType);
      For i := 0 To srcComment.TagCount - 1 Do
        Begin
          AddToken('@' + srcComment.Tag[i].TagName, ttIdentifier);
          For j := 0 To srcComment.Tag[i].TokenCount - 1 Do
            AddToken(srcComment.Tag[i].Tokens[j].Token, srcComment.Tag[i].Tokens[j].TokenType);
        End;
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
Procedure TComment.Assign(const strComment: String);
Begin
  ResetTagMode;
  ParseComment(strComment);
End;

(**

  This method returns a string representation of the comment tokens with the
  specified indent and broken into lines by the max width parameter.

  @precon  iIndent is the indent in space required of the comment, iMaxWidth is
           the maximum width before the comment is broken onto another line and
           ShowHTML determines if the routine outputs the HTML Tags in the
           resulting string.
  @postcon Returns a string representation of the comment indented and broken
           into lines.

  @param   iMaxWidth as an Integer
  @param   boolShowHTML  as a Boolean
  @return  a String

**)
Function TComment.AsString(iMaxWidth: Integer; boolShowHTML: Boolean): String;

Begin
  Result := OutputCommentAndTag(Self, iMaxWidth, boolShowHTML);
End;

(**

  This is the constructor method for the TComment class.

  @precon  None.
  @postcon Allows a comment to be constructed from another comment (clone).

  @param   srcComment as a TComment

**)
Constructor TComment.Create(srcComment: TComment);

Begin
  If srcComment <> Nil Then
    Inherited Create('', srcComment.Line, srcComment.Column)
  Else
    Inherited Create('', 0, 0);
  FLastTag := Nil;
  FTags := TObjectList.Create(True);
  FTagMode := False;
  Assign(srcComment);
End;

(**


  This is the TComment constructor. It create a token list and a tag list. Then
  it passes the comment to the comment parser.

  @precon  strComment is a string of text to be parsed as a comment, iLine is
           the line number of the comment and iCol is the column number of
           the comment.
  @postcon It create a token list and a tag list. Then it passes the comment to
           the comment parser.

  @param   strComment as a String as a constant
  @param   iLine      as an Integer
  @param   iCol       as an Integer

**)
Constructor TComment.Create(const strComment: String; iLine, iCol: Integer);
Begin
  Inherited Create('', iLine, iCol);
  FLastTag := Nil;
  FTags := TObjectList.Create(True);
  FTagMode := False;
  ParseComment(strComment);
End;

(**

  This is a constructor for the TComment class.

  @precon  None.
  @postcon Implements a basic comment with no start and end character removed.
           This method should be overridden by descendants to handle their
           different comment styles.

  @param   strComment as a String as a constant
  @param   iLine      as an Integer
  @param   iCol       as an Integer
  @return  a TComment

**)
Class Function TComment.CreateComment(const strComment: String; iLine, iCol: Integer): TComment;
Begin
  Result := Create(strComment, iLine, iCol);
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

  This is a getter method for the Tag array property of the TComment class.
  It returns a TTag reference to the indexed tag item.

  @precon  iTagIndex is the index of the tag required.
  @postcon Returns an instance of the specified tag.

  @param   iTagIndex as an Integer
  @return  a TTag

**)
Function TComment.GetTag(iTagIndex: Integer): TTag;
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

  This method removes trailing white space tokens from the parsed comments and
  tags.

  @precon  None.
  @postcon Removes trailing white space tokens from the parsed comments and
           tags.

**)
Procedure TComment.TrimTrailingWhiteSpace;

Var
  iToken: Integer;
  iTag: Integer;

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
  For iTag := 0 To TagCount - 1 Do
    Begin
      If Tag[iTag].TokenCount = 0 Then
        Continue;
      iToken := Tag[iTag].TokenCount - 1;
      While (iToken >= 0) And (Tag[iTag].Tokens[iToken].TokenType In [ttWhiteSpace]) Do
        Begin
          Tag[iTag].DeleteToken(iToken);
          Dec(iToken);
        End;
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
Function TComment.FindTag(const strTagName: String): Integer;

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

  This method takes the given comment and parses it into tokens. It pulls out
  all the tags at the same time. Tag should be at the end of the comment.

  @precon  strComment is a string of text to be parsed as a comment.
  @postcon Takes the given comment and parses it into tokens. It pulls out
           all the tags at the same time. Tag should be at the end of the comment.

  @param   strComment as a String as a constant

**)
Procedure TComment.ParseComment(const strComment: String);

Type
  TBlockType = (btNone, btHTML, btLink, btSingle, btDouble);

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
  FTagLine := Line;
  FTagColumn := Column + 1;
  LastTokenAdded := ttUnknown;
  For i := 1 To Length(strComment) Do
    Begin
      LastToken := CurToken;
      If IsInSet(strComment[i], strWhiteSpace) Then
        CurToken := ttWhiteSpace
      Else If IsInSet(strComment[i], ['''']) Then
        CurToken := ttSingleLiteral
      Else If IsInSet(strComment[i], ['"']) Then
        CurToken := ttDoubleLiteral
      Else If IsInSet(strComment[i], ['@', '_', 'a' .. 'z', 'A' .. 'Z']) Then
        Begin
          If (LastToken = ttNumber) And (IsInSet(strComment[i], ['A' .. 'F', 'a' .. 'f'])) Then
            CurToken := ttNumber
          Else
            CurToken := ttIdentifier;
        End
      Else If IsInSet(strComment[i], ['0' .. '9']) Then
        Begin
          CurToken := ttNumber;
          If LastToken = ttIdentifier Then
            CurToken := ttIdentifier;
        End
      Else If IsInSet(strComment[i], strLineEnd) Then
        CurToken := ttLineEnd
      Else If IsInSet(strComment[i], [#33 .. #128] - ['a' .. 'z', 'A' .. 'Z', '@', '#']) Then
        CurToken := ttSymbol
      Else
        CurToken := ttUnknown;

      If ((CurToken <> LastToken) And (BlockType = btNone)) Or (strComment[i] = '<') Then
        Begin
          SetLength(strToken, iTokenLen);
          If iTokenLen > 0 Then
            Begin
              If Not(IsInSet(strToken[1], strWhiteSpace + strLineEnd)) Then
                Begin
                  AddToken(strToken, LastToken);
                  LastTokenAdded := LastToken;
                End
              Else If Not(LastTokenAdded In [ttWhiteSpace, ttLineEnd]) Then
                Begin
                  AddToken(#32, ttWhiteSpace);
                  LastTokenAdded := ttWhiteSpace;
                End;
              LastToken := CurToken;
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

      If (BlockType = btNone) And (strToken[1] = '{') Then
        BlockType := btLink
      Else If (BlockType = btNone) And (strToken[1] = '<') Then
        BlockType := btHTML;

      If (BlockType = btLink) And (strToken[iTokenLen] = '}') Then
        Begin
          BlockType := btNone;
          CurToken := ttLinkTag;
        End;
      If (BlockType = btHTML) And (strToken[iTokenLen] = '>') Then
        Begin
          BlockType := btNone;
          If strToken[2] = '/' Then
            CurToken := ttHTMLEndTag
          Else
            CurToken := ttHTMLStartTag;
        End;

        // Check for single string literals
      If CurToken = ttSingleLiteral Then
        If BlockType = btSingle Then
          BlockType := btNone
        Else If BlockType = btNone Then
          BlockType := btSingle;
        // Check for Double string literals
      If CurToken = ttDoubleLiteral Then
        If BlockType = btDouble Then
          BlockType := btNone
        Else If BlockType = btNone Then
          BlockType := btDouble;

      If strComment[i] = #10 Then
        Begin
          FTagColumn := Column + 1;
          Inc(FTagLine);
        End
      Else
        Inc(FTagColumn);
    End;
  If (iTokenLen > 0) Then
    Begin
      SetLength(strToken, iTokenLen);
      If Not(IsInSet(strToken[1], strWhiteSpace + strLineEnd)) Then
        AddToken(strToken, LastToken);
    End;
  TrimTrailingWhiteSpace;
End;

End.
