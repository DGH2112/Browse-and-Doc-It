(**

  DFMModule : A unit to tokenise DFM code.

  @Author  David Hoyle
  @Version 1.861
  @Date    15 Aug 2020

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
Unit BADI.DFM.Module;

Interface

Uses
  SysUtils,
  Windows,
  Contnrs,
  Classes,
  BADI.Base.Module,
  BADI.ElementContainer,
  BADI.Types,
  BADI.Comment;

{$INCLUDE CompilerDefinitions.inc}

Type
  (**

    This is the main class for dealing with object pascal units and program
    source files. It creates and manages instances of the other classes as
    needed.

  **)
  TDFMModule = Class(TBaseLanguageModule)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FSource                  : String;
    { Grammar Parsers }
    Procedure Goal;
    Function  DFMObject(Const Container : TElementContainer) : Boolean;
    Function  DFMProperty(Const Container : TElementContainer) : Boolean;
    Function  DFMIdentifier(Const Container : TElementContainer) : Boolean;
    Function  StringLiteral(Const Container : TElementContainer) : Boolean;
    Function  Number(Const Container : TElementContainer) : Boolean;
    Function  DFMSet(Const Container : TElementContainer) : Boolean;
    Function  ItemList(Const Container : TElementContainer) : Boolean;
    Function  BinaryData(Const Container : TElementContainer) : Boolean;
    Function  ListData(Const Container : TElementContainer) : Boolean;
    Function  QualifiedIdent : String;
    Procedure IdentList(Const Container : TElementContainer);
    Function  Item(Const Container : TElementContainer) : Boolean;
    Procedure DFMIndex(Const Container : TElementContainer);
    (* Helper method to the grammar parsers *)
    Procedure TokenizeStream;
    Procedure ParseTokens;
    Procedure TidyUpEmptyElements;
    Procedure ConcatStrings(Const Container : TElementContainer);
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetModuleName : String; Override;
    Function GetComment(Const CommentPosition : TCommentPosition = cpBeforeCurrentToken) : TComment;
      Override;
  Public
    Constructor CreateParser(Const Source, strFileName : String; Const IsModified : Boolean;
      Const ModuleOptions : TModuleOptions); Override;
    Destructor Destroy; Override;
    Function ReservedWords : TKeyWords; Override;
    Function Directives : TKeyWords; Override;
    Procedure ProcessCompilerDirective(var iSkip : Integer); Override;
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF DEBUG}
  BADI.Options,
  BADI.TokenInfo,
  BADI.ResourceStrings,
  BADI.Functions,
  BADI.Constants,
  BADI.Module.Dispatcher,
  BADI.DFM.ObjectDecl,
  BADI.DFM.Types,
  BADI.DFM.PropertyDecl,
  BADI.DFM.Item;

Const
  (**
    A sorted list of keywords. Used for identifying tokens as keyword.

    This key words nil and string have been disables as there are used like
    identifiers and as types.

  **)
  strReservedWords : Array[0..3] Of String = (
    'end', 'inherited', 'inline', 'object'
  );

  (** This is a list of reserved, directives word and a semi colon which are
      token that can be sort as then next place to start parsing from when an
      error is  encountered. **)
  strSeekableOnErrorTokens : Array[1..1] Of String = ('end');

(**

  This function returns a string representation of the unit.

  @precon  None .
  @postcon Returns a string representation of the unit .

  @nohints

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
function TDFMModule.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean): String;

begin
  Result := ExtractFileName(Identifier);
end;

(**

  This method parses the BinaryData element of the grammar.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Returns true if a BinaryData element was parsed.

  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
Function TDFMModule.BinaryData(Const Container: TElementContainer): Boolean;

Begin
  Result := Token.TokenType In [ttCustomUserToken];
  If Result Then
    AddToExpression(Container);
End;

(**

  This method concatenates single and double string literal is they are on the same line and occur next 
  to each other without spaces.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Concatenates single and double string literal is they are on the same line and occur next to 
           each other without spaces.

  @param   Container as a TElementContainer as a constant

**)
procedure TDFMModule.ConcatStrings(Const Container: TElementContainer);

Var
  L : TTokenInfo;
  strToken: String;

begin
  L := Token;
  strToken := Token.Token;
  NextNonCommentToken;
  While (Token.TokenType In [ttSingleLiteral, ttDoubleLiteral]) And
    (Token.Line = L.Line) And (Token.Column = L.Column + L.Length) Do
    Begin
      L := Token;
      strToken := strToken + Token.Token;
      NextNonCommentToken;
    End;
  Container.AddToken(strToken, ttSingleLiteral, L.Line, L.Column);
end;

(**

  This is the constructor method for the TDFMModule class.

  @precon  Source is a valid TStream descendant containing as stream of text, that is the contents of a 
           source code module and Filename is the file name of the module being parsed and IsModified 
           determines if the source code module has been modified since the last save to disk.
  @postcon Creates an instance of the module parser.

  @param   Source        as a String as a constant
  @param   strFileName   as a String as a constant
  @param   IsModified    as a Boolean as a constant
  @param   ModuleOptions as a TModuleOptions as a constant

**)
Constructor TDFMModule.CreateParser(Const Source, strFileName : String; Const IsModified : Boolean;
  Const ModuleOptions : TModuleOptions);

Const
  strStart = 'Start';
  strTokenize = 'Tokenize';
  strParse = 'Parse';
  strRefs = 'Refs';
  strSpelling = 'Spelling';

Begin
  Inherited CreateParser(Source, strFileName, IsModified, ModuleOptions);
  CompilerDefines.Assign(BADIOptions.Defines);
  FSource := Source;
  AddTickCount(strStart);
  CommentClass := Nil;
  TokenizeStream;
  AddTickCount(strTokenize);
  If moParse In ModuleOptions Then
    Begin
      ParseTokens;
      AddTickCount(strParse);
      Add(strErrors, iiErrorFolder, scNone);
      Add(strWarnings, iiWarningFolder, scNone);
      Add(strHints, iiHintFolder, scNone);
      Add(strDocumentationConflicts, iiDocConflictFolder, scNone);
      AddTickCount(strRefs);
      AddTickCount(strSpelling);
      TidyUpEmptyElements;
    End;
End;

(**


  This is a destructor for the TDFMModule class.

  @precon  None.
  @postcon Frees the memory for this instance.


**)
Destructor TDFMModule.Destroy;
begin
  Inherited Destroy;
end;

(**

  This method parses the Identifier element of the grammar.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Returns true IF the current token is an identifier.

  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
function TDFMModule.DFMIdentifier(Const Container: TElementContainer): Boolean;
begin
  Result := Token.TokenType In [ttIdentifier];
  If Result Then
    Container.AddToken(QualifiedIdent);
end;

(**

  This method parses the Index element of the grammar.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Parses the Index element of the grammar.

  @param   Container as a TElementContainer as a constant

**)
procedure TDFMModule.DFMIndex(Const Container: TElementContainer);
begin
  If Token.Token = '[' Then
    Begin
      AddToExpression(Container);
      Number(Container);
      If Token.Token = ']' Then
        AddToExpression(Container)
      Else
        ErrorAndSeekToken(strLiteralExpected, ']', strSeekableOnErrorTokens, stActual, Self);
    End;
end;

(**

  This method parses the Object element of the grammar.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Returns true if an object was found.

  @nocheck EmptyWhile

  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
function TDFMModule.DFMObject(Const Container : TElementContainer): Boolean;

Const
  astrObjects : TArray<String> = ['inherited', 'inline', 'object'];
  strINHERITED = 'INHERITED';
  strINLINE = 'INLINE';
  strEND = 'END';

Var
  O : TDFMObject;
  AObjectType : TObjectType;

begin
  Result := False;
  If IsKeyWord(Token.UToken, astrObjects) Then
    Begin
      Result := True;
      AObjectType := otObject;
      If Token.UToken = strINHERITED Then
        AObjectType := otInherited
      Else If Token.UToken = strINLINE Then
        AObjectType := otinline;
      NextNonCommentToken;
      If Token.TokenType In [ttIdentifier] Then
        Begin
          AddIdentifier(Token.Token);
          O := Container.Add(TDFMObject.Create(Token.Token, scPublic, Token.Line,
            Token.Column, iiPublicObject, Nil)) As TDFMObject;
          O.ObjectType := AObjectType;
          NextNonCommentToken;
          If Token.Token = ':' Then
            Begin
              NextNonCommentToken;
              If Token.TokenType In [ttIdentifier] Then
                Begin
                  AddToExpression(O);
                  DFMIndex(O);
                  While DFMObject(O) Or DFMProperty(O) Do;
                  If Token.UToken = strEND Then
                    NextNonCommentToken
                  Else
                    ErrorAndSeekToken(strReservedWordExpected, strEND, strSeekableOnErrorTokens,
                      stActual, Self);
                End Else
                  ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual,
                    Self);
            End Else
              ErrorAndSeekToken(strLiteralExpected, ':', strSeekableOnErrorTokens, stActual, Self);
        End Else
          ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
    End;
end;

(**

  This method parses the property element of the grammar.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Returns true is a property was parsed.

  @nocheck EmptyThen

  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
function TDFMModule.DFMProperty(Const Container: TElementContainer): Boolean;

Var
  P: TDFMProperty;
  strIdentifier : String;

begin
  Result := False;
  If Token.TokenType In [ttIdentifier] Then
    Begin
      Result := True;
      strIdentifier := QualifiedIdent;
      P := Container.Add(TDFMProperty.Create(strIdentifier, scPublic, Token.Line,
        Token.Column, iiPublicProperty, Nil)) As TDFMProperty;
      If Token.Token = '=' Then
        Begin
          NextNonCommentToken;
          If DFMIdentifier(P) Or StringLiteral(P) Or Number(P) Or DFMSet(P) Or
            ItemList(P) Or BinaryData(P) Or ListData(P) Then
            {Do nothing ???};
        End Else
          ErrorAndSeekToken(strLiteralExpected, '=', strSeekableOnErrorTokens, stActual, Self);
    End;
end;

(**

  This method parses the DFMSet element of the grammar.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Returns true if a DFM Set was parsed.

  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
function TDFMModule.DFMSet(Const Container: TElementContainer): Boolean;
begin
  Result := False;
  If Token.Token = '[' Then
    Begin
      Result := True;
      AddToExpression(Container);
      IdentList(Container);
      If Token.Token = ']' Then
        AddToExpression(Container)
      Else
        ErrorAndSeekToken(strLiteralExpected, ']', strSeekableOnErrorTokens, stActual, Self);
    End;
end;

(**

  This method returns an array of key words for use in the explorer module.

  @precon  None.
  @postcon Returns an array of key words for use in the explorer module.

  @return  a TKeyWords

**)
function TDFMModule.Directives: TKeyWords;

begin
  Result := Nil;
end;

(**

  This is an overridden GetComment which only returns Nil. Comments aren`t used.

  @precon  None.
  @postcon Returns nil.

  @nohints

  @param   CommentPosition as a TCommentPosition as a constant
  @return  a TComment

**)
function TDFMModule.GetComment(Const CommentPosition: TCommentPosition): TComment;

Begin
  Result := Nil;
End;

(**

  This is a getter method for the ModuleName property.

  @precon  None.
  @postcon Overrides the inherited method to place the module type in front of
           the module name.

  @return  a String

**)
Function TDFMModule.GetModuleName : String;

Begin
  Result := Inherited GetModuleName;
End;

(**

  This method is the starting position for the parsing of an object pascal
  module. It finds the first non comment token and begins the grammar checking
  from their by delegating to the program, library, unit and package methods.

  @precon  None.
  @postcon It finds the first non comment token and begins the grammar checking
           from their by delegating to the program, library, unit and package
           methods.

**)
procedure TDFMModule.Goal;

begin
  Try
    If TokenCount > 0 Then
      Begin
        // Find first non comment token
        While (Token.TokenType In [ttLineComment, ttBlockComment,
          ttCompilerDirective]) And Not EndOfTokens Do
          NextNonCommentToken;
        // Check for end of file else must be identifier
        If Not EndOfTokens Then
          Begin
            DFMObject(Self);
            If Not (Token.TokenType In [ttFileEnd]) Then
              AddIssue(strUnExpectedEndOfFile, scNone, 0, 0, etError, Self);
          End Else
          Begin
            AddIssue(strUnExpectedEndOfFile, scNone, 0, 0, etError, Self);
            Raise EBADIParserAbort.Create(strParsingAborted);
          End;
      End;
  Except
    On E : EBADIParserAbort Do
      AddIssue(E.Message, scNone, 0, 0, etError, Self);
  End;
end;

(**

  This method parses the IdentList element of the grammar.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Adds an list of comma separated identifiers to the container.

  @param   Container as a TElementContainer as a constant

**)
procedure TDFMModule.IdentList(Const Container: TElementContainer);

begin
  If Token.TokenType In [ttIdentifier] Then
    Begin
      AddToExpression(Container);
      While Token.Token = ',' Do
        Begin
          AddToExpression(Container);
          If Token.TokenType In [ttIdentifier] Then
            AddToExpression(Container)
          Else
            ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
        End;
    End;
end;

(**

  This method parses the item element of the grammar.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Parses an item if found.

  @nocheck EmptyWhile

  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
Function TDFMModule.Item(Const Container: TElementContainer) : Boolean;

Const
  strItem = 'Item';
  strEND = 'END';

Var
  I : TDFMItem;

begin
  Result := False;
  If CompareText(Token.Token, strItem) = 0 Then
    Begin
      Result := True;
      I := Container.Add(TDFMItem.Create(Token.Token, scPublic, Token.Line,
        Token.Column, iiPublicField, Nil)) As TDFMItem;
      NextNonCommentToken;
      While DFMProperty(I) Do;
      If Token.UToken = strEND Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strReservedWordExpected, strEND, strSeekableOnErrorTokens, stActual, Self);
    End;
end;

(**

  This method parses the ItemList element of the grammar.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Returns true if a ItemList was parsed.

  @nocheck EmptyWhile

  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
function TDFMModule.ItemList(Const Container: TElementContainer): Boolean;

Begin
  Result := False;
  If Token.Token = '<' Then
    Begin
      Result := True;
      AddToExpression(Container);
      While Item(Container) Do;
      If Token.Token = '>' Then
        AddToExpression(Container)
      Else
        ErrorAndSeekToken(strLiteralExpected, '>', strSeekableOnErrorTokens, stActual, Self);
    End;
End;

(**

  This method parses the list data element of the grammar.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Returns if a list data element was parsed.

  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
function TDFMModule.ListData(Const Container: TElementContainer): Boolean;
begin
  Result := False;
  If Token.Token = '(' Then
    Begin
      Result := True;
      AddToExpression(Container);
      While Token.TokenType In [ttNumber, ttSingleLiteral, ttDoubleLiteral] Do
        Case Token.Create.TokenType Of
          ttSingleLiteral, ttDoubleLiteral: StringLiteral(Container);
        Else
          AddToExpression(Container);
        End;
      If Token.Token = ')' Then
        AddToExpression(Container)
      Else
        ErrorAndSeekToken(strLiteralExpected, ')', strSeekableOnErrorTokens, stActual, Self);
    End;
end;

(**

  This method parses the number element of the grammar.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Returns true IF the current token is a number.

  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
function TDFMModule.Number(Const Container: TElementContainer): Boolean;

begin
  Result := (Token.TokenType In [ttNumber]) Or (Token.Token = '-');
  If Result Then
    Begin
      If Token.Token = '-' Then
        Begin
          AddToExpression(Container);
          If Not (Token.TokenType In [ttNumber]) Then
            ErrorAndSeekToken(strNumberExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
        End;
      AddToExpression(Container);
    End;
end;

(**

  This is the method that should be called to parse the previously parse tokens.

  @precon  None.
  @postcon Attempts to parse the token list and check it grammatically for
           Errors while providing declaration elements for browsing.

**)
Procedure TDFMModule.ParseTokens;

Begin
  Goal;
End;

(**

  This method processes a compiler directive looking for conditional statements.

  @precon  None.
  @postcon Processes a compiler directive looking for conditional statements.

  @nohints
  @nocheck EmptyMethod

  @param   iSkip as an Integer as a reference

**)
procedure TDFMModule.ProcessCompilerDirective(Var iSkip: Integer);

Begin
End;

(**

  This method parses the QualifiedIdent element of the grammar.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Returns the fully qualified (multi-part) identifier.

  @return  a String

**)
function TDFMModule.QualifiedIdent: String;
begin
  Result := Token.Token;
  NextNonCommentToken;
  While Token.Token = '.' Do
    Begin
      NextNonCommentToken;
      If Token.TokenType In [ttIdentifier] Then
        Begin
          Result := Result + '.' + Token.Token;
          NextNonCommentToken;
        End Else
          ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
    End;
end;

(**

  This method returns an array of key words for use in the explorer module.

  @precon  None.
  @postcon Returns an array of key words for use in the explorer module.

  @return  a TKeyWords

**)
function TDFMModule.ReservedWords: TKeyWords;

Var
  i : Integer;

begin
  SetLength(Result, Succ(High(strReservedWords)));
  For i := Low(strReservedWords) To High(strReservedWords) Do
    Result[i] := strReservedWords[i];
end;

(**

  This method parses the stringliteral element of the grammar.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Returns true IF the current token is a string literal.

  @param   Container as a TElementContainer as a constant
  @return  a Boolean

**)
Function TDFMModule.StringLiteral(Const Container: TElementContainer): Boolean;

ResourceString
  strLiterals = 'Literals';

Begin
  Result := Token.TokenType In [ttSingleLiteral, ttDoubleLiteral];
  If Result Then
    Begin
      ConcatStrings(Container);
      While Token.Token = '+' Do
        Begin
          AddToExpression(Container);
          If Token.TokenType In [ttSingleLiteral, ttDoubleLiteral] Then
            ConcatStrings(Container)
          Else
            ErrorAndSeekToken(strStringExpected, Token.Token, strSeekableOnErrorTokens, stActual, Self);
        End;
      If doSpellCheckDFMLiterals In TBADIOptions.BADIOptions.Options Then
        ProcessLiteralsForSpelling(Container, strLiterals);
    End;
End;

(**

  This method remove the Implement Methods and Exported Headings IF they have
  no elements.

  @precon  None.
  @postcon Remove the Implement Methods and Exported Headings IF they have
           no elements.

**)
procedure TDFMModule.TidyUpEmptyElements;

Var
  iElement : Integer;

begin
  For iElement := ElementCount DownTo 1 Do
    If Elements[iElement].ElementCount = 0 Then
      If Not (Elements[iElement] Is TDFMObject) Then
        DeleteElement(iElement);
end;

(**

  This method tokenises the stream of text passed to the constructor and splits
  it into tokens.

  @precon  None.
  @postcon Tokenises the stream of text passed to the constructor and splits
           it into tokens.

**)
Procedure TDFMModule.TokenizeStream;

Type
  (** State machine for block types. **)
  TBlockType = (btNoBlock, btStringLiteral, btBinaryData, btChar);

Const
  (** A set of characters for alpha characters **)
  strTokenChars : Set Of AnsiChar = ['_', 'a'..'z', 'A'..'Z'];
  (** A set of numbers **)
  strNumbers : Set Of AnsiChar = ['$', '0'..'9'];
  (** A set of characters for general symbols **)
  strSymbols : Set Of AnsiChar = ['&', '(', ')', '*', '+', ',', '-', '.', '/', ':',
    ';', '<', '=', '>', '@', '[', ']', '^'];
  (** A set of characters for quotes **)
  strStringLiteral : Set Of AnsiChar = [''''];
  strChar : Set Of AnsiChar = ['#'];
  (** Growth size of the token buffer. **)
  iTokenCapacity = 100;
  strSingleSymbols : Set Of AnsiChar = ['(', ')', ';', ',', '[', ']', '^',
    '-', '+', '/', '*', '<', '>'];
  strFileEnd = '<FileEnd>';

Var
  (** Token buffer. **)
  strToken : String;
  CurCharType : TBADITokenType;
  LastCharType : TBADITokenType;
  BlockType : TBlockType;
  (** Current line number **)
  iLine : Integer;
  (** Current column number **)
  iColumn : Integer;
  (** Token stream position. Fast to increment this than read the stream position. **)
  iStreamPos : Integer;
  (** Token line **)
  iTokenLine : Integer;
  (** Token column **)
  iTokenColumn : Integer;
  (** Current character position **)
  iStreamCount : Integer;
  Ch : Char;
  LastChar : Char;
  (** Token size **)
  iTokenLen : Integer;
  LastToken : TBADITokenType;
  iChar: Integer;

Begin
  BlockType := btNoBlock;
  iStreamPos := 0;
  iTokenLine := 1;
  iTokenColumn := 1;
  CurCharType := ttUnknown;
  iStreamCount := 0;
  iLine := 1;
  iColumn := 1;
  LastChar := #0;
  strToken := '';

  iTokenLen := 0;
  SetLength(strToken, iTokenCapacity);

  For iChar := 1 To Length(FSource) Do
    Begin
      ch := FSource[iChar];
      If (iChar = 1) And (Ch = 'ÿ') Then
        Exit;
      Inc(iStreamCount);
      LastCharType := CurCharType;

      If IsInSet(ch, strWhiteSpace) Then
        CurCharType := ttWhiteSpace
      Else If IsInSet(ch, strTokenChars) Then
        Begin
          If (LastCharType = ttNumber) And (IsInSet(Ch, ['A'..'F', 'a'..'f'])) Then
            CurCharType := ttNumber
          Else
            CurCharType := ttIdentifier;
        End
      Else If IsInSet(ch, strNumbers) Then
        Begin
          CurCharType := ttNumber;
          Case LastCharType Of
            ttIdentifier   : CurCharType := ttIdentifier;
            ttDoubleLiteral: CurCharType := ttDoubleLiteral;
          End;
          If IsInSet(LastChar, ['-', '+']) Then
            LastCharType := ttNumber;
        End
      Else If IsInSet(ch, strLineEnd) Then
        CurCharType := ttLineEnd
      Else If IsInSet(ch, strStringLiteral) Then
        CurCharType := ttSingleLiteral
      Else If IsInSet(ch, strChar) Then
        CurCharType := ttDoubleLiteral
      Else If IsInSet(ch, strSymbols) Then
        Begin
          If (LastCharType = ttNumber) And (IsInSet(LastChar, ['e', 'E'])) And
            (IsInSet(Ch, ['-', '+'])) Then
            CurCharType := ttNumber
          Else If (LastCharType = ttNumber) And (Ch = '.') Then
            CurCharType := ttNumber
          Else
            CurCharType := ttSymbol
        End
      Else
        CurCharType := ttUnknown;

      If (LastCharType <> CurCharType) Or (IsInSet(Ch, strSingleSymbols)) Or
        (IsInSet(LastChar, strSingleSymbols)) Then
        Begin
          If ((BlockType In [btStringLiteral]) And (CurCharType <> ttLineEnd)) Or
            (BlockType In [btBinaryData]) Then
            Begin
              Inc(iTokenLen);
              If iTokenLen > Length(strToken) Then
                SetLength(strToken, iTokenCapacity + Length(strToken));
              strToken[iTokenLen] := Ch;
            End Else
            Begin
              SetLength(strToken, iTokenLen);
              If iTokenLen > 0 Then
                If Not (IsInSet(strToken[1], strWhiteSpace + strLineEnd)) Then
                  Begin
                    If LastCharType = ttIdentifier Then
                      If IsKeyWord(strToken, strReservedWords) Then
                        LastCharType := ttReservedWord;
                    AddToken(TTokenInfo.Create(strToken, iStreamPos,
                      iTokenLine, iTokenColumn, Length(strToken), LastCharType));
                    LastToken := LastCharType;
                  End;
             // Store Stream position, line number and column of
             // token start
             iStreamPos := iStreamCount;
             iTokenLine := iLine;
             iTokenColumn := iColumn;
             BlockType := btNoBlock;
             iTokenLen := 1;
             SetLength(strToken, iTokenCapacity);
             strToken[iTokenLen] := Ch;
            End;
        End Else
        Begin
          Inc(iTokenLen);
          If iTokenLen > Length(strToken) Then
            SetLength(strToken, iTokenCapacity + Length(strToken));
          strToken[iTokenLen] := Ch;
        End;

      // Check for string literals
      If CurCharType = ttSingleLiteral Then
        Case BlockType Of
          btStringLiteral: BlockType := btNoBlock;
          btNoBlock:       BlockType := btStringLiteral;
        End;

      // Check for block Comments
      If (BlockType = btNoBlock) And (Ch = '{') Then
        Begin
          CurCharType := ttCustomUserToken;
          BlockType := btBinaryData;
        End;
      If (BlockType = btBinaryData) And (Ch = '}') Then
        Begin
          CurCharType := ttCustomUserToken;
          BlockType := btNoBlock;
        End;

      Inc(iColumn);
      If Ch = #10 Then
        Begin
          Inc(iLine);
          iColumn := 1;
          If BlockType In [btStringLiteral, btChar] Then
            BlockType := btNoBlock;
        End;
      LastChar := Ch;
    End;
    If iTokenLen > 0 Then
      Begin
        SetLength(strToken, iTokenLen);
        If CurCharType = ttIdentifier Then
          If IsKeyWord(strToken, strReservedWords) Then
            CurCharType := ttReservedWord;
        If Not (IsInSet(strToken[1], strWhiteSpace + strLineEnd)) Then
          AddToken(TTokenInfo.Create(strToken, iStreamPos,
            iTokenLine, iTokenColumn, Length(strToken), CurCharType));
      End;
  AddToken(TTokenInfo.Create(strFileEnd, iTokenLine, iTokenColumn,
    Length(FSource), 0, ttFileEnd));
End;

End.
