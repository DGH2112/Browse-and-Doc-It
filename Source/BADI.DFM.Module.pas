(**

  DFMModule : A unit to tokenize DFM code.

  @Version    1.0
  @Date       15 Oct 2017
  @Author     David Hoyle

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
    Function  DFMObject(Container : TElementContainer) : Boolean;
    Function  DFMProperty(Container : TElementContainer) : Boolean;
    Function  DFMIdentifier(Container : TElementContainer) : Boolean;
    Function  StringLiteral(Container : TElementContainer) : Boolean;
    Function  Number(Container : TElementContainer) : Boolean;
    Function  DFMSet(Container : TElementContainer) : Boolean;
    Function  ItemList(Container : TElementContainer) : Boolean;
    Function  BinaryData(Container : TElementContainer) : Boolean;
    Function  ListData(Container : TElementContainer) : Boolean;
    Function  QualifiedIdent : String;
    Procedure IdentList(Container : TElementContainer);
    Function  Item(Container : TElementContainer) : Boolean;
    Procedure DFMIndex(Container : TElementContainer);
    (* Helper method to the grammar parsers *)
    Procedure TokenizeStream;
    Procedure ParseTokens;
    Procedure TidyUpEmptyElements;
    Procedure ConcatStrings(Container : TElementContainer);
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetModuleName : String; Override;
    Function GetComment(
      CommentPosition : TCommentPosition = cpBeforeCurrentToken) : TComment;
      Override;
  Public
    Constructor CreateParser(const Source, strFileName : String;
      IsModified : Boolean; ModuleOptions : TModuleOptions); Override;
    Destructor Destroy; Override;
    Function ReservedWords : TKeyWords; Override;
    Function Directives : TKeyWords; Override;
    Procedure ProcessCompilerDirective(var iSkip : Integer); Override;
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

uses
  BADI.Options,
  BADI.TokenInfo,
  BADI.ResourceStrings,
  BADI.Functions,
  BADI.Constants,
  BADI.Module.Dispatcher, BADI.DFM.ObjectDecl, BADI.DFM.Types, BADI.DFM.PropertyDecl,
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

  This method parses the BinaryData element of the grammar.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Returns true if a BinaryData element was parsed.

  @param   Container as a TElementContainer
  @return  a Boolean

**)
function TDFMModule.BinaryData(Container: TElementContainer): Boolean;
begin
  Result := Token.TokenType In [ttCustomUserToken];
  If Result Then
    AddToExpression(Container);
end;

(**

  This method concatenates single and double string literal is they are on
  the same line and occur next to each other without spaces.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Concatenates single and double string literal is they are on
           the same line and occur next to each other without spaces.

  @param   Container as a TElementContainer

**)
procedure TDFMModule.ConcatStrings(Container: TElementContainer);

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
  Container.AddToken(strToken);
end;

(**

  This is the constructor method for the TPascalDocModule class.

  @precon  Source is a valid TStream descendant containing as stream of text,
           that is the contents of a source code module and Filename is the
           file name of the module being parsed and IsModified determines if
           the source code module has been modified since the last save to
           disk.
  @postcon Creates an instance of the module parser.

  @param   Source        as a String as a constant
  @param   strFileName   as a String as a constant
  @param   IsModified    as a Boolean
  @param   ModuleOptions as a TModuleOptions

**)
Constructor TDFMModule.CreateParser(const Source, strFileName : String;
  IsModified : Boolean; ModuleOptions : TModuleOptions);

Begin
  Inherited CreateParser(Source, strFileName, IsModified, ModuleOptions);
  CompilerDefines.Assign(BADIOptions.Defines);
  FSource := Source;
  AddTickCount('Start');
  CommentClass := Nil;
  TokenizeStream;
  AddTickCount('Tokenize');
  If moParse In ModuleOptions Then
    Begin
      ParseTokens;
      AddTickCount('Parse');
      Add(strErrors, iiErrorFolder, scNone, Nil);
      Add(strWarnings, iiWarningFolder, scNone, Nil);
      Add(strHints, iiHintFolder, scNone, Nil);
      Add(strDocumentationConflicts, iiDocConflictFolder, scNone, Nil);
      AddTickCount('Refs');
      TidyUpEmptyElements;
    End;
End;

(**


  This is a destructor for the TDFMModule class.

  @precon  None.
  @postcon Fress the memory fo this instance.


**)
Destructor TDFMModule.Destroy;
begin
  Inherited Destroy;
end;

(**

  Thids method parses the Object element of the grammar.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Returns true if an onject was found.

  @param   Container as a TElementContainer
  @return  a Boolean

**)
function TDFMModule.DFMObject(Container : TElementContainer): Boolean;

Var
  O : TDFMObject;
  AObjectType : TObjectType;

begin
  Result := False;
  If IsKeyWord(Token.UToken, ['inherited', 'inline', 'object']) Then
    Begin
      Result := True;
      AObjectType := otObject;
      If Token.UToken = 'INHERITED' Then
        AObjectType := otInherited
      Else If Token.UToken = 'INLINE' Then
        AObjectType := otinline;
      NextNonCommentToken;
      If Token.TokenType In [ttIdentifier] Then
        Begin
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
                  If Token.UToken = 'END' Then
                    NextNonCommentToken
                  Else
                    ErrorAndSeekToken(strReservedWordExpected, 'END', strSeekableOnErrorTokens,
                      stActual);
                End Else
                  ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual);
            End Else
              ErrorAndSeekToken(strLiteralExpected, ':', strSeekableOnErrorTokens, stActual);
        End Else
          ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses the property element of the grammar.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Returns true is a property was parsed.

  @param   Container as a TElementContainer
  @return  a Boolean

**)
function TDFMModule.DFMProperty(Container: TElementContainer): Boolean;

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
          ErrorAndSeekToken(strLiteralExpected, '=', strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses the DFMSet element of the grammar.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Returns true if a DFM Set was parsed.

  @param   Container as a TElementContainer
  @return  a Boolean

**)
function TDFMModule.DFMSet(Container: TElementContainer): Boolean;
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
        ErrorAndSeekToken(strLiteralExpected, ']', strSeekableOnErrorTokens, stActual);
    End;
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
  (** A set of characters for alpha characaters **)
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
  (** Token stream position. Fast to inc this than read the stream position. **)
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
  AddToken(TTokenInfo.Create('<FileEnd>', iTokenLine, iTokenColumn,
    Length(FSource), 0, ttFileEnd));
End;

(**

  This is the method that should be called to parse the previously parse tokens.

  @precon  None.
  @postcon Attempts to parse the token list and check it grammatically for
           Errors while providing delcaration elements for browsing.

**)
procedure TDFMModule.ParseTokens;
begin
  Goal;
end;

(**

  This method parses the list data elmement of the grammar.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Returns if a list data element was parsed.

  @param   Container as a TElementContainer
  @return  a Boolean

**)
function TDFMModule.ListData(Container: TElementContainer): Boolean;
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
        ErrorAndSeekToken(strLiteralExpected, ')', strSeekableOnErrorTokens, stActual);
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

  This method parses the number element of the grammar.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Returns true IF the current token is a number.

  @param   Container as a TElementContainer
  @return  a Boolean

**)
function TDFMModule.Number(Container: TElementContainer): Boolean;
begin
  Result := (Token.TokenType In [ttNumber]) Or (Token.Token = '-');
  If Result Then
    Begin
      If Token.Token = '-' Then
        Begin
          AddToExpression(Container);
          If Not (Token.TokenType In [ttNumber]) Then
            ErrorAndSeekToken(strNumberExpected, Token.Token, strSeekableOnErrorTokens, stActual);
        End;
      AddToExpression(Container);
    End;
end;

(**

  This is an overridden GetComment which only returns Nil. Comments aren`t used.

  @precon  None.
  @postcon Returns nil.

  @param   CommentPosition as a TCommentPosition
  @return  a TComment

**)
function TDFMModule.GetComment(CommentPosition: TCommentPosition): TComment;
begin
  Result := Nil;
end;

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
  from their by deligating to the program, library, unit and package methods.

  @grammar Goal -> ( Program | Package | Library | Unit )

  @precon  None.
  @postcon It finds the first non comment token and begins the grammar checking
           from their by deligating to the program, library, unit and package
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
              AddIssue(strUnExpectedEndOfFile, scNone, 0, 0, etError);
          End Else
          Begin
            AddIssue(strUnExpectedEndOfFile, scNone, 0, 0, etError);
            Raise EBADIParserAbort.Create(strParsingAborted);
          End;
      End;
  Except
    On E : EBADIParserAbort Do
      AddIssue(E.Message, scNone, 0, 0, etError);
  End;
end;

(**

  This method parses the IdentList element of the grammar.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Adds an list of comma separated identifiers to the container.

  @param   Container as a TElementContainer

**)
procedure TDFMModule.IdentList(Container: TElementContainer);
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
            ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual);
        End;
    End;
end;

(**

  This method parses the item element of the grammar.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Parses an item if found.

  @param   Container as a TElementContainer
  @return  a Boolean

**)
Function TDFMModule.Item(Container: TElementContainer) : Boolean;

Var
  I : TDFMItem;

begin
  Result := False;
  If CompareText(Token.Token, 'Item') = 0 Then
    Begin
      Result := True;
      I := Container.Add(TDFMItem.Create(Token.Token, scPublic, Token.Line,
        Token.Column, iiPublicField, Nil)) As TDFMItem;
      NextNonCommentToken;
      While DFMProperty(I) Do;
      If Token.UToken = 'END' Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strReservedWordExpected, 'END', strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses the ItemList element of the grammar.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Returns true if a ItemList was parsed.

  @param   Container as a TElementContainer
  @return  a Boolean

**)
function TDFMModule.ItemList(Container: TElementContainer): Boolean;
begin
  Result := False;
  If Token.Token = '<' Then
    Begin
      Result := True;
      AddToExpression(Container);
      While Item(Container) Do;
      If Token.Token = '>' Then
        AddToExpression(Container)
      Else
        ErrorAndSeekToken(strLiteralExpected, '>', strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses the Identifier element of the grammar.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Returns true IF the current token is an identifier.

  @param   Container as a TElementContainer
  @return  a Boolean

**)
function TDFMModule.DFMIdentifier(Container: TElementContainer): Boolean;
begin
  Result := Token.TokenType In [ttIdentifier];
  If Result Then
    Container.AddToken(QualifiedIdent);
end;

(**

  This method parses the Index element of the grammar.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Parses the Index element of the grammar.

  @param   Container as a TElementContainer

**)
procedure TDFMModule.DFMIndex(Container: TElementContainer);
begin
  If Token.Token = '[' Then
    Begin
      AddToExpression(Container);
      Number(Container);
      If Token.Token = ']' Then
        AddToExpression(Container)
      Else
        ErrorAndSeekToken(strLiteralExpected, ']', strSeekableOnErrorTokens, stActual);
    End;
end;

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

  This function returns a string repreentation of the unit.

  @precon  None .
  @postcon Returns a string repreentation of the unit .

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
function TDFMModule.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean): String;

begin
  Result := ExtractFileName(Identifier);
end;

(**

  This method processes a compiler directive looking for conditional statements.

  @precon  None.
  @postcon Processes a compiler directive looking for conditional statements.

  @param   iSkip as an Integer as a reference

**)
procedure TDFMModule.ProcessCompilerDirective(var iSkip : Integer);

begin
end;

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
          ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekableOnErrorTokens, stActual);
    End;
end;

(**

  This method parses the stringliteral element of the grammar.

  @precon  Container must be a valid instance of TElementContainer class.
  @postcon Returns true IF the current token is a string literal.

  @param   Container as a TElementContainer
  @return  a Boolean

**)
function TDFMModule.StringLiteral(Container: TElementContainer): Boolean;
begin
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
            ErrorAndSeekToken(strStringExpected, Token.Token, strSeekableOnErrorTokens, stActual);
        End;
    End;
end;

End.
