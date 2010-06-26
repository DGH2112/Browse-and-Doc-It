(**

  DFMModule : A unit to tokenize DFM code.

  @Version    1.0
  @Date       26 Jun 2010
  @Author     David Hoyle

**)
Unit DFMModule;

Interface

Uses
  SysUtils, Windows, Contnrs, Classes, BaseLanguageModule;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This class represent a DFM object in the file. **)
  TDFMObject = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FIsInherited: Boolean;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Constructor Create(strName : String; AScope : TScope; iLine,
      iColumn : Integer; AImageIndex : TImageIndex; AComment : TComment); Override;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property sets and gets the whether the object is Inherited or not.
      @precon  None.
      @postcon Sets and gets the whether the object is Inherited or not.
      @return  a Boolean
    **)
    Property IsInherited : Boolean Read FIsInherited Write FIsInherited;
  End;

  (** This class represent a DFM property in the file. **)
  TDFMProperty = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

  (** This class represent a DFM Item in the file. **)
  TDFMItem = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FItemName: String;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetName : String; Override;
  Public
    Constructor Create(strName : String; AScope : TScope; iLine,
      iColumn : Integer; AImageIndex : TImageIndex; AComment : TComment); Override;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

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
    Constructor CreateParser(Source : String; strFileName : String;
      IsModified : Boolean; ModuleOptions : TModuleOptions); Override;
    Destructor Destroy; Override;
    Function ReservedWords : TKeyWords; Override;
    Function Directives : TKeyWords; Override;
    Procedure ProcessCompilerDirective(var iSkip : Integer); Override;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

Const
  (**
    A sorted list of keywords. Used for identifying tokens as keyword.

    This key words nil and string have been disables as there are used like
    identifiers and as types.

  **)
  strReservedWords : Array[0..2] Of String = (
    'end', 'inherited', 'object'
  );

  (** This is a list of reserved, directives word and a semi colon which are
      token that can be sort as then next place to start parsing from when an
      error is  encountered. **)
  strSeekableOnErrorTokens : Array[1..1] Of String = ('end');

{ TDFMObject }

(**

  This method returns a string represetation of the DFM object.

  @precon  None.
  @postcon Returns a string represetation of the DFM object.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TDFMObject.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;
begin
  If FIsInherited Then
    Result := 'Inherited'
  Else
    Result := 'Object';
  Result := Result + #32 + BuildStringRepresentation(True, boolForDocumentation,
    ':', BrowseAndDocItOptions.MaxDocOutputWidth)
end;

(**

  A constructor for the TDFMObject class.

  @precon  None.
  @postcon Initialises FInherited to false.

  @param   strName     as a String
  @param   AScope      as a TScope
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   AImageIndex as a TImageIndex
  @param   AComment    as a TComment

**)
constructor TDFMObject.Create(strName: String; AScope: TScope; iLine,
  iColumn: Integer; AImageIndex: TImageIndex; AComment: TComment);
begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FIsInherited := False;
end;

{ TDFMProperty }

(**

  This method returns a string represetation of the DFM property.

  @precon  None.
  @postcon Returns a string represetation of the DFM property.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TDFMProperty.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;
begin
  Result := BuildStringRepresentation(True, boolForDocumentation, '=',
    BrowseAndDocItOptions.MaxDocOutputWidth,
    ['(', '[', '{', ')', ']', '}', ';', ',', '.', '!', '?', '<', '>'],
    ['(', '[', '{', '.', '^', '-'],
    ['=', ':', '+', '*', '\'])
end;

{ TDFMItem }

(**

  This method returns a string representation of the item.

  @precon  None.
  @postcon Returns a string representation of the item.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TDFMItem.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;
begin
  Result := 'Item';
end;

(**

  A constructor for the TDFMItem class.

  @precon  None.
  @postcon Creates a unique name for the item.

  @param   strName     as a String
  @param   AScope      as a TScope
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   AImageIndex as a TImageIndex
  @param   AComment    as a TComment

**)
constructor TDFMItem.Create(strName: String; AScope: TScope; iLine,
  iColumn: Integer; AImageIndex: TImageIndex; AComment: TComment);
begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FItemName := Format('%s:%4.4d:%4.4d', [strName, iLine, iColumn]);
end;

(**

  This is an overridden GetName to provide a unqiue name for the item.

  @precon  None.
  @postcon Returns a unqiue name for the item.

  @return  a String

**)
function TDFMItem.GetName: String;
begin
  Result := FItemName;
end;

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

  @param   Source        as a String
  @param   strFileName   as a String
  @param   IsModified    as a Boolean
  @param   ModuleOptions as a TModuleOptions

**)
Constructor TDFMModule.CreateParser(Source : String; strFileName : String;
  IsModified : Boolean; ModuleOptions : TModuleOptions);

Begin
  Inherited CreateParser(Source, strFileName, IsModified, ModuleOptions);
  CompilerDefines.Assign(BrowseAndDocItOptions.Defines);
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
  boolInherited : Boolean;

begin
  Result := False;
  If IsKeyWord(Token.UToken, ['inherited', 'object']) Then
    Begin
      Result := True;
      boolInherited := Token.UToken = 'INHERITED';
      NextNonCommentToken;
      If Token.TokenType In [ttIdentifier] Then
        Begin
          O := Container.Add(TDFMObject.Create(Token.Token, scPublic, Token.Line,
            Token.Column, iiPublicObject, Nil)) As TDFMObject;
          O.IsInherited := boolInherited;
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
                    ErrorAndSeekToken(strReservedWordExpected, 'DFMObject', 'END',
                      strSeekableOnErrorTokens, stActual);
                End Else
                  ErrorAndSeekToken(strIdentExpected, 'DFMObject', Token.Token,
                    strSeekableOnErrorTokens, stActual);
            End Else
              ErrorAndSeekToken(strLiteralExpected, 'DFMObject', ':',
                strSeekableOnErrorTokens, stActual);
        End Else
          ErrorAndSeekToken(strIdentExpected, 'DFMObject', Token.Token,
            strSeekableOnErrorTokens, stActual);
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
            ItemList(P) Or BinaryData(P) Or ListData(P) Then;
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'DFMProperty', '=',
            strSeekableOnErrorTokens, stActual);
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
        ErrorAndSeekToken(strLiteralExpected, 'DFMSet', ']',
          strSeekableOnErrorTokens, stActual);
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
  LastCharType := ttUnknown;
  iStreamCount := 0;
  iLine := 1;
  iColumn := 1;
  LastChar := #0;
  strToken := '';
  LastToken := ttUnknown;

  iTokenLen := 0;
  SetLength(strToken, iTokenCapacity);

  Try
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
          If Not (IsInSet(strToken[1], strWhiteSpace + strLineEnd)) Then
            AddToken(TTokenInfo.Create(strToken, iStreamPos,
              iTokenLine, iTokenColumn, Length(strToken), LastCharType));
        End;
    AddToken(TTokenInfo.Create('<FileEnd>', iTokenLine, iTokenColumn,
      Length(FSource), 0, ttFileEnd));
  Except
    On E : Exception Do
      AddIssue(E.Message, scGlobal, 'TokenizeStream', 0, 0, etError);
  End
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
        ErrorAndSeekToken(strLiteralExpected, 'ListData', ')',
          strSeekableOnErrorTokens, stActual);
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
            ErrorAndSeekToken(strNumberExpected, 'Number', Token.Token,
              strSeekableOnErrorTokens, stActual);
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
              AddIssue(strUnExpectedEndOfFile, scNone, 'Goal', 0, 0, etError);
          End Else
          Begin
            AddIssue(strUnExpectedEndOfFile, scNone, 'Goal', 0, 0, etError);
            Raise EParserAbort.Create('Parsing Aborted!');
          End;
      End;
  Except
    On E : EParserAbort Do
      AddIssue(E.Message, scNone, 'Goal', 0, 0, etError);
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
            ErrorAndSeekToken(strIdentExpected, 'IdentList', Token.Token,
              strSeekableOnErrorTokens, stActual);
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
        ErrorAndSeekToken(strReservedWordExpected, 'Item', 'END',
          strSeekableOnErrorTokens, stActual);
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
        ErrorAndSeekToken(strLiteralExpected, 'ItemList', '>',
          strSeekableOnErrorTokens, stActual);
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
        ErrorAndSeekToken(strLiteralExpected, 'DFMIndex', ']',
          strSeekableOnErrorTokens, stActual);
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

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TDFMModule.AsString(boolShowIdentifier,
  boolForDocumentation : Boolean): String;

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
          ErrorAndSeekToken(strIdentExpected, 'QualifiedIdent', Token.Token,
            strSeekableOnErrorTokens, stActual);
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
            ErrorAndSeekToken(strStringExpected, 'StringLiteral', Token.Token,
              strSeekableOnErrorTokens, stActual);
        End;
    End;
end;

End.
