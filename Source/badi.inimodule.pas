(**

  EidolonModule : A unit to parser Eidolon code. Please refer to the file
  "Eidolon Map File Grammar.bnf" for the complete grammar implemented.

  @Version    1.0
  @Date       12 Feb 2017
  @Author     David Hoyle

**)
Unit BADI.INIModule;

Interface

Uses
  SysUtils,
  Windows,
  Contnrs,
  Classes,
  BADI.BaseLanguageModule;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A XML specific implementation of comments. **)
  TINIComment = Class(TComment)
  Public
    Class Function CreateComment(strComment: String; iLine, iCol: Integer)
      : TComment; Override;
  End;

  (** This is the main class for dealing with backus-naur grammar files. **)
  TINIModule = Class(TBaseLanguageModule)
    {$IFDEF D2005} Strict {$ENDIF} Private
    FSource               : String;
    FSections             : TLabelContainer;
    FCurrentSection       : TLabelContainer;
    { Grammar Parsers }
    Procedure Goal;
    Function  Section : Boolean;
    Function  SectionHeader : Boolean;
    Function  KeyValuePair : Boolean;
    (* Helper method to the grammar parsers *)
    Procedure TokenizeStream;
    Procedure ParseTokens;
    Procedure EatLineEnds;
    Function  CheckLineEnd(strMethod: String): Boolean;
    Procedure EatWhitespace;
    {$IFDEF D2005} Strict {$ENDIF} Protected
    Function  GetComment(CommentPosition: TCommentPosition = cpBeforeCurrentToken)
      : TComment; Override;
    Procedure TidyUpEmptyElements;
    Function  GetModuleName: String; Override;
    Function  BuildSection(strSectionName : String; iLine,
      iColumn : Integer) : TLabelContainer;
  Public
    Constructor CreateParser(Source: String; strFileName: String; IsModified: Boolean;
      ModuleOptions: TModuleOptions); Override;
    Destructor Destroy; Override;
    Function  ReservedWords: TKeyWords; Override;
    Function  Directives: TKeyWords; Override;
    Procedure ProcessCompilerDirective(Var iSkip: Integer); Override;
    Function  ReferenceSymbol(AToken: TTokenInfo): Boolean; Override;
    Function  AsString(boolShowIdentifier, boolForDocumentation: Boolean): String;
      Override;
  End;

  (** This is a class to represent the Key and Value pairs. **)
  TKeyValuePair = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function  GetName : String; Override;
  Public
    Function  AsString(boolShowIdenifier, boolForDocumentation : Boolean) : String;
      Override;
  End;

Implementation

Uses
  DGHLibrary;

ResourceString
  (** A resource string for an error message where a line end token was expected. **)
  strExpectedLineEnd = 'Expected <LineEnd> but found ''%s'' at line %d column %d.';
  (** A resource string for an error message where the end of file is not found. **)
  strExpectedFileEnd = 'Expected <FileEnd> but found ''%s'' at line %d column %d.';
  (** A resource string for an error message where a key value is not found. **)
  strExpectedKey = 'Expected Key name but found ''%s'' at line %d column %d.';

Const
  (** This is a list of reserved, directives word and a semi colon which are
      token that can be sort as then next place to start parsing from when an
      error is  encountered. **)
  strSeekableOnErrorTokens: Array [1 .. 2] Of String = ('<CR>', '<LF>');

(**


  This method is a class method to first check the comment for being a
  documentation comment and then creating an instance of a TComment class and
  parsing the comment via the constructor.

  @precon  strComment is the full comment to be checked and parsed, iLine is
           the line number of the comment and iCol is the column number of
           the comment.

  @postcon Returns Nil if this is not a documentation comment or returns a
           valid TComment class.

  @param   strComment as a String
  @param   iLine      as an Integer
  @param   iCol       as an Integer
  @return  a TComment

**)
Class Function TINIComment.CreateComment(strComment: String; iLine, iCol: Integer)
  : TComment;

Begin //: @note Not currently configured or used.
  Result := Nil;
  If Length(strComment) > 0 Then
    Begin
      Case strComment[1] Of
        '/':
          strComment := Copy(strComment, 2, Length(strComment) - 1);
      End;
      If Length(strComment) > 0 Then
        Begin
          If strComment[1] = '*' Then
            strComment := Copy(strComment, 2, Length(strComment) - 3);
          If strComment[1] = '/' Then
            strComment := Copy(strComment, 2, Length(strComment) - 1);
          If Length(strComment) > 0 Then
            Begin
              If strComment[1] = ':' Then
                Begin;
                  strComment := Copy(strComment, 2, Length(strComment) - 1);
                  Result     := Create(strComment, iLine, iCol);
                End
              Else If strComment[1] = '*' Then
                Begin;
                  strComment := Copy(strComment, 2, Length(strComment) - 2);
                  Result     := Create(strComment, iLine, iCol);
                End;
            End;
        End;
    End;
End;

(**

  This method returns a string representation of the module.

  @precon  None.
  @postcon Returns a string representation of the module.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TINIModule.AsString(boolShowIdentifier, boolForDocumentation
    : Boolean): String;

Begin
  Result := ChangeFileExt(Inherited AsString(boolShowIdentifier,
      boolForDocumentation), '');
End;

(**

  This method creates the heirarchical sections from the section name given.

  @precon  None.
  @postcon Creates the heirarchical sections from the section name given and returns the
           most nested level.

  @param   strSectionName as a String
  @param   iLine          as an Integer
  @param   iColumn        as an Integer
  @return  a TLabelContainer

**)
Function TINIModule.BuildSection(strSectionName: String; iLine,
  iColumn : Integer): TLabelContainer;

Var
  S : TLabelContainer;
  slSections : TStringList;
  i : Integer;
  
Begin
  Result := Nil;
  S := FSections;
  slSections := TStringList.Create;
  Try
    slSections.Text := strSectionName;
    slSections.Text := StringReplace(slSections.Text, '.', #13#10, [rfReplaceAll]);
    slSections.Text := StringReplace(slSections.Text, '\', #13#10, [rfReplaceAll]);
    slSections.Text := StringReplace(slSections.Text, '/', #13#10, [rfReplaceAll]);
    For i := 0 To slSections.Count - 1 Do
      If slSections[i] <> '' Then
        Begin
          Result := S.Add(TLabelContainer.Create(slSections[i], scGlobal,
            iLine, iColumn, iiClassesLabel, Nil)) As TLabelContainer;
          S := Result;
        End;
  Finally
    slSections.Free;
  End;
End;

(**

  This method checks for the presents of line end characters in the token stream
  and returns true if found and moves to the next non comment token after the
  line end characters.

  @precon  None.
  @postcon Checks for the presents of line end characters in the token stream
           and returns true if found and moves to the next non comment token
           after the line end characters.

  @param   strMethod  as a String
  @return  a Boolean

**)
Function TINIModule.CheckLineEnd(strMethod: String): Boolean;
Begin
  Result := False;
  If Token.TokenType In [ttLineEnd] Then
    Begin
      Result := True;
      EatLineEnds;
    End
  Else
    ErrorAndSeekToken(strExpectedLineEnd, strMethod, Token.Token,
      strSeekableOnErrorTokens, stActual);
End;

(**

  This is the constructor method for the TINIModule class.

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
Constructor TINIModule.CreateParser(Source: String; strFileName: String;
  IsModified: Boolean; ModuleOptions: TModuleOptions);

Begin
  Inherited CreateParser(Source, strFileName, IsModified, ModuleOptions);
  FSource        := Source;
  AddTickCount('Start');
  CommentClass := TINIComment;
  FSections := Add(TLabelContainer.Create('Sections', scGlobal, 0, 0, iiModule,
    Nil)) As TLabelContainer;
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
      If FindElement(strErrors).ElementCount = 0 Then
        CheckReferences;
      AddTickCount('Refs');
      TidyUpEmptyElements;
    End;
End;

(**

  This is a destructor for the TINIModule class.

  @precon  None.
  @postcon Fress the memory fo this instance.


**)
Destructor TINIModule.Destroy;

Begin
  Inherited Destroy;
End;

(**

  This method eats the line ends when found and puts the token at the first
  token on the next non-null line.

  @precon  None.
  @postcon Eats the line ends when found and puts the token at the first
           token on the next non-null line.

**)
Procedure TINIModule.EatLineEnds;
Begin
  While Token.TokenType In [ttLineEnd] Do
    NextNonCommentToken;
End;

(**

  This method eats the whitespace when found and puts the token at the next non
  whitespace token.

  @precon  None.
  @postcon Eats the whitespace when found and puts the token at the next non
           whitespace token.

**)
Procedure TINIModule.EatWhitespace;
Begin
  While Token.TokenType In [ttWhiteSpace] Do
    NextNonCommentToken;
End;

(**

  This method tokenises the stream of text passed to the constructor and splits
  it into tokens.

  @precon  None.
  @postcon Tokenises the stream of text passed to the constructor and splits
           it into tokens.

**)
Procedure TINIModule.TokenizeStream;

Type
  (** State machine for block types. **)
  TBlockType = (btNoBlock, btLineComment);

Const
  (** Growth size of the token buffer. **)
  iTokenCapacity   = 100;
  strSingleSymbols = [#9, #10, #13, #32, ';', '(', ')', '*', '+', ',', '='];
  (** A set of characters for single quotes **)
  strSingleQuotes = [''''];
  (** A set of characters for double quotes **)
  strDoubleQuotes = ['"'];
  (** A set of identifier characters. **)
  strIdentifiers = ['a' .. 'z', 'A' .. 'Z', '_', '-', '%', #192 .. #214, #216 .. #246,
    #248 .. #255];
  (** A set of number characters. **)
  strNumbers  = ['.', '0' .. '9'];
  strAllChars = [#32 .. #255];
  (** A set of characters for general symbols **)
  strSymbols = (strAllChars - strIdentifiers - strNumbers - strSingleQuotes -
      strDoubleQuotes);

Var
  (** Token buffer. **)
  strToken    : String;
  CurCharType : TBADITokenType;
  LastCharType: TBADITokenType;
  BlockType   : TBlockType;
  (** Current line number **)
  iLine: Integer;
  (** Current column number **)
  iColumn: Integer;
  (** Token stream position. Fast to inc this than read the stream position. **)
  iStreamPos: Integer;
  (** Token line **)
  iTokenLine: Integer;
  (** Token column **)
  iTokenColumn: Integer;
  (** Current character position **)
  iStreamCount: Integer;
  Ch          : Char;
  LastChar    : Char;
  (** Token size **)
  iTokenLen: Integer;
  iChar    : Integer;

  (**

    This INLINE procedure changes the whitepace tokens for more human readable
    tokens.

    @precon  strToken must be a non-null string.
    @postcon Changes the whitepace tokens for more human readable
             tokens.

    @param   strToken as a String as a reference

  **)
  Procedure ProcessWhiteSpace(Var strToken: String); {$IFDEF D2005} InLine; {$ENDIF}
  Begin
    If strToken = #13 Then
      strToken := '<LF>';
    If strToken = #10 Then
      strToken := '<CR>';
  End;

Begin
  BlockType    := btNoBlock;
  iStreamPos   := 0;
  iTokenLine   := 1;
  iTokenColumn := 1;
  CurCharType  := ttUnknown;
  LastCharType := ttUnknown;
  iStreamCount := 0;
  iLine        := 1;
  iColumn      := 1;
  LastChar     := #0;
  strToken     := '';

  iTokenLen := 0;
  SetLength(strToken, iTokenCapacity);

  Try
    For iChar := 1 To Length(FSource) Do
      Begin
        Ch := FSource[iChar];
        Inc(iStreamCount);
        LastCharType := CurCharType;

        If IsInSet(Ch, strWhiteSpace) Then
          CurCharType := ttWhiteSpace
        Else If IsInSet(Ch, strLineEnd) Then
          CurCharType := ttLineEnd
        Else If IsInSet(Ch, strSymbols) Then
          CurCharType := ttSymbol
        Else If IsInSet(Ch, strIdentifiers) Then
          Begin
            If (LastCharType = ttNumber) And (IsInSet(Ch, ['A' .. 'F', 'a' .. 'f'])) Then
              CurCharType := ttNumber
            Else
              CurCharType := ttIdentifier;
          End
        Else If IsInSet(Ch, strNumbers) Then
          Begin
            CurCharType := ttNumber;
            If LastCharType = ttIdentifier Then
              CurCharType := ttIdentifier;
          End
        Else
          CurCharType := ttUnknown;

        // Check for line comments
        If (BlockType = btNoBlock) And (Ch = ';') Then
          BlockType := btLineComment;

        If (LastCharType <> CurCharType) Or (IsInSet(Ch, strSingleSymbols)) Or
          (IsInSet(LastChar, strSingleSymbols)) Then
          Begin
            If ((BlockType In [btLineComment]) And (CurCharType <> ttLineEnd)) Then
              Begin
                Inc(iTokenLen);
                If iTokenLen > Length(strToken) Then
                  SetLength(strToken, iTokenCapacity + Length(strToken));
                strToken[iTokenLen] := Ch;
              End
            Else
              Begin
                SetLength(strToken, iTokenLen);
                If iTokenLen > 0 Then
                  Begin
                    If BlockType = btLineComment Then
                      LastCharType := ttLineComment;
                    ProcessWhiteSpace(strToken);
                    AddToken(TTokenInfo.Create(strToken, iStreamPos, iTokenLine,
                        iTokenColumn, Length(strToken), LastCharType));
                  End;
               // Store Stream position, line number and column of
               // token start
                iStreamPos   := iStreamCount;
                iTokenLine   := iLine;
                iTokenColumn := iColumn;
                BlockType    := btNoBlock;
                iTokenLen    := 1;
                SetLength(strToken, iTokenCapacity);
                strToken[iTokenLen] := Ch;
              End;
          End
        Else
          Begin
            Inc(iTokenLen);
            If iTokenLen > Length(strToken) Then
              SetLength(strToken, iTokenCapacity + Length(strToken));
            strToken[iTokenLen] := Ch;
          End;

        Inc(iColumn);
        If Ch = #10 Then
          Begin
            Inc(iLine);
            iColumn := 1;
          End;
        LastChar := Ch;
      End;
    If iTokenLen > 0 Then
      Begin
        SetLength(strToken, iTokenLen);
        ProcessWhiteSpace(strToken);
        AddToken(TTokenInfo.Create(strToken, iStreamPos, iTokenLine, iTokenColumn,
            Length(strToken), LastCharType));
      End;
    AddToken(TTokenInfo.Create('<end-of-file>', iStreamPos, iTokenLine, iTokenColumn, 0,
        ttFileEnd));
  Except
    On E: Exception Do
      AddIssue(E.Message, scGlobal, 'TokenizeStream', 0, 0, etError);
  End
End;

(**

  This is the method that should be called to parse the previously parse tokens.

  @precon  None.
  @postcon Attempts to parse the token list and check it grammatically for
           Errors while providing delcaration elements for browsing.

**)
Procedure TINIModule.ParseTokens;
Begin
  Goal;
End;

(**

  This method returns an array of key words for use in the explorer module.

  @precon  None.
  @postcon Returns an array of key words for use in the explorer module.

  @return  a TKeyWords

**)
Function TINIModule.ReservedWords: TKeyWords;

Begin
  Result := Nil;
End;

(**

  This method parses the section portion of the INI grammar.

  @precon  None.
  @postcon Parses the section portion of the INI grammar.

  @return  a Boolean

**)
Function TINIModule.Section: Boolean;

Begin
  Result := SectionHeader;
  If Result Then
    While KeyValuePair Do;
  EatLineEnds;
End;

(**

  This method parses the section header element of the INI grammar.

  @precon  None.
  @postcon Parses the section header element of the INI grammar.

  @return  a Boolean

**)
Function TINIModule.SectionHeader: Boolean;

Var
  strSectionName : String;
  SectionToken : TTokenInfo;

Begin
  Result := Token.Token = '[';
  If Result Then
    Begin
      SectionToken := Token;
      NextNonCommentToken;
      While Token.Token <> ']' Do
        Begin
          strSectionName := strSectionName + Token.Token;
          NextNonCommentToken;
        End;
      If strSectionName <> '' Then
        Begin
          FCurrentSection := BuildSection(strSectionName, SectionToken.Line,
            SectionToken.Column);
          If Token.Token = ']' Then
            Begin
              NextNonCommentToken;
              EatWhitespace;
              CheckLineEnd('SectionHeader');
            End Else
              ErrorAndSeekToken(strLiteralExpected, 'Section', Token.Token, ['<LF>', '<CR>'], stActual);
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'Section', Token.Token, ['<LF>', '<CR>'],
          stActual);
    End;
End;

(**

  This method returns an array of key words for use in the explorer module.

  @precon  None.
  @postcon Returns an array of key words for use in the explorer module.

  @return  a TKeyWords

**)
Function TINIModule.Directives: TKeyWords;

Begin
  Result := Nil;
End;

(**

  This method tries to get a document comment from the previous token and return
  a TComment class to the calling routine.

  @note    All comments found are automatically added to the comment collection
           for disposal when the parser is destroyed.

  @precon  None.
  @postcon Returns the comment immediately before the current token else nil.

  @param   CommentPosition as a TCommentPosition
  @return  a TComment

**)
Function TINIModule.GetComment(CommentPosition: TCommentPosition): TComment;

Var
  T      : TTokenInfo;
  iOffset: Integer;
  iToken : TTokenIndex;

Begin
  Result := Nil;
  If CommentPosition = cpBeforeCurrentToken Then
    iOffset := -1
  Else
    iOffset := -2;
  iToken    := TokenIndex + iOffset;
  If iToken > -1 Then
    Begin
      While (iToken > -1) And ((Tokens[iToken] As TTokenInfo).TokenType
          In [ttLineEnd, ttLineContinuation]) Do
        Dec(iToken);
      If iToken > -1 Then
        Begin;
          T := Tokens[iToken] As TTokenInfo;
          If T.TokenType In [ttLineComment, ttBlockComment] Then
            Begin
              Result := TINIComment.CreateComment(T.Token, T.Line, T.Column);
              OwnedItems.Add(Result);
            End;
        End;
    End;
End;

(**

  This method returns a string representing the name of the module.
  @precon  None.
  @postcon Returns a string representing the name of the module.

  @return  a String

**)
Function TINIModule.GetModuleName: String;
Begin
  Result := ExtractFilename(FileName);
End;

(**

  This method process conditional compiler directives.

  @precon  None.
  @postcon Does nothings as conditional compilations is not supported.

  @param   iSkip as an Integer as a reference

**)
Procedure TINIModule.ProcessCompilerDirective(Var iSkip: Integer);

Begin
  // Do nothing, i.e. Conditional Compilation is NOT supported.
End;

(**

  This method does nothing as we are not referencing symbols in XML.

  @precon  None.
  @postcon Returns false always.

  @param   AToken as a TTokenInfo
  @return  a Boolean

**)
Function TINIModule.ReferenceSymbol(AToken: TTokenInfo): Boolean;

Begin
  Result := False;
End;

(**

  This method deletes any root elements which dont and items in them.

  @precon  None.
  @postcon Deletes any root elements which dont and items in them.

**)
Procedure TINIModule.TidyUpEmptyElements;

Var
  iElement: Integer;

Begin
  For iElement := ElementCount DownTo 1 Do
    If Elements[iElement].ElementCount = 0 Then
      If Elements[iElement] Is TLabelContainer Then
        If Pos('Definitions', Elements[iElement].Identifier) = 0 Then
          DeleteElement(iElement);
End;

(**

  This method is the starting position for the parsing of an Eidolon module. It
  finds the first non comment token and begins the grammar checking from their
  by deligating Syntax.

  @precon  None.
  @postcon It finds the first non comment token and begins the grammar checking
           from their by deligating Syntax.

**)
Procedure TINIModule.Goal;

Var
  C: TComment;

Begin
  Line   := 1;
  Column := 1;
  Try
    If TokenCount > 0 Then
      Begin
        // Find first non comment token
        While (Token.TokenType In [ttLineComment, ttBlockComment, ttLineEnd]) And
          Not EndOfTokens Do
          Begin
            If Token.TokenType In [ttLineComment, ttBlockComment] Then
              Begin
                C := TINIComment.CreateComment(Token.Token, Token.Line, Token.Column);
                AddBodyComment(C);
                If Comment = Nil Then
                  Comment := C;
              End;
            NextToken;
          End;
        EatLineEnds;
        // Check for end of file else must be identifier
        While Section Do;
        If Not (Token.TokenType In [ttFileEnd]) Then
          ErrorAndSeekToken(strExpectedFileEnd, 'Goal', Token.Token, ['<LF>', '<CR>'], stActual);
      End;
  Except
    On E: EParserAbort Do
      AddIssue(E.Message, scNone, 'Goal', 0, 0, etError);
  End;
End;

(**

  This method parses the Key Value pair element of the INI grammar.

  @precon  None.
  @postcon Parses the Key Value pair element of the INI grammar.

  @return  a Boolean

**)
Function TINIModule.KeyValuePair: Boolean;

Var 
  strKey : String;
  KVT : TTokenInfo;
  KVI: TKeyValuePair;
  
  
Begin
  Result := False;
  If Token.Token = '[' Then
    Exit;
  If Token.TokenType In [ttFileEnd] Then
    Exit;
  KVT := Nil;
  While (Token.Token <> '=') And Not (Token.TokenType In [ttLineEnd]) Do
    Begin
      strKey := strKey + Token.Token;
      If KVT = Nil Then
        KVT := Token;
      NextNonCommentToken;
    End;
  If Token.Token = '=' Then
    Begin
      Result := True;
      NextNonCommentToken;
      If strKey <> '' Then
        Begin
          KVI := FCurrentSection.Add(TKeyValuePair.Create(strKey, scGlobal, KVT.Line,
            KVT.Column, iiPublicField, Nil)) As TKeyValuePair;
          While Not (Token.TokenType In [ttLineEnd]) Do
            Begin
              KVI.AddToken(Token.Token);
              NextNonCommentToken;
              If Token.TokenType In [ttFileEnd] Then
                Exit;
            End;
          CheckLineEnd('KeyValuePair');
        End Else
          ErrorAndSeekToken(strExpectedKey, 'KeyValuePair', Token.Token, ['<LF>', '<CR>'],
            stActual);
    End;
  EatLineEnds;
End;

{ TKeyValuePair }

(**

  This method returns a string representation of the key value pair.

  @precon  None.
  @postcon Returns a string representation of the key value pair.

  @param   boolShowIdenifier    as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TKeyValuePair.AsString(boolShowIdenifier, boolForDocumentation: Boolean): String;

Begin
  Result := BuildStringRepresentation(True, False, '=',
    BrowseAndDocItOptions.MaxDocOutputWidth, [#32..#255], [#32..#255], []);
End;

(**

  This is a getter method for the Name property.

  @precon  None.
  @postcon Returns the internal name of the element.

  @return  a String

**)
Function TKeyValuePair.GetName: String;

Begin
  Result := Format('%s%4.4d', [Identifier, Random(9999)]);
End;

(** Register the file source code extensions that can be parsed by this module. **)
Initialization
  ModuleDispatcher.Add('.ini', TINIModule, True, ctCPPBlock, ctCPPLine, ctCPPBlock);
  ModuleDispatcher.Add('.tli', TINIModule, True, ctCPPBlock, ctCPPLine, ctCPPBlock);
End.
