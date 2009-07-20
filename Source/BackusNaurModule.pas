(**

  BackusNaurModule : A unit to tokenize Backus-Naur Grammar.

  @Version    1.0
  @Date       20 Jul 2009
  @Author     David Hoyle

**)
Unit BackusNaurModule;

Interface

Uses
  SysUtils, Windows, Contnrs, Classes, BaseLanguageModule;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This class represents the BNF rule found in the code. **)
  TBNFRule = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function  AsString(boolShowIdenifier, boolForDocumentation : Boolean) : String;
      Override;
  End;

  (** A pascal specific implementation of comments. **)
  TBackusNaurComment = Class(TComment)
  Public
    Class Function CreateComment(strComment: String; iLine,
      iCol: Integer): TComment; Override;
  End;

  (** This is the main class for dealing with backus-naur grammar files. **)
  TBackusNaurModule = Class(TBaseLanguageModule)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FSource        : String;
    FRules         : TLabelContainer;
    FRequiredRules : TStringList;
    { Grammar Parsers }
    Procedure Goal;
    Procedure Syntax;
    Procedure Rule;
    Procedure Expression(R : TBNFRule);
    Procedure RepeatOperator(R : TBNFRule);
    Procedure List(R : TBNFRule);
    Procedure Term(R : TBNFRule);
    Procedure LineEnd;
    (* Helper method to the grammar parsers *)
    Procedure TokenizeStream;
    Procedure ParseTokens;
    Procedure EatLineEnds;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetComment(
      CommentPosition : TCommentPosition = cpBeforeCurrentToken) : TComment;
      Override;
    procedure TidyUpEmptyElements;
    Procedure CheckRules;
  Public
    Constructor CreateParser(Source : String; strFileName : String;
      IsModified : Boolean; ModuleOptions : TModuleOptions); Override;
    Destructor Destroy; Override;
    Function KeyWords : TKeyWords; Override;
    Procedure ProcessCompilerDirective(var iSkip : Integer); Override;
    Function ReferenceSymbol(AToken : TTokenInfo) : Boolean; Override;
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

Uses
  DGHLibrary;

Resourcestring
  (** This is an error message for duplicate identifiers. **)
  strDuplicateIdentifierFound = 'Duplicate Identifier ''%s'' found at line %d column %d.';

Const
  (** A set of characters for general symbols **)
  strSymbols : Set Of AnsiChar = ['&', '(', ')', '*', '+', ',', '-', '.', '/', ':',
    ';', '=', '@', '[', ']', '^', '|'];
  (** A set of characters for quotes **)
  strQuote : Set Of AnsiChar = ['''', '"'];
  (** A set of identifier characters. **)
  strIdentifiers :  Set Of AnsiChar = ['a'..'z', 'A'..'Z', '<', '>'];

  (** A set of reserved words (not used in this parser.) **)
  strReservedWords : Array[1..1] Of String = ('(none)');

  (** This is a list of reserved, directives word and a semi colon which are
      token that can be sort as then next place to start parsing from when an
      error is  encountered. **)
  strSeekableOnErrorTokens : Array[1..3] Of String = (#10, #13, #13#10);

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
class function TBackusNaurComment.CreateComment(strComment: String; iLine,
  iCol: Integer): TComment;

begin
  Result := Nil;
  If Length(strComment) > 0 Then
    Begin
      Case strComment[1] Of
        '/' : strComment := Copy(strComment, 2, Length(strComment) - 1);
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
                  Result := Create(strComment, iLine, iCol);
                End
              Else If strComment[1] = '*' Then
                Begin;
                  strComment := Copy(strComment, 2, Length(strComment) - 2);
                  Result := Create(strComment, iLine, iCol);
                End;
            End;
        End;
    End;
end;

(**

  This method checks the rules that have been collected in Term() to see if they
  have been defined in the grammar. Any missing rules are output as warnings.

  @precon  None.
  @postcon Checks the rules that have been collected in Term() to see if they
           have been defined in the grammar. Any missing rules are output as
           warnings.

**)
procedure TBackusNaurModule.CheckRules;

var
  iRule: Integer;
  iIndex: Integer;
  iCode: Integer;
  iLine: Integer;
  iColumn: Integer;

begin
  If FRules <> Nil Then
    Begin
      For iRule := 1 To FRules.ElementCount Do
        If FRequiredRules.Find(FRules.Elements[iRule].Identifier, iIndex) Then
          Begin
            FRequiredRules.Objects[iIndex] := TObject(0);
            FRules.Elements[iRule].Referenced := True;
          End;
      For iRule := 0 To FRequiredRules.Count - 1 Do
        If FRequiredRules.Objects[iRule] <> Nil Then
          Begin
            iCode := Integer(FRequiredRules.Objects[iRule]);
            iLine := iCode And $FFFF;
            iColumn := (iCode And $FFFF0000) Shr 16;
            AddIssue(Format('The rule ''%s'' has not been defined.',
              [FRequiredRules[iRule]]), scNone, 'CheckRules', iLine, iColumn,
                etWarning);
          End;
      For iRule := 1 To FRules.ElementCount Do
        If Not FRules.Elements[iRule].Referenced Then
          If Not Like('<*Goal*>', FRules.Elements[iRule].Identifier) Then
            AddIssue(Format('The rule ''%s'' has not been referenced in the code.',
              [FRules.Elements[iRule].Identifier]), scNone, 'CheckRules',
              FRules.Elements[iRule].Line, FRules.Elements[iRule].Column, etHint);
    End;
end;

(**

  This is the constructor method for the TBackusnaurModule class.

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
Constructor TBackusNaurModule.CreateParser(Source : String; strFileName : String;
  IsModified : Boolean; ModuleOptions : TModuleOptions);

Var
  boolCascade : Boolean;

Begin
  Inherited CreateParser(Source, strFileName, IsModified, ModuleOptions);
  CompilerDefines.Assign(BrowseAndDocItOptions.Defines);
  FSource := Source;
  FRules := Nil;
  FRequiredRules := TStringList.Create;
  FRequiredRules.Duplicates := dupIgnore;
  FRequiredRules.Sorted := True;
  AddTickCount('Start');
  CommentClass := TBackusNaurComment;
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
      CheckRules;
      AddTickCount('Refs');
      boolCascade := True;
      If moCheckForDocumentConflicts In ModuleOptions Then
        CheckDocumentation(boolCascade);
      AddTickCount('Check');
      TidyUpEmptyElements;
    End;
End;

(**


  This is a destructor for the TBackusNaurModule class.

  @precon  None.
  @postcon Fress the memory fo this instance.


**)
Destructor TBackusNaurModule.Destroy;
begin
  FRequiredRules.Free;
  Inherited Destroy;
end;

(**

  This method moves to the next token is a line end token is found until is
  finds a non-line end token.

  @precon  None.
  @postcon Moves to the next token is a line end token is found until is
           finds a non-line end token.

**)
procedure TBackusNaurModule.EatLineEnds;
begin
  While token.TokenType In [ttLineEnd] Do
    NextNonCommentToken;
end;

(**

  This method parsers an expression as a list and an optional a further
  expression.

  @precon  R must be a valid instance of a TBNFFule class.
  @postcon Parsers an expression as a list and an optional a further
           expression.

  @param   R as a TBNFRule

**)
procedure TBackusNaurModule.Expression(R : TBNFRule);
begin
  List(R);
  If Token.Token = '|' Then
    Begin
      AddToExpression(R);
      Expression(R);
    End;
end;

(**

  This method tokenises the stream of text passed to the constructor and splits
  it into tokens.

  @precon  None.
  @postcon Tokenises the stream of text passed to the constructor and splits
           it into tokens.

**)
Procedure TBackusNaurModule.TokenizeStream;

Type
  (** State machine for block types. **)
  TBlockType = (btNoBlock, btStringLiteral, btLineComment, btFullComment,
    btCompoundSymbol, btRule, btTextRule);

Const
  (** Growth size of the token buffer. **)
  iTokenCapacity = 100;
  strSingleSymbols : Set Of AnsiChar = ['(', ')', ';', ',', '[', ']', '^',
    '-', '+', '/', '*'];

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

  iTokenLen := 0;
  SetLength(strToken, iTokenCapacity);

  Try
    For iChar := 1 To Length(FSource) Do
      Begin
        ch := FSource[iChar];
        Inc(iStreamCount);
        LastCharType := CurCharType;

        {$IFNDEF D2009}
        If ch In strWhiteSpace Then
        {$ELSE}
        If CharInSet(ch, strWhiteSpace) Then
        {$ENDIF}
          CurCharType := ttWhiteSpace
        {$IFNDEF D2009}
        Else If ch In strLineEnd Then
        {$ELSE}
        Else If CharInSet(ch, strLineEnd) Then
        {$ENDIF}
          CurCharType := ttLineEnd
        {$IFNDEF D2009}
        Else If ch In strQuote Then
        {$ELSE}
        Else If CharInSet(ch, strQuote) Then
        {$ENDIF}
          CurCharType := ttStringLiteral
        {$IFNDEF D2009}
        Else If ch In strSymbols Then
        {$ELSE}
        Else If CharInSet(ch, strSymbols) Then
        {$ENDIF}
          CurCharType := ttSymbol
        {$IFNDEF D2009}
        Else If ch In strIdentifiers Then
        {$ELSE}
        Else If CharInSet(ch, strIdentifiers) Then
        {$ENDIF}
          CurCharType := ttIdentifier
        Else
          CurCharType := ttUnknown;

        // Check for full block comments
        If (BlockType = btNoBlock) And (LastChar = '/') And (Ch = '*') Then
          BlockType := btFullComment;

        // Check for line comments
        If (BlockType = btNoBlock) And (LastChar = '/') And (Ch = '/') Then
          BlockType := btLineComment;

        If (BlockType = btNoBlock) And (LastChar = '<') Then
          BlockType := btRule;

        If (BlockType = btNoBlock) And (LastChar = '?') And (LastCharType <> ttDirective) Then
          BlockType := btTextRule;

        {$IFNDEF D2009}
        If (LastCharType <> CurCharType) Or (Ch In strSingleSymbols) Or
          (LastChar In strSingleSymbols)Then
        {$ELSE}
        If (LastCharType <> CurCharType) Or (CharInSet(Ch, strSingleSymbols)) Or
          (CharInSet(LastChar, strSingleSymbols)) Then
        {$ENDIF}
          Begin
            If ((BlockType In [btStringLiteral, btLineComment, btRule, btTextRule]) And (CurCharType <> ttLineEnd)) Or
              (BlockType In [btFullComment, btCompoundSymbol]) Then
              Begin
                Inc(iTokenLen);
                If iTokenLen > Length(strToken) Then
                  SetLength(strToken, iTokenCapacity + Length(strToken));
                strToken[iTokenLen] := Ch;
              End Else
              Begin
                SetLength(strToken, iTokenLen);
                If iTokenLen > 0 Then
                  {$IFNDEF D2009}
                  If Not (strToken[1] In strWhiteSpace) Then
                  {$ELSE}
                  If Not (CharInSet(strToken[1], strWhiteSpace)) Then
                  {$ENDIF}
                    Begin
                      If BlockType = btLineComment Then
                        LastCharType := ttLineComment;
                      AddToken(TTokenInfo.Create(strToken, iStreamPos,
                        iTokenLine, iTokenColumn, Length(strToken), LastCharType));
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

        // Check for the end of a block comment
        If (BlockType = btFullComment) And (LastChar = '*') And (Ch = '/') Then
          Begin
            BlockType := btNoBlock;
            CurCharType := ttBlockComment;
          End;

        If (BlockType = btRule) And (Ch = '>') Then
          Begin
            BlockType := btNoBlock;
            CurCharType := ttIdentifier;
          End;
        If (BlockType = btTextRule) And (Ch = '?') Then
          Begin
            BlockType := btNoBlock;
            CurCharType := ttDirective;
          End;

        // Check for string literals
        If CurCharType = ttStringLiteral Then
          If BlockType = btStringLiteral Then
            BlockType := btNoBlock
          Else If BlockType = btNoBlock Then
            BlockType := btStringLiteral;

        If BlockType = btCompoundSymbol Then
          BlockType := btNoBlock;

        Inc(iColumn);
        If Ch = #10 Then
          Begin
            Inc(iLine);
            iColumn := 1;
            If BlockType In [btLineComment, btStringLiteral] Then
              BlockType := btNoBlock;
          End;
        LastChar := Ch;
      End;
      If iTokenLen > 0 Then
        Begin
          SetLength(strToken, iTokenLen);
          {$IFNDEF D2009}
          If Not (strToken[1] In strWhiteSpace) Then
          {$ELSE}
          If Not (CharInSet(strToken[1], strWhiteSpace)) Then
          {$ENDIF}
            AddToken(TTokenInfo.Create(strToken, iStreamPos,
              iTokenLine, iTokenColumn, Length(strToken), LastCharType));
        End;
    AddToken(TTokenInfo.Create('', iStreamPos, iTokenLine, iTokenColumn, 0,
      ttFileEnd));
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
procedure TBackusNaurModule.ParseTokens;
begin
  Goal;
end;

(**

  This method returns an array of key words for use in the explorer module.

  @precon  None.
  @postcon Returns an array of key words for use in the explorer module.

  @return  a TKeyWords

**)
function TBackusNaurModule.KeyWords: TKeyWords;

Var
  i : Integer;

begin
  SetLength(Result, Succ(High(strReservedWords)));
  For i := Low(strReservedWords) To High(strReservedWords) Do
    Result[i] := strReservedWords[i];
end;

(**

  This method checks to see if the token is a line end token, if so moves to
  the next token else raises and error and seeks the end of the line.

  @precon  None.
  @postcon Checks to see if the token is a line end token, if so moves to
           the next token else raises and error and seeks the end of the line.

**)
procedure TBackusNaurModule.LineEnd;
begin
  If Token.TokenType In [ttLineEnd] Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken('Expected ''<end-of-line>'' but ''%s'' found.', 'LineEnd',
      Token.Token, strSeekableOnErrorTokens, stActual);
end;

(**

  This method processes the list of terms in the grammar.

  @precon  R must be a valid instance of a TBNFRule class.
  @postcon Processes the list of terms in the grammar.

  @param   R as a TBNFRule

**)
procedure TBackusNaurModule.List(R : TBNFRule);

Var
  StartToken : TTokenInfo;

begin
  StartToken := Token;
  If Token.Token = '(' Then
    Begin
      AddToExpression(R);
      Expression(R);
      If Token.Token = ')' Then
        AddToExpression(R)
      Else
        ErrorAndSeekToken(strLiteralExpected, 'List', ')',
          strSeekableOnErrorTokens, stActual);
      RepeatOperator(R);
      List(R);
    End Else
  If Token.Token = '[' Then
    Begin
      AddToExpression(R);
      Expression(R);
      If Token.Token = ']' Then
        AddToExpression(R)
      Else
        ErrorAndSeekToken(strLiteralExpected, 'List', ']',
          strSeekableOnErrorTokens, stActual);
      RepeatOperator(R);
      List(R);
    End Else
    Begin
      Term(R);
      If StartToken <> Token Then
        List(R);
    End;
end;

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
Function TBackusNaurModule.GetComment(
  CommentPosition : TCommentPosition) : TComment;

Var
  T : TTokenInfo;
  iOffset : Integer;
  iToken: TTokenIndex;

Begin
  Result := Nil;
  If CommentPosition = cpBeforeCurrentToken Then
    iOffset := -1
  Else
    iOffset := -2;
  iToken := TokenIndex + iOffset;
  If iToken > -1 Then
    Begin
      While (iToken > -1) And ((Tokens[iToken] As TTokenInfo).TokenType In
        [ttLineEnd, ttLineContinuation]) Do
        Dec(iToken);
      If iToken > -1 Then
        Begin;
          T := Tokens[iToken] As TTokenInfo;
          If T.TokenType In [ttLineComment, ttBlockComment] Then
            Begin
              Result := TBackusNaurComment.CreateComment(T.Token, T.Line, T.Column);
              OwnedItems.Add(Result);
            End;
        End;
    End;
{
      T := Tokens[iToken] As TTokenInfo;
      If T.TokenType In [ttLineComment] Then
        If T.Line = iLastCmtLine - 1 Then
          Begin
            iLine := T.Line;
            iColumn := T.Column;
            If strComment <> '' Then
              strComment := #13#10 + strComment;
            strComment := T.Token + strComment;
            iLastCmtLine := T.Line;
          End Else
            Break;
      Dec(iToken);
}
End;

(**

  This method process conditional compiler directives.

  @precon  None.
  @postcon Does nothings as conditional compilations is not supported.

  @param   iSkip as an Integer as a reference

**)
Procedure TBackusNaurModule.ProcessCompilerDirective(var iSkip : Integer);

Begin
  // Do nothing, i.e. Conditional Compilation is NOT supported.
End;

(**

  This method returns false and does not reference any tokens.

  @precon  None.
  @postcon Does nothing.

  @param   AToken as a TTokenInfo
  @return  a Boolean

**)
Function TBackusNaurModule.ReferenceSymbol(AToken : TTokenInfo) : Boolean;

Begin
  Result := False;
End;

(**

  This method checks to see if the token is either one of the repeat tokens and
  adds it to the rule if so.

  @precon  R must be a valid instance of a TBNFRule class.
  @postcon Checks to see if the token is either one of the repeat tokens and
           adds it to the rule if so.

  @param   R as a TBNFRule

**)
procedure TBackusNaurModule.RepeatOperator(R: TBNFRule);
begin
  If (Token.Token = '*') Or (Token.Token = '+') Then
    AddToExpression(R);
end;

(**

  This method processes the Terms of the grammar.

  @precon  R must be a valid instance of a TBNFRule class.
  @postcon Processes the Terms of the grammar.

  @param   R as a TBNFRule

**)
procedure TBackusNaurModule.Term(R : TBNFRule);
var
  iCode: Integer;

begin
  If Token.TokenType In [ttIdentifier] Then
    Begin
      If Like('<*>', Token.Token) Then
        Begin
          iCode := Token.Line Or (Token.Column Shl 16);
          FRequiredRules.AddObject(Token.Token, TObject(iCode));
          AddToExpression(R)
        End
      Else
        ErrorAndSeekToken('Rules should start and end with ''<'' and ''>'' respectively.',
          'Term', '', strSeekableOnErrorTokens, stActual);
    End Else
  If Token.TokenType In [ttDirective, ttStringLiteral] Then
    AddToExpression(R);
end;

(**

  This method deletes any root elements which don't and items in them.

  @precon  None.
  @postcon Deletes any root elements which don't and items in them.

**)
procedure TBackusNaurModule.TidyUpEmptyElements;

Var
  iElement : Integer;

begin
  For iElement := ElementCount DownTo 1 Do
    If Elements[iElement].ElementCount = 0 Then
      DeleteElement(iElement);
end;

(**

  This method parses the basic rule definition as defined by the grammar below.

  @precon  None.
  @postcon Parses the basic rule definition as defined by the grammar below.

**)
Procedure TBackusNaurModule.Rule;

Var
  R : TBNFRule;

begin
  If Token.TokenType In [ttIdentifier] Then
    Begin
      If Like('<*>', Token.Token) Then
        Begin
          R := TBNFRule.Create(Token.Token, scPublic, Token.Line, Token.Column,
            iiPublicType, GetComment);
          If FRules = Nil Then
            FRules := Add(TLabelContainer.Create('Rules', scNone, 0, 0,
              iiPublicTypesLabel, Nil)) As TLabelContainer;
          If FRules.Add(R) <> R Then
            Begin
              AddIssue(Format(strDuplicateIdentifierFound, [Token.Token, Token.Line,
                Token.Column]), scNone, 'Rule', Token.Line, Token.Column, etError);
            End;
          NextNonCommentToken;
          If Token.Token = '::=' Then
            Begin
              NextNonCommentToken;
              Expression(R);
            End Else
              ErrorAndSeekToken('Expected ''::='' but ''%s'' found.', 'Syntax',
                Token.Token, strSeekableOnErrorTokens, stActual);
          LineEnd;
          If R.TokenCount = 0 Then
            AddIssue(Format('The rule ''%s'' has no definition.', [R.Identifier]),
              scNone, 'Rule', R.Line, R.Column, etWarning);
        End Else
          ErrorAndSeekToken('Expected ''<rule>'' but ''%s'' found.', 'Syntax', Token.Token,
            strSeekableOnErrorTokens, stActual);
      End Else
        ErrorAndSeekToken('Rules should start and end with ''<'' and ''>'' respectively.',
          'Syntax', Token.Token, strSeekableOnErrorTokens, stActual);
end;

(**

  This method processes the overall syntax of the code by repeatedly calling
  rule until there are no more rules and the end if the file is found.

  @precon  None.
  @postcon Processes the overall syntax of the code by repeatedly calling
           rule until there are no more rules and the end if the file is found.

**)
procedure TBackusNaurModule.Syntax;

begin
  While Not( Token.TokenType In [ttFileEnd]) Do
    Begin
      Rule;
      EatLineEnds;
    End;
end;

(**

  This method returns a string representation of the module.

  @precon  None.
  @postcon Returns a string representation of the module.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TBackusNaurModule.AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String;

Begin
  Result := ChangeFileExt(Inherited AsString(boolShowIdentifier,
    boolForDocumentation), '');
End;

(**

  This method is the starting position for the parsing of an backus-naur
  module. It finds the first non comment token and begins the grammar checking
  from their by deligating Syntax.

  @precon  None.
  @postcon It finds the first non comment token and begins the grammar checking
           from their by deligating Syntax.

**)
procedure TBackusNaurModule.Goal;

begin
  Try
    If TokenCount > 0 Then
      Begin
        // Find first non comment token
        While (Token.TokenType In [ttLineComment, ttBlockComment, ttLineEnd]) And
          Not EndOfTokens Do
          NextNonCommentToken;
        // Check for end of file else must be identifier
        If Not EndOfTokens Then
          Begin
            Comment := GetComment;
            Syntax;
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

{ TBNFRule }

(**

  This method returns a string representation of the BNG Rule.

  @precon  None.
  @postcon Returns a string representation of the BNG Rule.

  @param   boolShowIdenifier    as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TBNFRule.AsString(boolShowIdenifier,
  boolForDocumentation: Boolean): String;
begin
  Result := Name + ' ::= ' +
    BuildStringRepresentation(False, boolForDocumentation, '::=',
      BrowseAndDocItOptions.MaxDocOutputWidth);
end;

End.
