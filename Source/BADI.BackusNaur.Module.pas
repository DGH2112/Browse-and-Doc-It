(**

  BackusNaurModule : A unit to tokenize Backus-Naur Grammar.

  @Version    1.0
  @Author     David Hoyle
  @Date       17 Aug 2019

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
Unit BADI.BackusNaur.Module;

Interface

Uses
  SysUtils,
  Windows,
  Contnrs,
  Classes,
  BADI.Base.Module,
  BADI.ElementContainer,
  BADI.Comment,
  BADI.Types,
  BADI.TokenInfo,
  BADI.BackusNaur.Rule;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This is the main class for dealing with backus-naur grammar files. **)
  TBackusNaurModule = Class(TBaseLanguageModule)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FSource        : String;
    FRules         : TLabelContainer;
    FRequiredRules : TStringList;
    FUseSemiColon  : Boolean;
    FMainGoal      : String;
    { Grammar Parsers }
    Procedure Goal;
    Procedure Syntax;
    Procedure Rule;
    Procedure Expression(R : TBackusNaurRule);
    Procedure RepeatOperator(R : TBackusNaurRule);
    Procedure List(R : TBackusNaurRule);
    Procedure SimpleExpression(R : TBackusNaurRule);
    Procedure Term(R : TBackusNaurRule);
    Procedure Literal(R : TBackusNaurRule);
    Function  CharRef(R : TBackusNaurRule) : Boolean;
    Function  LiteralChar(R : TBackusNaurRule) : Boolean;
    Function  DecChar(R : TBackusNaurRule) : Boolean;
    Function  HexChar(R : TBackusNaurRule) : Boolean;
    Procedure Terminator;
    Procedure LineEnd;
    procedure SemiColon;
    (* Helper method to the grammar parsers *)
    Procedure TokenizeStream;
    Procedure ParseTokens;
    Procedure EatLineEnds;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetComment(Const CommentPosition : TCommentPosition = cpBeforeCurrentToken) : TComment;
      Override;
    procedure TidyUpEmptyElements;
    Procedure CheckRules;
    Procedure ProcessTags;
    Function GetModuleName : String; Override;
  Public
    Constructor CreateParser(Const Source, strFileName : String; Const IsModified : Boolean; 
      Const ModuleOptions : TModuleOptions); Override;
    Destructor Destroy; Override;
    Function ReservedWords : TKeyWords; Override;
    Function Directives : TKeyWords; Override;
    Procedure ProcessCompilerDirective(var iSkip : Integer); Override;
    Function ReferenceSymbol(Const AToken : TTokenInfo) : Boolean; Override;
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

Uses
  BADI.Options,
  BADI.ResourceStrings,
  BADI.Functions,
  BADI.Constants,
  BADI.Module.Dispatcher,
  BADI.BackusNaur.Comment;

Resourcestring
  (** This is an error message for duplicate identifiers. **)
  strDuplicateIdentifierFound = 'Duplicate Identifier ''%s'' found at line ' +
    '%d column %d.';
  (** This is an error message for a rule that has not been defined. **)
  strRuleHasNotBeenDefined = 'The rule ''%s'' has not been defined at line ' +
    '%d column %d.';
  (** This is an error message for a rule that has not been referenced in code. **)
  strTheRuleHasNotBeenRef = 'The rule ''%s'' has not been referenced in the ' +
    'code at line %d column %d.';
  (** This is an error message for an expected end of line. **)
  strExpectedLineEnd = 'Expected ''<end-of-line>'' but ''%s'' found at line ' +
    '%d column %d.';
  (** This is an error message for rules not start and end with <>. **)
  strRulesShouldStartAndEndWith = 'Rules should start and end with ''<'' and' +
    ' ''>'' respectively at line %d column %d.';
  (** This is an error message for expecting ::= but not found. **)
  strExpectedEquality = 'Expected ''::='' but ''%s'' found at line %d column' +
    ' %d.';
  (** This is an error message for a rule with no definition. **)
  strTheRuleHasNoDefinition = 'The rule ''%s'' has no definition at line %d ' +
    'column %d.';
  (** This is an error message for a rule expected but not found. **)
  strExpectedRuleButFound = 'Expected ''<rule>'' but ''%s'' found at line %d' +
    ' column %d.';
  (** This is an error message for expecting a ; but one not found. **)
  strExpectedSemiColon = 'Expected '';'' but ''%s'' found at line %d column ' +
    '%d.';
  (** This is an error message for an invalid hexidecimal character. **)
  strInvalidHexCharRef = 'Invalid hexidecimal character reference ''%s'' at ' +
    'line %d column %d.';
  (** This is an error message for an invalid decimal character. **)
  strInvalidDecCharRef = 'Invalid decimal character reference ''%s'' at line ' +
    '%d column %d.';
  (** This is an error message for a missing terminal character. **)
  strMissingTerminalChar = 'Missing terminal character ''%s'' at line %d col' +
  'umn %d.';
  (** This is an error message where a rule or literal is expected but not found. **)
  strExpectedRuleOrLiteral = 'Expected rule or literal but ''%s'' found at l' +
    'ine %d column %d.';
  (** This is an error message where a rule is NULL. **)
  strNULLIdentifierFound = 'NULL Identifier ''%s'' found at line %d column %d.';

Const
  (** A set of characters for general symbols **)
  strSymbols : Set Of AnsiChar = ['&', '(', ')', '*', '+', ',', '-', '.', '/',
    ':', ';', '=', '@', '[', ']', '^', '|'];
  (** A set of characters for single quotes **)
  strSingleQuotes : Set Of AnsiChar = [''''];
  (** A set of characters for double quotes **)
  strDoubleQuotes : Set Of AnsiChar = ['"'];
  (** A set of identifier characters. **)
  strIdentifiers :  Set Of AnsiChar = ['a'..'z', 'A'..'Z', '<', '>'];
  (** A set of number characters. **)
  strNumbers:  Set Of AnsiChar = ['#', '$', '0'..'9'];

  (** A set of reserved words (not used in this parser.) **)
  strReservedWords : Array[1..1] Of String = ('(none)');

  (** This is a list of reserved, directives word and a semi colon which are
      token that can be sort as then next place to start parsing from when an
      error is  encountered. **)
  strSeekableOnErrorTokens : Array[1..2] Of String = (';', '<line-end>');

(**

  This method checks for a CharRef and return true one was found.

  @precon  T must be a valid instance of a TBackusNaurRule class.
  @postcon Checks for a CharRef and return true one was found. Additionally,
           the token is added to the BNF rule.

  @param   R as a TBackusNaurRule
  @return  a Boolean

**)
Function TBackusNaurModule.CharRef(R: TBackusNaurRule) : Boolean;
begin
  Result := DecChar(R);
  If Not Result Then
    Result := HexChar(R);
  If Not Result Then
    Result := LiteralChar(R);
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
            AddIssue(Format(strRuleHasNotBeenDefined, [FRequiredRules[iRule],
              iLine, iColumn]), scNone, iLine, iColumn, etWarning, Self);
          End;
      For iRule := 1 To FRules.ElementCount Do
        If Not FRules.Elements[iRule].Referenced Then
          If Not Like('<' + FMainGoal + '>', FRules.Elements[iRule].Identifier) Then
            AddIssue(Format(strTheRuleHasNotBeenRef,
              [FRules.Elements[iRule].Identifier, FRules.Elements[iRule].Line,
              FRules.Elements[iRule].Column]), scNone, FRules.Elements[iRule].Line,
                FRules.Elements[iRule].Column, etHint, Self);
    End;
end;

(**

  This is the constructor method for the TBackusnaurModule class.

  @precon  Source is a valid TStream descendant containing as stream of text, that is the contents of a 
           source code module and Filename is the file name of the module being parsed and IsModified 
           determines if the source code module has been modified since the last save to disk.
  @postcon Creates an instance of the module parser.

  @param   Source        as a String as a constant
  @param   strFileName   as a String as a constant
  @param   IsModified    as a Boolean as a constant
  @param   ModuleOptions as a TModuleOptions as a constant

**)
Constructor TBackusNaurModule.CreateParser(Const Source, strFileName : String;
  Const IsModified : Boolean; Const ModuleOptions : TModuleOptions);

Var
  boolCascade : Boolean;

Begin
  Inherited CreateParser(Source, strFileName, IsModified, ModuleOptions);
  CompilerDefines.Assign(BADIOptions.Defines);
  FSource := Source;
  FRules := Nil;
  FMainGoal := '*goal*';
  FRequiredRules := TStringList.Create;
  FRequiredRules.Duplicates := dupIgnore;
  FRequiredRules.Sorted := True;
  FRequiredRules.CaseSensitive := True;
  AddTickCount('Start');
  CommentClass := TBackusNaurComment;
  TokenizeStream;
  AddTickCount('Tokenize');
  If moParse In ModuleOptions Then
    Begin
      ParseTokens;
      AddTickCount('Parse');
      Add(strErrors, iiErrorFolder, scNone);
      Add(strWarnings, iiWarningFolder, scNone);
      Add(strHints, iiHintFolder, scNone);
      Add(strDocumentationConflicts, iiDocConflictFolder, scNone);
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

  This method checks the token to see if its a Decimal Character, if so adds it
  to the rule and returns true.

  @precon  R must be a valid instance of a TBackusNaurRule class.
  @postcon Checks the token to see if its a Decimal Character, if so adds it
           to the rule and returns true.

  @param   R as a TBackusNaurRule
  @return  a Boolean

**)
Function TBackusNaurModule.DecChar(R: TBackusNaurRule) : Boolean;

var
  i: Integer;
  strToken : String;
  iErrors: Integer;

begin
  strToken := Token.Token;
  Result := strToken[1] = '#';
  If Result Then
    Begin
      iErrors := 0;
      For i := 2 To Length(strToken) Do
        If Not IsInSet(strToken[i], ['0'..'9']) Then
          Inc(iErrors);
      If iErrors = 0 Then
        AddToExpression(R)
      Else
        ErrorAndSeekToken(strInvalidDecCharRef, Token.Token, strSeekableOnErrorTokens, stActual, Self);
    End;
end;

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

  @param   R as a TBackusNaurRule

**)
procedure TBackusNaurModule.Expression(R : TBackusNaurRule);

Var
  iTokens : Integer;

begin
  iTokens := 0;
  If R <> Nil Then
    iTokens := R.TokenCount;
  List(R);
  If R <> Nil Then
    If iTokens = R.TokenCount Then
      Begin
        ErrorAndSeekToken(strExpectedRuleOrLiteral, Token.Token, strSeekableOnErrorTokens, stActual,
          Self);
        Exit;
      End;
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
  TBlockType = (btNoBlock, btSingleLiteral, btDoubleLiteral, btLineComment,
    btFullComment, btCompoundSymbol, btRule, btTextRule, btDecChar, btHexChar);

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
      Inc(iStreamCount);
      LastCharType := CurCharType;

      If IsInSet(ch, strWhiteSpace) Then
        CurCharType := ttWhiteSpace
      Else If IsInSet(ch, strLineEnd) Then
        CurCharType := ttLineEnd
      Else If IsInSet(ch, strSingleQuotes) Then
        CurCharType := ttSingleLiteral
      Else If IsInSet(ch, strDoubleQuotes) Then
        CurCharType := ttDoubleLiteral
      Else If IsInSet(ch, strSymbols) Then
        CurCharType := ttSymbol
      Else If IsInSet(ch, strIdentifiers) Then
        Begin
          If (LastCharType = ttNumber) And (IsInSet(Ch, ['A'..'F', 'a'..'f'])) Then
            CurCharType := ttNumber
          Else
            CurCharType := ttIdentifier;
        End
      Else If IsInSet(ch, strNumbers) Then
        CurCharType := ttNumber
      Else
        CurCharType := ttUnknown;

      If (BlockType = btNoBlock) Then
        Begin
          // Check for full block comments
          If (LastChar = '/') And (Ch = '*') Then
            BlockType := btFullComment;
          // Check for line comments
          If (LastChar = '/') And (Ch = '/') Then
            BlockType := btLineComment;
          If (LastChar = '<') Then
            BlockType := btRule;
          If (LastChar = '?') And (LastCharType <> ttCustomUserToken) Then
            BlockType := btTextRule;
        End;

      If (LastCharType <> CurCharType) Or (IsInSet(Ch, strSingleSymbols)) Or
        (IsInSet(LastChar, strSingleSymbols)) Or
        ((BlockType In [btNoBlock]) And (CurCharType = ttLineEnd) And (Ch = #13)) Then
        Begin
          If Not (((BlockType In [btLineComment, btSingleLiteral, btDoubleLiteral,
            btRule, btTextRule]) And (CurCharType <> ttLineEnd)) Or
            (BlockType In [btFullComment, btCompoundSymbol])) Or
            ((BlockType In [btNoBlock]) And (CurCharType = ttLineEnd) And (Ch = #13)) Then
            Begin
              SetLength(strToken, iTokenLen);
              If iTokenLen > 0 Then
                If Not (IsInSet(strToken[1], strWhiteSpace)) Then
                  Begin
                    If BlockType = btLineComment Then
                      LastCharType := ttLineComment;
                    If IsInSet(strToken[1], strLineEnd) Then
                      strToken := StringReplace(strToken, #13#10, '<line-end>',
                        [rfReplaceAll]);
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
            End Else
            Begin
              Inc(iTokenLen);
              If iTokenLen > Length(strToken) Then
                SetLength(strToken, iTokenCapacity + Length(strToken));
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
      // Check for single string literals
      If CurCharType = ttSingleLiteral Then
        If BlockType = btSingleLiteral Then
          BlockType := btNoBlock
        Else If BlockType = btNoBlock Then
          BlockType := btSingleLiteral;
      // Check for Double string literals
      If CurCharType = ttDoubleLiteral Then
        If BlockType = btDoubleLiteral Then
          BlockType := btNoBlock
        Else If BlockType = btNoBlock Then
          BlockType := btDoubleLiteral;
      If (BlockType = btTextRule) And (Ch = '?') Then
        Begin
          BlockType := btNoBlock;
          CurCharType := ttCustomUserToken;
        End;
      If BlockType = btCompoundSymbol Then
        BlockType := btNoBlock;

      Inc(iColumn);
      If Ch = #10 Then
        Begin
          Inc(iLine);
          iColumn := 1;
          If BlockType In [btLineComment, btSingleLiteral, btDoubleLiteral] Then
            BlockType := btNoBlock;
        End;
      LastChar := Ch;
    End;
    If iTokenLen > 0 Then
      Begin
        SetLength(strToken, iTokenLen);
        If Not (IsInSet(strToken[1], strWhiteSpace)) Then
          Begin
            If IsInSet(strToken[1], strLineEnd) Then
              strToken := StringReplace(strToken, #13#10, '<line-end>', [rfReplaceAll]);
            AddToken(TTokenInfo.Create(strToken, iStreamPos,
              iTokenLine, iTokenColumn, Length(strToken), CurCharType));
          End;
      End;
  AddToken(TTokenInfo.Create('<end-of-file>', iStreamPos, iTokenLine, iTokenColumn, 0,
    ttFileEnd));
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
function TBackusNaurModule.ReservedWords: TKeyWords;

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
function TBackusNaurModule.Directives: TKeyWords;

begin
  Result := Nil;
end;

(**

  This method checks to see if the token is a line end token, if so moves to
  the next token else raises and error and seeks the end of the line.

  @precon  None.
  @postcon Checks to see if the token is a line end token, if so moves to
           the next token else raises and error and seeks the end of the line.

**)
procedure TBackusNaurModule.Terminator;
begin
  If Not FUseSemiColon Then
    LineEnd
  Else
    SemiColon;
end;

(**

  This method processes line end tokens IF the code is not using semi-colons.

  @precon  None.
  @postcon Processes line end tokens IF the code is not using semi-colons.

**)
procedure TBackusNaurModule.LineEnd;
begin
  If Token.TokenType In [ttLineEnd] Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken(strExpectedLineEnd, Token.Token, strSeekableOnErrorTokens, stActual, Self);
end;

(**

  This method processes the list of terms in the grammar.

  @precon  R must be a valid instance of a TBackusNaurRule class.
  @postcon Processes the list of terms in the grammar.

  @param   R as a TBackusNaurRule

**)
procedure TBackusNaurModule.List(R : TBackusNaurRule);

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
        ErrorAndSeekToken(strLiteralExpected, ')', strSeekableOnErrorTokens, stActual, Self);
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
        ErrorAndSeekToken(strLiteralExpected, ']', strSeekableOnErrorTokens, stActual, Self);
      RepeatOperator(R);
      List(R);
    End Else
    Begin
      SimpleExpression(R);
      If StartToken <> Token Then
        List(R);
    End;
end;

(**

  This method parses literal tokens and adds them to the rule if found.

  @precon  R must be a valid instance of a TBNDRule class.
  @postcon Parses literal tokens and adds them to the rule if found.

  @param   R as a TBackusNaurRule

**)
procedure TBackusNaurModule.Literal(R: TBackusNaurRule);
begin
  If CharRef(R) Then
    Begin
      If Token.Token = '..' Then
        Begin
          AddToExpression(R);
          CharRef(R);
        End;
    End Else
      If Token.TokenType In [ttCustomUserToken, ttSingleLiteral, ttDoubleLiteral] Then
        If Token.Token[1] = Token.Token[Length(Token.Token)] Then
          AddToExpression(R)
        Else
          ErrorAndSeekToken(strMissingTerminalChar, Token.Token, strSeekableOnErrorTokens, stActual,
            Self);
end;

(**

  This method checks the token to see if its a single character literal and if
  so adds it the rule and returns true.

  @precon  R must be a valid instance of a TBNDRule class.
  @postcon Checks the token to see if its a single character literal and if
           so adds it the rule and returns true.

  @param   R as a TBackusNaurRule
  @return  a Boolean

**)
Function TBackusNaurModule.LiteralChar(R: TBackusNaurRule) : Boolean;

begin
  Result := (Token.TokenType In [ttSingleLiteral, ttDoubleLiteral]) And
    (Length(Token.Token) = 3);
  If Result Then
    AddToExpression(R);
end;

(**

  This method tries to get a document comment from the previous token and return a TComment class to the 
  calling routine.

  @precon  None.
  @postcon Returns the comment immediately before the current token else nil.

  @note    All comments found are automatically added to the comment collection for disposal when the 
           parser is destroyed.

  @param   CommentPosition as a TCommentPosition as a constant
  @return  a TComment

**)
Function TBackusNaurModule.GetComment(Const CommentPosition : TCommentPosition) : TComment;

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
End;

(**

  This method returns a string representing the name of the module.
  @precon  None.
  @postcon Returns a string representing the name of the module.

  @return  a String

**)
function TBackusNaurModule.GetModuleName: String;
begin
  Result := ExtractFilename(FileName);
end;

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

  This method process the modules comment looking for options to use
  case-insensitive rules and an options semi=colon for a rule terminator.

  @precon  None.
  @postcon Process the modules comment looking for options to use
           case-insensitive rules and an options semi=colon for a rule
           terminator.

**)
procedure TBackusNaurModule.ProcessTags;

var
  iTag: Integer;

begin
  If Comment <> Nil Then
    Begin
      FRequiredRules.CaseSensitive := Comment.FindTag('caseinsensitive') = -1;
      FUseSemiColon := Comment.FindTag('usesemicolon') > -1;
      iTag := Comment.FindTag('goal');
      If iTag > -1 Then
        FMainGoal := Comment.Tag[itag].AsString(999999, False);
    End;
end;

(**

  This method returns false and does not reference any tokens.

  @precon  None.
  @postcon Does nothing.

  @param   AToken as a TTokenInfo as a constant
  @return  a Boolean

**)
Function TBackusNaurModule.ReferenceSymbol(Const AToken : TTokenInfo) : Boolean;

Begin
  Result := False;
End;

(**

  This method checks to see if the token is either one of the repeat tokens and
  adds it to the rule if so.

  @precon  R must be a valid instance of a TBackusNaurRule class.
  @postcon Checks to see if the token is either one of the repeat tokens and
           adds it to the rule if so.

  @param   R as a TBackusNaurRule

**)
procedure TBackusNaurModule.RepeatOperator(R: TBackusNaurRule);
begin
  If (Token.Token = '*') Or (Token.Token = '+') Then
    AddToExpression(R);
end;

(**

  This method processes the Terms of the grammar.

  @precon  R must be a valid instance of a TBackusNaurRule class.
  @postcon Processes the Terms of the grammar.

  @param   R as a TBackusNaurRule

**)
procedure TBackusNaurModule.Term(R : TBackusNaurRule);
var
  iCode: Integer;

begin
  If FUseSemiColon Then
    EatLineEnds;
  If Token.TokenType In [ttIdentifier] Then
    Begin
      If Like('<*>', Token.Token) Then
        Begin
          iCode := Token.Line Or (Token.Column Shl 16);
          FRequiredRules.AddObject(Token.Token, TObject(iCode));
          If Token.Token = '<>' Then
            AddIssue(Format(strNULLIdentifierFound, [Token.Token,
                Token.Line, Token.Column]), scNone, Token.Line, Token.Column, etError, Self);
          AddToExpression(R);
          RepeatOperator(R);
        End
      Else
        ErrorAndSeekToken(strRulesShouldStartAndEndWith, '', strSeekableOnErrorTokens, stActual, Self);
    End Else
      Literal(R);
end;

(**

  This method deletes any root elements which don`t and items in them.

  @precon  None.
  @postcon Deletes any root elements which don`t and items in them.

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
  R : TBackusNaurRule;

begin
  If Token.TokenType In [ttIdentifier] Then
    Begin
      If Like('<*>', Token.Token) Then
        Begin
          R := TBackusNaurRule.Create(Token.Token, scPublic, Token.Line, Token.Column,
            iiPublicType, GetComment);
          If R.Identifier = '<>' Then
            AddIssue(Format(strNULLIdentifierFound, [Token.Token,
                Token.Line, Token.Column]), scNone, Token.Line, Token.Column, etError, Self);
          If FRules = Nil Then
            FRules := Add(TLabelContainer.Create('Rules', scNone, 0, 0,
              iiPublicTypesLabel, Nil)) As TLabelContainer;
          If FRules.Add(R) <> R Then
            Begin
              AddIssue(Format(strDuplicateIdentifierFound, [Token.Token,
                Token.Line, Token.Column]), scNone, Token.Line, Token.Column, etError, Self);
              R := Nil;
            End;
          NextNonCommentToken;
          If Token.Token = '::=' Then
            Begin
              NextNonCommentToken;
              Expression(R);
            End Else
              ErrorAndSeekToken(strExpectedEquality, Token.Token, strSeekableOnErrorTokens, stActual,
                Self);
          Terminator;
          If (R <> Nil) And (R.TokenCount = 0) Then
            AddIssue(Format(strTheRuleHasNoDefinition, [R.Identifier, R.Line,
              R.Column]), scNone, R.Line, R.Column, etWarning, Self);
        End Else
          ErrorAndSeekToken(strExpectedRuleButFound, Token.Token, strSeekableOnErrorTokens, stActual,
            Self);
      End Else
        ErrorAndSeekToken(strRulesShouldStartAndEndWith, Token.Token, strSeekableOnErrorTokens,
          stActual, Self);
end;

(**

  This method processes semi=colon tokens IF the code is using semi-colons.

  @precon  None.
  @postcon Processes semi-colon tokens IF the code is using semi-colons.

**)
procedure TBackusNaurModule.SemiColon;
begin
  If Token.Token = ';' Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken(strExpectedSemiColon, Token.Token, strSeekableOnErrorTokens, stActual, Self);
end;

(**

  This method parsers a simple expression looking for an exception operator.

  @precon  R must be a valid instance of a TBackusNaurRule class.
  @postcon Parsers a simple expression looking for an exception operator.

  @param   R as a TBackusNaurRule

**)
procedure TBackusNaurModule.SimpleExpression(R: TBackusNaurRule);
begin
  Term(R);
  If Token.Token = '-' Then
    Begin
      AddToExpression(R);
      Term(R);
    End;
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

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TBackusNaurModule.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;

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

var
  C: TComment;

begin
  Line := 1;
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
                C := TBackusNaurComment.CreateComment(Token.Token, Token.Line,
                  Token.Column);
                AddBodyComment(C);
                If Comment = Nil Then
                  Comment := C;
              End;
            NextToken;
          End;
        // Check for end of file else must be identifier
        If Not EndOfTokens Then
          Begin
            ProcessTags;
            Syntax;
          End;
      End;
  Except
    On E : EBADIParserAbort Do
      AddIssue(E.Message, scNone, 0, 0, etError, Self);
  End;
end;

(**

  This method checks the token to see if its a Hexidecimal Character, if so adds it
  to the rule and returns true.

  @precon  R must be a valid instance of a TBackusNaurRule class.
  @postcon Checks the token to see if its a Hexidecimal Character, if so adds it
           to the rule and returns true.

  @param   R as a TBackusNaurRule
  @return  a Boolean

**)
Function TBackusNaurModule.HexChar(R: TBackusNaurRule) : Boolean;

var
  i: Integer;
  strToken : String;
  iErrors: Integer;

begin
  strToken := Token.Token;
  Result := strToken[1] = '$';
  If Result Then
    Begin
      iErrors := 0;
      For i := 2 To Length(strToken) Do
        If Not IsInSet(strToken[i], ['0'..'9', 'A'..'F', 'a'..'f']) Then
          Inc(iErrors);
      If iErrors = 0 Then
        AddToExpression(R)
      Else
        ErrorAndSeekToken(strInvalidHexCharRef, Token.Token, strSeekableOnErrorTokens, stActual, Self);
    End;
end;

End.
