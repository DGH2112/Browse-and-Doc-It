(**

  Eidolon Module : A unit to parser Eidolon code. Please refer to the file
  "Eidolon Map File Grammar.bnf" for the complete grammar implemented.

  @Author  David Hoyle
  @Version 1.012
  @Date    06 May 2021

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
Unit BADI.INI.Module;

Interface

Uses
  SysUtils,
  Windows,
  Contnrs,
  Classes,
  BADI.Base.Module,
  BADI.Comment,
  BADI.ElementContainer,
  BADI.Types,
  BADI.TokenInfo;

{$INCLUDE CompilerDefinitions.inc}

Type
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
    Function  CheckLineEnd(): Boolean;
    Procedure EatWhitespace;
    {$IFDEF D2005} Strict {$ENDIF} Protected
    Function  GetComment(Const CommentPosition: TCommentPosition = cpBeforeCurrentToken) : TComment;
      Override;
    Procedure TidyUpEmptyElements;
    Function  GetModuleName: String; Override;
    Function  BuildSection(Const strSectionName : String; Const iLine,
      iColumn : Integer) : TLabelContainer;
  Public
    Constructor CreateParser(Const Source, strFileName: String; Const IsModified: Boolean;
      Const ModuleOptions: TModuleOptions); Override;
    Destructor Destroy; Override;
    Function  ReservedWords: TKeyWords; Override;
    Function  Directives: TKeyWords; Override;
    Procedure ProcessCompilerDirective; Override;
    Function  ReferenceSymbol(Const AToken: TTokenInfo): Boolean; Override;
    Function  AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String;
      Override;
  End;

Implementation

Uses
  BADI.ResourceStrings,
  BADI.Functions,
  BADI.Constants,
  BADI.Options,
  BADI.Module.Dispatcher,
  BADI.INI.Comment,
  BADI.INI.KeyValuePair;

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
  (** A constant for the line feed token **)
  strLineFeed = '<LF>';
  (** A constant for the carriage return token **)
  strCarriageReturn = '<CR>';

(**

  This method returns a string representation of the module.

  @precon  None.
  @postcon Returns a string representation of the module.

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TINIModule.AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String;

Begin
  Result := ChangeFileExt(Inherited AsString(boolShowIdentifier, boolForDocumentation), '');
End;

(**

  This method creates the hierarchical sections from the section name given.

  @precon  None.
  @postcon Creates the hierarchical sections from the section name given and returns the most nested 
           level.

  @param   strSectionName as a String as a constant
  @param   iLine          as an Integer as a constant
  @param   iColumn        as an Integer as a constant
  @return  a TLabelContainer

**)
Function TINIModule.BuildSection(Const strSectionName: String; Const iLine,
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

  @return  a Boolean

**)
Function TINIModule.CheckLineEnd(): Boolean;
Begin
  Result := False;
  If Token.TokenType In [ttLineEnd] Then
    Begin
      Result := True;
      EatLineEnds;
    End
  Else
    ErrorAndSeekToken(strExpectedLineEnd, Token.Token, strSeekableOnErrorTokens, stActual, Self);
End;

(**

  This is the constructor method for the TINIModule class.

  @precon  Source is a valid TStream descendant containing as stream of text, that is the contents of a 
           source code module and Filename is the file name of the module being parsed and IsModified 
           determines if the source code module has been modified since the last save to disk.
  @postcon Creates an instance of the module parser.

  @param   Source        as a String as a constant
  @param   strFileName   as a String as a constant
  @param   IsModified    as a Boolean as a constant
  @param   ModuleOptions as a TModuleOptions as a constant

**)
Constructor TINIModule.CreateParser(Const Source, strFileName: String; Const IsModified: Boolean;
      Const ModuleOptions: TModuleOptions);

ResourceString
  strStart = 'Start';
  strSections = 'Sections';
  strTokenize = 'Tokenize';
  strParse = 'Parse';
  strRefs = 'Refs';

Begin
  Inherited CreateParser(Source, strFileName, IsModified, ModuleOptions);
  FSource        := Source;
  AddTickCount(strStart);
  CommentClass := TINIComment;
  FSections := Add(TLabelContainer.Create(strSections, scGlobal, 0, 0, iiModule,
    Nil)) As TLabelContainer;
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
      If FindElement(strErrors).ElementCount = 0 Then
        CheckReferences;
      AddTickCount(strRefs);
      TidyUpEmptyElements;
    End;
End;

(**

  This is a destructor for the TINIModule class.

  @precon  None.
  @postcon Frees the memory for this instance.


**)
Destructor TINIModule.Destroy;

Begin
  Inherited Destroy;
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

  This method eats the white space when found and puts the token at the next non
  white space token.

  @precon  None.
  @postcon Eats the white space when found and puts the token at the next non
           white space token.

**)
Procedure TINIModule.EatWhitespace;
Begin
  While Token.TokenType In [ttWhiteSpace] Do
    NextNonCommentToken;
End;

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
Function TINIModule.GetComment(Const CommentPosition: TCommentPosition = cpBeforeCurrentToken) : TComment;

Const
  iCommentOffset1 = -1;
  iCommentOffset2 = -2;

Var
  T      : TTokenInfo;
  iOffset: Integer;
  iToken : TTokenIndex;

Begin
  Result := Nil;
  If CommentPosition = cpBeforeCurrentToken Then
    iOffset := iCommentOffset1
  Else
    iOffset := iCommentOffset2;
  iToken    := TokenIndex + iOffset;
  If iToken > iCommentOffset1 Then
    Begin
      While (iToken > iCommentOffset1) And ((Tokens[iToken] As TTokenInfo).TokenType
          In [ttLineEnd, ttLineContinuation]) Do
        Dec(iToken);
      If iToken > iCommentOffset1 Then
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

  This method is the starting position for the parsing of an Eidolon module. It
  finds the first non comment token and begins the grammar checking from their
  by delegating Syntax.

  @precon  None.
  @postcon It finds the first non comment token and begins the grammar checking
           from their by delegating Syntax.

  @nocheck EmptyWhile

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
          ErrorAndSeekToken(strExpectedFileEnd, Token.Token, [strLineFeed, strCarriageReturn], stActual,
            Self);
      End;
  Except
    On E: EBADIParserAbort Do
      AddIssue(E.Message, scNone, 0, 0, etError, Self);
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
          CheckLineEnd();
        End Else
          ErrorAndSeekToken(strExpectedKey, Token.Token, [strLineFeed, strCarriageReturn], stActual,
            Self);
    End;
  EatLineEnds;
End;

(**

  This is the method that should be called to parse the previously parse tokens.

  @precon  None.
  @postcon Attempts to parse the token list and check it grammatically for
           Errors while providing declaration elements for browsing.

**)
Procedure TINIModule.ParseTokens;

Begin
  Goal;
End;

(**

  This method process conditional compiler directives.

  @precon  None.
  @postcon Does nothings as conditional compilations is not supported.

  @nocheck EmptyMethod
  @nohint iSkip

**)
Procedure TINIModule.ProcessCompilerDirective;

Begin
  // Do nothing, i.e. Conditional Compilation is NOT supported.
End;

(**

  This method does nothing as we are not referencing symbols in XML.

  @precon  None.
  @postcon Returns false always.

  @nohint  AToken

  @param   AToken as a TTokenInfo as a constant
  @return  a Boolean

**)
Function TINIModule.ReferenceSymbol(Const AToken: TTokenInfo): Boolean;

Begin
  Result := False;
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

  @nocheck EmptyWhile

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
              CheckLineEnd();
            End Else
              ErrorAndSeekToken(strLiteralExpected, Token.Token, [strLineFeed, strCarriageReturn],
                stActual, Self);
        End Else
          ErrorAndSeekToken(strLiteralExpected, Token.Token, [strLineFeed, strCarriageReturn], stActual,
            Self);
    End;
End;


(**

  This method deletes any root elements which don`t and items in them.

  @precon  None.
  @postcon Deletes any root elements which don`t and items in them.

**)
Procedure TINIModule.TidyUpEmptyElements;

ResourceString
  strDefinitions = 'Definitions';

Var
  iElement: Integer;

Begin
  For iElement := ElementCount DownTo 1 Do
    If Elements[iElement].ElementCount = 0 Then
      If Elements[iElement] Is TLabelContainer Then
        If Pos(strDefinitions, Elements[iElement].Identifier) = 0 Then
          DeleteElement(iElement);
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
  strSymbols = (strAllChars - strIdentifiers - strNumbers - strSingleQuotes - strDoubleQuotes);
  strEndFile = '<end-of-file>';

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
  (** Token stream position. Fast to increment this than read the stream position. **)
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

    This INLINE procedure changes the white space tokens for more human readable
    tokens.

    @precon  strToken must be a non-null string.
    @postcon Changes the white space tokens for more human readable
             tokens.

    @param   strToken as a String as a reference

  **)
  Procedure ProcessWhiteSpace(Var strToken: String); {$IFDEF D2005} InLine; {$ENDIF}
  Begin
    If strToken = #13 Then
      strToken := strLineFeed;
    If strToken = #10 Then
      strToken := strCarriageReturn;
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
      If (BlockType = btNoBlock) And (Ch = ';') And (iColumn = 1) Then
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
  AddToken(TTokenInfo.Create(strEndFile, iStreamPos, iTokenLine, iTokenColumn, 0,
      ttFileEnd));
End;

End.
