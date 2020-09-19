(**

  This module contains code to parser VB/VBA code (and perhaps will be extended
  to parser VB.NET code later).

  @Author     David Hoyle
  @Version    1.001
  @Date    19 Sep 2020

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

  @nometrics

**)
Unit BADI.VB.ModuleFull;

Interface

Uses
  SysUtils,
  Classes,
  Contnrs,
  BADI.Base.Module,
  BADI.Types,
  BADI.ElementContainer,
  BADI.Comment,
  BADI.Generic.MethodDecl,
  BADI.VB.VariableDecl,
  BADI.VB.Version,
  BADI.Generic.FunctionDecl,
  BADI.VB.Interfaces,
  BADI.TokenInfo,
  BADI.VB.Types;

{$INCLUDE CompilerDefinitions.Inc}

Type
  (** A set type to define a set of Browse and Doc It Token Types **)
  TBADITokenTypes = Set Of TBADITokenType;

  (**

    This is the main class for dealing with object pascal units and program
    source files. It creates and manages instances of the other classes as
    needed.

  **)
  TVBModule = Class(TBaseLanguageModule)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FSource: String;
    FModuleType : TVBModuleType;
    FUnResolvedSymbols : TStringList;
    FEventHandlerPatterns: TStringList;
    Procedure TokenizeStream;
    { Grammer Parsers }
    Procedure Goal;
    Function  Version : Boolean;
    Function  VBBegin(C : TElementContainer) : Boolean;
    Function  Attributes : Boolean;
    Function  Attribute(C : TElementContainer) : Boolean;
    Function  Options : Boolean;
    Function  Implements : Boolean;
    Function Declarations : Boolean;
    Procedure InterfaceSection;
    Procedure ImplementationSection;
    Function  Consts(AScope : TScope; C : TComment) : Boolean;
    Function  Dims(AScope : TScope; C : TComment) : Boolean;
    Function  Subs(AScope : TScope; C : TComment; boolStatic : Boolean;
      DeclareLabel : TLabelContainer) : Boolean;
    Function  Functions(AScope : TScope; C : TComment; boolStatic : Boolean;
      DeclareLabel : TLabelContainer) : Boolean;
    Function  Declares(AScope : TScope; C: TComment) : Boolean;
    Function  Props(AScope : TScope; C : TComment; boolStatic : Boolean) : Boolean;
    Function  Records(AScope : TScope; C : TComment) : Boolean;
    Function  Enum(AScope : TScope; C: TComment) : Boolean;
    Procedure Parameters(Container : TElementContainer);
    Procedure MethodDecl(M : TGenericMethodDecl; C : TComment);
    Function Vars(AScope : TScope; C : TComment) : Boolean;
    Function VarDecl(AScope : TScope; C : TComment) : Boolean;
    Procedure ArraySizeDecl(Variable: TVBVar);
    Function FloatingPointLiteral(V : TVBVersion) : Boolean;
    Procedure Block;
    Function Statements : Boolean;
    Function Statement : Boolean;
    Function LabelDeclarationStatement : Boolean;
    Function LocalDeclarationStatement : Boolean;
    Function WithStatement : Boolean;
    Function EventStatement : Boolean;
    Function AssignmentStatement : Boolean;
    Function InvocationStatement : Boolean;
    Function ConditionalStatement : Boolean;
    Function LoopStatement : Boolean;
    Function ErrorHandlingStatement : Boolean;
    Function BranchStatement : Boolean;
    Function ArrayHandlingStatement : Boolean;


    Function QualifiedIdentifier(Container : TElementContainer;
      TokenTypes : TBADITokenTypes) : String;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Procedure ProcessCompilerDirective(var iSkip : Integer); Override;
    procedure TidyUpEmptyElements;
    Function GetComment(Const CommentPosition : TCommentPosition = cpBeforeCurrentToken) : TComment;
      Override;
    Procedure NextNonCommentToken; Override;
    Procedure CheckExceptionHandling;
    Procedure ResolvedForwardReferences;
    Procedure CheckForInterfaceScope(var AScope : TScope);
    Procedure CheckForImplementationScope(var AScope : TScope);
    procedure CheckMethodEnd(strEndKeyWord : String);
    procedure ProcessVar(Variable: TVBVar);
    procedure CheckElementExceptionHandling(M: TGenericFunction;
      ExceptionHandler : IExceptionHandling);
    procedure PatchAndCheckReferences;
  Public
    Constructor CreateParser(Const Source, strFileName : String; Const IsModified : Boolean;
      Const ModuleOptions : TModuleOptions); Override;
    Destructor Destroy; Override;
    Function ReservedWords : TKeyWords; Override;
    Function Directives : TKeyWords; Override;
    Function ReferenceSymbol(Const AToken : TTokenInfo) : Boolean; Override;
    { Properties }
  End;

Implementation

Uses
  Windows,
  BADI.Options,
  BADI.VB.Comment,
  BADI.ResourceStrings,
  BADI.VB.Constants,
  BADI.Functions,
  BADI.VB.ResourceStrings,
  BADI.Constants,
  BADI.VB.ImplementedItem,
  BADI.VB.Attribute,
  BADI.VB.Option,
  BADI.VB.Parameter,
  BADI.VB.TypeDecl,
  BADI.VB.MethodDecl,
  BADI.VB.PropertyDecl,
  BADI.VB.ConstantDecl,
  BADI.VB.RecordDecl,
  BADI.VB.FieldDecl,
  BADI.VB.EnumerateDecl,
  BADI.VB.EnumIdent;

(**

  This is the constructor method for the TVBDocModule class.

  @precon  Source is a valid TStream descendant containing as stream of text
           that is the contents of a source code module, Filename is the file
           name of the module being parsed and IsModified determines if the
           source code module has been modified since the last save to disk.
  @postcon Initialise the class and parses the text stream.

  @param   Source        as a String
  @param   strFileName   as a String
  @param   IsModified    as a Boolean
  @param   ModuleOptions as a TModuleOptions

**)
Constructor TVBModule.CreateParser(Const Source, strFileName : String; Const IsModified : Boolean;
      Const ModuleOptions : TModuleOptions);

var
  boolCascade: Boolean;

Begin
  Inherited CreateParser(Source, strFileName, IsModified, ModuleOptions);
  FUnResolvedSymbols := TStringList.Create;
  FUnResolvedSymbols.Duplicates := dupIgnore;
  FUnResolvedSymbols.Sorted := True;
  FEventHandlerPatterns := TStringList.Create;
  FEventHandlerPatterns.Add('*_*');
  CompilerDefines.Assign(BADIOptions.Defines);
  FSource := Source;
  AddTickCount('Start');
  CommentClass := TVBComment;
  If CompareText(ExtractFileExt(FileName), '.cls') = 0 Then
    FModuleType := mtClass
  Else If CompareText(ExtractFileExt(FileName), '.frm') = 0 Then
    FModuleType := mtForm
  Else
    FModuleType := mtModule;
  ModuleName := ChangeFileExt(ExtractFileName(strFileName), '');
  TokenizeStream;
  AddTickCount('Tokenize');
  If moParse In ModuleOptions Then
    Begin
      Goal;
      AddTickCount('Parse');
      ResolvedForwardReferences;
      AddTickCount('Resolve');
      Add(strErrors, iiErrorFolder, scNone, 0);
      Add(strWarnings, iiWarningFolder, scNone);
      Add(strHints, iiHintFolder, scNone);
      Add(strDocumentationConflicts, iiDocConflictFolder, scNone);
      PatchAndCheckReferences;
      AddTickCount('Refs');
      If doShowMissingVBExceptionWarnings In BADIOptions.Options Then
        CheckExceptionHandling;
      boolCascade := True;
      If moCheckForDocumentConflicts In ModuleOptions Then
        CheckDocumentation(boolCascade);
      AddTickCount('Check');
      TidyUpEmptyElements;
    End;
End;

(**

  This is the destructor method for the TVBDocModule class.

  @precon  None.
  @postcon Frees memory used by internal objects.

**)
Destructor TVBModule.Destroy;

Begin
  FEventHandlerPatterns.Free;
  FUnResolvedSymbols.Free;
  Inherited Destroy;
End;

(**

  This method remove the Implement Methods and Exported Headings IF they have
  no elements.

  @precon  None.
  @postcon Remove the Implement Methods and Exported Headings IF they have
           no elements.

**)
procedure TVBModule.TidyUpEmptyElements;

Var
  iElement : Integer;

begin
  For iElement := ElementCount DownTo 1 Do
    If Elements[iElement].ElementCount = 0 Then
      DeleteElement(iElement);
end;

(**

  This method returns an array of key words for use in the explorer module.

  @precon  None.
  @postcon Returns an array of key words for use in the explorer module.

  @return  a TKeyWords

**)
function TVBModule.ReservedWords: TKeyWords;

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
function TVBModule.Directives: TKeyWords;

Var
  i : Integer;

begin
  SetLength(Result, Succ(High(strDirectives)));
  For i := Low(strDirectives) To High(strDirectives) Do
    Result[i] := strDirectives[i];
end;

(**

  This method is an overridden method to processes compiler directives.

  @precon  None.
  @postcon Not implemented.

  @param   iSkip as an Integer as a reference

**)
procedure TVBModule.ProcessCompilerDirective(var iSkip: Integer);
begin
  {Do nothing}
end;

(**

  This method tokenises the stream of text passed to the constructor and splits
  it into visual basic tokens.

  @precon  None.
  @postcon Tokenises the stream of text passed to the constructor and splits
           it into visual basic tokens.

**)
Procedure TVBModule.TokenizeStream;

Type
  (** State machine for block types. **)
  TBlockType = (btNoBlock, btStringLiteral, btLineComment, btBraceComment);

Const
  (** Growth size of the token buffer. **)
  iTokenCapacity = 25;

Var
  (** Token buffer. **)
  strToken : String;
  CurCharType : TBADITokenType;
  LastToken : TBADITokenType;
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
  (** Token size **)
  iTokenLen : Integer;
  iChar: Integer;

Begin
  BlockType := btNoBlock;
  iStreamPos := 0;
  iTokenLine := 1;
  iTokenColumn := 1;
  CurCharType := ttUnknown;
  //: @debug LastToken := ttUnknown;
  iStreamCount := 0;
  iLine := 1;
  iColumn := 1;
  strToken := '';

  iTokenLen := 0;
  SetLength(strToken, iTokenCapacity);

  For iChar := 1 To Length(FSource) Do
    Begin
      ch := FSource[iChar];
      Inc(iStreamCount);
      LastToken := CurCharType;

      If IsInSet(ch, strWhiteSpace) Then
        CurCharType := ttWhiteSpace
      Else If IsInSet(ch, strTokenChars) Then
        Begin
          If (LastToken = ttNumber) And (IsInSet(Ch, ['A'..'F', 'H', 'a'..'f', 'h'])) Then
            CurCharType := ttNumber
          Else
            If (LastToken In [ttWhiteSpace]) And (IsInSet(Ch, ['_'])) Then
              CurCharType := ttLineContinuation
            Else
              Begin
                If LastToken In [ttLineContinuation] Then
                  LastToken := ttIdentifier;
                CurCharType := ttIdentifier
              End;
        End
      Else If IsInSet(ch, strNumbers) Then
        Begin
          CurCharType := ttNumber;
          If LastToken = ttIdentifier Then
            CurCharType := ttIdentifier;
        End
      Else If IsInSet(ch, strLineEnd) Then
        CurCharType := ttLineEnd
      Else If IsInSet(ch, strQuote) Then
        CurCharType := ttDoubleLiteral
      Else If IsInSet(ch, strSymbols) Then
        Begin
          CurCharType := ttSymbol;
          If (Ch = '.') And (LastToken = ttNumber) Then
            CurCharType := ttNumber;
          If (LastToken In [ttIdentifier]) And (IsInSet(Ch, ['%', '$', '&'])) Then
            CurCharType := ttIdentifier;
        End
      Else
        CurCharType := ttUnknown;

      If (LastToken <> CurCharType) Or (CurCharType = ttSymbol) Then
        Begin
          If ((BlockType In [btStringLiteral, btLineComment]) And
            (CurCharType <> ttLineEnd)) Or (BlockType In [btBraceComment]) Then
            Begin
              Inc(iTokenLen);
              If iTokenLen > Length(strToken) Then
                SetLength(strToken, iTokenCapacity + Length(strToken));
              strToken[iTokenLen] := Ch;
            End Else
            Begin
              SetLength(strToken, iTokenLen);
              If iTokenLen > 0 Then
                If Not (IsInSet(strToken[1], strWhiteSpace)) Then
                  Begin
                    If LastToken = ttIdentifier Then
                      Begin
                        If IsKeyWord(strToken, strReservedWords) Then
                          LastToken := ttReservedWord;
                        If IsKeyWord(strToken, strDirectives) Then
                          LastToken := ttDirective;
                      End;
                    If BlockType = btLineComment Then
                      LastToken := ttLineComment;
                    If (LastToken = ttLineComment) And (Length(strToken) > 2) Then
                      If (strToken[1] = '{') And (strToken[2] = '$') Then
                        LastToken := ttCompilerDirective;
                    If IsInSet(strToken[1], strLineEnd) Then
                      strToken := StringReplace(strToken, #13#10, '<line-end>',
                        [rfReplaceAll]);
                    AddToken(TTokenInfo.Create(strToken, iStreamPos,
                      iTokenLine, iTokenColumn, Length(strToken), LastToken));
                    //Inc(iCounter);
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

      // Check for line comments
      If (BlockType = btNoBlock) And (Ch = '''') Then
        BlockType := btLineComment;

      // Check for string literals
      If CurCharType = ttDoubleLiteral Then
        If BlockType = btStringLiteral Then
          BlockType := btNoBlock
        Else If BlockType = btNoBlock Then
          BlockType := btStringLiteral;

        // Check for block Comments
        If (BlockType = btNoBlock) And (Ch = '{') Then
          Begin
            CurCharType := ttLineComment;
            BlockType := btBraceComment;
          End;
        If (BlockType = btBraceComment) And (Ch = '}') Then
          Begin
            CurCharType := ttLineComment;
            BlockType := btNoBlock;
          End;
//              If BlockType = btCompoundSymbol Then
//                BlockType := btNoBlock;

      Inc(iColumn);
      If Ch = #10 Then
        Begin
          Inc(iLine);
          iColumn := 1;
          If BlockType In [btLineComment, btStringLiteral] Then
            BlockType := btNoBlock;
        End;
    End;
  If iTokenLen > 0 Then
    Begin
      SetLength(strToken, iTokenLen);
      If iTokenLen > 0 Then
        If Not (IsInSet(strToken[1], strWhiteSpace)) Then
          Begin
            If CurCharType = ttIdentifier Then
              Begin
                If IsKeyWord(strToken, strReservedWords) Then
                  CurCharType := ttReservedWord;
                If IsKeyWord(strToken, strDirectives) Then
                  CurCharType := ttDirective;
              End;
            If IsInSet(strToken[1], strLineEnd) Then
              strToken := StringReplace(strToken, #13#10, '<line-end>',
                [rfReplaceAll]);
            AddToken(TTokenInfo.Create(strToken, iStreamPos,
              iTokenLine, iTokenColumn, Length(strToken), CurCharType));
          End;
    End;
  AddToken(TTokenInfo.Create('<end-of-file>', iStreamPos, iTokenLine, iTokenColumn, 0,
    ttFileEnd));
End;

(**

  This method returns the comment before the current token.

  @precon  None.
  @postcon Returns the comment before the current token.

  @param   CommentPosition as a TCommentPosition
  @return  a TComment

**)
Function TVBModule.GetComment(Const CommentPosition : TCommentPosition = cpBeforeCurrentToken) : TComment;

Var
  T : TTokenInfo;
  strComment : String;
  iToken : Integer;
  iLine, iColumn : Integer;
  iOffset: Integer;
  iLastCmtLine : Integer;

begin
  Result := Nil;
  iLine := 0;
  iColumn := 0;
  iLastCmtLine := Token.Line;
  If CommentPosition = cpBeforeCurrentToken Then
    iOffset := -1
  Else
    iOffset := -2;
  iToken := TokenIndex + iOffset;
  While iToken > -1 Do
    Begin
      While (iToken > -1) And ((Tokens[iToken] As TTokenInfo).TokenType In
        [ttLineEnd, ttLineContinuation]) Do
        Dec(iToken);
      If iToken <= -1 Then
        Break;
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
    End;
  If strComment <> '' Then
    Begin
      Result := TVBComment.CreateComment(strComment, iLine, iColumn);
      OwnedItems.Add(Result);
    End;
end;

(**

  This method starts the process of parsing the visual basic delcarations.

  @precon  None.
  @postcon Parses the code in the stream of tokens.

**)
procedure TVBModule.Goal;

var
  iMethod : Integer;
  Methods : Array[1..5] Of Function : Boolean Of Object;

begin
  Try
    While Token.TokenType In [ttLineComment, ttBlockComment, ttLineEnd] Do
      NextNonCommentToken;
    Methods[1] := Version;
    Methods[2] := Attributes;
    Methods[3] := Options;
    Methods[4] := Implements;
    Methods[5] := Declarations;
    For iMethod := Low(Methods) To High(Methods) Do
      Begin
        If EndOfTokens Then
          Break;
        If Methods[iMethod] Then
          While Not EndOfTokens And (Token.TokenType In [ttLineEnd]) Do
            NextNonCommentToken;
      End;
  Except
    On E : EBADIParserAbort Do
      AddIssue(E.Message, scNone, 0, 0, etError, Self);
  End;
end;

(**

  This mnethod parses the ImplementationSection declarations in the module.

  @precon  None.
  @postcon Parses the ImplementationSection declarations in the module.

**)
Procedure TVBModule.ImplementationSection;

Var
  C : TComment;
  boolResult : Boolean;
  AScope : TScope;

begin
  Repeat
    C := GetComment;
    CheckForImplementationScope(AScope);
    boolResult :=
      Subs(AScope, C, False, Nil) Or
      Functions(AScope, C, False, Nil) Or
      Props(AScope, C, False);
    If Not boolResult Then
      If Not EndOfTokens Then
        If Token.TokenType In [ttLineEnd] Then
          NextNonCommentToken
        Else
          ErrorAndSeekToken(strUnDefinedToken, Token.Token, strSeekTokens, stActual, Self);
  Until EndOfTokens;
end;

(**

  This method parses any Implements delcarations in the module.

  @precon  None.
  @postcon Returns true if an implements delcaration was parsed.

  @return  a Boolean

**)
Function TVBModule.Implements : Boolean;

Var
  ImplementsLabel : TLabelContainer;
  strQID : String;
  I: TImplementedItem;
  QStartToken: TTokenInfo;

begin
  Result := False;
  While Token.UToken = 'IMPLEMENTS' Do
    Begin
      Result := True;
      ImplementsLabel := FindElement(strImplementsLabel) As TLabelContainer;
      If ImplementsLabel = Nil Then
        ImplementsLabel := Add(TLabelContainer.Create(strImplementsLabel, scNone,
          0, 0, iiInterfacesLabel, Nil)) As TLabelContainer;
      NextNonCommentToken;
      I := Nil;
      QStartToken := Token;
      Repeat
        If Token.TokenType In [ttIdentifier] Then
          Begin
            strQID := QualifiedIdentifier(Nil, [ttIdentifier]);
            I := TImplementedItem.Create(strQID, scNone, QStartToken.Line,
              QStartToken.Column, iiPublicInterface, Nil);
            ImplementsLabel.Add(I);
          End Else
            ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekTokens, stActual, Self);
      Until Not IsToken(',', I);
      If Token.TokenType In [ttLineEnd] Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strLineEndExpected, Token.Token, strSeekTokens, stActual, Self);
    End;
end;

(**

  This method parses the InterfaceSection declarations of the grammar.

  @precon  None.
  @postcon Parses the InterfaceSection declarations of the grammar.

**)
Procedure TVBModule.InterfaceSection;

Var
  C : TComment;
  boolResult: Boolean;
  AScope : TScope;

begin
  Repeat
    C := GetComment;
    CheckForInterfaceScope(AScope);
    boolResult :=
      Consts(AScope, C) Or
      Declares(AScope, C) Or
      Dims(AScope, C) Or
      Records(AScope, C) Or
      Enum(AScope, C) Or
      Vars(AScope, C);
  Until Not boolResult;
  If PrevToken <> Nil Then
    If IsKeyWord(PrevToken.Token, ['friend', 'private', 'public']) Then
      RollBackToken;
end;

function TVBModule.InvocationStatement: Boolean;
begin
  Result := False;
end;

function TVBModule.LabelDeclarationStatement: Boolean;
begin
  Result := False;
end;

function TVBModule.LocalDeclarationStatement: Boolean;
begin
  Result := False;
end;

function TVBModule.LoopStatement: Boolean;
begin
  Result := False;
end;

(**

  This method processes a version declarations that appears at the top of
  module, classes, etc, which are output to disk.

  @precon  None.
  @postcon Processes a version declarations that appears at the top of module,
           classes, etc, which are output to disk.

  @return  a Boolean

**)
Function TVBModule.Version : Boolean;

Var
  L : TLabelContainer;
  V : TVBVersion;

begin
  Result := False;
  If IsKeyWord(Token.Token, ['version']) Then
    Begin
      Result := True;
      If Comment = Nil Then
        Comment := GetComment;
      L := Add(TLabelContainer.Create(strVersionLabel, scNone, 0, 0,
        iiUsesLabel, Nil)) As TLabelContainer;
      V := L.Add(TVBVersion.Create(Token.Token, scNone, Token.Line,
        Token.Column, iiUsesLabel, Nil)) As TVBVersion;
      NextNonCommentToken;
      If FloatingPointLiteral(V) Then
        Begin
          If Token.UToken = 'CLASS' Then
            AddToExpression(V);
          If Token.TokenType In [ttLineEnd] Then
            Begin
              NextNonCommentToken;
              VBBegin(V);
            End Else
              ErrorAndSeekToken(strLineEndExpected, Token.Token, strSeekTokens, stActual, Self);
        End Else
          ErrorAndSeekToken(strNumberExpected, Token.Token, strSeekTokens, stActual, Self);
    End;
end;

function TVBModule.WithStatement: Boolean;
begin
  Result := False;
end;

(**

  This method processes variable declarations.

  @precon  None.
  @postcon Processes variable declarations.

  @param   AScope as a TScope
  @param   C      as a TComment
  @return  a Boolean

**)
function TVBModule.Vars(AScope: TScope; C: TComment): Boolean;

begin
  Result := False;
  Repeat
    Result := VarDecl(AScope, C) Or Result;
  Until Not IsToken(',', Nil);
  If Result Then
    If Token.TokenType In [ttLineEnd] Then
      NextNonCommentToken
    Else
      ErrorAndSeekToken(strLineEndExpected, Token.Token, strSeekTokens, stActual, Self);
end;

(**

  This method parses the VarDecl element of the grammar.

  @precon  None.
  @postcon Parses the VarDecl element of the grammar.

  @param   AScope as a TScope
  @param   C      as a TComment
  @return  a Boolean

**)
function TVBModule.VarDecl(AScope: TScope; C: TComment): Boolean;

Var
  boolWithEvents : Boolean;
  V : TVBVar;
  VarsLabel: TLabelContainer;

Begin
  Result := False;
  boolWithevents := Token.UToken = 'WITHEVENTS';
  If boolWithEvents Then
    NextNonCommentToken;
  If Token.TokenType In [ttIdentifier] Then
    Begin
      Result := True;
      V := TVBVar.Create(Token.Token, AScope, Token.Line, Token.Column,
        iiPublicVariable, C);
      VarsLabel := FindElement(strVarsLabel) As TLabelContainer;
      If VarsLabel = Nil Then
        VarsLabel := Add(TLabelContainer.Create(strVarsLabel, scNone, 0, 0,
          iiPublicVariablesLabel, Nil)) As TLabelContainer;
      V := VarsLabel.Add(V) As TVBVar;
      V.Referenced := True;
      V.Comment := C;
      V.WithEvents := boolWithEvents;
      NextNonCommentToken;
      ProcessVar(V);
    End;
End;

(**

  This method processes the BEGIN / END section of a module version clause.

  @precon  None .
  @postcon Processes the BEGIN / END section of a module version clause .

  @param   C as a TElementContainer
  @return  a Boolean

**)
Function TVBModule.VBBegin(C : TElementContainer) : Boolean;

Var
  Container : TElementContainer;
  strModifier: String;
  strQID: String;
  QStartToken: TTokenInfo;

Begin
  Result := False;
  Container := C;
  If IsKeyWord(Token.Token, ['begin', 'beginproperty']) Then
    Begin
      Result := True;
      strModifier := LowerCase(Token.Token);
      Delete(strModifier, 1, 5);
      NextNonCommentToken;
      If strModifier = '' Then
        Begin
          If Token.TokenType In [ttIdentifier, ttDirective] Then
            Begin
              QStartToken := Token;
              strQID := QualifiedIdentifier(Nil, [ttIdentifier, ttDirective]);
              Container := TVBAttribute.Create(strQID, scNone, QStartToken.Line,
                QStartToken.Column, iiUsesItem, Nil);
              Container := C.Add(Container);
              If Token.TokenType In [ttIdentifier, ttDirective] Then
                Begin
                  Container.AddToken(Token.Token);
                  NextNonCommentToken;
                End;
            End;
        End Else
        Begin
          If Token.TokenType In [ttIdentifier, ttDirective] Then
            Begin
              strQID := Token.Token;
              NextNonCommentToken;
              Container := TVBAttribute.Create(strQID, scNone, Token.Line,
                Token.Column, iiUsesItem, Nil);
              Container := C.Add(Container);
            End Else
              ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekTokens, stActual, Self);
        End;
      If Token.TokenType In [ttLineEnd] Then
        Begin
          NextNonCommentToken;
          Repeat
            // Do nothing
          Until Not (
            VBBegin(Container) Or
            Attribute(Container)
          );
          If IsKeyWord(Token.Token, ['end' + strModifier]) Then
            Begin
              NextNonCommentToken;
              If Token.TokenType In [ttLineEnd] Then
                NextNonCommentToken
              Else
                ErrorAndSeekToken(strLineEndExpected, Token.Token, strSeekTokens, stActual, Self);
            End
          Else
            ErrorAndSeekToken(strReservedWordExpected, 'END', strSeekTokens, stActual, Self);
        End Else
          ErrorAndSeekToken(strReservedWordExpected, 'BEGIN', strSeekTokens, stActual, Self);
    End;
End;

(**

  This method processes a list of attributes declared at the header of a visual
  basic module.

  @precon  None.
  @postcon Processes a list of attributes declared at the header of a visual
           basic module.

  @return  a Boolean

**)
Function TVBModule.Attributes : Boolean;
var
  AttrsLabel: TLabelContainer;

Begin
  Result := False;
  While IsKeyWord(Token.Token, ['attribute']) Do
    Begin
      Result := True;
      If Comment = Nil Then
        Comment := GetComment;
      AttrsLabel := FindElement(strAttributesLabel) As TLabelContainer;
      If AttrsLabel = Nil Then
        AttrsLabel := Add(TLabelContainer.Create(strAttributesLabel,
          scNone, 0, 0, iiUsesLabel, Nil)) As TLabelContainer;
      NextNonCommentToken;
      Attribute(AttrsLabel);
    End;
end;

procedure TVBModule.Block;

begin
  While Statements Do;
end;

function TVBModule.BranchStatement: Boolean;
begin
  Result := False;
end;

(**

  This method parses a single attribute at the top of the module.

  @precon  None .
  @postcon Parses a single attribute at the top of the module .

  @param   C as a TElementContainer
  @return  a Boolean

**)
function TVBModule.AssignmentStatement: Boolean;
begin
  Result := False;
end;

Function TVBModule.Attribute(C : TElementContainer) : Boolean;

Var
  A : TVBAttribute;
  strQID : String;
  QStartToken: TTokenInfo;

begin
  Result := False;
  If (Token.TokenType In [ttIdentifier, ttDirective]) And
    (CompareText(Token.Token, 'endproperty') <> 0) Then
    Begin
      Result := True;
      QStartToken := Token;
      strQID := QualifiedIdentifier(Nil, [ttIdentifier, ttDirective]);
      A := C.Add(TVBAttribute.Create(strQID, scNone, QStartToken.Line,
        QStartToken.Column, iiUsesItem, Nil)) As TVBAttribute;
      If Token.Token = '=' Then
        Begin
          A.AddToken(Token.Token);
          NextNonCommentToken;
          If Token.Token = '-' Then
            AddToExpression(A);
          If Token.TokenType In [ttNumber, ttIdentifier, ttDirective,
            ttDoubleLiteral] Then
            Begin
              AddToExpression(A);
              If Token.Token = ':' Then
                Begin
                  AddToExpression(A);
                  If Token.TokenType In [ttNumber] Then
                    Begin
                      AddToExpression(A);
                      If Token.TokenType In [ttLineEnd] Then
                        NextNonCommentToken
                      Else
                        ErrorAndSeekToken(strLineEndExpected, Token.Token, strSeekTokens, stActual, Self);
                    End Else
                      ErrorAndSeekToken(strNumberExpected, Token.Token, strSeekTokens, stActual, Self);
                End Else
              If Token.TokenType In [ttLineEnd] Then
                NextNonCommentToken
              Else
                ErrorAndSeekToken(strLineEndExpected, Token.Token, strSeekTokens, stActual, Self);
            End Else
              ErrorAndSeekToken(strValueExpected, Token.Token, strSeekTokens, stActual, Self);
        End Else
          ErrorAndSeekToken(strLiteralExpected, '=', strSeekTokens, stActual, Self);
    End;
end;

(**

  This method parsers the options statements at the top of the modules.

  @precon  None.
  @postcon Parsers the options statements at the top of the modules.

  @return  a Boolean

**)
Function TVBModule.Options : Boolean;

Const
  strBases : Array[0..1] Of String = ('0', '1');
  strCompares : Array[0..2] Of String = ('binary', 'database', 'text');

Var
  O : TVBOption;
  OptionsLabel: TLabelContainer;

Begin
  Result := False;
  While Token.UToken = 'OPTION' Do
    Begin
      Result := True;
      If Comment = Nil Then
        Comment := GetComment;
      OptionsLabel := FindElement(strOptionsLabel) As TLabelContainer;
      If OptionsLabel = Nil Then
        OptionsLabel := Add(TLabelContainer.Create(strOptionsLabel, scNone,
          0, 0, iiUsesLabel, Nil)) As TLabelContainer;
      NextNonCommentToken;
      If Token.UToken = 'BASE' Then
        Begin
          O := OptionsLabel.Add(TVBOption.Create(Token.Token, scNone,
            Token.Line, Token.Column, iiUsesItem, Nil)) As TVBOption;
          NextNonCommentToken;
          If IsKeyWord(Token.Token, strBases) Then
            Begin
              AddToExpression(O);
              If Token.TokenType In [ttLineEnd] Then
                NextNonCommentToken
              Else
                ErrorAndSeekToken(strLineEndExpected, Token.Token, strSeekTokens, stActual, Self);
            End Else
              ErrorAndSeekToken(strReservedWordExpected, '0 or 1', strSeekTokens, stActual, Self);
        End Else
      If Token.UToken = 'COMPARE' Then
        Begin
          O := OptionsLabel.Add(TVBOption.Create(Token.Token, scNone,
            Token.Line, Token.Column, iiUsesItem, Nil)) As TVBOption;
          NextNonCommentToken;
          If IsKeyWord(Token.Token, strCompares) Then
            Begin
              AddToExpression(O);
              If Token.TokenType In [ttLineEnd] Then
                NextNonCommentToken
              Else
                ErrorAndSeekToken(strLineEndExpected, Token.Token, strSeekTokens, stActual, Self);
            End Else
              ErrorAndSeekToken(strReservedWordExpected, 'BINARY, DATABASE or TEXT', strSeekTokens,
                stActual, Self);
        End Else
      If Token.UToken = 'PRIVATE' Then
        Begin
          O := OptionsLabel.Add(TVBOption.Create(Token.Token, scNone,
            Token.Line, Token.Column, iiUsesItem, Nil)) As TVBOption;
          NextNonCommentToken;
          If Token.UToken = 'MODULE' Then
            Begin
              AddToExpression(O);
              If Token.TokenType In [ttLineEnd] Then
                NextNonCommentToken
              Else
                ErrorAndSeekToken(strLineEndExpected, Token.Token, strSeekTokens, stActual, Self);
            End Else
              ErrorAndSeekToken(strReservedWordExpected, 'MODULE', strSeekTokens, stActual, Self);
        End Else
      If Token.UToken = 'EXPLICIT' Then
        Begin
          OptionsLabel.Add(TVBOption.Create(Token.Token, scNone,
            Token.Line, Token.Column, iiUsesItem, Nil));
          NextNonCommentToken;
          If Token.TokenType In [ttLineEnd] Then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strLineEndExpected, Token.Token, strSeekTokens, stActual, Self);
        End Else
          ErrorAndSeekToken(strReservedWordExpected, 'BASE, COMPARE, EXPLICIT or PRIVATE',
            strSeekTokens, stActual, Self);
    End;
End;

(**

  This method dispatches to sub functions to have the keywords found.

  @precon  None.
  @postcon Dispatches to sub functions to have the keywords found.

  @return  a Boolean

**)
Function TVBModule.Declarations : Boolean;

Begin
  Result := False;
  InterfaceSection;
  ImplementationSection;
End;

(**

  This method parses parameters for method and properties.

  @precon  Method must be a valid instance of a method .
  @postcon Parses parameters for method and properties .

  @param   Container as a TElementContainer

**)
Procedure TVBModule.Parameters(Container : TElementContainer);

Var
  boolOptional : Boolean;
  P : TVBParameter;
  PM : TParamModifier;
  boolParamArray : Boolean;
  boolArray : Boolean;
  Ident : TTokenInfo;
  ParamType : TVBTypeDecl;
  DefaultValue: String;

Begin
  Repeat
    boolOptional := Token.UToken = 'OPTIONAL';
    If boolOptional Then
      NextNonCommentToken;
    PM := pamNone;
    If Token.UToken = 'BYVAL' Then
      Begin
        PM := pamNone;
        NextNonCommentToken;
      End;
    If Token.UToken = 'BYREF' Then
      Begin
        PM := pamVar;
        NextNonCommentToken;
      End;
    boolParamArray := Token.UToken = 'PARAMARRAY';
    If boolParamArray Then
      NextNonCommentToken;
    Ident := Token;
    NextNonCommentToken;
    boolArray := False;
    If Token.Token = '(' Then
      Begin
        boolArray := True;
        NextNonCommentToken;
        If Token.Token <> ')' Then
          ErrorAndSeekToken(strLiteralExpected, ')', strSeekTokens, stActual, Self);
        NextNonCommentToken;
      End;
    ParamType := Nil;
    Try
      If Token.UToken = 'AS' Then
        Begin
          NextNonCommentToken;
          ParamType := TVBTypeDecl.Create('', scNone, Token.Line, Token.Column,
            iiNone, Nil);
          ParamType.AppendToken(Token);
          NextNonCommentToken;
          If Token.Token = '.' Then
            Begin
              ParamType.AddToken('.');
              NextNonCommentToken;
              ParamType.AddToken(Token.Token);
              NextNonCommentToken;
            End;
        End Else
        Begin
          ParamType := TVBTypeDecl.Create('', scNone, Token.Line, Token.Column,
            iiNone, Nil);
          ParamType.AddToken('Variant');
        End;
      DefaultValue := '';
      If (Token.Token = '=') And boolOptional Then
        Begin
          If Token.Token = '=' Then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strLiteralExpected, '=', strSeekTokens, stActual, Self);
          While (Token.Token <> ',') And (Token.Token <> ')') Do
            Begin
              DefaultValue := DefaultValue + Token.Token;
              NextNonCommentToken;
            End;
        End;
      P := TVBParameter.Create(pm, Ident.Token, boolArray, ParamType,
        DefaultValue, scLocal, Token.Line, Token.Column);
      If Container Is TVBMethod Then
        (Container AS TVBMethod).AddParameter(P)
      Else
        (Container AS TVBProperty).AddParameter(P);
      P.Optional := boolOptional;
      P.ParamArray := boolParamArray;
    Finally
      ParamType.Free;
    End;
  Until Not IsToken(',', Nil);
End;

Function TVBModule.Statement: Boolean;

Begin
  Result :=
    LabelDeclarationStatement Or
    LocalDeclarationStatement Or
    WithStatement Or
    EventStatement Or
    AssignmentStatement Or
    InvocationStatement Or
    ConditionalStatement Or
    LoopStatement Or
    ErrorHandlingStatement Or
    BranchStatement Or
    ArrayHandlingStatement;
End;

function TVBModule.Statements: Boolean;
begin
  Result := Statement;
  {: @debug If Not Result Then
    Begin
      Result := Statements;
      If Token.Token = ':' Then
        Begin
          NextNonCommentToken;
          If Not Statements Then
            ErrorAndSeekToken(strStatementExpected, 'Statements', Token.Token,
              strSeekTokens, stActual, Self);
        End Else
          ErrorAndSeekToken(strLiteralExpected, 'Statements', Token.Token,
            strSeekTokens, stActual, Self);
    End;}
end;

(**

  This method defers parsing of subroutines to the MethodDecl method.

  @precon  None.
  @postcon Defers parsing of subroutines to the MethodDecl method.

  @param   AScope as a TScope
  @param   C            as a TComment
  @param   boolStatic   as a Boolean
  @param   DeclareLabel as a TLabelContainer
  @return  a Boolean

**)
Function TVBModule.Subs(AScope : TScope; C : TComment; boolStatic : Boolean;
  DeclareLabel : TLabelContainer) : Boolean;

Var
  M : TVBMethod;
  MethodsLabel: TLabelContainer;

Begin
  Result := False;
  If Token.UToken = 'SUB' Then
    Begin
      Result := True;
      NextNonCommentToken;
      M := TVBMethod.Create(mtProcedure, Token.Token, AScope, Token.Line,
        Token.column);
      If DeclareLabel = Nil Then
        Begin
          MethodsLabel := FindElement(strImplementedMethodsLabel) As TLabelContainer;
          If MethodsLabel = Nil Then
            MethodsLabel := Add(TLabelContainer.Create(
              strImplementedMethodsLabel, scNone, 0, 0, iiImplementedMethods, Nil)
              ) As TLabelContainer;
          M := MethodsLabel.Add(M) As TVBMethod;
        End Else
          M := DeclareLabel.Add(M) As TVBMethod;
      MethodDecl(M, C);
      If M.Ext = '' Then
        Begin
          Block;
          CheckMethodEnd('SUB');
        End;
    End;
End;

(**

  This method defers parsing of functions to the MethodDecl method.

  @precon  None.
  @postcon Defers parsing of function to the MethodDecl method.

  @param   AScope as a TScope
  @param   C            as a TComment
  @param   boolStatic   as a Boolean
  @param   DeclareLabel as a TLabelContainer
  @return  a Boolean

**)
Function TVBModule.Functions(AScope : TScope; C : TComment; boolStatic : Boolean;
  DeclareLabel : TLabelContainer) : Boolean;

Var
  M : TVBMethod;
  MethodsLabel: TLabelContainer;

Begin
  Result := False;
  If Token.UToken = 'FUNCTION' Then
    Begin
      Result := True;
      NextNonCommentToken;
      M := TVBMethod.Create(mtFunction, Token.Token, AScope, Token.Line,
        Token.column);
      If DeclareLabel = Nil Then
        Begin
          MethodsLabel := FindElement(strImplementedMethodsLabel) As TLabelContainer;
          If MethodsLabel = Nil Then
            MethodsLabel := Add(TLabelContainer.Create(
              strImplementedMethodsLabel, scNone, 0, 0, iiImplementedMethods, Nil)
              ) As TLabelContainer;
          M := MethodsLabel.Add(M) As TVBMethod;
        End Else
          M := DeclareLabel.Add(M) As TVBMethod;
      MethodDecl(M, C);
      If M.Ext = '' Then
        Begin
          Block;
          CheckMethodEnd('FUNCTION');
        End;
    End;
End;

(**

  This method parses the sub and function declarations.

  @precon  M must be a valid method declaration.
  @postcon Parses the sub and function declarations.

  @param   M as a TGenericMethodDecl
  @param   C as a TComment

**)
Procedure TVBModule.MethodDecl(M : TGenericMethodDecl; C : TComment);

Var
  T : TVBTypeDecl;

Begin
  M.Comment := C;
  M.Identifier := Token.Token;
  M.ClassNames.Add(ModuleName);
  NextNonCommentToken;
  If Token.UToken = 'LIB' Then
    Begin
      NextNonCommentToken;
      M.Ext := Token.Token;
      NextNonCommentToken
    End;
  If Token.UToken = 'ALIAS' Then
    Begin
      NextNonCommentToken;
      M.Alias := Token.Token;
      NextNonCommentToken
    End;
  If Token.Token = '(' Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken(strLiteralExpected, '(', strSeekTokens, stActual, Self);
  If Token.Token <> ')' Then
    Parameters(M);
  If Token.Token = ')' Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken(strLiteralExpected, ')', strSeekTokens, stActual, Self);
  If Token.UToken = 'AS' Then
    Begin
      NextNonCommentToken;
      T := TVBTypeDecl.Create(Token.Token, scNone, 0, 0, iiNone, Nil);
      M.ReturnType.Add(T);
      T.AppendToken(Token);
      If Token.TokenType In [ttIdentifier, ttReservedWord] Then
        Begin
          NextNonCommentToken;
          If Token.Token = '.' Then
            Begin
              T.AddToken('.');
              NextNonCommentToken;
              T.AppendToken(Token);
              If Not EndOfTokens Then
                NextNonCommentToken
              Else
                Exit;
            End;
          If Token.Token = '(' Then
            Begin
              NextNonCommentToken;
              If Token.Token = ')' Then
                NextNonCommentToken
              Else
                ErrorAndSeekToken(strLiteralExpected, Token.Token, strSeekTokens, stActual, Self);
            End;
        End Else
          ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekTokens, stActual, Self);
    End;
  If Token.TokenType In [ttLineEnd] Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken(strLineEndExpected, Token.Token, strSeekTokens, stActual, Self);
End;


(**

  This method overrides the default NextNonCommentToken to skip line-ends if
  preceeded by a line continuation token.

  @precon  None.
  @postcon Overrides the default NextNonCommentToken to skip line-ends if
           preceeded by a line continuation token.

**)
procedure TVBModule.NextNonCommentToken;

begin
  Inherited NextNonCommentToken;
  If Token.TokenType In [ttLineContinuation] Then
    Begin
      NextToken;
      If Token.TokenType In [ttLineEnd] Then
        NextToken
      Else
        ErrorAndSeekToken(strLineEndExpected, Token.Token, strSeekTokens, stActual, Self);
    End;
end;

(**

  This method checks for a gloating point literal in the code and returns true
  if found at the current token and adds the token to the expression.

  @precon  V must be a valid instance.
  @postcon Checks for a gloating point literal in the code and returns true
           if found at the current token and adds the token to the expression.

  @param   V as a TVBVersion
  @return  a Boolean

**)
function TVBModule.FloatingPointLiteral(V: TVBVersion): Boolean;

begin
  Result := False;
  If Token.TokenType In [ttNumber] Then
    Begin
      Result := True;
      AddToExpression(V);
    End;
end;

(**

  This method checks the methods and properties for exception handling code
  (TException.Push / Pop etc).

  @precon  None.
  @postcon Checks the methods and properties for exception handling code.

**)
procedure TVBModule.CheckExceptionHandling;

Var
  I : TElementContainer;
  j : Integer;

begin
  I := FindElement(strImplementedMethodsLabel);
  If i <> Nil Then
    Begin
      For j := 1 To I.ElementCount Do
        CheckElementExceptionHandling(I.Elements[j] As TVBMethod,
          (I.Elements[j] As TVBMethod).ExceptionHandling);
    End;
  I := FindElement(strImplementedPropertiesLabel);
  If i <> Nil Then
    Begin
      For j := 1 To I.ElementCount Do
        CheckElementExceptionHandling(I.Elements[j] As TVBProperty,
          (I.Elements[j] As TVBProperty).ExceptionHandling);
    End;
end;

procedure TVBModule.CheckForImplementationScope(var AScope: TScope);

begin
  AScope := scPublic;
  If IsKeyWord(Token.Token, ['friend', 'private', 'public']) Then
    Begin
      If CompareText(Token.Token, 'private') = 0 Then
        AScope := scPrivate
      Else If CompareText(Token.Token, 'friend') = 0 Then
        AScope := scFriend;
      NextNonCommentToken;
    End;
end;

(**

  This method checks for a scope changes using the scope keywords.

  @precon  None.
  @postcon If a scope change is found it changes the AScope variable to make the
           scope change and moves to the next token.

  @param   AScope as a TScope as a reference

**)
procedure TVBModule.CheckForInterfaceScope(var AScope: TScope);

begin
  AScope := scPublic;
  If IsKeyWord(Token.Token, ['private', 'public']) Then
    Begin
      If CompareText(Token.Token, 'private') = 0 Then
        AScope := scPrivate;
      NextNonCommentToken;
    End;
end;

(**

  This method parses visual basic constants.

  @precon  None.
  @postcon Parses visual basic constants.

  @param   AScope as a TScope
  @param   C      as a TComment
  @return  a Boolean

**)
function TVBModule.ConditionalStatement: Boolean;
begin
  Result := False;
end;

Function TVBModule.Consts(AScope : TScope; C : TComment) : Boolean;

Var
  Con : TVBConstant;
  ConstantsLabel: TLabelContainer;

Begin
  Result := False;
  If Token.UToken = 'CONST' Then
    Begin
      Result := True;
      NextNonCommentToken;
      If Token.TokenType In [ttIdentifier] Then
        Begin
          Con := TVBConstant.Create(Token.Token, AScope, Token.Line, Token.Column,
            iiPublicConstant, C);
          ConstantsLabel := FindElement(strConstantsLabel) As TLabelContainer;
          If ConstantsLabel = Nil Then
            ConstantsLabel := Add(TLabelContainer.Create(strConstantsLabel, scNone, 0,
              0, iiPublicConstantsLabel, Nil)) As TLabelContainer;
          Con := ConstantsLabel.Add(Con) As TVBConstant;
          Con.Referenced := True;
          NextNonCommentToken;
          Con.Comment := C;
          If Token.UToken = 'AS' Then
            Begin
              AddToExpression(Con);
              QualifiedIdentifier(Con, [ttIdentifier, ttReservedWord, ttDirective]);
            End;
          If Token.Token = '=' Then
            Begin
              AddToExpression(Con);
              While Not (Token.TokenType In [ttReservedWord, ttLineEnd, ttFileEnd]) Do
                AddToExpression(Con);
            End Else
              ErrorAndSeekToken(strLiteralExpected, '=', strSeekTokens, stActual, Self);
          If Token.TokenType In [ttLineEnd] then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strLineEndExpected, Token.Token, strSeekTokens, stActual, Self);
        End Else
          ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekTokens, stActual, Self);
    End;
End;

(**

  This method processes DIM statements.

  @precon  None.
  @postcon Processes DIM statements.

  @param   AScope as a TScope
  @param   C     as a TComment
  @return  a Boolean

**)
Function TVBModule.Dims(AScope : TScope; C : TComment) : Boolean;

Begin
  Result := False;
  PushTokenPosition;
  If IsKeyWord(Token.Token, ['dim']) Then
    Begin
      NextNonCommentToken;
      Result := Vars(AScope, C);
      If Not Result Then
        PopTokenPosition;
    End;
End;

(**

  This method processes the declare statement.

  @precon  None.
  @postcon Processes the declare statement.

  @param   AScope as a TScope
  @param   C     as a TComment
  @return  a Boolean

**)
Function TVBModule.Declares(AScope : TScope; C: TComment) : Boolean;

Var
  R : Boolean;
  DeclaresLabel: TLabelContainer;

begin
  Result := False;
  If Token.UToken = 'DECLARE' Then
    Begin
      Result := True;
      NextNonCommentToken;
      DeclaresLabel := FindElement(strDeclaresLabel) As TLabelContainer;
      If DeclaresLabel = Nil Then
        DeclaresLabel := Add(TLabelContainer.Create(strDeclaresLabel,
          scNone, 0, 0, iiExportedHeadingsLabel, Nil)) As TLabelContainer;
      R := Subs(AScope, C, False, DeclaresLabel);
      If Not R Then
        R := Functions(AScope, C, False, DeclaresLabel);
      If Not R Then
        ErrorAndSeekToken(strReservedWordExpected, Token.Token, strSeekTokens, stActual, Self);
    End;
end;

(**

  This method processes the properties statements.

  @precon  None.
  @postcon Processes the properties statements.

  @param   AScope as a TScope
  @param   C          as a TComment
  @param   boolStatic as a Boolean
  @return  a Boolean

**)
Function TVBModule.Props(AScope : TScope; C : TComment; boolStatic : Boolean) : Boolean;

Var
  pt : TVBPropertyType;
  P : TVBProperty;
  PropertiesLabel: TLabelContainer;
  T : TVBTypeDecl;

Begin
  Result := False;
  If Token.UToken = 'PROPERTY' Then
    Begin
      Result := True;
      NextNonCommentToken;
      pt := ptUnknown;
      If Token.UToken = 'GET' Then
        pt := ptGet
      Else If Token.UToken = 'SET' Then
        pt := ptSet
      Else If Token.UToken = 'LET' Then
        pt := ptLet
      Else
        ErrorAndSeekToken(strReservedWordExpected, 'GET, SET, or LET', strSeekTokens, stActual, Self);
      If pt In [ptGet, ptLet, ptSet] Then
        Begin
          NextNonCommentToken;
          P := TVBProperty.Create(pt, Token.Token, AScope, Token.Line,
            Token.Column, iiPublicProperty, C);
          PropertiesLabel := FindElement(strImplementedPropertiesLabel) As TLabelContainer;
          If PropertiesLabel = Nil Then
            PropertiesLabel := Add(TLabelContainer.Create(strImplementedPropertiesLabel,
              scNone, 0, 0, iiPropertiesLabel, Nil)) As TLabelContainer;
          P := PropertiesLabel.Add(P) As TVBProperty;
          NextNonCommentToken;
          If Token.Token = '(' Then
            Begin
              NextNonCommentToken;
              If Token.Token <> ')' Then
                Parameters(P);
              If Token.Token = ')' Then
                NextNonCommentToken
              Else
                ErrorAndSeekToken(strLiteralExpected, ')', strSeekTokens, stActual, Self);
              If Token.UToken = 'AS' Then
                Begin
                  NextNonCommentToken;
                  If Token.TokenType In [ttIdentifier, ttReservedWord] Then
                    Begin
                      T := TVBTypeDecl.Create(Token.Token, scNone, 0, 0, iiNone, Nil);
                      P.ReturnType.Add(T);
                      AddToExpression(T);
                    End;
                End;
              If Token.TokenType In [ttLineEnd] Then
                NextNonCommentToken
              Else
                ErrorAndSeekToken(strLineEndExpected, Token.Token, strSeekTokens, stActual, Self);
              Block;
              CheckMethodEnd('PROPERTY');
            End Else
              ErrorAndSeekToken(strLiteralExpected, '(', strSeekTokens, stActual, Self);
        End;
  End;
End;

(**

  This method processes Type/Record declarations.

  @precon  None.
  @postcon Processes Type/Record declarations.

  @param   AScope as a TScope
  @param   C     as a TComment
  @return  a Boolean

**)
Function TVBModule.Records(AScope : TScope; C : TComment) : Boolean;

Var
  R : TVBRecordDecl;
  F : TVBField;
  T: TTokenInfo;
  Com: TComment;
  TypesLabel: TLabelContainer;

Begin
  Result := False;
  If Token.UToken = 'TYPE' Then
    Begin
      Result := True;
      NextNonCommentToken;
      If Token.TokenType In [ttIdentifier] Then
        Begin
          R := TVBRecordDecl.Create(Token.Token, AScope, Token.Line, Token.Column,
            iiPublicRecord, C);
          TypesLabel := FindElement(strTypesLabel) As TLabelContainer;
          If TypesLabel = Nil Then
            TypesLabel := Add(TLabelContainer.Create(strTypesLabel, scNone, 0, 0,
              iiPublicTypesLabel, Nil)) As TLabelContainer;
          R := TypesLabel.Add(R) As TVBRecordDecl;
          R.Comment := C;
          NextNonCommentToken;
          If Token.TokenType In [ttLineEnd] then
            Begin
              NextNonCommentToken;
              Repeat
                Begin
                  If Token.TokenType In [ttLineEnd] Then
                    NextNonCommentToken;
                  If Token.TokenType In [ttReservedWord, ttIdentifier] Then
                    Begin
                      T := Token;
                      Com := GetComment;
                      PushTokenPosition;
                      NextNonCommentToken;
                      If (PrevToken.UToken = 'END') And (Token.UToken = 'TYPE')  Then
                        Begin
                          PopTokenPosition;
                          Break;
                        End;
                      F := TVBField.Create(T.Token, scPublic, T.Line, T.Column,
                        iiPublicField, Com);
                      F := R.Add(F) As TVBField;
                      ProcessVar(F);
                      If Token.TokenType In [ttLineEnd] Then
                        NextNonCommentToken
                      Else
                        ErrorAndSeekToken(strLineEndExpected, Token.Token, strSeekTokens, stActual, Self);
                      {
                      If Token.UToken = 'AS' Then
                        Begin
                          NextNonCommentToken;
                          If Token.TokenType In [ttIdentifier, ttReservedWord] Then
                            Begin
                              AddToExpression(F);
                              If Token.TokenType In [ttLineEnd] then
                                NextNonCommentToken
                              Else
                                ErrorAndSeekToken(strLineEndExpected, 'Records',
                                  Token.Token, strSeekTokens, stActual, Self);
                            End Else
                              ErrorAndSeekToken(strIdentExpected, 'Records',
                                Token.Token, strSeekTokens, stActual, Self);
                        End Else
                          ErrorAndSeekToken(strLiteralExpected, 'Records',
                            'AS', strSeekTokens, stActual, Self);
                      }
                    End Else
                      ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekTokens, stActual, Self);
                End;
              Until False;
            End Else
              ErrorAndSeekToken(strLineEndExpected, Token.Token, strSeekTokens, stActual, Self);
        End Else
          ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekTokens, stActual, Self);
      NextNonCommentToken;
      If Token.UToken = 'TYPE' Then
        Begin
          NextNonCommentToken;
          If Token.TokenType In [ttLineEnd] then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strLineEndExpected, Token.Token, strSeekTokens, stActual, Self);
        End Else
          ErrorAndSeekToken(strReservedWordExpected, 'TYPE', strSeekTokens, stActual, Self);
    End;
End;

(**

  This method refernces symbols found in the code with respect to the variables, constants and types.

  @precon  None.
  @postcon Refernces symbols found in the code with respect to the variables, constants and types.

  @param   AToken as a TTokenInfo as a constant
  @return  a Boolean

**)
Function TVBModule.ReferenceSymbol(Const AToken : TTokenInfo) : Boolean;

  (**

    This function checks the given element for an occurrance of the token.

    @precon  None.
    @postcon Marks the element item as referenced is the token is found.

    @param   Section as a TElementContainer
    @return  a Boolean

  **)
  Function CheckElement(Section : TElementContainer) : Boolean;

  Var
    boolFound: Boolean;
    i: Integer;

  Begin
    // Check Module Local Methods, Properties, Declares, ...
    boolFound := False;
    If Section <> Nil Then
      For i := 1 To Section.ElementCount Do
        If CompareText(Section[i].Identifier, AToken.Token) = 0 Then
          Begin
            Section[i].Referenced := True;
            AToken.Reference := trResolved;
            boolFound := True;
          End;
    Result := boolFound;
    If Result Then
      Exit;
  End;

begin
  Result := ReferenceSection(AToken, FindElement(strVarsLabel) As TLabelContainer);
  If Result Then
    Exit;
  Result := ReferenceSection(AToken, FindElement(strConstantsLabel) As TLabelContainer);
  If Result Then
    Exit;
  Result := ReferenceSection(AToken, FindElement(strTypesLabel) As TLabelContainer);
  If Result Then
    Exit;
  Result := CheckElement(FindElement(strImplementedMethodsLabel) As TLabelContainer);
  If Result Then
    Exit;
  Result := CheckElement(FindElement(strImplementedPropertiesLabel) As TLabelContainer);
  If Result Then
    Exit;
  Result := CheckElement(FindElement(strDeclaresLabel) As TLabelContainer);
  If Result Then
    Exit;
end;

Procedure TVBModule.CheckMethodEnd(strEndKeyWord : String);

Begin
  If Token.UToken = 'END' Then
    Begin
      NextNonCommentToken;
      If Token.UToken = strEndKeyWord Then
        Begin
          NextNonCommentToken;
          If Token.TokenType In [ttLineEnd] Then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strLineEndExpected, Token.Token, strSeekTokens, stActual, Self);
        End
      Else
        ErrorAndSeekToken(strReservedWordExpected, strEndKeyWord, strSeekTokens, stActual, Self);
    End
  Else
    ErrorAndSeekToken(strReservedWordExpected, 'END', strSeekTokens, stActual, Self);
End;

(**

  This method checks the references and removes event handler from being shown.

  @precon  None.
  @postcon Checks the references and removes event handler from being shown.

**)
procedure TVBModule.PatchAndCheckReferences;

  (**

    This method returns true if the identifier contains an underscore.

    @precon  None.
    @postcon Returns true if the identifier contains an underscore.

    @param   strIdentifier as a String
    @return  a Boolean

  **)
  Function IsEventHandler(strIdentifier : String) : Boolean;

  Var
    i: Integer;

  Begin
    Result := False;
    For i := 0 To FEventHandlerPatterns.Count - 1 Do
      Begin
        Result := Result Or Like(FEventHandlerPatterns[i], strIdentifier);
        If Result Then
          Break;
      End;
  End;

Var
  j: Integer;
  I: TElementContainer;

begin
  If FindElement(strErrors).ElementCount = 0 Then
    Begin
      I := FindElement(strImplementedMethodsLabel);
      If I <> Nil Then
        Begin
          For j := 1 To I.ElementCount Do
            If Not I[j].Referenced Then
              I[j].Referenced := IsEventHandler(I[j].Identifier);
        End;
      CheckReferences;
    End;
end;

(**

  This method checks the exception handling for the given function and it`s
  exception handler.

  @precon  M and ExceptionHandler must be valid instances.
  @postcon Checks the exception handling for the given function and it`s
           exception handler and outputs an issue IF something is missing.

  @param   M                as a TGenericFunction
  @param   ExceptionHandler as an IExceptionHandling

**)
procedure TVBModule.CheckElementExceptionHandling(M: TGenericFunction;
  ExceptionHandler : IExceptionHandling);

var
  boolNoTag: Boolean;
  i: Integer;
  iIndex : Integer;

begin
  // Check Exception Push and Pop
  boolNoTag :=
    (Comment <> Nil) And (Comment.FindTag('noexception') > -1) Or
    (M.Comment <> Nil) And (M.Comment.FindTag('noexception') > -1);
  If Not ExceptionHandler.HasPush And Not boolNoTag Then
    AddIssue(Format(strExceptionPush, [M.Identifier]), scNone,
     M.Line, M.Column, etWarning, Self);
  If (ExceptionHandler.PushName = '') And Not boolNoTag Then
    AddIssue(Format(strExceptionPushName, [M.Identifier]), scNone,
      M.Line, M.Column, etWarning, Self);
  If Not boolNoTag Then
    If CompareText(Format('"%s.%s"', [ModuleName, M.Identifier]),
      ExceptionHandler.PushName) <> 0 Then
      AddIssue(Format(strExceptionPushNameIncorrect,
        [ExceptionHandler.PushName, ModuleName, M.Identifier]), scNone,
        M.Line, M.Column, etWarning, Self);
  If Not ExceptionHandler.HasPop And Not boolNoTag Then
    AddIssue(Format(strExceptionPop, [M.Identifier]), scNone,
      M.Line, M.Column, etWarning, Self);
  // Check Exception Parameters
  boolNoTag :=
    (Comment <> Nil) And (Comment.FindTag('noexception') > -1) Or
    (M.Comment <> Nil) And (M.Comment.FindTag('noexception') > -1) Or
    (M.Comment <> Nil) And (M.Comment.FindTag('noexceptionparams') > -1);
  If Not boolNoTag Then
    For i := 0 To M.ParameterCount - 1 Do
      Begin
        iIndex := ExceptionHandler.PushParams.IndexOf(M.Parameters[i].Identifier);
        If iIndex = -1 Then
          AddIssue(Format(strExceptionPushParameter, [M.Parameters[i].Identifier,
            ModuleName, M.Identifier]), scNone, M.Line, M.Column, etWarning, Self)
        Else if iIndex <> i Then
          AddIssue(Format(strExceptionPushParamPos, [M.Parameters[i].Identifier,
            ModuleName, M.Identifier, i, iIndex]), scNone, M.Line, M.Column, etWarning, Self);
      End;
  If Not boolNoTag Then
    If M.ParameterCount <> ExceptionHandler.PushParams.Count Then
      AddIssue(Format(strExceptionPushParamCount, [ModuleName, M.Identifier,
        M.ParameterCount, ExceptionHandler.PushParams.Count]), scNone,
        M.Line, M.Column, etWarning, Self);
  // Check Error Handling
  boolNoTag :=
    (Comment <> Nil) And (Comment.FindTag('noerror') > -1) Or
    (M.Comment <> Nil) And (M.Comment.FindTag('noerror') > -1);
  If Not ExceptionHandler.HasErrorHnd And Not boolNoTag Then
    AddIssue(Format(strErrorHandling, [M.Identifier]), scNone,
      M.Line, M.Column, etWarning, Self);
  If ExceptionHandler.HasExit And ExceptionHandler.HasErrorHnd And Not boolNoTag Then
    AddIssue(Format(strExitStatement, [M.Identifier]), scNone,
      ExceptionHandler.ExitLine,
      ExceptionHandler.ExitCol, etWarning, Self);
end;

(**

  This method tries to resolved the references for the symbols left over in
  FUnResolvedSymbols as they could be forward referenced at the time of the
  original check.

  @precon  None.
  @postcon Tries to resolved the references for the symbols left over in
           FUnResolvedSymbols as they could be forward referenced at the time
           of the original check.

**)
procedure TVBModule.ResolvedForwardReferences;

var
  i: Integer;
  T : TTokenInfo;

begin
  For i := 0 To FUnResolvedSymbols.Count - 1 Do
    Begin
      T := TTokenInfo.Create(FUnResolvedSymbols[i], 0, 0, 0, 0, ttIdentifier);
      Try
        ReferenceSymbol(T)
      Finally
        T.Free;
      End;
    End;
end;

(**

  This method processes a variable declaration on a line.

  @precon  Variable must be a value instance of a previous created variable
           descendant.
  @postcon Processes a variable declaration on a line.

  @param   Variable as a TVBVar

**)
procedure TVBModule.ProcessVar(Variable: TVBVar);

begin
  If Token.Token = '(' Then
    Begin
      NextNonCommentToken;
      If Token.Token <> ')' Then
        Begin
          ArraySizeDecl(Variable);
          If Token.Token = ')' Then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strLiteralExpected, Token.Token, strSeekTokens, stActual, Self);
        End Else
        Begin
          Variable.AddDimension('', '');
          NextNonCommentToken;
        End;
    End;
  If Token.UToken = 'AS' then
    begin
      NextNonCommentToken;
      if Token.UToken = 'NEW' then
        AddToExpression(Variable);
      QualifiedIdentifier(Variable, [ttIdentifier, ttReservedWord]);
      {
          if (PrevToken.UToken = 'STRING') and (Token.Token = '*') then
            begin
              AddToExpression(Variable);
              if Token.TokenType in [ttNumber] then
                AddToExpression(Variable)
              else
                ErrorAndSeekToken(strNumberExpected, 'ProcessVar', Token.Token,
                  strSeekTokens, stActual, Self);
            end;
      }
    end;
end;

(**

  This method parses the ArraySizeDecl element of the grammar.

  @precon  Variable must be a valid instance.
  @postcon Parses the ArraySizeDecl element of the grammar.

  @param   Variable as a TVBVar

**)
function TVBModule.ArrayHandlingStatement: Boolean;
begin
  Result := False;
end;

Procedure TVBModule.ArraySizeDecl(Variable: TVBVar);

var
  strHigh: string;
  strLow: string;

Begin
  Repeat
    If Token.TokenType In [ttIdentifier, ttNumber] Then
      Begin
        strLow := '0';
        strHigh := Token.Token;
        NextNonCommentToken;
        If Token.UToken = 'TO' Then
          Begin
            NextNonCommentToken;
            If Token.TokenType In [ttIdentifier, ttNumber] Then
              Begin
                strLow := strHigh;
                strHigh := Token.Token;
                NextNonCommentToken;
              End Else
                ErrorAndSeekToken(strNumberExpected, Token.Token, strSeekTokens, stActual, Self);
          End;
        Variable.AddDimension(strLow, strHigh);
      End Else
        ErrorAndSeekToken(strNumberExpected, Token.Token, strSeekTokens, stActual, Self);
  Until Not IsToken(',', Nil);
End;

(**

  This method processes Enumerate declarations.

  @precon  None.
  @postcon Processes Enumerate declarations.

  @param   AScope as a TScope
  @param   C     as a TComment
  @return  a Boolean

**)
Function TVBModule.Enum(AScope : TScope; C: TComment) : Boolean;

Var
  E : TVBEnumerateDecl;
  I : TVBEnumIdent;
  TypesLabel: TLabelContainer;

Begin
  Result := False;
  If Token.UToken = 'ENUM' Then
    Begin
      Result := True;
      NextNonCommentToken;
      If Token.TokenType = ttIdentifier Then
        Begin
          E := TVBEnumerateDecl.Create(Token.Token, AScope, Token.Line,
            Token.Column, iiPublicType, C);
          TypesLabel := FindElement(strTypesLabel) As TLabelContainer;
          If TypesLabel = Nil Then
            TypesLabel := Add(TLabelContainer.Create(strTypesLabel, scNone, 0, 0,
              iiPublicTypesLabel, Nil)) As TLabelContainer;
          E := TypesLabel.Add(E) As TVBEnumerateDecl;
          E.Comment := C;
          NextNonCommentToken;
          If Token.TokenType In [ttLineEnd] then
            Begin
              NextNonCommentToken;
              While Token.UToken <> 'END' Do
                Begin
                  If Token.TokenType In [ttIdentifier] Then
                    Begin
                      I := TVBEnumIdent.Create(Token.Token, scNone, Token.Line,
                        Token.Column, iiPublicField, GetComment);
                      I := E.Add(I) As TVBEnumIdent;
                      NextNonCommentToken;
                      If Token.Token = '=' Then
                        Begin
                          NextNonCommentToken;
                          If Token.TokenType In [ttNumber] Then
                            Begin
                              AddToExpression(I);
                            End Else
                              ErrorAndSeekToken(strNumberExpected, Token.Token, strSeekTokens, stActual, Self);
                        End;
                      If Token.TokenType In [ttLineEnd] then
                        NextNonCommentToken
                      Else
                        ErrorAndSeekToken(strLineEndExpected, Token.Token, strSeekTokens, stActual, Self);
                    End Else
                      ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekTokens, stActual, Self);
                End;
            End Else
              ErrorAndSeekToken(strLineEndExpected, Token.Token, strSeekTokens, stActual, Self);
        End Else
          ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekTokens, stActual, Self);
      NextNonCommentToken;
      If Token.UToken = 'ENUM' Then
        Begin
          NextNonCommentToken;
          If Token.TokenType In [ttLineEnd] then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strLineEndExpected, Token.Token, strSeekTokens, stActual, Self);
        End Else
          ErrorAndSeekToken(strReservedWordExpected, 'Enum', strSeekTokens, stActual, Self);
    End;
end;

function TVBModule.ErrorHandlingStatement: Boolean;
begin
  Result := False;
end;

function TVBModule.EventStatement: Boolean;
begin
  Result := False;
end;

(**

  This method parses a optionally Qualified identifier and adds the qualifier to
  the given container as well as returning the qualified identifier in the
  resultant string.

  @precon  None.
  @postcon Parses a optionally Qualified identifier and adds the qualifier to
           the given container as well as returning the qualified identifier in
           the resultant string.

  @param   Container  as a TElementContainer
  @param   TokenTypes as a TBADITokenTypes
  @return  a String

**)
Function TVBModule.QualifiedIdentifier(Container : TElementContainer;
  TokenTypes : TBADITokenTypes) : String;

Begin
  Result := '';
  Repeat
    If Token.TokenType In TokenTypes Then
      Begin
        If Result <> '' Then
          Result := Result + '.';
        Result := Result + Token.Token;
        AddToExpression(Container);
      End Else
        ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekTokens, stActual, Self);
  Until Not IsToken('.', Container);
End;

End.
