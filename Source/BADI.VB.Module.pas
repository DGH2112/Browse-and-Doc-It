(**

  This module contains code to parser VB/VBA code (and perhaps will be extended
  to parser VB.NET code later).

  @Author     David Hoyle
  @Version    1.0
  @Date    17 Aug 2019

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
Unit BADI.VB.Module;

Interface

Uses
  SysUtils,
  Classes,
  Contnrs,
  BADI.Base.Module,
  BADI.Comment,
  BADI.ElementContainer,
  BADI.VB.Types,
  BADI.Generic.MethodDecl,
  BADI.VB.Interfaces,
  BADI.VB.VariableDecl,
  BADI.Generic.FunctionDecl,
  BADI.TokenInfo,
  BADI.Types;

{$INCLUDE CompilerDefinitions.Inc}

Type
  (**

    This is the main class for dealing with object pascal units and program
    source files. It creates and manages instances of the other classes as
    needed.

  **)
  TVBModule = Class(TBaseLanguageModule)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FTypesLabel: TLabelContainer;
    FConstantsLabel: TLabelContainer;
    FVariablesLabel: TLabelContainer;
    FImplementedMethodsLabel: TLabelContainer;
    FDeclaredMethodsLabel: TLabelContainer;
    FImplementedPropertiesLabel: TLabelContainer;
    FAttributesLabel: TLabelContainer;
    FOptionsLabel: TLabelContainer;
    FImplementsLabel: TLabelContainer;
    FEventsLabel: TLabelContainer;
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
    Function  Declarations : Boolean;
    Function  Privates(C : TComment) : Boolean;
    Function  Publics(C : TComment) : Boolean;
    Function  Consts(Scope : TScope; C : TComment) : Boolean;
    Function  Dims(Scope : TScope; C : TComment) : Boolean;
    Function  Subs(Scope : TScope; C : TComment; boolStatic : Boolean;
      DeclareLabel : TLabelContainer) : Boolean;
    Function  Functions(Scope : TScope; C : TComment; boolStatic : Boolean;
      DeclareLabel : TLabelContainer) : Boolean;
    Function  Declares(Scope : TScope; C: TComment) : Boolean;
    Function  Friends(C: TComment) : Boolean;
    Function  Props(Scope : TScope; C : TComment; boolStatic : Boolean) : Boolean;
    Function  Records(Scope : TScope; C : TComment) : Boolean;
    Function  Enum(Scope : TScope; C: TComment) : Boolean;
    Procedure Parameters(Container : TElementContainer);
    Procedure MethodDecl(M : TGenericMethodDecl; C : TComment);
    Procedure FindMethodEnd(AExceptionHnd : IExceptionHandling; strMethodType : String);
    Function  Vars(Scope : TScope; C : TComment) : Boolean;
    Function  Events(Scope : TScope; C : TComment) : Boolean;
    Procedure ProcessVar(Variable: TVBVar);
    Procedure CheckElementExceptionHandling(M: TGenericFunction;
      ExceptionHandler: IExceptionHandling);
    Procedure PatchAndCheckReferences;
    {$IFDEF D2005} Strict {$ENDIF} Protected
    Procedure ProcessCompilerDirective(var iSkip : Integer); Override;
    procedure TidyUpEmptyElements;
    Function GetComment(Const CommentPosition : TCommentPosition = cpBeforeCurrentToken) : TComment;
      Override;
    Procedure NextNonCommentToken; Override;
    Procedure CheckExceptionHandling;
    Procedure ResolvedForwardReferences;
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
  BADI.Functions,
  BADI.Options,
  BADI.ResourceStrings,
  BADI.Constants,
  BADI.Module.Dispatcher,
  BADI.VB.Comment,
  BADI.VB.Constants,
  BADI.VB.ResourceStrings,
  BADI.VB.ImplementedItem,
  BADI.VB.Version,
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
  BADI.VB.EnumIdent,
  BADI.VB.EventDecl;

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

  This is the constructor method for the TVBDocModule class.

  @precon  Source is a valid TStream descendant containing as stream of text that is the contents of a 
           source code module, Filename is the file name of the module being parsed and IsModified 
           determines if the source code module has been modified since the last save to disk.
  @postcon Initialise the class and parses the text stream.

  @param   Source        as a String as a constant
  @param   strFileName   as a String as a constant
  @param   IsModified    as a Boolean as a constant
  @param   ModuleOptions as a TModuleOptions as a constant

**)
Constructor TVBModule.CreateParser(Const Source, strFileName : String; Const IsModified : Boolean;
      Const ModuleOptions : TModuleOptions);

var
  boolCascade: Boolean;

Begin
  Inherited CreateParser(Source, strFileName, IsModified, ModuleOptions);
  FTypesLabel                 := Nil;
  FConstantsLabel             := Nil;
  FVariablesLabel             := Nil;
  FImplementedMethodsLabel    := Nil;
  FImplementedPropertiesLabel := Nil;
  FDeclaredMethodsLabel       := Nil;
  FAttributesLabel            := Nil;
  FOptionsLabel               := Nil;
  FImplementsLabel            := Nil;
  FEventsLabel                := Nil;
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
      Add(strErrors, iiErrorFolder, scNone);
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

  @param   CommentPosition as a TCommentPosition as a constant
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

  This method parses any Implements delcarations in the module.

  @precon  None.
  @postcon Returns true if an implements delcaration was parsed.

  @return  a Boolean

**)
Function TVBModule.Implements : Boolean;
begin
  Result := False;
  While Token.UToken = 'IMPLEMENTS' Do
    Begin
      Result := True;
      If FImplementsLabel = Nil Then
        FImplementsLabel := Add(TLabelContainer.Create(strImplementsLabel, scNone,
          0, 0, iiInterfacesLabel, Nil)) As TLabelContainer;
      NextNonCommentToken;
      If Token.TokenType In [ttIdentifier] Then
        Begin
          FImplementsLabel.Add(TImplementedItem.Create(Token.Token, scNone,
            Token.Line, Token.Column, iiPublicInterface, Nil));
          NextNonCommentToken
        End Else
          ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekTokens, stActual, Self);
      If Token.TokenType In [ttLineEnd] Then
        NextNonCommentToken
      Else
        ErrorAndSeekToken(strLineEndExpected, Token.Token, strSeekTokens, stActual, Self);
    End;
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
      If Token.TokenType In [ttNumber] Then
        Begin
          AddToExpression(V);
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

(**

  This method processes variable declarations.

  @precon  None.
  @postcon Processes variable declarations.

  @param   Scope as a TScope
  @param   C     as a TComment
  @return  a Boolean

**)
function TVBModule.Vars(Scope: TScope; C: TComment): Boolean;

Var
  boolWithEvents : Boolean;
  V : TVBVar;

begin
  Result := False;
  Repeat
    boolWithevents := Token.UToken = 'WITHEVENTS';
    If boolWithEvents Then
      NextNonCommentToken;
    If Token.TokenType In [ttIdentifier] Then
      Begin
        Result := True;
        V := TVBVar.Create(Token.Token, Scope, Token.Line, Token.Column,
          iiPublicVariable, C);
        If FVariablesLabel = Nil Then
          FVariablesLabel := Add(TLabelContainer.Create(strVarsLabel, scNone, 0, 0,
            iiPublicVariablesLabel, Nil)) As TLabelContainer;
        V := FVariablesLabel.Add(V) As TVBVar;
        V.Referenced := True;
        V.Comment := C;
        V.WithEvents := boolWithEvents;
        NextNonCommentToken;
        ProcessVar(V);
      End Else
        ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekTokens, stActual, Self);
  Until Not IsToken(',', Nil);
  If Token.TokenType In [ttLineEnd] Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken(strLineEndExpected, Token.Token, strSeekTokens, stActual, Self);
end;

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

Begin
  Result := False;
  Container := C;
  If IsKeyWord(Token.Token, ['begin', 'beginproperty']) Then
    Begin
      Result := True;
      strModifier := LowerCase(Token.Token);
      Delete(strModifier, 1, 5);
      NextNonCommentToken;
      If Token.TokenType In [ttIdentifier, ttDirective] Then
        Begin
          Container := TVBAttribute.Create(Token.Token, scNone, Token.Line,
            Token.Column, iiUsesItem, Nil);
          Container := C.Add(Container);
          NextNonCommentToken;
          If Token.Token = '.' Then
            Begin
              Container.AddToken(Token.Token);
              NextNonCommentToken;
              If Token.TokenType In [ttIdentifier, ttDirective] Then
                Begin
                  Container.AddToken(Token.Token);
                  NextNonCommentToken;
                  If Token.TokenType In [ttIdentifier, ttDirective] Then
                    Begin
                      Container.AddToken(Token.Token);
                      NextNonCommentToken;
                    End Else
                      ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekTokens, stActual, Self);
                End Else
                  ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekTokens, stActual, Self);
            End;
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

Begin
  Result := False;
  While IsKeyWord(Token.Token, ['attribute']) Do
    Begin
      Result := True;
      If Comment = Nil Then
        Comment := GetComment;
      If FAttributesLabel = Nil Then
        FAttributesLabel := Add(TLabelContainer.Create(strAttributesLabel,
          scNone, 0, 0, iiUsesLabel, Nil)) As TLabelContainer;
      NextNonCommentToken;
      Attribute(FAttributesLabel);
    End;
end;

(**

  This method parses a single attribute at the top of the module.

  @precon  None .
  @postcon Parses a single attribute at the top of the module .

  @param   C as a TElementContainer
  @return  a Boolean

**)
Function TVBModule.Attribute(C : TElementContainer) : Boolean;

Var
  A : TVBAttribute;

begin
  Result := False;
  If (Token.TokenType In [ttIdentifier, ttDirective]) And
    (CompareText(Token.Token, 'endproperty') <> 0) Then
    Begin
      Result := True;
      A := C.Add(TVBAttribute.Create(Token.Token, scNone, Token.Line,
        Token.Column, iiUsesItem, Nil)) As TVBAttribute;
      NextNonCommentToken;
      If Token.Token = '.' Then
        Begin
          AddToExpression(A);
          If Token.TokenType In [ttIdentifier, ttDirective] Then
            AddToExpression(A)
          Else
            ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekTokens, stActual, Self);
        End;
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

Begin
  Result := False;
  While Token.UToken = 'OPTION' Do
    Begin
      Result := True;
      If Comment = Nil Then
        Comment := GetComment;
      If FOptionsLabel = Nil Then
        FOptionsLabel := Add(TLabelContainer.Create(strOptionsLabel, scNone,
          0, 0, iiUsesLabel, Nil)) As TLabelContainer;
      NextNonCommentToken;
      If Token.UToken = 'BASE' Then
        Begin
          O := FOptionsLabel.Add(TVBOption.Create(Token.Token, scNone,
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
          O := FOptionsLabel.Add(TVBOption.Create(Token.Token, scNone,
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
          O := FOptionsLabel.Add(TVBOption.Create(Token.Token, scNone,
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
          FOptionsLabel.Add(TVBOption.Create(Token.Token, scNone,
            Token.Line, Token.Column, iiUsesItem, Nil));
          NextNonCommentToken;
          If Token.TokenType In [ttLineEnd] Then
            NextNonCommentToken
          Else
            ErrorAndSeekToken(strLineEndExpected, Token.Token, strSeekTokens, stActual, Self);
        End Else
          ErrorAndSeekToken(strReservedWordExpected,
            'BASE, COMPARE, EXPLICIT or PRIVATE', strSeekTokens, stActual, Self);
    End;
End;

(**

  This method dispatches to sub functions to have the keywords found.

  @precon  None.
  @postcon Dispatches to sub functions to have the keywords found.

  @return  a Boolean

**)
Function TVBModule.Declarations : Boolean;

Var
  C : TComment;

Begin
  Repeat
    C := GetComment;
    Result :=
      Privates(C) Or
      Publics(C) Or
      Consts(scPublic, C) Or
      Dims(scPublic, C) Or
      Subs(scPublic, C, False, Nil) Or
      Functions(scPublic, C, False, Nil) Or
      Declares(scPublic, C) Or
      Friends(C) Or
      Props(scPublic, C, False) Or
      Records(scPublic, C) Or
      Enum(scPublic, C) Or
      Attributes;
    If Not Result Then
      If Not EndOfTokens Then
        If Token.TokenType In [ttLineEnd] Then
          NextNonCommentToken
        Else
          ErrorAndSeekToken(strUnDefinedToken, Token.Token, strSeekTokens, stActual, Self);
  Until EndOfTokens;
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
          ReferenceSymbol(Token);
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

(**

  This method defers parsing of subroutines to the MethodDecl method.

  @precon  None.
  @postcon Defers parsing of subroutines to the MethodDecl method.

  @param   Scope        as a TScope
  @param   C            as a TComment
  @param   boolStatic   as a Boolean
  @param   DeclareLabel as a TLabelContainer
  @return  a Boolean

**)
Function TVBModule.Subs(Scope : TScope; C : TComment; boolStatic : Boolean;
  DeclareLabel : TLabelContainer) : Boolean;

Var
  M : TVBMethod;

Begin
  Result := False;
  If Token.UToken = 'SUB' Then
    Begin
      Result := True;
      NextNonCommentToken;
      M := TVBMethod.Create(mtProcedure, Token.Token, Scope, Token.Line,
        Token.column);
      If DeclareLabel = Nil Then
        Begin
          If FImplementedMethodsLabel = Nil Then
            FImplementedMethodsLabel := Add(TLabelContainer.Create(
              strImplementedMethodsLabel, scNone, 0, 0, iiImplementedMethods, Nil)
              ) As TLabelContainer;
          M := FImplementedMethodsLabel.Add(M) As TVBMethod;
        End Else
          M := DeclareLabel.Add(M) As TVBMethod;
      MethodDecl(M, C);
      If M.Ext = '' Then
        FindMethodEnd(M, 'SUB');
    End;
End;

(**

  This method defers parsing of functions to the MethodDecl method.

  @precon  None.
  @postcon Defers parsing of function to the MethodDecl method.

  @param   Scope        as a TScope
  @param   C            as a TComment
  @param   boolStatic   as a Boolean
  @param   DeclareLabel as a TLabelContainer
  @return  a Boolean

**)
Function TVBModule.Functions(Scope : TScope; C : TComment; boolStatic : Boolean;
  DeclareLabel : TLabelContainer) : Boolean;

Var
  M : TVBMethod;

Begin
  Result := False;
  If Token.UToken = 'FUNCTION' Then
    Begin
      Result := True;
      NextNonCommentToken;
      M := TVBMethod.Create(mtFunction, Token.Token, Scope, Token.Line,
        Token.column);
      If DeclareLabel = Nil Then
        Begin
          If FImplementedMethodsLabel = Nil Then
            FImplementedMethodsLabel := Add(TLabelContainer.Create(
              strImplementedMethodsLabel, scNone, 0, 0, iiImplementedMethods, Nil)
              ) As TLabelContainer;
          M := FImplementedMethodsLabel.Add(M) As TVBMethod;
        End Else
          M := DeclareLabel.Add(M) As TVBMethod;
      MethodDecl(M, C);
      If M.Ext = '' Then
        FindMethodEnd(M, 'FUNCTION');
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
      T.AppendToken(Token);
      M.ReturnType.Add(T);
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

  This method attempts to find the end of the method while looking for exception
  / error handling code and exit statements.

  @precon  Method must be a valid TVBMethod instance .
  @postcon Attempts to find the end of the method while looking for exception /
           error handling code and exit statements .

  @param   AExceptionHnd as an IExceptionHandling
  @param   strMethodType as a String

**)
Procedure TVBModule.FindMethodEnd(AExceptionHnd : IExceptionHandling;
  strMethodType : String);

Begin
  RollBackToken;
  Repeat
    NextNonCommentToken;
    If Token.TokenType In [ttIdentifier] Then
      If Not ReferenceSymbol(Token) Then
        FUnResolvedSymbols.Add(Token.Token);
    // Check for Exception.Push & Exception.Pop
    If CompareText(Token.Token, 'Exception') = 0 Then
      Begin
        NextNonCommentToken;
        If Token.Token = '.' Then
          Begin
            NextNonCommentToken;
            If CompareText(Token.Token, 'Push') = 0 Then
              Begin
                AExceptionHnd.HasPush := True;
                NextNonCommentToken;
                While Token.TokenType In [ttIdentifier, ttReservedWord,
                  ttDoubleLiteral] Do
                  Begin
                    AExceptionHnd.PushName := AExceptionHnd.PushName + Token.Token;
                    NextNonCommentToken;
                    If (Token.Token = '+') Or (Token.Token = '&') Then
                      Begin
                        AExceptionHnd.PushName := AExceptionHnd.PushName +
                          Token.Token;
                        NextNonCommentToken;
                      End;
                  End;
                While Token.Token = ',' Do
                  Begin
                    NextNonCommentToken;
                    AExceptionHnd.PushParams.Add(Token.Token);
                    NextNonCommentToken;
                  End;
              End;
            If CompareText(Token.Token, 'Pop') = 0 Then
              Begin
                AExceptionHnd.HasPop := True;
                NextNonCommentToken;
              End;
          End;
      End;
    // Check for On Error
    If CompareText(Token.Token, 'On') = 0 Then
      Begin
        NextNonCommentToken;
        If CompareText(Token.Token, 'Error') = 0 Then
          Begin
            AExceptionHnd.HasErrorHnd := True;
            NextNonCommentToken;
          End;
      End;
    // Check for Exit Sub / Function
    If CompareText(Token.Token, 'Exit') = 0 Then
      Begin
        NextNonCommentToken;
        If IsKeyWord(Token.Token, ['function', 'sub']) Then
          Begin
            AExceptionHnd.HasExit := True;
            AExceptionHnd.ExitLine := PrevToken.Line;
            AExceptionHnd.ExitCol := PrevToken.Column;
            NextNonCommentToken;
          End;
      End;
    If (CompareText(Token.Token, 'GOTO') = 0) And (CompareText(PrevToken.Token, 'ERROR') <> 0) Then
      AddIssue(Format(strKeywordGOTOFound, [AExceptionHnd.MethodName, Token.Line,
        Token.Column]), scNone, Token.Line, Token.Column, etHint, Self);
  Until (PrevToken.UToken = 'END') And (Token.UToken = strMethodType);
  NextNonCommentToken;
  If Token.TokenType In [ttLineEnd] Then
    NextNonCommentToken
  Else
    ErrorAndSeekToken(strLineEndExpected, Token.Token, strSeekTokens, stActual, Self);
End;

(**

  This method sets the scope to private and defers parsing to a sub routine
  based on the key word found.

  @precon  None.
  @postcon Sets the scope to private and defers parsing to a sub routine based
           on the key word found.

  @param   C as a TComment
  @return  a Boolean

**)
Function TVBModule.Privates(C : TComment) : Boolean;
Begin
  Result := False;
  If Token.UToken = 'PRIVATE' Then
    Begin
      NextNonCommentToken;
      Result :=
        Consts(scPrivate, C) Or
        Subs(scPrivate, C, False, Nil) Or
        Functions(scPrivate, C, False, Nil) Or
        Props(scPrivate, C, False) Or
        Declares(scPrivate, C) Or
        Events(scPrivate, C) Or
        Records(scPrivate, C) Or
        Enum(scPrivate, C) Or
        Vars(scPrivate, C);
    End;
End;

(**

  This method sets the scope to private and defers parsing to a sub routine
  based on the key word found.

  @precon  None.
  @postcon Sets the scope to private and defers parsing to a sub routine based
           on the key word found.

  @param   C as a TComment
  @return  a Boolean

**)
Function TVBModule.Publics(C : TComment) : Boolean;
Begin
  Result := False;
  If Token.UToken = 'PUBLIC' Then
    Begin
      NextNonCommentToken;
      Result :=
        Consts(scPublic, C) Or
        Subs(scPublic, C, False, Nil) Or
        Functions(scPublic, C, False, Nil) Or
        Props(scPublic, C, False) Or
        Declares(scPublic, C) Or
        Events(scPublic, C) Or
        Records(scPublic, C) Or
        Enum(scPublic, C) Or
        Vars(scPublic, C);
    End;
End;

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

(**

  This method parses visual basic constants.

  @precon  None.
  @postcon Parses visual basic constants.

  @param   Scope as a TScope
  @param   C     as a TComment
  @return  a Boolean

**)
Function TVBModule.Consts(Scope : TScope; C : TComment) : Boolean;

Var
  Con : TVBConstant;

Begin
  Result := False;
  If Token.UToken = 'CONST' Then
    Begin
      Result := True;
      NextNonCommentToken;
      If Token.TokenType In [ttIdentifier] Then
        Begin
          Con := TVBConstant.Create(Token.Token, Scope, Token.Line, Token.Column,
            iiPublicConstant, C);
          If FConstantsLabel = Nil Then
            FConstantsLabel := Add(TLabelContainer.Create(strConstantsLabel, scNone, 0,
              0, iiPublicConstantsLabel, Nil)) As TLabelContainer;
          Con := FConstantsLabel.Add(Con) As TVBConstant;
          Con.Referenced := True;
          NextNonCommentToken;
          Con.Comment := C;
          If Token.UToken = 'AS' Then
            Begin
              AddToExpression(Con);
              If Token.TokenType In [ttIdentifier, ttReservedWord, ttDirective] Then
                Begin
                  AddToExpression(Con);
                  While Token.Token = '.' Do
                    Begin
                      AddToExpression(Con);
                      If Token.TokenType In [ttIdentifier] Then
                        Con.AddToken(Token.Token)
                      Else
                        ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekTokens, stActual, Self);
                    End;
                End Else
                  ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekTokens, stActual, Self);
            End;
          If Token.Token = '=' Then
            Begin
              AddToExpression(Con);
              While Not (Token.TokenType In [ttReservedWord, ttLineEnd, ttFileEnd]) Do
                AddToExpression(Con);
            End;
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

  @param   Scope as a TScope
  @param   C     as a TComment
  @return  a Boolean

**)
Function TVBModule.Dims(Scope : TScope; C : TComment) : Boolean;

Begin
  Result := False;
  If Token.UToken = 'DIM' Then
    Begin
      Result := True;
      NextNonCommentToken;
      Vars(Scope, C);
    End;
End;

(**

  This method processes the declare statement.

  @precon  None.
  @postcon Processes the declare statement.

  @param   Scope as a TScope
  @param   C     as a TComment
  @return  a Boolean

**)
Function TVBModule.Declares(Scope : TScope; C: TComment) : Boolean;

Var
  R : Boolean;

begin
  Result := False;
  If Token.UToken = 'DECLARE' Then
    Begin
      Result := True;
      NextNonCommentToken;
      If FDeclaredMethodsLabel = Nil Then
        FDeclaredMethodsLabel := Add(TLabelContainer.Create(strDeclaresLabel,
          scNone, 0, 0, iiExportedHeadingsLabel, Nil)) As TLabelContainer;
      R := Subs(Scope, C, False, FDeclaredMethodsLabel);
      If Not R Then
        R := Functions(Scope, C, False, FDeclaredMethodsLabel);
      If Not R Then
        ErrorAndSeekToken(strReservedWordExpected, Token.Token, strSeekTokens, stActual, Self);
    End;
end;

(**

  This method processes friend statements.

  @precon  None.
  @postcon Processes friend statements.

  @param   C as a TComment
  @return  a Boolean

**)
Function TVBModule.Friends(C: TComment) : Boolean;

Var
  R : Boolean;

begin
  Result := False;
  If Token.UToken = 'FRIEND' then
    Begin
      Result := True;
      NextNonCommentToken;
      R := Functions(scFriend, C, False, Nil);
      If Not R Then
        R := Subs(scFriend, C, False, Nil);
      If Not R Then
        R := Props(scFriend, C, True);
      If Not R Then
        ErrorAndSeekToken(strReservedWordExpected, 'FUNCTION, SUB or PROPERTY', strSeekTokens, stActual, Self);
    End;
end;

(**

  This method processes the properties statements.

  @precon  None.
  @postcon Processes the properties statements.

  @param   Scope      as a TScope
  @param   C          as a TComment
  @param   boolStatic as a Boolean
  @return  a Boolean

**)
Function TVBModule.Props(Scope : TScope; C : TComment; boolStatic : Boolean) : Boolean;

Var
  pt : TVBPropertyType;
  P : TVBProperty;
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
          P := TVBProperty.Create(pt, Token.Token, Scope, Token.Line,
            Token.Column, iiPublicProperty, C);
          If FImplementedPropertiesLabel = Nil Then
            FImplementedPropertiesLabel := Add(TLabelContainer.Create(strImplementedPropertiesLabel,
              scNone, 0, 0, iiPropertiesLabel, Nil)) As TLabelContainer;
          P := FImplementedPropertiesLabel.Add(P) As TVBProperty;
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
              FindMethodEnd(P, 'PROPERTY');
            End Else
              ErrorAndSeekToken(strLiteralExpected, '(', strSeekTokens, stActual, Self);
        End;
  End;
End;

(**

  This method processes Type/Record declarations.

  @precon  None.
  @postcon Processes Type/Record declarations.

  @param   Scope as a TScope
  @param   C     as a TComment
  @return  a Boolean

**)
Function TVBModule.Records(Scope : TScope; C : TComment) : Boolean;

Var
  R : TVBRecordDecl;
  F : TVBField;
  T: TTokenInfo;
  Com: TComment;

Begin
  Result := False;
  If Token.UToken = 'TYPE' Then
    Begin
      Result := True;
      NextNonCommentToken;
      If Token.TokenType In [ttIdentifier] Then
        Begin
          R := TVBRecordDecl.Create(Token.Token, Scope, Token.Line, Token.Column,
            iiPublicRecord, C);
          If FTypesLabel = Nil Then
            FTypesLabel := Add(TLabelContainer.Create(strTypesLabel, scNone, 0, 0,
              iiPublicTypesLabel, Nil)) As TLabelContainer;
          R := FTypesLabel.Add(R) As TVBRecordDecl;
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
  Result := ReferenceSection(AToken, FVariablesLabel);
  If Result Then
    Exit;
  Result := ReferenceSection(AToken, FConstantsLabel);
  If Result Then
    Exit;
  Result := ReferenceSection(AToken, FTypesLabel);
  If Result Then
    Exit;
  Result := CheckElement(FImplementedMethodsLabel);
  If Result Then
    Exit;
  Result := CheckElement(FImplementedPropertiesLabel);
  If Result Then
    Exit;
  Result := CheckElement(FDeclaredMethodsLabel);
  If Result Then
    Exit;
end;

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
    AddIssue(Format(strErrorHandling, [M.Identifier]), scNone, M.Line, M.Column, etWarning, Self);
  If ExceptionHandler.HasExit And ExceptionHandler.HasErrorHnd And Not boolNoTag Then
    AddIssue(Format(strExitStatement, [M.Identifier]), scNone, ExceptionHandler.ExitLine,
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

var
  strHigh: string;
  strLow: string;

begin
  if Token.Token = '(' then
    begin
      NextNonCommentToken;
      if Token.Token <> ')' then
        begin
          repeat
            if Token.TokenType in [ttIdentifier, ttNumber] then
            begin
              strLow := Token.Token;
              strHigh := strLow;
              NextNonCommentToken;
              if Token.UToken = 'TO' then
              begin
                NextNonCommentToken;
                if Token.TokenType in [ttIdentifier, ttNumber] then
                begin
                  strHigh := Token.Token;
                  NextNonCommentToken;
                end
                else
                  ErrorAndSeekToken(strNumberExpected, Token.Token, strSeekTokens, stActual, Self);
              end;
              Variable.AddDimension(strLow, strHigh);
            end
            else
              ErrorAndSeekToken(strNumberExpected, Token.Token, strSeekTokens, stActual, Self);
          until not IsToken(',', nil);
          if Token.Token = ')' then
            NextNonCommentToken
          else
            ErrorAndSeekToken(strLiteralExpected, Token.Token, strSeekTokens, stActual, Self);
        end
        else
        begin
          Variable.AddDimension('', '');
          NextNonCommentToken;
        end;
    end;
  if Token.UToken = 'AS' then
  begin
    NextNonCommentToken;
    if Token.UToken = 'NEW' then
      AddToExpression(Variable);
    if Token.TokenType in [ttIdentifier, ttReservedWord] then
    begin
      If ReferenceSymbol(Token) Then
        FUnResolvedSymbols.Add(Token.Token);
      AddToExpression(Variable);
      if (PrevToken.UToken = 'STRING') and (Token.Token = '*') then
      begin
        AddToExpression(Variable);
        if Token.TokenType in [ttNumber] then
          AddToExpression(Variable)
        else
          ErrorAndSeekToken(strNumberExpected, Token.Token, strSeekTokens, stActual, Self);
      end;
    end
    else
      ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekTokens, stActual, Self);
    if Token.Token = '.' then
    begin
      AddToExpression(Variable);
      if Token.TokenType in [ttIdentifier, ttReservedWord] then
        AddToExpression(Variable)
      else
        ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekTokens, stActual, Self);
    end;
  end;
end;

(**

  This method processes Enumerate declarations.

  @precon  None.
  @postcon Processes Enumerate declarations.

  @param   Scope as a TScope
  @param   C     as a TComment
  @return  a Boolean

**)
Function TVBModule.Enum(Scope : TScope; C: TComment) : Boolean;

Var
  E : TVBEnumerateDecl;
  I : TVBEnumIdent;

Begin
  Result := False;
  If Token.UToken = 'ENUM' Then
    Begin
      Result := True;
      NextNonCommentToken;
      If Token.TokenType = ttIdentifier Then
        Begin
          E := TVBEnumerateDecl.Create(Token.Token, Scope, Token.Line,
            Token.Column, iiPublicType, C);
          If FTypesLabel = Nil Then
            FTypesLabel := Add(TLabelContainer.Create(strTypesLabel, scNone, 0, 0,
              iiPublicTypesLabel, Nil)) As TLabelContainer;
          E := FTypesLabel.Add(E) As TVBEnumerateDecl;
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
                          If Token.TokenType In [ttNumber, ttIdentifier] Then
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

(**

  This method parses the event element of the grammar.

  @precon  None.
  @postcon The event if found is parsed and added to the module tree.

  @param   Scope as a TScope
  @param   C     as a TComment
  @return  a Boolean

**)
Function TVBModule.Events(Scope: TScope; C: TComment): Boolean;

Var
  E : TEventDecl;

Begin
  Result := False;
  If Token.UToken = 'EVENT' Then
    Begin
      Result := True;
      NextNonCommentToken;
      If Token.TokenType In [ttIdentifier] Then
        Begin
          E := TEventDecl.Create(mtProcedure, Token.Token, Scope, Token.Line,
            Token.Column);
          NextNonCommentToken;
          If FEventsLabel = Nil Then
            FEventsLabel := Add(TLabelContainer.Create(strEventsLabel,
              scNone, 0, 0, iiDispInterfacesLabel, Nil)) As TLabelContainer;
          E := FEventsLabel.Add(E) As TEventDecl;
          E.Comment := C;
          If Token.Token = '(' Then
            Begin
              NextNonCommentToken;
              If Token.Token <> ')' Then
                Parameters(E);
              If Token.Token = ')' Then
                NextNonCommentToken
              Else
                ErrorAndSeekToken(strLiteralExpected, ')', strSeekTokens, stActual, Self);
            End Else
              ErrorAndSeekToken(strLiteralExpected, '(', strSeekTokens, stActual, Self);
      End Else
        ErrorAndSeekToken(strIdentExpected, Token.Token, strSeekTokens, stActual, Self);
    End;
End;

End.
