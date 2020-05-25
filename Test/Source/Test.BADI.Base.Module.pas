(**
  
  This module contains DUnit test for the Browse and Doc It code.

  @Author  David Hoyle
  @Version 1.011
  @Date    24 May 2020

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
unit Test.BADI.Base.Module;

interface

uses
  TestFramework,
  Graphics,
  SysUtils,
  Classes,
  BADI.Base.Module,
  Contnrs,
  BADI.ElementContainer,
  BADI.TokenInfo,
  BADI.Types,
  Xml.XMLSchemaTags,
  BADI.Comment,
  BADI.Generic.TypeDecl,
  BADI.Generic.Constant,
  BADI.Generic.Variable,
  BADI.Generic.Parameter,
  BADI.Generic.MethodDecl,
  BADI.Generic.PropertyDecl;

type
  (** This class represents a single identifier with line, col and comment
      attributes. **)
  TIdent = Class {$IFDEF D2005} Abstract {$ENDIF} (TElementContainer);

  TElementContainerHelper = Class Helper for TElementContainer
    Function  FindIssue(Const strTypeName : String; Const slSource : TStringList = Nil) : String;
    Function  FirstError(Const slSource : TStringList = Nil) : String;
    Function  FirstWarning(Const slSource : TStringList = Nil) : String;
    Function  FirstHint(Const slSource : TStringList = Nil) : String;
    Function  FirstDocConflict(Const slSource : TStringList = Nil) : String;
    Function  FirstCheck(Const slSource : TStringList = Nil) : String;
    Function  FirstMetric(Const slSource : TStringList = Nil) : String;
    Function  DocConflict(Const iConflict : Integer) : String;
    Procedure DeleteDocumentConflicts;
    Function  HeadingCount(Const strHeading : String) : Integer;
  End;

  TBaseLanguageModuleHelper = Class Helper For TBaseLanguageModule
    Function CurrentToken : TTokenInfo;
  End;

  TClassOfTGenericTypeDecl = Class Of TGenericTypeDecl;

  TTestType = (ttErrors, ttWarnings, ttHints, ttDocConflicts, ttChecks, ttMetrics);
  TTestTypes = Set of TTestType;

  TExtendedTestCase = Class(TTestCase)
  Strict Private
    FSource : TStringList;
  Public
    Constructor Create(MethodName: String); Overload; Override;
    Constructor Create(MethodName: String; RunCount: Int64); Overload; Override;
    Destructor Destroy; Override;
    Procedure CheckEquals(ttExpected, ttActual : TBADITokenType; strMsg : String = ''); Overload;
    Procedure CheckEquals(iiExpected, iiActual : TBADIImageIndex; strMsg : String = ''); Overload;
    Procedure CheckEquals(iiExpected : TBADIImageIndex; iActual : Integer;
      strMsg : String = ''); Overload;
    Procedure CheckEquals(scExpected, scActual : TScope; strMsg : String = ''); Overload;
    Procedure CheckEquals(trExpected, trActual : TTokenReference; strMsg : String = ''); Overload;
    Procedure CheckEquals(ctExpected, ctActual : TCommentType; strMsg : String = ''); Overload;
    Procedure CheckEquals(strExpected, strActual : String; strMsg : String = ''); Overload; Override;
    Procedure TestGrammarForErrors(Const Parser : TBaseLanguageModuleClass; Const strTemplate,
      strInterface, strImplementation: String; Const TestTypes : TTestTypes;
      Const strCheckValues : Array Of String;
      Const iErrors : Integer = 0;
      Const iWarnings : Integer = 0;
      Const iHints : Integer = 0;
      Const iDocConflicts : Integer = 0;
      Const iChecks : Integer = 0;
      Const iMetrics : Integer = 0);
  Published
  End;

  // Test methods for class TElementContainer

  TTestElementContainer = Class(TElementContainer)
  Public
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

  TTestIdent = Class(TIdent)
  Public
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

  TTestGenericTypeDecl = Class(TGenericTypeDecl)
  Public
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

  TTestGenericConstant = Class(TGenericConstant)
  Public
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

  TTestGenericVariable = Class(TGenericVariable)
  Public
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

  TTestGenericParameter = Class(TGenericParameter)
  Public
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

  TTestGenericMethodDecl = Class(TGenericMethodDecl)
  Public
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

  TTestGenericProperty = Class(TGenericProperty)
  Public
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

  //-----------------------------------------------------------------------------------------------

  TTestBaseLanguageModule = Class(TBaseLanguageModule)
  Public
    Property CompilerDefines;
    Function GetComment(Const CommentPosition : TCommentPosition = cpBeforeCurrentToken) : TComment;
      Override;
    procedure ProcessCompilerDirective(var iSkip : Integer); Override;
    Function ReservedWords : TKeyWords; Override;
    Function Directives : TKeyWords; Override;
  End;

  // Test methods for class TBaseLanguageModule

  TestTBaseLanguageModule = class(TTestCase)
  strict private
    FSource : String;
    FBaseLanguageModule: TTestBaseLanguageModule;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCreate;
    Procedure TestAddTickCount;
    Procedure TestAddDef;
    Procedure TestDeleteDef;
    Procedure TestIfDef;
    Procedure TestIfNotDef;
    Procedure TestCheckDocumentation;
    Procedure TestAsString;
    Procedure TestAddToExpression;
    Procedure TestIsToken;
    Procedure TestOpTickCount;
    Procedure TestOpTickCounts;
    Procedure TestOpTickCountByIndex;
    Procedure TestOpTickCountName;
    Procedure TestModuleName;
    Procedure TestBodyComment;
    Procedure TestBodyCommentCount;
    Procedure TestFileName;
    Procedure TestModified;
    Procedure TestCompilerConditionStack;
    Procedure TestBytes;
    Procedure TestLines;
    //Procedure TestMemoryLeak;
  end;

implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  Windows,
  TypInfo,
  BADI.ResourceStrings,
  BADI.Constants,
  BADI.Options;

{ TTestBaseLanguageModule }

function TBaseLanguageModuleHelper.CurrentToken: TTokenInfo;

begin
  Result := Token;
end;

procedure TElementContainerHelper.DeleteDocumentConflicts;

Var
  i : Integer;

begin
  For i := ElementCount DownTo 1 Do
    If Copy(Elements[i].AsString(True, False), 1, Length(strDocumentationConflicts)) =
      strDocumentationConflicts Then
      DeleteElement(i);
end;

function TElementContainerHelper.DocConflict(Const iConflict : Integer): String;

Var
  E : TElementContainer;

begin
  Result := '(No Documentation Conflicts)';
  E := FindElement(strDocumentationConflicts);
  If (E <> Nil) And (E.ElementCount > 0) Then
    Begin
      E := E.Elements[1];
      If E.ElementCount >= iConflict Then
        Begin
          E := E.Elements[iConflict];
          Result := Format('%d) %s', [iConflict, E.AsString(True, False)]);
        End;
    End;
end;

Function TElementContainerHelper.FirstDocConflict(Const slSource : TStringList = Nil) : String;

Begin
  Result := FindIssue(strDocumentationConflicts, slSource);
End;

Function TElementContainerHelper.FirstError(Const slSource : TStringList = Nil) : String;

Begin
  Result := FindIssue(strErrors, slSource);
End;

Function TElementContainerHelper.FirstHint(Const slSource : TStringList = Nil) : String;

Begin
  Result := FindIssue(strHints, slSource);
End;

Function TElementContainerHelper.FindIssue(Const strTypeName: String;
  Const slSource: TStringList): String;

Var
  E : TElementContainer;
  strMsg: String;
  strLineNo: String;

begin
  Result := '';
  E := FindElement(strTypeName);
  If Assigned(E) Then
    Begin
      E := E.Elements[1];
      strMsg := E.AsString(True, False);
      If Assigned(slSource) Then
        Begin
          strLineNo := Format('[%4.4d]', [E.Line]);
          strMsg := strMsg + #13#10 + strLineNo + slSource[Pred(E.Line)];
          strMsg := strMsg + #13#10 + StringOfChar(#32, Length(strLineNo) + Pred(E.Column)) + '^';
          
        End;
      Result := strMsg + #13#10;
    End;
End;

Function TElementContainerHelper.FirstCheck(Const slSource : TStringList = Nil) : String;

Begin
  Result := FindIssue(strChecks, slSource);
End;

Function TElementContainerHelper.FirstMetric(Const slSource : TStringList = Nil) : String;

Begin
  Result := FindIssue(strMetrics, slSource);
End;

Function TElementContainerHelper.FirstWarning(Const slSource : TStringList = Nil) : String;

begin
  Result := FindIssue(strWarnings, slSource);
end;

function TElementContainerHelper.HeadingCount(Const strHeading : String): Integer;

var
  E: TElementContainer;

begin
  Result := 0;
  E := FindElement(strHeading);
  If E <> Nil Then
    Result := E.ElementCount;
end;

{ TTestBaseLanguageModule }

Function TTestBaseLanguageModule.GetComment(Const CommentPosition : TCommentPosition = cpBeforeCurrentToken) : TComment;
begin
  Result := Nil;
end;

function TTestBaseLanguageModule.ReservedWords: TKeyWords;
begin
end;

function TTestBaseLanguageModule.Directives: TKeyWords;
begin
end;

procedure TTestBaseLanguageModule.ProcessCompilerDirective(var iSkip: Integer);
begin
end;

{ TExtendedTestCase }

Procedure TExtendedTestCase.TestGrammarForErrors(Const Parser : TBaseLanguageModuleClass;
  Const strTemplate, strInterface, strImplementation: String; Const TestTypes : TTestTypes;
  Const strCheckValues : Array Of String; Const iErrors : Integer = 0; Const iWarnings : Integer = 0;
  Const iHints : Integer = 0; Const iDocConflicts : Integer = 0; Const iChecks : Integer = 0;
  Const iMetrics : Integer = 0);

Const
  cDelimiter : Char = '\';

  Function GetElements(Element, ParentElement  : TElementContainer) : String;

  Var
    i : Integer;

  Begin
    Result := '';
    If Element = Nil Then
      Begin
        For i := 1 To ParentElement.ElementCount Do
          Begin
            If Result <> '' Then
              Result := Result + ', ';
            Result := Result + '[' + ParentElement.Elements[i].Identifier + ']';
          End;
      End;
  End;

  Function SearchForElement(Element : TElementContainer; strValue : String) : TElementContainer;

  Begin
    Result := Element.FindElement(strValue);
    If Result = Nil Then
      Result := Element.FindElement(strValue, ftIdentifier);
  End;

Var
  P: TBaseLanguageModule;
  T, U : TElementContainer;
  strValue: String;
  iCheck: Integer;
  strCheckValue: String;
  i : Integer;
  strKey : String;
  strValueScope : String;

Begin
  FSource.Text := Format(strTemplate, [strInterface, strImplementation]);
  P := Parser.CreateParser(FSource.Text, 'TestSource.pas', False, [moParse]);
  Try
    If ttErrors In TestTypes Then
      CheckEquals(iErrors, P.HeadingCount(strErrors), 'ERRORS:'#13#10 + P.FirstError(FSource));
    If ttWarnings In TestTypes Then
      CheckEquals(iWarnings, P.HeadingCount(strWarnings), 'WARNINGS:'#13#10 + P.FirstWarning(FSource));
    If ttHints In TestTypes Then
      CheckEquals(iHints, P.HeadingCount(strHints), 'HINTS:'#13#10 + P.FirstHint(Fsource));
    If ttDocConflicts In TestTypes Then
      CheckEquals(iDocConflicts, P.HeadingCount(strDocumentationConflicts), 'DOCCONFLICTS:'#13#10 +
        P.FirstDocConflict);
    If ttChecks In TestTypes Then
      CheckEquals(iChecks, P.HeadingCount(strChecks), 'CHECKS:'#13#10 + P.FirstCheck);
    If ttMetrics In TestTypes Then
      CheckEquals(iMetrics, P.HeadingCount(strMetrics), 'METRICS:'#13#10 + P.FirstMetric);
    For iCheck := Low(strCheckValues) to High(strCheckValues) Do
      If strCheckValues[iCheck] <> '' Then
        Begin
         strCheckValue := strCheckValues[iCheck];
          T := P;
          While (Pos(cDelimiter, strCheckValue) > 0) And (Pos(cDelimiter, strCheckValue) < Pos('|', strCheckValue)) Do
            Begin
              strValue := Copy(strCheckValue, 1, Pos(cDelimiter, strCheckValue) - 1);
              Delete(strCheckValue, 1, Pos(cDelimiter, strCheckValue));
              U := SearchForElement(T, strValue);
              Check(U <> Nil, Format('%d.2) %s not found (found %s): %s', [Succ(iCheck), strValue, GetElements(U, T), strCheckValues[iCheck]]));
              T := U;
            End;
          Check(T.ElementCount > 0, Format('%d.3) Element Count: %s', [Succ(iCheck), strCheckValue]));
          i := Pos('|', strCheckValue);
          Check(i > 0, Format('%d.4) Cannot find KEY to search for: %s', [Succ(iCheck), strCheckValue]));
          strKey := Copy(strCheckvalue, 1, i - 1);
          Delete(strCheckValue, 1, i);
          i := Pos('|', strCheckValue);
          Check(i > 0, Format('%d.5) Cannot get scope: %s', [Succ(iCheck), strCheckvalue]));
          strValueScope := Copy(strCheckValue, i + 1, Length(strCheckValue) - i);
          Delete(strCheckValue, i, Length(strCheckValue) - (i - 1));

          U := SearchForElement(T, strKey);
          Check(U <> Nil, Format('%d.6) Cannot find KEY to check (%s): %s', [Succ(iCheck), GetElements(U, T), strCheckValues[iCheck]]));
          CheckEquals(strCheckValue, U.AsString(True, False), Format('%d.7) Value check failed (%s, %s): ', [Succ(iCheck), U.ClassName, strCheckValues[iCheck]]));
          CheckEquals(strValueScope, GetEnumName(TypeInfo(TScope), Ord(U.Scope)), Format('%d.8) Incorrect Scope: %s', [Succ(iCheck), strCheckValues[iCheck]]));
        End Else
          Check(strCheckValue <> '', Format('%d.1) strCheckValue is NULL!', [Succ(iCheck)]));
  Finally
    P.Free;
  End;
End;

procedure TExtendedTestCase.CheckEquals(ttExpected, ttActual: TBADITokenType;
  strMsg: String = '');

begin
  FCheckCalled := True;
  If CompareText(strTokenType[ttExpected], strTokenType[ttActual]) <> 0 Then
    FailNotEquals(strTokenType[ttExpected], strTokenType[ttActual], strMsg,
      ReturnAddress);
end;

procedure TExtendedTestCase.CheckEquals(iiExpected, iiActual: TBADIImageIndex;
  strMsg: String = '');

begin
  FCheckCalled := True;
  If iiExpected <> iiActual Then
    FailNotEquals(BADIImageList[iiExpected].FResourcename,
      BADIImageList[iiActual].FResourcename,
      strMsg, ReturnAddress);
end;

procedure TExtendedTestCase.CheckEquals(scExpected, scActual: TScope;
  strMsg: String);

Const
  strScopes : Array[Low(TScope)..High(TScope)] Of String = (
    'scNone', 'scGlobal', 'scLocal', 'scPrivate', 'scProtected', 'scPublic',
    'scPublished', 'scFriend');

begin
  FCheckCalled := True;
  If CompareText(strScopes[scExpected], strScopes[scActual]) <> 0 Then
    FailNotEquals(strScopes[scExpected], strScopes[scActual], strMsg,
      ReturnAddress);
end;

procedure TExtendedTestCase.CheckEquals(trExpected, trActual: TTokenReference;
  strMsg: String);

Const
  strTokenReference : Array[Low(TTokenReference)..High(TTokenReference)] Of String = (
    'trUnknown', 'trUnresolved', 'trResolved');

begin
  FCheckCalled := True;
  If CompareText(strTokenReference[trExpected], strTokenReference[trActual]) <> 0 Then
    FailNotEquals(strTokenReference[trExpected], strTokenReference[trActual], strMsg,
      ReturnAddress);
end;

procedure TExtendedTestCase.CheckEquals(ctExpected, ctActual: TCommentType;
  strMsg: String);

Const
  strCommentTypes : Array[Low(TCommentType)..High(TCommentType)] Of String = (
    'ctNone', 'ctPascalBlock', 'ctPascalBrace', 'ctCPPBlock', 'ctCPPLine',
    'ctVBLine', 'ctXML');

begin
  FCheckCalled := True;
  If CompareText(strCommentTypes[ctExpected], strCommentTypes[ctActual]) <> 0 Then
    FailNotEquals(strCommentTypes[ctExpected], strCommentTypes[ctActual], strMsg,
      ReturnAddress);
end;

procedure TExtendedTestCase.CheckEquals(iiExpected: TBADIImageIndex;
  iActual: Integer; strMsg: String);

Var
  i : Integer;

begin
  FCheckCalled := True;
  i := Integer(iiExpected) - 1;
  If i <> iActual Then
    FailNotEquals(BADIImageList[iiExpected].FResourcename,
      BADIImageList[TBADIImageIndex(iActual + 1)].FResourcename, strMsg, ReturnAddress);
end;

Procedure TExtendedTestCase.CheckEquals(strExpected, strActual: String;
  strMsg: String);

Var
  i : Integer;
  iPosition : Integer;

Begin
  FCheckCalled := True;
  If CompareText(strExpected, strActual) <> 0 Then
    Begin
      iPosition := 0;
      For i := 1 To Length(strExpected) Do
        If Length(strActual) >= i Then
           If strActual[i] <> strExpected[i] Then
             Begin
               iPosition := i;
               Break;
             End;
      If iPosition = 0 Then
        strMsg := strMsg + ' {Actual too small}'
      Else
        strMsg := strMsg + Format( ' [[Difference @ character %d: %s]]',
          [iPosition, Copy(strActual, 1, iPosition)]);
      FailNotEquals(strExpected, strActual, strMsg, ReturnAddress);
    End;
End;

Constructor TExtendedTestCase.Create(MethodName: String);

Begin
  Inherited Create(MethodName);
  FSource := TStringList.Create;
End;

Constructor TExtendedTestCase.Create(MethodName: String; RunCount: Int64);

Begin
  Inherited Create(MethodName, RunCount);
  FSource := TStringList.Create;
End;

Destructor TExtendedTestCase.Destroy;

Begin
  FSource.Free;
  Inherited Destroy;
End;

{ TTestElementContainer }

Function TTestElementContainer.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;
begin
  Result := Identifier;
end;

{ TTestIdent }

Function TTestIdent.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;
begin
  result := Identifier;
end;

{ TTestGenericTypeDecl }

Function TTestGenericTypeDecl.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;
begin
  Result := BuildStringRepresentation(boolShowIdentifier, boolForDocumentation, '', 9999);
end;

{ TTestGenericConstant }

Function TTestGenericConstant.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;
begin
  Result := Identifier;
end;

{ TTestGenericVariable }

Function TTestGenericVariable.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;
begin
  Result := Identifier;
end;

{ TTestGenericParameter }

Function TTestGenericParameter.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;
begin
  Result := Identifier + #32'='#32 + ParamType.AsString(boolShowIdentifier,
    boolForDocumentation);
end;

{ TTestGenericMethodDecl }

Function TTestGenericMethodDecl.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;
begin
  Result := Identifier;
end;

{ TTestGenericProperty }

function TTestGenericProperty.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean): String;
begin
  Result := Identifier;
end;

// Test methods for class TBaseLanguageModule

procedure TestTBaseLanguageModule.SetUp;
begin
  FSource := 'This is some text.';
  FBaseLanguageModule := TTestBaseLanguageModule.CreateParser(FSource,
    'D:\Path\TestFile.txt', True, [moParse, moCheckForDocumentConflicts]);
  FBaseLanguageModule.AddToken(TTokenInfo.Create('Hello', 0, 1, 1, 5, ttIdentifier));
  FBaseLanguageModule.AddToken(TTokenInfo.Create('Goodbye', 7, 1, 7, 7, ttIdentifier));
end;

procedure TestTBaseLanguageModule.TearDown;
begin
  FBaseLanguageModule.Free;
  FBaseLanguageModule := nil;
end;

procedure TestTBaseLanguageModule.TestAddDef;
begin
  CheckEquals(0, FBaseLanguageModule.CompilerDefines.Count);
  FBaseLanguageModule.AddDef('Compiler_Def');
  CheckEquals(1, FBaseLanguageModule.CompilerDefines.Count);
end;

procedure TestTBaseLanguageModule.TestAddTickCount;
begin
  CheckEquals(0, FBaseLanguageModule.OpTickCounts);
  FBaseLanguageModule.AddTickCount('Hello');
  CheckEquals(1, FBaseLanguageModule.OpTickCounts);
end;

procedure TestTBaseLanguageModule.TestAddToExpression;
var
  C: TElementContainer;
begin
  C := TTestElementContainer.Create('', scNone, 0, 0, iiNone, Nil);
  Try
    Checkequals(0, C.TokenCount);
    FBaseLanguageModule.AddToExpression(C);
    Checkequals(1, C.TokenCount);
  Finally
    C.Free;
  End;
end;

procedure TestTBaseLanguageModule.TestAsString;
begin
  CheckEquals('TestFile.txt', FBaseLanguageModule.AsString(True, False));
end;

procedure TestTBaseLanguageModule.TestBodyComment;
begin
  FBaseLanguageModule.AddBodyComment(TComment.Create('Hello.', 0, 0, 0));
  CheckEquals('Hello.', FBaseLanguageModule.BodyComment[0].AsString(99, False));
end;

procedure TestTBaseLanguageModule.TestBodyCommentCount;
begin
  CheckEquals(0, FBaseLanguageModule.BodyCommentCount);
  FBaseLanguageModule.AddBodyComment(TComment.Create('Hello.', 0, 0, 0));
  CheckEquals(1, FBaseLanguageModule.BodyCommentCount);
end;

procedure TestTBaseLanguageModule.TestBytes;
begin
  CheckEquals(5 + 1 + 7, FBaseLanguageModule.Bytes);
end;

procedure TestTBaseLanguageModule.TestCheckDocumentation;
var
  boolCascade: Boolean;
  C: TComment;
begin
  FBaseLanguageModule.CheckDocumentation(boolCascade);
  Checkequals('1) This module has no document comment.', FBaseLanguageModule.DocConflict(1));
  FBaseLanguageModule.DeleteDocumentConflicts;
  C := TComment.Create('This is a description.', 0, 0, 0);
  Try
    FBaseLanguageModule.Comment := C;
    FBaseLanguageModule.CheckDocumentation(boolCascade);
    Checkequals('1) This module is missing a documentation date (''' +
      FormatDateTime('dd mmm yyyy', Now) + ''').', FBaseLanguageModule.DocConflict(1));
  Finally
    C.Free;
  End;
  FBaseLanguageModule.DeleteDocumentConflicts;
  C := TComment.Create('This is a description.'#13#10 +
    '@date ' + FormatDateTime('dd mmm yyyy', Now - 7), 0, 0, 0);
  Try
    FBaseLanguageModule.Comment := C;
    FBaseLanguageModule.CheckDocumentation(boolCascade);
    Checkequals('1) The module documentation date ''' +
      FormatDateTime('dd mmm yyyy', Now - 7) + ''' is incorrect (''' +
      FormatDateTime('dd mmm yyyy', Now) + ''').', FBaseLanguageModule.DocConflict(1));
  Finally
    C.Free;
  End;
  FBaseLanguageModule.DeleteDocumentConflicts;
  C := TComment.Create('This is a description.'#13#10 +
    '@date ' + FormatDateTime('dd mmm yyyy', Now), 0, 0, 0);
  Try
    FBaseLanguageModule.Comment := C;
    FBaseLanguageModule.CheckDocumentation(boolCascade);
    Checkequals('1) This module is missing a documentation version.', FBaseLanguageModule.DocConflict(1));
  Finally
    C.Free;
  End;
  FBaseLanguageModule.DeleteDocumentConflicts;
  C := TComment.Create('This is a description.'#13#10 +
    '@date ' + FormatDateTime('dd mmm yyyy', Now) +#13#10+
    '@version 1.0', 0, 0, 0);
  Try
    FBaseLanguageModule.Comment := C;
    FBaseLanguageModule.CheckDocumentation(boolCascade);
    Checkequals('1) This module is missing a documentation author.', FBaseLanguageModule.DocConflict(1));
  Finally
    C.Free;
  End;
  FBaseLanguageModule.DeleteDocumentConflicts;
  C := TComment.Create('This is a description.'#13#10 +
    '@date 32 jan 2008', 0, 0, 0);
  Try
    FBaseLanguageModule.Comment := C;
    FBaseLanguageModule.CheckDocumentation(boolCascade);
    Checkequals('1) The module documentation date ''32 jan 2008'' is not valid (''' +
      FormatDateTime('dd mmm yyyy', Now) + ''').', FBaseLanguageModule.DocConflict(1));
  Finally
    C.Free;
  End;
end;

procedure TestTBaseLanguageModule.TestCompilerConditionStack;
begin
  CheckEquals(False, FBaseLanguageModule.CompilerConditionStack.CanPop);
  FBaseLanguageModule.CompilerConditionStack.Push(cdtIFDEF, ccIncludeCode, 1);
  CheckEquals(True, FBaseLanguageModule.CompilerConditionStack.CanPop);
  FBaseLanguageModule.CompilerConditionStack.Pop;
  CheckEquals(False, FBaseLanguageModule.CompilerConditionStack.CanPop);
end;

procedure TestTBaseLanguageModule.TestCreate;
begin
  CheckEquals(True, FBaseLanguageModule.Modified);
  CheckEquals('D:\Path\TestFile.txt', FBaseLanguageModule.FileName);
end;

procedure TestTBaseLanguageModule.TestDeleteDef;
begin
  CheckEquals(0, FBaseLanguageModule.CompilerDefines.Count);
  FBaseLanguageModule.AddDef('Hello');
  CheckEquals(1, FBaseLanguageModule.CompilerDefines.Count);
  FBaseLanguageModule.DeleteDef('Hello');
  CheckEquals(0, FBaseLanguageModule.CompilerDefines.Count);
end;

procedure TestTBaseLanguageModule.TestFileName;
begin
  CheckEquals('D:\Path\TestFile.txt', FBaseLanguageModule.FileName);
end;

procedure TestTBaseLanguageModule.TestIfDef;
begin
  CheckEquals(False, FBaseLanguageModule.IfDef('Hello'));
  FBaseLanguageModule.AddDef('Hello');
  CheckEquals(True, FBaseLanguageModule.IfDef('Hello'));
  FBaseLanguageModule.DeleteDef('Hello');
  CheckEquals(False, FBaseLanguageModule.IfDef('Hello'));
end;

procedure TestTBaseLanguageModule.TestIfNotDef;
begin
  CheckEquals(True, FBaseLanguageModule.IfNotDef('Hello'));
  FBaseLanguageModule.AddDef('Hello');
  CheckEquals(False, FBaseLanguageModule.IfNotDef('Hello'));
  FBaseLanguageModule.DeleteDef('Hello');
  CheckEquals(True, FBaseLanguageModule.IfNotDef('Hello'));
end;

procedure TestTBaseLanguageModule.TestIsToken;
begin
  CheckEquals(True, FBaseLanguageModule.IsToken('Hello', Nil));
end;

procedure TestTBaseLanguageModule.TestLines;
begin
  CheckEquals(1, FBaseLanguageModule.Lines);
end;

//Procedure TestTBaseLanguageModule.TestMemoryLeak;
//
//Begin
//  TObject.Create;
//End;

procedure TestTBaseLanguageModule.TestModified;
begin
  CheckEquals(True, FBaseLanguageModule.Modified);
end;

procedure TestTBaseLanguageModule.TestModuleName;
begin
  CheckEquals('D:\Path\TestFile.txt', FBaseLanguageModule.ModuleName);
  FBaseLanguageModule.ModuleName := 'TestFile.txt';
  CheckEquals('TestFile.txt', FBaseLanguageModule.ModuleName);
end;

procedure TestTBaseLanguageModule.TestOpTickCount;
var
  dblDiff: Double;
begin
  FBaseLanguageModule.AddTickCount('Hello');
  Sleep(100);
  FBaseLanguageModule.AddTickCount('Goodbye');
  dblDiff := FBaseLanguageModule.OpTickCount['Hello', 'Goodbye'] - 100;
  If dblDiff < 0 Then
    dblDiff := -dblDiff;
  Check(dblDiff < 2.0);
end;

procedure TestTBaseLanguageModule.TestOpTickCountByIndex;
begin
  FBaseLanguageModule.AddTickCount('Hello'); // more than 24 days since last restart.
  //Role over of GetTickCount() due to being put in a 32 Int instead of a 64 bit int
  FBaseLanguageModule.AddTickCount('Goodbye');
  Check(FBaseLanguageModule.OpTickCountByIndex[0] > 0);
  Check(FBaseLanguageModule.OpTickCountByIndex[1] >= FBaseLanguageModule.OpTickCountByIndex[0]);
end;

procedure TestTBaseLanguageModule.TestOpTickCountName;
begin
  FBaseLanguageModule.AddTickCount('Hello');
  FBaseLanguageModule.AddTickCount('Goodbye');
  CheckEquals('Hello', FBaseLanguageModule.OpTickCountName[0]);
  CheckEquals('Goodbye', FBaseLanguageModule.OpTickCountName[1]);
end;

procedure TestTBaseLanguageModule.TestOpTickCounts;
begin
  CheckEquals(0, FBaseLanguageModule.OpTickCounts);
  FBaseLanguageModule.AddTickCount('Hello');
  CheckEquals(1, FBaseLanguageModule.OpTickCounts);
  FBaseLanguageModule.AddTickCount('Goodbye');
  CheckEquals(2, FBaseLanguageModule.OpTickCounts);
end;

initialization
  TBADIOptions.BADIOptions.Options := [doCustomDrawing..doStrictConstantExpressions];
  // Register any test cases with the test runner
  RegisterTest('BADI.Base.Module Tests', TestTBaseLanguageModule.Suite);
End.
