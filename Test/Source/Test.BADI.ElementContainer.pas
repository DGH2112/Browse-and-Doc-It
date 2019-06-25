(**
  
  This module contains DUnit test for the Browse and Doc It code.

  @Author  David Hoyle
  @Version 1.0
  @Date    21 Jun 2019

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
Unit Test.BADI.ElementContainer;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.ElementContainer,
  BADI.Comment;

Type
  TestTElementContainer = Class(TExtendedTestCase)
  Strict Private
    FElementContainer: TTestElementContainer;
    FComment: TComment;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestAdd;
    Procedure TestAdd1;
    Procedure TestAdd2;
    Procedure TestAddTokens;
    Procedure TestFindElement;
    Procedure TestAssign;
    Procedure TestFindToken;
    Procedure TestDeleteElement;
    Procedure TestCheckDocumentation;
    Procedure TestReferenceSymbol;
    Procedure TestAddIssue;
    Procedure TestAddDocumentConflict;
    Procedure TestAsString;
    Procedure TestCheckReferences;
    Procedure TestReferenceSection;
    Procedure TestElementCount;
    Procedure TestElements;
    Procedure TestComment;
    Procedure TestScope;
    Procedure TestImageIndex;
    Procedure TestImageIndexAdjustedForScope;
    Procedure TestSorted;
    Procedure TestReferenced;
    Procedure TestParent;
  End;

Implementation

uses
  BADI.Types, BADI.TokenInfo, BADI.ResourceStrings, BADI.DocIssue, BADI.Functions;

Procedure TestTElementContainer.SetUp;
Begin
  FComment := TComment.CreateComment('This is a test comment.', 12, 23);
  FElementContainer := TTestElementContainer.Create('TestElement', scPrivate, 12, 23,
    iiPublicConstant, FComment);
End;

Procedure TestTElementContainer.TearDown;
Begin
  FComment.Free;
  FElementContainer.Free;
  FElementContainer := Nil;
End;

Procedure TestTElementContainer.TestAdd;

Begin
  FElementContainer.Add(TTestElementContainer.Create('Hello', scPrivate, 1, 2, iiUsesItem, Nil));
  CheckEquals(1, FElementContainer.ElementCount);
  CheckEquals('Hello', FElementContainer.Elements[1].Identifier);
End;

Procedure TestTElementContainer.TestAdd1;

Begin
  FElementContainer.Add('Hello', iiUsesItem, scPrivate, Nil);
  CheckEquals(1, FElementContainer.ElementCount);
  CheckEquals('Hello', FElementContainer.Elements[1].Identifier);
End;

Procedure TestTElementContainer.TestAdd2;

Var
  T: TTokenInfo;

Begin
  T := TTokenInfo.Create('Hello', 1, 2, 3, 5, ttUnknown);
  Try
    FElementContainer.Add(T, scPrivate, iiUsesItem, Nil);
    CheckEquals(1, FElementContainer.ElementCount);
    CheckEquals('Hello', FElementContainer.Elements[1].Identifier);
  Finally
    T.Free;
  End;
End;

Procedure TestTElementContainer.TestAddDocumentConflict;

Var
  rec: TDocConflictTable;
  DC: TElementContainer;
  strCategory: String;

Begin
  strCategory := 'Things';
  rec.FMessage := 'This is a document conflict message (%s, %s).';
  rec.FDescription := 'This is a document conflict description.';
  rec.FConflictType := dciMissing;
  FElementContainer.AddDocumentConflict(['First', 'Second'], 12, 23, Nil,
    strCategory, rec);
  DC := FElementContainer.FindElement(strDocumentationConflicts);
  Check(DC <> Nil, 'DC is null');
  CheckEquals(strDocumentationConflicts, DC.Identifier);
  DC := DC.FindElement('Things');
  Check(DC <> Nil, 'DC is null');
  CheckEquals('Things', DC.Identifier);
  DC := DC.Elements[1];
  Check(DC Is TDocumentConflict, 'DC is not TDocIssue');
  CheckEquals('This is a document conflict message (First, Second).',
    (DC As TDocumentConflict).AsString(True, False));
End;

Procedure TestTElementContainer.TestAddIssue;

Begin
  FElementContainer.AddIssue('This is a warning.', scNone, 1, 2, etWarning, FElementContainer);
  CheckEquals(1, FElementContainer.ElementCount);
  CheckEquals('Warnings', FElementContainer.Elements[1].AsString(False, False));
  CheckEquals('This is a warning.',
    FElementContainer.Elements[1].Elements[1].AsString(False, False));
End;

Procedure TestTElementContainer.TestAddTokens;

Var
  E: TElementContainer;

Begin
  E := TTestElementContainer.Create('Tmp', scNone, 0, 0, iiNone, Nil);
  Try
    E.AddToken('Hello');
    E.AddToken('Dave');
    E.AddToken('.');
    FElementContainer.AddTokens(E);
    CheckEquals(3, FElementContainer.TokenCount);
    CheckEquals('Hello', FElementContainer.Tokens[0].Token);
    CheckEquals('Dave', FElementContainer.Tokens[1].Token);
    CheckEquals('.', FElementContainer.Tokens[2].Token);
  Finally
    E.Free;
  End;
End;

Procedure TestTElementContainer.TestAssign;

Var
  E: TElementContainer;

Begin
  E := TTestElementContainer.Create('', scNone, 0, 0, iiNone, Nil);
  Try
    E.Assign(FElementContainer);
    CheckEquals(scPrivate, E.Scope);
    CheckEquals(12, E.Line);
    CheckEquals(23, E.Column);
    Check(E.Comment <> Nil, 'Comment is null');
  Finally
    E.Free;
  End;
End;

Procedure TestTElementContainer.TestAsString;

Begin
  CheckEquals('TestElement', FElementContainer.AsString(True, False));
End;

Procedure TestTElementContainer.TestCheckDocumentation;

Var
  boolCascade: Boolean;

Begin
  FElementContainer.CheckDocumentation(boolCascade);
  CheckEquals(0, FElementContainer.ElementCount);
End;

Procedure TestTElementContainer.TestCheckReferences;
Begin
  FElementContainer.Referenced := True;
  FElementContainer.CheckReferences;
  CheckEquals(0, FElementContainer.ElementCount);
  FElementContainer.Referenced := False;
  FElementContainer.CheckReferences;
  CheckEquals(1, FElementContainer.ElementCount);
End;

Procedure TestTElementContainer.TestComment;
Begin
  CheckEquals('This is a test comment.', FElementContainer.Comment.AsString(9999, False));
End;

Procedure TestTElementContainer.TestCreate;
Begin
  CheckEquals('TestElement', FElementContainer.Identifier);
  CheckEquals(scPrivate, FElementContainer.Scope);
  CheckEquals(12, FElementContainer.Line);
  CheckEquals(23, FElementContainer.Column);
  CheckEquals(BADIImageIndex(iiPublicConstant, scPrivate),
    FElementContainer.ImageIndexAdjustedForScope);
  Check(FElementContainer.Comment <> Nil, 'Comment is null');
End;

Procedure TestTElementContainer.TestDeleteElement;

Var
  E: TElementContainer;

Begin
  E := TTestElementContainer.Create('Tmp3', scNone, 0, 0, iiNone, Nil);
  FElementContainer.Add(E);
  E := TTestElementContainer.Create('Tmp2', scNone, 0, 0, iiNone, Nil);
  FElementContainer.Add(E);
  E := TTestElementContainer.Create('Tmp1', scNone, 0, 0, iiNone, Nil);
  FElementContainer.Add(E);
  CheckEquals(3, FElementContainer.ElementCount);
  FElementContainer.DeleteElement(2);
  CheckEquals(2, FElementContainer.ElementCount);
  CheckEquals('Tmp1', FElementContainer.Elements[1].Identifier);
  CheckEquals('Tmp3', FElementContainer.Elements[2].Identifier);
End;

Procedure TestTElementContainer.TestElementCount;

Var
  E: TElementContainer;

Begin
  CheckEquals(0, FElementContainer.ElementCount);
  E := TTestElementContainer.Create('Tmp3', scNone, 0, 0, iiNone, Nil);
  FElementContainer.Add(E);
  CheckEquals(1, FElementContainer.ElementCount);
  E := TTestElementContainer.Create('Tmp2', scNone, 0, 0, iiNone, Nil);
  FElementContainer.Add(E);
  CheckEquals(2, FElementContainer.ElementCount);
  E := TTestElementContainer.Create('Tmp1', scNone, 0, 0, iiNone, Nil);
  FElementContainer.Add(E);
  CheckEquals(3, FElementContainer.ElementCount);
End;

Procedure TestTElementContainer.TestElements;

Var
  E: TElementContainer;

Begin
  E := TTestElementContainer.Create('Tmp3', scNone, 0, 0, iiNone, Nil);
  FElementContainer.Add(E);
  E := TTestElementContainer.Create('Tmp2', scNone, 0, 0, iiNone, Nil);
  FElementContainer.Add(E);
  E := TTestElementContainer.Create('Tmp1', scNone, 0, 0, iiNone, Nil);
  FElementContainer.Add(E);
  CheckEquals('Tmp1', FElementContainer.Elements[1].Identifier);
  CheckEquals('Tmp2', FElementContainer.Elements[2].Identifier);
  CheckEquals('Tmp3', FElementContainer.Elements[3].Identifier);
End;

Procedure TestTElementContainer.TestFindElement;

Var
  E: TElementContainer;

Begin
  E := TTestElementContainer.Create('Tmp3', scNone, 0, 0, iiNone, Nil);
  FElementContainer.Add(E);
  E := TTestElementContainer.Create('Tmp2', scNone, 0, 0, iiNone, Nil);
  FElementContainer.Add(E);
  E := TTestElementContainer.Create('Tmp1', scNone, 0, 0, iiNone, Nil);
  FElementContainer.Add(E);
  Check(FElementContainer.FindElement('Tmp2') <> Nil, 'FindElement is null');
  Check(FElementContainer.FindElement('Tmp3', ftIdentifier) <> Nil, 'FindElement is null');
End;

Procedure TestTElementContainer.TestFindToken;

Begin
  FElementContainer.AddToken('Hello');
  FElementContainer.AddToken('Dave');
  FElementContainer.AddToken('.');
  CheckEquals(1, FElementContainer.FindToken('Dave'));
End;

Procedure TestTElementContainer.TestImageIndex;
Begin
  CheckEquals(iiPublicConstant, FElementContainer.ImageIndex);
End;

Procedure TestTElementContainer.TestImageIndexAdjustedForScope;

Begin
  CheckEquals(BADIImageIndex(iiPublicConstant, scPrivate), FElementContainer.ImageIndexAdjustedForScope);
End;

Procedure TestTElementContainer.TestParent;
Var
  E: TElementContainer;
Begin
  Check(FElementContainer.Parent = Nil, 'Parent is not null');
  E := TTestElementContainer.Create('Test', scNone, 1, 2, iiNone, Nil);
  FElementContainer.Add(E);
  Check(E.Parent = FElementContainer, 'Parent is not FElementContainer');
End;

Procedure TestTElementContainer.TestReferenced;
Begin
  CheckEquals(False, FElementContainer.Referenced);
  FElementContainer.Referenced := True;
  CheckEquals(True, FElementContainer.Referenced);
End;

Procedure TestTElementContainer.TestReferenceSection;

Var
  V: TLabelContainer;
  AToken: TTokenInfo;
  T: TElementContainer;

Begin
  V := TLabelContainer.Create(strVarsLabel, scNone, 0, 0, iiPublicVariablesLabel, Nil);
  FElementContainer.Add(V);
  T := TTestIdent.Create('Test', scPrivate, 1, 2, iiPublicVariable, Nil);
  V.Add(T);
  AToken := TTokenInfo.Create('Test', 0, 1, 2, 5, ttUnknown);
  Try
    CheckEquals(trUnknown, AToken.Reference);
    CheckEquals(False, T.Referenced);
    Check(FElementContainer.ReferenceSection(AToken, V));
    CheckEquals(trResolved, AToken.Reference);
    CheckEquals(True, T.Referenced);
  Finally
    AToken.Free;
  End;
End;

Procedure TestTElementContainer.TestReferenceSymbol;

Var
  AToken: TTokenInfo;

Begin
  AToken := TTokenInfo.Create('Hello', 0, 1, 2, 5, ttUnknown);
  Try
    CheckEquals(False, FElementContainer.ReferenceSymbol(AToken));
  Finally
    AToken.Free;
  End;
End;

Procedure TestTElementContainer.TestScope;
Begin
  CheckEquals(scPrivate, FElementContainer.Scope);
End;

Procedure TestTElementContainer.TestSorted;
Begin
  CheckEquals(True, FElementContainer.Sorted);
End;

Initialization
  RegisterTest('BaseLanguageModule Tests', TestTElementContainer.Suite);
End.
