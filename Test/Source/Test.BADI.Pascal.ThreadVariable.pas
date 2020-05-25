(**
  
  This module contains DUnit test for the Browse and Doc It code.

  @Author  David Hoyle
  @Version 1.001
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
Unit Test.BADI.Pascal.ThreadVariable;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.ThreadVariableDecl;

Type
  TestTThreadVar = Class(TExtendedTestCase)
  Strict Private
    FThreadVar: TThreadVar;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestCheckDocumentation;
  End;
  // Test methods for class TField

Implementation

Uses
  BADI.Types,
  BADI.Comment,
  BADI.Functions;

Procedure TestTThreadVar.SetUp;
Begin
  FThreadVar := TThreadVar.Create('MyThreadVar', scPrivate, 12, 23, iiPublicThreadVar, Nil);
End;

Procedure TestTThreadVar.TearDown;
Begin
  FThreadVar.Free;
  FThreadVar := Nil;
End;

Procedure TestTThreadVar.TestCheckDocumentation;
Var
  boolCascade: Boolean;
  C: TComment;
Begin
  CheckEquals(0, FThreadVar.ElementCount);
  FThreadVar.CheckDocumentation(boolCascade);
  CheckEquals(1, FThreadVar.ElementCount);
  CheckEquals('1) Thread variable ''MyThreadVar'' is undocumented.', FThreadVar.DocConflict(1));
  FThreadVar.DeleteDocumentConflicts;
  C := TComment.Create('This is a comment for the threadvar.', 0, 0, 0);
  Try
    FThreadVar.Comment := C;
    FThreadVar.CheckDocumentation(boolCascade);
    CheckEquals(0, FThreadVar.ElementCount);
  Finally
    C.Free;
  End;
End;

Procedure TestTThreadVar.TestCreate;
Begin
  CheckEquals('MyThreadVar', FThreadVar.Identifier);
  CheckEquals(scPrivate, FThreadVar.Scope);
  CheckEquals(12, FThreadVar.Line);
  CheckEquals(23, FThreadVar.Column);
  CheckEquals(iiPublicThreadVar, FThreadVar.ImageIndex);
  CheckEquals(BADIImageIndex(iiPublicThreadVar, scPrivate), FThreadVar.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('PascalModule Tests', TestTThreadVar.Suite);
End.
