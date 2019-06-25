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
Unit Test.BADI.Generic.Variable;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Generic.Variable;

Type
  TestTGenericVariable = Class(TExtendedTestCase)
  Strict Private
    FGenericVariable: TGenericVariable;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestCheckDocumentation;
  End;

Implementation

Uses
  BADI.Types,
  BADI.Comment,
  BADI.Functions;

Procedure TestTGenericVariable.SetUp;
Begin
  FGenericVariable := TTestGenericVariable.Create('MyVariable', scPublic, 23,
    34, iiPublicConstant, Nil);
End;

Procedure TestTGenericVariable.TearDown;
Begin
  FGenericVariable.Free;
  FGenericVariable := Nil;
End;

Procedure TestTGenericVariable.TestCheckDocumentation;
Var
  boolCascade: Boolean;
  C: TComment;
Begin
  FGenericVariable.CheckDocumentation(boolCascade);
  CheckEquals(1, FGenericVariable.ElementCount);
  CheckEquals('1) Variable ''MyVariable'' is undocumented.', FGenericVariable.DocConflict(1));
  FGenericVariable.DeleteDocumentConflicts;
  C := TComment.Create('This is a comment.', 0, 0);
  Try
    FGenericVariable.Comment := C;
    FGenericVariable.CheckDocumentation(boolCascade);
    CheckEquals(0, FGenericVariable.ElementCount, FGenericVariable.DocConflict(1));
  Finally
    C.Free;
  End;
End;

Procedure TestTGenericVariable.TestCreate;
Begin
  CheckEquals('MyVariable', FGenericVariable.Identifier);
  CheckEquals(scPublic, FGenericVariable.Scope);
  CheckEquals(23, FGenericVariable.Line);
  CheckEquals(34, FGenericVariable.Column);
  CheckEquals(BADIImageIndex(iiPublicConstant, scPublic), FGenericVariable.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('BaseLanguageModule Tests', TestTGenericVariable.Suite);
End.
