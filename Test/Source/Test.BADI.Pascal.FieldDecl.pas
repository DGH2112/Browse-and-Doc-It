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
Unit Test.BADI.Pascal.FieldDecl;

Interface

Uses
  TestFramework,
  Test.BADI.base.Module,
  BADI.Pascal.FieldDecl;

Type
  TestTField = Class(TExtendedTestCase)
  Strict Private
    FField: TField;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestAsString;
    Procedure TestCheckDocumentation;
  End;

Implementation

Uses
  BADI.Types,
  BADI.Comment,
  BADI.Functions;

Procedure TestTField.SetUp;
Begin
  FField := TField.Create('MyField', scPublic, 12, 23, iiPublicField, Nil);
  FField.AddToken('Integer');
End;

Procedure TestTField.TearDown;
Begin
  FField.Free;
  FField := Nil;
End;

Procedure TestTField.TestAsString;
Begin
  CheckEquals('MyField : Integer', FField.AsString(True, False));
End;

Procedure TestTField.TestCheckDocumentation;
Var
  boolCascade: Boolean;
  C: TComment;
Begin
  CheckEquals(0, FField.ElementCount);
  FField.CheckDocumentation(boolCascade);
  CheckEquals(1, FField.ElementCount);
  CheckEquals('1) Field ''MyField'' is undocumented.', FField.DocConflict(1));
  FField.DeleteDocumentConflicts;
  C := TComment.Create('This is a comment for the field.', 0, 0);
  Try
    FField.Comment := C;
    FField.CheckDocumentation(boolCascade);
    CheckEquals(0, FField.ElementCount);
  Finally
    C.Free;
  End;
End;

Procedure TestTField.TestCreate;
Begin
  CheckEquals('MyField', FField.Identifier);
  CheckEquals(scPublic, FField.Scope);
  CheckEquals(12, FField.Line);
  CheckEquals(23, FField.Column);
  CheckEquals(BADIImageIndex(iiPublicField, scPublic), FField.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('PascalModule Tests', TestTField.Suite);
End.
