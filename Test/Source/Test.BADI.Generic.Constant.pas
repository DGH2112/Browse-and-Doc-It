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
Unit Test.BADI.Generic.Constant;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Generic.Constant;

Type
  TestTGenericConstant = Class(TExtendedTestCase)
  Strict Private
    FGenericConstant: TGenericConstant;
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

Procedure TestTGenericConstant.SetUp;
Begin
  FGenericConstant := TTestGenericConstant.Create('MyConstant', scPublished, 23,
    34, iiPublicConstant, Nil);
End;

Procedure TestTGenericConstant.TearDown;
Begin
  FGenericConstant.Free;
  FGenericConstant := Nil;
End;

Procedure TestTGenericConstant.TestCheckDocumentation;
Var
  boolCascade: Boolean;
  C: TComment;
Begin
  FGenericConstant.CheckDocumentation(boolCascade);
  CheckEquals(1, FGenericConstant.ElementCount);
  CheckEquals('1) Constant ''MyConstant'' is undocumented.', FGenericConstant.DocConflict(1));
  FGenericConstant.DeleteDocumentConflicts;
  C := TComment.Create('This is a comment.', 0, 0);
  Try
    FGenericConstant.Comment := C;
    FGenericConstant.CheckDocumentation(boolCascade);
    CheckEquals(0, FGenericConstant.ElementCount, FGenericConstant.DocConflict(1));
  Finally
    C.Free;
  End;
End;

Procedure TestTGenericConstant.TestCreate;
Begin
  CheckEquals('MyConstant', FGenericConstant.Identifier);
  CheckEquals(scPublished, FGenericConstant.Scope);
  CheckEquals(23, FGenericConstant.Line);
  CheckEquals(34, FGenericConstant.Column);
  CheckEquals(iiPublicConstant, FGenericConstant.ImageIndex);
  CheckEquals(BADIImageIndex(iiPublicConstant, scPublished), FGenericConstant.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('BaseLanguageModule Tests', TestTGenericConstant.Suite);
End.
