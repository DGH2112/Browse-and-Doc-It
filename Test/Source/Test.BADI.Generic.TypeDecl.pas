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
Unit Test.BADI.Generic.TypeDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Generic.TypeDecl;

Type
  TestTGenericTypeDecl = Class(TExtendedTestCase)
  Strict Private
    FGenericTypeDecl: TGenericTypeDecl;
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

Procedure TestTGenericTypeDecl.SetUp;
Begin
  FGenericTypeDecl := TTestGenericTypeDecl.Create('MyType', scProtected, 23, 34,
    iiPublicType, Nil);
End;

Procedure TestTGenericTypeDecl.TearDown;
Begin
  FGenericTypeDecl.Free;
  FGenericTypeDecl := Nil;
End;

Procedure TestTGenericTypeDecl.TestCheckDocumentation;
Var
  boolCascade: Boolean;
  C: TComment;
Begin
  FGenericTypeDecl.CheckDocumentation(boolCascade);
  CheckEquals(1, FGenericTypeDecl.ElementCount);
  CheckEquals('1) Type ''MyType'' is undocumented.', FGenericTypeDecl.DocConflict(1));
  FGenericTypeDecl.DeleteDocumentConflicts;
  C := TComment.Create('This is a comment.', 0, 0, 0);
  Try
    FGenericTypeDecl.Comment := C;
    FGenericTypeDecl.CheckDocumentation(boolCascade);
    CheckEquals(0, FGenericTypeDecl.ElementCount, FGenericTypeDecl.DocConflict(1));
  Finally
    C.Free;
  End;
End;

Procedure TestTGenericTypeDecl.TestCreate;
Begin
  CheckEquals('MyType', FGenericTypeDecl.Identifier);
  CheckEquals(scProtected, FGenericTypeDecl.Scope);
  CheckEquals(23, FGenericTypeDecl.Line);
  CheckEquals(34, FGenericTypeDecl.Column);
  CheckEquals(iiPublicType, FGenericTypeDecl.ImageIndex);
  CheckEquals(BADIImageIndex(iiPublicType, scProtected), FGenericTypeDecl.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('BaseLanguageModule Tests', TestTGenericTypeDecl.Suite);
End.
