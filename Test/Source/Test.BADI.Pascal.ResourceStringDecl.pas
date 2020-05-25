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
Unit Test.BADI.Pascal.ResourceStringDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.ResourceStringDecl;

Type
  TestTResourceString = Class(TExtendedTestCase)
  Strict Private
    FResourceString: TResourceString;
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

Procedure TestTResourceString.SetUp;
Begin
  FResourceString := TResourceString.Create('MyResourceString', scPublic, 12, 23,
    iiPublicResourceString, Nil);
  FResourceString.AddToken('''This is a string literal.''');
End;

Procedure TestTResourceString.TearDown;
Begin
  FResourceString.Free;
  FResourceString := Nil;
End;

Procedure TestTResourceString.TestCheckDocumentation;
Var
  boolCascade: Boolean;
  C: TComment;
Begin
  CheckEquals(0, FResourceString.ElementCount);
  FResourceString.CheckDocumentation(boolCascade);
  CheckEquals(1, FResourceString.ElementCount);
  CheckEquals('1) Resource string ''MyResourceString'' is undocumented.',
    FResourceString.DocConflict(1));
  FResourceString.DeleteDocumentConflicts;
  C := TComment.Create('This is a comment for the resource string.', 0, 0, 0);
  Try
    FResourceString.Comment := C;
    FResourceString.CheckDocumentation(boolCascade);
    CheckEquals(0, FResourceString.ElementCount);
  Finally
    C.Free;
  End;
End;

Procedure TestTResourceString.TestCreate;
Begin
  CheckEquals('MyResourceString', FResourceString.Identifier);
  CheckEquals(scPublic, FResourceString.Scope);
  CheckEquals(12, FResourceString.Line);
  CheckEquals(23, FResourceString.Column);
  CheckEquals(BADIImageIndex(iiPublicResourceString, scPublic), FResourceString.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('PascalModule Tests', TestTResourceString.Suite);
End.
