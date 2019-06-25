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
Unit Test.BADI.Pascal.ObjectDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.ObjectDecl;

Type
  TestTObjectDecl = Class(TExtendedTestCase)
  Strict Private
    FObjectDecl: TObjectDecl;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestCheckDocumentation;
    Procedure TestAsString;
    Procedure TestReferenceSymbol;
  End;

Implementation

Uses
  BADI.Types,
  BADI.Comment,
  BADI.TokenInfo,
  BADI.Functions;

Procedure TestTObjectDecl.SetUp;
Begin
  FObjectDecl := TObjectDecl.Create('MyObject', scProtected, 12, 23, iiPublicObject, Nil);
End;

Procedure TestTObjectDecl.TearDown;
Begin
  FObjectDecl.Free;
  FObjectDecl := Nil;
End;

Procedure TestTObjectDecl.TestCheckDocumentation;
Var
  boolCascade: Boolean;
  C: TComment;

Begin
  CheckEquals(0, FObjectDecl.ElementCount);
  FObjectDecl.CheckDocumentation(boolCascade);
  CheckEquals(1, FObjectDecl.ElementCount);
  CheckEquals('1) Object type ''MyObject'' is undocumented.', FObjectDecl.DocConflict(1));
  FObjectDecl.DeleteDocumentConflicts;
  C := TComment.Create('This is a comment for the object.', 0, 0);
  Try
    FObjectDecl.Comment := C;
    FObjectDecl.CheckDocumentation(boolCascade);
    CheckEquals(0, FObjectDecl.ElementCount);
  Finally
    C.Free;
  End;
End;

Procedure TestTObjectDecl.TestCreate;
Begin
  CheckEquals('MyObject', FObjectDecl.Identifier);
  CheckEquals(scProtected, FObjectDecl.Scope);
  CheckEquals(12, FObjectDecl.Line);
  CheckEquals(23, FObjectDecl.Column);
  CheckEquals(iiPublicObject, FObjectDecl.ImageIndex);
  CheckEquals(BADIImageIndex(iiPublicObject, scProtected), FObjectDecl.ImageIndexAdjustedForScope);
End;

Procedure TestTObjectDecl.TestAsString;
Begin
  CheckEquals('MyObject = Object', FObjectDecl.AsString(True, False));
End;

Procedure TestTObjectDecl.TestReferenceSymbol;
Var
  AToken: TTokenInfo;
Begin
  AToken := TTokenInfo.Create('Hello', 0, 0, 0, 5, ttIdentifier);
  Try
    CheckEquals(False, FObjectDecl.ReferenceSymbol(AToken));
    // : @todo Requires more tests.
  Finally
    AToken.Free;
  End;
End;

Initialization
  RegisterTest('PascalModule Tests', TestTObjectDecl.Suite);
End.
