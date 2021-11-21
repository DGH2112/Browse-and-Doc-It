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
Unit Test.BADI.Pascal.DispInterfaceDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.DispInterfaceDecl;

Type
  TestTDispInterfaceDecl = Class(TExtendedTestCase)
  Strict Private
    FDispInterfaceDecl: TDispInterfaceDecl;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestCheckDocumentation;
    Procedure TestAsString;
  End;

Implementation

Uses
  BADI.Types,
  BADI.Comment,
  BADI.TokenInfo,
  BADI.Functions;

Procedure TestTDispInterfaceDecl.SetUp;
Begin
  FDispInterfaceDecl := TDispInterfaceDecl.Create('MyDispInterface', scPrivate, 12, 23,
    iiPublicDispInterface, Nil);
End;

Procedure TestTDispInterfaceDecl.TearDown;
Begin
  FDispInterfaceDecl.Free;
  FDispInterfaceDecl := Nil;
End;

Procedure TestTDispInterfaceDecl.TestCheckDocumentation;
Var
  boolCascade: Boolean;
  C: TComment;
Begin
  CheckEquals(0, FDispInterfaceDecl.ElementCount);
  FDispInterfaceDecl.CheckDocumentation(boolCascade);
  CheckEquals(1, FDispInterfaceDecl.ElementCount);
  CheckEquals('1) DispInterface type ''MyDispInterface'' is undocumented.',
    FDispInterfaceDecl.DocConflict(1));
  FDispInterfaceDecl.DeleteDocumentConflicts;
  C := TComment.Create('This is a comment for the DispInterface.', 0, 0, 0);
  Try
    FDispInterfaceDecl.Comment := C;
    FDispInterfaceDecl.CheckDocumentation(boolCascade);
    CheckEquals(0, FDispInterfaceDecl.ElementCount);
  Finally
    C.Free;
  End;
End;

Procedure TestTDispInterfaceDecl.TestCreate;
Begin
  CheckEquals('MyDispInterface', FDispInterfaceDecl.Identifier);
  CheckEquals(scPrivate, FDispInterfaceDecl.Scope);
  CheckEquals(12, FDispInterfaceDecl.Line);
  CheckEquals(23, FDispInterfaceDecl.Column);
  CheckEquals(iiPublicDispInterface, FDispInterfaceDecl.ImageIndex);
  CheckEquals(BADIImageIndex(iiPublicDispInterface, scPrivate), FDispInterfaceDecl.ImageIndexAdjustedForScope);
End;

Procedure TestTDispInterfaceDecl.TestAsString;
Var
  H: TTokenInfo;
Begin
  CheckEquals('MyDispInterface = DispInterface', FDispInterfaceDecl.AsString(True, False));
  H := TTokenInfo.Create('IUnknown', 0, 0, 0, 7, ttIdentifier);
  Try
    FDispInterfaceDecl.Heritage.Add(H, scNone, iiNone, Nil);
    CheckEquals('MyDispInterface = DispInterface(IUnknown)',
      FDispInterfaceDecl.AsString(True, False));
  Finally
    H.Free;
  End;
End;

Initialization
  RegisterTest('PascalModule Tests', TestTDispInterfaceDecl.Suite);
End.
