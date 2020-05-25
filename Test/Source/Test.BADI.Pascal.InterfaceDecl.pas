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
Unit Test.BADI.Pascal.InterfaceDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.InterfaceDecl;

Type
  TestTInterfaceDecl = Class(TExtendedTestCase)
  Strict Private
    FInterfaceDecl: TInterfaceDecl;
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

Procedure TestTInterfaceDecl.SetUp;
Begin
  FInterfaceDecl := TInterfaceDecl.Create('MyInterface', scPublished, 12, 23,
    iiPublicInterface, Nil);
End;

Procedure TestTInterfaceDecl.TearDown;
Begin
  FInterfaceDecl.Free;
  FInterfaceDecl := Nil;
End;

Procedure TestTInterfaceDecl.TestCheckDocumentation;
Var
  boolCascade: Boolean;
  C: TComment;
Begin
  CheckEquals(0, FInterfaceDecl.ElementCount);
  FInterfaceDecl.CheckDocumentation(boolCascade);
  CheckEquals(1, FInterfaceDecl.ElementCount);
  CheckEquals('1) Interface type ''MyInterface'' is undocumented.', FInterfaceDecl.DocConflict(1));
  FInterfaceDecl.DeleteDocumentConflicts;
  C := TComment.Create('This is a comment for the Interface.', 0, 0, 0);
  Try
    FInterfaceDecl.Comment := C;
    FInterfaceDecl.CheckDocumentation(boolCascade);
    CheckEquals(0, FInterfaceDecl.ElementCount);
  Finally
    C.Free;
  End;
End;

Procedure TestTInterfaceDecl.TestCreate;
Begin
  CheckEquals('MyInterface', FInterfaceDecl.Identifier);
  CheckEquals(scPublished, FInterfaceDecl.Scope);
  CheckEquals(12, FInterfaceDecl.Line);
  CheckEquals(23, FInterfaceDecl.Column);
  CheckEquals(iiPublicInterface, FInterfaceDecl.ImageIndex);
  CheckEquals(BADIImageIndex(iiPublicInterface, scPublished), FInterfaceDecl.ImageIndexAdjustedForScope);
End;

Procedure TestTInterfaceDecl.TestAsString;
Var
  H: TTokenInfo;
Begin
  CheckEquals('MyInterface = Interface', FInterfaceDecl.AsString(True, False));
  H := TTokenInfo.Create('IUnknown', 0, 0, 0, 7, ttIdentifier);
  Try
    FInterfaceDecl.Heritage.Add(H, scNone, iiNone, Nil);
    CheckEquals('MyInterface = Interface(IUnknown)', FInterfaceDecl.AsString(True, False));
  Finally
    H.Free;
  End;
End;

Initialization
  RegisterTest('PascalModule Tests', TestTInterfaceDecl.Suite);
End.
