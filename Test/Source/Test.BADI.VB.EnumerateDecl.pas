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
Unit Test.BADI.VB.EnumerateDecl;

Interface

Uses
  Test.BADI.Base.Module,
  BADI.VB.EnumerateDecl;

Type
  //
  // Test Class for the TVBEnumerateDecl Class Methods.
  //
  TestTVBEnumerateDecl = Class(TExtendedTestCase)
  Strict Private
    FVBEnumerateDecl : TVBEnumerateDecl;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestAsString;
  End;

Implementation

uses
  BADI.Types,
  BADI.Functions,
  TestFramework;

//
// Test methods for the class TVBEnumerateDecl.
//
Procedure TestTVBEnumerateDecl.Setup;

Begin
  FVBEnumerateDecl := TVBEnumerateDecl.Create('MyEnumerate', scPrivate, 12, 23,
    iiPublicType, Nil);
End;

Procedure TestTVBEnumerateDecl.TearDown;

Begin
  FVBEnumerateDecl.Free;
End;

Procedure TestTVBEnumerateDecl.TestAsString;

Begin
  CheckEquals('Enum MyEnumerate', FVBEnumerateDecl.AsString(true, True));
End;

procedure TestTVBEnumerateDecl.TestCreate;
begin
  CheckEquals('MyEnumerate', FVBEnumerateDecl.Identifier);
  CheckEquals(scPrivate, FVBEnumerateDecl.Scope);
  CheckEquals(12, FVBEnumerateDecl.Line);
  CheckEquals(23, FVBEnumerateDecl.Column);
  CheckEquals(BADIImageIndex(iiPublicType, scPrivate), FVBEnumerateDecl.ImageIndexAdjustedForScope);
  Check(Nil = FVBEnumerateDecl.Comment);
end;

initialization
  RegisterTest('VB Module Tests', TestTVBEnumerateDecl.Suite);
End.
