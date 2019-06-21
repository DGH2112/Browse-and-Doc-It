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
Unit Test.BADI.VB.EnumIdent;

Interface

uses
  Test.BADI.Base.Module,
  BADI.VB.EnumIdent;

Type
  //
  // Test Class for the TVBEnumIdent Class Methods.
  //
  TestTVBEnumIdent = Class(TExtendedTestCase)
  Strict Private
    FVBEnumIdent : TVBEnumIdent;
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
// Test methods for the class TVBEnumIdent.
//
Procedure TestTVBEnumIdent.Setup;

Begin
  FVBEnumIdent := TVBEnumIdent.Create('MyEnumIdent', scPublic, 12, 23,
    iiUsesItem, Nil);
End;

Procedure TestTVBEnumIdent.TearDown;

Begin
  FVBEnumIdent.Free;
End;

Procedure TestTVBEnumIdent.TestAsString;

Begin
  CheckEquals('MyEnumIdent', FVBEnumIdent.AsString(true, True));
  FVBEnumIdent.AddToken('1');
  CheckEquals('MyEnumIdent = 1', FVBEnumIdent.AsString(true, True));
End;

procedure TestTVBEnumIdent.TestCreate;
begin
  CheckEquals('MyEnumIdent', FVBEnumIdent.Identifier);
  CheckEquals(scPublic, FVBEnumIdent.Scope);
  CheckEquals(12, FVBEnumIdent.Line);
  CheckEquals(23, FVBEnumIdent.Column);
  CheckEquals(BADIImageIndex(iiUsesItem, scPublic), FVBEnumIdent.ImageIndexAdjustedForScope);
  Check(Nil = FVBEnumIdent.Comment);
end;

initialization
  RegisterTest('VB Module Tests', TestTVBEnumIdent.Suite);
End.
