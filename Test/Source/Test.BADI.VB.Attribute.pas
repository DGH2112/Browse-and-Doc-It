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
Unit Test.BADI.VB.Attribute;

Interface

uses
  Test.BADI.Base.Module,
  BADI.VB.Attribute;

Type
  //
  // Test Class for the TVBAttribute Class Methods.
  //
  TestTVBAttribute = Class(TExtendedTestCase)
  Strict Private
    FVBAttribute : TVBAttribute;
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
// Test methods for the class TVBAttribute.
//
Procedure TestTVBAttribute.Setup;

Begin
  FVBAttribute := TVBAttribute.Create('Attribute', scPublic, 12, 23, iiUsesItem,
    Nil);
End;

Procedure TestTVBAttribute.TearDown;

Begin
  FVBAttribute.Free;
End;

Procedure TestTVBAttribute.TestAsString;

Begin
  CheckEquals('Attribute', FVBAttribute.AsString(True, True));
  FVBAttribute.AddToken('iThing');
  FVBAttribute.AddToken('=');
  FVBAttribute.AddToken('1');
  CheckEquals('Attribute iThing = 1', FVBAttribute.AsString(True, True));
End;

procedure TestTVBAttribute.TestCreate;
begin
  CheckEquals('Attribute', FVBAttribute.Identifier);
  CheckEquals(scPublic, FVBAttribute.Scope);
  CheckEquals(12, FVBAttribute.Line);
  CheckEquals(23, FVBAttribute.Column);
  CheckEquals(BADIImageIndex(iiUsesItem, scPublic), FVBAttribute.ImageIndexAdjustedForScope);
  Check(Nil = FVBAttribute.Comment);
end;

initialization
  RegisterTest('VB Module Tests', TestTVBAttribute.Suite);
End.
