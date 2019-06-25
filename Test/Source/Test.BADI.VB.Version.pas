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
Unit Test.BADI.VB.Version;

Interface

uses
  Test.BADI.Base.Module,
  BADI.VB.Version;

Type
  //
  // Test Class for the TVBVersion Class Methods.
  //
  TestTVBVersion = Class(TExtendedTestCase)
  Strict Private
    FVBVersion : TVBVersion;
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
// Test methods for the class TVBVersion.
//
Procedure TestTVBVersion.Setup;

Begin
  FVBVersion := TVBVersion.Create('Version', scPublic, 12, 23, iiUsesItem, Nil);
End;

Procedure TestTVBVersion.TearDown;

Begin
  FVBVersion.Free;
End;

Procedure TestTVBVersion.TestAsString;

Begin
  CheckEquals('Version', FVBVersion.AsString(True, True));
  FVBVersion.AddToken('1.0');
  FVBVersion.AddToken('Class');
  CheckEquals('Version 1.0 Class', FVBVersion.AsString(True, True));
End;

procedure TestTVBVersion.TestCreate;
begin
  CheckEquals('Version', FVBVersion.Identifier);
  CheckEquals(scPublic, FVBVersion.Scope);
  CheckEquals(12, FVBVersion.Line);
  CheckEquals(23, FVBVersion.Column);
  CheckEquals(BADIImageIndex(iiUsesItem, scPublic), FVBVersion.ImageIndexAdjustedForScope);
  Check(Nil = FVBVersion.Comment);
end;

initialization
  RegisterTest('VB Module Tests', TestTVBVersion.Suite);
End.
