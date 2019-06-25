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
Unit Test.BADI.VB.FieldDecl;

Interface

uses
  Test.BADI.Base.Module,
  BADI.VB.FieldDecl;

Type
  //
  // Test Class for the TVBField Class Methods.
  //
  TestTVBField = Class(TExtendedTestCase)
  Strict Private
    FVBField : TVBField;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestAsString;
  End;

Implementation

Uses
  BADI.Types,
  BADI.Functions,
  TestFramework;

//
// Test methods for the class TVBField.
//
Procedure TestTVBField.Setup;

Begin
  FVBField := TVBField.Create('MyField', scPublic, 12, 23, iiPublicField, Nil);
End;

Procedure TestTVBField.TearDown;

Begin
  FVBField.Free;
End;

Procedure TestTVBField.TestAsString;

Begin
  CheckEquals('MyField', FVBField.AsString(True, True));
  FVBField.AddToken('Long');
  CheckEquals('MyField As Long', FVBField.AsString(True, True));
End;

procedure TestTVBField.TestCreate;
begin
  CheckEquals('MyField', FVBField.Identifier);
  CheckEquals(scPublic, FVBField.Scope);
  CheckEquals(12, FVBField.Line);
  CheckEquals(23, FVBField.Column);
  CheckEquals(BADIImageIndex(iiPublicField, scPublic), FVBField.ImageIndexAdjustedForScope);
  Check(Nil = FVBField.Comment);
end;

initialization
  RegisterTest('VB Module Tests', TestTVBField.Suite);
End.
