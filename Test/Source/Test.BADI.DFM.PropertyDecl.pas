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
Unit Test.BADI.DFM.PropertyDecl;

Interface

Uses
  TestFramework,
  Test.BADI.base.Module,
  BADI.DFM.PropertyDecl;

Type
  //
  // Test Class for the TDFMProperty Class Methods.
  //
  TestTDFMProperty = Class(TExtendedTestCase)
  Strict Private
    FDFMProperty: TDFMProperty;
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
  BADI.Functions;

//
// Test methods for the class TDFMProperty.
//
Procedure TestTDFMProperty.SetUp;

Begin
  FDFMProperty := TDFMProperty.Create('MyIdentifier', scPublic, 12, 23, iiPublicProperty, Nil);
  FDFMProperty.AddToken('AnIdentifier');
End;

Procedure TestTDFMProperty.TearDown;

Begin
  FDFMProperty.Free;
End;

Procedure TestTDFMProperty.TestAsString;

Begin
  CheckEquals('MyIdentifier = AnIdentifier', FDFMProperty.AsString(True, True));
End;

Procedure TestTDFMProperty.TestCreate;
Begin
  CheckEquals('MyIdentifier', FDFMProperty.Identifier);
  CheckEquals(scPublic, FDFMProperty.Scope);
  CheckEquals(12, FDFMProperty.Line);
  CheckEquals(23, FDFMProperty.Column);
  CheckEquals(BADIImageIndex(iiPublicProperty, scPublic), FDFMProperty.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('DFM Module', TestTDFMProperty.Suite);
End.
