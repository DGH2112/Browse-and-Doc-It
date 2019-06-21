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
Unit Test.BADI.DFM.ObjectDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.DFM.ObjectDecl;

Type
  //
  // Test Class for the TDFMObject Class Methods.
  //
  TestTDFMObject = Class(TExtendedTestCase)
  Strict Private
    FDFMObject : TDFMObject;
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
// Test methods for the class TDFMObject.
//
Procedure TestTDFMObject.Setup;

Begin
  FDFMObject := TDFMObject.Create('Identifier', scPublic, 12, 23, iiPublicObject, Nil);
  FDFMObject.AddToken('TfrmMyForm');
End;

Procedure TestTDFMObject.TearDown;

Begin
  FDFMObject.Free;
End;

Procedure TestTDFMObject.TestAsString;

Begin
  Checkequals('Object Identifier : TfrmMyForm', FDFMObject.AsString(True, True));
End;

procedure TestTDFMObject.TestCreate;
begin
  CheckEquals('Identifier', FDFMObject.Identifier);
  CheckEquals(scPublic, FDFMObject.Scope);
  CheckEquals(12, FDFMObject.Line);
  CheckEquals(23, FDFMObject.Column);
  CheckEquals(BADIImageIndex(iiPublicObject, scPublic), FDFMObject.ImageIndexAdjustedForScope);
end;

Initialization
  RegisterTest('DFM Module', TestTDFMObject.Suite);
End.
