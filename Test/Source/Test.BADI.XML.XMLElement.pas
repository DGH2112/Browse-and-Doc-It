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
Unit Test.BADI.XML.XMLElement;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.XML.XMLElement;

Type
  //
  // Test Class for the TXMLElement Class Methods.
  //
  TestTXMLElement = Class(TExtendedTestCase)
  Strict Private
    FXMLElement : TXMLElement;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestAsString;
    Procedure TestCreate;
    Procedure TestGetName;
  End;


Implementation

Uses
  BADI.Types,
  BADI.Functions;

//
// Test Methods for Class TXMLElement.
//
Procedure TestTXMLElement.Setup;
Begin
  FXMLElement := TXMLElement.Create('xml', scNone, 12, 23, iiPublicObject, Nil);
End;

Procedure TestTXMLElement.TearDown;

Begin
  FXMLElement.Free;
End;

Procedure TestTXMLElement.TestAsString;

Begin
  CheckEquals('<xml></xml>', FXMLElement.AsString(True, True));
End;

Procedure TestTXMLElement.TestCreate;

Begin
  CheckEquals('xml', FXMLElement.Identifier);
  CheckEquals(scNone, FXMLElement.Scope);
  CheckEquals(12, FXMLElement.Line);
  CheckEquals(23, FXMLElement.Column);
  CheckEquals(BADIImageIndex(iiPublicObject, scNone), FXMLElement.ImageIndexAdjustedForScope);
End;

Procedure TestTXMLElement.TestGetName;

Begin
  CheckEquals('xml:0012:0023', FXMLElement.GetName);
End;

Initialization
  RegisterTest('XML Module', TestTXMLElement.Suite);
End.
