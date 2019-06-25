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
Unit Test.BADI.XML.XMLDocType;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.XML.DocType;

Type
  //
  // Test Class for the TXMLDocType Class Methods.
  //
  TestTXMLDocType = Class(TExtendedTestCase)
  Strict Private
    FXMLDocType : TXMLDocType;
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
// Test Methods for Class TXMLDocType.
//
Procedure TestTXMLDocType.Setup;
Begin
  FXMLDocType := TXMLDocType.Create('!DOCTYPE', scNone, 12, 23, iiPublicObject, Nil);
End;

Procedure TestTXMLDocType.TearDown;

Begin
  FXMLDocType.Free;
End;

Procedure TestTXMLDocType.TestAsString;

Begin
  CheckEquals('!DOCTYPE', FXMLDocType.AsString(True, True));
End;

procedure TestTXMLDocType.TestCreate;
begin
  CheckEquals('!DOCTYPE', FXMLDocType.Identifier);
  CheckEquals(scNone, FXMLDocType.Scope);
  CheckEquals(12, FXMLDocType.Line);
  CheckEquals(23, FXMLDocType.Column);
  CheckEquals(BADIImageIndex(iiPublicObject, scNone), FXMLDocType.ImageIndexAdjustedForScope);
end;

Initialization
  RegisterTest('XML Module', TestTXMLDocType.Suite);
End.
