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
Unit Test.BADI.XML.XMLElemDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.XML.XMLElemDecl;

Type
  //
  // Test Class for the TXMLElemDecl Class Methods.
  //
  TestTXMLElemDecl = Class(TExtendedTestCase)
  Strict Private
    FXMLElemDecl: TXMLElemDecl;
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
// Test Methods for Class TXMLElemDecl.
//
Procedure TestTXMLElemDecl.SetUp;
Begin
  FXMLElemDecl := TXMLElemDecl.Create('!ELEMENT', scNone, 12, 23, iiPublicObject, Nil);
  FXMLElemDecl.AddToken('Element');
End;

Procedure TestTXMLElemDecl.TearDown;

Begin
  FXMLElemDecl.Free;
End;

Procedure TestTXMLElemDecl.TestAsString;

Begin
  CheckEquals('!ELEMENT Element', FXMLElemDecl.AsString(True, True));
End;

Procedure TestTXMLElemDecl.TestCreate;
Begin
  CheckEquals('!ELEMENT', FXMLElemDecl.Identifier);
  CheckEquals(scNone, FXMLElemDecl.Scope);
  CheckEquals(12, FXMLElemDecl.Line);
  CheckEquals(23, FXMLElemDecl.Column);
  CheckEquals(
  BADIImageIndex(iiPublicObject, scNone), FXMLElemDecl.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('XML Module', TestTXMLElemDecl.Suite);
End.
