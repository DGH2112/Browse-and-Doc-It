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
Unit Test.BADI.XML.XMLDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.XML.XMLDecl;

Type
  //
  // Test Class for the TXMLDecl Class Methods.
  //
  TestTXMLDecl = Class(TExtendedTestCase)
  Strict Private
    FXMLDecl : TXMLDecl;
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
// Test Methods for Class TXMLDecl.
//
Procedure TestTXMLDecl.Setup;
Begin
  FXMLDecl := TXMLDecl.Create('xml', scNone, 12, 23, iiPublicType, Nil);
End;

Procedure TestTXMLDecl.TearDown;

Begin
  FXMLDecl.Free;
End;

Procedure TestTXMLDecl.TestAsString;

Begin
  CheckEquals('xml', FXMLDecl.AsString(True, True));
End;

procedure TestTXMLDecl.TestCreate;
begin
  CheckEquals('xml', FXMLDecl.Identifier);
  CheckEquals(scNone, FXMLDecl.Scope);
  CheckEquals(12, FXMLDecl.Line);
  CheckEquals(23, FXMLDecl.Column);
  CheckEquals(BADIImageIndex(iiPublicType, scNone), FXMLDecl.ImageIndexAdjustedForScope);
end;

Initialization
  RegisterTest('XML Module', TestTXMLDecl.Suite);
End.
