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
Unit Test.BADI.Pascal.ExportsItem;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.ExportsItem;

Type
  TestTExportsItem = Class(TExtendedTestCase)
  Strict Private
    FExportsItem: TExportsItem;
  Public
    Procedure TestCreate;
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestAsString;
  End;

Implementation

Uses
  BADI.Types;

Procedure TestTExportsItem.SetUp;
Begin
  FExportsItem := TExportsItem.Create('MyMethod', scPublic, 12, 23, iiPublicExportedFunction, Nil);
End;

Procedure TestTExportsItem.TearDown;
Begin
  FExportsItem.Free;
  FExportsItem := Nil;
End;

Procedure TestTExportsItem.TestAsString;
Begin
  Checkequals('MyMethod', FExportsItem.AsString(True, False));
End;

Procedure TestTExportsItem.TestCreate;
Begin
  Checkequals('MyMethod', FExportsItem.Identifier);
  Checkequals(scPublic, FExportsItem.Scope);
  Checkequals(12, FExportsItem.Line);
  Checkequals(23, FExportsItem.Column);
  Checkequals(iiPublicExportedFunction, FExportsItem.ImageIndex);
  Checkequals(iiPublicExportedFunction, FExportsItem.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('PascalModule Tests', TestTExportsItem.Suite);
End.
