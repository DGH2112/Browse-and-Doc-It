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
Unit Test.BADI.Pascal.TypeDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.TypeDecl;

Type
  TestTTypes = Class(TExtendedTestCase)
  Strict Private
    FTypes: TTypes;
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

Procedure TestTTypes.SetUp;

Begin
  FTypes := TTypes.Create('MyType', scPrivate, 12, 23, iiPublicType, Nil);
End;

Procedure TestTTypes.TearDown;

Begin
  FTypes.Free;
  FTypes := Nil;
End;

Procedure TestTTypes.TestAsString;

Begin
  CheckEquals('MyType', FTypes.AsString(True, False));
  FTypes.AddToken('Integer');
  CheckEquals('MyType = Integer', FTypes.AsString(True, False));
End;

Procedure TestTTypes.TestCreate;

Begin
  CheckEquals(scPrivate, FTypes.Scope);
  CheckEquals(12, FTypes.Line);
  CheckEquals(23, FTypes.Column);
  CheckEquals(iiPublicType, FTypes.ImageIndex);
  CheckEquals(BADIImageIndex(iiPublicType, scPrivate), FTypes.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('PascalModule Tests', TestTTypes.Suite);
End.
