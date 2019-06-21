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
Unit Test.BADI.Pascal.ArrayType;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.TypeDecl;

Type
  TestTArrayType = Class(TExtendedTestCase)
  Strict Private
    FArrayType: TArrayType;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestAsString;
    Procedure TestAddDimension;
  End;

Implementation

Uses
  BADI.Types,
  BADI.Functions;

Procedure TestTArrayType.SetUp;

Begin
  FArrayType := TArrayType.Create('MyArrayType', scProtected, 12, 23, iiPublicType, Nil);
End;

Procedure TestTArrayType.TearDown;

Begin
  FArrayType.Free;
  FArrayType := Nil;
End;

Procedure TestTArrayType.TestAsString;

Begin
  CheckEquals('MyArrayType', FArrayType.AsString(True, False));
  FArrayType.AddToken('Array');
  FArrayType.AddToken('Of');
  FArrayType.AddToken('Integer');
  CheckEquals('MyArrayType = Array Of Integer', FArrayType.AsString(True, False));
  FArrayType.ClearTokens;
  FArrayType.AddToken('Array');
  FArrayType.AddToken('[');
  FArrayType.AddToken('1');
  FArrayType.AddToken('..');
  FArrayType.AddToken('2');
  FArrayType.AddToken(']');
  FArrayType.AddToken('Of');
  FArrayType.AddToken('Integer');
  CheckEquals('MyArrayType = Array[1..2] Of Integer', FArrayType.AsString(True, False));
End;

Procedure TestTArrayType.TestCreate;

Begin
  CheckEquals(scProtected, FArrayType.Scope);
  CheckEquals(12, FArrayType.Line);
  CheckEquals(23, FArrayType.Column);
  CheckEquals(iiPublicType, FArrayType.ImageIndex);
  CheckEquals(BADIImageIndex(iiPublicType, scProtected), FArrayType.ImageIndexAdjustedForScope);
End;

Procedure TestTArrayType.TestAddDimension;

Begin
  CheckEquals(0, FArrayType.Dimensions);
  FArrayType.AddDimension;
  CheckEquals(1, FArrayType.Dimensions);
  FArrayType.AddDimension;
  CheckEquals(2, FArrayType.Dimensions);
End;

Initialization
  RegisterTest('PascalModule Tests', TestTArrayType.Suite);
End.
