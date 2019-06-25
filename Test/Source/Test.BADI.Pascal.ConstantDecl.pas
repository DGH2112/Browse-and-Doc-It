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
Unit Test.BADI.Pascal.ConstantDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.ConstantDecl;

Type
  TestTConstant = Class(TExtendedTestCase)
  Strict Private
    FConstant: TConstant;
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

Procedure TestTConstant.SetUp;
Begin
  FConstant := TConstant.Create('MyConstant', scPrivate, 12, 23, iiPublicConstant, Nil);
  FConstant.AddToken('1');
End;

Procedure TestTConstant.TearDown;
Begin
  FConstant.Free;
  FConstant := Nil;
End;

Procedure TestTConstant.TestAsString;
Begin
  Checkequals('MyConstant = 1', FConstant.AsString(True, False));
  FConstant.AddToken('+');
  FConstant.AddToken('2');
  FConstant.AddToken('*');
  FConstant.AddToken('3');
  Checkequals('MyConstant = 1 + 2 * 3', FConstant.AsString(True, False));
End;

Procedure TestTConstant.TestCreate;
Begin
  Checkequals('MyConstant', FConstant.Identifier);
  Checkequals(scPrivate, FConstant.Scope);
  Checkequals(12, FConstant.Line);
  Checkequals(23, FConstant.Column);
  Checkequals(iiPublicConstant, FConstant.ImageIndex);
  Checkequals(BADIImageIndex(iiPublicConstant, scPrivate), FConstant.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('PascalModule Tests', TestTConstant.Suite);
End.
