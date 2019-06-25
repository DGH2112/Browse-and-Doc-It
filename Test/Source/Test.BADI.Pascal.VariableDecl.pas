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
Unit Test.BADI.Pascal.VariableDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.VariableDecl;

Type
  TestTVar = Class(TExtendedTestCase)
  Strict Private
    FVar: TVar;
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

Procedure TestTVar.SetUp;
Begin
  FVar := TVar.Create('MyVar', scPrivate, 12, 23, iiPublicVariable, Nil);
  FVar.AddToken('Integer');
End;

Procedure TestTVar.TearDown;
Begin
  FVar.Free;
  FVar := Nil;
End;

Procedure TestTVar.TestAsString;
Begin
  Checkequals('MyVar : Integer', FVar.AsString(True, False));
End;

Procedure TestTVar.TestCreate;
Begin
  Checkequals('MyVar', FVar.Identifier);
  Checkequals(scPrivate, FVar.Scope);
  Checkequals(12, FVar.Line);
  Checkequals(23, FVar.Column);
  Checkequals(iiPublicVariable, FVar.ImageIndex);
  Checkequals(BADIImageIndex(iiPublicVariable, scPrivate), FVar.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('PascalModule Tests', TestTVar.Suite);
End.
