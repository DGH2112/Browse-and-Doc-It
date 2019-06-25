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
Unit Test.BADI.Generic.Parameter;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Generic.Parameter;

Type
  TestTGenericParameter = Class(TExtendedTestCase)
  Private
    FType: TTestGenericTypeDecl;
  Strict Private
    FGenericParameter: TGenericParameter;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestParamModifier;
    Procedure TestArrayOf;
    Procedure TestParamType;
    Procedure TestDefaultValue;
  End;

Implementation

Uses
  BADI.Types;

Procedure TestTGenericParameter.SetUp;
Begin
  FType := TTestGenericTypeDecl.Create('', scNone, 0, 0, iiNone, Nil);
  FType.AddToken('String');
  FGenericParameter := TTestGenericParameter.Create(pamVar, 'MyParam', True,
    FType, 'Something', scPrivate, 3, 4);
End;

Procedure TestTGenericParameter.TearDown;
Begin
  FGenericParameter.Free;
  FGenericParameter := Nil;
  FType.Free;
End;

Procedure TestTGenericParameter.TestArrayOf;
Begin
  CheckEquals(True, FGenericParameter.ArrayOf);
End;

Procedure TestTGenericParameter.TestCreate;
Begin
  CheckEquals('MyParam', FGenericParameter.Identifier);
  CheckEquals(scPrivate, FGenericParameter.Scope);
  CheckEquals(3, FGenericParameter.Line);
  CheckEquals(4, FGenericParameter.Column);
End;

Procedure TestTGenericParameter.TestDefaultValue;
Begin
  CheckEquals('Something', FGenericParameter.DefaultValue);
End;

Procedure TestTGenericParameter.TestParamModifier;

Const
  strPM: Array [Low(TParamModifier) .. High(TParamModifier)] Of String = (
    'pamNone', 'pamVar', 'pamConst', 'pamOut');

Begin
  CheckEquals(strPM[pamVar], strPM[FGenericParameter.ParamModifier]);
End;

Procedure TestTGenericParameter.TestParamType;
Begin
  CheckEquals('String', FGenericParameter.ParamType.AsString(False, False));
End;

Initialization
  RegisterTest('BaseLanguageModule Tests', TestTGenericParameter.Suite);
End.
