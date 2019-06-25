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
Unit Test.BADI.TokenInfo;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.TokenInfo;

Type
  TestTTokenInfo = Class(TExtendedTestCase)
  Strict Private
    FTokenInfo: TTokenInfo;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestAppend;
  End;

Implementation

Uses
  BADI.Types;

Procedure TestTTokenInfo.SetUp;

Begin
  FTokenInfo := TTokenInfo.Create('Hello', 12, 23, 34, 5, ttIdentifier);
End;

Procedure TestTTokenInfo.TearDown;

Begin
  FTokenInfo.Free;
  FTokenInfo := Nil;
End;

Procedure TestTTokenInfo.TestAppend;

Begin
  FTokenInfo.Append('Dave');
  CheckEquals('HelloDave', FTokenInfo.Token);
  CheckEquals(12, FTokenInfo.BufferPos);
  CheckEquals(23, FTokenInfo.Line);
  CheckEquals(34, FTokenInfo.Column);
  CheckEquals(9, FTokenInfo.Length);
End;

Procedure TestTTokenInfo.TestCreate;

Begin
  CheckEquals('Hello', FTokenInfo.Token);
  CheckEquals('', FTokenInfo.UToken);
  CheckEquals(12, FTokenInfo.BufferPos);
  CheckEquals(23, FTokenInfo.Line);
  CheckEquals(34, FTokenInfo.Column);
  CheckEquals(5, FTokenInfo.Length);
  CheckEquals(ttIdentifier, FTokenInfo.TokenType);
  CheckEquals(trUnknown, FTokenInfo.Reference);
End;

Initialization
  RegisterTest('BaseLanguageModule Tests', TestTTokenInfo.Suite);
End.
