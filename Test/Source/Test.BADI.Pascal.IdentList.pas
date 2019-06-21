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
Unit Test.BADI.Pascal.IdentList;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.IdentList;

Type
  TestTIdentList = Class(TExtendedTestCase)
  Strict Private
    FIdentList: TIdentList;
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

Procedure TestTIdentList.SetUp;

Begin
  FIdentList := TIdentList.Create('Hello', scNone, 12, 23, iiNone, Nil);
End;

Procedure TestTIdentList.TearDown;

Begin
  FIdentList.Free;
  FIdentList := Nil;
End;

Procedure TestTIdentList.TestAsString;

Begin
  CheckEquals('Hello', FIdentList.AsString(True, False));
  FIdentList.AddToken('=', ttSymbol);
  FIdentList.AddToken('1', ttNumber);
  CheckEquals('Hello = 1', FIdentList.AsString(True, False));
  FIdentList.ClearTokens;
  FIdentList.AddToken('In', ttReservedWord);
  FIdentList.AddToken('''D:\Path\PascalFile.pas''', ttSingleLiteral);
  CheckEquals('Hello In ''D:\Path\PascalFile.pas''', FIdentList.AsString(True, False));
End;

Procedure TestTIdentList.TestCreate;

Begin
  CheckEquals(scNone, FIdentList.Scope);
  CheckEquals(12, FIdentList.Line);
  CheckEquals(23, FIdentList.Column);
  CheckEquals(iiNone, FIdentList.ImageIndex);
  CheckEquals(BADIImageIndex(iiNone, scNone), FIdentList.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('PascalModule Tests', TestTIdentList.Suite);
End.
