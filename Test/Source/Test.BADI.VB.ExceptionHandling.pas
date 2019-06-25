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
Unit Test.BADI.VB.ExceptionHandling;

Interface

Uses
  Test.BADI.Base.Module,
  BADI.VB.ExceptionHandling;

Type
  //
  // Test Class for the TExceptionHandling Class Methods.
  //
  TestTExceptionHandling = Class(TExtendedTestCase)
  Strict Private
    FExceptionHandling : TExceptionHandling;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestGetHasErrorHnd;
    Procedure TestGetHasExit;
    Procedure TestGetHasPop;
    Procedure TestGetHasPush;
    Procedure TestGetPushName;
    Procedure TestGetPushParams;
    Procedure TestSetHasErrorHnd;
    Procedure TestSetHasExit;
    Procedure TestSetHasPop;
    Procedure TestSetHasPush;
    Procedure TestSetPushName;
  End;

Implementation

uses
  TestFramework;

//
// Test methods for the class TExceptionHandling.
//
Procedure TestTExceptionHandling.Setup;

Begin
  FExceptionHandling := TExceptionHandling.Create('MyMethod');
End;

Procedure TestTExceptionHandling.TearDown;

Begin
  FExceptionHandling.Free;
End;

Procedure TestTExceptionHandling.TestGetHasErrorHnd;

Begin
  CheckEquals(False, FExceptionHandling.GetHasErrorHnd);
  FExceptionHandling.SetHasErrorHnd(True);
  CheckEquals(True, FExceptionHandling.GetHasErrorHnd);
End;

Procedure TestTExceptionHandling.TestGetHasExit;

Begin
  CheckEquals(False, FExceptionHandling.GetHasExit);
  FExceptionHandling.SetHasExit(True);
  CheckEquals(True, FExceptionHandling.GetHasExit);
End;

Procedure TestTExceptionHandling.TestGetHasPop;

Begin
  CheckEquals(False, FExceptionHandling.GetHasPop);
  FExceptionHandling.SetHasPop(True);
  CheckEquals(True, FExceptionHandling.GetHasPop);
End;

Procedure TestTExceptionHandling.TestGetHasPush;

Begin
  CheckEquals(False, FExceptionHandling.GetHasPush);
  FExceptionHandling.SetHasPush(True);
  CheckEquals(True, FExceptionHandling.GetHasPush);
End;

Procedure TestTExceptionHandling.TestGetPushName;

Begin
  CheckEquals('', FExceptionHandling.GetPushName);
  FExceptionHandling.SetPushName('MyPushName');
  CheckEquals('MyPushName', FExceptionHandling.GetPushName);
End;

Procedure TestTExceptionHandling.TestGetPushParams;

Begin
  CheckEquals(0, FExceptionHandling.GetPushParams.Count);
  FExceptionHandling.GetPushParams.Add('Hello');
  CheckEquals(1, FExceptionHandling.GetPushParams.Count);
End;

Procedure TestTExceptionHandling.TestSetHasErrorHnd;

Begin
  CheckEquals(False, FExceptionHandling.GetHasErrorHnd);
  FExceptionHandling.SetHasErrorHnd(True);
  CheckEquals(True, FExceptionHandling.GetHasErrorHnd);
End;

Procedure TestTExceptionHandling.TestSetHasExit;

Begin
  CheckEquals(False, FExceptionHandling.GetHasExit);
  FExceptionHandling.SetHasExit(True);
  CheckEquals(True, FExceptionHandling.GetHasExit);
End;

Procedure TestTExceptionHandling.TestSetHasPop;

Begin
  CheckEquals(False, FExceptionHandling.GetHasPop);
  FExceptionHandling.SetHasPop(True);
  CheckEquals(True, FExceptionHandling.GetHasPop);
End;

Procedure TestTExceptionHandling.TestSetHasPush;

Begin
  CheckEquals(False, FExceptionHandling.GetHasPush);
  FExceptionHandling.SetHasPush(True);
  CheckEquals(True, FExceptionHandling.GetHasPush);
End;

Procedure TestTExceptionHandling.TestSetPushName;

Begin
  CheckEquals('', FExceptionHandling.GetPushName);
  FExceptionHandling.SetPushName('MyPushName');
  CheckEquals('MyPushName', FExceptionHandling.GetPushName);
End;

initialization
  RegisterTest('VB Module Tests', TestTExceptionHandling.Suite);
End.
