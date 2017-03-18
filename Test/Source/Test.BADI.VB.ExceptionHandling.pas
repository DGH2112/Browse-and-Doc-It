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
