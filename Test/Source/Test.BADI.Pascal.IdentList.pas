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
  BADI.Types;

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
  CheckEquals(iiNone, FIdentList.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('PascalModule Tests', TestTIdentList.Suite);
End.
