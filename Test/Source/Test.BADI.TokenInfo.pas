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
