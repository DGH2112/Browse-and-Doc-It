Unit Test.BADI.DocIssue;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.DocIssue;

Type
  TestTDocIssue = Class(TExtendedTestCase)
  Strict Private
    FDocIssue: TDocIssue;
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

Procedure TestTDocIssue.SetUp;
Begin
  FDocIssue := TDocIssue.Create('This is a simple message.',
    scNone, 'MyMethod', 1, 2, iiWarning);
End;

Procedure TestTDocIssue.TearDown;
Begin
  FDocIssue.Free;
  FDocIssue := Nil;
End;

Procedure TestTDocIssue.TestAsString;

Begin
  CheckEquals('This is a simple message. [MyMethod]', FDocIssue.AsString(True, False));
End;

Procedure TestTDocIssue.TestCreate;
Begin
  CheckEquals('This is a simple message. [MyMethod]', FDocIssue.AsString(True, False));
  CheckEquals(scNone, FDocIssue.Scope);
  CheckEquals(1, FDocIssue.Line);
  CheckEquals(2, FDocIssue.Column);
  CheckEquals(iiWarning, FDocIssue.ImageIndex);
End;

Initialization
  RegisterTest('BaseLanguageModule Tests', TestTDocIssue.Suite);
End.
