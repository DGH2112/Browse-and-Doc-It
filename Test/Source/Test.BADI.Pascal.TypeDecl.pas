Unit Test.BADI.Pascal.TypeDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.TypeDecl;

Type
  TestTTypes = Class(TExtendedTestCase)
  Strict Private
    FTypes: TTypes;
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

Procedure TestTTypes.SetUp;

Begin
  FTypes := TTypes.Create('MyType', scPrivate, 12, 23, iiPublicType, Nil);
End;

Procedure TestTTypes.TearDown;

Begin
  FTypes.Free;
  FTypes := Nil;
End;

Procedure TestTTypes.TestAsString;

Begin
  CheckEquals('MyType', FTypes.AsString(True, False));
  FTypes.AddToken('Integer');
  CheckEquals('MyType = Integer', FTypes.AsString(True, False));
End;

Procedure TestTTypes.TestCreate;

Begin
  CheckEquals(scPrivate, FTypes.Scope);
  CheckEquals(12, FTypes.Line);
  CheckEquals(23, FTypes.Column);
  CheckEquals(iiPublicType, FTypes.ImageIndex);
  CheckEquals(BADIImageIndex(iiPublicType, scPrivate), FTypes.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('PascalModule Tests', TestTTypes.Suite);
End.
