Unit Test.BADI.Pascal.PropertySpec;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.PropertySpec;

Type
  TestTPascalPropertySpec = Class(TExtendedTestCase)
  Strict Private
    FPropertySpec: TPropertySpec;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestAsString;
  End;

Implementation

Uses
  BADI.Types;

Procedure TestTPascalPropertySpec.SetUp;

Begin
  FPropertySpec := TPropertySpec.Create('', scNone, 0, 0, iiNone, Nil);
  FPropertySpec.AddToken('FValue');
End;

Procedure TestTPascalPropertySpec.TearDown;

Begin
  FPropertySpec.Free;
  FPropertySpec := Nil;
End;

Procedure TestTPascalPropertySpec.TestAsString;

Begin
  CheckEquals('FValue', FPropertySpec.AsString(True, False));
End;

Initialization
  RegisterTest('PascalModule Tests', TestTPascalPropertySpec.Suite);
End.
