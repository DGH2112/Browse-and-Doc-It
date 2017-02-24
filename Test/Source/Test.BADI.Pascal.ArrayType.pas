Unit Test.BADI.Pascal.ArrayType;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.TypeDecl;

Type
  TestTArrayType = Class(TExtendedTestCase)
  Strict Private
    FArrayType: TArrayType;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestAsString;
    Procedure TestAddDimension;
  End;

Implementation

Uses
  BADI.Types;

Procedure TestTArrayType.SetUp;

Begin
  FArrayType := TArrayType.Create('MyArrayType', scProtected, 12, 23, iiPublicType, Nil);
End;

Procedure TestTArrayType.TearDown;

Begin
  FArrayType.Free;
  FArrayType := Nil;
End;

Procedure TestTArrayType.TestAsString;

Begin
  CheckEquals('MyArrayType', FArrayType.AsString(True, False));
  FArrayType.AddToken('Array');
  FArrayType.AddToken('Of');
  FArrayType.AddToken('Integer');
  CheckEquals('MyArrayType = Array Of Integer', FArrayType.AsString(True, False));
  FArrayType.ClearTokens;
  FArrayType.AddToken('Array');
  FArrayType.AddToken('[');
  FArrayType.AddToken('1');
  FArrayType.AddToken('..');
  FArrayType.AddToken('2');
  FArrayType.AddToken(']');
  FArrayType.AddToken('Of');
  FArrayType.AddToken('Integer');
  CheckEquals('MyArrayType = Array[1..2] Of Integer', FArrayType.AsString(True, False));
End;

Procedure TestTArrayType.TestCreate;

Begin
  CheckEquals(scProtected, FArrayType.Scope);
  CheckEquals(12, FArrayType.Line);
  CheckEquals(23, FArrayType.Column);
  CheckEquals(iiPublicType, FArrayType.ImageIndex);
  CheckEquals(iiProtectedType, FArrayType.ImageIndexAdjustedForScope);
End;

Procedure TestTArrayType.TestAddDimension;

Begin
  CheckEquals(0, FArrayType.Dimensions);
  FArrayType.AddDimension;
  CheckEquals(1, FArrayType.Dimensions);
  FArrayType.AddDimension;
  CheckEquals(2, FArrayType.Dimensions);
End;

Initialization
  RegisterTest('PascalModule Tests', TestTArrayType.Suite);
End.
