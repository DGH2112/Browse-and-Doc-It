Unit Test.BADI.Pascal.ConstantDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.ConstantDecl;

Type
  TestTConstant = Class(TExtendedTestCase)
  Strict Private
    FConstant: TConstant;
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

Procedure TestTConstant.SetUp;
Begin
  FConstant := TConstant.Create('MyConstant', scPrivate, 12, 23, iiPublicConstant, Nil);
  FConstant.AddToken('1');
End;

Procedure TestTConstant.TearDown;
Begin
  FConstant.Free;
  FConstant := Nil;
End;

Procedure TestTConstant.TestAsString;
Begin
  Checkequals('MyConstant = 1', FConstant.AsString(True, False));
  FConstant.AddToken('+');
  FConstant.AddToken('2');
  FConstant.AddToken('*');
  FConstant.AddToken('3');
  Checkequals('MyConstant = 1 + 2 * 3', FConstant.AsString(True, False));
End;

Procedure TestTConstant.TestCreate;
Begin
  Checkequals('MyConstant', FConstant.Identifier);
  Checkequals(scPrivate, FConstant.Scope);
  Checkequals(12, FConstant.Line);
  Checkequals(23, FConstant.Column);
  Checkequals(iiPublicConstant, FConstant.ImageIndex);
  Checkequals(BADIImageIndex(iiPublicConstant, scPrivate), FConstant.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('PascalModule Tests', TestTConstant.Suite);
End.
