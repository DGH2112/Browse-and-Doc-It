Unit Test.BADI.Generic.Parameter;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Generic.Parameter;

Type
  TestTGenericParameter = Class(TExtendedTestCase)
  Private
    FType: TTestGenericTypeDecl;
  Strict Private
    FGenericParameter: TGenericParameter;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestParamModifier;
    Procedure TestArrayOf;
    Procedure TestParamType;
    Procedure TestDefaultValue;
  End;

Implementation

Uses
  BADI.Types;

Procedure TestTGenericParameter.SetUp;
Begin
  FType := TTestGenericTypeDecl.Create('', scNone, 0, 0, iiNone, Nil);
  FType.AddToken('String');
  FGenericParameter := TTestGenericParameter.Create(pamVar, 'MyParam', True,
    FType, 'Something', scPrivate, 3, 4);
End;

Procedure TestTGenericParameter.TearDown;
Begin
  FGenericParameter.Free;
  FGenericParameter := Nil;
  FType.Free;
End;

Procedure TestTGenericParameter.TestArrayOf;
Begin
  CheckEquals(True, FGenericParameter.ArrayOf);
End;

Procedure TestTGenericParameter.TestCreate;
Begin
  CheckEquals('MyParam', FGenericParameter.Identifier);
  CheckEquals(scPrivate, FGenericParameter.Scope);
  CheckEquals(3, FGenericParameter.Line);
  CheckEquals(4, FGenericParameter.Column);
End;

Procedure TestTGenericParameter.TestDefaultValue;
Begin
  CheckEquals('Something', FGenericParameter.DefaultValue);
End;

Procedure TestTGenericParameter.TestParamModifier;

Const
  strPM: Array [Low(TParamModifier) .. High(TParamModifier)] Of String = (
    'pamNone', 'pamVar', 'pamConst', 'pamOut');

Begin
  CheckEquals(strPM[pamVar], strPM[FGenericParameter.ParamModifier]);
End;

Procedure TestTGenericParameter.TestParamType;
Begin
  CheckEquals('String', FGenericParameter.ParamType.AsString(False, False));
End;

Initialization
  RegisterTest('BaseLanguageModule Tests', TestTGenericParameter.Suite);
End.
