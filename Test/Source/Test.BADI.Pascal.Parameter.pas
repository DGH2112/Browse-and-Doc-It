Unit Test.BADI.Pascal.Parameter;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module;

Type
  TestTPascalParameter = Class(TExtendedTestCase)
  Strict Private
  Public
  Published
    Procedure TestAsString;
  End;

Implementation

Uses
  BADI.Types,
  BADI.Pascal.TypeDecl,
  BADI.Pascal.ParameterDecl;

Procedure TestTPascalParameter.TestAsString;

Var
  P: TPascalParameter;
  AType: TInteger;

Begin
  AType := TInteger.Create('', scNone, 0, 0, iiNone, Nil);
  Try
    AType.AddToken('Integer');
    P := TPascalParameter.Create(pamNone, 'MyParam', False, AType, '', scNone, 12, 23);
    Try
      CheckEquals(scNone, P.Scope);
      CheckEquals(12, P.Line);
      CheckEquals(23, P.Column);
      CheckEquals('MyParam : Integer', P.AsString(True, False));
    Finally
      P.Free;
    End;
    P := TPascalParameter.Create(pamVar, 'MyParam', False, AType, '', scNone, 12, 23);
    Try
      CheckEquals(scNone, P.Scope);
      CheckEquals(12, P.Line);
      CheckEquals(23, P.Column);
      CheckEquals('var MyParam : Integer', P.AsString(True, False));
    Finally
      P.Free;
    End;
    P := TPascalParameter.Create(pamConst, 'MyParam', False, AType, '', scNone, 12, 23);
    Try
      CheckEquals(scNone, P.Scope);
      CheckEquals(12, P.Line);
      CheckEquals(23, P.Column);
      CheckEquals('const MyParam : Integer', P.AsString(True, False));
    Finally
      P.Free;
    End;
    P := TPascalParameter.Create(pamOut, 'MyParam', False, AType, '', scNone, 12, 23);
    Try
      CheckEquals(scNone, P.Scope);
      CheckEquals(12, P.Line);
      CheckEquals(23, P.Column);
      CheckEquals('out MyParam : Integer', P.AsString(True, False));
    Finally
      P.Free;
    End;
    P := TPascalParameter.Create(pamNone, 'MyParam', True, AType, '', scNone, 12, 23);
    Try
      CheckEquals(scNone, P.Scope);
      CheckEquals(12, P.Line);
      CheckEquals(23, P.Column);
      CheckEquals('MyParam : Array Of Integer', P.AsString(True, False));
    Finally
      P.Free;
    End;
    P := TPascalParameter.Create(pamNone, 'MyParam', False, AType, '0', scNone, 12, 23);
    Try
      CheckEquals(scNone, P.Scope);
      CheckEquals(12, P.Line);
      CheckEquals(23, P.Column);
      CheckEquals('MyParam : Integer = 0', P.AsString(True, False));
    Finally
      P.Free;
    End;
  Finally
    AType.Free;
  End;
End;

Initialization
  RegisterTest('PascalModule Tests', TestTPascalParameter.Suite);
End.
