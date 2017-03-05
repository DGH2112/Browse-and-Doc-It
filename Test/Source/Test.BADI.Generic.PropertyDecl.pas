Unit Test.BADI.Generic.PropertyDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Generic.PropertyDecl;

Type
  TestTGenericProperty = Class(TExtendedTestCase)
  Strict Private
    FGenericProperty: TGenericProperty;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestAddParameter;
    Procedure TestCheckDocumentation;
    Procedure TestParameterCount;
    Procedure TestParameters;
    Procedure TestTypeId;
  End;

Implementation

Uses
  BADI.Types,
  BADI.Generic.Parameter,
  BADI.Comment,
  BADI.Functions;

Procedure TestTGenericProperty.SetUp;

Var
  T: TTestGenericTypeDecl;

Begin
  FGenericProperty := TTestGenericProperty.Create('MyProperty', scProtected, 12,
    23, iiPublicProperty, Nil);
  T := TTestGenericTypeDecl.Create('Boolean', scNone, 0, 0, iiNone, Nil);
  T.AddToken('Boolean');
  FGenericProperty.ReturnType.Add(T);
End;

Procedure TestTGenericProperty.TearDown;
Begin
  FGenericProperty.Free;
  FGenericProperty := Nil;
End;

Procedure TestTGenericProperty.TestAddParameter;

Var
  P: TGenericParameter;
  AType: TTestGenericTypeDecl;

Begin
  AType := TTestGenericTypeDecl.Create('', scNone, 0, 0, iiNone, Nil);
  Try
    AType.AddToken('String');
    P := TTestGenericParameter.Create(pamNone, 'Param1', False, AType, '', scNone, 0, 0);
    FGenericProperty.AddParameter(P);
    CheckEquals(1, FGenericProperty.ParameterCount);
    CheckEquals('Param1 = String', FGenericProperty.Parameters[0].AsString(False, False));
  Finally
    AType.Free;
  End;
End;

Procedure TestTGenericProperty.TestCheckDocumentation;

Var
  AType: TTestGenericTypeDecl;
  P: TTestGenericParameter;
  C: TComment;
  boolCascade: Boolean;

Begin
  AType := TTestGenericTypeDecl.Create('', scNone, 0, 0, iiNone, Nil);
  Try
    AType.AddToken('String');
    P := TTestGenericParameter.Create(pamNone, 'Param1', False, AType, '', scNone, 0, 0);
    FGenericProperty.AddParameter(P);
    AType.ClearTokens;
    AType.AddToken('Integer');
    P := TTestGenericParameter.Create(pamVar, 'Param2', True, AType, '0', scNone, 0, 0);
    FGenericProperty.AddParameter(P);
    AType.ClearTokens;
    AType.AddToken('Byte');
    P := TTestGenericParameter.Create(pamConst, 'Param3', False, AType, '', scNone, 0, 0);
    FGenericProperty.AddParameter(P);
    AType.ClearTokens;
    AType.AddToken('Double');
    P := TTestGenericParameter.Create(pamOut, 'Param4', False, AType, '', scNone, 0, 0);
    FGenericProperty.AddParameter(P);
    CheckEquals(4, FGenericProperty.ParameterCount);
    CheckEquals('Param1 = String', FGenericProperty.Parameters[0].AsString(False, False));
    CheckEquals('Param2 = Integer', FGenericProperty.Parameters[1].AsString(False, False));
    CheckEquals('Param3 = Byte', FGenericProperty.Parameters[2].AsString(False, False));
    CheckEquals('Param4 = Double', FGenericProperty.Parameters[3].AsString(False, False));
    FGenericProperty.CheckDocumentation(boolCascade);
    CheckEquals('1) Property ''MyProperty'' has not been documented.',
      FGenericProperty.DocConflict(1));
    FGenericProperty.DeleteDocumentConflicts;
    C := TComment.Create(
      '', 0, 0);
    Try
      FGenericProperty.Comment := C;
      FGenericProperty.CheckDocumentation(boolCascade);
      CheckEquals('1) Property ''MyProperty'' has no description.',
        FGenericProperty.DocConflict(1));
    Finally
      C.Free;
    End;
    FGenericProperty.DeleteDocumentConflicts;
    C := TComment.Create(
      'This is a description.', 0, 0);
    Try
      FGenericProperty.Comment := C;
      FGenericProperty.CheckDocumentation(boolCascade);
      CheckEquals('1) A pre-condition in Property ''MyProperty'' is not documented.',
        FGenericProperty.DocConflict(1));
      CheckEquals('2) Property ''MyProperty'' has a different parameter count (4 not 0).',
        FGenericProperty.DocConflict(2));
      CheckEquals('3) Property ''MyProperty'' has missing pre-condition tags.',
        FGenericProperty.DocConflict(3));
    Finally
      C.Free;
    End;
    FGenericProperty.DeleteDocumentConflicts;
    C := TComment.Create(
      ''#13#10 +
      '  This method does something interesting.'#13#10 +
      '  @precon  None.'#13#10 +
      '  @postcon Does something very interesting.'#13#10 +
      '  @param   Param1 as a Integer'#13#10 +
      '  @param   Param2 as an array of Integer as a reference'#13#10 +
      '  @param   Param3 as a Byte as a constant'#13#10 +
      '  @param   Param4 as a Double as an out parameter'#13#10 +
      '  @return  a Boolean', 0, 0);
    Try
      FGenericProperty.Comment := C;
      FGenericProperty.CheckDocumentation(boolCascade);
      CheckEquals
        ('1) The parameter type for ''Param1'' in Property ''MyProperty'' is incorrect (''String'').',
        FGenericProperty.DocConflict(1));
    Finally
      C.Free;
    End;
    FGenericProperty.DeleteDocumentConflicts;
    C := TComment.Create(
      ''#13#10 +
      '  This method does something interesting.'#13#10 +
      '  @precon  None.'#13#10 +
      '  @postcon Does something very interesting.'#13#10 +
      '  @param   Param1 as a String'#13#10 +
      '  @param   Param2 as an array of Integer as a reference'#13#10 +
      '  @param   Param3 as a Byte as a constant'#13#10 +
      '  @param   Param4 as a Double as an out parameter', 0, 0);
    Try
      FGenericProperty.Comment := C;
      FGenericProperty.CheckDocumentation(boolCascade);
      CheckEquals('1) Property ''MyProperty''`s return type is not documented.',
        FGenericProperty.DocConflict(1));
    Finally
      C.Free;
    End;
    FGenericProperty.DeleteDocumentConflicts;
    C := TComment.Create(
      ''#13#10 +
      '  This method does something interesting.'#13#10 +
      '  @precon  None.'#13#10 +
      '  @postcon Does something very interesting.'#13#10 +
      '  @param   Param1 as a String'#13#10 +
      '  @param   Param2 as an array of Integer as a reference'#13#10 +
      '  @param   Param3 as a Byte as a constant'#13#10 +
      '  @param   Param4 as a Double as an out parameter'#13#10 +
      '  @return  a String', 0, 0);
    Try
      FGenericProperty.Comment := C;
      FGenericProperty.CheckDocumentation(boolCascade);
      CheckEquals('1) Property ''MyProperty''`s return type is incorrect (''Boolean'').',
        FGenericProperty.DocConflict(1));
    Finally
      C.Free;
    End;
    FGenericProperty.DeleteDocumentConflicts;
    C := TComment.Create(
      ''#13#10 +
      '  This method does something interesting.'#13#10 +
      '  @precon '#13#10 +
      '  @param   Param1 as a String'#13#10 +
      '  @param   Param2 as an array of Integer as a reference'#13#10 +
      '  @param   Param3 as a Byte as a constant'#13#10 +
      '  @param   Param4 as a Double as an out parameter'#13#10 +
      '  @return  a Boolean', 0, 0);
    Try
      FGenericProperty.Comment := C;
      FGenericProperty.CheckDocumentation(boolCascade);
      CheckEquals('1) A pre-condition in Property ''MyProperty'' is not documented.',
        FGenericProperty.DocConflict(1));
    Finally
      C.Free;
    End;
    FGenericProperty.DeleteDocumentConflicts;
    C := TComment.Create(
      ''#13#10 +
      '  This method does something interesting.'#13#10 +
      '  @precon None'#13#10 +
      '  @precon None'#13#10 +
      '  @param   Param1 as a String'#13#10 +
      '  @param   Param2 as an array of Integer as a reference'#13#10 +
      '  @param   Param3 as a Byte as a constant'#13#10 +
      '  @param   Param4 as a Double as an out parameter'#13#10 +
      '  @return  a Boolean', 0, 0);
    Try
      FGenericProperty.Comment := C;
      FGenericProperty.CheckDocumentation(boolCascade);
      CheckEquals('1) Property ''MyProperty'' has too many pre-condition tags.',
        FGenericProperty.DocConflict(1));
    Finally
      C.Free;
    End;
    FGenericProperty.DeleteDocumentConflicts;
    C := TComment.Create(
      ''#13#10 +
      '  This method does something interesting.'#13#10 +
      '  @precon None'#13#10 +
      '  @postcon'#13#10 +
      '  @param   Param1 as a String'#13#10 +
      '  @param   Param2 as an array of Integer as a reference'#13#10 +
      '  @param   Param3 as a Byte as a constant'#13#10 +
      '  @param   Param4 as a Double as an out parameter'#13#10 +
      '  @return  a Boolean', 0, 0);
    Try
      FGenericProperty.Comment := C;
      FGenericProperty.CheckDocumentation(boolCascade);
      CheckEquals('1) A post-condition in Property ''MyProperty'' is not documented.',
        FGenericProperty.DocConflict(1));
    Finally
      C.Free;
    End;
    FGenericProperty.DeleteDocumentConflicts;
    C := TComment.Create(
      ''#13#10 +
      '  This method does something interesting.'#13#10 +
      '  @precon None'#13#10 +
      '  @param   Param1 as a String'#13#10 +
      '  @param   Param2 as an array of Integer as a reference'#13#10 +
      '  @param   Param3 as a Byte as a constant'#13#10 +
      '  @param   Param4 as a Double as an out parameter'#13#10 +
      '  @return  a Boolean', 0, 0);
    Try
      FGenericProperty.Comment := C;
      FGenericProperty.CheckDocumentation(boolCascade);
      CheckEquals('1) Property ''MyProperty'' has a missing post-condition tag.',
        FGenericProperty.DocConflict(1));
    Finally
      C.Free;
    End;
    FGenericProperty.DeleteDocumentConflicts;
    C := TComment.Create(
      ''#13#10 +
      '  This method does something interesting.'#13#10 +
      '  @precon None'#13#10 +
      '  @postcon None'#13#10 +
      '  @postcon None'#13#10 +
      '  @param   Param1 as a String'#13#10 +
      '  @param   Param2 as an array of Integer as a reference'#13#10 +
      '  @param   Param3 as a Byte as a constant'#13#10 +
      '  @param   Param4 as a Double as an out parameter'#13#10 +
      '  @return  a Boolean', 0, 0);
    Try
      FGenericProperty.Comment := C;
      FGenericProperty.CheckDocumentation(boolCascade);
      CheckEquals('1) Property ''MyProperty'' has too many post-condition tags.',
        FGenericProperty.DocConflict(1));
    Finally
      C.Free;
    End;

    FGenericProperty.DeleteDocumentConflicts;
    C := TComment.Create(
      ''#13#10 +
      '  This method does something interesting.'#13#10 +
      '  @precon  None.'#13#10 +
      '  @postcon Does something very interesting.'#13#10 +
      '  @param   Param1 as a String'#13#10 +
      '  @param   Param2 as an array of Integer as a reference'#13#10 +
      '  @param   Param3 as a Byte as a constant'#13#10 +
      '  @param   Param4 as a Double as an out parameter'#13#10 +
      '  @return  a Boolean', 0, 0);
    Try
      FGenericProperty.Comment := C;
      FGenericProperty.CheckDocumentation(boolCascade);
      CheckEquals(0, FGenericProperty.ElementCount, FGenericProperty.DocConflict(1));
    Finally
      C.Free;
    End;
  Finally
    AType.Free;
  End;
End;

Procedure TestTGenericProperty.TestCreate;
Begin
  CheckEquals('MyProperty', FGenericProperty.Identifier);
  CheckEquals(scProtected, FGenericProperty.Scope);
  CheckEquals(12, FGenericProperty.Line);
  CheckEquals(23, FGenericProperty.Column);
  CheckEquals(iiPublicProperty, FGenericProperty.ImageIndex);
  CheckEquals(BADIImageIndex(iiPublicProperty, scProtected), FGenericProperty.ImageIndexAdjustedForScope);
End;

Procedure TestTGenericProperty.TestParameterCount;
Var
  AType: TTestGenericTypeDecl;
  P: TTestGenericParameter;
Begin
  AType := TTestGenericTypeDecl.Create('', scNone, 0, 0, iiNone, Nil);
  Try
    AType.AddToken('String');
    P := TTestGenericParameter.Create(pamNone, 'Param1', False, AType, '', scNone, 0, 0);
    FGenericProperty.AddParameter(P);
    CheckEquals(1, FGenericProperty.ParameterCount);
  Finally
    AType.Free;
  End;
End;

Procedure TestTGenericProperty.TestParameters;
Var
  AType: TTestGenericTypeDecl;
  P: TTestGenericParameter;
Begin
  AType := TTestGenericTypeDecl.Create('', scNone, 0, 0, iiNone, Nil);
  Try
    AType.AddToken('String');
    P := TTestGenericParameter.Create(pamNone, 'Param1', False, AType, '', scNone, 0, 0);
    FGenericProperty.AddParameter(P);
    CheckEquals(1, FGenericProperty.ParameterCount);
  Finally
    AType.Free;
  End;
End;

Procedure TestTGenericProperty.TestTypeId;
Begin
  CheckEquals('Boolean', FGenericProperty.ReturnType.AsString(False, False));
End;

Initialization
  RegisterTest('BaseLanguageModule Tests', TestTGenericProperty.Suite);
End.
