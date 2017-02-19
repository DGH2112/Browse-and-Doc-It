Unit Test.BADI.Generic.MethodDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Generic.MethodDecl;

Type
  TestTGenericMethodDecl = Class(TExtendedTestCase)
  Strict Private
    FGenericMethodDecl: TGenericMethodDecl;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestAddParameter;
    Procedure TestCheckDocumentation;
    Procedure TestMethodType;
    Procedure TestClassNames;
    Procedure TestParameterCount;
    Procedure TestParameters;
    Procedure TestReturnType;
    Procedure TestMsg;
    Procedure TestExt;
    Procedure TestClassMethod;
    Procedure TestQualifiedName;
    Procedure TestAlias;
    Procedure TestForwardDecl;
  End;

Implementation

Uses
  BADI.Types,
  BADI.Generic.Parameter,
  BADI.Generic.TypeDecl,
  BADI.Comment,
  BADI.ResourceStrings;

Procedure TestTGenericMethodDecl.SetUp;

Var
  T: TTestGenericTypeDecl;

Begin
  FGenericMethodDecl := TTestGenericMethodDecl.Create(mtFunction, 'MyFunction',
    scProtected, 34, 45);
  T := TTestGenericTypeDecl.Create('Boolean', scNone, 0, 0, iiNone, Nil);
  T.AddToken('Boolean');
  FGenericMethodDecl.ReturnType.Add(T);

End;

Procedure TestTGenericMethodDecl.TearDown;
Begin
  FGenericMethodDecl.Free;
  FGenericMethodDecl := Nil;
End;

Procedure TestTGenericMethodDecl.TestAddParameter;

Var
  P: TGenericParameter;
  AType: TGenericTypeDecl;

Begin
  AType := TTestGenericTypeDecl.Create('', scNone, 0, 0, iiNone, Nil);
  Try
    AType.AddToken('String');
    P := TTestGenericParameter.Create(pamNone, 'Param1', False, AType, '', scNone, 0, 0);
    FGenericMethodDecl.AddParameter(P);
    CheckEquals(1, FGenericMethodDecl.ParameterCount);
    CheckEquals('Param1 = String', FGenericMethodDecl.Parameters[0].AsString(False, False));
  Finally
    AType.Free;
  End;
End;

Procedure TestTGenericMethodDecl.TestAlias;
Begin
  CheckEquals('', FGenericMethodDecl.Alias);
  FGenericMethodDecl.Alias := 'MyFunctionA';
  CheckEquals('MyFunctionA', FGenericMethodDecl.Alias);
End;

Procedure TestTGenericMethodDecl.TestCheckDocumentation;

Var
  boolCascade: Boolean;
  AType: TTestGenericTypeDecl;
  P: TTestGenericParameter;
  C: TComment;
  M: TGenericMethodDecl;

Begin
  AType := TTestGenericTypeDecl.Create('', scNone, 0, 0, iiNone, Nil);
  Try
    AType.AddToken('String');
    P := TTestGenericParameter.Create(pamNone, 'Param1', False, AType, '', scNone, 0, 0);
    FGenericMethodDecl.AddParameter(P);
    AType.ClearTokens;
    AType.AddToken('Integer');
    P := TTestGenericParameter.Create(pamVar, 'Param2', True, AType, '0', scNone, 0, 0);
    FGenericMethodDecl.AddParameter(P);
    AType.ClearTokens;
    AType.AddToken('Byte');
    P := TTestGenericParameter.Create(pamConst, 'Param3', False, AType, '', scNone, 0, 0);
    FGenericMethodDecl.AddParameter(P);
    AType.ClearTokens;
    AType.AddToken('Double');
    P := TTestGenericParameter.Create(pamOut, 'Param4', False, AType, '', scNone, 0, 0);
    FGenericMethodDecl.AddParameter(P);
    CheckEquals(4, FGenericMethodDecl.ParameterCount);
    CheckEquals('Param1 = String', FGenericMethodDecl.Parameters[0].AsString(False, False));
    CheckEquals('Param2 = Integer', FGenericMethodDecl.Parameters[1].AsString(False, False));
    CheckEquals('Param3 = Byte', FGenericMethodDecl.Parameters[2].AsString(False, False));
    CheckEquals('Param4 = Double', FGenericMethodDecl.Parameters[3].AsString(False, False));
    FGenericMethodDecl.CheckDocumentation(boolCascade);
    CheckEquals('1) Method ''MyFunction'' has not been documented.',
      FGenericMethodDecl.DocConflict(1));
    FGenericMethodDecl.DeleteDocumentConflicts;
    C := TComment.Create('', 0, 0);
    Try
      FGenericMethodDecl.Comment := C;
      FGenericMethodDecl.CheckDocumentation(boolCascade);
      CheckEquals('1) Method ''MyFunction'' has no description.',
        FGenericMethodDecl.DocConflict(1));
    Finally
      C.Free;
    End;
    FGenericMethodDecl.DeleteDocumentConflicts;
    C := TComment.Create(
      'This is a description.', 0, 0);
    Try
      FGenericMethodDecl.Comment := C;
      FGenericMethodDecl.CheckDocumentation(boolCascade);
      CheckEquals('1) A pre-condition in Method ''MyFunction'' is not documented.',
        FGenericMethodDecl.DocConflict(1));
      CheckEquals('2) Method ''MyFunction'' has a different parameter count (4 not 0).',
        FGenericMethodDecl.DocConflict(2));
      CheckEquals('3) Method ''MyFunction'' has missing pre-condition tags.',
        FGenericMethodDecl.DocConflict(3));
      CheckEquals('4) Parameter ''Param1'' in Method ''MyFunction'' is not documented.',
        FGenericMethodDecl.DocConflict(4));
    Finally
      C.Free;
    End;
    FGenericMethodDecl.DeleteDocumentConflicts;
    C := TComment.Create(
      'This is a description.'#13#10 +
      '@precon', 0, 0);
    Try
      FGenericMethodDecl.Comment := C;
      FGenericMethodDecl.CheckDocumentation(boolCascade);
      CheckEquals('1) A pre-condition in Method ''MyFunction'' is not documented.',
        FGenericMethodDecl.DocConflict(1));
    Finally
      C.Free;
    End;
    FGenericMethodDecl.DeleteDocumentConflicts;
    C := TComment.Create(
      'This is a description.'#13#10 +
      '@precon None.', 0, 0);
    Try
      FGenericMethodDecl.Comment := C;
      FGenericMethodDecl.CheckDocumentation(boolCascade);
      CheckEquals('1) Method ''MyFunction'' has a different parameter count (4 not 0).',
        FGenericMethodDecl.DocConflict(1));
    Finally
      C.Free;
    End;
    FGenericMethodDecl.DeleteDocumentConflicts;
    C := TComment.Create(
      'This is a description.'#13#10'@precon None.'#13#10 +
      '@precon None.', 0, 0);
    Try
      FGenericMethodDecl.Comment := C;
      FGenericMethodDecl.CheckDocumentation(boolCascade);
      CheckEquals('2) Method ''MyFunction'' has too many pre-condition tags.',
        FGenericMethodDecl.DocConflict(2));
    Finally
      C.Free;
    End;
    FGenericMethodDecl.DeleteDocumentConflicts;
    C := TComment.Create(
      'This is a description.'#13#10 +
      '@precon None.'#13#10 +
      '@postcon '#13#10 +
      '@param param1 as an String'#13#10 +
      '@param param2 as an Array of Integer as a reference'#13#10 +
      '@param param3 as an Byte as a constant'#13#10 +
      '@param param4 as a Double as an out parameter'#13#10 +
      '@return a Boolean', 0, 0);
    Try
      FGenericMethodDecl.Comment := C;
      FGenericMethodDecl.CheckDocumentation(boolCascade);
      CheckEquals('1) A post-condition in Method ''MyFunction'' is not documented.',
        FGenericMethodDecl.DocConflict(1));
    Finally
      C.Free;
    End;
    FGenericMethodDecl.DeleteDocumentConflicts;
    C := TComment.Create(
      'This is a description.'#13#10 +
      '@precon None.'#13#10 +
      '@param param1 as an String'#13#10 +
      '@param param2 as an Array of Integer as a reference'#13#10 +
      '@param param3 as an Byte as a constant'#13#10 +
      '@param param4 as a Double as an out parameter'#13#10 +
      '@return a Boolean', 0, 0);
    Try
      FGenericMethodDecl.Comment := C;
      FGenericMethodDecl.CheckDocumentation(boolCascade);
      CheckEquals('1) Method ''MyFunction'' has a missing post-condition tag.',
        FGenericMethodDecl.DocConflict(1));
    Finally
      C.Free;
    End;
    FGenericMethodDecl.DeleteDocumentConflicts;
    C := TComment.Create(
      'This is a description.'#13#10 +
      '@precon None.'#13#10 +
      '@postcon None.'#13#10 +
      '@postcon None.'#13#10 +
      '@param param1 as an String'#13#10 +
      '@param param2 as an Array of Integer as a reference'#13#10 +
      '@param param3 as an Byte as a constant'#13#10 +
      '@param param4 as a Double as an out parameter'#13#10 +
      '@return a Boolean', 0, 0);
    Try
      FGenericMethodDecl.Comment := C;
      FGenericMethodDecl.CheckDocumentation(boolCascade);
      CheckEquals('1) Method ''MyFunction'' has too many post-condition tags.',
        FGenericMethodDecl.DocConflict(1));
    Finally
      C.Free;
    End;
    FGenericMethodDecl.DeleteDocumentConflicts;
    C := TComment.Create(
      'This is a description.'#13#10'@precon None.'#13#10 +
      '@postcon None.'#13#10 +
      '@param param1 as an Integer'#13#10 +
      '@param param2 as an Array of Integer as a reference'#13#10 +
      '@param param3 as an Byte as a constant'#13#10 +
      '@param param4 as a Double as an out parameter', 0, 0);
    Try
      FGenericMethodDecl.Comment := C;
      FGenericMethodDecl.CheckDocumentation(boolCascade);
      CheckEquals
        ('1) The parameter type for ''Param1'' in Method ''MyFunction'' is incorrect (''String'').',
        FGenericMethodDecl.DocConflict(1));
      CheckEquals('2) Method ''MyFunction''`s return type is not documented.',
        FGenericMethodDecl.DocConflict(2));
    Finally
      C.Free;
    End;
    FGenericMethodDecl.DeleteDocumentConflicts;
    C := TComment.Create(
      'This is a description.'#13#10'@precon None.'#13#10 +
      '@postcon None.'#13#10 +
      '@param param1 as an String'#13#10 +
      '@param param2 as an Array of Integer as a reference'#13#10 +
      '@param param3 as an Byte as a constant'#13#10 +
      '@param param4 as a Double as an out parameter'#13#10 +
      '@return a Thingy', 0, 0);
    Try
      FGenericMethodDecl.Comment := C;
      FGenericMethodDecl.CheckDocumentation(boolCascade);
      CheckEquals('1) Method ''MyFunction''`s return type is incorrect (''Boolean'').',
        FGenericMethodDecl.DocConflict(1));
    Finally
      C.Free;
    End;
    M := TTestGenericMethodDecl.Create(mtProcedure, 'MyProcedure', scPrivate, 0, 0);
    Try
      M.DeleteDocumentConflicts;
      C := TComment.Create(
        'This is a description.'#13#10'@precon None.'#13#10 +
        '@postcon None.'#13#10 +
        '@return a Thingy', 0, 0);
      Try
        M.Comment := C;
        M.CheckDocumentation(boolCascade);
        CheckEquals('1) Method ''MyProcedure''`s return type is not required.',
          M.DocConflict(1));
      Finally
        C.Free;
      End;
    Finally
      M.Free;
    End;

    FGenericMethodDecl.DeleteDocumentConflicts;
    C := TComment.Create(
      'This is a description.'#13#10'@precon None.'#13#10 +
      '@postcon None.'#13#10 +
      '@param param1 as an String - a string'#13#10 +
      '@param param2 as an Array of Integer as a reference - an array'#13#10 +
      '@param param3 as an Byte as a constant - a constant'#13#10 +
      '@param param4 as a Double as an out parameter - a double'#13#10 +
      '@return a Boolean - something', 0, 0);
    Try
      FGenericMethodDecl.Comment := C;
      FGenericMethodDecl.CheckDocumentation(boolCascade);
      CheckEquals(0, FGenericMethodDecl.HeadingCount(strDocumentationConflicts),
        FGenericMethodDecl.DocConflict(1));
    Finally
      C.Free;
    End;

  Finally
    AType.Free;
  End;
End;

Procedure TestTGenericMethodDecl.TestClassMethod;
Begin
  CheckEquals(False, FGenericMethodDecl.ClassMethod);
  FGenericMethodDecl.ClassMethod := True;
  CheckEquals(True, FGenericMethodDecl.ClassMethod);
End;

Procedure TestTGenericMethodDecl.TestClassNames;
Begin
  CheckEquals(0, FGenericMethodDecl.ClassNames.Count);
  FGenericMethodDecl.ClassNames.Add('THello');
  CheckEquals(1, FGenericMethodDecl.ClassNames.Count);
  CheckEquals('THello', FGenericMethodDecl.ClassNames[0]);
End;

Procedure TestTGenericMethodDecl.TestCreate;
Begin
  CheckEquals('MyFunction', FGenericMethodDecl.Identifier);
  CheckEquals(scProtected, FGenericMethodDecl.Scope);
  CheckEquals(34, FGenericMethodDecl.Line);
  CheckEquals(45, FGenericMethodDecl.Column);
  CheckEquals(iiPublicFunction, FGenericMethodDecl.ImageIndex);
  CheckEquals(iiProtectedFunction, FGenericMethodDecl.ImageIndexAdjustedForScope);
End;

Procedure TestTGenericMethodDecl.TestExt;
Begin
  CheckEquals('', FGenericMethodDecl.Ext);
  FGenericMethodDecl.Ext := 'MyFunctionA';
  CheckEquals('MyFunctionA', FGenericMethodDecl.Ext);
End;

Procedure TestTGenericMethodDecl.TestForwardDecl;
Begin
  CheckEquals(False, FGenericMethodDecl.ForwardDecl);
  FGenericMethodDecl.ForwardDecl := True;
  CheckEquals(True, FGenericMethodDecl.ForwardDecl);
End;

Procedure TestTGenericMethodDecl.TestMethodType;

Const
  strMT: Array [Low(TMethodType) .. High(TMethodType)] Of String = (
    'mtConstructor', 'mtDestructor', 'mtProcedure', 'mtFunction', 'mtOperator');

Begin
  CheckEquals(strMT[mtFunction], strMT[FGenericMethodDecl.MethodType]);
End;

Procedure TestTGenericMethodDecl.TestMsg;
Begin
  CheckEquals('', FGenericMethodDecl.Msg);
  FGenericMethodDecl.Msg := 'WM_PAINT';
  CheckEquals('WM_PAINT', FGenericMethodDecl.Msg);
End;

Procedure TestTGenericMethodDecl.TestParameterCount;
Var
  AType: TTestGenericTypeDecl;
  P: TTestGenericParameter;
Begin
  AType := TTestGenericTypeDecl.Create('', scNone, 0, 0, iiNone, Nil);
  Try
    AType.AddToken('String');
    P := TTestGenericParameter.Create(pamNone, 'Param1', False, AType, '', scNone, 0, 0);
    FGenericMethodDecl.AddParameter(P);
    CheckEquals(1, FGenericMethodDecl.ParameterCount);
  Finally
    AType.Free;
  End;
End;

Procedure TestTGenericMethodDecl.TestParameters;
Var
  AType: TTestGenericTypeDecl;
  P: TTestGenericParameter;
Begin
  AType := TTestGenericTypeDecl.Create('', scNone, 0, 0, iiNone, Nil);
  Try
    AType.AddToken('String');
    P := TTestGenericParameter.Create(pamNone, 'Param1', False, AType, '', scNone, 0, 0);
    FGenericMethodDecl.AddParameter(P);
    CheckEquals('Param1 = String', FGenericMethodDecl.Parameters[0].AsString(False, False));
  Finally
    AType.Free;
  End;
End;

Procedure TestTGenericMethodDecl.TestQualifiedName;
Begin
  CheckEquals('MyFunction', FGenericMethodDecl.QualifiedName);
  FGenericMethodDecl.ClassNames.Add('THello');
  CheckEquals('THello.MyFunction', FGenericMethodDecl.QualifiedName);
End;

Procedure TestTGenericMethodDecl.TestReturnType;
Begin
  CheckEquals('Boolean', FGenericMethodDecl.ReturnType.AsString(False, False));
End;

Initialization
  RegisterTest('BaseLanguageModule Tests', TestTGenericMethodDecl.Suite);
End.
