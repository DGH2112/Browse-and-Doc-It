Unit Test.BADI.Pascal.ClassDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.ClassDecl;

Type
  TestTClassDecl = Class(TExtendedTestCase)
  Strict Private
    FClassDecl: TClassDecl;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestCheckDocumentation;
    Procedure TestAsString;
    Procedure TestReferenceSymbol;
  End;

Implementation

Uses
  BADI.Types,
  BADI.Comment,
  BADI.TokenInfo, BADI.Functions;

Procedure TestTClassDecl.SetUp;
Begin
  FClassDecl := TClassDecl.Create('MyClass', scPublic, 12, 23, iiPublicClass, Nil);
End;

Procedure TestTClassDecl.TearDown;
Begin
  FClassDecl.Free;
  FClassDecl := Nil;
End;

Procedure TestTClassDecl.TestCheckDocumentation;
Var
  boolCascade: Boolean;
  C: TComment;
Begin
  CheckEquals(0, FClassDecl.ElementCount);
  FClassDecl.CheckDocumentation(boolCascade);
  CheckEquals(1, FClassDecl.ElementCount);
  CheckEquals('1) Class type ''MyClass'' is undocumented.', FClassDecl.DocConflict(1));
  FClassDecl.DeleteDocumentConflicts;
  C := TComment.Create('This is a comment for the class.', 0, 0);
  Try
    FClassDecl.Comment := C;
    FClassDecl.CheckDocumentation(boolCascade);
    CheckEquals(0, FClassDecl.ElementCount);
  Finally
    C.Free;
  End;
End;

Procedure TestTClassDecl.TestCreate;
Begin
  CheckEquals('MyClass', FClassDecl.Identifier);
  CheckEquals(scPublic, FClassDecl.Scope);
  CheckEquals(12, FClassDecl.Line);
  CheckEquals(23, FClassDecl.Column);
  CheckEquals(iiPublicClass, FClassDecl.ImageIndex);
  CheckEquals(BADIImageIndex(iiPublicClass, scPublic), FClassDecl.ImageIndexAdjustedForScope);
End;

Procedure TestTClassDecl.TestAsString;

Var
  H1, H2: TTokenInfo;

Begin
  CheckEquals('MyClass = Class', FClassDecl.AsString(True, False));
  H1 := TTokenInfo.Create('TObject', 0, 0, 0, 7, ttIdentifier);
  Try
    FClassDecl.Heritage.Add(H1, scNone, iiNone, Nil);
    CheckEquals('MyClass = Class(TObject)', FClassDecl.AsString(True, False));
    H2 := TTokenInfo.Create('IMyInterface', 0, 0, 0, 7, ttIdentifier);
    Try
      FClassDecl.Heritage.Add(H2, scNone, iiNone, Nil);
      CheckEquals('MyClass = Class(TObject, IMyInterface)', FClassDecl.AsString(True, False));
      FClassDecl.AbstractClass := True;
      CheckEquals('MyClass = Class Abstract(TObject, IMyInterface)',
        FClassDecl.AsString(True, False));
      FClassDecl.AbstractClass := False;
      FClassDecl.SealedClass := True;
      CheckEquals('MyClass = Class Sealed(TObject, IMyInterface)',
        FClassDecl.AsString(True, False));
      FClassDecl.SealedClass := False;
      FClassDecl.HelperClass := True;
      FClassDecl.HelperClassName := 'TSomething';
      CheckEquals('MyClass = Class Helper(TObject, IMyInterface) For TSomething',
        FClassDecl.AsString(True, False));
    Finally
      H2.Free;
    End;
  Finally
    H1.Free;
  End;
End;

Procedure TestTClassDecl.TestReferenceSymbol;
Var
  AToken: TTokenInfo;
Begin
  AToken := TTokenInfo.Create('Hello', 0, 0, 0, 5, ttIdentifier);
  Try
    CheckEquals(False, FClassDecl.ReferenceSymbol(AToken));
    // : @todo Requires more tests.
  Finally
    AToken.Free;
  End;
End;

Initialization
  RegisterTest('PascalModule Tests', TestTClassDecl.Suite);
End.
