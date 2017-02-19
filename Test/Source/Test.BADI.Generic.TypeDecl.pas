Unit Test.BADI.Generic.TypeDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Generic.TypeDecl;

Type
  TestTGenericTypeDecl = Class(TExtendedTestCase)
  Strict Private
    FGenericTypeDecl: TGenericTypeDecl;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestCheckDocumentation;
  End;

Implementation

Uses
  BADI.Types,
  BADI.Comment;

Procedure TestTGenericTypeDecl.SetUp;
Begin
  FGenericTypeDecl := TTestGenericTypeDecl.Create('MyType', scProtected, 23, 34,
    iiPublicType, Nil);
End;

Procedure TestTGenericTypeDecl.TearDown;
Begin
  FGenericTypeDecl.Free;
  FGenericTypeDecl := Nil;
End;

Procedure TestTGenericTypeDecl.TestCheckDocumentation;
Var
  boolCascade: Boolean;
  C: TComment;
Begin
  FGenericTypeDecl.CheckDocumentation(boolCascade);
  CheckEquals(1, FGenericTypeDecl.ElementCount);
  CheckEquals('1) Type ''MyType'' is undocumented.', FGenericTypeDecl.DocConflict(1));
  FGenericTypeDecl.DeleteDocumentConflicts;
  C := TComment.Create('This is a comment.', 0, 0);
  Try
    FGenericTypeDecl.Comment := C;
    FGenericTypeDecl.CheckDocumentation(boolCascade);
    CheckEquals(0, FGenericTypeDecl.ElementCount, FGenericTypeDecl.DocConflict(1));
  Finally
    C.Free;
  End;
End;

Procedure TestTGenericTypeDecl.TestCreate;
Begin
  CheckEquals('MyType', FGenericTypeDecl.Identifier);
  CheckEquals(scProtected, FGenericTypeDecl.Scope);
  CheckEquals(23, FGenericTypeDecl.Line);
  CheckEquals(34, FGenericTypeDecl.Column);
  CheckEquals(iiPublicType, FGenericTypeDecl.ImageIndex);
  CheckEquals(iiProtectedType, FGenericTypeDecl.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('BaseLanguageModule Tests', TestTGenericTypeDecl.Suite);
End.
