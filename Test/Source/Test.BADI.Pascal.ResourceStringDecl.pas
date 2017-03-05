Unit Test.BADI.Pascal.ResourceStringDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.ResourceStringDecl;

Type
  TestTResourceString = Class(TExtendedTestCase)
  Strict Private
    FResourceString: TResourceString;
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
  BADI.Comment,
  BADI.Functions;

Procedure TestTResourceString.SetUp;
Begin
  FResourceString := TResourceString.Create('MyResourceString', scPublic, 12, 23,
    iiPublicResourceString, Nil);
  FResourceString.AddToken('''This is a string literal.''');
End;

Procedure TestTResourceString.TearDown;
Begin
  FResourceString.Free;
  FResourceString := Nil;
End;

Procedure TestTResourceString.TestCheckDocumentation;
Var
  boolCascade: Boolean;
  C: TComment;
Begin
  CheckEquals(0, FResourceString.ElementCount);
  FResourceString.CheckDocumentation(boolCascade);
  CheckEquals(1, FResourceString.ElementCount);
  CheckEquals('1) Resource string ''MyResourceString'' is undocumented.',
    FResourceString.DocConflict(1));
  FResourceString.DeleteDocumentConflicts;
  C := TComment.Create('This is a comment for the resource string.', 0, 0);
  Try
    FResourceString.Comment := C;
    FResourceString.CheckDocumentation(boolCascade);
    CheckEquals(0, FResourceString.ElementCount);
  Finally
    C.Free;
  End;
End;

Procedure TestTResourceString.TestCreate;
Begin
  CheckEquals('MyResourceString', FResourceString.Identifier);
  CheckEquals(scPublic, FResourceString.Scope);
  CheckEquals(12, FResourceString.Line);
  CheckEquals(23, FResourceString.Column);
  CheckEquals(BADIImageIndex(iiPublicResourceString, scPublic), FResourceString.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('PascalModule Tests', TestTResourceString.Suite);
End.
