Unit Test.BADI.Generic.Constant;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.generic.Constant;

Type
  TestTGenericConstant = Class(TExtendedTestCase)
  Strict Private
    FGenericConstant: TGenericConstant;
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

Procedure TestTGenericConstant.SetUp;
Begin
  FGenericConstant := TTestGenericConstant.Create('MyConstant', scPublished, 23,
    34, iiPublicConstant, Nil);
End;

Procedure TestTGenericConstant.TearDown;
Begin
  FGenericConstant.Free;
  FGenericConstant := Nil;
End;

Procedure TestTGenericConstant.TestCheckDocumentation;
Var
  boolCascade: Boolean;
  C: TComment;
Begin
  FGenericConstant.CheckDocumentation(boolCascade);
  CheckEquals(1, FGenericConstant.ElementCount);
  CheckEquals('1) Constant ''MyConstant'' is undocumented.', FGenericConstant.DocConflict(1));
  FGenericConstant.DeleteDocumentConflicts;
  C := TComment.Create('This is a comment.', 0, 0);
  Try
    FGenericConstant.Comment := C;
    FGenericConstant.CheckDocumentation(boolCascade);
    CheckEquals(0, FGenericConstant.ElementCount, FGenericConstant.DocConflict(1));
  Finally
    C.Free;
  End;
End;

Procedure TestTGenericConstant.TestCreate;
Begin
  CheckEquals('MyConstant', FGenericConstant.Identifier);
  CheckEquals(scPublished, FGenericConstant.Scope);
  CheckEquals(23, FGenericConstant.Line);
  CheckEquals(34, FGenericConstant.Column);
  CheckEquals(iiPublicConstant, FGenericConstant.ImageIndex);
  CheckEquals(iiPublishedConstant, FGenericConstant.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('BaseLanguageModule Tests', TestTGenericConstant.Suite);
End.
