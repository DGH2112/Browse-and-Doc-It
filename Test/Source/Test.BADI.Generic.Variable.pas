Unit Test.BADI.Generic.Variable;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Generic.Variable;

Type
  TestTGenericVariable = Class(TExtendedTestCase)
  Strict Private
    FGenericVariable: TGenericVariable;
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

Procedure TestTGenericVariable.SetUp;
Begin
  FGenericVariable := TTestGenericVariable.Create('MyVariable', scPublic, 23,
    34, iiPublicConstant, Nil);
End;

Procedure TestTGenericVariable.TearDown;
Begin
  FGenericVariable.Free;
  FGenericVariable := Nil;
End;

Procedure TestTGenericVariable.TestCheckDocumentation;
Var
  boolCascade: Boolean;
  C: TComment;
Begin
  FGenericVariable.CheckDocumentation(boolCascade);
  CheckEquals(1, FGenericVariable.ElementCount);
  CheckEquals('1) Variable ''MyVariable'' is undocumented.', FGenericVariable.DocConflict(1));
  FGenericVariable.DeleteDocumentConflicts;
  C := TComment.Create('This is a comment.', 0, 0);
  Try
    FGenericVariable.Comment := C;
    FGenericVariable.CheckDocumentation(boolCascade);
    CheckEquals(0, FGenericVariable.ElementCount, FGenericVariable.DocConflict(1));
  Finally
    C.Free;
  End;
End;

Procedure TestTGenericVariable.TestCreate;
Begin
  CheckEquals('MyVariable', FGenericVariable.Identifier);
  CheckEquals(scPublic, FGenericVariable.Scope);
  CheckEquals(23, FGenericVariable.Line);
  CheckEquals(34, FGenericVariable.Column);
  CheckEquals(BADIImageIndex(iiPublicConstant, scPublic), FGenericVariable.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('BaseLanguageModule Tests', TestTGenericVariable.Suite);
End.
