Unit Test.BADI.Pascal.ThreadVariable;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.ThreadVariableDecl;

Type
  TestTThreadVar = Class(TExtendedTestCase)
  Strict Private
    FThreadVar: TThreadVar;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestCheckDocumentation;
  End;
  // Test methods for class TField

Implementation

Uses
  BADI.Types,
  BADI.Comment,
  BADI.Functions;

Procedure TestTThreadVar.SetUp;
Begin
  FThreadVar := TThreadVar.Create('MyThreadVar', scPrivate, 12, 23, iiPublicThreadVar, Nil);
End;

Procedure TestTThreadVar.TearDown;
Begin
  FThreadVar.Free;
  FThreadVar := Nil;
End;

Procedure TestTThreadVar.TestCheckDocumentation;
Var
  boolCascade: Boolean;
  C: TComment;
Begin
  CheckEquals(0, FThreadVar.ElementCount);
  FThreadVar.CheckDocumentation(boolCascade);
  CheckEquals(1, FThreadVar.ElementCount);
  CheckEquals('1) Thread variable ''MyThreadVar'' is undocumented.', FThreadVar.DocConflict(1));
  FThreadVar.DeleteDocumentConflicts;
  C := TComment.Create('This is a comment for the threadvar.', 0, 0);
  Try
    FThreadVar.Comment := C;
    FThreadVar.CheckDocumentation(boolCascade);
    CheckEquals(0, FThreadVar.ElementCount);
  Finally
    C.Free;
  End;
End;

Procedure TestTThreadVar.TestCreate;
Begin
  CheckEquals('MyThreadVar', FThreadVar.Identifier);
  CheckEquals(scPrivate, FThreadVar.Scope);
  CheckEquals(12, FThreadVar.Line);
  CheckEquals(23, FThreadVar.Column);
  CheckEquals(iiPublicThreadVar, FThreadVar.ImageIndex);
  CheckEquals(BADIImageIndex(iiPublicThreadVar, scPrivate), FThreadVar.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('PascalModule Tests', TestTThreadVar.Suite);
End.
