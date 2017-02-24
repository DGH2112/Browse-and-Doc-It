Unit Test.BADI.Pascal.FinalizationDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.FinalizationDecl;

Type
  TestTFinalizationSection = Class(TExtendedTestCase)
  Strict Private
    FFinalizationSection: TFinalizationSection;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestAsString;
    Procedure TestCheckDocumentation;
  End;

Implementation

Uses
  BADI.Types,
  BADI.Comment;

Procedure TestTFinalizationSection.SetUp;
Begin
  FFinalizationSection := TFinalizationSection.Create('Finalization', scNone, 12, 23,
    iiFinalization, Nil);
End;

Procedure TestTFinalizationSection.TearDown;
Begin
  FFinalizationSection.Free;
  FFinalizationSection := Nil;
End;

Procedure TestTFinalizationSection.TestAsString;
Begin
  Checkequals('Finalization', FFinalizationSection.AsString(True, False));
End;

Procedure TestTFinalizationSection.TestCheckDocumentation;
Var
  boolCascade: Boolean;
  C: TComment;
Begin
  Checkequals(0, FFinalizationSection.ElementCount);
  FFinalizationSection.CheckDocumentation(boolCascade);
  Checkequals(1, FFinalizationSection.ElementCount);
  Checkequals('1) The module is missing an Finalization comment.',
    FFinalizationSection.DocConflict(1));
  FFinalizationSection.DeleteDocumentConflicts;
  C := TComment.Create('This is a comment for the finalisation section.', 0, 0);
  Try
    FFinalizationSection.Comment := C;
    FFinalizationSection.CheckDocumentation(boolCascade);
    Checkequals(0, FFinalizationSection.ElementCount);
  Finally
    C.Free;
  End;
End;

Procedure TestTFinalizationSection.TestCreate;
Begin
  Checkequals('Finalization', FFinalizationSection.Identifier);
  Checkequals(scNone, FFinalizationSection.Scope);
  Checkequals(12, FFinalizationSection.Line);
  Checkequals(23, FFinalizationSection.Column);
  Checkequals(iiFinalization, FFinalizationSection.ImageIndex);
  Checkequals(iiFinalization, FFinalizationSection.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('PascalModule Tests', TestTFinalizationSection.Suite);
End.
