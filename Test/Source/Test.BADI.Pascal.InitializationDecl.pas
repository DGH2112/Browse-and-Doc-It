Unit Test.BADI.Pascal.InitializationDecl;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Pascal.InitializationDecl;

Type
  TestTInitializationSection = Class(TExtendedTestCase)
  Strict Private
    FInitializationSection: TInitializationSection;
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

Procedure TestTInitializationSection.SetUp;
Begin
  FInitializationSection := TInitializationSection.Create('Initialization', scNone, 12, 23,
    iiInitialization, Nil);
End;

Procedure TestTInitializationSection.TearDown;
Begin
  FInitializationSection.Free;
  FInitializationSection := Nil;
End;

Procedure TestTInitializationSection.TestAsString;
Begin
  Checkequals('Initialization', FInitializationSection.AsString(True, False));
End;

Procedure TestTInitializationSection.TestCheckDocumentation;
Var
  boolCascade: Boolean;
  C: TComment;
Begin
  Checkequals(0, FInitializationSection.ElementCount);
  FInitializationSection.CheckDocumentation(boolCascade);
  Checkequals(1, FInitializationSection.ElementCount);
  Checkequals('1) The module is missing an Initialization comment.',
    FInitializationSection.DocConflict(1));
  FInitializationSection.DeleteDocumentConflicts;
  C := TComment.Create('This is a comment for the initialisation section.', 0, 0);
  Try
    FInitializationSection.Comment := C;
    FInitializationSection.CheckDocumentation(boolCascade);
    Checkequals(0, FInitializationSection.ElementCount);
  Finally
    C.Free;
  End;
End;

Procedure TestTInitializationSection.TestCreate;
Begin
  Checkequals('Initialization', FInitializationSection.Identifier);
  Checkequals(scNone, FInitializationSection.Scope);
  Checkequals(12, FInitializationSection.Line);
  Checkequals(23, FInitializationSection.Column);
  Checkequals(iiInitialization, FInitializationSection.ImageIndex);
  Checkequals(iiInitialization, FInitializationSection.ImageIndexAdjustedForScope);
End;

Initialization
  RegisterTest('PascalModule Tests', TestTInitializationSection.Suite);
End.
