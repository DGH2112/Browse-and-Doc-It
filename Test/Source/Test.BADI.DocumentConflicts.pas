Unit Test.BADI.DocumentConflicts;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.DocIssue;

Type
  TestTDocumentConflict = Class(TExtendedTestCase)
  Strict Private
    FDocumentConflict: TDocumentConflict;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestAsString;
    Procedure TestCommentLine;
    Procedure TestCommentColumn;
  End;

Implementation

Uses
  BADI.Types;

Procedure TestTDocumentConflict.SetUp;
Begin
  FDocumentConflict := TDocumentConflict.Create(['First', 'Second'], 12, 23, 34,
    45, 'This is a message with a first parameter (%s) and a second parameter (%s).',
    'This is a description string.', iiDocConflictMissing);
End;

Procedure TestTDocumentConflict.TearDown;
Begin
  FDocumentConflict.Free;
  FDocumentConflict := Nil;
End;

Procedure TestTDocumentConflict.TestAsString;

Begin
  Checkequals('This is a message with a first parameter (First) and a second ' +
    'parameter (Second).', FDocumentConflict.AsString(True, False));
End;

Procedure TestTDocumentConflict.TestCommentColumn;
Begin
  Checkequals(45, FDocumentConflict.CommentColumn);
End;

Procedure TestTDocumentConflict.TestCommentLine;
Begin
  Checkequals(34, FDocumentConflict.CommentLine);
End;

Procedure TestTDocumentConflict.TestCreate;
Begin
  Checkequals(12, FDocumentConflict.Line);
  Checkequals(23, FDocumentConflict.Column);
End;

Initialization
  RegisterTest('BaseLanguageModule Tests', TestTDocumentConflict.Suite);
End.
