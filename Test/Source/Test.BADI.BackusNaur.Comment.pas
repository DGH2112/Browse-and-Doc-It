Unit Test.BADI.BackusNaur.Comment;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.BackusNaur.Comment;

Type
  //
  // Test Class for the TBackusNaurComment Class Methods.
  //
  TestTBackusNaurComment = Class(TExtendedTestCase)
  Strict Private
    FBackusNaurComment : TBackusNaurComment;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreateComment;
  End;

Implementation

Uses
  BADI.Comment;

//
// Test Methods for Class TBackusNaurComment.
//
Procedure TestTBackusNaurComment.SetUp;

Begin
  FBackusNaurComment := TBackusNaurComment.Create('This is a comment.', 12, 23);
End;

Procedure TestTBackusNaurComment.TearDown;

Begin
  FBackusNaurComment.Free;
End;

Procedure TestTBackusNaurComment.TestCreateComment;

Var
  C: TComment;

Begin
  Checkequals(8, FBackusNaurComment.TokenCount);
  Checkequals(12, FBackusNaurComment.Line);
  Checkequals(23, FBackusNaurComment.Column);
  C := TBackusNaurComment.CreateComment('/* This is a comment. */', 1, 2);
  Check(C = Nil, '/* Comment */ is not NULL');
  C := TBackusNaurComment.CreateComment('// This is a comment.', 1, 2);
  Check(C = Nil, '// Comment is not NULL');
  C := TBackusNaurComment.CreateComment('/** This is a comment. **/', 1, 2);
  Try
    Check(C <> Nil, '/** Comment **/ is NULL');
    Checkequals(8, C.TokenCount);
    Checkequals(1, C.Line);
    Checkequals(2, C.Column);
  Finally
    C.Free;
  End;
  C := TBackusNaurComment.CreateComment('//: This is a comment.', 1, 2);
  Try
    Check(C <> Nil, '//: Comment is NULL');
    Checkequals(8, C.TokenCount);
    Checkequals(1, C.Line);
    Checkequals(2, C.Column);
  Finally
    C.Free;
  End;
End;

Initialization
  RegisterTest('Backus-Naur', TestTBackusNaurComment.Suite);
End.
