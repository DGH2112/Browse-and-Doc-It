Unit Test.BADI.Eidolon.Comment;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Eidolon.Comment;

Type
  //
  // Test Class for the TEidolonComment Class Methods.
  //
  TestTEidolonComment = Class(TExtendedTestCase)
  Strict Private
    FEidolonComment : TEidolonComment;
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
// Test Methods for Class TEidolonComment.
//
Procedure TestTEidolonComment.Setup;
Begin
  FEidolonComment := TEidolonComment.Create('This is a comment!', 12, 23);
End;

Procedure TestTEidolonComment.TearDown;

Begin
  FEidolonComment.Free;
End;

Procedure TestTEidolonComment.TestCreateComment;

Var
  C: TComment;

Begin
  C := TEidolonComment.CreateComment('/** This is a comment! **/', 12, 23);
  Try
    CheckEquals('This is a comment!', C.AsString(9999, False));
    CheckEquals(12, C.Line);
    CheckEquals(23, C.Column);
  Finally
    C.Free;
  End;
End;

Initialization
  RegisterTest('Eidolon Module', TestTEidolonComment.Suite);
End.
