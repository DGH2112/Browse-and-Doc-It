Unit Test.BADI.VB.Comment;

Interface

Uses
  TestFramework;

Type
  //
  // Test Class for the TVBComment Class Methods.
  //
  TestTVBComment = Class(TTestCase)
  Strict Private
  Public
  Published
    Procedure TestCreateComment;
  End;

Implementation

uses
  BADI.Comment,
               BADI.VB.Comment;

//
// Test methods for the class TVBComment.
//
Procedure TestTVBComment.TestCreateComment;

Var
  C: TComment;

Begin
  C := TVBComment.CreateComment('', 0, 0);
  Try
    Check(C = Nil, 'Comment is not NULL!');
  Finally
    C.Free;
  End;
  C := TVBComment.CreateComment(''' This is a standard VB Comment.', 0, 0);
  Try
    Check(C = Nil, 'Comment is not NULL!');
  Finally
    C.Free;
  End;
  C := TVBComment.CreateComment(''''' This is a standard VB Comment.', 0, 0);
  Try
    Check(C <> Nil, 'Comment is NULL!');
    CheckEquals('This is a standard VB Comment.', C.AsString(9999, True));
  Finally
    C.Free;
  End;
  C := TVBComment.CreateComment(''': This is a standard VB Comment.', 0, 0);
  Try
    Check(C <> Nil, 'Comment is NULL!');
    CheckEquals('This is a standard VB Comment.', C.AsString(9999, True));
  Finally
    C.Free;
  End;
  C := TVBComment.CreateComment(
    ''': This is a standard VB Comment.'#13#10 +
    ''': @todo Hello Dave.'#13#10, 0, 0);
  Try
    Check(C <> Nil, 'Comment is NULL!');
    CheckEquals('This is a standard VB Comment.', C.AsString(9999, True));
    CheckEquals(1, C.TagCount);
    CheckEquals('Hello Dave.', C.Tag[0].AsString(80, True));
  Finally
    C.Free;
  End;
End;

initialization
  RegisterTest('VB Module Tests', TestTVBComment.Suite);
End.
