Unit Test.BADI.Pascal.Comment;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module;

Type
  TestTPascalComment = Class(TExtendedTestCase)
  Strict Private
  Public
  Published
    Procedure TestCreateComment;
  End;

Implementation

Uses
  BADI.Comment,
  BADI.Pascal.Comment;

Procedure TestTPascalComment.TestCreateComment;

Var
  ReturnValue: TComment;

Begin
  ReturnValue := TPascalComment.CreateComment('(** Sorry Dave, I can`t do that! **)', 12, 23);
  Try
    CheckEquals(15, ReturnValue.TokenCount);
    CheckEquals('Sorry', ReturnValue.Tokens[0].Token);
    CheckEquals('Dave', ReturnValue.Tokens[2].Token);
    CheckEquals(',', ReturnValue.Tokens[3].Token);
    CheckEquals('I', ReturnValue.Tokens[5].Token);
    CheckEquals('can', ReturnValue.Tokens[7].Token);
    CheckEquals('`', ReturnValue.Tokens[8].Token);
    CheckEquals('t', ReturnValue.Tokens[9].Token);
    CheckEquals('do', ReturnValue.Tokens[11].Token);
    CheckEquals('that', ReturnValue.Tokens[13].Token);
    CheckEquals('!', ReturnValue.Tokens[14].Token);
    CheckEquals('Sorry Dave, I can`t do that!', ReturnValue.AsString(999, True));
  Finally
    ReturnValue.Free;
  End;
  ReturnValue := TPascalComment.CreateComment('//: Sorry Dave, I can`t do that!', 12, 23);
  Try
    CheckEquals(15, ReturnValue.TokenCount);
    CheckEquals('Sorry', ReturnValue.Tokens[0].Token);
    CheckEquals('Dave', ReturnValue.Tokens[2].Token);
    CheckEquals(',', ReturnValue.Tokens[3].Token);
    CheckEquals('I', ReturnValue.Tokens[5].Token);
    CheckEquals('can', ReturnValue.Tokens[7].Token);
    CheckEquals('`', ReturnValue.Tokens[8].Token);
    CheckEquals('t', ReturnValue.Tokens[9].Token);
    CheckEquals('do', ReturnValue.Tokens[11].Token);
    CheckEquals('that', ReturnValue.Tokens[13].Token);
    CheckEquals('!', ReturnValue.Tokens[14].Token);
    CheckEquals('Sorry Dave, I can`t do that!', ReturnValue.AsString(999, True));
  Finally
    ReturnValue.Free;
  End;
  ReturnValue := TPascalComment.CreateComment('{: Sorry Dave, I can`t do that! }', 12, 23);
  Try
    CheckEquals(15, ReturnValue.TokenCount);
    CheckEquals('Sorry', ReturnValue.Tokens[0].Token);
    CheckEquals('Dave', ReturnValue.Tokens[2].Token);
    CheckEquals(',', ReturnValue.Tokens[3].Token);
    CheckEquals('I', ReturnValue.Tokens[5].Token);
    CheckEquals('can', ReturnValue.Tokens[7].Token);
    CheckEquals('`', ReturnValue.Tokens[8].Token);
    CheckEquals('t', ReturnValue.Tokens[9].Token);
    CheckEquals('do', ReturnValue.Tokens[11].Token);
    CheckEquals('that', ReturnValue.Tokens[13].Token);
    CheckEquals('!', ReturnValue.Tokens[14].Token);
    CheckEquals('Sorry Dave, I can`t do that!', ReturnValue.AsString(999, True));
  Finally
    ReturnValue.Free;
  End;
End;

Initialization
  RegisterTest('PascalModule Tests', TestTPascalComment.Suite);
End.
