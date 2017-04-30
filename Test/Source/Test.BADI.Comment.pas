Unit Test.BADI.Comment;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Comment;

Type
  TestTComment = Class(TExtendedTestCase)
  Strict Private
    FComment: TComment;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestCreateComment;
    Procedure TestAssign;
    Procedure TestAssign1;
    Procedure TestAsString;
    Procedure TestFindTag;
    Procedure TestWrap;
    Procedure TestCarriageReturns;
    Procedure TestDoubleAtSign;
    Procedure TestFailure01;
  End;

Implementation

uses
  BADI.Types, BADI.Options;

Procedure TestTComment.SetUp;

Const
  strComment =
    ''#13#10 +
    '  This method does <b>something</b> wonderful.'#13#10 +
    ''#13#10 +
    '  @todo  Requires implementing.'#13#10 +
    '  @see   Something interesting.'#13#10 +
    '  @code  '#13#10 +
    '  Something more'#13#10 +
    '  interesting information.'#13#10 +
    '  @refactor Something even more interesting.'#13#10 +
    '  @code'#13#10 +
    'Something more'#13#10 +
    '  and keep indent!'#13#10 +
    '  @code'#13#10 +
    '  Even more and keep indent'#13#10 +
    'but not here'#13#10 +
    '  @code'#13#10 +
    '  Something more'#13#10 +
    '    and keep indent!'#13#10 +
    '  @code'#13#10 +
    '    Even more and keep indent'#13#10 +
    '  but not here'#13#10 +
    '';
Var
  iTag: Integer;

Begin
  For iTag := 0 To TBADIOptions.BADIOptions.SpecialTags.Count - 1 Do
    If TBADIOptions.BADIOptions.SpecialTags[iTag].FName = 'code' Then
      Begin
        TBADIOptions.BADIOptions.SpecialTags.Delete(iTag);
        Break;
      End;
  TBADIOptions.BADIOptions.SpecialTags.Add(TBADISpecialTag.Create('code', 'Example Code',
    [tpShowInTree..tpFixed]));
  FComment := TComment.Create(strComment, 12, 34);
End;

Procedure TestTComment.TearDown;
Begin
  FComment.Free;
  FComment := Nil;
End;

Procedure TestTComment.TestCarriageReturns;

Var
  C: TComment;

Begin
  C := TComment.CreateComment('This is a comment#that has a carriage ' +
    'return#or 2 in it.', 12, 13);
  Try
    CheckEquals('This is a comment'#13#10'that has a carriage ' +
      'return'#13#10'or 2 in it.', C.AsString(80, False));
  Finally
    C.Free;
  End;
End;

Procedure TestTComment.TestCreate;

Begin
  CheckEquals(12, FComment.TokenCount, 'TokenCount');
  CheckEquals('This', FComment.Tokens[0].Token);
  CheckEquals('method', FComment.Tokens[2].Token);
  CheckEquals('does', FComment.Tokens[4].Token);
  CheckEquals('<b>', FComment.Tokens[6].Token);
  CheckEquals('something', FComment.Tokens[7].Token);
  CheckEquals('</b>', FComment.Tokens[8].Token);
  CheckEquals('wonderful', FComment.Tokens[10].Token);
  CheckEquals('.', FComment.Tokens[11].Token);
  CheckEquals(ttidentifier, FComment.Tokens[0].TokenType);
  CheckEquals(ttidentifier, FComment.Tokens[2].TokenType);
  CheckEquals(ttidentifier, FComment.Tokens[4].TokenType);
  CheckEquals(ttHTMLStartTag, FComment.Tokens[6].TokenType);
  CheckEquals(ttidentifier, FComment.Tokens[7].TokenType);
  CheckEquals(ttHTMLEndTag, FComment.Tokens[8].TokenType);
  CheckEquals(ttidentifier, FComment.Tokens[10].TokenType);
  CheckEquals(ttSymbol, FComment.Tokens[11].TokenType);
  CheckEquals('todo', FComment.Tag[0].TagName);
  CheckEquals('Requires implementing.', FComment.Tag[0].AsString(80, False));
  CheckEquals('see', FComment.Tag[1].TagName);
  CheckEquals('Something interesting.', FComment.Tag[1].AsString(80, False));
  CheckEquals('code', FComment.Tag[2].TagName);
  CheckEquals('Something more'#13#10'interesting information.', FComment.Tag[2].AsString(80, False));
  CheckEquals('refactor', FComment.Tag[3].TagName);
  CheckEquals('Something even more interesting.', FComment.Tag[3].AsString(80, False));
  CheckEquals('code', FComment.Tag[4].TagName);
  CheckEquals('Something more'#13#10'  and keep indent!', FComment.Tag[4].AsString(80, False));
  CheckEquals('code', FComment.Tag[5].TagName);
  CheckEquals('  Even more and keep indent'#13#10'but not here', FComment.Tag[5].AsString(80, False));
  CheckEquals('code', FComment.Tag[6].TagName);
  CheckEquals('Something more'#13#10'  and keep indent!', FComment.Tag[6].AsString(80, False));
  CheckEquals('code', FComment.Tag[7].TagName);
  CheckEquals('  Even more and keep indent'#13#10'but not here', FComment.Tag[7].AsString(80, False));
  CheckEquals(8, FComment.TagCount, 'TagCount');
  CheckEquals(12, FComment.Line, 'Line');
  CheckEquals(34, FComment.Column, 'Column');
End;

Procedure TestTComment.TestCreateComment;

Const
  strComment =
    ''#13#10 +
    '  This method does something <b>wonderful</b>.'#13#10 +
    ''#13#10 +
    '  @todo  Requires <e>implementing</e>.'#13#10 +
    '  @see   Something1 interesting.'#13#10 +
    '';

Var
  ReturnValue: TComment;

Begin
  ReturnValue := FComment.CreateComment(strComment, 12, 23);
  Try
    CheckEquals(12, ReturnValue.TokenCount);
    CheckEquals('This', ReturnValue.Tokens[0].Token);
    CheckEquals('method', ReturnValue.Tokens[2].Token);
    CheckEquals('does', ReturnValue.Tokens[4].Token);
    CheckEquals('something', ReturnValue.Tokens[6].Token);
    CheckEquals('<b>', ReturnValue.Tokens[8].Token);
    CheckEquals('wonderful', ReturnValue.Tokens[9].Token);
    CheckEquals('</b>', ReturnValue.Tokens[10].Token);
    CheckEquals('.', ReturnValue.Tokens[11].Token);
    CheckEquals(ttidentifier, ReturnValue.Tokens[0].TokenType);
    CheckEquals(ttidentifier, ReturnValue.Tokens[2].TokenType);
    CheckEquals(ttidentifier, ReturnValue.Tokens[4].TokenType);
    CheckEquals(ttidentifier, ReturnValue.Tokens[6].TokenType);
    CheckEquals(ttHTMLStartTag, ReturnValue.Tokens[8].TokenType);
    CheckEquals(ttidentifier, ReturnValue.Tokens[9].TokenType);
    CheckEquals(ttHTMLEndTag, ReturnValue.Tokens[10].TokenType);
    CheckEquals(ttSymbol, ReturnValue.Tokens[11].TokenType);
    CheckEquals('todo', ReturnValue.Tag[0].TagName);
    CheckEquals(15, ReturnValue.Tag[0].Line);
    CheckEquals(26, ReturnValue.Tag[0].Column);
    CheckEquals('Requires implementing.', ReturnValue.Tag[0].AsString(80, False));
    CheckEquals('Requires <e>implementing</e>.', ReturnValue.Tag[0].AsString(80, True));
    CheckEquals('see', ReturnValue.Tag[1].TagName);
    CheckEquals(16, ReturnValue.Tag[1].Line);
    CheckEquals(26, ReturnValue.Tag[1].Column);
    CheckEquals('Something1 interesting.', ReturnValue.Tag[1].AsString(80, False));
    CheckEquals(2, ReturnValue.TagCount);
    CheckEquals(12, ReturnValue.Line);
    CheckEquals(23, ReturnValue.Column);
  Finally
    ReturnValue.Free;
  End;
End;

Procedure TestTComment.TestDoubleAtSign;

Var
  C: TComment;

Begin
  C := TComment.CreateComment('This is a test for the @@token.', 12, 23);
  Try
    CheckEquals(14, C.TokenCount);
    CheckEquals('This', C.Tokens[0].Token);
    CheckEquals('is', C.Tokens[2].Token);
    CheckEquals('a', C.Tokens[4].Token);
    CheckEquals('test', C.Tokens[6].Token);
    CheckEquals('for', C.Tokens[8].Token);
    CheckEquals('the', C.Tokens[10].Token);
    CheckEquals('@@token', C.Tokens[12].Token);
    CheckEquals('This is a test for the @token.', C.AsString(80, False));
  Finally
    C.Free;
  End;
End;

Procedure TestTComment.TestAssign;

Var
  srcComment: TComment;

Begin
  srcComment := TComment.Create(Nil);
  Try
    srcComment.Assign(FComment);
    CheckEquals(12, srcComment.TokenCount, 'TokenCount');
    CheckEquals('This', srcComment.Tokens[0].Token);
    CheckEquals('method', srcComment.Tokens[2].Token);
    CheckEquals('does', srcComment.Tokens[4].Token);
    CheckEquals('something', srcComment.Tokens[7].Token);
    CheckEquals('wonderful', srcComment.Tokens[10].Token);
    CheckEquals('.', srcComment.Tokens[11].Token);
    CheckEquals(ttidentifier, srcComment.Tokens[0].TokenType);
    CheckEquals(ttidentifier, srcComment.Tokens[2].TokenType);
    CheckEquals(ttidentifier, srcComment.Tokens[4].TokenType);
    CheckEquals(ttidentifier, srcComment.Tokens[7].TokenType);
    CheckEquals(ttidentifier, srcComment.Tokens[10].TokenType);
    CheckEquals(ttSymbol, srcComment.Tokens[11].TokenType);
    CheckEquals('todo', srcComment.Tag[0].TagName);
    CheckEquals('Requires implementing.', srcComment.Tag[0].AsString(80, False));
    CheckEquals('see', srcComment.Tag[1].TagName);
    CheckEquals('Something interesting.', srcComment.Tag[1].AsString(80, False));
    CheckEquals('code', FComment.Tag[2].TagName);
    CheckEquals('Something more'#13#10'interesting information.', FComment.Tag[2].AsString(80, False));
    CheckEquals('refactor', FComment.Tag[3].TagName);
    CheckEquals('Something even more interesting.', FComment.Tag[3].AsString(80, False));
    CheckEquals('code', FComment.Tag[4].TagName);
    CheckEquals('Something more'#13#10'  and keep indent!', FComment.Tag[4].AsString(80, False));
    CheckEquals('code', FComment.Tag[5].TagName);
    CheckEquals('  Even more and keep indent'#13#10'but not here', FComment.Tag[5].AsString(80, False));
    CheckEquals('code', FComment.Tag[6].TagName);
    CheckEquals('Something more'#13#10'  and keep indent!', FComment.Tag[6].AsString(80, False));
    CheckEquals('code', FComment.Tag[7].TagName);
    CheckEquals('  Even more and keep indent'#13#10'but not here', FComment.Tag[7].AsString(80, False));
    CheckEquals(8, srcComment.TagCount, 'TagCount');
    CheckEquals(12, srcComment.Line, 'Line');
    CheckEquals(34, srcComment.Column, 'Column');
  Finally
    srcComment.Free;
  End;
End;

Procedure TestTComment.TestAssign1;

Const
  strComment =
    ''#13#10 +
    '  This method does something wonderful.'#13#10 +
    ''#13#10 +
    '  @todo  Requires implementing.'#13#10 +
    '  @see   Something interesting.'#13#10 +
    '';

Var
  C: TComment;

Begin
  C := TComment.Create(Nil);
  Try
    C.Assign(strComment);
    CheckEquals(10, C.TokenCount);
    CheckEquals('This', C.Tokens[0].Token);
    CheckEquals('method', C.Tokens[2].Token);
    CheckEquals('does', C.Tokens[4].Token);
    CheckEquals('something', C.Tokens[6].Token);
    CheckEquals('wonderful', C.Tokens[8].Token);
    CheckEquals('.', C.Tokens[9].Token);
    CheckEquals(ttidentifier, C.Tokens[0].TokenType);
    CheckEquals(ttidentifier, C.Tokens[2].TokenType);
    CheckEquals(ttidentifier, C.Tokens[4].TokenType);
    CheckEquals(ttidentifier, C.Tokens[6].TokenType);
    CheckEquals(ttidentifier, C.Tokens[8].TokenType);
    CheckEquals(ttSymbol, C.Tokens[9].TokenType);
    CheckEquals('todo', C.Tag[0].TagName);
    CheckEquals('Requires implementing.', C.Tag[0].AsString(80, False));
    CheckEquals('see', C.Tag[1].TagName);
    CheckEquals('Something interesting.', C.Tag[1].AsString(80, False));
    CheckEquals(2, C.TagCount);
    CheckEquals(0, C.Line);
    CheckEquals(0, C.Column);
  Finally
    C.Free;
  End;
End;

Procedure TestTComment.TestAsString;

Begin
  CheckEquals('This method does something wonderful.', FComment.AsString(80, False));
  CheckEquals('This method does <b>something</b> wonderful.', FComment.AsString(80, True));
End;

Procedure TestTComment.TestFailure01;

Var
  C: TComment;

Begin
  C := TComment.Create(
    ' This is a comment.'#13#10 +
    ''#13#10 +
    '@todo This is the first todo item.'#13#10 +
    '@todo This is the second todo item.'#13#10 +
    '@todo This is the third todo item.'#13#10, 0, 0);
  Try
    CheckEquals(8, C.TokenCount);
    CheckEquals(3, C.TagCount);
    CheckEquals('This is the first todo item.', C.Tag[0].AsString(80, False));
    CheckEquals('This is the second todo item.', C.Tag[1].AsString(80, False));
    CheckEquals('This is the third todo item.', C.Tag[2].AsString(80, False));
  Finally
    C.Free;
  End;
End;

Procedure TestTComment.TestFindTag;

Begin
  CheckEquals( - 1, FComment.FindTag('hello'));
  CheckEquals(0, FComment.FindTag('todo'));
  CheckEquals(1, FComment.FindTag('see'));
End;

Procedure TestTComment.TestWrap;

Var
  C: TComment;

Begin
  C := TComment.CreateComment('This is a comment that should get wrap at the ' +
    '80th column of text, so will see if this works.', 12, 13);
  Try
    CheckEquals('This is a comment that should get wrap at the 80th column of ' +
      'text, so will see '#13#10'if this works.', C.AsString(80, False));
  Finally
    C.Free;
  End;
End;

Initialization
  RegisterTest('BaseLanguageModule Tests', TestTComment.Suite);
End.
