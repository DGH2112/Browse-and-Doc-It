Unit Test.BADI.Comment.Tag;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.Comment.Tag;

Type
  TestTTag = Class(TExtendedTestCase)
  Strict Private
    FTag: TTag;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestAddToken;
    Procedure TestAsString;
  End;

Implementation

Uses
  BADI.Types;

Procedure TestTTag.SetUp;

Begin
  FTag := TTag.Create('todo', 12, 23);
End;

Procedure TestTTag.TearDown;

Begin
  FTag.Free;
  FTag := Nil;
End;

Procedure TestTTag.TestAddToken;

Begin
  FTag.AddToken('Hello', ttIdentifier);
  FTag.AddToken('Dave', ttIdentifier);
  FTag.AddToken('.', ttSymbol);
  CheckEquals(3, FTag.TokenCount);
  CheckEquals('Hello', FTag.Tokens[0].Token);
  CheckEquals(ttIdentifier, FTag.Tokens[0].TokenType);
  CheckEquals('Dave', FTag.Tokens[1].Token);
  CheckEquals(ttIdentifier, FTag.Tokens[1].TokenType);
  CheckEquals('.', FTag.Tokens[2].Token);
  CheckEquals(ttSymbol, FTag.Tokens[2].TokenType);
End;

Procedure TestTTag.TestAsString;

Begin
  FTag.AddToken('Hello', ttIdentifier);
  FTag.AddToken(#32, ttWhiteSpace);
  FTag.AddToken('<b>', ttHTMLStartTag);
  FTag.AddToken('Dave', ttIdentifier);
  FTag.AddToken('</b>', ttHTMLEndTag);
  FTag.AddToken('.', ttSymbol);
  CheckEquals(ttHTMLStartTag, FTag.Tokens[2].TokenType);
  CheckEquals(ttHTMLEndTag, FTag.Tokens[4].TokenType);
  CheckEquals(6, FTag.TokenCount);
  CheckEquals('Hello Dave.', FTag.AsString(80, False));
  CheckEquals('Hello <b>Dave</b>.', FTag.AsString(80, True));
End;

Procedure TestTTag.TestCreate;

Begin
  CheckEquals('todo', FTag.TagName);
  CheckEquals(12, FTag.Line);
  CheckEquals(23, FTag.Column);
End;

Initialization
  RegisterTest('BaseLanguageModule Tests', TestTTag.Suite);
End.
