Unit TestBackusNaurModule;

Interface

Uses
  TestFramework, BaseLanguageModule, TestBaseLanguageModule, BackusNaurModule;

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

  //
  // Test Class for the TBackusNaurModule Class Methods.
  //
  TestTBackusNaurModule = Class(TExtendedTestCase)
  Strict Private
    FBackusNaurModule : TBackusNaurModule;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestAsString;
    Procedure TestCreateParser;
    Procedure TestTokenizeStream;
    Procedure TestSyntax;
    Procedure TestRule;
    Procedure TestExpression;
    Procedure TestList;
    Procedure TestFailures01;
    Procedure TestFailures02;
    Procedure TestFailures03;
  End;

Implementation

uses ModuleDispatcher;

//
// Test Methods for Class TBackusNaurComment.
//
Procedure TestTBackusNaurComment.Setup;
Begin
  FBackusNaurComment := TBackusNaurComment.Create('This is a comment.', 12, 23);
End;

Procedure TestTBackusNaurComment.TearDown;

Begin
  FBackusNaurComment.Free;
End;

Procedure TestTBackusNaurComment.TestCreateComment;

var
  C: TComment;

Begin
  Checkequals(8, FBackusNaurComment.TokenCount);
  CheckEquals(12, FBackusNaurComment.Line);
  CheckEquals(23, FBackusNaurComment.Column);
  C := TBackusNaurComment.CreateComment('/* This is a comment. */', 1, 2);
  Check(C = Nil, '/* Comment */ is not NULL');
  C := TBackusNaurComment.CreateComment('// This is a comment.', 1, 2);
  Check(C = Nil, '// Comment is not NULL');
  C := TBackusNaurComment.CreateComment('/** This is a comment. **/', 1, 2);
  Try
    Check(C <> Nil, '/** Comment **/ is NULL');
    CheckEquals(8, C.TokenCount);
    CheckEquals(1, C.Line);
    CheckEquals(2, C.Column);
  Finally
    C.Free;
  End;
  C := TBackusNaurComment.CreateComment('//: This is a comment.', 1, 2);
  Try
    Check(C <> Nil, '//: Comment is NULL');
    CheckEquals(8, C.TokenCount);
    CheckEquals(1, C.Line);
    CheckEquals(2, C.Column);
  Finally
    C.Free;
  End;
End;

//
// Test Methods for Class TBackusNaurModule.
//
procedure TestTBackusNaurModule.TestExpression;

Const
  strCode =
    '<rule1> ::= <option1> | <option2> | <option3>'#13#10 +
    '<rule2> ::= ( <option1> | <option2> | <option3> )'#13#10 +
    '<rule3> ::= ( <option1> | <option2> | <option3> )*'#13#10 +
    '<rule4> ::= ( <option1> | <option2> | <option3> )+'#13#10 +
    '<rule5> ::= <option1> | <option2> [ <option3> ]'#13#10 +
    '<rule6> ::= <option1> | <option2> [ <option3> ]*'#13#10 +
    '<rule7> ::= <option1> | <option2> [ <option3> ]+'#13#10 +
    '<option1> ::= ? option 1 ?'#13#10 +
    '<option2> ::= ? option 2 ?'#13#10 +
    '<option3> ::= ? option 3 ?'#13#10;

Var
  S : TBaseLanguageModule;

begin
  S := Dispatcher(strCode, 'D:\Path\Backus-Naur Grammar.bnf', True, [moParse]);
  Try
    CheckEquals(0, S.HeadingCount(strErrors), S.FirstError);
    CheckEquals(0, S.HeadingCount(strWarnings), S.FirstWarning);
    //CheckEquals(0, S.HeadingCount(strHints), S.FirstHint);
  Finally
    S.Free;
  End;
end;

procedure TestTBackusNaurModule.TestFailures01;

Const
  strCode =
    '<TextTable> ::= <DefinitionName> ''='' ''CLASS'' ''('' ''TEXTTABLE'' ' +
     ''')'' <LineEnd> ''{'' <LineEnd> [ <TextTableDef> ] ( <FieldDef> )+ ' +
     '''}'' <LineEnd>'#13#10;

Var
  S : TBaseLanguageModule;

begin
  S := Dispatcher(strCode, 'D:\Path\Backus-Naur Grammar.bnf', True, [moParse]);
  Try
    CheckEquals(0, S.HeadingCount(strErrors), S.FirstError);
    //CheckEquals(0, S.HeadingCount(strWarnings), S.FirstWarning);
    //CheckEquals(0, S.HeadingCount(strHints), S.FirstHint);
  Finally
    S.Free;
  End;
end;

procedure TestTBackusNaurModule.TestFailures02;

Const
  strCode =
    '<rule5> ::= <option1> | <option2> [ <option3> ]'#13#10 +
    '<option1> ::= ? option 1 ?'#13#10 +
    '<option2> ::= ? option 2 ?'#13#10 +
    '<option3> ::= ? option 3 ?'#13#10;

Var
  S : TBaseLanguageModule;

begin
  S := Dispatcher(strCode, 'D:\Path\Backus-Naur Grammar.bnf', True, [moParse]);
  Try
    CheckEquals(0, S.HeadingCount(strErrors), S.FirstError);
    CheckEquals(0, S.HeadingCount(strWarnings), S.FirstWarning);
    //CheckEquals(0, S.HeadingCount(strHints), S.FirstHint);
  Finally
    S.Free;
  End;
end;

procedure TestTBackusNaurModule.TestFailures03;

Const
  strCode =
    '<ArrayConstant> ::= ''('' ( <TypedConstant> [ '','' <TypedConstant> ] )* '')'''#13#10 +
    '<TypedConstant> ::= ?? '#13#10;

Var
  S : TBaseLanguageModule;

begin
  S := Dispatcher(strCode, 'D:\Path\Backus-Naur Grammar.bnf', True, [moParse]);
  Try
    CheckEquals(0, S.HeadingCount(strErrors), S.FirstError);
    CheckEquals(0, S.HeadingCount(strWarnings), S.FirstWarning);
    //CheckEquals(0, S.HeadingCount(strHints), S.FirstHint);
  Finally
    S.Free;
  End;
end;

procedure TestTBackusNaurModule.TestList;

Const
  strCode =
    '<rule> ::= <option1> | <option2> <option3>'#13#10 +
    '<option1> ::= ? option 1 ?'#13#10 +
    '<option2> ::= ? option 2 ?'#13#10 +
    '<option3> ::= ? option 3 ?'#13#10;

Var
  S : TBaseLanguageModule;

begin
  S := Dispatcher(strCode, 'D:\Path\Backus-Naur Grammar.bnf', True, [moParse]);
  Try
    CheckEquals(0, S.HeadingCount(strErrors), S.FirstError);
    CheckEquals(0, S.HeadingCount(strWarnings), S.FirstWarning);
    //CheckEquals(0, S.HeadingCount(strHints), S.FirstHint);
  Finally
    S.Free;
  End;
end;

Procedure TestTBackusNaurModule.Setup;
Begin
  FBackusNaurModule := TBackusNaurModule.CreateParser(
    '// A line comment.'#13#10 +
    '<rule> ::= ( <rule-name> | ''Text'' "Somemore" )*'#13#10 +
    '<rule-name> ::= ? All visible characters ? /* Hello */'#13#10,
    'D:\Path\Backus-Naur Grammar.bnf', True, [moParse])
End;

Procedure TestTBackusNaurModule.TearDown;

Begin
  FBackusNaurModule.Free;
End;

Procedure TestTBackusNaurModule.TestAsString;

Begin
  CheckEquals('Backus-Naur Grammar', FBackusNaurModule.AsString(True, True));
End;

Procedure TestTBackusNaurModule.TestCreateParser;

Begin
  CheckEquals(18, FBackusNaurModule.TokenCount);
  CheckEquals('D:\Path\Backus-Naur Grammar.bnf', FBackusNaurModule.FileName);
  CheckEquals(True, FBackusNaurModule.Modified);
End;


procedure TestTBackusNaurModule.TestTokenizeStream;
begin
  CheckEquals('// A line comment.', FBackusNaurModule.Tokens[0].Token);
  CheckEquals(ttLineComment, FBackusNaurModule.Tokens[0].TokenType);
  CheckEquals(#13#10, FBackusNaurModule.Tokens[1].Token);
  CheckEquals(ttLineEnd, FBackusNaurModule.Tokens[1].TokenType);

  CheckEquals('<rule>', FBackusNaurModule.Tokens[2].Token);
  CheckEquals(ttIdentifier, FBackusNaurModule.Tokens[2].TokenType);
  CheckEquals('::=', FBackusNaurModule.Tokens[3].Token);
  CheckEquals(ttSymbol, FBackusNaurModule.Tokens[3].TokenType);
  CheckEquals('(', FBackusNaurModule.Tokens[4].Token);
  CheckEquals(ttSymbol, FBackusNaurModule.Tokens[4].TokenType);
  CheckEquals('<rule-name>', FBackusNaurModule.Tokens[5].Token);
  CheckEquals(ttIdentifier, FBackusNaurModule.Tokens[5].TokenType);
  CheckEquals('|', FBackusNaurModule.Tokens[6].Token);
  CheckEquals(ttSymbol, FBackusNaurModule.Tokens[6].TokenType);
  CheckEquals('''Text''', FBackusNaurModule.Tokens[7].Token);
  CheckEquals(ttStringLiteral, FBackusNaurModule.Tokens[7].TokenType);
  CheckEquals('"Somemore"', FBackusNaurModule.Tokens[8].Token);
  CheckEquals(ttStringLiteral, FBackusNaurModule.Tokens[8].TokenType);
  CheckEquals(')', FBackusNaurModule.Tokens[9].Token);
  CheckEquals(ttSymbol, FBackusNaurModule.Tokens[9].TokenType);
  CheckEquals('*', FBackusNaurModule.Tokens[10].Token);
  CheckEquals(ttSymbol, FBackusNaurModule.Tokens[10].TokenType);
  CheckEquals(#13#10, FBackusNaurModule.Tokens[11].Token);
  CheckEquals(ttLineEnd, FBackusNaurModule.Tokens[11].TokenType);

  CheckEquals('<rule-name>', FBackusNaurModule.Tokens[12].Token);
  CheckEquals(ttIdentifier, FBackusNaurModule.Tokens[12].TokenType);
  CheckEquals('::=', FBackusNaurModule.Tokens[13].Token);
  CheckEquals(ttSymbol, FBackusNaurModule.Tokens[13].TokenType);
  CheckEquals('? All visible characters ?', FBackusNaurModule.Tokens[14].Token);
  CheckEquals(ttDirective, FBackusNaurModule.Tokens[14].TokenType);
  CheckEquals('/* Hello */', FBackusNaurModule.Tokens[15].Token);
  CheckEquals(ttBlockComment, FBackusNaurModule.Tokens[15].TokenType);
  CheckEquals(#13#10, FBackusNaurModule.Tokens[16].Token);
  CheckEquals(ttLineEnd, FBackusNaurModule.Tokens[16].TokenType);

  CheckEquals('', FBackusNaurModule.Tokens[17].Token);
  CheckEquals(ttFileEnd, FBackusNaurModule.Tokens[17].TokenType);
end;

procedure TestTBackusNaurModule.TestRule;

Const
  strCode =
    '<rule> ::= ''Text'''#13#10;

Var
  S : TBaseLanguageModule;

begin
  S := Dispatcher(strCode, 'D:\Path\Backus-Naur Grammar.bnf', True, [moParse]);
  Try
    CheckEquals(ttFileEnd, S.CurrentToken.TokenType);
    CheckEquals(0, S.HeadingCount(strErrors), S.FirstError);
    CheckEquals(0, S.HeadingCount(strWarnings), S.FirstWarning);
    //CheckEquals(0, S.HeadingCount(strHints), S.FirstHint);
  Finally
    S.Free;
  End;
end;

procedure TestTBackusNaurModule.TestSyntax;

Const
  strCode =
   '/* A comment */'#13#10 +
    '<rule1> ::= ''Text'''#13#10 +
    '// another comment'#13#10 +
    '<rule2> ::= "hello" | "goodbye"'#13#10 +
    '<another-rule> ::= ? another rule ?'#13#10;

Var
  S : TBaseLanguageModule;

begin
  S := Dispatcher(strCode, 'D:\Path\Backus-Naur Grammar.bnf', True, [moParse]);
  Try
    CheckEquals(ttFileEnd, S.CurrentToken.TokenType);
    CheckEquals(0, S.HeadingCount(strErrors), S.FirstError);
    CheckEquals(0, S.HeadingCount(strWarnings), S.FirstWarning);
    //CheckEquals(0, S.HeadingCount(strHints), S.FirstHint);
  Finally
    S.Free;
  End;
end;

Initialization
  RegisterTest('Backus-Naur', TestTBackusNaurComment.Suite);
  RegisterTest('Backus-Naur', TestTBackusNaurModule.Suite);
End.