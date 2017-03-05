Unit Test.BADI.BackusNaur.Module;

Interface

Uses
  TestFramework,
  BADI.Base.Module,
  Test.BADI.Base.Module,
  BADI.BackusNaur.Module;

Type
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
    Procedure TestGoal;
    Procedure TestSyntax;
    Procedure TestRule;
    Procedure TestExpression;
    Procedure TestList;
    Procedure TestTerm;
    Procedure TestSimpleExpression;
    Procedure TestHexChar;
    Procedure TestDecChar;
    Procedure TestLiteral;
    Procedure TestFailure01;
    Procedure TestFailure02;
    Procedure TestFailure03;
    Procedure TestFailure04;
  End;

Implementation

uses
  BADI.Comment,
  BADI.Types,
  BADI.ResourceStrings;

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
  S := TBackusNaurModule.CreateParser(strCode, 'D:\Path\Backus-Naur Grammar.bnf', True, [moParse]);
  Try
    CheckEquals(0, S.HeadingCount(strErrors), S.FirstError);
    CheckEquals(0, S.HeadingCount(strWarnings), S.FirstWarning);
    //CheckEquals(0, S.HeadingCount(strHints), S.FirstHint);
  Finally
    S.Free;
  End;
end;

procedure TestTBackusNaurModule.TestFailure01;

Const
  strCode =
    '<TextTable> ::= <DefinitionName> ''='' ''CLASS'' ''('' ''TEXTTABLE'' ' +
     ''')'' <LineEnd> ''{'' <LineEnd> [ <TextTableDef> ] ( <FieldDef> )+ ' +
     '''}'' <LineEnd>'#13#10;

Var
  S : TBaseLanguageModule;

begin
  S := TBackusNaurModule.CreateParser(strCode, 'D:\Path\Backus-Naur Grammar.bnf', True, [moParse]);
  Try
    CheckEquals(0, S.HeadingCount(strErrors), S.FirstError);
    //CheckEquals(0, S.HeadingCount(strWarnings), S.FirstWarning);
    //CheckEquals(0, S.HeadingCount(strHints), S.FirstHint);
  Finally
    S.Free;
  End;
end;

procedure TestTBackusNaurModule.TestFailure02;

Const
  strCode =
    '<rule5> ::= <option1> | <option2> [ <option3> ]'#13#10 +
    '<option1> ::= ? option 1 ?'#13#10 +
    '<option2> ::= ? option 2 ?'#13#10 +
    '<option3> ::= ? option 3 ?'#13#10;

Var
  S : TBaseLanguageModule;

begin
  S := TBackusNaurModule.CreateParser(strCode, 'D:\Path\Backus-Naur Grammar.bnf', True, [moParse]);
  Try
    CheckEquals(0, S.HeadingCount(strErrors), S.FirstError);
    CheckEquals(0, S.HeadingCount(strWarnings), S.FirstWarning);
    //CheckEquals(0, S.HeadingCount(strHints), S.FirstHint);
  Finally
    S.Free;
  End;
end;

procedure TestTBackusNaurModule.TestFailure03;

Const
  strCode =
    '<ArrayConstant> ::= ''('' ( <TypedConstant> [ '','' <TypedConstant> ] )* '')'''#13#10 +
    '<TypedConstant> ::= ?? '#13#10;

Var
  S : TBaseLanguageModule;

begin
  S := TBackusNaurModule.CreateParser(strCode, 'D:\Path\Backus-Naur Grammar.bnf', True, [moParse]);
  Try
    CheckEquals(0, S.HeadingCount(strErrors), S.FirstError);
    CheckEquals(0, S.HeadingCount(strWarnings), S.FirstWarning);
    //CheckEquals(0, S.HeadingCount(strHints), S.FirstHint);
  Finally
    S.Free;
  End;
end;

procedure TestTBackusNaurModule.TestFailure04;

Const
  strCode =
    '<goal> ::= <rule1>'#13#10 +
    ''#13#10 +
    '/*/---------'#13#10 + // Error line
    ''#13#10 +
    '<rule1> ::= ? Something ?'#13#10 +
    ''#13#10 +
    ''#13#10;

Var
  S : TBaseLanguageModule;

begin
  S := TBackusNaurModule.CreateParser(strCode, 'D:\Path\Backus-Naur Grammar.bnf', True, [moParse]);
  Try
    CheckEquals(1, S.HeadingCount(strErrors), S.FirstError);
    CheckEquals(0, S.HeadingCount(strWarnings), S.FirstWarning);
    CheckEquals(0, S.HeadingCount(strHints), S.FirstHint);
  Finally
    S.Free;
  End;
end;

procedure TestTBackusNaurModule.TestGoal;

Const
  strCode = ''#13#10;

Var
  S : TBaseLanguageModule;

begin
  S := TBackusNaurModule.CreateParser(strCode, 'D:\Path\Backus-Naur Grammar.bnf', True, [moParse]);
  Try
    CheckEquals(0, S.HeadingCount(strErrors), S.FirstError);
    CheckEquals(0, S.HeadingCount(strWarnings), S.FirstWarning);
    //CheckEquals(0, S.HeadingCount(strHints), S.FirstHint);
  Finally
    S.Free;
  End;
end;

procedure TestTBackusNaurModule.TestHexChar;

Const
  strCode =
    '<rule> ::= $20'#13#10 +
    '<myrule> ::= $8F'#13#10;

Var
  S : TBaseLanguageModule;

begin
  S := TBackusNaurModule.CreateParser(strCode, 'D:\Path\Backus-Naur Grammar.bnf', True, [moParse]);
  Try
    CheckEquals(0, S.HeadingCount(strErrors), S.FirstError);
    CheckEquals(0, S.HeadingCount(strWarnings), S.FirstWarning);
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
  S := TBackusNaurModule.CreateParser(strCode, 'D:\Path\Backus-Naur Grammar.bnf', True, [moParse]);
  Try
    CheckEquals(0, S.HeadingCount(strErrors), S.FirstError);
    CheckEquals(0, S.HeadingCount(strWarnings), S.FirstWarning);
  Finally
    S.Free;
  End;
end;

procedure TestTBackusNaurModule.TestLiteral;

Const
  strCode =
    '<rule> ::= #32..#128'#13#10;

Var
  S : TBaseLanguageModule;

begin
  S := TBackusNaurModule.CreateParser(strCode, 'D:\Path\Backus-Naur Grammar.bnf', True, [moParse]);
  Try
    CheckEquals(0, S.HeadingCount(strErrors), S.FirstError);
    CheckEquals(0, S.HeadingCount(strWarnings), S.FirstWarning);
  Finally
    S.Free;
  End;
end;

Procedure TestTBackusNaurModule.Setup;
Begin
  FBackusNaurModule := TBackusNaurModule.CreateParser(
    '// A line comment.'#13#10 +
    '<rule> ::= ( ''"'' <rule-name> ''"'' | ''Text'' "Somemore" )*'#13#10 +
    '<rule-name> ::= ? All visible characters ? /* Hello */'#13#10#13#10 +
    '<myrule> ::= $20 | #32'#13#10,
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
  CheckEquals(27, FBackusNaurModule.TokenCount);
  CheckEquals('D:\Path\Backus-Naur Grammar.bnf', FBackusNaurModule.FileName);
  CheckEquals(True, FBackusNaurModule.Modified);
End;


procedure TestTBackusNaurModule.TestDecChar;

Const
  strCode =
    '<rule> ::= #13'#13#10 +
    '<myrule> ::= #10'#13#10;

Var
  S : TBaseLanguageModule;

begin
  S := TBackusNaurModule.CreateParser(strCode, 'D:\Path\Backus-Naur Grammar.bnf', True, [moParse]);
  Try
    CheckEquals(0, S.HeadingCount(strErrors), S.FirstError);
    CheckEquals(0, S.HeadingCount(strWarnings), S.FirstWarning);
  Finally
    S.Free;
  End;
end;

procedure TestTBackusNaurModule.TestTerm;

Const
  strCode =
    '<rule> ::= <myrule1> | <myrule2>+ | <myrule3>*'#13#10;

Var
  S : TBaseLanguageModule;

begin
  S := TBackusNaurModule.CreateParser(strCode, 'D:\Path\Backus-Naur Grammar.bnf', True, [moParse]);
  Try
    CheckEquals(0, S.HeadingCount(strErrors), S.FirstError);
  Finally
    S.Free;
  End;
end;

procedure TestTBackusNaurModule.TestTokenizeStream;
begin
  CheckEquals('// A line comment.', FBackusNaurModule.Tokens[0].Token);
  CheckEquals(ttLineComment, FBackusNaurModule.Tokens[0].TokenType);
  CheckEquals('<line-end>', FBackusNaurModule.Tokens[1].Token);
  CheckEquals(ttLineEnd, FBackusNaurModule.Tokens[1].TokenType);

  CheckEquals('<rule>', FBackusNaurModule.Tokens[2].Token);
  CheckEquals(ttIdentifier, FBackusNaurModule.Tokens[2].TokenType);
  CheckEquals('::=', FBackusNaurModule.Tokens[3].Token);
  CheckEquals(ttSymbol, FBackusNaurModule.Tokens[3].TokenType);
  CheckEquals('(', FBackusNaurModule.Tokens[4].Token);
  CheckEquals(ttSymbol, FBackusNaurModule.Tokens[4].TokenType);

  CheckEquals('''"''', FBackusNaurModule.Tokens[5].Token);
  CheckEquals(ttSingleLiteral, FBackusNaurModule.Tokens[5].TokenType);
  CheckEquals('<rule-name>', FBackusNaurModule.Tokens[6].Token);
  CheckEquals(ttIdentifier, FBackusNaurModule.Tokens[6].TokenType);
  CheckEquals('''"''', FBackusNaurModule.Tokens[7].Token);
  CheckEquals(ttSingleLiteral, FBackusNaurModule.Tokens[7].TokenType);

  CheckEquals('|', FBackusNaurModule.Tokens[8].Token);
  CheckEquals(ttSymbol, FBackusNaurModule.Tokens[8].TokenType);
  CheckEquals('''Text''', FBackusNaurModule.Tokens[9].Token);
  CheckEquals(ttSingleLiteral, FBackusNaurModule.Tokens[9].TokenType);
  CheckEquals('"Somemore"', FBackusNaurModule.Tokens[10].Token);
  CheckEquals(ttDoubleLiteral, FBackusNaurModule.Tokens[10].TokenType);
  CheckEquals(')', FBackusNaurModule.Tokens[11].Token);
  CheckEquals(ttSymbol, FBackusNaurModule.Tokens[11].TokenType);
  CheckEquals('*', FBackusNaurModule.Tokens[12].Token);
  CheckEquals(ttSymbol, FBackusNaurModule.Tokens[12].TokenType);
  CheckEquals('<line-end>', FBackusNaurModule.Tokens[13].Token);
  CheckEquals(ttLineEnd, FBackusNaurModule.Tokens[13].TokenType);

  CheckEquals('<rule-name>', FBackusNaurModule.Tokens[14].Token);
  CheckEquals(ttIdentifier, FBackusNaurModule.Tokens[14].TokenType);
  CheckEquals('::=', FBackusNaurModule.Tokens[15].Token);
  CheckEquals(ttSymbol, FBackusNaurModule.Tokens[15].TokenType);
  CheckEquals('? All visible characters ?', FBackusNaurModule.Tokens[16].Token);
  CheckEquals(ttCustomUserToken, FBackusNaurModule.Tokens[16].TokenType);
  CheckEquals('/* Hello */', FBackusNaurModule.Tokens[17].Token);
  CheckEquals(ttBlockComment, FBackusNaurModule.Tokens[17].TokenType);
  CheckEquals('<line-end>', FBackusNaurModule.Tokens[18].Token);
  CheckEquals(ttLineEnd, FBackusNaurModule.Tokens[18].TokenType);
  CheckEquals('<line-end>', FBackusNaurModule.Tokens[19].Token);
  CheckEquals(ttLineEnd, FBackusNaurModule.Tokens[19].TokenType);

  CheckEquals('<myrule>', FBackusNaurModule.Tokens[20].Token);
  CheckEquals(ttIdentifier, FBackusNaurModule.Tokens[20].TokenType);
  CheckEquals('::=', FBackusNaurModule.Tokens[21].Token);
  CheckEquals(ttSymbol, FBackusNaurModule.Tokens[21].TokenType);
  CheckEquals('$20', FBackusNaurModule.Tokens[22].Token);
  CheckEquals(ttNumber, FBackusNaurModule.Tokens[22].TokenType);
  CheckEquals('|', FBackusNaurModule.Tokens[23].Token);
  CheckEquals(ttSymbol, FBackusNaurModule.Tokens[23].TokenType);
  CheckEquals('#32', FBackusNaurModule.Tokens[24].Token);
  CheckEquals(ttNumber, FBackusNaurModule.Tokens[24].TokenType);
  CheckEquals('<line-end>', FBackusNaurModule.Tokens[25].Token);
  CheckEquals(ttLineEnd, FBackusNaurModule.Tokens[25].TokenType);

  CheckEquals('<end-of-file>', FBackusNaurModule.Tokens[26].Token);
  CheckEquals(ttFileEnd, FBackusNaurModule.Tokens[26].TokenType);
end;

procedure TestTBackusNaurModule.TestRule;

Const
  strCode =
    '<rule> ::= ''Text'''#13#10;

Var
  S : TBaseLanguageModule;

begin
  S := TBackusNaurModule.CreateParser(strCode, 'D:\Path\Backus-Naur Grammar.bnf', True, [moParse]);
  Try
    CheckEquals(ttFileEnd, S.CurrentToken.TokenType);
    CheckEquals(0, S.HeadingCount(strErrors), S.FirstError);
    CheckEquals(0, S.HeadingCount(strWarnings), S.FirstWarning);
    //CheckEquals(0, S.HeadingCount(strHints), S.FirstHint);
  Finally
    S.Free;
  End;
end;

procedure TestTBackusNaurModule.TestSimpleExpression;

Const
  strCode =
    '<rule> ::= <rule1> - <rule2>'#13#10;

Var
  S : TBaseLanguageModule;

begin
  S := TBackusNaurModule.CreateParser(strCode, 'D:\Path\Backus-Naur Grammar.bnf', True, [moParse]);
  Try
    CheckEquals(ttFileEnd, S.CurrentToken.TokenType);
    CheckEquals(0, S.HeadingCount(strErrors), S.FirstError);
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
  S := TBackusNaurModule.CreateParser(strCode, 'D:\Path\Backus-Naur Grammar.bnf', True, [moParse]);
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
  RegisterTest('Backus-Naur', TestTBackusNaurModule.Suite);
End.