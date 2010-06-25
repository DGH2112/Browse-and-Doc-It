Unit TestGenericTokenizer;

Interface

Uses
  TestFramework, BaseLanguageModule, TestBaseLanguageModule, GenericTokenizer;

Type
  //
  // Test Class for the Functions Class Methods.
  //
  TestFunctions = Class(TExtendedTestCase)
  Strict Private
  Public
  Published
    Procedure TestTokenize;
    Procedure TestPasBlockCommentOnLine;
    Procedure TestPasBlockCommentOverMultiLines;
    Procedure TestBraceComment;
    Procedure TestLineComment;
    Procedure TestCPPBlockCommentOnLine;
    Procedure TestCPPBlockCommentOverMultiLines;
  End;

Implementation

Uses
  Classes;

//
// Test Methods for Class Functions.
//
procedure TestFunctions.TestBraceComment;

Const
  strCode =
    'Begin'#13#10 +
    '  { Do something }'#13#10 +
    '  DoSomething(1 {, 2});'#13#10 +
    '  DoSomethingElse;'#13#10 +
    'End;'#13#10;

Var
  strReservedWords, strDirectives : TKeyWords;

Var
  sl : TStringList;

Begin
  SetLength(strReservedWords, 2);
  strReservedWords[0] := 'begin';
  strReservedWords[1] := 'end';
  strDirectives := Nil;
  sl := Tokenize(strCode, strReservedWords, strDirectives);
  Try
    CheckEquals(21, sl.Count);
    CheckEquals('Begin', sl[0]);
    CheckEquals(ttReservedWord, TBADITokenType(sl.Objects[0]));
    CheckEquals(#13#10, sl[1]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[1]));
    CheckEquals('  ', sl[2]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[2]));
    CheckEquals('{ Do something }', sl[3]);
    CheckEquals(ttBlockComment, TBADITokenType(sl.Objects[3]));
    CheckEquals(#13#10, sl[4]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[4]));
    CheckEquals('  ', sl[5]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[5]));
    CheckEquals('DoSomething', sl[6]);
    CheckEquals(ttIdentifier, TBADITokenType(sl.Objects[6]));
    CheckEquals('(', sl[7]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[7]));
    CheckEquals('1', sl[8]);
    CheckEquals(ttNumber, TBADITokenType(sl.Objects[8]));
    CheckEquals(' ', sl[9]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[9]));
    CheckEquals('{, 2}', sl[10]);
    CheckEquals(ttBlockComment, TBADITokenType(sl.Objects[10]));
    CheckEquals(')', sl[11]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[11]));
    CheckEquals(';', sl[12]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[12]));
    CheckEquals(#13#10, sl[13]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[13]));
    CheckEquals('  ', sl[14]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[14]));
    CheckEquals('DoSomethingElse', sl[15]);
    CheckEquals(ttIdentifier, TBADITokenType(sl.Objects[15]));
    CheckEquals(';', sl[16]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[16]));
    CheckEquals(#13#10, sl[17]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[17]));
    CheckEquals('End', sl[18]);
    CheckEquals(ttReservedWord, TBADITokenType(sl.Objects[18]));
    CheckEquals(';', sl[19]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[19]));
    CheckEquals(#13#10, sl[20]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[20]));
  Finally
    sl.Free;
  End;
end;

procedure TestFunctions.TestCPPBlockCommentOnLine;

Const
  strCode =
    'Begin'#13#10 +
    '  /* Do something */'#13#10 +
    '  DoSomething(1 {, 2});'#13#10 +
    '  DoSomethingElse;'#13#10 +
    'End;'#13#10;

Var
  strReservedWords, strDirectives : TKeyWords;

Var
  sl : TStringList;

Begin
  SetLength(strReservedWords, 2);
  strReservedWords[0] := 'begin';
  strReservedWords[1] := 'end';
  strDirectives := Nil;
  sl := Tokenize(strCode, strReservedWords, strDirectives);
  Try
    CheckEquals(21, sl.Count);
    CheckEquals('Begin', sl[0]);
    CheckEquals(ttReservedWord, TBADITokenType(sl.Objects[0]));
    CheckEquals(#13#10, sl[1]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[1]));
    CheckEquals('  ', sl[2]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[2]));
    CheckEquals('/* Do something */', sl[3]);
    CheckEquals(ttBlockComment, TBADITokenType(sl.Objects[3]));
    CheckEquals(#13#10, sl[4]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[4]));
    CheckEquals('  ', sl[5]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[5]));
    CheckEquals('DoSomething', sl[6]);
    CheckEquals(ttIdentifier, TBADITokenType(sl.Objects[6]));
    CheckEquals('(', sl[7]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[7]));
    CheckEquals('1', sl[8]);
    CheckEquals(ttNumber, TBADITokenType(sl.Objects[8]));
    CheckEquals(' ', sl[9]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[9]));
    CheckEquals('{, 2}', sl[10]);
    CheckEquals(ttBlockComment, TBADITokenType(sl.Objects[10]));
    CheckEquals(')', sl[11]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[11]));
    CheckEquals(';', sl[12]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[12]));
    CheckEquals(#13#10, sl[13]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[13]));
    CheckEquals('  ', sl[14]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[14]));
    CheckEquals('DoSomethingElse', sl[15]);
    CheckEquals(ttIdentifier, TBADITokenType(sl.Objects[15]));
    CheckEquals(';', sl[16]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[16]));
    CheckEquals(#13#10, sl[17]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[17]));
    CheckEquals('End', sl[18]);
    CheckEquals(ttReservedWord, TBADITokenType(sl.Objects[18]));
    CheckEquals(';', sl[19]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[19]));
    CheckEquals(#13#10, sl[20]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[20]));
  Finally
    sl.Free;
  End;
end;

procedure TestFunctions.TestCPPBlockCommentOverMultiLines;

Const
  strCode =
    'Begin'#13#10 +
    '  /*'#13#10 +
    '    Do something'#13#10 +
    '  */'#13#10 +
    '  DoSomething(1 {, 2});'#13#10 +
    '  DoSomethingElse;'#13#10 +
    'End;'#13#10;

Var
  strReservedWords, strDirectives : TKeyWords;

Var
  sl : TStringList;

Begin
  SetLength(strReservedWords, 2);
  strReservedWords[0] := 'begin';
  strReservedWords[1] := 'end';
  strDirectives := Nil;
  sl := Tokenize(strCode, strReservedWords, strDirectives);
  Try
    CheckEquals(21, sl.Count);
    CheckEquals('Begin', sl[0]);
    CheckEquals(ttReservedWord, TBADITokenType(sl.Objects[0]));
    CheckEquals(#13#10, sl[1]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[1]));
    CheckEquals('  ', sl[2]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[2]));
    CheckEquals('/*'#13#10'    Do something'#13#10'  */', sl[3]);
    CheckEquals(ttBlockComment, TBADITokenType(sl.Objects[3]));
    CheckEquals(#13#10, sl[4]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[4]));
    CheckEquals('  ', sl[5]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[5]));
    CheckEquals('DoSomething', sl[6]);
    CheckEquals(ttIdentifier, TBADITokenType(sl.Objects[6]));
    CheckEquals('(', sl[7]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[7]));
    CheckEquals('1', sl[8]);
    CheckEquals(ttNumber, TBADITokenType(sl.Objects[8]));
    CheckEquals(' ', sl[9]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[9]));
    CheckEquals('{, 2}', sl[10]);
    CheckEquals(ttBlockComment, TBADITokenType(sl.Objects[10]));
    CheckEquals(')', sl[11]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[11]));
    CheckEquals(';', sl[12]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[12]));
    CheckEquals(#13#10, sl[13]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[13]));
    CheckEquals('  ', sl[14]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[14]));
    CheckEquals('DoSomethingElse', sl[15]);
    CheckEquals(ttIdentifier, TBADITokenType(sl.Objects[15]));
    CheckEquals(';', sl[16]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[16]));
    CheckEquals(#13#10, sl[17]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[17]));
    CheckEquals('End', sl[18]);
    CheckEquals(ttReservedWord, TBADITokenType(sl.Objects[18]));
    CheckEquals(';', sl[19]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[19]));
    CheckEquals(#13#10, sl[20]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[20]));
  Finally
    sl.Free;
  End;
end;

procedure TestFunctions.TestLineComment;

Const
  strCode =
    'Begin'#13#10 +
    '  // Do something'#13#10 +
    '  DoSomething(1 {, 2});'#13#10 +
    '  DoSomethingElse;'#13#10 +
    'End;'#13#10;

Var
  strReservedWords, strDirectives : TKeyWords;

Var
  sl : TStringList;

Begin
  SetLength(strReservedWords, 2);
  strReservedWords[0] := 'begin';
  strReservedWords[1] := 'end';
  strDirectives := Nil;
  sl := Tokenize(strCode, strReservedWords, strDirectives);
  Try
    CheckEquals(21, sl.Count);
    CheckEquals('Begin', sl[0]);
    CheckEquals(ttReservedWord, TBADITokenType(sl.Objects[0]));
    CheckEquals(#13#10, sl[1]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[1]));
    CheckEquals('  ', sl[2]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[2]));
    CheckEquals('// Do something', sl[3]);
    CheckEquals(ttLineComment, TBADITokenType(sl.Objects[3]));
    CheckEquals(#13#10, sl[4]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[4]));
    CheckEquals('  ', sl[5]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[5]));
    CheckEquals('DoSomething', sl[6]);
    CheckEquals(ttIdentifier, TBADITokenType(sl.Objects[6]));
    CheckEquals('(', sl[7]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[7]));
    CheckEquals('1', sl[8]);
    CheckEquals(ttNumber, TBADITokenType(sl.Objects[8]));
    CheckEquals(' ', sl[9]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[9]));
    CheckEquals('{, 2}', sl[10]);
    CheckEquals(ttBlockComment, TBADITokenType(sl.Objects[10]));
    CheckEquals(')', sl[11]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[11]));
    CheckEquals(';', sl[12]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[12]));
    CheckEquals(#13#10, sl[13]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[13]));
    CheckEquals('  ', sl[14]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[14]));
    CheckEquals('DoSomethingElse', sl[15]);
    CheckEquals(ttIdentifier, TBADITokenType(sl.Objects[15]));
    CheckEquals(';', sl[16]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[16]));
    CheckEquals(#13#10, sl[17]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[17]));
    CheckEquals('End', sl[18]);
    CheckEquals(ttReservedWord, TBADITokenType(sl.Objects[18]));
    CheckEquals(';', sl[19]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[19]));
    CheckEquals(#13#10, sl[20]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[20]));
  Finally
    sl.Free;
  End;
end;

procedure TestFunctions.TestPasBlockCommentOnLine;

Const
  strCode =
    'Begin'#13#10 +
    '  (* Do something *)'#13#10 +
    '  DoSomething(1 {, 2});'#13#10 +
    '  DoSomethingElse;'#13#10 +
    'End;'#13#10;

Var
  strReservedWords, strDirectives : TKeyWords;

Var
  sl : TStringList;

Begin
  SetLength(strReservedWords, 2);
  strReservedWords[0] := 'begin';
  strReservedWords[1] := 'end';
  strDirectives := Nil;
  sl := Tokenize(strCode, strReservedWords, strDirectives);
  Try
    CheckEquals(21, sl.Count);
    CheckEquals('Begin', sl[0]);
    CheckEquals(ttReservedWord, TBADITokenType(sl.Objects[0]));
    CheckEquals(#13#10, sl[1]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[1]));
    CheckEquals('  ', sl[2]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[2]));
    CheckEquals('(* Do something *)', sl[3]);
    CheckEquals(ttBlockComment, TBADITokenType(sl.Objects[3]));
    CheckEquals(#13#10, sl[4]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[4]));
    CheckEquals('  ', sl[5]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[5]));
    CheckEquals('DoSomething', sl[6]);
    CheckEquals(ttIdentifier, TBADITokenType(sl.Objects[6]));
    CheckEquals('(', sl[7]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[7]));
    CheckEquals('1', sl[8]);
    CheckEquals(ttNumber, TBADITokenType(sl.Objects[8]));
    CheckEquals(' ', sl[9]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[9]));
    CheckEquals('{, 2}', sl[10]);
    CheckEquals(ttBlockComment, TBADITokenType(sl.Objects[10]));
    CheckEquals(')', sl[11]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[11]));
    CheckEquals(';', sl[12]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[12]));
    CheckEquals(#13#10, sl[13]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[13]));
    CheckEquals('  ', sl[14]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[14]));
    CheckEquals('DoSomethingElse', sl[15]);
    CheckEquals(ttIdentifier, TBADITokenType(sl.Objects[15]));
    CheckEquals(';', sl[16]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[16]));
    CheckEquals(#13#10, sl[17]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[17]));
    CheckEquals('End', sl[18]);
    CheckEquals(ttReservedWord, TBADITokenType(sl.Objects[18]));
    CheckEquals(';', sl[19]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[19]));
    CheckEquals(#13#10, sl[20]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[20]));
  Finally
    sl.Free;
  End;
end;

procedure TestFunctions.TestPasBlockCommentOverMultiLines;

Const
  strCode =
    'Begin'#13#10 +
    '  (*'#13#10 +
    '    Do something'#13#10 +
    '  *)'#13#10 +
    '  DoSomething(1 {, 2});'#13#10 +
    '  DoSomethingElse;'#13#10 +
    'End;'#13#10;

Var
  strReservedWords, strDirectives : TKeyWords;

Var
  sl : TStringList;

Begin
  SetLength(strReservedWords, 2);
  strReservedWords[0] := 'begin';
  strReservedWords[1] := 'end';
  strDirectives := Nil;
  sl := Tokenize(strCode, strReservedWords, strDirectives);
  Try
    CheckEquals(21, sl.Count);
    CheckEquals('Begin', sl[0]);
    CheckEquals(ttReservedWord, TBADITokenType(sl.Objects[0]));
    CheckEquals(#13#10, sl[1]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[1]));
    CheckEquals('  ', sl[2]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[2]));
    CheckEquals('(*'#13#10'    Do something'#13#10'  *)', sl[3]);
    CheckEquals(ttBlockComment, TBADITokenType(sl.Objects[3]));
    CheckEquals(#13#10, sl[4]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[4]));
    CheckEquals('  ', sl[5]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[5]));
    CheckEquals('DoSomething', sl[6]);
    CheckEquals(ttIdentifier, TBADITokenType(sl.Objects[6]));
    CheckEquals('(', sl[7]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[7]));
    CheckEquals('1', sl[8]);
    CheckEquals(ttNumber, TBADITokenType(sl.Objects[8]));
    CheckEquals(' ', sl[9]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[9]));
    CheckEquals('{, 2}', sl[10]);
    CheckEquals(ttBlockComment, TBADITokenType(sl.Objects[10]));
    CheckEquals(')', sl[11]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[11]));
    CheckEquals(';', sl[12]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[12]));
    CheckEquals(#13#10, sl[13]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[13]));
    CheckEquals('  ', sl[14]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[14]));
    CheckEquals('DoSomethingElse', sl[15]);
    CheckEquals(ttIdentifier, TBADITokenType(sl.Objects[15]));
    CheckEquals(';', sl[16]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[16]));
    CheckEquals(#13#10, sl[17]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[17]));
    CheckEquals('End', sl[18]);
    CheckEquals(ttReservedWord, TBADITokenType(sl.Objects[18]));
    CheckEquals(';', sl[19]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[19]));
    CheckEquals(#13#10, sl[20]);
    CheckEquals(ttLineEnd, TBADITokenType(sl.Objects[20]));
  Finally
    sl.Free;
  End;
end;

Procedure TestFunctions.TestTokenize;

Const
  strCode = 'qwerty = one + two + ''q'' - "p" - Hello * ? Custom ? + <p> '''' "" </p>';

Var
  strReservedWords, strDirectives : TKeyWords;

Var
  sl : TStringList;

Begin
  SetLength(strReservedWords, 2);
  strReservedWords[0] := 'one';
  strReservedWords[1] := 'two';
  strDirectives := Nil;
  sl := Tokenize(strCode, strReservedWords, strDirectives);
  Try
    CheckEquals(35, sl.Count);
    CheckEquals('qwerty', sl[0]);
    CheckEquals(ttIdentifier, TBADITokenType(sl.Objects[0]));
    CheckEquals(' ', sl[1]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[1]));
    CheckEquals('=', sl[2]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[2]));
    CheckEquals(' ', sl[3]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[3]));
    CheckEquals('one', sl[4]);
    CheckEquals(ttReservedWord, TBADITokenType(sl.Objects[4]));
    CheckEquals(' ', sl[5]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[5]));
    CheckEquals('+', sl[6]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[6]));
    CheckEquals(' ', sl[7]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[7]));
    CheckEquals('two', sl[8]);
    CheckEquals(ttReservedWord, TBADITokenType(sl.Objects[8]));
    CheckEquals(' ', sl[9]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[9]));
    CheckEquals('+', sl[10]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[10]));
    CheckEquals(' ', sl[11]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[11]));
    CheckEquals('''q''', sl[12]);
    CheckEquals(ttSingleLiteral, TBADITokenType(sl.Objects[12]));
    CheckEquals(' ', sl[13]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[13]));
    CheckEquals('-', sl[14]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[14]));
    CheckEquals(' ', sl[15]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[15]));
    CheckEquals('"p"', sl[16]);
    CheckEquals(ttDoubleLiteral, TBADITokenType(sl.Objects[16]));
    CheckEquals(' ', sl[17]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[17]));
    CheckEquals('-', sl[18]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[18]));
    CheckEquals(' ', sl[19]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[19]));
    CheckEquals('Hello', sl[20]);
    CheckEquals(ttIdentifier, TBADITokenType(sl.Objects[20]));
    CheckEquals(' ', sl[21]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[21]));
    CheckEquals('*', sl[22]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[22]));
    CheckEquals(' ', sl[23]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[23]));
    CheckEquals('? Custom ?', sl[24]);
    CheckEquals(ttCustomUserToken, TBADITokenType(sl.Objects[24]));
    CheckEquals(' ', sl[25]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[25]));
    CheckEquals('+', sl[26]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[26]));
    CheckEquals(' ', sl[27]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[27]));
    CheckEquals('<p>', sl[28]);
    CheckEquals(ttHTMLStartTag, TBADITokenType(sl.Objects[28]));
    CheckEquals(' ', sl[29]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[29]));

    CheckEquals('''''', sl[30]);
    CheckEquals(ttSingleLiteral, TBADITokenType(sl.Objects[30]));
    CheckEquals(' ', sl[31]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[31]));
    CheckEquals('""', sl[32]);
    CheckEquals(ttDoubleLiteral, TBADITokenType(sl.Objects[32]));
    CheckEquals(' ', sl[33]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[33]));


    CheckEquals('</p>', sl[34]);
    CheckEquals(ttHTMLEndTag, TBADITokenType(sl.Objects[34]));
  Finally
    sl.Free;
  End;
End;


Initialization
  RegisterTest('Generic Tokeniser', TestFunctions.Suite);
End.