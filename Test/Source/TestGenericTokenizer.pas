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
  End;

Implementation

Uses
  Classes;

//
// Test Methods for Class Functions.
//
Procedure TestFunctions.TestTokenize;

Const
  strCode = 'qwerty = one + two + ''q'' - "p" - Hello * ? Custom ? + <p> </p>';

Var
  strKeyWords : TKeyWords;

Var
  sl : TStringList;

Begin
  SetLength(strKeyWords, 2);
  strKeyWords[0] := 'one';
  strKeyWords[1] := 'two';
  sl := Tokenize(strCode, strKeyWords);
  Try
    CheckEquals(31, sl.Count);
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
    CheckEquals('</p>', sl[30]);
    CheckEquals(ttHTMLEndTag, TBADITokenType(sl.Objects[30]));
  Finally
    sl.Free;
  End;
End;


Initialization
  RegisterTest('Generic Tokeniser', TestFunctions.Suite);
End.