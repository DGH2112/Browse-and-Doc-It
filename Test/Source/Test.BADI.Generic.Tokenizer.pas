(**
  
  This module contains DUnit test for the Browse and Doc It code.

  @Author  David Hoyle
  @Version 1.0
  @Date    21 Jun 2019

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2019  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
  
**)
Unit Test.BADI.Generic.Tokenizer;

Interface

Uses
  Classes,
  TestFramework,
  BADI.Base.Module,
  Test.BADI.Base.Module,
  BADI.Generic.Tokenizer,
  BADI.Types;

Type
  //
  // Test Class for the Functions Class Methods.
  //
  TestFunctions = Class(TExtendedTestCase)
  Strict Private
    Procedure CheckToken(sl : TStringList; iToken : Integer; strToken : String;
      TokenType : TBADITokenType);
  Public
  Published
    Procedure TestTokenize;
    Procedure TestPasBlockCommentOnLine;
    Procedure TestPasBlockCommentOverMultiLines;
    Procedure TestBraceComment;
    Procedure TestLineComment;
    Procedure TestCPPBlockCommentOnLine;
    Procedure TestCPPBlockCommentOverMultiLines;
    Procedure TestQuoteNotInXML;
    Procedure TestQuoteInXML;
  End;

Implementation

Uses
  SysUtils;

//
// Test Methods for Class Functions.
//
Procedure TestFunctions.CheckToken(sl: TStringList; iToken: Integer; strToken : String;
  TokenType : TBADITokenType);

Begin
  CheckEquals(strToken, sl[iToken]);
  CheckEquals(TokenType, TBADITokenType(sl.Objects[iToken]),
    Format('%d)%s|%s', [iToken, sl[iToken], strToken]));
End;

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

Procedure TestFunctions.TestQuoteInXML;

Const
  strText = '<p class="Name">This don''t fail!</p>';

Var
  strReservedWords, strDirectives : TKeyWords;
  sl : TStringList;

Begin
  sl := Tokenize(strText, strReservedWords, strDirectives);
  Try
    CheckToken(sl,  0, '<', ttSymbol);
    CheckToken(sl,  1, 'p', ttHTMLStartTag);
    CheckToken(sl,  3, 'class', ttIdentifier);
    CheckToken(sl,  4, '=', ttSymbol);
    CheckToken(sl,  5, '"Name"', ttDoubleLiteral);
    CheckToken(sl,  6, '>', ttSymbol);
    CheckToken(sl,  7, 'This', ttIdentifier);
    CheckToken(sl,  9, 'don', ttIdentifier);
    CheckToken(sl, 10, '''', ttSymbol);
    CheckToken(sl, 11, 't', ttIdentifier);
    CheckToken(sl, 13, 'fail', ttIdentifier);
    CheckToken(sl, 14, '!', ttSymbol);
    CheckToken(sl, 15, '<', ttSymbol);
    CheckToken(sl, 16, '/', ttSymbol);
    CheckToken(sl, 17, 'p', ttHTMLEndTag);
    CheckToken(sl, 18, '>', ttSymbol);
  Finally
    sl.Free;
  End;
End;

Procedure TestFunctions.TestQuoteNotInXML;

Const
  strText = 'This is some ''quoted'' text and some "more" text!';

Var
  strReservedWords, strDirectives : TKeyWords;
  sl : TStringList;

Begin
  sl := Tokenize(strText, strReservedWords, strDirectives);
  Try
    CheckToken(sl, 0, 'This', ttIdentifier);
    CheckToken(sl, 2, 'is', ttIdentifier);
    CheckToken(sl, 4, 'some', ttIdentifier);
    CheckToken(sl, 6, '''quoted''', ttSingleLiteral);
    CheckToken(sl, 8, 'text', ttIdentifier);
    CheckToken(sl, 10, 'and', ttIdentifier);
    CheckToken(sl, 12, 'some', ttIdentifier);
    CheckToken(sl, 14, '"more"', ttDoubleLiteral);
    CheckToken(sl, 16, 'text', ttIdentifier);
    CheckToken(sl, 17, '!', ttSymbol);
  Finally
    sl.Free;
  End;
End;

Procedure TestFunctions.TestTokenize;

Const
  //                            1               2                      3            4
  // Tokens  0     1234  5678  9012    3456  7890    1234         56789012 3 456 7890
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
    CheckEquals(42, sl.Count);
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
    CheckEquals('<', sl[28]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[28]));
    CheckEquals('p', sl[29]);
    CheckEquals(ttHTMLStartTag, TBADITokenType(sl.Objects[29]));
    CheckEquals('>', sl[30]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[30]));
    CheckEquals(' ', sl[31]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[31]));
    CheckEquals('''', sl[32]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[32]));
    CheckEquals('''', sl[33]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[33]));
    CheckEquals(' ', sl[34]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[34]));
    CheckEquals('"', sl[35]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[35]));
    CheckEquals('"', sl[36]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[36]));
    CheckEquals(' ', sl[37]);
    CheckEquals(ttWhiteSpace, TBADITokenType(sl.Objects[37]));
    CheckEquals('<', sl[38]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[38]));
    CheckEquals('/', sl[39]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[39]));
    CheckEquals('p', sl[40]);
    CheckEquals(ttHTMLEndTag, TBADITokenType(sl.Objects[40]));
    CheckEquals('>', sl[41]);
    CheckEquals(ttSymbol, TBADITokenType(sl.Objects[41]));
  Finally
    sl.Free;
  End;
End;


Initialization
  RegisterTest('Generic Tokeniser', TestFunctions.Suite);
End.
