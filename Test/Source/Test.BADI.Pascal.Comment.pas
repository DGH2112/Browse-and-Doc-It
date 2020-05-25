(**
  
  This module contains DUnit test for the Browse and Doc It code.

  @Author  David Hoyle
  @Version 6.210
  @Date    25 May 2020

  @nodocumentation @nochecks @nometrics

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
    Procedure TestCreateStdPascalComment;
    Procedure TestCreateStdMultiLinePascalComment;
    Procedure TestCreateStdPascalTaggedComment;
    Procedure TestCreateStdPascalMultiLineTaggedComment;
    Procedure TestCreateBracePascalComment;
    Procedure TestCreateBracePascalMutliLineComment;
    Procedure TestCreateBracePascalTaggedComment;
    Procedure TestCreateBracePascalMutliLineTaggedComment;
    Procedure TestCreateCPPLineComment;
    Procedure TestCreateCPPLineTaggedComment;
  End;

Implementation

Uses
  BADI.Comment,
  BADI.Pascal.Comment;

Procedure TestTPascalComment.TestCreateBracePascalComment;

Const
  //                  1         2         3
  //        0123456789012345678901234567890123
  strCmt = '{: Sorry Dave, I can`t do that! }';

Var
  ReturnValue: TComment;

Begin
  ReturnValue := TPascalComment.CreateComment(strCmt, 12, 23);
  Try
    CheckEquals(15, ReturnValue.TokenCount);
    CheckEquals('Sorry', ReturnValue.Tokens[0].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[0].Line);
    CheckEquals(23 + 03, ReturnValue.Tokens[0].Column);
    CheckEquals('Dave', ReturnValue.Tokens[2].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[2].Line);
    CheckEquals(23 + 09, ReturnValue.Tokens[2].Column);
    CheckEquals(',', ReturnValue.Tokens[3].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[3].Line);
    CheckEquals(23 + 13, ReturnValue.Tokens[3].Column);
    CheckEquals('I', ReturnValue.Tokens[5].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[5].Line);
    CheckEquals(23 + 15, ReturnValue.Tokens[5].Column);
    CheckEquals('can', ReturnValue.Tokens[7].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[7].Line);
    CheckEquals(23 + 17, ReturnValue.Tokens[7].Column);
    CheckEquals('`', ReturnValue.Tokens[8].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[8].Line);
    CheckEquals(23 + 20, ReturnValue.Tokens[8].Column);
    CheckEquals('t', ReturnValue.Tokens[9].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[9].Line);
    CheckEquals(23 + 21, ReturnValue.Tokens[9].Column);
    CheckEquals('do', ReturnValue.Tokens[11].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[11].Line);
    CheckEquals(23 + 23, ReturnValue.Tokens[11].Column);
    CheckEquals('that', ReturnValue.Tokens[13].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[13].Line);
    CheckEquals(23 + 26, ReturnValue.Tokens[13].Column);
    CheckEquals('!', ReturnValue.Tokens[14].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[14].Line);
    CheckEquals(23 + 30, ReturnValue.Tokens[14].Column);
    CheckEquals('Sorry Dave, I can`t do that!', ReturnValue.AsString(999, True));
  Finally
    ReturnValue.Free;
  End;
End;

Procedure TestTPascalComment.TestCreateBracePascalMutliLineComment;

Const
  //                  1           0        1
  //        012345678901234       1234567890123456
  strCmt = '{: Sorry Dave,'#13#10'I can`t do that! }';

Var
  ReturnValue: TComment;

Begin
  ReturnValue := TPascalComment.CreateComment(strCmt, 12, 23);
  Try
    CheckEquals(15, ReturnValue.TokenCount);
    CheckEquals('Sorry', ReturnValue.Tokens[0].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[0].Line);
    CheckEquals(23 + 03, ReturnValue.Tokens[0].Column);
    CheckEquals('Dave', ReturnValue.Tokens[2].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[2].Line);
    CheckEquals(23 + 09, ReturnValue.Tokens[2].Column);
    CheckEquals(',', ReturnValue.Tokens[3].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[3].Line);
    CheckEquals(23 + 13, ReturnValue.Tokens[3].Column);
    CheckEquals('I', ReturnValue.Tokens[5].Token);
    CheckEquals(12 + 01, ReturnValue.Tokens[5].Line);
    CheckEquals(     01, ReturnValue.Tokens[5].Column);
    CheckEquals('can', ReturnValue.Tokens[7].Token);
    CheckEquals(12 + 01, ReturnValue.Tokens[7].Line);
    CheckEquals(     03, ReturnValue.Tokens[7].Column);
    CheckEquals('`', ReturnValue.Tokens[8].Token);
    CheckEquals(12 + 01, ReturnValue.Tokens[8].Line);
    CheckEquals(     06, ReturnValue.Tokens[8].Column);
    CheckEquals('t', ReturnValue.Tokens[9].Token);
    CheckEquals(12 + 01, ReturnValue.Tokens[9].Line);
    CheckEquals(     07, ReturnValue.Tokens[9].Column);
    CheckEquals('do', ReturnValue.Tokens[11].Token);
    CheckEquals(12 + 01, ReturnValue.Tokens[11].Line);
    CheckEquals(     09, ReturnValue.Tokens[11].Column);
    CheckEquals('that', ReturnValue.Tokens[13].Token);
    CheckEquals(12 + 01, ReturnValue.Tokens[13].Line);
    CheckEquals(     12, ReturnValue.Tokens[13].Column);
    CheckEquals('!', ReturnValue.Tokens[14].Token);
    CheckEquals(12 + 01, ReturnValue.Tokens[14].Line);
    CheckEquals(     16, ReturnValue.Tokens[14].Column);
    CheckEquals('Sorry Dave, I can`t do that!', ReturnValue.AsString(999, True));
  Finally
    ReturnValue.Free;
  End;
End;

Procedure TestTPascalComment.TestCreateBracePascalMutliLineTaggedComment;

Const
  //                  
  //        01        12345678901        12345678901234567890123       123456789012345678901234567890        123
  strCmt = '{:'#13#10'Hello Dave!'#13#10'@tag This is tag text.'#13#10'  @tag2 This is more Tag text!'#13#10'}';

Var
  ReturnValue: TComment;

Begin
  ReturnValue := TPascalComment.CreateComment(strCmt, 12, 23);
  Try
    CheckEquals(4, ReturnValue.TokenCount);
    CheckEquals('Hello', ReturnValue.Tokens[0].Token);
    CheckEquals(12 + 01, ReturnValue.Tokens[0].Line);
    CheckEquals(      1, ReturnValue.Tokens[0].Column);
    CheckEquals('Dave', ReturnValue.Tokens[2].Token);
    CheckEquals(12 + 01, ReturnValue.Tokens[2].Line);
    CheckEquals(      7, ReturnValue.Tokens[2].Column);
    CheckEquals('!', ReturnValue.Tokens[3].Token);
    CheckEquals(12 + 01, ReturnValue.Tokens[3].Line);
    CheckEquals(     11, ReturnValue.Tokens[3].Column);
    CheckEquals(2, ReturnValue.TagCount);
    CheckEquals('tag', ReturnValue.Tag[0].Name);
    CheckEquals(12 + 02, ReturnValue.Tag[0].Line);
    CheckEquals(      2, ReturnValue.Tag[0].Column);
    CheckEquals('This', ReturnValue.Tag[0].Tokens[0].Token);
    CheckEquals(12 + 02, ReturnValue.Tag[0].Tokens[0].Line);
    CheckEquals(      6, ReturnValue.Tag[0].Tokens[0].Column);
    CheckEquals('is', ReturnValue.Tag[0].Tokens[2].Token);
    CheckEquals(12 + 02, ReturnValue.Tag[0].Tokens[2].Line);
    CheckEquals(     11, ReturnValue.Tag[0].Tokens[2].Column);
    CheckEquals('tag', ReturnValue.Tag[0].Tokens[4].Token);
    CheckEquals(12 + 02, ReturnValue.Tag[0].Tokens[4].Line);
    CheckEquals(     14, ReturnValue.Tag[0].Tokens[4].Column);
    CheckEquals('text', ReturnValue.Tag[0].Tokens[6].Token);
    CheckEquals(12 + 02, ReturnValue.Tag[0].Tokens[6].Line);
    CheckEquals(     18, ReturnValue.Tag[0].Tokens[6].Column);
    CheckEquals('.', ReturnValue.Tag[0].Tokens[7].Token);
    CheckEquals(12 + 02, ReturnValue.Tag[0].Tokens[7].Line);
    CheckEquals(     22, ReturnValue.Tag[0].Tokens[7].Column);
    CheckEquals('tag2', ReturnValue.Tag[1].Name);
    CheckEquals(12 + 03, ReturnValue.Tag[1].Line);
    CheckEquals(     04, ReturnValue.Tag[1].Column);
    CheckEquals('This', ReturnValue.Tag[1].Tokens[0].Token);
    CheckEquals(12 + 03, ReturnValue.Tag[1].Tokens[0].Line);
    CheckEquals(     09, ReturnValue.Tag[1].Tokens[0].Column);
    CheckEquals('is', ReturnValue.Tag[1].Tokens[2].Token);
    CheckEquals(12 + 03, ReturnValue.Tag[1].Tokens[2].Line);
    CheckEquals(     14, ReturnValue.Tag[1].Tokens[2].Column);
    CheckEquals('more', ReturnValue.Tag[1].Tokens[4].Token);
    CheckEquals(12 + 03, ReturnValue.Tag[1].Tokens[4].Line);
    CheckEquals(     17, ReturnValue.Tag[1].Tokens[4].Column);
    CheckEquals('tag', ReturnValue.Tag[1].Tokens[6].Token);
    CheckEquals(12 + 03, ReturnValue.Tag[1].Tokens[6].Line);
    CheckEquals(     22, ReturnValue.Tag[1].Tokens[6].Column);
    CheckEquals('text', ReturnValue.Tag[1].Tokens[8].Token);
    CheckEquals(12 + 03, ReturnValue.Tag[1].Tokens[8].Line);
    CheckEquals(     26, ReturnValue.Tag[1].Tokens[8].Column);
    CheckEquals('!', ReturnValue.Tag[1].Tokens[9].Token);
    CheckEquals(12 + 03, ReturnValue.Tag[1].Tokens[9].Line);
    CheckEquals(     30, ReturnValue.Tag[1].Tokens[9].Column);
    CheckEquals('Hello Dave!', ReturnValue.AsString(999, True));
    CheckEquals('This is tag text.', ReturnValue.Tag[0].AsString(999, True));
    CheckEquals('This is more tag text!', ReturnValue.Tag[1].AsString(999, True));
  Finally
    ReturnValue.Free;
  End;
End;

Procedure TestTPascalComment.TestCreateBracePascalTaggedComment;

Const
  //                  1         2         3         4         5         6
  //        0123456789012345678901234567890123456789012345678901234567890123456789
  strCmt = '{: Hello Dave! @tag This is tag text. @tag2 This is more Tag text! }';

Var
  ReturnValue: TComment;

Begin
  ReturnValue := TPascalComment.CreateComment(strCmt, 12, 23);
  Try
    CheckEquals(4, ReturnValue.TokenCount);
    CheckEquals('Hello', ReturnValue.Tokens[0].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[0].Line);
    CheckEquals(23 + 03, ReturnValue.Tokens[0].Column);
    CheckEquals('Dave', ReturnValue.Tokens[2].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[2].Line);
    CheckEquals(23 + 09, ReturnValue.Tokens[2].Column);
    CheckEquals('!', ReturnValue.Tokens[3].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[3].Line);
    CheckEquals(23 + 13, ReturnValue.Tokens[3].Column);
    CheckEquals(2, ReturnValue.TagCount);
    CheckEquals('tag', ReturnValue.Tag[0].Name);
    CheckEquals(12 + 00, ReturnValue.Tag[0].Line);
    CheckEquals(23 + 16, ReturnValue.Tag[0].Column);
    CheckEquals('This', ReturnValue.Tag[0].Tokens[0].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[0].Tokens[0].Line);
    CheckEquals(23 + 20, ReturnValue.Tag[0].Tokens[0].Column);
    CheckEquals('is', ReturnValue.Tag[0].Tokens[2].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[0].Tokens[2].Line);
    CheckEquals(23 + 25, ReturnValue.Tag[0].Tokens[2].Column);
    CheckEquals('tag', ReturnValue.Tag[0].Tokens[4].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[0].Tokens[4].Line);
    CheckEquals(23 + 28, ReturnValue.Tag[0].Tokens[4].Column);
    CheckEquals('text', ReturnValue.Tag[0].Tokens[6].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[0].Tokens[6].Line);
    CheckEquals(23 + 32, ReturnValue.Tag[0].Tokens[6].Column);
    CheckEquals('.', ReturnValue.Tag[0].Tokens[7].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[0].Tokens[7].Line);
    CheckEquals(23 + 36, ReturnValue.Tag[0].Tokens[7].Column);
    CheckEquals('tag2', ReturnValue.Tag[1].Name);
    CheckEquals(12 + 00, ReturnValue.Tag[1].Line);
    CheckEquals(23 + 39, ReturnValue.Tag[1].Column);
    CheckEquals('This', ReturnValue.Tag[1].Tokens[0].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[1].Tokens[0].Line);
    CheckEquals(23 + 44, ReturnValue.Tag[1].Tokens[0].Column);
    CheckEquals('is', ReturnValue.Tag[1].Tokens[2].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[1].Tokens[2].Line);
    CheckEquals(23 + 49, ReturnValue.Tag[1].Tokens[2].Column);
    CheckEquals('more', ReturnValue.Tag[1].Tokens[4].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[1].Tokens[4].Line);
    CheckEquals(23 + 52, ReturnValue.Tag[1].Tokens[4].Column);
    CheckEquals('tag', ReturnValue.Tag[1].Tokens[6].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[1].Tokens[6].Line);
    CheckEquals(23 + 57, ReturnValue.Tag[1].Tokens[6].Column);
    CheckEquals('text', ReturnValue.Tag[1].Tokens[8].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[1].Tokens[8].Line);
    CheckEquals(23 + 61, ReturnValue.Tag[1].Tokens[8].Column);
    CheckEquals('!', ReturnValue.Tag[1].Tokens[9].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[1].Tokens[9].Line);
    CheckEquals(23 + 65, ReturnValue.Tag[1].Tokens[9].Column);
    CheckEquals('Hello Dave!', ReturnValue.AsString(999, True));
    CheckEquals('This is tag text.', ReturnValue.Tag[0].AsString(999, True));
    CheckEquals('This is more tag text!', ReturnValue.Tag[1].AsString(999, True));
  Finally
    ReturnValue.Free;
  End;
End;

Procedure TestTPascalComment.TestCreateCPPLineComment;

Const
  //        01234567890123456789012345678901
  strCmt = '//: Sorry Dave, I can`t do that!';

Var
  ReturnValue: TComment;

Begin
  ReturnValue := TPascalComment.CreateComment(strCmt, 12, 23);
  Try
    CheckEquals(15, ReturnValue.TokenCount);
    CheckEquals('Sorry', ReturnValue.Tokens[0].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[0].Line);
    CheckEquals(23 + 04, ReturnValue.Tokens[0].Column);
    CheckEquals('Dave', ReturnValue.Tokens[2].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[2].Line);
    CheckEquals(23 + 10, ReturnValue.Tokens[2].Column);
    CheckEquals(',', ReturnValue.Tokens[3].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[3].Line);
    CheckEquals(23 + 14, ReturnValue.Tokens[3].Column);
    CheckEquals('I', ReturnValue.Tokens[5].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[5].Line);
    CheckEquals(23 + 16, ReturnValue.Tokens[5].Column);
    CheckEquals('can', ReturnValue.Tokens[7].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[7].Line);
    CheckEquals(23 + 18, ReturnValue.Tokens[7].Column);
    CheckEquals('`', ReturnValue.Tokens[8].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[8].Line);
    CheckEquals(23 + 21, ReturnValue.Tokens[8].Column);
    CheckEquals('t', ReturnValue.Tokens[9].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[9].Line);
    CheckEquals(23 + 22, ReturnValue.Tokens[9].Column);
    CheckEquals('do', ReturnValue.Tokens[11].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[11].Line);
    CheckEquals(23 + 24, ReturnValue.Tokens[11].Column);
    CheckEquals('that', ReturnValue.Tokens[13].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[13].Line);
    CheckEquals(23 + 27, ReturnValue.Tokens[13].Column);
    CheckEquals('!', ReturnValue.Tokens[14].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[14].Line);
    CheckEquals(23 + 31, ReturnValue.Tokens[14].Column);
    CheckEquals('Sorry Dave, I can`t do that!', ReturnValue.AsString(999, True));
  Finally
    ReturnValue.Free;
  End;
End;

Procedure TestTPascalComment.TestCreateCPPLineTaggedComment;

Const
  //                  1         2         3         4         5         6
  //        0123456789012345678901234567890123456789012345678901234567890123456789
  strCmt = '//: Hello Dave! @tag This is tag text. @tag2 This is more Tag text!';

Var
  ReturnValue: TComment;

Begin
  ReturnValue := TPascalComment.CreateComment(strCmt, 12, 23);
  Try
    CheckEquals(4, ReturnValue.TokenCount);
    CheckEquals('Hello', ReturnValue.Tokens[0].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[0].Line);
    CheckEquals(23 + 04, ReturnValue.Tokens[0].Column);
    CheckEquals('Dave', ReturnValue.Tokens[2].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[2].Line);
    CheckEquals(23 + 10, ReturnValue.Tokens[2].Column);
    CheckEquals('!', ReturnValue.Tokens[3].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[3].Line);
    CheckEquals(23 + 14, ReturnValue.Tokens[3].Column);
    CheckEquals(2, ReturnValue.TagCount);
    CheckEquals('tag', ReturnValue.Tag[0].Name);
    CheckEquals(12 + 00, ReturnValue.Tag[0].Line);
    CheckEquals(23 + 17, ReturnValue.Tag[0].Column);
    CheckEquals('This', ReturnValue.Tag[0].Tokens[0].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[0].Tokens[0].Line);
    CheckEquals(23 + 21, ReturnValue.Tag[0].Tokens[0].Column);
    CheckEquals('is', ReturnValue.Tag[0].Tokens[2].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[0].Tokens[2].Line);
    CheckEquals(23 + 26, ReturnValue.Tag[0].Tokens[2].Column);
    CheckEquals('tag', ReturnValue.Tag[0].Tokens[4].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[0].Tokens[4].Line);
    CheckEquals(23 + 29, ReturnValue.Tag[0].Tokens[4].Column);
    CheckEquals('text', ReturnValue.Tag[0].Tokens[6].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[0].Tokens[6].Line);
    CheckEquals(23 + 33, ReturnValue.Tag[0].Tokens[6].Column);
    CheckEquals('.', ReturnValue.Tag[0].Tokens[7].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[0].Tokens[7].Line);
    CheckEquals(23 + 37, ReturnValue.Tag[0].Tokens[7].Column);
    CheckEquals('tag2', ReturnValue.Tag[1].Name);
    CheckEquals(12 + 00, ReturnValue.Tag[1].Line);
    CheckEquals(23 + 40, ReturnValue.Tag[1].Column);
    CheckEquals('This', ReturnValue.Tag[1].Tokens[0].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[1].Tokens[0].Line);
    CheckEquals(23 + 45, ReturnValue.Tag[1].Tokens[0].Column);
    CheckEquals('is', ReturnValue.Tag[1].Tokens[2].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[1].Tokens[2].Line);
    CheckEquals(23 + 50, ReturnValue.Tag[1].Tokens[2].Column);
    CheckEquals('more', ReturnValue.Tag[1].Tokens[4].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[1].Tokens[4].Line);
    CheckEquals(23 + 53, ReturnValue.Tag[1].Tokens[4].Column);
    CheckEquals('tag', ReturnValue.Tag[1].Tokens[6].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[1].Tokens[6].Line);
    CheckEquals(23 + 58, ReturnValue.Tag[1].Tokens[6].Column);
    CheckEquals('text', ReturnValue.Tag[1].Tokens[8].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[1].Tokens[8].Line);
    CheckEquals(23 + 62, ReturnValue.Tag[1].Tokens[8].Column);
    CheckEquals('!', ReturnValue.Tag[1].Tokens[9].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[1].Tokens[9].Line);
    CheckEquals(23 + 66, ReturnValue.Tag[1].Tokens[9].Column);
    CheckEquals('Hello Dave!', ReturnValue.AsString(999, True));
    CheckEquals('This is tag text.', ReturnValue.Tag[0].AsString(999, True));
    CheckEquals('This is more Tag text!', ReturnValue.Tag[1].AsString(999, True));
  Finally
    ReturnValue.Free;
  End;
End;

Procedure TestTPascalComment.TestCreateStdMultiLinePascalComment;

Const       
  //                  1                     1
  //        012345678901234        1234567890123456789
  strCmt = '(** Sorry Dave,'#13#10'I can`t do that! **)';

Var
  ReturnValue: TComment;

Begin
  ReturnValue := TPascalComment.CreateComment(strCmt, 12, 23);
  Try
    CheckEquals(15, ReturnValue.TokenCount);
    CheckEquals('Sorry', ReturnValue.Tokens[0].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[0].Line);
    CheckEquals(23 + 04, ReturnValue.Tokens[0].Column);
    CheckEquals('Dave', ReturnValue.Tokens[2].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[2].Line);
    CheckEquals(23 + 10, ReturnValue.Tokens[2].Column);
    CheckEquals(',', ReturnValue.Tokens[3].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[3].Line);
    CheckEquals(23 + 14, ReturnValue.Tokens[3].Column);
    CheckEquals('I', ReturnValue.Tokens[5].Token);
    CheckEquals(12 + 01, ReturnValue.Tokens[5].Line);
    CheckEquals(      1, ReturnValue.Tokens[5].Column);
    CheckEquals('can', ReturnValue.Tokens[7].Token);
    CheckEquals(12 + 01, ReturnValue.Tokens[7].Line);
    CheckEquals(      3, ReturnValue.Tokens[7].Column);
    CheckEquals('`', ReturnValue.Tokens[8].Token);
    CheckEquals(12 + 01, ReturnValue.Tokens[8].Line);
    CheckEquals(      6, ReturnValue.Tokens[8].Column);
    CheckEquals('t', ReturnValue.Tokens[9].Token);
    CheckEquals(12 + 01, ReturnValue.Tokens[9].Line);
    CheckEquals(      7, ReturnValue.Tokens[9].Column);
    CheckEquals('do', ReturnValue.Tokens[11].Token);
    CheckEquals(12 + 01, ReturnValue.Tokens[11].Line);
    CheckEquals(      9, ReturnValue.Tokens[11].Column);
    CheckEquals('that', ReturnValue.Tokens[13].Token);
    CheckEquals(12 + 01, ReturnValue.Tokens[13].Line);
    CheckEquals(     12, ReturnValue.Tokens[13].Column);
    CheckEquals('!', ReturnValue.Tokens[14].Token);
    CheckEquals(12 + 01, ReturnValue.Tokens[14].Line);
    CheckEquals(     16, ReturnValue.Tokens[14].Column);
    CheckEquals('Sorry Dave, I can`t do that!', ReturnValue.AsString(999, True));
  Finally
    ReturnValue.Free;
  End;
End;

Procedure TestTPascalComment.TestCreateStdPascalComment;

Const       
  //                  1         2         3
  //        0123456789012345678901234567890123456789
  strCmt = '(** Sorry Dave, I can`t do that! **)';

Var
  ReturnValue: TComment;

Begin
  ReturnValue := TPascalComment.CreateComment(strCmt, 12, 23);
  Try
    CheckEquals(15, ReturnValue.TokenCount);
    CheckEquals('Sorry', ReturnValue.Tokens[0].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[0].Line);
    CheckEquals(23 + 04, ReturnValue.Tokens[0].Column);
    CheckEquals('Dave', ReturnValue.Tokens[2].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[2].Line);
    CheckEquals(23 + 10, ReturnValue.Tokens[2].Column);
    CheckEquals(',', ReturnValue.Tokens[3].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[3].Line);
    CheckEquals(23 + 14, ReturnValue.Tokens[3].Column);
    CheckEquals('I', ReturnValue.Tokens[5].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[5].Line);
    CheckEquals(23 + 16, ReturnValue.Tokens[5].Column);
    CheckEquals('can', ReturnValue.Tokens[7].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[7].Line);
    CheckEquals(23 + 18, ReturnValue.Tokens[7].Column);
    CheckEquals('`', ReturnValue.Tokens[8].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[8].Line);
    CheckEquals(23 + 21, ReturnValue.Tokens[8].Column);
    CheckEquals('t', ReturnValue.Tokens[9].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[9].Line);
    CheckEquals(23 + 22, ReturnValue.Tokens[9].Column);
    CheckEquals('do', ReturnValue.Tokens[11].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[11].Line);
    CheckEquals(23 + 24, ReturnValue.Tokens[11].Column);
    CheckEquals('that', ReturnValue.Tokens[13].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[13].Line);
    CheckEquals(23 + 27, ReturnValue.Tokens[13].Column);
    CheckEquals('!', ReturnValue.Tokens[14].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[14].Line);
    CheckEquals(23 + 31, ReturnValue.Tokens[14].Column);
    CheckEquals('Sorry Dave, I can`t do that!', ReturnValue.AsString(999, True));
  Finally
    ReturnValue.Free;
  End;
End;

Procedure TestTPascalComment.TestCreateStdPascalMultiLineTaggedComment;

Const
  //                  
  //        012        12345678901        12345678901234567890123       123456789012345678901234567890        123
  strCmt = '(**'#13#10'Hello Dave!'#13#10'@tag This is tag text.'#13#10'  @tag2 This is more Tag text!'#13#10'**)';

Var
  ReturnValue: TComment;

Begin
  ReturnValue := TPascalComment.CreateComment(strCmt, 12, 23);
  Try
    CheckEquals(4, ReturnValue.TokenCount);
    CheckEquals('Hello', ReturnValue.Tokens[0].Token);
    CheckEquals(12 + 01, ReturnValue.Tokens[0].Line);
    CheckEquals(      1, ReturnValue.Tokens[0].Column);
    CheckEquals('Dave', ReturnValue.Tokens[2].Token);
    CheckEquals(12 + 01, ReturnValue.Tokens[2].Line);
    CheckEquals(      7, ReturnValue.Tokens[2].Column);
    CheckEquals('!', ReturnValue.Tokens[3].Token);
    CheckEquals(12 + 01, ReturnValue.Tokens[3].Line);
    CheckEquals(     11, ReturnValue.Tokens[3].Column);
    CheckEquals(2, ReturnValue.TagCount);
    CheckEquals('tag', ReturnValue.Tag[0].Name);
    CheckEquals(12 + 02, ReturnValue.Tag[0].Line);
    CheckEquals(      2, ReturnValue.Tag[0].Column);
    CheckEquals('This', ReturnValue.Tag[0].Tokens[0].Token);
    CheckEquals(12 + 02, ReturnValue.Tag[0].Tokens[0].Line);
    CheckEquals(      6, ReturnValue.Tag[0].Tokens[0].Column);
    CheckEquals('is', ReturnValue.Tag[0].Tokens[2].Token);
    CheckEquals(12 + 02, ReturnValue.Tag[0].Tokens[2].Line);
    CheckEquals(     11, ReturnValue.Tag[0].Tokens[2].Column);
    CheckEquals('tag', ReturnValue.Tag[0].Tokens[4].Token);
    CheckEquals(12 + 02, ReturnValue.Tag[0].Tokens[4].Line);
    CheckEquals(     14, ReturnValue.Tag[0].Tokens[4].Column);
    CheckEquals('text', ReturnValue.Tag[0].Tokens[6].Token);
    CheckEquals(12 + 02, ReturnValue.Tag[0].Tokens[6].Line);
    CheckEquals(     18, ReturnValue.Tag[0].Tokens[6].Column);
    CheckEquals('.', ReturnValue.Tag[0].Tokens[7].Token);
    CheckEquals(12 + 02, ReturnValue.Tag[0].Tokens[7].Line);
    CheckEquals(     22, ReturnValue.Tag[0].Tokens[7].Column);
    CheckEquals('tag2', ReturnValue.Tag[1].Name);
    CheckEquals(12 + 03, ReturnValue.Tag[1].Line);
    CheckEquals(     04, ReturnValue.Tag[1].Column);
    CheckEquals('This', ReturnValue.Tag[1].Tokens[0].Token);
    CheckEquals(12 + 03, ReturnValue.Tag[1].Tokens[0].Line);
    CheckEquals(     09, ReturnValue.Tag[1].Tokens[0].Column);
    CheckEquals('is', ReturnValue.Tag[1].Tokens[2].Token);
    CheckEquals(12 + 03, ReturnValue.Tag[1].Tokens[2].Line);
    CheckEquals(     14, ReturnValue.Tag[1].Tokens[2].Column);
    CheckEquals('more', ReturnValue.Tag[1].Tokens[4].Token);
    CheckEquals(12 + 03, ReturnValue.Tag[1].Tokens[4].Line);
    CheckEquals(     17, ReturnValue.Tag[1].Tokens[4].Column);
    CheckEquals('tag', ReturnValue.Tag[1].Tokens[6].Token);
    CheckEquals(12 + 03, ReturnValue.Tag[1].Tokens[6].Line);
    CheckEquals(     22, ReturnValue.Tag[1].Tokens[6].Column);
    CheckEquals('text', ReturnValue.Tag[1].Tokens[8].Token);
    CheckEquals(12 + 03, ReturnValue.Tag[1].Tokens[8].Line);
    CheckEquals(     26, ReturnValue.Tag[1].Tokens[8].Column);
    CheckEquals('!', ReturnValue.Tag[1].Tokens[9].Token);
    CheckEquals(12 + 03, ReturnValue.Tag[1].Tokens[9].Line);
    CheckEquals(     30, ReturnValue.Tag[1].Tokens[9].Column);
    CheckEquals('Hello Dave!', ReturnValue.AsString(999, True));
    CheckEquals('This is tag text.', ReturnValue.Tag[0].AsString(999, True));
    CheckEquals('This is more tag text!', ReturnValue.Tag[1].AsString(999, True));
  Finally
    ReturnValue.Free;
  End;
End;

Procedure TestTPascalComment.TestCreateStdPascalTaggedComment;

Const
  //                  1         2         3         4         5         6
  //        0123456789012345678901234567890123456789012345678901234567890123456789
  strCmt = '(** Hello Dave! @tag This is tag text. @tag2 This is more Tag text! **)';

Var
  ReturnValue: TComment;

Begin
  ReturnValue := TPascalComment.CreateComment(strCmt, 12, 23);
  Try
    CheckEquals(4, ReturnValue.TokenCount);
    CheckEquals('Hello', ReturnValue.Tokens[0].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[0].Line);
    CheckEquals(23 + 04, ReturnValue.Tokens[0].Column);
    CheckEquals('Dave', ReturnValue.Tokens[2].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[2].Line);
    CheckEquals(23 + 10, ReturnValue.Tokens[2].Column);
    CheckEquals('!', ReturnValue.Tokens[3].Token);
    CheckEquals(12 + 00, ReturnValue.Tokens[3].Line);
    CheckEquals(23 + 14, ReturnValue.Tokens[3].Column);
    CheckEquals(2, ReturnValue.TagCount);
    CheckEquals('tag', ReturnValue.Tag[0].Name);
    CheckEquals(12 + 00, ReturnValue.Tag[0].Line);
    CheckEquals(23 + 17, ReturnValue.Tag[0].Column);
    CheckEquals('This', ReturnValue.Tag[0].Tokens[0].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[0].Tokens[0].Line);
    CheckEquals(23 + 21, ReturnValue.Tag[0].Tokens[0].Column);
    CheckEquals('is', ReturnValue.Tag[0].Tokens[2].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[0].Tokens[2].Line);
    CheckEquals(23 + 26, ReturnValue.Tag[0].Tokens[2].Column);
    CheckEquals('tag', ReturnValue.Tag[0].Tokens[4].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[0].Tokens[4].Line);
    CheckEquals(23 + 29, ReturnValue.Tag[0].Tokens[4].Column);
    CheckEquals('text', ReturnValue.Tag[0].Tokens[6].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[0].Tokens[6].Line);
    CheckEquals(23 + 33, ReturnValue.Tag[0].Tokens[6].Column);
    CheckEquals('.', ReturnValue.Tag[0].Tokens[7].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[0].Tokens[7].Line);
    CheckEquals(23 + 37, ReturnValue.Tag[0].Tokens[7].Column);
    CheckEquals('tag2', ReturnValue.Tag[1].Name);
    CheckEquals(12 + 00, ReturnValue.Tag[1].Line);
    CheckEquals(23 + 40, ReturnValue.Tag[1].Column);
    CheckEquals('This', ReturnValue.Tag[1].Tokens[0].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[1].Tokens[0].Line);
    CheckEquals(23 + 45, ReturnValue.Tag[1].Tokens[0].Column);
    CheckEquals('is', ReturnValue.Tag[1].Tokens[2].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[1].Tokens[2].Line);
    CheckEquals(23 + 50, ReturnValue.Tag[1].Tokens[2].Column);
    CheckEquals('more', ReturnValue.Tag[1].Tokens[4].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[1].Tokens[4].Line);
    CheckEquals(23 + 53, ReturnValue.Tag[1].Tokens[4].Column);
    CheckEquals('tag', ReturnValue.Tag[1].Tokens[6].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[1].Tokens[6].Line);
    CheckEquals(23 + 58, ReturnValue.Tag[1].Tokens[6].Column);
    CheckEquals('text', ReturnValue.Tag[1].Tokens[8].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[1].Tokens[8].Line);
    CheckEquals(23 + 62, ReturnValue.Tag[1].Tokens[8].Column);
    CheckEquals('!', ReturnValue.Tag[1].Tokens[9].Token);
    CheckEquals(12 + 00, ReturnValue.Tag[1].Tokens[9].Line);
    CheckEquals(23 + 66, ReturnValue.Tag[1].Tokens[9].Column);
    CheckEquals('Hello Dave!', ReturnValue.AsString(999, True));
    CheckEquals('This is tag text.', ReturnValue.Tag[0].AsString(999, True));
    CheckEquals('This is more tag text!', ReturnValue.Tag[1].AsString(999, True));
  Finally
    ReturnValue.Free;
  End;
End;

Initialization
  RegisterTest('PascalModule Tests', TestTPascalComment.Suite);
End.
