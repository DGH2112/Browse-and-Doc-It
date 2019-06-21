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
