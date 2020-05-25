(**
  
  This module contains DUnit test for the Browse and Doc It code.

  @Author  David Hoyle
  @Version 1.001
  @Date    24 May 2020

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
  FTag := TTag.Create('todo', scNone, 12, 23);
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
