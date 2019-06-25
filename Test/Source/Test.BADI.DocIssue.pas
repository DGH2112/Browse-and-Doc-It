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
Unit Test.BADI.DocIssue;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.DocIssue;

Type
  TestTDocIssue = Class(TExtendedTestCase)
  Strict Private
    FDocIssue: TDocIssue;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestAsString;
  End;

Implementation

Uses
  BADI.Types;

Procedure TestTDocIssue.SetUp;

Begin
  FDocIssue := TDocIssue.Create('This is a simple message.', scNone, 1, 2, etWarning);
End;

Procedure TestTDocIssue.TearDown;

Begin
  FDocIssue.Free;
  FDocIssue := Nil;
End;

Procedure TestTDocIssue.TestAsString;

Begin
  CheckEquals('This is a simple message.', FDocIssue.AsString(True, False));
End;

Procedure TestTDocIssue.TestCreate;

Begin
  CheckEquals('This is a simple message.', FDocIssue.AsString(True, False));
  CheckEquals(scNone, FDocIssue.Scope);
  CheckEquals(1, FDocIssue.Line);
  CheckEquals(2, FDocIssue.Column);
  CheckEquals(iiWarning, FDocIssue.ImageIndex);
End;

Initialization
  RegisterTest('BaseLanguageModule Tests', TestTDocIssue.Suite);
End.
