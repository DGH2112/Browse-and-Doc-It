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
Unit Test.BADI.DocumentConflicts;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.DocIssue;

Type
  TestTDocumentConflict = Class(TExtendedTestCase)
  Strict Private
    FDocumentConflict: TDocumentConflict;
  Public
    Procedure SetUp; Override;
    Procedure TearDown; Override;
  Published
    Procedure TestCreate;
    Procedure TestAsString;
    Procedure TestCommentLine;
    Procedure TestCommentColumn;
  End;

Implementation

Uses
  BADI.Types;

Procedure TestTDocumentConflict.SetUp;
Begin
  FDocumentConflict := TDocumentConflict.Create(['First', 'Second'], 12, 23, 34,
    45, 'This is a message with a first parameter (%s) and a second parameter (%s).',
    'This is a description string.', iiDocConflictMissing);
End;

Procedure TestTDocumentConflict.TearDown;
Begin
  FDocumentConflict.Free;
  FDocumentConflict := Nil;
End;

Procedure TestTDocumentConflict.TestAsString;

Begin
  Checkequals('This is a message with a first parameter (First) and a second ' +
    'parameter (Second).', FDocumentConflict.AsString(True, False));
End;

Procedure TestTDocumentConflict.TestCommentColumn;
Begin
  Checkequals(45, FDocumentConflict.CommentColumn);
End;

Procedure TestTDocumentConflict.TestCommentLine;
Begin
  Checkequals(34, FDocumentConflict.CommentLine);
End;

Procedure TestTDocumentConflict.TestCreate;
Begin
  Checkequals(12, FDocumentConflict.Line);
  Checkequals(23, FDocumentConflict.Column);
End;

Initialization
  RegisterTest('BaseLanguageModule Tests', TestTDocumentConflict.Suite);
End.
