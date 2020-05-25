(**
  
  This module contains DUnit test for the Browse and Doc It code.

  @Author  David Hoyle
  @Version 1.002
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
Unit Test.BADI.BackusNaur.Comment;

Interface

Uses
  TestFramework,
  Test.BADI.Base.Module,
  BADI.BackusNaur.Comment;

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

Implementation

Uses
  BADI.Comment;

//
// Test Methods for Class TBackusNaurComment.
//
Procedure TestTBackusNaurComment.SetUp;

Begin
  FBackusNaurComment := TBackusNaurComment.Create('This is a comment.', 12, 23, 0);
End;

Procedure TestTBackusNaurComment.TearDown;

Begin
  FBackusNaurComment.Free;
End;

Procedure TestTBackusNaurComment.TestCreateComment;

Var
  C: TComment;

Begin
  Checkequals(8, FBackusNaurComment.TokenCount);
  Checkequals(12, FBackusNaurComment.Line);
  Checkequals(23, FBackusNaurComment.Column);
  C := TBackusNaurComment.CreateComment('/* This is a comment. */', 1, 2);
  Check(C = Nil, '/* Comment */ is not NULL');
  C := TBackusNaurComment.CreateComment('// This is a comment.', 1, 2);
  Check(C = Nil, '// Comment is not NULL');
  C := TBackusNaurComment.CreateComment('/** This is a comment. **/', 1, 2);
  Try
    Check(C <> Nil, '/** Comment **/ is NULL');
    Checkequals(8, C.TokenCount);
    Checkequals(1, C.Line);
    Checkequals(2, C.Column);
  Finally
    C.Free;
  End;
  C := TBackusNaurComment.CreateComment('//: This is a comment.', 1, 2);
  Try
    Check(C <> Nil, '//: Comment is NULL');
    Checkequals(8, C.TokenCount);
    Checkequals(1, C.Line);
    Checkequals(2, C.Column);
  Finally
    C.Free;
  End;
End;

Initialization
  RegisterTest('Backus-Naur', TestTBackusNaurComment.Suite);
End.
