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
Unit Test.BADI.VB.Comment;

Interface

Uses
  TestFramework;

Type
  //
  // Test Class for the TVBComment Class Methods.
  //
  TestTVBComment = Class(TTestCase)
  Strict Private
  Public
  Published
    Procedure TestCreateComment;
  End;

Implementation

uses
  BADI.Comment,
               BADI.VB.Comment;

//
// Test methods for the class TVBComment.
//
Procedure TestTVBComment.TestCreateComment;

Var
  C: TComment;

Begin
  C := TVBComment.CreateComment('', 0, 0);
  Try
    Check(C = Nil, 'Comment is not NULL!');
  Finally
    C.Free;
  End;
  C := TVBComment.CreateComment(''' This is a standard VB Comment.', 0, 0);
  Try
    Check(C = Nil, 'Comment is not NULL!');
  Finally
    C.Free;
  End;
  C := TVBComment.CreateComment(''''' This is a standard VB Comment.', 0, 0);
  Try
    Check(C <> Nil, 'Comment is NULL!');
    CheckEquals('This is a standard VB Comment.', C.AsString(9999, True));
  Finally
    C.Free;
  End;
  C := TVBComment.CreateComment(''': This is a standard VB Comment.', 0, 0);
  Try
    Check(C <> Nil, 'Comment is NULL!');
    CheckEquals('This is a standard VB Comment.', C.AsString(9999, True));
  Finally
    C.Free;
  End;
  C := TVBComment.CreateComment(
    ''': This is a standard VB Comment.'#13#10 +
    ''': @todo Hello Dave.'#13#10, 0, 0);
  Try
    Check(C <> Nil, 'Comment is NULL!');
    CheckEquals('This is a standard VB Comment.', C.AsString(9999, True));
    CheckEquals(1, C.TagCount);
    CheckEquals('Hello Dave.', C.Tag[0].AsString(80, True));
  Finally
    C.Free;
  End;
End;

initialization
  RegisterTest('VB Module Tests', TestTVBComment.Suite);
End.
