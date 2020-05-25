(**

  This module contains a class to represent a Backus-Naur comment.

  @Author  David Hoyle
  @Version 1.047
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
Unit BADI.BackusNaur.Comment;

Interface

Uses
  BADI.Comment;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A pascal specific implementation of comments. **)
  TBackusNaurComment = Class(TComment)
  Public
    Class Function CreateComment(Const strComment: String; Const iLine, iCol : Integer): TComment; Override;
  End;

Implementation

Uses
  BADI.Types;

(**

  This method is a class method to first check the comment for being a documentation comment and then
  creating an instance of a TComment class and parsing the comment via the constructor.

  @precon  strComment is the full comment to be checked and parsed, iLine is the line number of the
           comment and iCol is the column number of the comment.
  @postcon Returns Nil if this is not a documentation comment or returns a valid TComment class.

  @param   strComment as a String as a constant
  @param   iLine      as an Integer as a constant
  @param   iCol       as an Integer as a constant
  @return  a TComment

**)
Class Function TBackusNaurComment.CreateComment(Const strComment: String; Const iLine, iCol : Integer): TComment;

Var
  strText: String;

Begin
  Result := Nil;
  strText := strComment;
  If Length(strText) > 0 Then
    Begin
      Case strText[1] Of
        '/': strText := Copy(strText, 2, Length(strText) - 1);
      End;
      If Length(strText) > 0 Then
        Begin
          If strText[1] = '*' Then
            strText := Copy(strText, 2, Length(strText) - 3);
          If Length(strText) > 0 Then
            Begin
              If strText[1] = '/' Then
                strText := Copy(strText, 2, Length(strText) - 1);
              If Length(strText) > 0 Then
                Begin
                  If strText[1] = ':' Then
                    Begin;
                      strText := Copy(strText, 2, Length(strText) - 1);
                      Result := Create(strText, iLine, iCol, 3);
                    End
                  Else If strText[1] = '*' Then
                    Begin;
                      strText := Copy(strText, 2, Length(strText) - 2);
                      Result := Create(strText, iLine, iCol, 3);
                    End;
                End;
            End;
        End;
    End;
End;

End.
