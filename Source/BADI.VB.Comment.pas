(**

  This module contains a class to represent a VB comment.

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
Unit BADI.VB.Comment;

Interface

{$INCLUDE CompilerDefinitions.Inc}

Uses
  BADI.Comment;

Type
  (** An imlpementation for visual basic comments. **)
  TVBComment = Class(TComment)
  Public
    Class Function CreateComment(Const strComment: String; Const iLine, iCol: Integer): TComment;
      Override;
  End;

Implementation

Uses
  SysUtils,
  Classes,
  BADI.Functions,
  BADI.Types;

(**

  This is a constructor for the TVBComment class.

  @precon  None.
  @postcon Parses a VB comment by simply stripping the single quotes from the text.

  @param   strComment as a String as a constant
  @param   iLine      as an Integer as a constant
  @param   iCol       as an Integer as a constant
  @return  a TComment

**)
class function TVBComment.CreateComment(const strComment: String; Const iLine, iCol: Integer): TComment;

Var
  sl : TStringList;
  iCommentLine : Integer;
  boolDocComment: Boolean;

  (**

    This function replaces the indexed character in the passed string with the new given character. This 
    is a workaround for the immutable nature of TStringList items Strings.

    @precon  none.
    @postcon Replaces the indexed character in the passed string with the new given character.

    @param   strText     as a String as a constant
    @param   iIndex      as an Integer as a constant
    @param   chCharacter as a Char as a constant
    @return  a String

  **)
  Function ReplaceCharacter(Const strText : String; Const iIndex : Integer;
    Const chCharacter : Char) : String;

  Begin
    Result := strText;
    Result[iIndex] := chCharacter;
  End;

Const
  iSecondChar = 2;

begin
  Result := Nil;
  boolDocComment := False;
  If Length(strComment) > 0 Then
    Begin
      sl := TStringList.Create;
      Try
        sl.Text := strComment;
        For iCommentLine := sl.Count - 1 DownTo 0 Do
          Begin
            If sl[iCommentLine][1] = '''' Then
              sl[iCommentLine] := ReplaceCharacter(sl[iCommentLine], 1, #32);
            If Length(sl[iCommentLine]) > 1 Then
              If (IsInSet(sl[iCommentLine][iSecondChar], [':', ''''])) Then
                Begin
                  boolDocComment := True;
                  sl[iCommentLine] := ReplaceCharacter(sl[iCommentLine], iSecondChar, #32);
                End Else
                  sl.Delete(iCommentLine);
          End;
        If boolDocComment Then
          Result := Create(sl.Text, iLine, iCol);
      Finally
        sl.Free;
      End;
    End;
end;

End.
