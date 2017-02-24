(**

  This module contains an Object Pascal specific comment implementation.

  @Author  David Hoyle
  @Version 1.0
  @Date    24 Feb 2017

**)
Unit BADI.Pascal.Comment;

Interface

Uses
  BADI.Comment;

Type
  (** A pascal specific implementation of comments. **)
  TPascalComment = Class(TComment)
  Public
    Class Function CreateComment(Const strComment: String; iLine, iCol: Integer): TComment;
      Override;
  End;

Implementation

uses
  BADI.Functions;

(**


  This method is a class method to first check the comment for being a
  documentation comment and then creating an instance of a TComment class and
  parsing the comment via the constructor.

  @precon  strComment is the full comment to be checked and parsed, iLine is
           the line number of the comment and iCol is the column number of
           the comment.

  @postcon Returns Nil if this is not a documentation comment or returns a
           valid TComment class.

  @param   strComment as a String as a constant
  @param   iLine      as an Integer
  @param   iCol       as an Integer
  @return  a TComment

**)
Class Function TPascalComment.CreateComment(Const strComment: String; iLine, iCol: Integer)
  : TComment;

Var
  strText: String;

Begin
  Result := Nil;
  strText := strComment;
  If Length(strText) > 0 Then
    Begin
      Case strText[1] Of
        '/': strText := Copy(strText, 3, Length(strText) - 2);
        '{': strText := Copy(strText, 2, Length(strText) - 2);
        '(': strText := Copy(strText, 3, Length(strText) - 4);
      End;
      If Length(strText) > 0 Then
        Begin
          If strText[Length(strText)] = '*' Then
            SetLength(strText, Length(strText) - 1);
          If Length(strText) > 0 Then
            Begin
              If (IsInSet(strText[1], [':', '*'])) Then
                Begin;
                  strText := Copy(strText, 2, Length(strText) - 1);
                  Result := Create(strText, iLine, iCol);
                End;
            End;
        End;
    End;
End;

End.
