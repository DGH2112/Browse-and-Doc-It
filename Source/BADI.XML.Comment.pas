(**

  This module contains a class to represent an XML Comment.

  @Author  David Hoyle
  @Version 1.0
  @Date    29 Oct 2017

**)
Unit BADI.XML.Comment;

Interface

Uses
  BADI.Comment;

Type
  (** A XML specific implementation of comments. **)
  TXMLComment = Class(TComment)
  Public
    Class Function CreateComment(Const strComment: String; Const iLine, iCol: Integer): TComment;
      Override;
  End;

Implementation

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
class Function TXMLComment.CreateComment(Const strComment: String; Const iLine, iCol: Integer): TComment;

Const
  iSecondChar = 2;
  
Var
  strText : String;

begin //: @note Not currently configured or used.
  Result := Nil;
  strText := strComment;
  If Length(strText) > 0 Then
    Begin
      Case strText[1] Of
        '/' : strText := Copy(strText, iSecondChar, Length(strText) - 1);
      End;
      If Length(strText) > 0 Then
        Begin
          If strText[1] = '*' Then
            strText := Copy(strText, iSecondChar, Length(strText) - (iSecondChar + 1));
          If strText[1] = '/' Then
            strText := Copy(strText, iSecondChar, Length(strText) - 1);
          If Length(strText) > 0 Then
            Begin
              If strText[1] = ':' Then
                Begin;
                  strText := Copy(strText, iSecondChar, Length(strText) - 1);
                  Result := Create(strText, iLine, iCol);
                End
              Else If strText[1] = '*' Then
                Begin;
                  strText := Copy(strText, iSecondChar, Length(strText) - iSecondChar);
                  Result := Create(strText, iLine, iCol);
                End;
            End;
        End;
    End;
end;

End.
