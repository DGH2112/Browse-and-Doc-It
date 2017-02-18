(**

  This module contains a class to represent each token created by the tokenizers and then parsed
  by the parsers.

  @Author  David Hoyle
  @Version 1.0
  @Date    18 Feb 2017

**)
Unit BADI.TokenInfo;

Interface

Uses
  BADI.Types;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This is a class the store information about each token **)
  TTokenInfo = Class
  {$IFDEF D2005} Strict {$ENDIF} Private
    FToken : String;
    FColumn : Integer;
    FBufferPos: Integer;
    FLine: Integer;
    FLength : Integer;
    FTokenType: TBADITokenType;
    FUToken : String;
    FReference : TTokenReference;
  Public
    Constructor Create(const strToken : String; iPos, iLine, iCol,
      iLength : Integer; TType : TBADITokenType); Overload;
    Procedure Append(const strToken : String);
    (**
      Returns the token as a string.
      @precon  None.
      @postcon Returns the token as a string.
      @return  a String
    **)
    Property Token : String read FToken;
    (**
      Returns the uppercase version of the token. Used for keyword comparisons.
      @precon  None.
      @postcon Returns the uppercase version of the token. Used for keyword comparisons.
      @return  a String
    **)
    Property UToken : String read FUToken;
    (**
      Returns the buffer position of the token start point.
      @precon  None.
      @postcon Returns the buffer position of the token start point.
      @return  an Integer
    **)
    Property BufferPos : Integer read FBufferPos;
    (**
      Returns the line number of the token start point.
      @precon  None.
      @postcon Returns the line number of the token start point.
      @return  an Integer
    **)
    Property Line : Integer read FLine;
    (**
      Returns the column number of the token start point.
      @precon  None.
      @postcon Returns the column number of the token start point.
      @return  an Integer
    **)
    Property Column : Integer read FColumn;
    (**
      Returns the length of the token.
      @precon  None.
      @postcon Returns the length of the token.
      @return  an Integer
    **)
    Property Length : Integer read FLength;
    (**
      Returns the token type for the token.
      @precon  None.
      @postcon Returns the token type for the token.
      @return  a TBADITokenType
    **)
    Property TokenType : TBADITokenType read FTokenType;
    (**
      This property gets and sets the reference information for the token.
      @precon  None.
      @postcon Gets and sets the reference information for the token.
      @return  a TTokenReference
    **)
    Property Reference : TTokenReference Read FReference Write FReference;
  End;

Implementation

Uses
  SysUtils;

(**

  This method appends the given string to the end of the token.

  @precon  None.
  @postcon Appends the given string to the end of the token.

  @param   strToken as a String as a constant

**)
Procedure TTokenInfo.Append(const strToken : String);

Begin
  FToken := FToken + strToken;
  Inc(FLength, System.Length(strToken));
  If FTokenType In [ttReservedWord, ttDirective] Then
    FUToken := FUtoken + UpperCase(strToken);
End;

(**

  This is a constructor for the TTokenInfo class. It assigns values to all the
  properties.

  @precon  strToken is a text token to create a token info object for, iPos is
           the module buffer position of the token, iLine is the line number of
           the token, iCol is the column number of the token and iLength is the
           length of the token and TType is the enumerate type of the token
           (reserved word, identifier).
  @postcon Initialises the class.

  @param   strToken as a String as a Constant
  @param   iPos     as an Integer
  @param   iLine    as an Integer
  @param   iCol     as an Integer
  @param   iLength  as an Integer
  @param   TType    as a TBADITokenType

**)
Constructor TTokenInfo.Create(const strToken: String; iPos, iLine, iCol, iLength: Integer;
  TType: TBADITokenType);

Begin
  FToken := strToken;
  FBufferPos := iPos;
  FLine := iLine;
  FColumn := iCol;
  FLength := iLength;
  FTokenType := TType;
  FUToken := '';
  If FTokenType In [ttReservedWord, ttDirective] Then
    FUToken := UpperCase(strToken);
  FReference := trUnknown;
End;

End.
