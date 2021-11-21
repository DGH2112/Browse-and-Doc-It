(**

  This module contains a class to represent each token created by the tokenizers and then parsed
  by the parsers.

  @Author  David Hoyle
  @Version 1.001
  @Date    19 Sep 2020

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2020  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

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
Unit BADI.TokenInfo;

Interface

Uses
  BADI.Types;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This is a class the store information about each token **)
  TTokenInfo = Class
  Strict Private
    FToken : String;
    FColumn : Integer;
    FBufferPos: Integer;
    FLine: Integer;
    FLength : Integer;
    FTokenType: TBADITokenType;
    FUToken : String;
    FReference : TTokenReference;
  Public
    Constructor Create(Const strToken: String; Const iPos, iLine, iCol, iLength: Integer;
      Const TType: TBADITokenType); Overload;
    Procedure Append(Const strToken: String);
    Procedure Replace(Const strToken: String; Const TType: TBADITokenType); Overload;
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
Procedure TTokenInfo.Append(Const strToken : String);

Begin
  FToken := FToken + strToken;
  Inc(FLength, System.Length(strToken));
  If FTokenType In [ttReservedWord, ttDirective] Then
    FUToken := FUtoken + UpperCase(strToken);
End;

(**

  This is a constructor for the TTokenInfo class. It assigns values to all the properties.

  @precon  strToken is a text token to create a token info object for, iPos is the module buffer
           position of the token, iLine is the line number of the token, iCol is the column
           number of the token and iLength is the length of the token and TType is the enumerate
           type of the token (reserved word, identifier).
  @postcon Initialises the class.

  @param   strToken as a String as a constant
  @param   iPos     as an Integer as a constant
  @param   iLine    as an Integer as a constant
  @param   iCol     as an Integer as a constant
  @param   iLength  as an Integer as a constant
  @param   TType    as a TBADITokenType as a constant

**)
Constructor TTokenInfo.Create(Const strToken: String; Const iPos, iLine, iCol, iLength: Integer;
  Const TType: TBADITokenType);

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

(**

  This method allows a caller to replace the token text in the token.

  @precon  None.
  @postcon The token text is changed to that which is given.

  @param   strToken as a String as a constant
  @param   TType    as a TBADITokenType as a constant

**)
Procedure TTokenInfo.Replace(Const strToken: String; Const TType: TBADITokenType);

Begin
  FToken := strToken;
  FTokenType := TType;
  FLength := System.Length(strToken);
  FUToken := '';
  If FTokenType In [ttReservedWord, ttDirective] Then
    FUToken := UpperCase(strToken);
End;

End.
