(**

  This module contains a class which represent the abstract base container for ALL containers in
  the Browse and Doc It system.

  @Author  David Hoyle
  @Version 1.114
  @Date    16 Oct 2022

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
Unit BADI.Base.Container;

Interface

Uses
  Classes,
  Contnrs,
  BADI.TokenInfo,
  BADI.Types,
  BADI.Constants;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This class defines an object that can contain tokens and has line and
      column numbers. It is the ancestor for TTag, TComment and
      TElementContainer. **)
  TBADIBaseContainer = Class Abstract
  Strict Private
    FName   : String;
    FScope  : TScope;
    FLine   : Integer;
    FColumn : Integer;
    FTokens : TObjectList;
    FFixed  : Boolean;
  Strict Protected
    Function GetTokenCount : Integer;
    Function GetTokens(Const iIndex : Integer) : TTokenInfo;
    Function GetName: String; Virtual;
    Procedure SetName(Const Value : String); Virtual;
  Public
    Constructor Create(Const strName : String; Const AScope : TScope; Const iLine,
      iColumn : Integer); Virtual;
    Destructor Destroy; Override;
    Procedure AddToken(Const strToken : String; Const ATokenType : TBADITokenType = ttUnknown;
      Const iLine : Integer = 0; Const iColumn : Integer = 0); Overload; Virtual;
    Procedure AddToken(Const AToken : TTokenInfo); Overload; Virtual;
    Procedure AppendToken(Const AToken : TTokenInfo); Virtual;
    Procedure InsertToken(Const strToken : String; Const iIndex : Integer;
      Const ATokenType : TBADITokenType = ttUnknown); Virtual;
    Procedure DeleteToken(Const iIndex : Integer); Virtual;
    Procedure ClearTokens; Virtual;
    //: @nometric LongParameterList
    Function BuildStringRepresentation(Const boolIdentifier, boolForDocumentation : Boolean;
      Const strDelimiter : String; Const iMaxWidth : Integer;
      Const strNoSpaceBefore : TSymbols = strNoSpaceBeforeSymbols;
      Const strNoSpaceAfter : TSymbols = strNoSpaceAfterSymbols;
      Const strSpaceAfter : TSymbols = strSpaceAfterSymbols;
      Const boolShowHTML : Boolean = False) : String; Virtual;
    (**
      This property returns the name of the element.
      @precon  None.
      @postcon Returns the name of the element.
      @return  a String
    **)
    Property Name : String Read GetName Write SetName;
    (**
      This property returns the identifier name (same as name) of the element.
      @precon  None.
      @postcon Returns the identifier name (same as name) of the element.
      @return  a String
    **)
    Property Identifier : String read FName Write FName;
    (**
      This property returns the line number associated with this element.
      @precon  None.
      @postcon Returns the line number associated with this element.
      @return  an Integer
    **)
    Property Line : Integer Read FLine Write FLine;
    (**
      This property returns the column number associated with this element.
      @precon  None.
      @postcon Returns the column number associated with this element.
      @return  an Integer
    **)
    Property Column : Integer Read FColumn Write FColumn;
    (**
      This property returns the Scope of the element.
      @precon  None.
      @postcon Returns the Scope of the element.
      @return  a TScope
    **)
    Property Scope : TScope Read FScope Write FScope;
    (**
      This property returns an instance of the indexed token from the
      collection.
      @precon  iIndex must be a valid index.
      @postcon Returns an instance of the indexed token from the collection.
      @param   iIndex as       an Integer as a Constant
      @return  a TTokenInfo
    **)
    Property Tokens[Const iIndex : Integer] : TTokenInfo Read GetTokens;
    (**
      This property returns the number of tokens in the collection.
      @precon  None.
      @postcon Returns the number of tokens in the collection.
      @return  an Integer
    **)
    Property TokenCount : Integer Read GetTokenCount;
    (**
      This property determines whether the tag is a fixed tag (think <pre>).
      @precon  None.
      @postcon Returns true if the tag is fixed.
      @return  a Boolean
    **)
    Property Fixed : Boolean Read FFixed Write FFixed;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF DEBUG}
  SysUtils,
  BADI.Functions;

(**

  This method adds a TTokenInfo class representation of the given string to the token collection.

  @precon  None.
  @postcon Adds a TTokenInfo class representation of the given string to the token collection.

  @param   strToken   as a String as a constant
  @param   ATokenType as a TBADITokenType as a constant
  @param   iLine      as an Integer as a constant
  @param   iColumn    as an Integer as a constant

**)
Procedure TBADIBaseContainer.AddToken(Const strToken: String;
  Const ATokenType: TBADITokenType = ttUnknown; Const iLine : Integer = 0; Const iColumn : Integer = 0);

Begin
  AddToken(TTokenInfo.Create(strToken, 0, iLine, iColumn, Length(strToken), ATokenType));
End;

(**

  This method adds the given TTokenInfo object to the token collection.

  @precon  AToken must be a valid token instance.
  @postcon Adds the given TTokenInfo object to the token collection. Note that the calling code must not
           free this memory - it will be freed by this container.

  @param   AToken as a TTokenInfo as a constant

**)
Procedure TBADIBaseContainer.AddToken(Const AToken: TTokenInfo);

Begin
  FTokens.Add(AToken);
End;

(**

  This method append a copy of the given token to the tokens collection.

  @precon  AToken must be a valid instance of a TTokenInfo.
  @postcon Append a copy of the given token to the tokens collection. Note, the calling code is
           responsible for freeing the AToken instance only.

  @param   AToken as a TTokenInfo as a constant

**)
Procedure TBADIBaseContainer.AppendToken(Const AToken: TTokenInfo);

Begin
  AddToken(TTokenInfo.Create(AToken.Token, AToken.BufferPos, AToken.Line, AToken.Column,
    AToken.Length, AToken.TokenType));
End;

(**

  This method builds a string from the identifier and tokens and tries to present it with the style of
  code you would probably except.

  @precon  None.
  @postcon Builds a string from the identifier and tokens and tries to present it with the style of code
           you would probably except.

  @nometric LongParameterList HardCodedInteger

  @param   boolIdentifier       as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @param   strDelimiter         as a String as a constant
  @param   iMaxWidth            as an Integer as a constant
  @param   strNoSpaceBefore     as a TSymbols as a constant
  @param   strNoSpaceAfter      as a TSymbols as a constant
  @param   strSpaceAfter        as a TSymbols as a constant
  @param   boolShowHTML         as a Boolean as a constant
  @return  a String

**)
Function TBADIBaseContainer.BuildStringRepresentation(
  Const boolIdentifier, boolForDocumentation: Boolean;
  Const strDelimiter: String;
  Const iMaxWidth: Integer;
  Const strNoSpaceBefore: TSymbols = strNoSpaceBeforeSymbols;
  Const strNoSpaceAfter: TSymbols = strNoSpaceAfterSymbols;
  Const strSpaceAfter: TSymbols = strSpaceAfterSymbols;
  Const boolShowHTML: Boolean = False): String;

Const
  strLFCRSpaceSpace = #13#10#32#32;
  iTwoSpaces = 2;

Var
  iToken: Integer;
  T, L, D: TTokenInfo;
  boolSpace: Boolean;
  iLength: Integer;

Begin
  Result := '';
  If boolIdentifier Then
    Begin
      Result := Identifier;
      If (Length(Result) > 0) And (Length(strDelimiter) > 0) Then
        If TokenCount > 0 Then
          Begin
            If Not(IsInSet(strDelimiter[1], strNoSpaceBefore)) Then
              Result := Result + #32;
            Result := Result + strDelimiter;
          End;
    End;
  iLength := Length(Result);
  D := TTokenInfo.Create(strDelimiter, 0, 0, 0, Length(strDelimiter), ttSymbol);
  Try
    L := D;
    For iToken := 0 To TokenCount - 1 Do
      If Not(Tokens[iToken].TokenType In [ttHTMLStartTag, ttHTMLEndTag]) Or
        ((Tokens[iToken].TokenType In [ttHTMLStartTag, ttHTMLEndTag]) And boolShowHTML) Then
        Begin
          boolSpace := (iToken > -1) Or (strDelimiter <> '');
          T := Tokens[iToken];
          boolSpace := boolSpace And Not(IsInSet(T.Token[1], strNoSpaceBefore));
          If (L <> Nil) And (L.Length > 0) Then
            boolSpace := boolSpace And Not(IsInSet(L.Token[1], strNoSpaceAfter));
          If Result <> '' Then
            If boolSpace Or ((L.Length > 0) And (IsInSet(L.Token[1], strSpaceAfter))) Then
              If Not(boolForDocumentation And (iLength + T.Length > iMaxWidth)) Then
                Begin
                  If (L.TokenType <> ttHTMLStartTag) And (T.TokenType <> ttHTMLEndTag) Then
                    Result := Result + #32;
                  Inc(iLength);
                End
              Else
                Begin
                  Result := Result + strLFCRSpaceSpace;
                  iLength := iTwoSpaces;
                End;
          Result := Result + T.Token;
          Inc(iLength, T.Length);
          L := T;
        End;
  Finally
    D.Free;
  End;
End;

(**

  This method clears the tokens in the collection.

  @precon  None.
  @postcon Clears the tokens in the collection.

**)
Procedure TBADIBaseContainer.ClearTokens;

Begin
  FTokens.Clear;
End;

(**

  This is a constructor for the TBADIBaseContainer class.

  @precon  None.
  @postcon Create the token collection and initialises the Line and Column data.

  @param   strName as a String as a constant
  @param   AScope  as a TScope as a constant
  @param   iLine   as an Integer as a constant
  @param   iColumn as an Integer as a constant

**)
Constructor TBADIBaseContainer.Create(Const strName : String; Const AScope: TScope; Const iLine,
  iColumn  : Integer);

Begin
  FTokens := TObjectList.Create(True);
  FName := strName;
  FScope := AScope;
  FLine := iLine;
  FColumn := iColumn;
  FFixed := False;
End;

(**

  This method deletes the indexed token from the token collection.

  @precon  iIndex must be a valid index between 0 and TokenCount - 1.
  @postcon Deletes the indexed token from the token collection.

  @param   iIndex as an Integer as a constant

**)
Procedure TBADIBaseContainer.DeleteToken(Const iIndex: Integer);

Begin
  FTokens.Delete(iIndex);
End;

(**

  This is a destructor for the TBADIBaseContainer class.

  @precon  None.
  @postcon Frees the token collection and the memory belonging to the tokens.

**)
Destructor TBADIBaseContainer.Destroy;

Begin
  FTokens.Free;
  Inherited Destroy;
End;

(**

  This is a getter method for the Name property.

  @precon  None.
  @postcon Returns the name of the element. This can be override for the
           purposes of find / sorting the elements. Identifier still returns
           the FName variable.

  @return  a String

**)
Function TBADIBaseContainer.GetName: String;

Begin
  Result := FName;
End;

(**

  This is a getter method for the TokenCount property.

  @precon  None.
  @postcon Returns the number of tokens in the collection.

  @return  an Integer

**)
Function TBADIBaseContainer.GetTokenCount: Integer;

Begin
  Result := FTokens.Count;
End;

(**

  This is a getter method for the Tokens property.

  @precon  iIndex must be a valid index between 0 and TokenCount - 1.
  @postcon Returns the instance of the indexed token.

  @param   iIndex as an Integer as a constant
  @return  a TTokenInfo

**)
Function TBADIBaseContainer.GetTokens(Const iIndex: Integer): TTokenInfo;

Begin
  Result := FTokens[iIndex] As TTokenInfo;
End;

(**

  This method inserts a token at the given index in the token collection.

  @precon  None.
  @postcon Inserts a token at the given index in the token collection.

  @param   strToken   as a String as a constant
  @param   iIndex     as an Integer as a constant
  @param   ATokenType as a TBADITokenType as a constant

**)
Procedure TBADIBaseContainer.InsertToken(Const strToken: String; Const iIndex: Integer;
  Const ATokenType: TBADITokenType = ttUnknown);

Var
  iTokenIndex : Integer;

Begin
  iTokenIndex := iIndex;
  If iTokenIndex >= FTokens.Count Then
    iTokenIndex := FTokens.Count - 1;
  If iTokenIndex < 0 Then
    iTokenIndex := 0;
  FTokens.Insert(iTokenIndex, TTokenInfo.Create(strToken, 0, 0, 0, Length(strToken), ATokenType));
End;

(**

  This is a setter method for the Name property.

  @precon  None.
  @postcon Sets the name of the container.

  @param   Value as a String as a Constant

**)
Procedure TBADIBaseContainer.SetName(const Value: String);

Begin
  FName := Value;
End;

End.
