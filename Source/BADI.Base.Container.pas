(**

  This module contains a class which represent the abtract base container for ALL containers in
  the Browse and Doc It system.

  @Author  David Hoyle
  @Version 1.0
  @Date    15 Apr 2017

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
      column numbers. It is the anscester for TTag, TComment and
      TElementContainer. **)
  TBADIBaseContainer = Class Abstract
  Strict Private
    FName : String;
    FLine : Integer;
    FColumn : Integer;
    FTokens : TObjectList;
    FFixed : Boolean;
  Strict Protected
    Function GetTokenCount : Integer;
    Function GetTokens(iIndex : Integer) : TTokenInfo;
    Function GetName: String; Virtual;
    Procedure SetName(const Value : String); Virtual;
  Public
    Constructor Create(const strName : String; iLine, iColumn  : Integer);
    Destructor Destroy; Override;
    Procedure AddToken(const strToken : String; ATokenType : TBADITokenType = ttUnknown);
      Overload; Virtual;
    Procedure AddToken(AToken : TTokenInfo); Overload; Virtual;
    Procedure AppendToken(AToken : TTokenInfo); Virtual;
    Procedure InsertToken(const strToken : String; iIndex : Integer;
      ATokenType : TBADITokenType = ttUnknown);
    Procedure DeleteToken(iIndex : Integer);
    Procedure ClearTokens;
    Function BuildStringRepresentation(boolIdentifier, boolForDocumentation : Boolean;
      const strDelimiter : String; iMaxWidth : Integer;
      strNoSpaceBefore : TSymbols = strNoSpaceBeforeSymbols;
      strNoSpaceAfter : TSymbols = strNoSpaceAfterSymbols;
      strSpaceAfter : TSymbols = strSpaceAfterSymbols;
      boolShowHTML : Boolean = False) : String; Virtual;
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
      This property returns an instance of the indexed token from the
      collection.
      @precon  iIndex must be a valid index.
      @postcon Returns an instance of the indexed token from the collection.
      @param   iIndex as       an Integer
      @return  a TTokenInfo
    **)
    Property Tokens[iIndex : Integer] : TTokenInfo Read GetTokens;
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
  BADI.Functions;

(**

  This method adds the given TTokenInfo object to the token collection.

  @precon  AToken must be a valid token instance.
  @postcon Adds the given TTokenInfo object to the token collection. Note that
           the calling code must not free this memeory - it will be freed by
           this container.

  @param   AToken as a TTokenInfo

**)
Procedure TBADIBaseContainer.AddToken(AToken: TTokenInfo);

Begin
  FTokens.Add(AToken);
End;

(**

  This method adds a TTokenInfo class representation of the given string to the
  token collection.

  @precon  None.
  @postcon Adds a TTokenInfo class representation of the given string to the
           token collection.

  @param   strToken   as a String as a Constant
  @param   ATokenType as a TBADITokenType

**)
Procedure TBADIBaseContainer.AddToken(const strToken: String; ATokenType: TBADITokenType = ttUnknown);

Begin
  AddToken(TTokenInfo.Create(strToken, 0, 0, 0, Length(strToken), ATokenType));
End;

(**

  This method append a copy of the given token to the tokens collection.

  @precon  AToken mustbe a valid instance of a TTokenInfo.
  @postcon Append a copy of the given token to the tokens collection. Note, the
           calling code is responsible for freeing the AToken instance only.

  @param   AToken as a TTokenInfo

**)
Procedure TBADIBaseContainer.AppendToken(AToken: TTokenInfo);

Begin
  AddToken(TTokenInfo.Create(AToken.Token, AToken.BufferPos, AToken.Line, AToken.Column,
    AToken.Length, AToken.TokenType));
End;

(**

  This method builds a string from the identifer and tokens and tries to
  present it with the style of code you would probably except.

  @precon  None.
  @postcon Builds a string from the identifer and tokens and tries to present
           it with the style of code you would probably except.

  @param   boolIdentifier       as a Boolean
  @param   boolForDocumentation as a Boolean
  @param   strDelimiter         as a String as a Constant
  @param   iMaxWidth            as an Integer
  @param   strNoSpaceBefore     as a TSymbols
  @param   strNoSpaceAfter      as a TSymbols
  @param   strSpaceAfter        as a TSymbols
  @param   boolShowHTML         as a Boolean
  @return  a String

**)
Function TBADIBaseContainer.BuildStringRepresentation(boolIdentifier, boolForDocumentation: Boolean;
  const strDelimiter: String; iMaxWidth: Integer; strNoSpaceBefore: TSymbols = strNoSpaceBeforeSymbols;
  strNoSpaceAfter: TSymbols = strNoSpaceAfterSymbols;
  strSpaceAfter: TSymbols = strSpaceAfterSymbols; boolShowHTML: Boolean = False): String;

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
                  Result := Result + #13#10#32#32;
                  iLength := 2;
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
  @postcon Create the token collection and initialises the Line and Column
           data.

  @param   strName as a String as a Constant
  @param   iLine   as an Integer
  @param   iColumn as an Integer

**)
Constructor TBADIBaseContainer.Create(const strName: String; iLine, iColumn: Integer);

Begin
  FTokens := TObjectList.Create(True);
  FName := strName;
  FLine := iLine;
  FColumn := iColumn;
  FFixed := False;
End;

(**

  This method deletes the indexed token from the token collection.

  @precon  iIndex must be a valid index between 0 and TokenCount - 1.
  @postcon Deletes the indexed token from the token collection.

  @param   iIndex as an Integer

**)
Procedure TBADIBaseContainer.DeleteToken(iIndex: Integer);

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

  @param   iIndex as an Integer
  @return  a TTokenInfo

**)
Function TBADIBaseContainer.GetTokens(iIndex: Integer): TTokenInfo;

Begin
  Result := FTokens[iIndex] As TTokenInfo;
End;

(**

  This method inserts a token at the given index in the token collection.

  @precon  None.
  @postcon Inserts a token at the given index in the token collection.

  @param   strToken   as a String as a Constant
  @param   iIndex     as an Integer
  @param   ATokenType as a TBADITokenType

**)
Procedure TBADIBaseContainer.InsertToken(const strToken: String; iIndex: Integer;
  ATokenType: TBADITokenType = ttUnknown);

Begin
  If iIndex >= FTokens.Count Then
    iIndex := FTokens.Count - 1;
  If iIndex < 0 Then
    iIndex := 0;
  FTokens.Insert(iIndex, TTokenInfo.Create(strToken, 0, 0, 0, Length(strToken), ATokenType));
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
