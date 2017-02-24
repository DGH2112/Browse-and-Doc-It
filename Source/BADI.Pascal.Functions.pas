(**

  This module contains a number of Object Pascal parser specific functions to help with parsing and
  building string representations.

  @Author  David Hoyle
  @Version 1.0
  @Date    24 Feb 2017

**)
Unit BADI.Pascal.Functions;

Interface

Uses
  BADI.Generic.FunctionDecl,
  BADI.Types,
  BADI.ElementContainer,
  BADI.TokenInfo,
  BADI.Comment,
  BADI.Pascal.Types;

  Function BuildParameterRepresentation(GF : TGenericFunction; boolShowIdentifier,
    boolForDocumentation : Boolean) : String;
  Function TypeToken(AToken : TTokenInfo; AScope : TScope; AComment : TComment;
    Container : TElementContainer) : TTypeToken;

Implementation

uses
  BADI.Pascal.Constants;

(**

  This function builds a string representation of a parameter list. It will use a short
  form for the same parameter types, i.e. Const a, b : Integer.

  @precon  GF must be a valid instance.
  @postcon A string is returned which represents the parameter list.

  @param   GF                   as a TGenericFunction
  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function BuildParameterRepresentation(GF : TGenericFunction; boolShowIdentifier,
  boolForDocumentation : Boolean) : String;

Const
  strIsSame : Array[False..True] Of String = (';', ',');

Var
  iParam : Integer;
  boolSameAsBefore :Boolean;
  boolSameAsAfter :Boolean;

Begin
  Result := '';
  For iParam := 0 To GF.ParameterCount - 1 Do
    Begin
      If boolForDocumentation Then
        Result := Result + #32#32;
      boolSameAsBefore := (iParam > 0) And (GF.Parameters[iParam].IsEquals(GF.Parameters[Pred(iParam)]));
      boolSameAsAfter := (iParam < GF.ParameterCount - 1) And (GF.Parameters[iParam].IsEquals(GF.Parameters[Succ(iParam)]));
      If Not boolSameAsBefore And boolSameAsAfter Then
        Result := Result + strParamModifier[GF.Parameters[iParam].ParamModifier] + GF.Parameters[iParam].Identifier
      Else If boolSameAsBefore And boolSameAsAfter Then
        Result := Result + GF.Parameters[iParam].Identifier
      Else If boolSameAsBefore And Not boolSameAsAfter Then
        Begin
          Result := Result + GF.Parameters[iParam].Identifier;
          If GF.Parameters[iParam].ParamType <> Nil Then
            Result := Result + ' : ' + GF.Parameters[iParam].ParamType.AsString(False,
              boolForDocumentation);
        End Else
          Result := Result + GF.Parameters[iParam].AsString(boolShowIdentifier,
            boolForDocumentation);
      If iParam < GF.ParameterCount - 1 Then
        Begin
          If boolForDocumentation Then
            Result := Result + strIsSame[boolSameAsAfter] + #13#10
          Else
            Result := Result + strIsSame[boolSameAsAfter] + ' ';
        End;
    End;
End;

(**

  This function creates a TTypeToken negating the need for a temporary
  variable.

  @precon  None.
  @postcon Creates a TTypeToken negating the need for a temporary variable.

  @param   AToken    as a TTokenInfo
  @param   AScope    as a TScope
  @param   AComment  as a TComment
  @param   Container as a TElementContainer
  @return  a TTypeToken

**)
Function TypeToken(AToken : TTokenInfo; AScope : TScope; AComment : TComment;
  Container : TElementContainer) : TTypeToken;

Begin
  If AToken <> Nil Then
    Begin
      Result.FIdentifier := AToken.Token;
      Result.FLine := AToken.Line;
      Result.FColumn := AToken.Column;
    End Else
    Begin
      Result.FIdentifier := '';
      Result.FLine := 0;
      Result.FColumn := 0;
    End;
  Result.FScope := AScope;
  Result.FComment := AComment;
  Result.FContainer := Container;
  //Assert(Container <> Nil, 'Container in TTypeToken is NULL!');
End;

End.
