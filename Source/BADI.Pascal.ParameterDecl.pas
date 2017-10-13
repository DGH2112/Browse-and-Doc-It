(**

  This module contains a class which implements an Object Pascal specific Parameter Declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    13 Oct 2017

**)
Unit BADI.Pascal.ParameterDecl;

Interface

Uses
  BADI.Generic.Parameter;

Type
  (** A class to represent a Object Pascal Parameter. **)
  TPascalParameter = Class(TGenericParameter)
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

uses
  BADI.Pascal.Constants;

(**

  This is a getter method for the AsString property.

  @precon  None.
  @postcon Outputs the parameter information in the style of object pascal code.

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TPascalParameter.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;

Const
  strEquals = #32'='#32;
  strColon = #32':'#32;

begin
  Result := '';
  If boolShowIdentifier Then
    Result := Result + strParamModifier[ParamModifier] + Identifier;
  If ParamType <> Nil Then
    Begin
      If boolShowIdentifier Then
        Result := Result + strColon;
      Result := Result + strArrayOf[ArrayOf];
      Result := Result + ParamType.AsString(False, boolForDocumentation);
    End;
  If DefaultValue <> '' Then
    Result := Result + strEquals + DefaultValue;
end;

End.
