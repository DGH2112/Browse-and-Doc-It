(**

  This module contains a class which represents a VB parameter.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Oct 2017

**)
Unit BADI.VB.Parameter;

Interface

{$INCLUDE CompilerDefinitions.Inc}

Uses
  BADI.Generic.Parameter;

Type
  (** A class to present parameters in methods and properties. **)
  TVBParameter = Class(TGenericParameter)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FOptional : Boolean;
    FParamArray: Boolean;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property determines of the parameter is optional or not.
      @precon  None.
      @postcon Determines of the parameter is optional or not.
      @return  a Boolean
    **)
    Property Optional : Boolean Read FOptional Write FOptional;
    (**
      This property determines of the parameter is a parameter array.
      @precon  None.
      @postcon Determines of the parameter is a parameter array.
      @return  a Boolean
    **)
    Property ParamArray : Boolean Read FParamArray Write FParamArray;
  End;

Implementation

Uses
  BADI.Types;

(**

  This method returns a string representation of the visual basic parameter.

  @precon  None .
  @postcon Returns a string representation of the visual basic parameter .

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TVBParameter.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;
Begin
  Result := '';
  If Optional Then
    Result := Result + 'Optional';
  If ParamModifier In [pamVar, pamConst] Then
    If Result <> '' Then
      Result := Result + #32;
  Case ParamModifier Of
    pamVar: Result := Result + 'ByRef';
    pamConst: Result := Result + 'ByVal';
  End;
  If ParamArray Then
    Begin
      If Result <> '' Then
        Result := Result + #32;
      Result := Result + 'ParamArray';
    End;
  If Result <> '' Then
    Result := Result + #32;
  If boolShowIdentifier Then
    Result := Result + Identifier;
  If Result <> '' Then
    Result := Result + #32'As'#32;
  Result := Result + ParamType.AsString(boolShowIdentifier, boolForDocumentation);
  If DefaultValue <> '' Then
    Result := Result + #32 + '=' + #32 + DefaultValue;
End;

End.
