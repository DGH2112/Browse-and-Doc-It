(**

  This module contains a class which implements an Object Pascal specific variable Declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Oct 2017

**)
Unit BADI.Pascal.VariableDecl;

Interface

Uses
  BADI.Generic.Variable;

Type
  (** This is a sub class for all variables. **)
  TVar = Class(TGenericVariable)
    {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String; Override;
  End;

Implementation

Uses
  BADI.Types,
  BADI.Options,
  BADI.ResourceStrings,
  BADI.Constants;

(**

  This is a getter method for the AsString property.

  @precon  None.
  @postcon Returns the variable declaration.

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TVar.AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String;

Begin
  Result := BuildStringRepresentation(boolShowIdentifier, boolForDocumentation, ':',
    BADIOptions.MaxDocOutputWidth);
End;

End.
