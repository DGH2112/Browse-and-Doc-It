(**

  This module contains a class which implements an Object Pascal specific Property Specification
  Declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    24 Feb 2017

**)
Unit BADI.Pascal.PropertySpec;

Interface

Uses
  BADI.ElementContainer;

Type
  (** This class defines a property specifier. **)
  TPropertySpec = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

(**


  This is a getter method for the AsString property.

  @precon  None.
  @postcon Returns the property specifier, Name = key word, tokens = value.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TPropertySpec.AsString(boolShowIdentifier, boolForDocumentation: Boolean): String;

Var
  iToken: Integer;

Begin
  Result := '';
  If boolShowIdentifier Then
    Result := Result + Identifier;
  For iToken := 0 To TokenCount - 1 Do
    Begin
      If Result <> '' Then
        Result := Result + #32;
      Result := Result + Tokens[iToken].Token;
    End;
End;

End.
