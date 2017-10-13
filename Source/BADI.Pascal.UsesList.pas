(**

  This module contains a class to represent a pascal uses clause item.

  @Author  David Hoyle
  @Version 1.0
  @Date    13 Oct 2017

**)
Unit BADI.Pascal.UsesList;

Interface

Uses
  BADI.ElementContainer,
  BADI.TokenInfo;

Type
  (** This class represents a list of identifiers **)
  TUsesList = Class(TElementContainer)
  Strict Private
  Strict Protected
  Public
    Function AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String; Override;
  End;

Implementation

Uses
  BADI.Options;

(**

  This is a getter method for the AsString property.

  @precon  None .
  @postcon Returns a string representation of the class information .

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TUsesList.AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String;

Begin
  Result := BuildStringRepresentation(boolShowIdentifier, boolForDocumentation, '',
    BADIOptions.MaxDocOutputWidth)
End;

End.
