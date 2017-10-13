(**

  This module contains a class to represent an implemented item.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Oct 2017

**)
Unit BADI.VB.ImplementedItem;

Interface

{$INCLUDE CompilerDefinitions.Inc}

Uses
  BADI.ElementContainer;

Type
  (** A class to represent an Implements item. **)
  TImplementedItem = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

(**

  This method returns a string representation of the Implements item.

  @precon  None .
  @postcon Returns a string representation of the imlpements item.

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TImplementedItem.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;
Begin
  Result := Identifier;
End;

End.
