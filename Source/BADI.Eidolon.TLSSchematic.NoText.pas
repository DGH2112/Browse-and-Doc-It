(**

  This module contains a class to represent the absense of text.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Oct 2017

**)
Unit BADI.Eidolon.TLSSchematic.NoText;

Interface

Uses
  BADI.ElementContainer;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A class to represent a schematic setting. **)
  TNoText = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

(**

  This method returns a string representation of a schematic object.

  @precon  None.
  @postcon Returns a string representation of a schematic object.

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TNoText.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;
begin
  Result := Identifier;
end;

End.
