(**

  This module contains a class that represents a VB Option.

  @Author  David Hoyle
  @Version 1.0
  @Date    01 Apr 2017

**)
Unit BADI.VB.Option;

Interface

{$INCLUDE CompilerDefinitions.Inc}

Uses
  BADI.ElementContainer;

Type
  (** A class to represent options. **)
  TVBOption = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

Uses
  BADI.Options;

(**

  This method returns a string representation of the visual basic option.

  @precon  None .
  @postcon Returns a string representation of the visual basic option .

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TVBOption.AsString(boolShowIdentifier, boolForDocumentation: Boolean): String;

Begin
  Result := BuildStringRepresentation(False, boolForDocumentation,
    '', BADIOptions.MaxDocOutputWidth);
  If boolShowIdentifier Then
    If Result <> '' Then
      Result := Identifier + #32 + Result
    Else
      Result := Identifier;
End;

End.
