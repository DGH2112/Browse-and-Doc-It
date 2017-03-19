(**

  This module contains a class to represents an enumerate identifier.

  @Author  David Hoyle
  @Version 1.0
  @Date    19 Mar 2017

**)
Unit BADI.VB.EnumIdent;

Interface

{$INCLUDE CompilerDefinitions.Inc}

Uses
  BADI.ElementContainer;

Type
  (** A class to represent VB Enumerate Value  **)
  TVBEnumIdent = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

Uses
  BADI.Options;

(**

  This method returns a string representation of the visual basic enumerate
  identifier.

  @precon  None .
  @postcon Returns a string representation of the visual basic enumerate
           identifier .

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TVBEnumIdent.AsString(boolShowIdentifier, boolForDocumentation: Boolean): String;
Begin
  Result := BuildStringRepresentation(boolShowIdentifier, boolForDocumentation, '=',
    BrowseAndDocItOptions.MaxDocOutputWidth);
End;

End.
