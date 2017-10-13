(**

  This module contains a class to represent a property in a DFM file.

  @Version 1.0
  @Author  David Hoyle
  @date    12 Oct 2017

**)
Unit BADI.DFM.PropertyDecl;

Interface

Uses
  BADI.ElementContainer;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This class represent a DFM property in the file. **)
  TDFMProperty = Class(TElementContainer)
    {$IFDEF D2005} Strict {$ENDIF} Private
    {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String; Override;
  End;

Implementation

Uses
  BADI.Options;

(**

  This method returns a string represetation of the DFM property.

  @precon  None.
  @postcon Returns a string represetation of the DFM property.

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TDFMProperty.AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String;

Begin
  Result := BuildStringRepresentation(True, boolForDocumentation, '=',
    BADIOptions.MaxDocOutputWidth,
    ['(', '[', '{', ')', ']', '}', ';', ',', '.', '!', '?', '<', '>'],
    ['(', '[', '{', '.', '^', '-'],
    ['=', ':', '+', '*', '\'])
End;

End.
