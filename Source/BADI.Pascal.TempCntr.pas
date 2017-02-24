(**

  This module contains a class which implements an Object Pascal specific temporary container.

  @Author  David Hoyle
  @Version 1.0
  @Date    24 Feb 2017

**)
Unit BADI.Pascal.TempCntr;

Interface

Uses
  BADI.ElementContainer;

Type
  (** This class represents a temporary list / collection **)
  TTempCntr = Class(TElementContainer)
    {$IFDEF D2005} Strict {$ENDIF} Private
    {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation: Boolean): String; Override;
  End;

Implementation

uses
  BADI.Types, BADI.Pascal.ResourceStrings;

(**

  This is a getter method for the AsString property.

  @precon  None .
  @postcon Returns a string representation of the class information .

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TTempCntr.AsString(boolShowIdentifier, boolForDocumentation: Boolean): String;

Begin
  Result := '';
  Raise EBADIParserError.Create(strTriedToRenderTmpCntr);
End;

End.
