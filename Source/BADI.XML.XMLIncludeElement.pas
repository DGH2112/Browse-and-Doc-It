(**

  Tihs module contains a class to represent an XML Include Element.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Oct 2017

**)
Unit BADI.XML.XMLIncludeElement;

Interface

Uses
  BADI.XML.BaseElement;

Type
  (** This class represents the individual xml PERef declarations in the document. **)
  TXMLIncludeElement = Class(TXMLBaseElement)
  Public
    Function AsString(Const boolShowIdenifier, boolForDocumentation: Boolean): String; Override;
  End;

Implementation

Uses
  BADI.Options;

(**

  This method returns a string presentation of the XML PE Ref element.

  @precon  None.
  @postcon Returns a string presentation of the XML PE Ref element.

  @param   boolShowIdenifier    as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TXMLIncludeElement.AsString(Const boolShowIdenifier, boolForDocumentation: Boolean): String;

Begin
  Result := BuildStringRepresentation(True, False, '', BADIOptions.MaxDocOutputWidth, [']'], ['[']);
End;

End.
