(**

  This modlue contains a class to represent an XML Ignore element.

  @Author  David Hoyle
  @Version 1.0
  @Date    01 Apr 2017

**)
Unit BADI.XML.XMLIgnoreElement;

Interface

Uses
  BADI.XML.BaseElement;

Type
  (** This class represents the individual xml PERef declarations in the document. **)
  TXMLIgnoreElement = Class(TXMLBaseElement)
  Public
    Function AsString(boolShowIdenifier, boolForDocumentation: Boolean): String; Override;
  End;

Implementation

Uses
  BADI.Options;

(**

  This method returns a string presentation of the XML PE Ref element.

  @precon  None.
  @postcon Returns a string presentation of the XML PE Ref element.

  @param   boolShowIdenifier    as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TXMLIgnoreElement.AsString(boolShowIdenifier, boolForDocumentation: Boolean): String;
Begin
  Result := BuildStringRepresentation(True, False, '', BADIOptions.MaxDocOutputWidth, [']'], ['[']);
End;

End.
