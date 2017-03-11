(**

  This module contains a class to represents an XML PI element.

  @Author  David Hoyle
  @Version 1.0
  @Date    11 Mar 2017

**)
Unit BADI.XML.XMLPI;

Interface

Uses
  BADI.XML.BaseElement;

Type
  (** This class represents the individual xml PI declarations in the document. **)
  TXMLPI = Class(TXMLBaseElement)
  Public
    Function AsString(boolShowIdenifier, boolForDocumentation : Boolean) : String;
      Override;
  End;

Implementation

Uses
  BADI.Options;

(**

  This method returns a string presentation of the XML PI element.

  @precon  None.
  @postcon Returns a string presentation of the XML PI element.

  @param   boolShowIdenifier    as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TXMLPI.AsString(boolShowIdenifier, boolForDocumentation: Boolean): String;
Begin
  Result := BuildStringRepresentation(boolShowIdenifier, boolForDocumentation, '',
    BrowseAndDocItOptions.MaxDocOutputWidth, [#32, '=', '?'], [#32, '=', '?'], []);
End;

End.
