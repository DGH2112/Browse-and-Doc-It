(**

  This module contains a class to represent an XML Element Declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    11 Mar 2017

**)
Unit BADI.XML.XMLElemDecl;

Interface

Uses
  BADI.XML.BaseElement;

Type
  (** This class represents the individual elements declarations in the document. **)
  TXMLElemDecl = Class(TXMLBaseElement)
  Public
    Function AsString(boolShowIdenifier, boolForDocumentation : Boolean) : String;
      Override;
  End;

Implementation

Uses
  BADI.Options;

(**

  This method returns a string representation of the XML Element Declaration.

  @precon  None.
  @postcon Returns a string representation of the XML Element Declaration.

  @param   boolShowIdenifier    as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TXMLElemDecl.AsString(boolShowIdenifier, boolForDocumentation: Boolean): String;

Begin
  Result := BuildStringRepresentation(boolShowIdenifier, boolForDocumentation, '',
    BrowseAndDocItOptions.MaxDocOutputWidth, [')', '*', '?', '+', ','], ['(', '+', '*', '>'], []);
End;

End.
