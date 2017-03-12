(**

  This modulel contains a class to represent an XML Doc Type element.

  @Author  David Hoyle
  @Version 1.0
  @Date   12 Mar 2017

**)
Unit BADI.XML.DocType;

Interface

Uses
  BADI.XML.BaseElement;

Type
  (** This class represents the documents doc type. **)
  TXMLDocType = Class(TXMLBaseElement)
  Public
    Function AsString(boolShowIdenifier, boolForDocumentation : Boolean) : String;
      Override;
  End;

Implementation

Uses
  BADI.Options;

(**

  This method returns a string representation of the XML Doc Type.

  @precon  None.
  @postcon Returns a string representation of the XML Doc Type.

  @param   boolShowIdenifier    as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TXMLDocType.AsString(boolShowIdenifier, boolForDocumentation: Boolean): String;

Begin
  Result := BuildStringRepresentation(True, False, '', BrowseAndDocItOptions.MaxDocOutputWidth,
    [']', '>'], ['[', '<']);
End;

End.
