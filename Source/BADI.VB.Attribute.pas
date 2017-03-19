(**

  This module contains a class to represent a VB attribute.

  @Author  David Hoyle
  @Version 1.0
  @Date    19 Mar 2017

**)
Unit BADI.VB.Attribute;

Interface

{$INCLUDE CompilerDefinitions.Inc}

Uses
  BADI.ElementContainer;

Type
  (** A class to represent attributes **)
  TVBAttribute = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetName : String; Override;
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

Uses
  BADI.Options;

(**

  This method returns a string representation of the visual basic attribute.

  @precon  None .
  @postcon Returns a string representation of the visual basic attribute .

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TVBAttribute.AsString(boolShowIdentifier, boolForDocumentation: Boolean): String;
Begin
  Result := BuildStringRepresentation(boolShowIdentifier, boolForDocumentation,
    '', BrowseAndDocItOptions.MaxDocOutputWidth);
End;

(**

  This is a getter method for the Name property.

  @precon  None.
  @postcon Returns an attribute name based on all tokens in the container.

  @return  a String

**)
Function TVBAttribute.GetName: String;
Begin
  Result := BuildStringRepresentation(True, False, '', 9999);
End;

End.
