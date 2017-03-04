Unit BADI.Pascal.UsesList;

Interface

Uses
  BADI.ElementContainer,
  BADI.TokenInfo;

Type
  (** This class represents a list of identifiers **)
  TUsesList = Class(TElementContainer)
  Strict Private
  Strict Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation: Boolean): String; Override;
    Procedure CheckReferences; Override;
  End;

Implementation

Uses
  BADI.Options;

(**

  This is a getter method for the AsString property.

  @precon  None .
  @postcon Returns a string representation of the class information .

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TUsesList.AsString(boolShowIdentifier, boolForDocumentation: Boolean): String;

Begin
  Result := BuildStringRepresentation(boolShowIdentifier, boolForDocumentation, '',
    BrowseAndDocItOptions.MaxDocOutputWidth)
End;

Procedure TUsesList.CheckReferences;

Begin
End;

End.
