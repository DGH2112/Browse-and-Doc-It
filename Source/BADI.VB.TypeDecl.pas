(**

  This module contains a class to represent a general type declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    01 Apr 2017

**)
Unit BADI.VB.TypeDecl;

Interface

{$INCLUDE CompilerDefinitions.Inc}

Uses
  BADI.Generic.TypeDecl;

Type
  (** A class to represent types in visual basic. **)
  TVBTypeDecl = Class(TGenericTypeDecl)
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

Uses
  BADI.Options;

(**

  This method returns a string representation of the visual basic return type.

  @precon  None .
  @postcon Returns a string representation of the visual basic return type .

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TVBTypeDecl.AsString(boolShowIdentifier, boolForDocumentation: Boolean): String;
Begin
  Result := BuildStringRepresentation(boolShowIdentifier, boolForDocumentation, '',
    BADIOptions.MaxDocOutputWidth);
End;


End.
