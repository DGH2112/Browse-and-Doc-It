(**

  This module contains a class to represent a VB record / structure declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    19 Mar 2017

**)
Unit BADI.VB.RecordDecl;

Interface

{$INCLUDE CompilerDefinitions.Inc}

Uses
  BADI.Generic.TypeDecl;

Type
  (** A class to represent records in visual basic. **)
  TVBRecordDecl = Class(TGenericTypeDecl)
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

(**

  This method returns a string representation of the visual basic record.

  @precon  None .
  @postcon Returns a string representation of the visual basic record .

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TVBRecordDecl.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;
Begin
  Result := 'Type';
  If boolShowIdentifier Then
    Result := Result + #32 + Identifier;
End;

End.
