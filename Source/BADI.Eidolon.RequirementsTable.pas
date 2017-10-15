(**

  This module contains a class to represent an Eidolon Requirements Table.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Oct 2017

**)
Unit BADI.Eidolon.RequirementsTable;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  BADI.Eidolon.DBTable,
  BADI.ElementContainer,
  BADI.Eidolon.Association;

Type
  (** This is a class to represent a requirements Table. **)
  TRequirementsTable = Class(TDBTable)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FAssociations: TLabelContainer;
  Public
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    Function AddAssociation(Association : TAssociation) : TAssociation;
  End;

Implementation

Uses
  BADI.Types;

(**

  This method adds an association to the requirements table.

  @precon  Association must be a valid instance of a TAssocaition.
  @postcon Adds an association to the requirements table.

  @param   Association as a TAssociation
  @return  a TAssociation

**)
function TRequirementsTable.AddAssociation(
  Association: TAssociation): TAssociation;
begin
  If FAssociations = Nil Then
    FAssociations := AddUnique(TLabelContainer.Create('Associations', scNone, 0, 0,
      iiClassesLabel, Nil)) As TLabelContainer;
  Result := FAssociations.AddUnique(Association) As TAssociation;
end;

(**

  This method returns a string representation of the class.

  @precon  None.
  @postcon Returns a string representation of the class

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TRequirementsTable.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;
begin
  Result := Identifier + '=Class(RequirementsTable)';
end;

End.
