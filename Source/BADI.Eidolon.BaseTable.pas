(**

  This module contains a class to represent Base Table for Eidolon Table Data.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Mar 2017

**)
Unit BADI.Eidolon.BaseTable;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  BADI.ElementContainer,
  BADI.Eidolon.FieldDef;

Type
  (** A base class from which all the tables are derived. **)
  TBaseTable = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FFields: TLabelContainer;
  Public
    Function  AddField(Field : TFieldDef) : TFieldDef;
  End;

Implementation

Uses
  BADI.Types;

(**

  This method adds fields to the table definition.

  @precon  Field must be a valid instance of a TFieldDef class.
  @postcon Adds fields to the table definition.

  @param   Field as a TFieldDef
  @return  a TFieldDef

**)
function TBaseTable.AddField(Field: TFieldDef): TFieldDef;
begin
  If FFields = Nil Then
    FFields := Addunique(TLabelContainer.Create('Fields', scNone, 0, 0, iiFieldsLabel,
      Nil)) As TLabelContainer;
  Result := FFields.AddUnique(Field) As TFieldDef;
end;

End.
