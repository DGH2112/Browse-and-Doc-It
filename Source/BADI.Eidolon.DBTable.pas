(**

  This module contains a class to represent an Eidolon Database Table.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Oct 2017

**)
Unit BADI.Eidolon.DBTable;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  BADI.Eidolon.BaseTable,
  BADI.ElementContainer,
  BADI.Eidolon.DBConnection;

Type
  (** A class to represent a TextTable definition. **)
  TDBTable = Class(TBaseTable)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FPrimary: TLabelContainer;
    FSecondary: TLabelContainer;
  Public
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    Function AddPrimary(DBConnection : TDBConnection) : TDBConnection;
    Function AddSecondary(DBConnection : TDBConnection) : TDBConnection;
  End;

Implementation

Uses
  BADI.Eidolon.ResourceStrings,
  BADI.Types;

(**

  This method adds a database connection element to the Primary section of the
  DBTable element.

  @precon  DBConnection must be a valid instance of a TDBConnection class.
  @postcon Adds a database connection element to the Primary section of
           DBTable element.

  @param   DBConnection as a TDBConnection
  @return  a TDBConnection

**)
function TDBTable.AddPrimary(DBConnection: TDBConnection): TDBConnection;
begin
  If FPrimary = Nil Then
    FPrimary := AddUnique(TLabelContainer.Create(strPrimaryLabel, scNone, 0, 0,
      iiPublicTypesLabel, Nil)) As TLabelContainer;
  Result := FPrimary.AddUnique(DBConnection) As TDBConnection;
end;

(**

  This method adds a database connection element to the Secondary section of the
  DBTable element.

  @precon  DBConnection must be a valid instance of a TDBConnection class.
  @postcon Adds a database connection element to the Secondary section of
           DBTable element.

  @param   DBConnection as a TDBConnection
  @return  a TDBConnection

**)
function TDBTable.AddSecondary(DBConnection: TDBConnection): TDBConnection;
begin
  If FSecondary = Nil Then
    FSecondary := AddUnique(TLabelContainer.Create('Secondary', scNone, 0, 0,
      iiPublicTypesLabel, Nil)) As TLabelContainer;
  Result := FSecondary.AddUnique(DBConnection) As TDBConnection;
end;

(**

  This method returns string representation of the database table definition.

  @precon  None.
  @postcon Returns string representation of the database table definition.

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
function TDBTable.AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String;

begin
  Result := Identifier + '=Class(DBTable)';
end;

End.
