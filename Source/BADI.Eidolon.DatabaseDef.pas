(**

  This module contains a class to represent an Eidolon Database Definition.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Oct 2017

**)
Unit BADI.Eidolon.DatabaseDef;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  BADI.Eidolon.DBConnection;

Type
  (** A class to represent a Database definition. **)
  TDatabaseDef = Class(TDBConnection)
  Public
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

Uses
  BADI.Eidolon.Functions;

(**

  This method returns string representation of the Database Definition.

  @precon  None.
  @postcon Returns string representation of the Database Definition.

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TDatabaseDef.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;

begin
  Result := '';
  If boolShowIdentifier Then
    Result := Result + Identifier + '=';
  Result:= Result + BuildLiteralString(Self);
end;

End.
