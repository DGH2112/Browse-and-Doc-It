(**

  This module contains a class to represent an Eidolon Output Rates Table.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Mar 2017

**)
Unit BADI.Eidolon.OutputTable;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  BADI.Eidolon.DBTable;

Type
  (** This is a class to represent an Output Table. **)
  TOutputTable = Class(TDBTable)
  {$IFDEF D2005} Strict {$ENDIF} Private
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

(**

  This method returns a string representation of the class.

  @precon  None.
  @postcon Returns a string representation of the class

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TOutputTable.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;
begin
  Result := Identifier + '=Class(OutputTable)';
end;

End.
