(**

  This module contains a class to represent an Eidolon Output Rates Table.

  @Author  David Hoyle
  @Version 1.0
  @Date    15 Oct 2017

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
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

(**

  This method returns a string representation of the class.

  @precon  None.
  @postcon Returns a string representation of the class

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TOutputTable.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;

Const
  strTableType = '=Class(OutputTable)';

begin
  Result := Identifier + strTableType;
end;

End.
