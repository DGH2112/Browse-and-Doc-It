(**

  This module contains a class to represent an Eidolon Text Table.

  @Author  David Hoyle
  @Version 1.0
  @Date    06 May 2017

**)
Unit BADI.Eidolon.TextTable;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  BADI.Eidolon.BaseTable;

Type
  (** A class to represent a TextTable definition. **)
  TTextTable = Class(TBaseTable)
  Strict Private
    FFileName : String;
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This method gets and sets the filename of the text table.
      @precon  None.
      @postcon Gets or sets the filename of the text table.
      @return  a String
    **)
    Property FileName : String Read FFileName Write FFileName;
  End;

Implementation

{ TTextTable }

(**

  This method returns string representation of the text table definition.

  @precon  None.
  @postcon Returns string representation of the text table definition.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TTextTable.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;

begin
  Result := Identifier + '=Class(TextTable)';
end;

End.
