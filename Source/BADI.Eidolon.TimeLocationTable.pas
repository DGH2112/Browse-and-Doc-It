(**

  This module contains a class to represent an Eidolon Time Location Table.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Mar 2017

**)
Unit BADI.Eidolon.TimeLocationTable;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  BADI.Eidolon.BaseTable,
  BADI.ElementContainer,
  BADI.Eidolon.Symbol;

Type
  (** A class to represent a TimeLocationTable definition. **)
  TTimeLocationTable = Class(TBaseTable)
    FSymbols: TLabelContainer;
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    Function AddSymbol(Symbol : TSymbol) : TSymbol;
  End;

Implementation

Uses
  BADI.Eidolon.ResourceStrings,
  BADI.Types;

(**

  This method adds the given Symbol to the time location table.

  @precon  Symbol must be a valid instance of a TSymbol class.
  @postcon A

  @param   Symbol as a TSymbol
  @return  a TSymbol

**)
function TTimeLocationTable.AddSymbol(Symbol: TSymbol): TSymbol;


begin
  If FSymbols = Nil Then
    FSymbols := AddUnique(TLabelContainer.Create(strSymbolsLabel, scNone, 0, 0,
      iiObjectsLabel, Nil)) As TLabelContainer;
  Result := FSymbols.AddUnique(Symbol) As TSymbol;
end;

(**

  This method returns string representation of the time location table definition.

  @precon  None.
  @postcon Returns string representation of the time location table definition.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
function TTimeLocationTable.AsString(boolShowIdentifier,
  boolForDocumentation: Boolean): String;

begin
  Result := Identifier + '=Class(TimeLocationTable)';
end;

End.
