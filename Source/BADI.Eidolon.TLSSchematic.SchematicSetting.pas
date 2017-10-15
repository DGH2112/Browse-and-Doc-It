(**

  This module contains a class to represent a schematic setting.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Oct 2017

**)
Unit BADI.Eidolon.TLSSchematic.SchematicSetting;

Interface

Uses
  BADI.ElementContainer;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A class to represent a schematic setting. **)
  TSchematicSetting = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FPercentage : Double;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property gets ans sets the percentage of the settings.
      @precon  None.
      @postcon Gets ans sets the percentage of the settings.
      @return  an Double
    **)
    Property Percentage : Double Read FPercentage Write FPercentage;
  End;

Implementation

Uses
  SysUtils;

(**

  This method returns a string representation of a schematic setting.

  @precon  None.
  @postcon Returns a string representation of a schematic setting.

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TSchematicSetting.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;

begin
  Result := Format('%s %1.1f%%', [Identifier, FPercentage]);
end;

End.
