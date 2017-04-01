(**

  This module contains a class to represent a key / value pair in the INI file.

  @Author  David Hoyle
  @Version 1.0
  @Date    01 Apr 2017

**)
Unit BADI.INI.KeyValuePair;

Interface

Uses
  BADI.ElementContainer;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This is a class to represent the Key and Value pairs. **)
  TKeyValuePair = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function  GetName : String; Override;
  Public
    Function  AsString(boolShowIdenifier, boolForDocumentation : Boolean) : String;
      Override;
  End;

Implementation

Uses
  SysUtils,
  BADI.Options;

(**

  This method returns a string representation of the key value pair.

  @precon  None.
  @postcon Returns a string representation of the key value pair.

  @param   boolShowIdenifier    as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TKeyValuePair.AsString(boolShowIdenifier, boolForDocumentation: Boolean): String;

Begin
  Result := BuildStringRepresentation(True, False, '=',
    BADIOptions.MaxDocOutputWidth, [#32..#255], [#32..#255], []);
End;

(**

  This is a getter method for the Name property.

  @precon  None.
  @postcon Returns the internal name of the element.

  @return  a String

**)
Function TKeyValuePair.GetName: String;

Begin
  Result := Format('%s%4.4d', [Identifier, Random(9999)]);
End;

End.
