(**

  This module contains a class to represent a key / value pair in the INI file.

  @Author  David Hoyle
  @Version 1.001
  @Date    19 Sep 2020

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2020  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
    Function  AsString(Const boolShowIdenifier, boolForDocumentation : Boolean) : String;
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

  @param   boolShowIdenifier    as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TKeyValuePair.AsString(Const boolShowIdenifier, boolForDocumentation : Boolean) : String;

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
