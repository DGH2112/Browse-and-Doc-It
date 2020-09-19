(**

  This module contains a class which represents a VB parameter.

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
Unit BADI.VB.Parameter;

Interface

{$INCLUDE CompilerDefinitions.Inc}

Uses
  BADI.Generic.Parameter;

Type
  (** A class to present parameters in methods and properties. **)
  TVBParameter = Class(TGenericParameter)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FOptional : Boolean;
    FParamArray: Boolean;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property determines of the parameter is optional or not.
      @precon  None.
      @postcon Determines of the parameter is optional or not.
      @return  a Boolean
    **)
    Property Optional : Boolean Read FOptional Write FOptional;
    (**
      This property determines of the parameter is a parameter array.
      @precon  None.
      @postcon Determines of the parameter is a parameter array.
      @return  a Boolean
    **)
    Property ParamArray : Boolean Read FParamArray Write FParamArray;
  End;

Implementation

Uses
  BADI.Types;

(**

  This method returns a string representation of the visual basic parameter.

  @precon  None .
  @postcon Returns a string representation of the visual basic parameter .

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TVBParameter.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;
Begin
  Result := '';
  If Optional Then
    Result := Result + 'Optional';
  If ParamModifier In [pamVar, pamConst] Then
    If Result <> '' Then
      Result := Result + #32;
  Case ParamModifier Of
    pamVar: Result := Result + 'ByRef';
    pamConst: Result := Result + 'ByVal';
  End;
  If ParamArray Then
    Begin
      If Result <> '' Then
        Result := Result + #32;
      Result := Result + 'ParamArray';
    End;
  If Result <> '' Then
    Result := Result + #32;
  If boolShowIdentifier Then
    Result := Result + Identifier;
  If Result <> '' Then
    Result := Result + #32'As'#32;
  Result := Result + ParamType.AsString(boolShowIdentifier, boolForDocumentation);
  If DefaultValue <> '' Then
    Result := Result + #32 + '=' + #32 + DefaultValue;
End;

End.
