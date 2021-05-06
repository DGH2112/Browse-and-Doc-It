(**

  This module contains a class to represent each compiler definition to be placed on the stack.

  @Author  David Hoyle
  @Version 1.203
  @Date    06 May 2021

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2021  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

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
Unit BADI.CompilerConditionData;

Interface

uses
  BADI.Types,
  BADI.Interfaces;

Type
  (** This class represents a single stack entry on the Compiler Condition stacks. **)
  TCompilerConditionData = Class(TInterfacedObject, IBADICompilerConditionData)
  Strict Private
    FCompilerDefType : TCompilerDefType;
    FCompilerCondition: TCompilerCondition;
    FTokenIndex: TTokenIndex;
  Strict Protected
    Function  GetCompilerDefType : TCompilerDefType;
    Function  GetCompilerCondition : TCompilerCondition;
    Function  GetTokenIndex : TTokenIndex;
  Public
    Constructor Create(Const iCompilerDefType : TCompilerDefType;
      Const iCompilerCondition: TCompilerCondition; Const iTokenIndex: TTokenIndex);
  End;

Implementation

(**

  A constructor for the TCompilerConditionData class.

  @precon  None.
  @postcon Initialises the class with data.

  @param   iCompilerDefType   as a TCompilerDefType as a constant
  @param   iCompilerCondition as a TCompilerCondition as a constant
  @param   iTokenIndex        as a TTokenIndex as a constant

**)
Constructor TCompilerConditionData.Create(Const iCompilerDefType : TCompilerDefType;
  Const iCompilerCondition: TCompilerCondition; Const iTokenIndex: TTokenIndex);

Begin
  FCompilerDefType := iCompilerDefType;
  FCompilerCondition := iCompilerCondition;
  FTokenIndex := iTokenIndex;
End;

(**

  This is a getter method for the Compiler Condition property.

  @precon  None.
  @postcon Returns the compiler condition.

  @return  a TCompilerCondition

**)
Function TCompilerConditionData.GetCompilerCondition: TCompilerCondition;

Begin
  Result := FCompilerCondition;
End;

(**

  This is a getter method for the Compiler Definition Type property.

  @precon  None.
  @postcon Returns the compiler definition type.

  @return  a TCompilerDefType

**)
Function TCompilerConditionData.GetCompilerDefType: TCompilerDefType;

Begin
  Result := FCompilerDefType;
End;

(**

  This is a getter method for the Token Index property.

  @precon  None.
  @postcon Returns the Token index.

  @return  a TTokenIndex

**)
Function TCompilerConditionData.GetTokenIndex: TTokenIndex;

Begin
  Result := FTokenIndex;
End;

End.
