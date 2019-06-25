(**

  This module contains a class to represent each compiler definition to be placed on the stack.

  @Author  David Hoyle
  @Version 1.0
  @Date    21 Jun 2019

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2019  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

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

Uses
  BADI.Types;

Type
  (** This class represents a single stack entry on the CompilerCondition stacks. **)
  TCompilerConditionData = Class
    {$IFDEF D2005} Strict {$ENDIF} Private
    FCompilerDefType : TCompilerDefType;
    FCompilerCondition: TCompilerCondition;
    FTokenIndex: TTokenIndex;
    {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Constructor Create(Const iCompilerDefType : TCompilerDefType;
      Const iCompilerCondition: TCompilerCondition; Const iTokenIndex: TTokenIndex);
    (**
      This property provide access to the compiler definition type of the class.
      @precon  None.
      @postcon Returns the compiler definiton type of the stack item.
      @return  a TCompilerDefType
    **)
    Property CompilerDefType : TCompilerDefType Read FCompilerDefType;
    (**
      This property gets and sets the compiler condition (include code or exclude code).
      @precon  None.
      @postcon Gets and sets the compiler condition (include code or exclude code).
      @return  a TCompilerCondition
    **)
    Property CompilerCondition: TCompilerCondition Read FCompilerCondition;
    (**
      This property gets or sets the token index of the compiler condition.
      @precon  None.
      @postcon Gets or sets the token index of the compiler condition.
      @return  a TTokenIndex
    **)
    Property TokenIndex: TTokenIndex Read FTokenIndex;
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

End.
