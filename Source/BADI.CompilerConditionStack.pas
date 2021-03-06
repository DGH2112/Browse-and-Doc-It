(**

  This module contains a class to handle compiler definiton information as a stack.

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
Unit BADI.CompilerConditionStack;

Interface

Uses
  Classes,
  Contnrs,
  BADI.Types,
  BADI.CompilerConditionData;

Type
  (** A type to handle the stack of compiler condition visibilities. **)
  TCompilerConditionStack = Class
    {$IFDEF D2005} Strict {$ENDIF} Private
    FStack: TObjectList;
    {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Push(Const iCompilerDefType : TCompilerDefType;
      Const iCompilerCondition: TCompilerCondition; Const iTokenIndex: TTokenIndex); Overload;
    Procedure Push(Const CompilerConditionData: TCompilerConditionData); Overload;
    Procedure Pop();
    Function Peek: TCompilerConditionData;
    Function CanPop: Boolean;
  End;

Implementation

uses
  BADI.ResourceStrings;

(**

  This method determines whether there is anything to pop from the stack.

  @precon  None.
  @postcon Returns true of there is anything on the stack to pop.

  @return  a Boolean

**)
Function TCompilerConditionStack.CanPop: Boolean;

Begin
  Result := FStack.Count > 0;
End;

(**

  A constructor for the TCompilerConditionStack class.

  @precon  None.
  @postcon The class is initialised.

**)
Constructor TCompilerConditionStack.Create;

Begin
  FStack := TObjectList.Create(True);
End;

(**

  A destructor for the TCompilerCondition class.

  @precon  None.
  @postcon Frees the memory used by the class.

**)
Destructor TCompilerConditionStack.Destroy;

Begin
  FStack.Free;
  Inherited Destroy;
End;

(**

  This method allow the caller to see the value on the top of the stack.

  @precon  None.
  @postcon Returns the value on the top of the stack.

  @return  a TCompilerConditionData

**)
Function TCompilerConditionStack.Peek: TCompilerConditionData;

Begin
  If FStack.Count > 0 Then
    Result := FStack[Pred(FStack.Count)] As TCompilerConditionData
  Else
    Raise EBADIParserError.Create(strCannotPeekTheCompilerCondition);
End;

(**

  This method removes the last item from the top of the stack.

  @precon  None.
  @postcon The value on the top of the stack is removed.

**)
Procedure TCompilerConditionStack.Pop;

Var
  CDT: TCompilerDefType;

Begin
  If FStack.Count > 0 Then
    Begin
      CDT := Peek.CompilerDefType;
      FStack.Delete(Pred(FStack.Count));
      If CDT = cdtELSE Then
        Begin
          CDT := Peek.CompilerDefType;
          If Not (CDT In [cdtIFDEF, cdtIFNDEF]) Then
            Raise EBADIParserError.Create(strCannotPopCompilerCondition);
          FStack.Delete(Pred(FStack.Count));
        End;
    End Else
      Raise EBADIParserError.Create(strCannotPopCompilerCondition);
End;

(**

  This method pushes the given compiler condition data on to the top of the stack.

  @precon  CompilerConditionData must be a valdi instance.
  @postcon The compiler condition data is placed on top of the stack.

  @param   CompilerConditionData as a TCompilerConditionData as a constant

**)
Procedure TCompilerConditionStack.Push(Const CompilerConditionData: TCompilerConditionData);

Begin
  Push(CompilerConditionData.CompilerDefType, CompilerConditionData.CompilerCondition,
    CompilerConditionData.TokenIndex);
End;

(**

  This method adds the given valud to the top of the stack.

  @precon  None.
  @postcon Adds the given valud to the top of the stack.

  @param   iCompilerDefType   as a TCompilerDefType as a constant
  @param   iCompilerCondition as a TCompilerCondition as a constant
  @param   iTokenIndex        as a TTokenIndex as a constant

**)
Procedure TCompilerConditionStack.Push(Const iCompilerDefType : TCompilerDefType;
  Const iCompilerCondition: TCompilerCondition; Const iTokenIndex: TTokenIndex);

Begin
  FStack.Add(TCompilerConditionData.Create(iCompilerDefType, iCompilerCondition, iTokenIndex));
End;

End.
