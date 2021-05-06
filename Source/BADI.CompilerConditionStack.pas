(**

  This module contains a class to handle compiler definition information as a stack.

  @Author  David Hoyle
  @Version 1.477
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
Unit BADI.CompilerConditionStack;

Interface

uses
  System.Classes,
  System.Contnrs,
  BADI.Types,
  BADI.CompilerConditionData,
  BADI.Interfaces;

Type
  (** A type to handle the stack of compiler condition visibilities. **)
  TCompilerConditionStack = Class(TInterfacedObject, IBADICompilerConditionStack)
  Strict Private
    FStack: TInterfaceList;
  Strict Protected
    Procedure Push(Const iCompilerDefType : TCompilerDefType;
      Const iCompilerCondition: TCompilerCondition; Const iTokenIndex: TTokenIndex); Overload;
    Procedure Push(Const CompilerConditionData: IBADICompilerConditionData); Overload;
    Function  Pop() : IBADICompilerConditionData;
    Function  Peek() : IBADICompilerConditionData;
    Function  CanPop() : Boolean;
    Function  CanParse() : Boolean;
  Public
    Constructor Create;
    Destructor Destroy; Override;
  End;

Implementation

uses
  System.TypInfo,
  BADI.ResourceStrings,
  CodeSiteLogging;

(**

  This method returns true if the top of the compiler definition stack has a define that allows parsing.

  @precon  None.
  @postcon Returns true if the top of the compiler definition stack has a define that allows parsing.

  @return  a Boolean

**)
Function TCompilerConditionStack.CanParse: Boolean;

Var
  iDef: Integer;

Begin
  Result := True;
  For iDef := FStack.Count - 1 DownTo 0 Do
    Result := Result And ((FStack[iDef] As IBADICompilerConditionData).CompilerCondition = ccIncludeCode);
End;

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
  FStack := TInterfaceList.Create();
End;

(**

  A destructor for the TCompilerConditionStack class.

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

  @return  a IBADICompilerConditionData

**)
Function TCompilerConditionStack.Peek: IBADICompilerConditionData;

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

  @return  an IBADICompilerConditionData

**)
Function TCompilerConditionStack.Pop() : IBADICompilerConditionData;

Begin
  If FStack.Count > 0 Then
    Begin
      Result := Peek;
      FStack.Delete(Pred(FStack.Count));
    End Else
      Raise EBADIParserError.Create(strCannotPopCompilerCondition);
End;

(**

  This method pushes the given compiler condition data on to the top of the stack.

  @precon  CompilerConditionData must be a valid instance.
  @postcon The compiler condition data is placed on top of the stack.

  @param   CompilerConditionData as a iBADICompilerConditionData as a constant

**)
Procedure TCompilerConditionStack.Push(Const CompilerConditionData: IBADICompilerConditionData);

Begin
  Push(CompilerConditionData.CompilerDefType, CompilerConditionData.CompilerCondition,
    CompilerConditionData.TokenIndex);
End;

(**

  This method adds the given value to the top of the stack.

  @precon  None.
  @postcon Adds the given value to the top of the stack.

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
