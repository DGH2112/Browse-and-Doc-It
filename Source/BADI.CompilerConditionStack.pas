(**

  This module contains a class to handle compiler definiton information as a stack.

  @Author  David Hoyle
  @Version 1.0
  @Date    18 Feb 2017

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
    Procedure Push(iCompilerCondition: TCompilerCondition; iTokenIndex: TTokenIndex); Overload;
    Procedure Push(CompilerConditionData: TCompilerConditionData); Overload;
    Procedure Pop();
    Function Peek: TCompilerConditionData;
    Procedure Poke(iCompilerCondition: TCompilerCondition; iTokenIndex: TTokenIndex);
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
    Raise EParserError.Create(strCannotPeekTheCompilerCondition);
End;

(**

  This method allows the caller to modify the top item on the stack.

  @precon  There must be an item on the stack top.
  @postcon The top item on the stack is changed.

  @param   iCompilerCondition as a TCompilerCondition
  @param   iTokenIndex        as a TTokenIndex

**)
Procedure TCompilerConditionStack.Poke(iCompilerCondition: TCompilerCondition;
  iTokenIndex: TTokenIndex);

Begin
  If FStack.Count > 0 Then
    Begin
      (FStack[Pred(FStack.Count)] As TCompilerConditionData).CompilerCondition :=
        iCompilerCondition;
      (FStack[Pred(FStack.Count)] As TCompilerConditionData).TokenIndex := iTokenIndex;
    End;
End;

(**

  This method removes the last item from the top of the stack.

  @precon  None.
  @postcon The value on the top of the stack is removed.

**)
Procedure TCompilerConditionStack.Pop;

Begin
  If FStack.Count > 0 Then
    FStack.Delete(Pred(FStack.Count))
  Else
    Raise EParserError.Create(strCannotPopCompilerCondition);
End;

(**

  This method pushes the given compiler condition data on to the top of the stack.

  @precon  CompilerConditionData must be a valdi instance.
  @postcon The compiler condition data is placed on top of the stack.

  @param   CompilerConditionData as a TCompilerConditionData

**)
Procedure TCompilerConditionStack.Push(CompilerConditionData: TCompilerConditionData);

Begin
  Push(CompilerConditionData.CompilerCondition, CompilerConditionData.TokenIndex);
End;

(**

  This method adds the given valud to the top of the stack.

  @precon  None.
  @postcon Adds the given valud to the top of the stack.

  @param   iCompilerCondition as a TCompilerCondition
  @param   iTokenIndex        as a TTokenIndex

**)
Procedure TCompilerConditionStack.Push(iCompilerCondition: TCompilerCondition;
  iTokenIndex: TTokenIndex);

Begin
  FStack.Add(TCompilerConditionData.Create(iCompilerCondition, iTokenIndex));
End;

End.
