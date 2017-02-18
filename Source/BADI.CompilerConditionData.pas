(**

  This module contains a class to represent each compiler definition to be placed on the stack.

  @Author  David Hoyle
  @Version 1.0
  @Date    18 Feb 2017

**)
Unit BADI.CompilerConditionData;

Interface

Uses
  BADI.Types;

Type
  (** This class represents a single stack entry on the CompilerCondition stacks. **)
  TCompilerConditionData = Class
    {$IFDEF D2005} Strict {$ENDIF} Private
    FCompilerCondition: TCompilerCondition;
    FTokenIndex: TTokenIndex;
    {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Constructor Create(iCompilerCondition: TCompilerCondition; iTokenIndex: TTokenIndex);
    (**
      This property gets and sets the compiler condition (include code or exclude code).
      @precon  None.
      @postcon Gets and sets the compiler condition (include code or exclude code).
      @return  a TCompilerCondition
    **)
    Property CompilerCondition: TCompilerCondition Read FCompilerCondition Write FCompilerCondition;
    (**
      This property gets or sets the token index of the compiler condition.
      @precon  None.
      @postcon Gets or sets the token index of the compiler condition.
      @return  a TTokenIndex
    **)
    Property TokenIndex: TTokenIndex Read FTokenIndex Write FTokenIndex;
  End;

Implementation

(**

  A constructor for the TCompilerConditionData class.

  @precon  None.
  @postcon Initialises the class with data.

  @param   iCompilerCondition as a TCompilerCondition
  @param   iTokenIndex        as a TTokenIndex

**)
Constructor TCompilerConditionData.Create(iCompilerCondition: TCompilerCondition;
  iTokenIndex: TTokenIndex);

Begin
  FCompilerCondition := iCompilerCondition;
  FTokenIndex := iTokenIndex;
End;

End.
