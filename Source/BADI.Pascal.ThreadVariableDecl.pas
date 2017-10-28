(**

  This module contains a class which implements an Object Pascal specific ThreadVar Declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    28 Oct 2017

**)
Unit BADI.Pascal.ThreadVariableDecl;

Interface

Uses
  BADI.Pascal.VariableDecl;

Type
  (** This is a sub class for all thread variables. **)
  TThreadVar = Class(TVar)
  Public
    Procedure CheckDocumentation(Var boolCascade: Boolean); Override;
  End;

Implementation

Uses
  BADI.Types,
  BADI.Options,
  BADI.Constants,
  BADI.ResourceStrings;

(**


  This method check whether the thread var has been documented correctly.

  @precon  None.
  @postcon Check whether the field has been documented correctly.

  @nohint

  @param   boolCascade as a Boolean as a reference

**)
Procedure TThreadVar.CheckDocumentation(Var boolCascade: Boolean);

Begin
  If doShowUndocumentedVars In BADIOptions.Options Then
    If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
      AddDocumentConflict([Identifier], Line, Column, Self, strThreadVarDocumentation,
        DocConflictTable[dctThreadVarClauseUndocumented]);
End;

End.
