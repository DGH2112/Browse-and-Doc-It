(**

  This module contains a class to represent a generic constant.

  @Author  David Hoyle
  @Version 1.0
  @Date    28 Oct 2017

**)
Unit BADI.Generic.Constant;

Interface

Uses
  BADI.ElementContainer;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This is a sub class for all constants **)
  TGenericConstant = Class {$IFDEF D2005} Abstract {$ENDIF} (TElementContainer)
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

  This method checks the documentation for the given clause item.

  @precon  C is a valid generic container to be checked for clause like
           documentation.
  @postcon Checks the passed clause for documentation errors.

  @param   boolCascade as a Boolean as a reference

**)
Procedure TGenericConstant.CheckDocumentation(Var boolCascade: Boolean);

Begin
  If doShowUndocumentedConsts In BADIOptions.Options Then
    If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
      Begin
        AddDocumentConflict([Identifier], Line, Column, Self, strConstantDocumentation,
          DocConflictTable[dctConstantClauseUndocumented]);
      End;
  Inherited CheckDocumentation(boolCascade);
End;

End.
