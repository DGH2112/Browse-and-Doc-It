(**

  This module contains a class to represent a generic variable.

  @Author  David Hoyle
  @Version 1.0
  @Date    01 Apr 2017

**)
Unit BADI.Generic.Variable;

Interface

Uses
  BADI.ElementContainer;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This is a sub class for all variables **)
  TGenericVariable = Class {$IFDEF D2005} Abstract {$ENDIF} (TElementContainer)
  Public
    Procedure CheckDocumentation(Var boolCascade: Boolean); Override;
  End;

Implementation

uses
  BADI.Types,
  BADI.Options,
  BADI.ResourceStrings,
  BADI.Constants;

(**

  This method checks the documentation for the given clause item.

  @precon  C is a valid generic container to be checked for clause like
           documentation.
  @postcon Checks the passed clause for documentation errors.

  @param   boolCascade as a Boolean as a reference

**)
Procedure TGenericVariable.CheckDocumentation(var boolCascade : Boolean);

Begin
  If doShowUndocumentedVars In BADIOptions.Options Then
    If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
      Begin
        AddDocumentConflict([Identifier], Line, Column, Comment,
          strVariableDocumentation, DocConflictTable[dctVariableClauseUndocumented]);
      End;
  Inherited CheckDocumentation(boolCascade);
End;

End.
