(**

  This module contains a class which implements an Object Pascal specific Resource String
  Declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    01 Apr 2017

**)
Unit BADI.Pascal.ResourceStringDecl;

Interface

Uses
  BADI.Pascal.ConstantDecl;

Type
  (** This is a sub class for all resource strings. **)
  TResourceString = Class(TConstant)
  Public
    Procedure CheckDocumentation(var boolCascade : Boolean); Override;
  End;

Implementation

Uses
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
Procedure TResourceString.CheckDocumentation(Var boolCascade: Boolean);

Begin
  If doShowUndocumentedConsts In BADIOptions.Options Then
    If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
      AddDocumentConflict([Identifier], Line, Column, Comment, strResourceStringDocumentation,
        DocConflictTable[dctResourceStringClauseUndocumented]);
End;

End.
