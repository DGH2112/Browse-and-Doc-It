(**

  This module contains a class to represent a generic type declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    12 Oct 2017

**)
Unit BADI.Generic.TypeDecl;

Interface

Uses
  BADI.ElementContainer;

Type
  (** This is a sub class for all types **)
  TGenericTypeDecl = Class(TElementContainer)
  Public
    Procedure CheckDocumentation(Var boolCascade: Boolean); Override;
    Function AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String; Override;
  End;

Implementation

uses
  BADI.Types,
  BADI.Options,
  BADI.ResourceStrings,
  BADI.Constants;

(**

  This method returns a string representation of the function return type.

  @precon  None.
  @postcon Returns a string representation of the functions return type.

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TGenericTypeDecl.AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String;

Begin
  Result := '';
  If ElementCount > 0 Then
    Result := Elements[1].AsString(boolShowIdentifier, boolForDocumentation);
End;

(**

  This method checks the documentation for the given clause item.

  @precon  C is a valid generic container to be checked for clause like
           documentation.
  @postcon Checks the passed clause for documentation errors.

  @param   boolCascade as a Boolean as a reference

**)
Procedure TGenericTypeDecl.CheckDocumentation(Var boolCascade: Boolean);

Begin
  If doShowUndocumentedTypes In BADIOptions.Options Then
    If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
      Begin
        AddDocumentConflict([Identifier], Line, Column, Comment, strTypeDocumentation,
          DocConflictTable[dctTypeClauseUndocumented]);
      End;
  Inherited CheckDocumentation(boolCascade);
End;

End.
