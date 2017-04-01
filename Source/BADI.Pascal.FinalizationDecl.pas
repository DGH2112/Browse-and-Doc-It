(**

  This module contains a class which implements an Object Pascal specific Finalization Declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    01 Apr 2017

**)
Unit BADI.Pascal.FinalizationDecl;

Interface

Uses
  BADI.ElementContainer;

Type
  (** A class to represent the finalization section **)
  TFinalizationSection = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation: Boolean): String; Override;
    Procedure CheckDocumentation(Var boolCascade: Boolean); Override;
  End;

Implementation

Uses
  BADI.Types,
  BADI.Options,
  BADI.Constants,
  BADI.ResourceStrings;

(**

This is a getter method for the AsString property.

  @precon  None .
  @postcon Returns the name of the Finalisation section as a String .

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TFinalizationSection.AsString(boolShowIdentifier, boolForDocumentation: Boolean): String;
Begin
  Result := '';
  If boolShowIdentifier Then
    Result := Result + Identifier;
End;

(**

  This method check the module`s finalisation sections for comments.

  @precon  None.
  @postcon Check the module`s finalisation sections for comments.

  @param   boolCascade as a Boolean as a reference

**)
Procedure TFinalizationSection.CheckDocumentation(Var boolCascade: Boolean);

Begin
  If doShowMissingFinalComment In BADIOptions.Options Then
    If (Comment = Nil) Or (Comment.TokenCount = 0) Then
      AddDocumentConflict([strFinalizationLabel], Line, Column, Comment, strModuleFinalSection,
        DocConflictTable[dctMissingFinalComment]);
End;

End.
