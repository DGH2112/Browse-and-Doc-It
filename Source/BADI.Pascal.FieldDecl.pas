(**

  This module contains a class which implements an Object Pascal specific Field Declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    24 Feb 2017

**)
Unit BADI.Pascal.FieldDecl;

Interface

Uses
  BADI.ElementContainer;

Type
  (** This class presents a field in a record, object, or class. **)
  TField = Class(TElementContainer)
    {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(boolShowIdentifier, boolForDocumentation: Boolean): String; Override;
    Procedure CheckDocumentation(Var boolCascade: Boolean); Override;
  End;

Implementation

Uses
  BADI.Options,
  BADI.Types,
  BADI.ResourceStrings,
  BADI.Constants;

(**

  This is a getter method for the AsString property.

  @precon  None.
  @postcon Returns the name of the field and = sign and then the definition.

  @param   boolShowIdentifier   as a Boolean
  @param   boolForDocumentation as a Boolean
  @return  a String

**)
Function TField.AsString(boolShowIdentifier, boolForDocumentation: Boolean): String;

Begin
  Result := BuildStringRepresentation(boolShowIdentifier, boolForDocumentation, ':',
    BrowseAndDocItOptions.MaxDocOutputWidth);
End;

(**


  This method check whether the field has been documented correctly.

  @precon  None.
  @postcon Check whether the field has been documented correctly.


  @param   boolCascade as a Boolean as a reference

**)
Procedure TField.CheckDocumentation(Var boolCascade: Boolean);
Begin
  If doShowUndocumentedFields In BrowseAndDocItOptions.Options Then
    If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
      AddDocumentConflict([Identifier], Line, Column, Comment, strFieldDocumentation,
        DocConflictTable[dctFieldClauseUndocumented]);
  Inherited CheckDocumentation(boolCascade);
End;

End.
