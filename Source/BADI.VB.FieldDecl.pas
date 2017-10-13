(**

  This module contains a class to represent a field declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    13 Oct 2017

**)
Unit BADI.VB.FieldDecl;

Interface

{$INCLUDE CompilerDefinitions.Inc}

Uses
  BADI.VB.VariableDecl;

Type
  (** A class to represent Field Values **)
  TVBField = Class(TVBVar)
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    Procedure CheckDocumentation(var boolCascade : Boolean); Override;
  End;

Implementation

Uses
  BADI.Options,
  BADI.Types,
  BADI.ResourceStrings,
  BADI.Constants;

(**

  This method returns a string representation of the visual basic field.

  @precon  None .
  @postcon Returns a string representation of the visual basic field .

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TVBField.AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String;

Const
  strAs = 'As';

Begin
  Result := BuildStringRepresentation(boolShowIdentifier, boolForDocumentation,
    strAs, BADIOptions.MaxDocOutputWidth);
End;

(**

  This method checks the documentation of the field and outputs a documentation
  conflict IF the options ask for one and it the documentation is missing.

  @precon  None.
  @postcon Checks the documentation of the field and outputs a documentation
           conflict IF the options ask for one and it the documentation is
           missing.

  @param   boolCascade as a Boolean as a reference

**)
Procedure TVBField.CheckDocumentation(Var boolCascade: Boolean);
Begin
  If doShowUndocumentedFields In BADIOptions.Options Then
    If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
      AddDocumentConflict([Identifier], Line, Column, Comment,
        strVariableDocumentation, DocConflictTable[dctFieldClauseUndocumented]);
End;

End.
