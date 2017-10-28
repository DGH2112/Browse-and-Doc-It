(**

  This module contains a class which implements an Object Pascal specific Initialization
  Declaration.

  @Author  David Hoyle
  @Version 1.0
  @Date    28 Oct 2017

**)
Unit BADI.Pascal.InitializationDecl;

Interface

Uses
  BADI.ElementContainer;

Type
  (** A class to represent the initialization section **)
  TInitializationSection = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    Procedure CheckDocumentation(var boolCascade : Boolean); Override;
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
  @postcon Returns the name of the Initialisation section as a String .

  @nohint 
  
  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TInitializationSection.AsString(Const boolShowIdentifier,
  boolForDocumentation : Boolean) : String;

Begin
  Result := '';
  If boolShowIdentifier Then
    Result := Result + Identifier;
End;

(**

  This method check the module`s initialisation sections for comments.

  @precon  None.
  @postcon Check the module`s initialisation sections for comments.

  @nohint 
  
  @param   boolCascade as a Boolean as a reference

**)
Procedure TInitializationSection.CheckDocumentation(Var boolCascade: Boolean);

Begin
  If doShowMissingInitComment In BADIOptions.Options Then
    If (Comment = Nil) Or (Comment.TokenCount = 0) Then
      AddDocumentConflict([strInitializationLabel], Line, Column, Self, strModuleInitSection,
        DocConflictTable[dctMissingInitComment]);
End;

End.
