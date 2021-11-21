(**

  This module contains a class which implements an Object Pascal specific Finalization Declaration.

  @Author  David Hoyle
  @Version 1.001
  @Date    19 Sep 2020

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2020  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
    Function AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String; Override;
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

  @nohint 
  
  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TFinalizationSection.AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String;

Begin
  Result := '';
  If boolShowIdentifier Then
    Result := Result + Identifier;
End;

(**

  This method check the module`s finalisation sections for comments.

  @precon  None.
  @postcon Check the module`s finalisation sections for comments.

  @nohint 
  
  @param   boolCascade as a Boolean as a reference

**)
Procedure TFinalizationSection.CheckDocumentation(Var boolCascade: Boolean);

Begin
  If doShowMissingFinalComment In BADIOptions.Options Then
    If (Comment = Nil) Or (Comment.TokenCount = 0) Then
      AddDocumentConflict([strFinalizationLabel], Line, Column, Self, strModuleFinalSection,
        DocConflictTable[dctMissingFinalComment]);
End;

End.
