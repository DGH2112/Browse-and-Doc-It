(**

  This module contains a class to represent a field declaration.

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

  @nohint

  @param   boolCascade as a Boolean as a reference

**)
Procedure TVBField.CheckDocumentation(Var boolCascade: Boolean);
Begin
  If doShowUndocumentedFields In BADIOptions.Options Then
    If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
      AddDocumentConflict([Identifier], Line, Column, Self,
        strVariableDocumentation, DocConflictTable[dctFieldClauseUndocumented]);
End;

End.
