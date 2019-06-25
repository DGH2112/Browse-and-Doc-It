(**

  This module contains a class to represent a generic variable.

  @Author  David Hoyle
  @Version 1.0
  @Date    21 Jun 2019

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2019  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

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
        AddDocumentConflict([Identifier], Line, Column, Self,
          strVariableDocumentation, DocConflictTable[dctVariableClauseUndocumented]);
      End;
  Inherited CheckDocumentation(boolCascade);
End;

End.
