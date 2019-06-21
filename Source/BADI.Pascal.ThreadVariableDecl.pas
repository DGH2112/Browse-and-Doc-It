(**

  This module contains a class which implements an Object Pascal specific ThreadVar Declaration.

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
Unit BADI.Pascal.ThreadVariableDecl;

Interface

Uses
  BADI.Pascal.VariableDecl;

Type
  (** This is a sub class for all thread variables. **)
  TThreadVar = Class(TVar)
  Public
    Procedure CheckDocumentation(Var boolCascade: Boolean); Override;
  End;

Implementation

Uses
  BADI.Types,
  BADI.Options,
  BADI.Constants,
  BADI.ResourceStrings;

(**


  This method check whether the thread var has been documented correctly.

  @precon  None.
  @postcon Check whether the field has been documented correctly.

  @nohint

  @param   boolCascade as a Boolean as a reference

**)
Procedure TThreadVar.CheckDocumentation(Var boolCascade: Boolean);

Begin
  If doShowUndocumentedVars In BADIOptions.Options Then
    If ((Comment = Nil) Or (Comment.TokenCount = 0)) And (Scope <> scLocal) Then
      AddDocumentConflict([Identifier], Line, Column, Self, strThreadVarDocumentation,
        DocConflictTable[dctThreadVarClauseUndocumented]);
End;

End.
