(**

  This module contains a class which implements an Object Pascal specific Exports Declaration.

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
Unit BADI.Pascal.ExportsItem;

Interface

Uses
  BADI.ElementContainer,
  BADI.Types,
  BADI.Comment;

Type
  (** This class represents an exported method. **)
  TExportsItem = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FResolved: Boolean;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Constructor Create(Const strName: String; Const AScope: TScope; Const iLine, iColumn: Integer;
      Const AImageIndex: TBADIImageIndex; Const AComment: TComment); Override;
    Function AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String; Override;
    (**
      This property gets and sets whether the symbol is resolved.
      @precon  None.
      @postcon Gets and sets whether the symbol is resolved.
      @return  a Boolean
    **)
    Property Resolved: Boolean Read FResolved Write FResolved;
  End;

Implementation

(**

  This is a getter method for the AsString property.

  @precon  None .
  @postcon Returns the Exported item declaration .

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TExportsItem.AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String;

Var
  iToken: Integer;

Begin
  Result := '';
  If boolShowIdentifier Then
    Result := Result + Identifier;
  For iToken := 0 To TokenCount - 1 Do
    Begin
      If Result <> '' Then
        Result := Result + #32;
      Result := Result + Tokens[iToken].Token;
    End;
End;

(**

  This is a constructor for the TExportsItem class.

  @precon  None.
  @postcon Initialises the class.

  @param   strName     as a String as a constant
  @param   AScope      as a TScope as a constant
  @param   iLine       as an Integer as a constant
  @param   iColumn     as an Integer as a constant
  @param   AImageIndex as a TBADIImageIndex as a constant
  @param   AComment    as a TComment as a constant

**)
Constructor TExportsItem.Create(Const strName: String; Const AScope: TScope; Const iLine, iColumn: Integer;
      Const AImageIndex: TBADIImageIndex; Const AComment: TComment);

Begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FResolved := False;
End;

End.
