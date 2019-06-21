(**

  This module contains a class to represent a DFM file item.

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
Unit BADI.DFM.Item;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  BADI.ElementContainer,
  BADI.Types,
  BADI.Comment;

Type
  (** This class represent a DFM Item in the file. **)
  TDFMItem = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FItemName: String;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function GetName: String; Override;
  Public
    Constructor Create(Const strName: String; Const AScope: TScope; Const iLine,
      iColumn: Integer; Const AImageIndex: TBADIImageIndex; Const AComment: TComment); Override;
    Function AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String; Override;
  End;

Implementation

Uses
  SysUtils;

(**

  This method returns a string representation of the item.

  @precon  None.
  @postcon Returns a string representation of the item.

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TDFMItem.AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String;

Begin
  Result := 'Item';
End;

(**

  A constructor for the TDFMItem class.

  @precon  None.
  @postcon Creates a unique name for the item.

  @param   strName     as a String as a constant
  @param   AScope      as a TScope as a constant
  @param   iLine       as an Integer as a constant
  @param   iColumn     as an Integer as a constant
  @param   AImageIndex as a TBADIImageIndex as a constant
  @param   AComment    as a TComment as a constant

**)
Constructor TDFMItem.Create(Const strName: String; Const AScope: TScope; Const iLine,
  iColumn: Integer; Const AImageIndex: TBADIImageIndex; Const AComment: TComment);

Begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FItemName := Format('%s:%4.4d:%4.4d', [strName, iLine, iColumn]);
End;

(**

  This is an overridden GetName to provide a unqiue name for the item.

  @precon  None.
  @postcon Returns a unqiue name for the item.

  @return  a String

**)
Function TDFMItem.GetName: String;

Begin
  Result := FItemName;
End;

End.
