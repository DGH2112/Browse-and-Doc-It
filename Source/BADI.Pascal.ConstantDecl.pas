(**

  This module contains a class which implements an Object Pascal specific constant Declaration.

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
Unit BADI.Pascal.ConstantDecl;

Interface

Uses
  BADI.Generic.Constant,
  BADI.Types,
  BADI.Comment;

Type
  (** This is a sub class for all constants. **)
  TConstant = Class(TGenericConstant)
    {$IFDEF D2005} Strict {$ENDIF} Private
    FTyped: Boolean;
    {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Constructor Create(Const strName: String; Const AScope: TScope; Const iLine, iColumn: Integer;
      Const AImageIndex: TBADIImageIndex; Const AComment: TComment); Override;
    Function AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String; Override;
    (**
      This property determines it the constant is typed or simple.
      @precon  None.
      @postcon Sets or gets whether the constant is typed or simple.
      @return  a Boolean
    **)
    Property Typed: Boolean Read FTyped Write FTyped;
  End;

Implementation

uses
  BADI.Options;

(**

  This is a getter method for the AsString property.

  @precon  None.
  @postcon Formats the constant information depending on whether its a simple constant or a typed
           constant.

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TConstant.AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String;

Begin
  If FTyped Then
    Result := BuildStringRepresentation(boolShowIdentifier, boolForDocumentation, ':',
      BADIOptions.MaxDocOutputWidth)
  Else
    Result := BuildStringRepresentation(boolShowIdentifier, boolForDocumentation, '=',
      BADIOptions.MaxDocOutputWidth);
End;

(**

  This is the constructor method for the TConstant class.

  @precon  None.
  @postcon Creates an instance of a TConstant declaration.

  @param   strName     as a String as a constant
  @param   AScope      as a TScope as a constant
  @param   iLine       as an Integer as a constant
  @param   iColumn     as an Integer as a constant
  @param   AImageIndex as a TBADIImageIndex as a constant
  @param   AComment    as a TComment as a constant

**)
Constructor TConstant.Create(Const strName: String; Const AScope: TScope; Const iLine, iColumn: Integer;
      Const AImageIndex: TBADIImageIndex; Const AComment: TComment);
Begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FTyped := False;
End;

End.
