(**

  This module contains a class to represent a n object in a DFM file.

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
Unit BADI.DFM.ObjectDecl;

Interface

Uses
  BADI.ElementContainer,
  BADI.DFM.Types,
  BADI.Types,
  BADI.Comment;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** This class represent a DFM object in the file. **)
  TDFMObject = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FObjectType: TObjectType;
  {$IFDEF D2005} Strict {$ENDIF} Protected
  Public
    Constructor Create(Const strName : String; Const AScope : TScope; Const iLine,
      iColumn : Integer; Const AImageIndex : TBADIImageIndex; Const AComment : TComment); Override;
    Function AsString(Const boolShowIdentifier, boolForDocumentation : Boolean) : String; Override;
    (**
      This property sets and gets the whether the Object, Inherited or Inline.
      @precon  None.
      @postcon Sets and gets the whether the Object, Inherited or Inline.
      @return  a TObjectType
    **)
    Property ObjectType : TObjectType Read FObjectType Write FObjectType;
  End;

Implementation

Uses
  BADI.Options;

(**

  This method returns a string represetation of the DFM object.

  @precon  None.
  @postcon Returns a string represetation of the DFM object.

  @param   boolShowIdentifier   as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TDFMObject.AsString(Const boolShowIdentifier, boolForDocumentation: Boolean): String;

Begin
  Case ObjectType Of
    otObject: Result := 'Object';
    otInherited: Result := 'Inherited';
    otinline: Result := 'Inline';
  End;
  Result := Result + #32 + BuildStringRepresentation(True, boolForDocumentation,
    ':', BADIOptions.MaxDocOutputWidth)
End;

(**

  A constructor for the TDFMObject class.

  @precon  None.
  @postcon Initialises FInherited to false.

  @param   strName     as a String as a constant
  @param   AScope      as a TScope as a constant
  @param   iLine       as an Integer as a constant
  @param   iColumn     as an Integer as a constant
  @param   AImageIndex as a TBADIImageIndex as a constant
  @param   AComment    as a TComment as a constant

**)
Constructor TDFMObject.Create(Const strName: String; Const AScope: TScope; Const iLine,
  iColumn: Integer; Const AImageIndex: TBADIImageIndex; Const AComment: TComment);

Begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  FObjectType := otObject;
End;

End.
