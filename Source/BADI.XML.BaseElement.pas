(**

  This modlue containsa class whic represents a base element class for all the XML elements in the
  parser.

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
Unit BADI.XML.BaseElement;

Interface

{$INCLUDE CompilerDefinitions.inc}

Uses
  BADI.ElementContainer,
  BADI.Types,
  BADI.Comment;

Type
  (** This is a base class for all the XML elements. **)
  TXMLBaseElement = Class(TElementContainer)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FElementName: String;
  Public
    Constructor Create(Const strName : String; Const AScope : TScope; Const iLine,
      iColumn : Integer; Const AImageIndex : TBADIImageIndex; Const AComment : TComment); Override;
    Function GetName : String; Override;
    Function AsString(Const boolShowIdenifier, boolForDocumentation : Boolean) : String; Override;
  End;

Implementation

Uses
  SysUtils;

(**

  This method returns a string representation of the XML Base Element.

  @precon  None.
  @postcon Returns a string representation of the XML Base Element.

  @param   boolShowIdenifier    as a Boolean as a constant
  @param   boolForDocumentation as a Boolean as a constant
  @return  a String

**)
Function TXMLBaseElement.AsString(Const boolShowIdenifier, boolForDocumentation: Boolean): String;

Begin
  Result := Identifier;
End;

(**

  This is a constructor for the TXMLBaseElement class.

  @precon  None.
  @postcon Initialises the class to be not sorted and have a unique name.

  @param   strName     as a String as a constant
  @param   AScope      as a TScope as a constant
  @param   iLine       as an Integer as a constant
  @param   iColumn     as an Integer as a constant
  @param   AImageIndex as a TBADIImageIndex as a constant
  @param   AComment    as a TComment as a constant

**)
Constructor TXMLBaseElement.Create(Const strName: String; Const AScope: TScope; Const iLine,
  iColumn: Integer; Const AImageIndex: TBADIImageIndex; Const AComment: TComment);

Const
  strOutputFmt = '%s:%4.4d:%4.4d';

Begin
  Inherited Create(strName, AScope, iLine, iColumn, AImageIndex, AComment);
  Sorted := False;
  FElementName := Format(strOutputFmt, [strName, iLine, iColumn]);
End;

(**

  This is a getter method for the Name property.

  @precon  None.
  @postcon Returns the name of the element created in the constructor.

  @return  a String

**)
Function TXMLBaseElement.GetName: String;

Begin
  Result := FElementName;
End;

End.
