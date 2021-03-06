(**

  This module contains a class that represents a tag in a comment (@name...).

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
Unit BADI.Comment.Tag;

Interface

Uses
  BADI.Base.Container;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A class to hold text about a single tag **)
  TTag = Class(TBADIBaseContainer)
  Strict Private
  Strict Protected
    Function GetTagName : String;
  Public
    Constructor Create(const strName : String; iLine, iColumn : Integer); Overload;
    Destructor Destroy; Override;
    Function AsString(iMaxWidth : Integer; boolShowHTML : Boolean) : String;
    (**
      Returns the tag name as a string.
      @precon  None.
      @postcon Returns the tag name as a string.
      @return  a String
    **)
    Property TagName : String Read GetTagName;
  End;


Implementation

Uses
  BADI.Functions,
  BADI.Options,
  BADI.Types;


(**

  This is the TTag class`s constructor method. It creates the token list.

  @precon  strName is the name of the new tag to be created, iLine is the line
           number of the tag and iColumn is the column position of the tag.
  @postcon Initialises the comment tag class.

  @param   strName as a String as a constant
  @param   iLine   as an Integer
  @param   iColumn as an Integer

**)
Constructor TTag.Create(const strName: String; iLine, iColumn: Integer);

Var
  iTag: Integer;

Begin
  Inherited Create(strName, iLine, iColumn);
  For iTag := 0 To TBADIOptions.BADIOptions.SpecialTags.Count - 1 Do
    If strName = TBADIOptions.BADIOptions.SpecialTags[iTag].FName Then
      Begin
        Fixed := tpFixed In TBADIOptions.BADIOptions.SpecialTags[iTag].FTagProperties;
        Break;
      End;
End;

(**

  This is the TTag class Destructor method. It disploses of the token list.

  @precon  None.
  @postcon Frees the tags tokens.

**)
Destructor TTag.Destroy;

Begin
  Inherited Destroy;
End;

(**

  This is a getter method for the TagName property.

  @precon  None.
  @postcon Gets the tah name for the tag from the identifer property.

  @return  a String

**)
Function TTag.GetTagName: String;

Begin
  Result := Identifier;
End;

(**

  This method returns all the tags tokens as a string with spaces in between.

  @precon  ShowHTML determines of the routine output the HTML tags in the
           resulting string.
  @postcon Returns a string representation of the tag.

  @param   iMaxWidth    as an Integer
  @param   boolShowHTML as a Boolean
  @return  a String

**)
Function TTag.AsString(iMaxWidth: Integer; boolShowHTML: Boolean): String;

Begin
  Result := OutputCommentAndTag(Self, iMaxWidth, boolShowHTML, Fixed);
End;

End.
