(**

  This module contains a class that represents a tag in a comment (@@name...).

  @Author  David Hoyle
  @Version 1.190
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
Unit BADI.Comment.Tag;

Interface

Uses
  BADI.Types,
  BADI.Base.Container;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A class to hold text about a single tag **)
  TTag = Class(TBADIBaseContainer)
  Strict Private
  Strict Protected
    Function GetTagName : String;
  Public
    Constructor Create(Const strName : String; Const AScope : TScope; Const iLine,
      iColumn : Integer); Override;
    Destructor Destroy; Override;
    Function AsString(Const iMaxWidth: Integer; Const boolShowHTML: Boolean): String;
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
  BADI.Options;

(**

  This method returns all the tags tokens as a string with spaces in between.

  @precon  boolShowHTML determines of the routine output the HTML tags in the resulting string.
  @postcon Returns a string representation of the tag.

  @param   iMaxWidth    as an Integer as a constant
  @param   boolShowHTML as a Boolean as a constant
  @return  a String

**)
Function TTag.AsString(Const iMaxWidth: Integer; Const boolShowHTML: Boolean): String;

Begin
  Result := OutputCommentAndTag(Self, iMaxWidth, boolShowHTML, Fixed);
End;

(**

  This is the TTag class`s constructor method. It creates the token list.

  @precon  strName is the name of the new tag to be created, iLine is the line number of the tag and 
           iColumn is the column position of the tag.
  @postcon Initialises the comment tag class.

  @param   strName as a String as a constant
  @param   AScope  as a TScope as a constant
  @param   iLine   as an Integer as a constant
  @param   iColumn as an Integer as a constant

**)
Constructor TTag.Create(const strName: String; Const AScope : TScope; Const iLine, iColumn : Integer);

Var
  iTag: Integer;

Begin
  Inherited Create(strName, AScope, iLine, iColumn);
  For iTag := 0 To TBADIOptions.BADIOptions.SpecialTags.Count - 1 Do
    If strName = TBADIOptions.BADIOptions.SpecialTags[iTag].FName Then
      Begin
        Fixed := tpFixed In TBADIOptions.BADIOptions.SpecialTags[iTag].FTagProperties;
        Break;
      End;
End;

(**

  This is the TTag class Destructor method. It disposes of the token list.

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
  @postcon Gets the tag name for the tag from the identifier property.

  @return  a String

**)
Function TTag.GetTagName: String;

Begin
  Result := Identifier;
End;

End.
