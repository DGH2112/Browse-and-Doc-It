(**

  This module contains a class that represents a tag in a comment (@name...).

  @Author  David Hoyle
  @Version 1.0
  @Date    14 Apr 2017

**)
Unit BADI.Comment.Tag;

Interface

Uses
  BADI.Base.Container;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A class to hold text about a single tag **)
  TTag = Class(TBaseContainer)
  Strict Private
    FFixed : Boolean;
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
    (**
      This property determines whether the tag is a fixed tag (think <pre>).
      @precon  None.
      @postcon Returns true if the tag is fixed.
      @return  a Boolean
    **)
    Property Fixed : Boolean Read FFixed;
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
  FFixed := False;
  For iTag := 0 To TBADIOptions.BADIOptions.SpecialTags.Count - 1 Do
    If strName = TBADIOptions.BADIOptions.SpecialTags[iTag].FName Then
      Begin
        FFixed := tpFixed In TBADIOptions.BADIOptions.SpecialTags[iTag].FTagProperties;
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
  Result := OutputCommentAndTag(Self, iMaxWidth, boolShowHTML, FFixed);
End;

End.
