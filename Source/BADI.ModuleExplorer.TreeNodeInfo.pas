(**

  This module contains a class to hold the tree node information for each module explorer node.

  @Author  David Hoyle
  @Version 1.0
  @Date    15 Aug 2019

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
Unit BADI.ModuleExplorer.TreeNodeInfo;

Interface

Uses
  Classes,
  Graphics,
  BADI.Types,
  BADI.Comment,
  BADI.ElementContainer,
  BADI.Base.Container,
  BADI.Comment.Tag;

Type
  (** This class represents information about each nodes. Used in a collection
      instead of hanging the data off the tree nodes.  **)
  TBADITreeNodeInfo = Class
  Strict Private
    FText          : String;
    FName          : String;
    FLine          : Integer;
    FCol           : Integer;
    FComment       : TComment;
    FImageIndex    : Integer;
    FTokens        : TStringList;
    FLevel         : Integer;
    FTitle         : Boolean;
    FTagProperties : TBADITagProperties;
    FFontStyles    : TFontStyles;
    FForeColour    : TColor;
    FBackColour    : TColor;
  Strict Protected
    Function GetTokens: TStringList;
  Public
    Constructor Create(Const Element : TElementContainer; Const iLevel : Integer); Overload;
    Constructor Create(Const Tag : TTag; Const iLevel : Integer; Const iImageIndex : Integer;
      Const TagProperties : TBADITagProperties; Const FontStyles : TFontStyles;
      Const FontColour : TColor; Const BackColour : TColor;Const Comment : TComment); Overload;
    Constructor Create(Const strText, strName: String; Const iLevel : Integer;
      Const iImageIndex: Integer; Const boolTitle: Boolean = False); Overload;
    Destructor Destroy; Override;
    (**
      This property returns the text representation of the tree element.
      @precon  None.
      @postcon Returns the text representation of the tree element.
      @return  a String
    **)
    Property Text: String Read FText;
    (**
      This property returns the Name of the tree element.
      @precon  None.
      @postcon Returns the Name of the tree element.
      @return  a String
    **)
    Property Name: String Read FName;
    (**
      This property returns the line number of the tree node comment.
      @precon  None.
      @postcon Returns the line associated with the node info structure.
      @return  an Integer
     **)
    Property Line: Integer Read FLine;
    (**
      This property returns the column number of the tree node comment.
      @precon  None.
      @postcon Returns the column associated with the node info structure.
      @return  an Integer
     **)
    Property Col: Integer Read FCol;
    (**
      This property returns the comment associated with the tree node info.
      @precon  None.
      @postcon Returns the comment associated with the node info structure.
      @return  a TComment
     **)
    Property Comment: TComment Read FComment;
    (**
      This property returns the integer image index of the tree element.
      @precon  None.
      @postcon Returns the integer image index of the tree element.
      @return  an Integer
    **)
    Property ImageIndex: Integer Read FImageIndex;
    (**
      This property returns a token list representing the text of this element.
      @precon  None.
      @postcon Returns a token list representing the text of this element.
      @return  a TStringList
    **)
    Property Tokens: TStringList Read GetTokens;
    (**
      This property provides information on the level within the treeview that
      this item represents.
      @precon  None.
      @postcon Provides information on the level within the treeview that
               this item represents.
      @return  an Integer
    **)
    Property Level: Integer Read FLevel;
    (**
      This property provides information on whether the items it a label or not.
      @precon  None.
      @postcon Provides information on whether the items it a label or not.
      @return  a Boolean
    **)
    Property Title: Boolean Read FTitle;
    (**
      This property deterines the tag properties of the tree node.
      @precon  None.
      @postcon Returns the tag properties of the tree node.
      @return  a TBADITagProperties
    **)
    Property TagProperties : TBADITagProperties Read FTagProperties;
    (**
      This property exposes the nodes custom foreground colour.
      @precon  None.
      @postcon Returns the nodes custom foreground colour.
      @return  a TColor
    **)
    Property ForeColour : TColor Read FForeColour;
    (**
      This property exposes the nodes custom background colour.
      @precon  None.
      @postcon Returns the nodes custom background colour.
      @return  a TColor
    **)
    Property BackColour : TColor Read FBackColour;
    (**
      This property exposes the nodes custom Font Styles.
      @precon  None.
      @postcon Returns the nodes custom font styles.
      @return  a TFontStyles
    **)
    Property FontStyles : TFontStyles Read FFontStyles;
  End;

Var
  (**
    A private variable which is assigned the key words array.
    @refactor These MUST be removed: GLOBAL Variables are BAD!!!!
  **)
  strReservedWords, strDirectives: TKeyWords;

Implementation

Uses
  BADI.Generic.Tokenizer,
  BADI.Options, BADI.DocIssue;

(**

  This is the constructor method for the TTreeNodeInfo class.

  @precon  This line number of the comment, This column number of the comment and This comment to
           be added to the node info object.
  @postcon Initialises the class.

  @param   strText     as a String as a constant
  @param   strName     as a String as a constant
  @param   iLevel      as an Integer as a constant
  @param   iImageIndex as an Integer as a constant
  @param   boolTitle   as a Boolean as a constant

**)
Constructor TBADITreeNodeInfo.Create(Const strText, strName: String; Const iLevel : Integer;
  Const iImageIndex : Integer; Const boolTitle: Boolean = False);

Begin
  FText :=  strText;
  FName :=  strName;
  FLevel := iLevel;
  FTitle := boolTitle;
  FLine :=  0;
  FCol :=   0;
  FComment := Nil;
  FImageIndex := iImageIndex;
  FTokens := Nil;
  FTagProperties := [];
  FFontStyles := [];
  FForeColour := clNone;
  FBackColour := clNone;
End;

(**

  This is the constructor method for the TTreeNodeInfo class.

  @precon  This line number of the comment, This column number of the comment and This comment to be 
           added to the node info object.
  @postcon Initialises the class.

  @param   Element as a TElementContainer as a constant
  @param   iLevel  as an Integer as a constant

**)
Constructor TBADITreeNodeInfo.Create(Const Element : TElementContainer; Const iLevel : Integer);

Begin
  Create(ELement.AsString(True, False), Element.Name, iLevel, Element.ImageIndexAdjustedForScope,
    Element Is TLabelContainer);
  FLine :=  Element.Line;
  FCol :=   Element.Column;
  FComment := TComment.Create(Element.Comment);
  FTagProperties := [tpSyntax];
  If Element Is TDocIssue Then
    Begin
      FTagProperties := [];
      Case (Element As TDocIssue).ErrorType Of
        etHint:
          If doSyntaxHighlightHints In TBADIOptions.BADIOptions.Options Then
            Include(FTagProperties, tpSyntax);
        etWarning:
          If doSyntaxHighlightWarnings In TBADIOptions.BADIOptions.Options Then
            Include(FTagProperties, tpSyntax);
        etError:
          If doSyntaxHighlightErrors In TBADIOptions.BADIOptions.Options Then
            Include(FTagProperties, tpSyntax);
      End;
    End;
  If Element Is TDocumentConflict Then
    Begin
      FTagProperties := [];
      If doSyntaxHighlightConflict In TBADIOptions.BADIOptions.Options Then
        Include(FTagProperties, tpSyntax);
    End;
End;

(**

  This is the constructor method for the TTreeNodeInfo class.

  @precon  This line number of the comment, This column number of the comment and This comment to be 
           added to the node info object.
  @postcon Initialises the class.

  @param   Tag           as a TTag as a constant
  @param   iLevel        as an Integer as a constant
  @param   iImageIndex   as an Integer as a constant
  @param   TagProperties as a TBADITagProperties as a constant
  @param   FontStyles    as a TFontStyles as a constant
  @param   FontColour    as a TColor as a constant
  @param   BackColour    as a TColor as a constant
  @param   Comment       as a TComment as a constant

**)
Constructor TBADITreeNodeInfo.Create(Const Tag: TTag; Const iLevel: Integer;
  Const iImageIndex : Integer; Const TagProperties : TBADITagProperties; Const FontStyles : TFontStyles;
  Const FontColour : TColor; Const BackColour : TColor; Const Comment : TComment);

Begin
  Create(Tag.AsString(MaxInt, False), Tag.Name, iLevel, iImageIndex);
  FLine :=  Tag.Line;
  FCol :=   Tag.Column;
  FComment := TComment.Create(Comment);
  FTagProperties := TagProperties * [tpFixed, tpSyntax];
  FFontStyles := FontStyles;
  FForeColour := FontColour;
  FBackColour := BackColour;
End;

(**

  This is the destructor method for the TTreeNodeInfo class.

  @precon  None.
  @postcon Destroys the instance of the Tree node info class.

 **)

Destructor TBADITreeNodeInfo.Destroy;

Begin
  FTokens.Free;
  FComment.Free;
  Inherited Destroy;
End;

(**

  This is a getter method for the Tokens property.

  @precon  None.
  @postcon Parses the text into token for custom drawing IF the text is
           required, i.e parse on demand.

  @return  a TStringList

**)

Function TBADITreeNodeInfo.GetTokens: TStringList;

Begin
  If FTokens = Nil Then
    FTokens := Tokenize(FText, strReservedWords, strDirectives,
      TBADIOptions.BADIOptions.TokenLimit);
  Result := FTokens;
End;

End.
