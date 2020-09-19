(**

  This module contains a custom virtual string tree class for the BADI module explorer.

  @Author  David Hoyle
  @Version 1.010
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
Unit BADI.ModuleExplorer.VirtualStringTree;

Interface

Uses
  VirtualTrees,
  Graphics,
  BADI.ModuleExplorer.TreeNodeInfo;

Type
  (** This is a descendant of the TVirtualStringTree in order to override the On Get Node Width
      method. **)
  TBADIVirtualStringTree = Class(TVirtualStringTree)
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function DoGetNodeWidth(Node: PVirtualNode; Column: TColumnIndex; Canvas: TCanvas = Nil) : Integer;
      Override;
  End;

  (** This is an enumerate to defines the information type in the explorer module tree nodes. **)
  TBADINodeType = (ntLabel, ntDocConflict, ntDocIssue, ntElement, ntSpellingIssue);

  (** This record described the data sorted in the virtual tree view. **)
  TBADITreeData = Record
    FNode : TBADITreeNodeInfo;
    FNodeType : TBADINodeType;
  End;
  (** A type to define a pointer to the above tree record. **)
  PBADITreeData = ^TBADITreeData;

Implementation

Uses
  Classes,
  BADI.Options,
  BADI.Functions,
  BADI.Types;

(**

  This method overrides the DoGetNodeWidth of the tree view and calculates the
  modified node width.

  @precon  None.
  @postcon Returns the modified node width.

  @nocheck MissingCONSTInParam
  @nohint  Column
  
  @param   Node   as a PVirtualNode
  @param   Column as a TColumnIndex
  @param   Canvas as a TCanvas
  @return  an Integer

**)
Function TBADIVirtualStringTree.DoGetNodeWidth(Node: PVirtualNode;
  Column: TColumnIndex; Canvas: TCanvas): Integer;

Const
  iPadding = 5;

Var
  NodeData: PBADITreeData;
  sl: TStringList;
  i: Integer;
  TokenFontInfo: TBADITokenFontInfoTokenSet;
  iBGColour: TColor;

Begin
  Result := iPadding;
  NodeData := GetNodeData(Node);
  //: @note Self.Canvas used to access the treeview canvas as Canvas from the parameters above is
  //:       NIL!
  InitCanvasFont(Self.Canvas, tpFixed In NodeData.FNode.TagProperties, TBADIOptions.BADIOptions);
  TokenFontInfo := TBADIOptions.BADIOptions.TokenFontInfo[TBADIOptions.BADIOptions.UseIDEEditorColours];
  iBGColour := TBADIOptions.BADIOptions.BGColour[TBADIOptions.BADIOptions.UseIDEEditorColours];
  If iBGColour = clNone Then
    iBGColour := TBADIOptions.BADIOptions.BGColour[False];
  sl := NodeData.FNode.Tokens;
  For i := 0 To sl.Count - 1 Do
    Begin
      GetFontInfo(
        sl,
        i,
        NodeData.FNode.Title,
        tpSyntax In NodeData.FNode.TagProperties,
        NodeData.FNode.ForeColour,
        NodeData.FNode.BackColour,
        NodeData.FNode.FontStyles,
        TokenFontInfo,
        iBGColour,
        Self.Canvas
      );
      Inc(Result, Self.Canvas.TextWidth(sl[i]) + 1);
    End;
End;

End.
