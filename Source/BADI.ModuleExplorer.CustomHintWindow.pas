(**

  This module contains the custom hint window for displaying information about the module explorer
  tree nodes.

  @Author  David Hoyle
  @Version 1.045
  @Date    10 Sep 2023

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
Unit BADI.ModuleExplorer.CustomHintWindow;

Interface

Uses
  Controls,
  BADI.Comment,
  BADI.Types,
  VirtualTrees,
  Classes,
  System.Generics.Collections,
  Windows,
  Graphics,
  BADI.Interfaces,
  BADI.ModuleExplorer.VirtualStringTree;

Type
  (** This is a custom hint window for displaying hints about the tree view
      items. Its customisation support the syntax highlighting custom draw
      support of the tree view.
   **)
  TBADICustomHintWindow = Class(THintWindow)
  Strict Private
    Const
      (** A constant to define the padding between text elements of the hint. **)
      iPadding = 2;
      (** This is the size of the bullet in the special tags list. **)
      iBulletSize = 8;
  Strict Private
    FComment     : TComment;
    FNodeLevel   : Integer;
    FTitle       : Boolean;
    FCustomDraw  : Boolean;
    FTreeView    : TVirtualStringTree;
    FNode        : PVirtualNode;
    FRect        : TRect;
    FBADIOptions : IBADIOptions;
  Strict Protected
    Procedure CalcTreeNodeWidth(Const SyntaxHighlight : Boolean; Const Node : PVirtualNode;
      Var Result : TRect); InLine;
    Procedure CalcSpecialTags(Const Comment : TComment; Var Result : TRect); InLine;
    Procedure CalcSpecialTag(Const iSpecialTag : Integer; Const Comment : TComment;
      Var Result : TRect); InLine;
    Procedure SetTokenFont(Const Canvas : TCanvas; Const eTokenType : TBADITokenType);
    Procedure PaintRenderTreeNode(Const NodeData : PBADITreeData; Var iBottom : Integer); InLine;
    Procedure PaintRenderComment(Var iBottom : Integer); InLine;
    Procedure PaintRenderSpecialTags(Const iBottom : Integer); InLine;
  Public
    Constructor Create(Const AOwner : TComponent; Const ATreeView : TVirtualStringTree); Reintroduce;
      Overload; Virtual;
    Procedure Paint; Override;
    Function CalcHintRect(MinWidth, MaxWidth : Integer; Node : PVirtualNode;
      SyntaxHighlight : Boolean; Comment : TComment) : TRect; Reintroduce; Overload; Virtual;
    Procedure ActivateHint(Rect : TRect; Node : PVirtualNode;
      SyntaxHighlight : Boolean; Comment : TComment); Reintroduce; Overload; Virtual;
    Function CanDrawSpecialTag(Const Comment : TComment; Const strSpecialTag : String) : Boolean;
  End;

Implementation

uses
  {$IFDEF CODESITE}
  CodeSiteLogging,
  {$ENDIF}
  System.SysUtils,
  System.Types,
  System.UITypes,
  Vcl.Forms,
  BADI.Options,
  BADI.Functions,
  BADI.Comment.Tag;

(**

  This method is an overridden ActivateHint method to allow for the passing of
  more information to the hint windows internal routines.

  @precon  Rect is the rectangle to display the hint in, Node is the tree node
           to create a hint for, SyntaxHighlight tells the routine to print
           either a plain text representation of the information or one with
           syntax highlighting and Comment is the comment associated with the
           tree node.
  @postcon Activates the hint window.

  @nocheck MissingCONSTInParam

  @param   Rect            as a TRect
  @param   Node            as a PVirtualNode
  @param   SyntaxHighlight as a Boolean
  @param   Comment         as a TComment

 **)
Procedure TBADICustomHintWindow.ActivateHint(Rect : TRect; Node : PVirtualNode;
  SyntaxHighlight : Boolean; Comment : TComment);

Var
  NodeData : PBADITreeData;

Begin
  FComment := Comment;
  NodeData := FTreeView.GetNodeData(Node);
  FNodeLevel := NodeData.FNode.Level;
  FCustomDraw := SyntaxHighlight;
  FNode := Node;
  Color := FBADIOptions.TokenFontInfo[FBADIOptions.UseIDEEditorColours][ttExplorerHighlight].FBackColour;
  ActivateHint(Rect, NodeData.FNode.Text);
End;

(**

  This method calculates the size of the hint window based on the tree node
  display window, a max and min width , whether to display syntax highlighting
  and a comment.

  @precon  MinWidth is the minimum width of the hint window to be displayed,
           MaxWidth is the minimum width of the hint window to be displayed,
           Node is the tree node to calculate the hint window for,
           SyntaxHighlight tells the routine to print either a plain text
           representation of the information or one with syntax highlighting,
           and Comment is the comment associated with the tree node and
  @postcon Returns the newly calculated rectangle of the hint window.

  @nocheck MissingCONSTInParam
  @nohint  MaxWidth

  @param   MinWidth        as an Integer
  @param   MaxWidth        as an Integer
  @param   Node            as a PVirtualNode
  @param   SyntaxHighlight as a Boolean
  @param   Comment         as a TComment
  @return  a TRect

 **)
Function TBADICustomHintWindow.CalcHintRect(MinWidth, MaxWidth : Integer;
  Node : PVirtualNode; SyntaxHighlight : Boolean; Comment : TComment) : TRect;

Var
  R : TRect;
  strText : String;

Begin
  Result := FTreeView.GetDisplayRect(Node, NoColumn, True);
  Inc(Result.Left, 1 + iPadding);
  Dec(Result.Top, 1 + iPadding);
  CalcTreeNodeWidth(SyntaxHighlight, Node, Result);
  Dec(Result.Left, 1 + iPadding);
  Inc(Result.Right, 1 + iPadding);
  // Check for comment
  If (Comment <> Nil) And ((Comment.TokenCount > 0) Or (Comment.TagCount > 0)) Then
    Begin
      If Result.Right - Result.Left < MinWidth Then
        Result.Right := Result.Left + MinWidth;
      Inc(Result.Bottom, iPadding + 1);
      Inc(Result.Bottom, iPadding + 1);
      Refresh;
      InitCanvasFont(Canvas, False, FBADIOptions);
      SetTokenFont(Canvas, ttCommentText);
      strText := Comment.AsString(MaxInt, False);
      R := Rect(Result.Left + iPadding, 0, Result.Right - iPadding, 0);
      Inc(Result.Bottom, DrawText(Canvas.Handle, PChar(strText), -1, R,
        DT_CALCRECT Or DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
        DrawTextBiDiModeFlagsReadingOnly) + iPadding);
      CalcSpecialTags(Comment, Result);
    End;
End;

(**

  This method calculates the additional height required for a special tag.

  @precon  None.
  @postcon The resultant hint window rectangle is increased in height for a special tag.

  @param   iSpecialTag as an Integer as a constant
  @param   Comment     as a TComment as a constant
  @param   Result      as a TRect as a reference

**)
Procedure TBADICustomHintWindow.CalcSpecialTag(Const iSpecialTag : Integer;
  Const Comment : TComment; Var Result : TRect);

Const
  iMultipler = 2;

Var
  iTag: Integer;
  R: TRect;
  strText: String;

Begin
  For iTag := 0 To Comment.TagCount - 1 Do
    If CompareText(FBADIOptions.SpecialTags[iSpecialTag].FName,
      Comment.Tag[iTag].TagName) = 0 Then
      Begin
        Refresh;
        InitCanvasFont(Canvas, Comment.Tag[iTag].Fixed, FBADIOptions);
        SetTokenFont(Canvas, ttTagHeaderText);
        R := Rect(Result.Left + iPadding, 0, Result.Right - iPadding - iMultipler * iBulletSize, 0);
        strText := Comment.Tag[iTag].AsString(MaxInt, False);
        Inc(Result.Bottom, DrawText(Canvas.Handle, PChar(strText), -1, R,
          DT_CALCRECT Or DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
          DrawTextBiDiModeFlagsReadingOnly) + iPadding);
      End;
End;

(**

  This method calculates the additional height required for all the special tags.

  @precon  None.
  @postcon The resultant hint window rectangle is increased in height for all the special tags.

  @param   Comment as a TComment as a constant
  @param   Result  as a TRect as a reference

**)
Procedure TBADICustomHintWindow.CalcSpecialTags(Const Comment : TComment; Var Result : TRect);

Var
  iSpecialTag : Integer;
  R: TRect;
  strText: String;

Begin
  For iSpecialTag := 0 To FBADIOptions.SpecialTags.Count - 1 Do
    Begin
      If CanDrawSpecialTag(Comment, FBADIOptions.SpecialTags[iSpecialTag].FName) Then
        Begin
          Refresh;
          InitCanvasFont(Canvas, False, FBADIOptions);
          SetTokenFont(Canvas, ttTagHeaderText);
          R := Rect(Result.Left + iPadding, 0, Result.Right - iPadding, 0);
          strText := FBADIOptions.SpecialTags[iSpecialTag].FDescription;
          Inc(Result.Bottom, DrawText(Canvas.Handle, PChar(strText), -1, R,
            DT_CALCRECT Or DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
            DrawTextBiDiModeFlagsReadingOnly) + iPadding);
          Inc(Result.Bottom, 1 + iPadding);
          CalcSpecialTag(iSpecialTag, Comment, Result);
        End;
    End;
End;

(**

  This method calculates the width of the hint window to accommodate the tree nodes text and comments
  .

  @precon  None.
  @postcon The rectangle for the hint window is updated.

  @param   SyntaxHighlight as a Boolean as a constant
  @param   Node            as a PVirtualNode as a constant
  @param   Result          as a TRect as a reference

**)
Procedure TBADICustomHintWindow.CalcTreeNodeWidth(Const SyntaxHighlight : Boolean;
  Const Node : PVirtualNode; Var Result : TRect);

Const
  iIndentMultiLines = 10;
  iMultipler = 2;
  strTextHeightTest = 'Ag';

Var
  NodeData : PBADITreeData;
  iPos : Integer;
  sl: TStringList;
  iLastmax: Integer;
  iToken: Integer;
  iMaxPos : Integer;
  TokenFontInfo : TBADITokenFontInfoTokenSet;
  iBGColour: TColor;

Begin
  iMaxPos := FTreeView.ScreenToClient(Point(Screen.WorkAreaRect.Right, 0)).X - Result.Left;
  If SyntaxHighlight Then
    Begin
      // Need to amend the width of the rectangle for the custom drawing
      iPos := iPadding;
      NodeData := FTreeView.GetNodeData(Node);
      sl := NodeData.FNode.Tokens;
      iLastmax := 0;
      InitCanvasFont(Canvas, tpFixed In NodeData.FNode.TagProperties, FBADIOptions);
      TokenFontInfo := FBADIOptions.TokenFontInfo[FBADIOptions.UseIDEEditorColours];
      iBGColour := FBADIOptions.BGColour[FBADIOptions.UseIDEEditorColours];
      If iBGColour = clNone Then
        iBGColour := FBADIOptions.BGColour[False];
      // Start with single line height
      Result.Bottom := Result.Top + iPadding * iMultipler + Canvas.TextHeight(strTextHeightTest);
      For iToken := 0 To sl.Count - 1 Do
        Begin
          GetFontInfo(sl, iToken, NodeData.FNode.Title, tpSyntax In NodeData.FNode.TagProperties,
            NodeData.FNode.ForeColour, NodeData.FNode.BackColour, NodeData.FNode.FontStyles,
            TokenFontInfo, iBGColour, Canvas);
          Inc(iPos, Canvas.TextWidth(sl[iToken]));
          If (iPos > iMaxPos) Or (sl[iToken] = #13#10) Then
            Begin
              Inc(Result.Bottom, Canvas.TextHeight(sl[iToken]));
              iPos := iPadding;
              If sl[iToken] <> #13#10 Then
                Begin
                  Inc(iPos, iIndentMultiLines); // indent multiple lines
                  iLastMax := iMaxPos;
                End;
            End Else
              If iPos > iLastMax Then
                iLastMax := iPos;
        End;
      Result.Right := Result.Left + iLastMax;
    End;
End;

(**

  This method determines if the special tag need to be rendered in the hint window.

  @precon  Comment is the comment to get tags from and strSpecialTag is the special tag information to 
           look for.
  @postcon Return true is the special tag was found in the comment.

  @param   Comment       as a TComment as a constant
  @param   strSpecialTag as a String as a constant
  @return  a Boolean

**)
Function TBADICustomHintWindow.CanDrawSpecialTag(Const Comment : TComment;
  Const strSpecialTag : String) : Boolean;

Var
  i : Integer;

Begin
  Result := False;
  For i := 0 To Comment.TagCount - 1 Do
    If CompareText(strSpecialTag, Comment.Tag[i].TagName) = 0 Then
      Begin
        Result := True;
        Exit;
      End;
End;

(**

  This is the constructor method for the TBADICustomHintWindow class.

  @precon  ATreeView must be a valid instance of the explorer treeview.
  @postcon Ensures the hint can get the tree views font information.

  @param   AOwner    as a TComponent as a constant
  @param   ATreeView as a TVirtualStringTree as a constant

**)
Constructor TBADICustomHintWindow.Create(Const AOwner: TComponent; Const ATreeView: TVirtualStringTree);

Begin
  Inherited Create(AOwner);
  FTreeView := ATreeView;
  FBADIOptions := TBADIOptions.BADIOptions;
End;

(**

  This method is the paint method for the customised hint window.

  @precon  None.
  @postcon Draws the custom hint window.

**)
Procedure TBADICustomHintWindow.Paint;

Var
  iBottom : Integer;
  NodeData : PBADITreeData;

Begin
  FRect := Rect(0, 0, Width - 1, Height - 1);
  InflateRect(FRect, -1 - iPadding, -1 - iPadding);
  iBottom := 0;
  Canvas.Font.Assign(FTreeView.Font);
  NodeData := FTreeView.GetNodeData(FNode);
  PaintRenderTreeNode(NodeData, iBottom);
  If (FComment <> Nil) And ((FComment.TokenCount > 0) Or (FComment.TagCount > 0)) Then
    Begin
      PaintRenderComment(iBottom);
      PaintRenderSpecialTags(iBottom);
    End;
End;

(**

  This method renders the comment associated with the tree node.

  @precon  None.
  @postcon The comment is rendered below the tree node text.

  @param   iBottom as an Integer as a reference

**)
Procedure TBADICustomHintWindow.PaintRenderComment(Var iBottom : Integer);

Const
  iMultipler = 2;

Var
  iTop :  Integer;
  iHeight : Integer;
  strText : String;
  R: TRect;

Begin
  iTop := iBottom + iPadding + 1; // plus 1 for width of line
  Inc(iBottom, iPadding + 1);
  Canvas.Pen.Color := clBlack;
  Canvas.MoveTo(0, iTop);
  Canvas.Lineto(Width, iTop);
  Inc(iTop, 1 + iPadding);
  Inc(iBottom, 1 + iPadding);
  Canvas.Refresh;
  SetTokenFont(Canvas, ttPlainText);
  strText := FComment.AsString(MaxInt, False);
  R := Rect(FRect.Left, iTop, Width - iPadding * iMultipler, Height);
  iHeight := DrawText(Canvas.Handle, PChar(strText), -1, R,
    DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
    DrawTextBiDiModeFlagsReadingOnly);
  Inc(iBottom, iHeight);
  Inc(iBottom, iPadding);
End;

(**

  This method renders the list of special tags at the bottom of the hint window.

  @precon  None.
  @postcon The special tags associated with the tree node are drawn at the bottom of the hint
           window.

  @param   iBottom as an Integer as a constant

**)
Procedure TBADICustomHintWindow.PaintRenderSpecialTags(Const iBottom : Integer);

Const
  iDivisor = 2;
  iMultipler = 2;

Var
  iSpecialTag: Integer;
  R : TRect;
  iTag: Integer;
  strText : String;
  S: TRect;
  TokenFontInfo: TBADITokenFontInfoTokenSet;

Begin
  R := FRect;
  R.Top := iBottom;
  TokenFontInfo := FBADIOptions.TokenFontInfo[FBADIOptions.UseIDEEditorColours];
  For iSpecialTag := 0 To FBADIOptions.SpecialTags.Count - 1 Do
    Begin
      If CanDrawSpecialTag(FComment, FBADIOptions.SpecialTags[iSpecialTag].FName) Then
        Begin
          Canvas.Refresh;
          SetTokenFont(Canvas, ttTreeHeader);
          Inc(R.Top, 1 + iPadding);
          strText := FBADIOptions.SpecialTags[iSpecialTag].FDescription;
          Inc(R.Top, DrawText(Canvas.Handle, PChar(strText), -1, R,
            DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
            DrawTextBiDiModeFlagsReadingOnly) + 1);
          Inc(R.Top, 1 + iPadding);
          For iTag := 0 To FComment.TagCount - 1 Do
            If CompareText(FBADIOptions.SpecialTags[iSpecialTag].FName,
              FComment.Tag[iTag].TagName) = 0 Then
              Begin
                SetTokenFont(Canvas, ttTagText);
                InitCanvasFont(Canvas, FComment.Tag[iTag].Fixed, FBADIOptions);
                Canvas.Pen.Color := clBlack;
                Canvas.Brush.Color := clBlack;
                Canvas.Ellipse(
                  iBulletSize Div iDivisor,
                  R.Top + iBulletSize Div iDivisor,
                  iBulletSize * iMultipler - iBulletSize Div iDivisor,
                  R.Top + iBulletSize * iMultipler - iBulletSize Div iDivisor
                );
                Canvas.Brush.Color := TokenFontInfo[ttExplorerHighlight].FBackColour;
                Canvas.Refresh;
                S := R;
                S.Left := iBulletSize * iMultipler;
                strText := FComment.Tag[iTag].AsString(MaxInt, False);
                Inc(R.Top, DrawText(Canvas.Handle, PChar(strText), -1, S,
                  DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
                  DrawTextBiDiModeFlagsReadingOnly));
              End;
        End;
    End;
End;

(**

  This method renders the text associated with the tree node in the hint window.

  @precon  NodeData must be a valid pointer to tree node data.
  @postcon The tree node text is rendered.

  @param   NodeData as a PBADITreeData as a constant
  @param   iBottom  as an Integer as a reference

**)
Procedure TBADICustomHintWindow.PaintRenderTreeNode(Const NodeData : PBADITreeData; Var iBottom : Integer);

Const
  strTextheightTest = 'Ag';

Var
  sl : TStringList;
  i: Integer;
  iLines : Integer;
  S: TRect;
  R : TRect;
  iLeft : Integer;
  iTop : Integer;
  TokenFontInfo: TBADITokenFontInfoTokenSet;
  iBGColour: TColor;

Begin
  iLines := 1;
  R := BoundsRect;
  If FCustomDraw Then
    Begin
      iLeft := FRect.Left;
      iTop := FRect.Top;
      InitCanvasFont(Canvas, tpFixed In NodeData.FNode.TagProperties, FBADIOptions);
      TokenFontInfo := FBADIOptions.TokenFontInfo[FBADIOptions.UseIDEEditorColours];
      iBGColour := FBADIOptions.BGColour[FBADIOptions.UseIDEEditorColours];
      If iBGColour = clNone Then
        iBGColour := FBADIOptions.BGColour[False];
      sl := NodeData.FNode.Tokens;
      For i := 0 To sl.Count - 1 Do
        Begin
          GetFontInfo(sl, i, FTitle, tpSyntax In NodeData.FNode.TagProperties, NodeData.FNode.ForeColour,
            NodeData.FNode.BackColour, NodeData.FNode.FontStyles, TokenFontInfo, iBGColour, Canvas);
          If Canvas.Brush.Color = iBGColour Then
            Canvas.Brush.Color := TokenFontInfo[ttExplorerHighlight].FBackColour;
          If (sl[i] = #13#10) Or (iLeft + Canvas.TextWidth(sl[i]) > Width) Then
            Begin
              iLeft := FRect.Left;
              Inc(iLines);
              Inc(iTop, Canvas.TextHeight(strTextheightTest));
              If sl[i] = #13#10 Then
                Continue;
            End;
          iBottom := FRect.Top + iLines * Canvas.TextHeight(strTextheightTest);
          S := Rect(iLeft, iTop, R.Right, R.Bottom);
          DrawText(Canvas.Handle, PChar(sl[i]), Length(sl[i]), S, DT_LEFT Or DT_VCENTER);
          Inc(iLeft, Canvas.TextWidth(sl[i]));
        End;
    End Else
    Begin
      S := FRect;
      InitCanvasFont(Canvas, tpFixed In NodeData.FNode.TagProperties, FBADIOptions);
      SetTokenFont(Canvas, ttPlainText);
      Inc(iBottom, DrawText(Canvas.Handle, PChar(NodeData.FNode.Text), Length(NodeData.FNode.Text), S,
        DT_LEFT Or DT_VCENTER));
    End;
End;

(**

  This method sets the font of the give canvas based on the token required.

  @precon  Canvas must be a valid instance.
  @postcon The canvas font is configure for the given token.

  @param   Canvas     as a TCanvas as a constant
  @param   eTokenType as a TBADITokenType as a constant

**)
Procedure TBADICustomHintWindow.SetTokenFont(Const Canvas : TCanvas; Const eTokenType: TBADITokenType);

Var
  TokenFontInfo: TBADITokenFontInfoTokenSet;
  iBGColour: TColor;

Begin
  TokenFontInfo := FBADIOptions.TokenFontInfo[FBADIOptions.UseIDEEditorColours];
  iBGColour := FBADIOptions.BGColour[FBADIOptions.UseIDEEditorColours];
  Canvas.Font.Name := FBADIOptions.TreeFontName;
  Canvas.Font.Size := FBADIOptions.TreeFontSize;
  Canvas.Font.Color := TokenFontInfo[eTokenType].FForeColour;
  Canvas.Font.Style := TokenFontInfo[eTokenType].FStyles;
  Canvas.Brush.Color := TokenFontInfo[eTokenType].FBackColour;
  If (Canvas.Brush.Color = clNone) Or (Canvas.Brush.Color = iBGColour) Then
    Canvas.Brush.Color := TokenFontInfo[ttExplorerHighlight].FBackColour;
End;

End.
