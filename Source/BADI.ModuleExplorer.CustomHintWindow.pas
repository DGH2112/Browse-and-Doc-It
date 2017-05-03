(**

  This module contains the custom hint window for displaying information about the module explorer
  tree nodes.

  @Author  David Hoyle
  @Version 1.0
  @Date    03 May 2017

**)
Unit BADI.ModuleExplorer.CustomHintWindow;

Interface

Uses
  Controls,
  BADI.Comment,
  BADI.Types,
  VirtualTrees,
  Classes,
  Windows,
  Graphics,
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
      (** This is the size of the bullett in the special tags list. **)
      iBulletSize = 8;
  Strict Private
    FComment    : TComment;
    FNodeLevel  : Integer;
    FTitle      : Boolean;
    FCustomDraw : Boolean;
    FTreeView   : TVirtualStringTree;
    FNode       : PVirtualNode;
    FRect       : TRect;
  Strict Protected
    Procedure CalcTreeNodeWidth(Const SyntaxHighlight : Boolean; Const Node : PVirtualNode;
      Var Result : TRect); InLine;
    Procedure CalcSpecialTags(Const Comment : TComment; Var Result : TRect); InLine;
    Procedure CalcSpecialTag(Const iSpecialTag : Integer; Const Comment : TComment;
      Var Result : TRect); InLine;
    Procedure SetTokenFont(Const Canvas : TCanvas; Const eTokenType : TBADITokenType);
    Procedure PaintRenderTreeNode(NodeData : PBADITreeData; var iBottom : Integer); InLine;
    Procedure PaintRenderComment(Var iBottom : Integer); InLine;
    Procedure PaintRenderSpecialTags(Const iBottom : Integer); InLine;
  Public
    Constructor Create(AOwner : TComponent; ATreeView : TVirtualStringTree); ReIntroduce;
    Procedure Paint; Override;
    Function CalcHintRect(MinWidth, MaxWidth : Integer; Node : PVirtualNode;
      SyntaxHighlight : Boolean; Comment : TComment) : TRect; Reintroduce; Overload;
    Procedure ActivateHint(Rect : TRect; Node : PVirtualNode;
      SyntaxHighlight : Boolean; Comment : TComment); Reintroduce; Overload;
    Function CanDrawSpecialTag(Comment : TComment; const strSpecialTag : String) : Boolean;
  End;

Implementation

Uses
  //CodeSiteLogging, //: @debug Remove CodeSite
  BADI.Options,
  Forms,
  BADI.Functions,
  SysUtils;

(**

  This method is an overridden ActivateHint method to allow for the passing of
  more information to the hint windows internal routines.

  @precon  Rect is the rectangle to display the hint in, Node is the tree node
           to create a hint for, SyntaxHighlight tells the routine to print
           either a plain text representation of the information or one with
           syntax highlighing and Comment is the comment associated with the
           tree node.
  @postcon Activates the hint window.

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
  Color := TBADIOptions.BADIOptions.TokenFontInfo[ttExplorerHighlight].FBackColour;
  ActivateHint(Rect, NodeData.FNode.Text);
End;

(**

  This method calcalates the size of the hint window based on the tree node
  display window, a max and min width , whether to display syntax highlighting
  and a comment.

  @precon  MinWidth is the minimum width of the hint window to be displayed,
           MaxWidth is the minimum width of the hint window to be displayed,
           Node is the tree node to calculate the hint window for,
           SyntaxHighlight tells the routine to print either a plain text
           representation of the information or one with syntax highlighing,
           and Comment is the comment associated with the tree node and
  @postcon Returns the newly calculated rectangle of the hint window.

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
      InitCanvasFont(Canvas, False);
      SetTokenFont(Canvas, ttCommentText);
      strText := Comment.AsString(MaxInt, False);
      R := Rect(Result.Left + iPadding, 0, Result.Right - iPadding, 0);
      Inc(Result.Bottom, DrawText(Canvas.Handle, PChar(strText), -1, R,
        DT_CALCRECT Or DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
        DrawTextBiDiModeFlagsReadingOnly) + 2);
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

Var
  iTag: Integer;
  R: TRect;
  strText: String;

Begin
  For iTag := 0 To Comment.TagCount - 1 Do
    If CompareText(TBADIOptions.BADIOptions.SpecialTags[iSpecialTag].FName,
      Comment.Tag[iTag].TagName) = 0 Then
      Begin
        Refresh;
        InitCanvasFont(Canvas, Comment.Tag[iTag].Fixed);
        SetTokenFont(Canvas, ttTagHeaderText);
        R := Rect(Result.Left + iPadding, 0, Result.Right - iPadding - 2 * iBulletSize, 0);
        strText := Comment.Tag[iTag].AsString(MaxInt, False);
        Inc(Result.Bottom, DrawText(Canvas.Handle, PChar(strText), -1, R,
          DT_CALCRECT Or DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
          DrawTextBiDiModeFlagsReadingOnly) + 2);
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
  For iSpecialTag := 0 To TBADIOptions.BADIOptions.SpecialTags.Count - 1 Do
    Begin
      If CanDrawSpecialTag(Comment, TBADIOptions.BADIOptions.SpecialTags[iSpecialTag].FName) Then
        Begin
          Refresh;
          InitCanvasFont(Canvas, False);
          SetTokenFont(Canvas, ttTagHeaderText);
          R := Rect(Result.Left + iPadding, 0, Result.Right - iPadding, 0);
          strText := TBADIOptions.BADIOptions.SpecialTags[iSpecialTag].FDescription;
          Inc(Result.Bottom, DrawText(Canvas.Handle, PChar(strText), -1, R,
            DT_CALCRECT Or DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
            DrawTextBiDiModeFlagsReadingOnly) + 2);
          Inc(Result.Bottom, 1 + iPadding);
          CalcSpecialTag(iSpecialTag, Comment, Result);
        End;
    End;
End;

(**

  This method calculates the width of the hint window to acommodate the tree nodes text and comments
  .

  @precon  None.
  @postcon The rectangle for the hint window is updated.

  @param   SyntaxHighlight as a Boolean as a constant
  @param   Node            as a PVirtualNode as a constant
  @param   Result          as a TRect as a reference

**)
Procedure TBADICustomHintWindow.CalcTreeNodeWidth(Const SyntaxHighlight : Boolean;
  Const Node : PVirtualNode; Var Result : TRect);

Var
  NodeData : PBADITreeData;
  iPos : Integer;
  sl: TStringList;
  iLastmax: Integer;
  iToken: Integer;
  iMaxPos : Integer;

Begin
  iMaxPos := FTreeView.ScreenToClient(Point(Screen.WorkAreaRect.Right, 0)).X - Result.Left;
  If SyntaxHighlight Then
    Begin
      // Need to amend the width of the rectangle for the custom drawing
      iPos := iPadding;
      NodeData := FTreeView.GetNodeData(Node);
      sl := NodeData.FNode.Tokens;
      iLastmax := 0;
      InitCanvasFont(Canvas, tpFixed In NodeData.FNode.TagProperties);
      // Start with single line height
      Result.Bottom := Result.Top + iPadding * 2 + Canvas.TextHeight('Ag');
      For iToken := 0 To sl.Count - 1 Do
        Begin
          GetFontInfo(sl, iToken, NodeData.FNode.Title, tpSyntax In NodeData.FNode.TagProperties,
            Canvas);
          Inc(iPos, Canvas.TextWidth(sl[iToken]));
          If (iPos > iMaxPos) Or (sl[iToken] = #13#10) Then
            Begin
              Inc(Result.Bottom, Canvas.TextHeight(sl[iToken]));
              iPos := iPadding;
              If sl[iToken] <> #13#10 Then
                Begin
                  Inc(iPos, 10); // indent multiple lines
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

  @precon  Comment is the comment to get tags from and strSpecialTag is the special tag information
           to look for.
  @postcon Return true is the special tag was found in the comment.

  @param   Comment       as a TComment
  @param   strSpecialTag as a String as a constant
  @return  a Boolean

**)
Function TBADICustomHintWindow.CanDrawSpecialTag(Comment : TComment;
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
  @postcon Ensures the hint can get the treeviews font information.

  @param   AOwner    as a TComponent
  @param   ATreeView as a TVirtualStringTree

**)
Constructor TBADICustomHintWindow.Create(AOwner: TComponent; ATreeView: TVirtualStringTree);

Begin
  Inherited Create(AOwner);
  FTreeView := ATreeView;
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
  @postcon The comment is rendered belwo the tree node text.

  @param   iBottom as an Integer as a reference

**)
Procedure TBADICustomHintWindow.PaintRenderComment(Var iBottom : Integer);

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
  SetTokenFont(Canvas, ttCommentText);
  strText := FComment.AsString(MaxInt, False);
  R := Rect(FRect.Left, iTop, Width - iPadding * 2, Height);
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

Var
  iSpecialTag: Integer;
  R : TRect;
  iTag: Integer;
  strText : String;
  S: TRect;

Begin
  R := FRect;
  R.Top := iBottom;
  For iSpecialTag := 0 To TBADIOptions.BADIOptions.SpecialTags.Count - 1 Do
    Begin
      If CanDrawSpecialTag(FComment, TBADIOptions.BADIOptions.SpecialTags[iSpecialTag].FName) Then
        Begin
          Canvas.Refresh;
          SetTokenFont(Canvas, ttTagHeaderText);
          Inc(R.Top, 1 + iPadding);
          strText := TBADIOptions.BADIOptions.SpecialTags[iSpecialTag].FDescription;
          Inc(R.Top, DrawText(Canvas.Handle, PChar(strText), -1, R,
            DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
            DrawTextBiDiModeFlagsReadingOnly) + 1);
          Inc(R.Top, 1 + iPadding);
          For iTag := 0 To FComment.TagCount - 1 Do
            If CompareText(TBADIOptions.BADIOptions.SpecialTags[iSpecialTag].FName,
              FComment.Tag[iTag].TagName) = 0 Then
              Begin
                SetTokenFont(Canvas, ttTagText);
                InitCanvasFont(Canvas, FComment.Tag[iTag].Fixed);
                Canvas.Pen.Color := clBlack;
                Canvas.Brush.Color := clBlack;
                Canvas.Ellipse(
                  iBulletSize Div 2,
                  R.Top + iBulletSize Div 2,
                  iBulletSize * 2 - iBulletSize Div 2,
                  R.Top + iBulletSize * 2 - iBulletSize Div 2
                );
                Canvas.Brush.Color :=
                  TBADIOptions.BADIOptions.TokenFontInfo[ttExplorerHighlight].FBackColour;
                Canvas.Refresh;
                S := R;
                S.Left := iBulletSize * 2;
                strText := FComment.Tag[iTag].AsString(MaxInt, False);
                Inc(R.Top, DrawText(Canvas.Handle, PChar(strText), -1, S,
                  DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
                  DrawTextBiDiModeFlagsReadingOnly));
              End;
        End;
    End;
End;

(**

  This method rennders the text associated with the tree node in the hint window.

  @precon  NodeData must be a valid pointer to tree node data.
  @postcon The treenode text is rendered.

  @param   NodeData as a PBADITreeData
  @param   iBottom  as an Integer as a reference

**)
Procedure TBADICustomHintWindow.PaintRenderTreeNode(NodeData : PBADITreeData; Var iBottom : Integer);

Var
  sl : TStringList;
  i: Integer;
  iLines : Integer;
  S: TRect;
  R : TRect;
  iLeft : Integer;
  iTop : Integer;

Begin
  iLines := 1;
  R := BoundsRect;
  If FCustomDraw Then
    Begin
      iLeft := FRect.Left;
      iTop := FRect.Top;
      InitCanvasFont(Canvas, tpFixed In NodeData.FNode.TagProperties);
      sl := NodeData.FNode.Tokens;
      For i := 0 To sl.Count - 1 Do
        Begin
          GetFontInfo(sl, i, FTitle, tpSyntax In NodeData.FNode.TagProperties, Canvas);
          If Canvas.Brush.Color = TBADIOptions.BADIOptions.BGColour Then
            Canvas.Brush.Color :=
              TBADIOptions.BADIOptions.TokenFontInfo[ttExplorerHighlight].FBackColour;
          If (sl[i] = #13#10) Or (iLeft + Canvas.TextWidth(sl[i]) > Width) Then
            Begin
              iLeft := FRect.Left;
              Inc(iLines);
              Inc(iTop, Canvas.TextHeight('Ag'));
              If sl[i] = #13#10 Then
                Continue;
            End;
          iBottom := FRect.Top + iLines * Canvas.TextHeight('Wg');
          S := Rect(iLeft, iTop, R.Right, R.Bottom);
          DrawText(Canvas.Handle, PChar(sl[i]), Length(sl[i]), S, DT_LEFT Or DT_VCENTER);
          Inc(iLeft, Canvas.TextWidth(sl[i]));
        End;
    End Else
    Begin
      S := FRect;
      InitCanvasFont(Canvas, tpFixed In NodeData.FNode.TagProperties);
      SetTokenFont(Canvas, ttPlainText);
      Inc(iBottom, DrawText(Canvas.Handle, PChar(NodeData.FNode.Text), Length(NodeData.FNode.Text), S,
        DT_LEFT Or DT_VCENTER));
    End;
End;

(**

  This method sets the font of the give canvas based on the token required.

  @precon  Canvas must be a valid TCanvas instance.
  @postcon The canvas font is configure for the given token.

  @param   Canvas     as a TCanvas as a constant
  @param   eTokenType as a TBADITokenType as a constant

**)
Procedure TBADICustomHintWindow.SetTokenFont(Const Canvas : TCanvas; Const eTokenType: TBADITokenType);

Var
  Ops : TBADIOptions;

Begin
  Ops := TBADIOptions.BADIOptions;
  Canvas.Font.Name := Ops.TreeFontName;
  Canvas.Font.Size := Ops.TreeFontSize;
  Canvas.Font.Color := Ops.TokenFontInfo[eTokenType].FForeColour;
  Canvas.Font.Style := Ops.TokenFontInfo[eTokenType].FStyles;
  Canvas.Brush.Color := Ops.TokenFontInfo[eTokenType].FBackColour;
  If (Canvas.Brush.Color = clNone) Or (Canvas.Brush.Color = Ops.BGColour) Then
    Canvas.Brush.Color := Ops.TokenFontInfo[ttExplorerHighlight].FBackColour;
End;

End.
