(**

  This module contains a frame which holds all the functionality of the
  module browser so that it can be independant of the application specifics.

  @Date    01 Apr 2017
  @Author  David Hoyle
  @Version 1.0

**)
Unit BADI.ModuleExplorerFrame;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ImgList,
  ComCtrls,
  ExtCtrls,
  Contnrs,
  BADI.Base.Module,
  ActnList,
  ToolWin,
  VirtualTrees,
  System.Actions,
  ImageList,
  Vcl.StdCtrls,
  RegularExpressions,
  BADI.Comment,
  BADI.ElementContainer;

{$INCLUDE CompilerDefinitions.Inc}

Type
  (** This class represents information about each nodes. Used in a collection
      instead of hanging the data off the tree nodes.  **)
  TTreeNodeInfo = Class
  {$IFDEF D2005} Strict {$ENDIF} Private
    FText       : String;
    FName       : String;
    FLine       : Integer;
    FCol        : Integer;
    FComment    : TComment;
    FImageIndex : Integer;
    FTokens     : TStringList;
    FLevel      : Integer;
    FTitle      : Boolean;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    function GetTokens: TStringList;
  Public
    Constructor Create(const strText, strName : String; iLevel : Integer; iImageIndex : Integer;
      iLine : Integer = 0; iColumn : Integer = 0; boolTitle : Boolean = False;
      AComment : TComment = Nil); Overload;
    Destructor Destroy; Override;
    (**
      This property returns the text representation of the tree element.
      @precon  None.
      @postcon Returns the text representation of the tree element.
      @return  a String
    **)
    Property Text : String Read FText;
    (**
      This property returns the Name of the tree element.
      @precon  None.
      @postcon Returns the Name of the tree element.
      @return  a String
    **)
    Property Name : String Read FName;
    (**
      This property returns the line number of the tree node comment.
      @precon  None.
      @postcon Returns the line associated with the node info structure.
      @return  an Integer
     **)
    Property Line : Integer Read FLine;
    (**
      This property returns the column number of the tree node comment.
      @precon  None.
      @postcon Returns the column associated with the node info structure.
      @return  an Integer
     **)
    Property Col : Integer Read FCol;
    (**
      This property returns the comment associated with the tree node info.
      @precon  None.
      @postcon Returns the comment associated with the node info structure.
      @return  a TComment
     **)
    Property Comment : TComment Read FComment;
    (**
      This property returns the integer image index of the tree element.
      @precon  None.
      @postcon Returns the integer image index of the tree element.
      @return  an Integer
    **)
    Property ImageIndex : Integer Read FImageIndex;
    (**
      This property returns a token list representing the text of this element.
      @precon  None.
      @postcon Returns a token list representing the text of this element.
      @return  a TStringList
    **)
    Property Tokens : TStringList Read GetTokens;
    (**
      This property provides information on the level within the treeview that
      this item represents.
      @precon  None.
      @postcon Provides information on the level within the treeview that
               this item represents.
      @return  an Integer
    **)
    Property Level : Integer Read FLevel;
    (**
      This property provides information on whether the items it a label or not.
      @precon  None.
      @postcon Provides information on whether the items it a label or not.
      @return  a Boolean
    **)
    Property Title : Boolean Read FTitle;
  End;

  (** This record contains information about the special tag nodes **)
  TSpecialTagNode = Record
    Node       : PVirtualNode;
    strTagName : String;
    strTagDesc : String;
    boolShow   : Boolean;
    boolExpand : Boolean;
  End;

  (** This is a custom hint window for displaying hints about the tree view
      items. Its customisation support the syntax highlighting custom draw
      support of the tree view.
   **)
  TCustomHintWindow = Class(THintWindow)
  {$IFDEF D2005} Strict {$ENDIF} Private
    FComment    : TComment;
    FNodeLevel  : Integer;
    FTitle      : Boolean;
    FCustomDraw : Boolean;
    FTreeView   : TVirtualStringTree;
    FNode       : PVirtualNode;
  Public
    Constructor Create(AOwner : TComponent; ATreeView : TVirtualStringTree); ReIntroduce;
    Procedure Paint; Override;
    Function CalcHintRect(MinWidth, MaxWidth : Integer; Node : PVirtualNode;
      SyntaxHighlight : Boolean; Comment : TComment) : TRect; Reintroduce; Overload;
    Procedure ActivateHint(Rect : TRect; Node : PVirtualNode;
      SyntaxHighlight : Boolean; Comment : TComment); Reintroduce; Overload;
    Function CanDrawSpecialTag(Comment : TComment; const strSpecialTag : String) : Boolean;
  End;

  (** This is a descendant of the TVirtualStringTree in order to override the
      OnGetNodeWidth method. **)
  TBADIVirtualStringTree = Class(TVirtualStringTree)
  {$IFDEF D2005} Strict {$ENDIF} Protected
    Function DoGetNodeWidth(Node: PVirtualNode; Column: TColumnIndex;
      Canvas: TCanvas = Nil) : Integer; Override;
  End;

  (** This is a procedure type for the positioning of the cursor in the
      current module. **)
  TSelectionChange = Procedure(iIdentLine, iIdentCol, iCommentLine,
    iCommentCol : Integer) Of Object;

  (** An enumerate to define the panels on the status bar. **)
  TStatusPanelIndex = (
    spiBytes,
    spiTokens,
    spiLines,
    spiFourth
  );

  (** This is a frame class to contain all the functionality of the module
      explorer so that it can be placed inside any container required and
      therefore does not need to know about things like BDS 2006 IDEs or
      application specifics. **)
  TframeModuleExplorer = class(TFrame)
    stbStatusBar: TStatusBar;
    ilScopeImages: TImageList;
    tbrExplorerScope: TToolBar;
    ilToolbar: TImageList;
    alToolbar: TActionList;
    actLocal: TAction;
    actPrivate: TAction;
    actProtected: TAction;
    actPublic: TAction;
    actPublished: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    tbtnSeparator: TToolButton;
    tbtnSyntaxHighlight: TToolButton;
    actSyntax: TAction;
    actShowHints: TAction;
    actConflicts: TAction;
    actErrors: TAction;
    actWarnings: TAction;
    actHints: TAction;
    actMethods: TAction;
    actProperties: TAction;
    tbtnShowHints: TToolButton;
    tbtnConflicts: TToolButton;
    tbtnErrors: TToolButton;
    tbtnWarnings: TToolButton;
    tbtnHints: TToolButton;
    tbtnMethods: TToolButton;
    tbtnProperties: TToolButton;
    tbtnSep2: TToolButton;
    tbtnSep3: TToolButton;
    tbtnConstants: TToolButton;
    tbtnVariables: TToolButton;
    tbtnTypes: TToolButton;
    actConstants: TAction;
    actVariables: TAction;
    actTypes: TAction;
    edtExplorerFilter: TEdit;
    procedure tvExplorerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure tvExplorerClick(Sender: TObject);
    procedure actLocalUpdate(Sender: TObject);
    procedure actLocalExecute(Sender: TObject);
    procedure tvExplorerKeyPress(Sender: TObject; var Key: Char);
    {$IFNDEF D2009}
    procedure tvExplorerGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    {$ELSE}
    procedure tvExplorerGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    {$ENDIF}
    procedure tvExplorerGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvExplorerBeforeItemPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
      var CustomDraw: Boolean);
    procedure tvExplorerMeasureItem(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
    procedure FilterChange;
    procedure FrameEnter(Sender: TObject);
    procedure edtExplorerFilterChange(Sender: TObject);
    procedure edtExplorerFilterMouseActivate(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y, HitTest: Integer; var MouseActivate: TMouseActivate);
    procedure edtExplorerFilterKeyPress(Sender: TObject; var Key: Char);
  {$IFDEF D2005} Strict {$ENDIF} private
    { Private declarations }
    FModule : PVirtualNode;
    FNodeInfo : TObjectList;
    FSelectionChange : TSelectionChange;
    FFocus : TNotifyEvent;
    FSpecialTagNodes : Array Of TSpecialTagNode;
    FHintWin : TCustomHintWindow;
    FLastNode : PVirtualNode;
    FINIFileName: String;
    FSelectionChanging: Boolean;
    FRendering: Boolean;
    FRefresh : TNotifyEvent;
    FExplorer : TBADIVirtualStringTree;
    FFilterRegEx : TRegEx;
    FMouseEnter : Boolean;
    { Private declarations }
    procedure GetBodyCommentTags(M : TBaseLanguageModule);
    Function AddNode(P : PVirtualNode; const strText, strName : String; iLevel : Integer;
      iImageIndex : Integer; iLine : Integer = 0; iColumn : Integer = 0;
      boolTitle : Boolean = False; AComment : TComment = Nil) : PVirtualNode;
    procedure CreateSpecialTagNodes(M : TBaseLanguageModule);
    procedure ExpandNodes;
    procedure OutputModuleInfo(Container : TElementContainer);
    function FindTreeItem(const strText: String): PVirtualNode;
    procedure GetExpandedNodes(StartNode : PVirtualNode);
    function GetNodePath(Node: PVirtualNode): String;
    procedure SetExpandedNodes(StartNode : PVirtualNode);
    Procedure RenderContainers(RootNode : PVirtualNode;
      Container : TElementContainer; iLevel : Integer);
    Procedure UpdateStatusBar(M : TBaseLanguageModule);
    Procedure ManageExpandedNodes;
    Procedure FilterProc(Sender: TBaseVirtualTree; Node: PVirtualNode; Data: Pointer;
      Var Abort: Boolean);
    Procedure SetStatusPanel(ePanel: TStatusPanelIndex; const strStatusBarText: string;
      iValue: Integer);
    Procedure LoadBADIImages;
    {$IFDEF D2005} Strict {$ENDIF} Protected
    Procedure CMMouseLeave(var Msg : TMessage); Message CM_MOUSELEAVE;
  public
    { Public declarations }
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
    procedure RenderModule(M : TBaseLanguageModule);
    (**
      This is an event for the selection change in the browser tree.
      @precon  None.
      @postcon Hooks an event handler for the On Selection change event.
      @return  a TSelectionChange
    **)
    Property OnSelectionChange : TSelectionChange Read FSelectionChange
      Write FSelectionChange;
    (**
      This is an event for the on focus change in the browser.
      @precon  None.
      @postcon Hooks an event handler for the On Focus change event.
      @return  a TNotifyEvent
    **)
    Property OnFocus : TNotifyEvent Read FFocus Write FFocus;
    (**
      This is an event handler for the On Options Change event.
      @precon  None.
      @postcon Hooks an event handler for the change of options event.
      @return  a TNotifyEvent
    **)
    Property OnRefresh : TNotifyEvent Read FRefresh Write FRefresh;
    (**
      This property exposes the virtual tree view to ouside sources.
      @precon  None.
      @postcon Exposes the virtual tree view to ouside sources.
      @return  a TBADIVirtualStringTree
    **)
    Property Explorer : TBADIVirtualStringTree Read FExplorer;
  end;

implementation

Uses
  IniFiles,
  Types,
  Math,
  DGHLibrary,
  BADI.Generic.Tokenizer,
  UITypes,
  RegularExpressionsCore,
  BADI.Types,
  BADI.Options,
  BADI.Constants,
  BADI.ResourceStrings,
  BADI.Functions;

Type
  (** This record described the data sorted in the virtual tree view. **)
  TTreeData = Record
    FNode : TTreeNodeInfo;
  End;
  (** A type to define a pointer to the above tree record. **)
  PTreeData = ^TTreeData;

Resourcestring
  (** A format pattern for the bytes statusbar text. **)
  strStatusbarBytesText = '%1.0n Bytes';
  (** A format pattern for the tokens statusbar text. **)
  strStatusbarTokensText = '%1.0n Tokens';
  (** A format pattern for the lines status bar panel. **)
  strStatusbarLinesText = '%1.0n Lines.';

{$R *.dfm}

Var
  (** A private variable which is assigned the key words array. **)
  strReservedWords, strDirectives : TKeyWords;

(**

  This procedure sets the font of the passed canvas to the appropiate style and
  colour for the words stored in the string list.

  @precon  sl is a string list of the tokenized word to display, i is the index
           of the word to change the canvas for, Level is the current
           indentation level of the tree node and Canvas is the canvas to be
           affected but the other parameters.
  @postcon Sets the font of the passed canvas to the appropiate style and
           colour for the words stored in the string list.

  @param   sl        as a TStringList
  @param   i         as an Integer
  @param   boolTitle as a Boolean
  @param   Canvas    as a TCanvas

**)
Procedure GetFontInfo(sl : TStringList; i : Integer; boolTitle : Boolean;
  Canvas : TCanvas);

Begin
  With TBADIOptions.BADIOptions Do
    Begin
      If Not boolTitle Then
        Begin
          Canvas.Font.Color := TokenFontInfo[TBADITokenType(sl.Objects[i])].FForeColour;
          Canvas.Font.Style := TokenFontInfo[TBADITokenType(sl.Objects[i])].FStyles;
          Canvas.Brush.Color := TokenFontInfo[TBADITokenType(sl.Objects[i])].FBackColour;
        End Else
        Begin
          Canvas.Font.Color := TokenFontInfo[ttTreeHeader].FForeColour;
          Canvas.Font.Style := TokenFontInfo[ttTreeHeader].FStyles;
          Canvas.Brush.Color := TokenFontInfo[ttTreeHeader].FBackColour;
        End;
      If Canvas.Brush.Color = clNone Then
        Canvas.Brush.Color := BGColour;
    End;
End;

(**

  This is the constructor method for the TTreeNodeInfo class.

  @precon  This line number of the comment, This column number of the comment
           and This comment to be added to the node info object.
  @postcon Initialises the class.

  @param   strText     as a String as a Constant
  @param   strName     as a String as a Constant
  @param   iLevel      as an Integer
  @param   iImageIndex as an Integer
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   boolTitle   as a Boolean
  @param   AComment    as a TComment

**)
Constructor TTreeNodeInfo.Create(const strText, strName : String; iLevel : Integer;
  iImageIndex : Integer; iLine : Integer = 0; iColumn : Integer = 0;
  boolTitle : Boolean = False; AComment : TComment = Nil);

Begin
  Inherited Create;
  FText := strText;
  FName  := strName;
  FLevel := iLevel;
  FTitle := boolTitle;
  FLine := iLine;
  FCol := iColumn;
  FComment := TComment.Create(AComment);
  FImageIndex:= iImageIndex;
  FTokens := Nil;
End;

(**

  This is the destructor method for the TTreeNodeInfo class.

  @precon  None.
  @postcon Destroys the instance of the Tree node info class.

 **)
Destructor TTreeNodeInfo.Destroy;

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
function TTreeNodeInfo.GetTokens: TStringList;
begin
  If FTokens = Nil Then
    FTokens := Tokenize(FText, strReservedWords, strDirectives,
      TBADIOptions.BADIOptions.TokenLimit);
  Result := FTokens;
end;

(**

  This method is the paint method for the customised hint window.

  @precon  None.
  @postcon Draws the custom hint window.

**)
Procedure TCustomHintWindow.Paint;

  (**

    This method rennders the text associated with the tree node in the hint window.

    @precon  NodeData must be a valid pointer to tree node data.
    @postcon The treenode text is rendered.

    @param   NodeData as a PTreeData
    @param   iPos     as an Integer as a reference
    @param   iLine    as an Integer as a reference

  **)
  Procedure RenderTreeNode(NodeData : PTreeData; var iPos, iLine : Integer);

  Var
    sl : TStringList;
    i: Integer;
    iLines : Integer;
    S: TRect;
    R : TRect;

  Begin
    iLines := 1;
    R := BoundsRect;
    If FCustomDraw Then
      Begin
        sl := NodeData.FNode.Tokens;
        For i := 0 To sl.Count - 1 Do
          Begin
            GetFontInfo(sl, i, FTitle, Canvas);
            If Canvas.Brush.Color = TBADIOptions.BADIOptions.BGColour Then
              Canvas.Brush.Color :=
                TBADIOptions.BADIOptions.TokenFontInfo[ttExplorerHighlight].FBackColour;
            If iPos + Canvas.TextWidth(sl[i]) > Width Then
              Begin
                iPos := 2 + 10; // Indent multiple lines
                Inc(iLines);
              End;
            iLine := (iLines - 1) * Canvas.TextHeight(sl[i]) - 1;
            S := Rect(iPos, iLine, R.Right, R.Bottom);
            DrawText(Canvas.Handle, PChar(sl[i]), Length(sl[i]), S, DT_LEFT Or DT_TOP);
            Inc(iPos, Canvas.TextWidth(sl[i]));
          End;
        End Else
        Begin
          Dec(iLine);
          S := Rect(iPos, iLine, R.Right, R.Bottom);
          DrawText(Canvas.Handle, PChar(NodeData.FNode.Text), Length(NodeData.FNode.Text), S,
            DT_LEFT Or DT_TOP);
        End;
  End;

  (**

    This method renders the comment associated with the tree node.

    @precon  None.
    @postcon The comment is rendered belwo the tree node text.

    @param   iHeight as an Integer as a constant
    @param   iLine   as an Integer as a constant
    @param   iPos    as an Integer as a reference

  **)
  Procedure RenderComment(const iHeight, iLine : Integer; var iPos : Integer);

  Var
    strText : String;
    R: TRect;

  Begin
    Canvas.FillRect(Rect(0, iHeight + 2 + iLine, Width, Height));
    Canvas.Pen.Color := clMaroon;
    Canvas.MoveTo(0, iHeight + 4 + iLine);
    Canvas.Lineto(Width, iHeight + 4 + iLine);
    Canvas.Refresh;
    Canvas.Font.Style := [];
    Canvas.Font.Color := clNavy;
    strText := FComment.AsString(MaxInt, False);
    R := Rect(2, iHeight + 6 + iLine, Width - 2, Height);
    iPos := DrawText(Canvas.Handle, PChar(strText), -1, R,
      DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
      DrawTextBiDiModeFlagsReadingOnly);
  End;

  (**

    This method renders the list of special tags at the bottom of the hint window.

    @precon  None.
    @postcon The special tags associated with the tree node are drawn at the bottom of the hint
             window.

    @param   iHeight as an Integer
    @param   iPos    as an Integer as a reference
    @param   iLine   as an Integer as a reference

  **)
  Procedure RenderSpecialTags(iHeight : Integer; var iPos, iLine : Integer);

  Var
    iSpecialTag: Integer;
    R : TRect;
    iTag: Integer;
    strText : String;

  Begin
    R := Rect(2, iHeight + 6 + iLine + iPos, Width - 4, Height);
    For iSpecialTag := 0 To TBADIOptions.BADIOptions.SpecialTags.Count - 1 Do
      Begin
        If CanDrawSpecialTag(FComment, TBADIOptions.BADIOptions.SpecialTags.Names[iSpecialTag]) Then
          Begin
            Canvas.Refresh;
            Canvas.Font.Style := [fsBold, fsUnderline];
            Canvas.Font.Color := clPurple;
            Inc(R.Top, 5);
            R := Rect(2, R.Top, Width - 2, Height);
            strText := TBADIOptions.BADIOptions.SpecialTags.Values[
              TBADIOptions.BADIOptions.SpecialTags.Names[iSpecialTag]];
            Inc(R.Top, DrawText(Canvas.Handle, PChar(strText), -1, R,
              DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
              DrawTextBiDiModeFlagsReadingOnly) + 1);
            For iTag := 0 To FComment.TagCount - 1 Do
              If CompareText(TBADIOptions.BADIOptions.SpecialTags.Names[iSpecialTag],
                FComment.Tag[iTag].TagName) = 0 Then
                Begin
                  Canvas.Pen.Color := clBlack;
                  Canvas.Brush.Color := clBlack;
                  Canvas.Ellipse(3, R.Top + 5, 7, R.Top + 9);
                  Canvas.Brush.Color :=
                    TBADIOptions.BADIOptions.TokenFontInfo[ttExplorerHighlight].FBackColour;
                  Canvas.Refresh;
                  Canvas.Font.Style := [];
                  Canvas.Font.Color := clMaroon;
                  R := Rect(10, R.Top, Width - 2, Height);
                  strText := FComment.Tag[iTag].AsString(MaxInt, False);
                  Inc(R.Top, DrawText(Canvas.Handle, PChar(strText), -1, R,
                    DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
                    DrawTextBiDiModeFlagsReadingOnly));
                End;
          End;
      End;
  End;

Var
  iPos : Integer;
  iLine : Integer;
  iHeight: Integer;
  NodeData : PTreeData;

Begin
  iLine := 0;
  iPos := 2;
  Canvas.Font.Assign(FTreeView.Font);
  NodeData := FTreeView.GetNodeData(FNode);
  RenderTreeNode(NodeData, iPos, iLine);
  Canvas.Brush.Color := TBADIOptions.BADIOptions.TokenFontInfo[ttExplorerHighlight].FBackColour;
  If (FComment <> Nil) And ((FComment.TokenCount > 0) Or (FComment.TagCount > 0)) Then
    Begin
      iHeight := Canvas.TextHeight('Wp');
      RenderComment(iHeight, iLine, iPos);
      RenderSpecialTags(iHeight, iPos, iLine);
    End;
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
Function TCustomHintWindow.CalcHintRect(MinWidth, MaxWidth : Integer;
  Node : PVirtualNode; SyntaxHighlight : Boolean; Comment : TComment) : TRect;

  (**

    This method calculates the width of the hint window to acommodate the tree nodes text and
    comments.

    @precon  None.
    @postcon The rectangle for the hint window is updated.

  **)
  Procedure CalcTreeNodeWidth;

  Var
    NodeData : PTreeData;
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
        iPos := 5;
        NodeData := FTreeView.GetNodeData(Node);
        sl := NodeData.FNode.Tokens;
        iLastmax := 0;
        For iToken := 0 To sl.Count - 1 Do
          Begin
            GetFontInfo(sl, iToken, NodeData.FNode.Title, Canvas);
            Inc(iPos, Canvas.TextWidth(sl[iToken]) + 1);
            If iPos > iMaxPos Then
              Begin
                Inc(Result.Bottom, Canvas.TextHeight(sl[iToken]) + 2); //: @bug Is this right for the hint width?
                iPos := 5 + 10; // indent multiple lines
                iLastMax := iMaxPos;
              End Else
                If iPos > iLastMax Then
                  iLastMax := iPos;
          End;
        Result.Right := Result.Left + iLastMax;
      End;
  End;

  (**

    This method calculates the additional height required for all the special tags.

    @precon  None.
    @postcon The resultant hint window rectangle is increased in height for all the special tags.

  **)
  Procedure CalcSpecialTags();

    (**

      This method calculates the additional height required for a special tag.

      @precon  None.
      @postcon The resultant hint window rectangle is increased in height for a special tag.

      @param   iSpecialTag as an Integer

    **)
    Procedure CalcSpecialTag(iSpecialTag : Integer);

    Var
      iTag: Integer;
      R: TRect;
      strText: String;

    Begin
      For iTag := 0 To Comment.TagCount - 1 Do
        If CompareText(TBADIOptions.BADIOptions.SpecialTags.Names[iSpecialTag],
          Comment.Tag[iTag].TagName) = 0 Then
          Begin
            Refresh;
            Canvas.Font.Style := [];
            R := Rect(Result.Left + 2, 0, Result.Right - 12, 0);
            strText := Comment.Tag[iTag].AsString(MaxInt, False);
            Inc(Result.Bottom, DrawText(Canvas.Handle, PChar(strText), -1, R,
              DT_CALCRECT Or DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
              DrawTextBiDiModeFlagsReadingOnly) + 2);
          End;
    End;

  Var
    iSpecialTag : Integer;
    R: TRect;
    strText: String;

  Begin
    For iSpecialTag := 0 To TBADIOptions.BADIOptions.SpecialTags.Count - 1 Do
      Begin
        If CanDrawSpecialTag(Comment, TBADIOptions.BADIOptions.SpecialTags.Names[iSpecialTag]) Then
          Begin
            Refresh;
            Canvas.Font.Style := [fsBold, fsUnderline];
            R := Rect(Result.Left + 2, 0, Result.Right - 2, 0);
            strText := TBADIOptions.BADIOptions.SpecialTags.Values[
              TBADIOptions.BADIOptions.SpecialTags.Names[iSpecialTag]];
            Inc(Result.Bottom, 5 + DrawText(Canvas.Handle, PChar(strText), -1, R,
              DT_CALCRECT Or DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
              DrawTextBiDiModeFlagsReadingOnly) + 2);
            CalcSpecialTag(iSpecialTag);
          End;
      End;
  End;

Var
  R : TRect;
  strText : String;

Begin
  Canvas.Font.Assign(FTreeView.Font);
  Result := FTreeView.GetDisplayRect(Node, NoColumn, True);
  Inc(Result.Left, 2); // Adjustment for
  Inc(Result.Top, 1);
  CalcTreeNodeWidth;
  Dec(Result.Bottom, 4);
  Dec(Result.Left, 1);
  Inc(Result.Right, 2);
  // Check for comment
  If (Comment <> Nil) And ((Comment.TokenCount > 0) Or (Comment.TagCount > 0)) Then
    Begin
      If Result.Right - Result.Left < MinWidth Then
        Result.Right := Result.Left + MinWidth;
      Inc(Result.Bottom, 5);
      Refresh;
      Canvas.Font.Style := [];
      strText := Comment.AsString(MaxInt, False);
      R := Rect(Result.Left + 2, 0, Result.Right - 2, 0);
      Inc(Result.Bottom, DrawText(Canvas.Handle, PChar(strText), -1, R,
        DT_CALCRECT Or DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
        DrawTextBiDiModeFlagsReadingOnly) + 2);
      CalcSpecialTags;
    End;
End;

(**

  This is the constructor method for the TCustomHintWindow class.

  @precon  ATreeView must be a valid instance of the explorer treeview.
  @postcon Ensures the hint can get the treeviews font information.

  @param   AOwner    as a TComponent
  @param   ATreeView as a TVirtualStringTree

**)
constructor TCustomHintWindow.Create(AOwner : TComponent; ATreeView: TVirtualStringTree);
begin
  Inherited Create(AOwner);
  FTreeView := ATreeView;
end;

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
Procedure TCustomHintWindow.ActivateHint(Rect : TRect; Node : PVirtualNode;
  SyntaxHighlight : Boolean; Comment : TComment);

Var
 NodeData : PTreeData;

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

  This method determines if the special tag need to be rendered in the hint window.

  @precon  Comment is the comment to get tags from and strSpecialTag is the special tag information
           to look for.
  @postcon Return true is the special tag was found in the comment.

  @param   Comment       as a TComment
  @param   strSpecialTag as a String as a constant
  @return  a Boolean

**)
Function TCustomHintWindow.CanDrawSpecialTag(Comment : TComment;
  const strSpecialTag : String) : Boolean;
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

  This is the constructor method for the TframeModuleExplorer class.

  @precon  None.
  @postcon Initialises the class.

  @param   AOwner as a TComponent

**)
Constructor TframeModuleExplorer.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  {$IFDEF D2009}
  DoubleBuffered := True;
  {$ENDIF}
  FExplorer := TBADIVirtualStringTree.Create(Self);
  With FExplorer Do
    Begin
      Parent := Self;
      Align := alClient;
      Header.AutoSizeIndex := 0;
      Header.Font.Charset := DEFAULT_CHARSET;
      Header.Font.Color := clWindowText;
      Header.Font.Height := -11;
      Header.Font.Name := 'Tahoma';
      Header.Font.Style := [];
      Header.MainColumn := -1;
      Header.Options := [hoColumnResize, hoDrag];
      Images := ilScopeImages;
      TabOrder := 3;
      OnBeforeItemPaint := tvExplorerBeforeItemPaint;
      OnClick := tvExplorerClick;
      OnGetImageIndex := tvExplorerGetImageIndex;
      OnKeyPress := tvExplorerKeyPress;
      OnMeasureItem := tvExplorerMeasureItem;
      OnMouseMove := tvExplorerMouseMove;
    End;
  FMouseEnter := False;
  FExplorer.NodeDataSize := SizeOf(TTreeData);
  FINIFileName := BuildRootKey(Nil, Nil);
  FNodeInfo := TObjectList.Create(True);
  FHintWin := TCustomHintWindow.Create(Self, FExplorer);
  FHintWin.Color := TBADIOptions.BADIOptions.TokenFontInfo[ttExplorerHighlight].FBackColour;
  FHintWin.Canvas.Font.Assign(FExplorer.Font);
  edtExplorerFilter.Font.Assign(FExplorer.Font);
  ilScopeImages.Clear;
  LoadBADIImages;
  FExplorer.OnGetText := tvExplorerGetText;
end;

(**

  This is the destructor method for the TframeModuleExplorer class.

  @precon  None.
  @postcon destroy the instance of the dockable form.

**)
Destructor TframeModuleExplorer.Destroy;

begin
  If FModule <> Nil Then
    GetExpandedNodes(FModule);
  FExplorer.Free;
  FSpecialTagNodes := Nil;
  ManageExpandedNodes;
  FHintWin.Free;
  FNodeInfo.Free;
  Inherited;
end;

(**

  This is an on change event handler for the Explorer Filter edit control.

  @precon  None.
  @postcon Notify that the filter has changed and the explorer should be filtered.

  @param   Sender as a TObject

**)
Procedure TframeModuleExplorer.edtExplorerFilterChange(Sender: TObject);

Begin
  FilterChange;
End;

(**

  This is an on key press event handler for the Explorer Filter edit control.

  @precon  None.
  @postcon Clears the filter is escape is pressed.

  @param   Sender as a TObject
  @param   Key    as a Char as a reference

**)
Procedure TframeModuleExplorer.edtExplorerFilterKeyPress(Sender: TObject; Var Key: Char);

Begin
  Case Key Of
    #27:
      Begin
        edtExplorerFilter.Clear;
        Key := #0;
      End;
  End;
End;

(**

  This is a mouse activate event handler for the explorer filter.

  @precon  None.
  @postcon Makes FMouseEnter true to signify that the mouse was clicked on the filter.

  @param   Sender        as a TObject
  @param   Button        as a TMouseButton
  @param   Shift         as a TShiftState
  @param   X             as an Integer
  @param   Y             as an Integer
  @param   HitTest       as an Integer
  @param   MouseActivate as a TMouseActivate as a reference

**)
procedure TframeModuleExplorer.edtExplorerFilterMouseActivate(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y, HitTest: Integer; var MouseActivate: TMouseActivate);
begin
  FMouseEnter := True;
end;

(**

  This method updates the filtering of the treeview.

  @precon  None.
  @postcon The treeview filter is updated onyl showing node that match the filter text.

**)
Procedure TframeModuleExplorer.FilterChange;

Var
  N : PVirtualNode;

Begin
  stbStatusBar.SimplePanel := False;
  Try
    If edtExplorerFilter.Text <> '' Then
      FFilterRegEx := TregEx.Create(edtExplorerFilter.Text, [roIgnoreCase, roCompiled,
        roSingleLine]);
  Except
    On E : ERegularExpressionError Do
      Begin
        stbStatusBar.SimpleText := E.Message;
        stbStatusBar.SimplePanel := True;
      End;
  End;
  FExplorer.BeginUpdate;
  Try
    N := FExplorer.RootNode.FirstChild;
    While N <> Nil Do
      Begin
        FExplorer.IterateSubtree(N, FilterProc, Nil);
        N := FExplorer.GetNextSibling(N);
      End;
  Finally
    FExplorer.EndUpdate;
  End;
End;

(**

  This method retrieves comments from the body comment collection and adds them
  to the comment node of the tree view.

  @precon  M is a valid instance of a TBaseLanguageModule that has been parsed.
  @postcon This method cycles through the body comments extracting special tags.

  @param   M as a TBaseLanguageModule

**)
procedure TframeModuleExplorer.GetBodyCommentTags(M : TBaseLanguageModule);

Var
  i, j, k : Integer;

Begin
  For i := 0 To M.BodyCommentCount - 1 Do
    With M.BodyComment[i] Do
      For j := 0 To TagCount - 1 Do
        For k := Low(FSpecialTagNodes) To High(FSpecialTagNodes) Do
          If FSpecialTagNodes[k].boolShow Then
            If CompareText(Tag[j].TagName, FSpecialTagNodes[k].strTagName) = 0 Then
              AddNode(
                FSpecialTagNodes[k].Node,
                Tag[j].AsString(MaxInt, False),
                Tag[j].Name,
                2,
                BADIImageIndex(iiToDoItem, scNone),
                M.BodyComment[i].Tag[j].Line,
                M.BodyComment[i].Tag[j].Column,
                False,
                M.BodyComment[i]
              );
End;

(**

  This method renders the modules sub-containers recursively.

  @precon  RootNode must be a valid tree node and Container must be a valid
           container.
  @postcon Renders the modules sub-containers recursively.

  @param   RootNode  as a PVirtualNode
  @param   Container as a TElementContainer
  @param   iLevel    as an Integer

**)
procedure TframeModuleExplorer.RenderContainers(RootNode : PVirtualNode;
  Container: TElementContainer; iLevel : Integer);

Var
  i : Integer;
  NewNode : PVirtualNode;

begin
  For i := 1 To Container.ElementCount Do
    If Container.Elements[i].Scope In TBADIOptions.BADIOptions.ScopesToRender + [scNone, scGlobal] Then
      Begin
        NewNode := AddNode(
          RootNode,
          Container.Elements[i].AsString(True, False),
          Container.Elements[i].Name,
          iLevel,
          Container.Elements[i].ImageIndexAdjustedForScope,
          Container.Elements[i].Line,
          Container.Elements[i].Column,
          Container.Elements[i] Is TLabelContainer,
          Container.Elements[i].Comment
        );
        RenderContainers(NewNode, Container[i], iLevel + 1);
      End;
end;

(**

  This method returns the path of the specified tree node.

  @precon  Node is the tree node to be pathed.
  @postcon Returns a string representation of the tree nodes path excluding
           the root item.

  @param   Node as a PVirtualNode
  @return  a String

**)
Function TframeModuleExplorer.GetNodePath(Node : PVirtualNode) : String;

Var
  P : PVirtualNode;
  str : String;
  NodeData : PTreeData;

Begin
  str := '';
  P := Node;
  While P <> Nil Do
    Begin
      If FExplorer.NodeParent[P] <> Nil Then
        Begin
          NodeData := FExplorer.GetNodeData(P);
          str := NodeData.FNode.Name + '.' + str;
        End;
      P := FExplorer.NodeParent[P];
    End;
  Result := str;
End;

(**

  This method loads the BADI images from the DLLs resources into an image list multipling the
  number of images by the number of overlay masks for scope.

  @precon  None.
  @postcon The images are loaded, one for each scope.

**)
Procedure TframeModuleExplorer.LoadBADIImages;

Var
  R: TRect;
  MainImage: TBitmap;
  ScopeImage: TBitmap;
  iImage: TBADIImageIndex;
  iScope: TScope;
  x: Integer;
  y: Integer;

Begin
  R := Rect(0, 0, 11, 11);
  MainImage := TBitMap.Create;
  Try
    ScopeImage := TBitmap.Create;
    Try
      For iImage := Succ(Low(TBADIImageIndex)) To High(TBADIImageIndex) Do
        For iScope := Low(TScope) To High(TScope) Do
          Begin
              MainImage.LoadFromResourceName(hInstance, BADIImageList[iImage].FResourceName);
              ScopeImage.LoadFromResourceName(hInstance, BADIScopeList[iScope].FResourceName);
              For x := 0 To 11 Do
                For y := 0 To 11 Do
                  If ScopeImage.Canvas.Pixels[x, y] <> BADIScopeList[iScope].FMaskColour Then
                    MainImage.Canvas.Pixels[x, y] := ScopeImage.Canvas.Pixels[x, y];
              ilScopeImages.AddMasked(MainImage, BADIImageList[iImage].FMaskColour);
          End;
    Finally
      ScopeImage.Free;
    End;
  Finally
    MainImage.Free;
  End;
End;

(**

  This method removed items from the list their date (TObject data) is more than
  a specific age in days.

  @precon  None.
  @postcon Removed items from the list their date (TObject data) is more than
           a specific age in days.

**)
procedure TframeModuleExplorer.ManageExpandedNodes;

Var
  i : Integer;
  dtDate: TDateTime;

begin
  With TBADIOptions.BADIOptions.ExpandedNodes Do
  For i := Count - 1 DownTo 0 Do
    Begin
      dtDate := Integer(Objects[i]);
      If dtDate < Now - TBADIOptions.BADIOptions.ManagedNodesLife Then
        Delete(i);
    End;
end;

(**

  This method gets the tree nodes that are currently expanded and stores them
  in a string list.

  @precon  Node is the tree node to be tested for expansion.
  @postcon Adds, update or deletes nodes from the expanded node list depending
           whether thhey are now expanded.

  @param   StartNode as a PVirtualNode

**)
Procedure TframeModuleExplorer.GetExpandedNodes(StartNode : PVirtualNode);

Var
  str : String;
  iIndex : Integer;
  Node : PVirtualNode;

Begin
  Node := FExplorer.GetFirstChild(StartNode);
  While Node <> Nil Do
    Begin
      If Node.ChildCount > 0 Then
        Begin
          str := GetNodePath(Node);
          If FExplorer.Expanded[Node] And (str <> '') Then
            Begin
              If Not TBADIOptions.BADIOptions.ExpandedNodes.Find(str, iIndex) Then
                iIndex := TBADIOptions.BADIOptions.ExpandedNodes.Add(str);
              TBADIOptions.BADIOptions.ExpandedNodes.Objects[iIndex] := TObject(Trunc(Now));
            End Else
              If TBADIOptions.BADIOptions.ExpandedNodes.Find(str, iIndex) Then
                TBADIOptions.BADIOptions.ExpandedNodes.Delete(iIndex);
          GetExpandedNodes(Node);
        End;
      Node := FExplorer.GetNextSibling(Node);
    End;
End;

(**

  This method expands the tree view nodes if they are foudn in the list.

  @precon  Node is the tree node to be expanded.
  @postcon Sets the node as expanded if it was in the edpanded node list.

  @param   StartNode as a PVirtualNode

**)
Procedure TframeModuleExplorer.SetExpandedNodes(StartNode : PVirtualNode);

Var
  i : Integer;
  str : String;
  Node : PVirtualNode;

Begin
  Node := FExplorer.GetFirstChild(StartNode);
  While Node <> Nil Do
    Begin
      If Node.ChildCount > 0 Then
        Begin
          str := GetNodePath(Node);
          If TBADIOptions.BADIOptions.ExpandedNodes.Find(str, i) Then
            FExplorer.Expanded[Node] := True;
          SetExpandedNodes(Node);
        End;
      Node := FExplorer.GetNextSibling(Node);
    End;
End;

(**

  This method sets the contents of a specific statusbar panel and sets its width accordingly.

  @precon  None.
  @postcon The status bar panel is updated.

  @param   ePanel           as a TStatusPanelIndex
  @param   strStatusBarText as a String as a constant
  @param   iValue           as an Integer

**)
Procedure TframeModuleExplorer.SetStatusPanel(ePanel: TStatusPanelIndex;
  const strStatusBarText: String; iValue: Integer);

Const
  iPadding = 10;

Var
  strText : String;

Begin
  strText := Format(strStatusbarText, [Int(iValue)]);
  stbStatusBar.Panels[Byte(ePanel)].Text := strText;
  stbStatusBar.Panels[Byte(ePanel)].Width :=
    stbStatusBar.Canvas.TextWidth(strText) + 2 * iPadding;
End;

(**

  This is a call back mechanism for the tree view in otder to determine whether a node
  when iterated can be visible.

  @precon  None.
  @postcon Filters the visible cells in the tree view if they match the filter text.

  @param   Sender as a TBaseVirtualTree
  @param   Node   as a PVirtualNode
  @param   Data   as a Pointer
  @param   Abort  as a Boolean as a reference

**)
Procedure TframeModuleExplorer.FilterProc(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Data: Pointer; Var Abort: Boolean);

Var
  NodeData : PTreeData;
  N : PVirtualNode;

Begin
  NodeData := Sender.GetNodeData(Node);
  If edtExplorerFilter.Text <> '' Then
    Begin
      Sender.IsVisible[Node] := FFilterRegEx.IsMatch(NodeData.FNode.Text);
      If Sender.IsVisible[Node] Then
        Begin
          N := FExplorer.NodeParent[Node];
          While N <> Nil Do
            Begin
              Sender.IsVisible[N] := True;
              N := FExplorer.NodeParent[N];
            End;
        End;
    End Else
      Sender.IsVisible[Node] := True;
End;

(**

  This function finds the tree node that has the path specified by the passed text.

  @precon  strText is the string representation of the node path to be found.
  @postcon Returns tree node index of the item corresponding to the given path.

  @param   strText as a String as a constant
  @return  a PVirtualNode

**)
Function TframeModuleExplorer.FindTreeItem(const strText : String) : PVirtualNode;

  (**

    This function recursively searches for the node path which matches the
    given text and returns the node is found.

    @precon  None.
    @postcon Recursively searches for the node path which matches the
             given text and returns the node is found.

    @param   Node as a PVirtualNode
    @return  a PVirtualNode

  **)
  Function FindNode(Node : PVirtualNode) : PVirtualNode;

  Var
    C : PVirtualNode;

  Begin
      Result := Nil;
      C := FExplorer.GetFirstChild(Node);
      While C <> Nil Do
        Begin
          If C.ChildCount > 0 Then
            Result := FindNode(C);
          If Result <> Nil Then
            Exit;
          If CompareText(GetNodePath(C), strText) = 0 Then
            Begin
              Result := C;
              Exit;
            End;
          C := FExplorer.GetNextSibling(C);
        End;
  End;

Begin
  Result := Nil;
  If strText <> '' Then
    Result := FindNode(FModule);
End;

(**

  This method sets the explorer frame as the focus when the form is activated.

  @precon  None.
  @postcon The explorer frame is focused.

  @param   Sender as a TObject

**)
procedure TframeModuleExplorer.FrameEnter(Sender: TObject);

begin
  If Not FMouseEnter Then
    If Parent.Visible And Visible And FExplorer.Visible And FExplorer.CanFocus Then
      FExplorer.SetFocus;
  FMouseEnter := False;
end;

(**

  This method displays the specified module in the treeview.

  @precon  M is a valid instance of a TBaseLanguageModule that has been parsed and
           strStatus is a text string to be displayed in the forms status bar.
  @postcon Renders the module information for the given module.

  @param   M as a TBaseLanguageModule

**)
procedure TframeModuleExplorer.RenderModule(M : TBaseLanguageModule);

Var
  strTop : String;
  strSelection : String;
  N : PVirtualNode;

Begin
  FExplorer.Color := TBADIOptions.BADIOptions.BGColour;
  If M = Nil Then
    Begin
      strReservedWords := Nil;
      strDirectives := Nil;
    End Else
    Begin
      strReservedWords := M.ReservedWords;
      strDirectives := M.Directives;
    End;
  If FRendering Then
    Exit;
  FRendering := True;
  Try
    FHintWin.ReleaseHandle; // Stop AV when refreshing the tree.
    FExplorer.Font.Name := TBADIOptions.BADIOptions.FontName;
    FExplorer.Font.Size := TBADIOptions.BADIOptions.FontSize;
    GetExpandedNodes(FModule);
    FModule := Nil;
    // Find and store the top item and the selected item in the tree view
    strTop := GetNodePath(FExplorer.TopNode);
    strSelection := GetNodePath(FExplorer.FocusedNode);
    FExplorer.BeginUpdate;
    Try
      edtExplorerFilter.Text := '';
      FExplorer.Clear;
      FNodeInfo.Clear;
      If M = Nil Then
        Exit;
      M.AddTickCount('Clear');
      SetLength(FSpecialTagNodes, TBADIOptions.BADIOptions.SpecialTags.Count);
      // Create Root Tree Node
      FModule := AddNode(
        Nil,
        M.AsString(True, False),
        M.Name,
        0,
        M.ImageIndexAdjustedForScope,
        M.ModuleNameLine,
        M.ModuleNameCol,
        False,
        M.Comment
      );
      CreateSpecialTagNodes(M);
      OutputModuleInfo(M);
      M.AddTickCount('Build');
      SetExpandedNodes(FModule);
      ExpandNodes;
      // Restore top and selected items
      If M.FindElement(strErrors) = Nil Then // Only if no errors.
        Begin
          N := FindTreeItem(strTop);
          If N <> Nil Then
            FExplorer.TopNode := N;
        End Else
          FExplorer.TopNode := FModule;
      N := FindTreeItem(strSelection);
      If N <> Nil Then
        Begin
          FExplorer.FocusedNode := N;
          FExplorer.Selected[FExplorer.FocusedNode] := True;
        End;
    Finally
      If M <> Nil Then
        M.AddTickCount('Setup');
      FExplorer.EndUpdate;
    End;
    M.AddTickCount('Render');
    UpdateStatusBar(M);
  Finally
    FRendering := False;
  End;
End;

(**

  This method outputs the modules information as items in the treeview.

  @precon  M is a valid instance of a TBaseLanguageModule that has been parsed.
  @postcon Outputs the different parts of the module.

  @param   Container as a TElementContainer

**)
procedure TframeModuleExplorer.OutputModuleInfo(Container : TElementContainer);

Const
  strPromotedLabels : Array[1..4] Of String = (strDocumentationConflicts,
    strHints, strWarnings, strErrors);

var
  i: Integer;
  Node : PVirtualNode;
  NodeData : PTreeData;

begin
  RenderContainers(FModule, Container, 1);
  GetBodyCommentTags(Container As TBaseLanguageModule);
  For i := Low(strPromotedLabels) To High(strPromotedLabels) Do
    Begin
      Node := FExplorer.GetFirstChild(FModule);
      While Node <> Nil Do
        Begin
          NodeData := FExplorer.GetNodeData(Node);
          If NodeData.FNode.Text = strPromotedLabels[i] Then
            FExplorer.MoveTo(Node, FModule, amAddChildFirst, False);
          Node := FExplorer.GetNextSibling(Node);
        End;
    End;
end;


(**

  This method expands, collapses or delete various nodes depending on the
  options and their contents.

  @precon  None.
  @postcon Expands, collapses or delete various nodes depending on the
           options and their contents.

**)
procedure TframeModuleExplorer.ExpandNodes;

Var
  iPromotedLabel : Integer;
  Node: PVirtualNode;
  NodeData: PTreeData;
  N: PVirtualNode;

begin
  FExplorer.Expanded[FModule] := True;
  For iPromotedLabel := Low(FSpecialTagNodes) to High(FSpecialTagNodes) Do
    If FSpecialTagNodes[iPromotedLabel].Node.ChildCount = 0 Then
      Begin
        FExplorer.DeleteNode(FSpecialTagNodes[iPromotedLabel].Node);
        FSpecialTagNodes[iPromotedLabel].Node := Nil;
      End;
  For iPromotedLabel := Low(FSpecialTagNodes) to High(FSpecialTagNodes) Do
    If FSpecialTagNodes[iPromotedLabel].Node <> Nil Then
      If FSpecialTagNodes[iPromotedLabel].boolExpand Then
        FExplorer.Expanded[FSpecialTagNodes[iPromotedLabel].Node] := True;
  Node := FExplorer.GetFirstChild(FModule);
  While Node <> Nil Do
    Begin
      NodeData := FExplorer.GetNodeData(Node);
      If CompareText(NodeData.FNode.Text, strDocumentationConflicts) = 0 Then
        If doShowConflicts In TBADIOptions.BADIOptions.Options Then
          Begin
            FExplorer.Expanded[Node] := True;
            N := FExplorer.GetFirstChild(Node);
            While N <> Nil Do
              Begin
                FExplorer.Expanded[N] := True;
                N := FExplorer.GetNextSibling(N);
              End;
          End;
      If CompareText(NodeData.FNode.Text, strHints) = 0 Then
        If doShowHints In TBADIOptions.BADIOptions.Options Then
          FExplorer.Expanded[Node] := True;
      If CompareText(NodeData.FNode.Text, strWarnings) = 0 Then
        If doShowWarnings In TBADIOptions.BADIOptions.Options Then
          FExplorer.Expanded[Node] := True;
      If CompareText(NodeData.FNode.Text, strErrors) = 0 Then
        If doShowErrors In TBADIOptions.BADIOptions.Options Then
          FExplorer.Expanded[Node] := True;
      Node := FExplorer.GetNextSibling(Node);
    End;
end;


(**

  This method create both the todo folder node and the document conflict
  folders in the treeview.

  @precon  None.
  @postcon Creates the special tag noNode.
  @param   M as a TBaseLanguageModule

**)
Procedure TframeModuleExplorer.CreateSpecialTagNodes(M : TBaseLanguageModule);

Var
  i : Integer;

Begin
  For i := Low(FSpecialTagNodes) to High(FSpecialTagNodes) Do
    Begin
      FSpecialTagNodes[i].strTagName := TBADIOptions.BADIOptions.SpecialTags.Names[i];
      FSpecialTagNodes[i].strTagDesc :=
        TBADIOptions.BADIOptions.SpecialTags.Values[TBADIOptions.BADIOptions.SpecialTags.Names[i]];
      FSpecialTagNodes[i].boolShow :=
        Integer(TBADIOptions.BADIOptions.SpecialTags.Objects[i]) And iShowInTree <> 0;
      FSpecialTagNodes[i].boolExpand :=
        Integer(TBADIOptions.BADIOptions.SpecialTags.Objects[i]) And iAutoExpand <> 0;
      FSpecialTagNodes[i].Node := AddNode(
        FModule,
        FSpecialTagNodes[i].strTagDesc,
        FSpecialTagNodes[i].strTagName,
        1,
        BADIImageIndex(iiTodoFolder, scNone),
        0,
        0,
        True
      );
    End;
End;

(**

  This is an on execute event handler for all the scope actions.

  @precon  None.
  @postcon Updates the ScopesToRender set based on the action invoked.

  @param   Sender as a TObject

**)
procedure TframeModuleExplorer.actLocalExecute(Sender: TObject);

  (**

    This procedure adds or removed the given scope from the ScopesToRender set.

    @precon  None.
    @postcon Adds or removed the given scope from the ScopesToRender set.

    @param   AScope as a TScope

  **)
  procedure UpdateScopes(AScope : TScope); InLine;

  Begin
    With TBADIOptions.BADIOptions Do
      If AScope In ScopesToRender Then
        ScopesToRender := ScopesToRender - [AScope]
      Else
        ScopesToRender := ScopesToRender + [AScope];
  End;

  (**

    This procedure adds or removed the given option from the options set.

    @precon  None.
    @postcon Adds or removed the given option from the Options set.

    @param   Option as a TDocOption

  **)
  procedure UpdateOptions(Option : TDocOption); InLine;

  Begin
    With TBADIOptions.BADIOptions Do
      If Option In Options Then
        Options := Options - [Option]
      Else
        Options := Options + [Option];
  End;

begin
  With TBADIOptions.BADIOptions Do
    If Sender = actLocal Then
      UpdateScopes(scLocal)
    Else If Sender = actPrivate Then
      UpdateScopes(scPrivate)
    Else If Sender = actProtected Then
      UpdateScopes(scProtected)
    Else If Sender = actPublic Then
      UpdateScopes(scPublic)
    Else If Sender = actPublished Then
      UpdateScopes(scPublished)
    Else If Sender = actSyntax Then
      UpdateOptions(doCustomDrawing)
    Else If Sender = actShowHints Then
      UpdateOptions(doShowCommentHints)
    Else If Sender = actConflicts Then
      UpdateOptions(doShowConflicts)
    Else If Sender = actErrors Then
      UpdateOptions(doShowErrors)
    Else If Sender = actWarnings Then
      UpdateOptions(doShowWarnings)
    Else If Sender = actHints Then
      UpdateOptions(doShowHints)
    Else If Sender = actMethods Then
      UpdateOptions(doShowMethodMissingDocs)
    Else If Sender = actProperties Then
      UpdateOptions(doShowPropertyMissingDoc)
    Else If Sender = actConstants Then
      UpdateOptions(doShowUndocumentedConsts)
    Else If Sender = actVariables Then
      UpdateOptions(doShowUndocumentedVars)
    Else If Sender = actTypes Then
      Begin
        UpdateOptions(doShowUndocumentedTypes);
        UpdateOptions(doShowUndocumentedRecords);
        UpdateOptions(doShowUndocumentedObjects);
        UpdateOptions(doShowUndocumentedClasses);
        UpdateOptions(doShowUndocumentedInterfaces);
      End;
  If Assigned(FRefresh) Then
    FRefresh(Sender);
end;

(**

  This is an on update event handler for the sceop actions.

  @precon  None.
  @postcon Checks the action depending on the scopes in ScopesToRender.

  @param   Sender as a TObject

**)
procedure TframeModuleExplorer.actLocalUpdate(Sender: TObject);
begin
  If Sender = actLocal Then
    (Sender As TAction).Checked := scLocal In TBADIOptions.BADIOptions.ScopesToRender
  Else If Sender = actPrivate Then
    (Sender As TAction).Checked := scPrivate In TBADIOptions.BADIOptions.ScopesToRender
  Else If Sender = actProtected Then
    (Sender As TAction).Checked := scProtected In TBADIOptions.BADIOptions.ScopesToRender
  Else If Sender = actPublic Then
    (Sender As TAction).Checked := scPublic In TBADIOptions.BADIOptions.ScopesToRender
  Else If Sender = actPublished Then
    (Sender As TAction).Checked := scPublished In TBADIOptions.BADIOptions.ScopesToRender
  Else If Sender = actSyntax Then
    (Sender As TAction).Checked := doCustomDrawing In TBADIOptions.BADIOptions.Options
  Else If Sender = actShowHints Then
    (Sender As TAction).Checked := doShowCommentHints In TBADIOptions.BADIOptions.Options
  Else If Sender = actConflicts Then
    (Sender As TAction).Checked := doShowConflicts In TBADIOptions.BADIOptions.Options
  Else If Sender = actErrors Then
    (Sender As TAction).Checked := doShowErrors In TBADIOptions.BADIOptions.Options
  Else If Sender = actWarnings Then
    (Sender As TAction).Checked := doShowWarnings In TBADIOptions.BADIOptions.Options
  Else If Sender = actHints Then
    (Sender As TAction).Checked := doShowHints In TBADIOptions.BADIOptions.Options
  Else If Sender = actMethods Then
    (Sender As TAction).Checked := doShowMethodMissingDocs In TBADIOptions.BADIOptions.Options
  Else If Sender = actProperties Then
    (Sender As TAction).Checked := doShowPropertyMissingDoc In TBADIOptions.BADIOptions.Options
  Else If Sender = actConstants Then
    (Sender As TAction).Checked := doShowUndocumentedConsts In TBADIOptions.BADIOptions.Options
  Else If Sender = actVariables Then
    (Sender As TAction).Checked := doShowUndocumentedVars In TBADIOptions.BADIOptions.Options
  Else If Sender = actTypes Then
    (Sender As TAction).Checked := doShowUndocumentedTypes In TBADIOptions.BADIOptions.Options
end;

(**

  This method adds a node to the treeview as a child of the give node. It assigns the line, column
  and comment information to the noNode.

  @precon  P is the parent node to attach this new child too, Element is the parser node to render.
  @postcon Returns a instance of the newly add / created tree node.

  @param   P           as a PVirtualNode
  @param   strText     as a String as a constant
  @param   strName     as a String as a constant
  @param   iLevel      as an Integer
  @param   iImageIndex as an Integer
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   boolTitle   as a Boolean
  @param   AComment    as a TComment
  @return  a PVirtualNode

**)
Function TframeModuleExplorer.AddNode(P : PVirtualNode; const strText, strName : String;
  iLevel : Integer; iImageIndex : Integer; iLine : Integer = 0;
  iColumn : Integer = 0; boolTitle : Boolean = False;
  AComment : TComment = Nil) : PVirtualNode;

Var
  NodeData : PTreeData;
  N : TTreeNodeInfo;

begin
  Result := FExplorer.AddChild(P);
  NodeData := FExplorer.GetNodeData(Result);
  N := TTreeNodeInfo.Create(strText, strName, iLevel, iImageIndex, iLine,
    iColumn, boolTitle, AComment);
  FNodeInfo.Add(N);
  NodeData.FNode := N;
end;

(**

  This is an on get image index event handler for the virtual tree view.

  @precon  None.
  @postcon Sets the image index of the tree view item from the associated
           Tree Bode Info class.

  @param   Sender     as a TBaseVirtualTree
  @param   Node       as a PVirtualNode
  @param   Kind       as a TVTImageKind
  @param   Column     as a TColumnIndex
  @param   Ghosted    as a Boolean as a reference
  @param   ImageIndex as an Integer as a reference

**)
procedure TframeModuleExplorer.tvExplorerGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);

Var
  NodeData : PTreeData;

begin
  NodeData := FExplorer.GetNodeData(Node);
  ImageIndex := NodeData.FNode.ImageIndex;
end;

(**

  This is an on get text event handler for the virtual tree view. It gets the
  text from the tree node info associated with the tree item.

  @precon  None.
  @postcon Gets the text for the tree node from the associated Tree Node Info
           class.

  @param   Sender   as a TBaseVirtualTree
  @param   Node     as a PVirtualNode
  @param   Column   as a TColumnIndex
  @param   TextType as a TVSTTextType
  @param   CellText as a String as a reference

**)
procedure TframeModuleExplorer.tvExplorerGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
{$IFNDEF D2009}
  var CellText: WideString);
{$ELSE}
  var CellText: string);
{$ENDIF}

Var
  NodeData : PTreeData;

begin
  NodeData := FExplorer.GetNodeData(Node);
  CellText := NodeData.FNode.Text;
end;


(**

  This is an on measure item event handler for the tree view.

  @precon  None.
  @postcon Returns the height of the item based on the tree view font.

  @param   Sender       as a TBaseVirtualTree
  @param   TargetCanvas as a TCanvas
  @param   Node         as a PVirtualNode
  @param   NodeHeight   as an Integer as a reference

**)
procedure TframeModuleExplorer.tvExplorerMeasureItem(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
begin
  TargetCanvas.Font.Name := TBADIOptions.BADIOptions.FontName;
  TargetCanvas.Font.Size := TBADIOptions.BADIOptions.FontSize;
  NodeHeight := 1 + TargetCanvas.TextHeight('Ag') + 1;
end;

(**

  This method is a mouse OnMouseMove event. If fires every time the mouse moves
  over the treeview and is used to show the hint messages.

  @precon  Sender is the control that invoked the event, Shift is the status of
           the keyboard modifiers, X is the horizontal position of the mouse
           over the control and Y is the vertical position of the mouse over the
           control
  @postcon Handles the displaying of hints when the mouse is over a tree branch.

  @param   Sender as a TObject
  @param   Shift  as a TShiftState
  @param   X      as an Integer
  @param   Y      as an Integer

**)
procedure TframeModuleExplorer.tvExplorerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);

Var
  Node : PVirtualNode;
  Rect : TRect;
  C : TComment;
  HitInfo: THitInfo;
  NodeData : PTreeData;

begin
  C := Nil;
  FExplorer.GetHitTestInfoAt(X, Y, True, HitInfo);
  If (hiOnItemLabel In HitInfo.HitPositions) Then
    Begin
      Node := HitInfo.HitNode;
      If (Node <> Nil) And (Node <> FLastNode) Then
        Begin
          FLastNode := Node;
          NodeData := FExplorer.GetNodeData(Node);
          If doShowCommentHints In TBADIOptions.BADIOptions.Options Then
            C := NodeData.FNode.Comment;
          Rect := FHintWin.CalcHintRect(FExplorer.ClientWidth, Screen.Width,
            Node, doCustomDrawing In TBADIOptions.BADIOptions.Options, C);
          If (Rect.Right <= FExplorer.ClientWidth) And ((C = Nil) Or
            ((C.TokenCount = 0) And (C.TagCount = 0))) Then
            Begin
              FHintWin.ReleaseHandle;
              Exit;
            End;
          Rect.TopLeft := FExplorer.ClientToScreen(Rect.TopLeft);
          Rect.BottomRight := FExplorer.ClientToScreen(Rect.BottomRight);
          FHintWin.ActivateHint(Rect, Node,
            doCustomDrawing In TBADIOptions.BADIOptions.Options, C);
          FLastNode := Node;
        End Else
          If (Node <> FLastNode) Then
            FHintWin.ReleaseHandle;
    End Else
    Begin
      FHintWin.ReleaseHandle;
      FLastNode := Nil;
    End;
end;

(**

  This method update the status of the module explorer statusbar.

  @precon  M must be a valid instance of a TBaseLanguageModule.
  @postcon Update the status of the module explorer statusbar.

  @param   M as a TBaseLanguageModule

**)
procedure TframeModuleExplorer.UpdateStatusBar(M: TBaseLanguageModule);

Var
  strTickLabel: String;
  dblTicks: Double;
  i : Integer;
  strText : String;

begin
  SetStatusPanel(spiBytes, strStatusbarBytesText, M.Bytes);
  SetStatusPanel(spiTokens, strStatusbarTokensText, M.TokenCount);
  SetStatusPanel(spiLines, strStatusbarLinesText, M.Lines);
  If doShowPerformanceCountersInModuleExplorer In TBADIOptions.BADIOptions.Options Then
    Begin
      strText := '';
      For i := 1 To M.OpTickCounts - 1 Do
        Begin
          strTickLabel := M.OpTickCountName[i];
          If strTickLabel <> '' Then
            strTickLabel := strTickLabel + ':';
          dblTicks := M.OpTickCountByIndex[i] - M.OpTickCountByIndex[i - 1];
          If dblTicks > 1.0 Then //: @debug Filter out small items - Add to Options.
            Begin
              If strText <> '' Then
                strText := strText + ', ';
              strText := strText + Format('%s %1.1n', [strTickLabel, dblTicks]);
            End;
        End;
      If strText <> '' Then
        strText := strText + ', ';
      strText := strText +
        Format('%s: %1.1n', ['Total', M.OpTickCountByIndex[M.OpTickCounts - 1] -
        M.OpTickCountByIndex[0]]);
      stbStatusBar.Panels[Byte(spiFourth)].Text := strText;
      stbStatusBar.Hint := strText;
    End Else
    Begin
      stbStatusBar.Panels[Byte(spiFourth)].Text := '';
      stbStatusBar.Hint := '';
    End;
end;

(**

  This is a message handler for the mouse leave message. It hides the hint
  window when the mouse leaves the control.

  @precon  Msg is the window message to handle.
  @postcon The is a mouse event event which hides the hint window.

  @param   Msg as a TMessage as a reference

**)
Procedure TframeModuleExplorer.CMMouseLeave(var Msg : TMessage);

Begin
  FHintWin.ReleaseHandle;
End;

(**

  This method is an OnDrawItem event handler for the treeview. Depending on the
  options it renders the syntax highlighted version of the tree.

  @precon  Sender is the control that invoked the event, Node is the node in
           the treeview to be drawn, State is the current drawing state of
           the node and DefaultDraw - this is change to false to allow custom
           drawing.
  @postcon This event handler draw a custom item in the tree view.

  @param   Sender       as a TBaseVirtualTree
  @param   TargetCanvas as a TCanvas
  @param   Node         as a PVirtualNode
  @param   ItemRect     as a TRect
  @param   CustomDraw   as a Boolean as a reference

**)
procedure TframeModuleExplorer.tvExplorerBeforeItemPaint(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  ItemRect: TRect; var CustomDraw: Boolean);

  (**

    This method sets the treviews canvas font to those in the BADI options.

    @precon  None.
    @postcon The TargetCanvas font is set for rendering.

    @param   TargetCanvas as a TCanvas

  **)
  Procedure InitCanvasFont(TargetCanvas : TCanvas); InLine;

  Begin
    TargetCanvas.Font.Name := TBADIOptions.BADIOptions.FontName;
    TargetCanvas.Font.Size := TBADIOptions.BADIOptions.FontSize;
  End;

  (**

    This method highlights the selected item in the treeview.

    @precon  None.
    @postcon The item is highlighted if selected.

    @param   TargetCanvas as a TCanvas
    @param   iColour      as a TColor
    @param   ItemRect     as a TRect

  **)
  Procedure HighlightSelectedItem(TargetCanvas : TCanvas; iColour : TColor;
    ItemRect : TRect); InLine;

  Begin
    TargetCanvas.Brush.Color := iColour;
    TargetCanvas.FillRect(ItemRect);
  End;

  (**

    This method renders a selection rectangle around the selected note.

    @precon  NodeData must be a valid pointer to a TTreeData node fo the node being rendered and sl
             must contain the string tokens to be rendered.
    @postcon The selection rectangle is rendered (accounting for extra width due to font
             preferences).

    @param   TargetCanvas      as a TCanvas
    @param   vstExplorer       as a TVirtualStringTree
    @param   Node              as a PVirtualNode
    @param   NodeData          as a PTreeData
    @param   sl                as a TStringList
    @param   ItemRect          as a TRect
    @param   iScopeImagesWidth as an Integer
    @param   iPos              as an Integer as a reference

  **)
  Procedure RenderSelectedNode(TargetCanvas : TCanvas; vstExplorer : TVirtualStringTree;
    Node : PVirtualNode; NodeData : PTreeData; sl : TStringList; ItemRect : TRect;
    iScopeImagesWidth : Integer; var iPos : Integer); InLine;

  Var
    R : TRect;
    HL: TTokenFontInfo;
    i: Integer;

  Begin
    If vstExplorer.Selected[Node] Then
      Begin
        // Need to amend the width of the rectangle for the custom drawing
        iPos := 6;
        For i := 0 To sl.Count - 1 Do
          Begin
            GetFontInfo(sl, i, NodeData.FNode.Title, TargetCanvas);
            Inc(iPos, TargetCanvas.TextWidth(sl[i]));
          End;
        R := ItemRect;
        R.Left := (NodeData.FNode.Level + 1) * Integer(vstExplorer.Indent) -
          vstExplorer.Left + iScopeImagesWidth + vstExplorer.Margin + vstExplorer.TextMargin - 1;
        R.Right := R.Left + iPos;
        TargetCanvas.Pen.Color := clBlack;
        HL := TBADIOptions.BADIOptions.TokenFontInfo[ttExplorerHighlight];
        If Node = vstExplorer.FocusedNode Then
          Begin
            TargetCanvas.Brush.Color := HL.FBackColour;
            TargetCanvas.Pen.Color := HL.FForeColour;
            TargetCanvas.FillRect(R);
          End;
        TargetCanvas.Rectangle(R);
      End;
  End;

  (**

    This method draws the tree outline for the given node.

    @precon  NodeData must be a v alid pointer to a TTreeData record for the current node.
    @postcon The tree outline is drawn.

    @param   NodeData as a PTreeData
    @param   R        as a TRect as a reference

  **)
  Procedure DrawTree(NodeData : PTreeData; var R : TRect);

    (**

      This method draw the vertical line between tree nodes.

      @precon  None.
      @postcon The vertical line between the nodes is drawn for different levels.

      @param   iTreeColour as a TColor

    **)
    Procedure DrawVerticalTreeLine(iTreeColour : TColor);

    Var
      P: PVirtualNode;
      i: Integer;

    Begin
      P := Node.Parent;
      For i := NodeData.FNode.Level - 1 DownTo 0 Do
        Begin
          If (P <> Nil) And (P.Parent <> Nil) Then
            If P.Index < P.Parent.ChildCount - 1  Then
              Begin
                TargetCanvas.Pen.Color := iTreeColour;
                TargetCanvas.Pen.Style := psSolid;
                TargetCanvas.MoveTo(Integer(FExplorer.Indent) * i + 8 - Sender.Left, R.Top);
                TargetCanvas.LineTo(Integer(FExplorer.Indent) * i + 8 - Sender.Left, R.Bottom);
              End;
          P := P.Parent;
        End;
    End;

    (**

      This method draws the top half of the node connector.

      @precon  None.
      @postcon The top half of the node connector is drawn.

      @param   iTreeColour as a TColor
      @param   iCentre     as an Integer

    **)
    Procedure DrawTopHalfOfNodeConnector(iTreeColour : TColor; iCentre : Integer);

    Begin
      TargetCanvas.Pen.Color := iTreeColour;
      TargetCanvas.Pen.Style := psSolid;
      TargetCanvas.MoveTo(R.Left + 8, iCentre);
      TargetCanvas.LineTo(R.Left + Integer(FExplorer.Indent), iCentre);
      If Node.Parent <> Nil Then
        Begin
          // Draw connection to item
          TargetCanvas.Pen.Color := iTreeColour;
          TargetCanvas.Pen.Style := psSolid;
          TargetCanvas.MoveTo(R.Left + 8, R.Top);
          TargetCanvas.LineTo(R.Left + 8, iCentre);
          If Node.Index < Node.Parent.ChildCount - 1 Then
            Begin
              // Draw connector to next node.
              TargetCanvas.Pen.Color := iTreeColour;
              TargetCanvas.Pen.Style := psSolid;
              TargetCanvas.MoveTo(R.Left + 8, iCentre);
              TargetCanvas.LineTo(R.Left + 8, R.Bottom);
            End;
        End;
    End;

    (**

      This method draws the treenode button containing the + or - signs for expanding and
      collapsing.

      @precon  None.
      @postcon The button is drawn.

      @param   iTreeColour as a TColor
      @param   iCentre     as an Integer

    **)
    Procedure DrawNodeButton(iTreeColour : TColor; iCentre : Integer);

    Begin
      If Node.ChildCount > 0 Then
        Begin
          // Draw button
          TargetCanvas.Pen.Color := iTreeColour;
          TargetCanvas.Pen.Style := psSolid;
          TargetCanvas.Rectangle(R.Left + 4, iCentre - 4,
            R.Left + 13, iCentre + 5);
          // Draw negative side
          TargetCanvas.Pen.Color := iTreeColour;
          TargetCanvas.MoveTo(R.Left + 6, iCentre);
          TargetCanvas.LineTo(R.Left + 11, iCentre);
          If Not Sender.Expanded[Node] Then
            Begin
              // Make positive sign
              TargetCanvas.MoveTo(R.Left + 8, iCentre - 2);
              TargetCanvas.LineTo(R.Left + 8, iCentre + 3);
            End;
        End;
    End;

  Var
    iTreeColour: Integer;
    iCentre: Integer;

  Begin
    R.Left := R.Left + (NodeData.FNode.Level * Integer(FExplorer.Indent)) - Sender.Left;
    iTreeColour := TBADIOptions.BADIOptions.TreeColour;
    iCentre := (R.Top + R.Bottom) Div 2;
    DrawVerticalTreeLine(iTreeColour);
    DrawTopHalfOfNodeConnector(iTreeColour, iCentre);
    DrawNodeButton(iTreeColour, iCentre);
  End;

  (**

    This method draws the icon for the current tree node.

    @precon  NodeData must be the TTreeData for the current node.
    @postcon The nodes icon is drawn.

    @param   NodeData as a PTreeData
    @param   R        as a TRect as a reference

  **)
  Procedure DrawImage(NodeData : PTreeData; var R : TRect);

  Begin
    R.Left := R.Left + Integer(FExplorer.Indent) + FExplorer.Margin;
    Inc(R.Top);
    ilScopeImages.Draw(TargetCanvas, R.Left, R.Top, NodeData.FNode.ImageIndex);
  End;

  (**

    This procedure draws the text on the explorer tree view.

    @precon  NodeData and sl must be valid instance.
    @postcon The treenode text is drawn.

    @param   NodeData as a PTreeData
    @param   R        as a TRect
    @param   sl       as a TStringList
    @param   iPos     as an Integer as a reference

  **)
  Procedure DrawText(NodeData : PTreeData; R : TRect; sl : TStringList; var iPos : Integer);

    (**

      This method draws the given text to the canvas using the current R rectangle and the current
      iPos for the left of the text.

      @precon  None.
      @postcon Draws the text and updates the iPos pixel position for the next token and the
               iTextPos to point to the net character in the string to draw.

      @param   strText  as a String
      @param   iTextPos as an Integer as a reference

    **)
    Procedure DrawTextToCanvas(strText : String; var iTextPos : Integer);

    Var
      S : TRect;

    Begin
      S := R;
      S.Left := iPos;
      Windows.DrawText(TargetCanvas.Handle, PChar(strText), Length(strText), S,
        DT_LEFT Or DT_TOP);
      Inc(iPos, TargetCanvas.TextWidth(strText));
      Inc(iTextPos, Length(strText));
    End;

  Type
    TMatchType = (mtNone, mtStart, mtFull, mtEnd, mtMiddle);

    TMatchResult = Record
      FMatchType : TMatchType;
      FStart     : Integer;
      FLength    : Integer;
    End;

    (**

      This method initalises a TMatchResult record as we cannot define constructors for annoymous
      records.

      @precon  None.
      @postcon The record is initialised.

      @param   eMatchType as a TMatchType
      @param   iStart     as an Integer
      @param   iLength    as an Integer
      @return  a TMatchResult

    **)
    Function InitMatchResult(eMatchType : TMatchType; iStart, iLength: Integer) : TMatchResult;
      InLine;

    Begin
      Result.FMatchType := eMatchType;
      Result.FStart := iStart;
      Result.FLength := iLength;
    End;

    (**

      This method searches the node text for matches and returns the match results.

      @precon  MC must be a valid instance.
      @postcon Searches the node text for matches and returns the match results.

      @param   iIndex  as an Integer
      @param   iLength as an Integer
      @param   MC      as a TMatchCollection
      @return  a TMatchResult

    **)
    Function IsMatched(iIndex, iLength: Integer; MC: TMatchCollection): TMatchResult; InLine;

    Var
      iMatch: Integer;
      M: TMatch;

    Begin
      Result.FMatchType := mtNone;
      For iMatch := 0 To MC.Count - 1 Do
        Begin
          M := MC.Item[iMatch];
          If (M.Index + M.Length >= iIndex) And (M.Index <= iIndex + iLength) Then
            Begin
              If (M.Index <= iIndex) And (M.Index + M.Length >= iIndex + iLength) Then
                Result := InitMatchResult(mtFull, 1, iLength)
              Else If (M.Index <= iIndex) And (M.Index + M.Length < iIndex + iLength) Then
                Result := InitMatchResult(mtStart, 1, M.Index + M.Length - iIndex)
              Else If (M.Index > iIndex) And (M.Index + M.Length >= iIndex + iLength) Then
                Result := InitMatchResult(mtEnd, M.Index - iIndex + 1, iIndex + iLength - M.Index)
              Else
                Result := InitMatchResult(mtMiddle, M.Index - iIndex + 1, M.Length);
              Break;
            End;
        End;
    End;

  Var
    i: Integer;
    iTextPos: Integer;
    MC: TMatchCollection;
    MR: TMatchResult;
    iColour: Integer;

  Begin
    R.Left := R.Left + ilScopeImages.Width + FExplorer.TextMargin;
    iPos := R.Left + 2;
    iTextPos := 1;
    If edtExplorerFilter.Text <> '' Then
      MC := FFilterRegEx.Matches(NodeData.FNode.Text);
    For i := 0 To sl.Count - 1 Do
      Begin
        GetFontInfo(sl, i, NodeData.FNode.Title, TargetCanvas);
        If Node = Sender.FocusedNode Then
          If TargetCanvas.Brush.Color = TBADIOptions.BADIOptions.BGColour Then
            TargetCanvas.Brush.Color :=
              TBADIOptions.BADIOptions.TokenFontInfo[ttExplorerHighlight].FBackColour;
        If edtExplorerFilter.Text = '' Then
          DrawTextToCanvas(sl[i], iTextPos)
        Else
          Begin
            MR := IsMatched(iTextPos, Length(sl[i]), MC);
            Case MR.FMatchType Of
              mtNone: DrawTextToCanvas(sl[i], iTextPos);
              mtStart:
                Begin
                  iColour := TargetCanvas.Brush.Color;
                  TargetCanvas.Brush.Color := clAqua;
                  DrawTextToCanvas(Copy(sl[i], 1, MR.FLength), iTextPos);
                  TargetCanvas.Brush.Color := iColour;
                  DrawTextToCanvas(Copy(sl[i], MR.FLength + 1, Length(sl[i]) - MR.FLength),
                    iTextPos);
                End;
              mtFull:
                Begin
                  TargetCanvas.Brush.Color := clAqua;
                  DrawTextToCanvas(sl[i], iTextPos)
                End;
              mtEnd:
                Begin
                  DrawTextToCanvas(Copy(sl[i], 1, MR.FStart - 1), iTextPos);
                  TargetCanvas.Brush.Color := clAqua;
                  DrawTextToCanvas(Copy(sl[i], MR.FStart, MR.FLength), iTextPos);
                End;
              mtMiddle:
                Begin
                  DrawTextToCanvas(Copy(sl[i], 1, MR.FStart - 1), iTextPos);
                  iColour := TargetCanvas.Brush.Color;
                  TargetCanvas.Brush.Color := clAqua;
                  DrawTextToCanvas(Copy(sl[i], MR.FStart, MR.FLength), iTextPos);
                  TargetCanvas.Brush.Color := iColour;
                  DrawTextToCanvas(Copy(sl[i], MR.FStart + MR.FLength,
                    Length(sl[i]) - MR.FStart - MR.FLength + 1),
                    iTextPos);
                End;
            End;
          End;
      End;
  End;

Var
  iPos: Integer;
  NodeData : PTreeData;
  R : TRect;
  sl : TStringList;

begin
  CustomDraw := (doCustomDrawing In TBADIOptions.BADIOptions.Options);
  If Not CustomDraw Then
    Exit;
  NodeData := Sender.GetNodeData(Node);
  sl := NodeData.FNode.Tokens;
  InitCanvasFont(TargetCanvas);
  HighlightSelectedItem(TargetCanvas, FExplorer.Color, ItemRect);
  RenderSelectedNode(TargetCanvas, FExplorer, Node, NodeData, sl, ItemRect, ilScopeImages.Width,
    iPos);
  R := ItemRect;
  DrawTree(NodeData, R);
  DrawImage(NodeData, R);
  DrawText(NodeData, R, sl, iPos);
end;

(**

  This is an on click event handler for the explorer tree view.

  @precon  None.
  @postcon Fires a SelectionChange event for the specifically selected item.

  @param   Sender as a TObject

 **)
procedure TframeModuleExplorer.tvExplorerClick(Sender: TObject);

Var
  NodeData : PTreeData;

begin
  If FSelectionChanging Then
    Exit;
  FSelectionChanging := True;
  Try
    If FExplorer.FocusedNode <> Nil Then
      If Assigned(FSelectionChange) And Not FRendering Then
        Begin
          NodeData := FExplorer.GetNodeData(FExplorer.FocusedNode);
          If NodeData.FNode <> Nil Then
            If NodeData.FNode.Comment = Nil Then
              FSelectionChange(NodeData.FNode.Line, NodeData.FNode.Col,
                NodeData.FNode.Line, NodeData.FNode.Col)
            Else
              FSelectionChange(NodeData.FNode.Line, NodeData.FNode.Col,
                NodeData.FNode.Comment.Line, NodeData.FNode.Comment.Column);
        End;
  Finally
    FSelectionChanging := False;
  End;
end;

(**

  This method is an on key press event handler for the tree view.

  @precon  None.
  @postcon If an on focus event handler is assigned and enter is pressed it is fired else edits the
           explorer tree view filter.

  @param   Sender as a TObject
  @param   Key    as a Char as a reference

**)
Procedure TframeModuleExplorer.tvExplorerKeyPress(Sender: TObject; Var Key: Char);

Begin
  Case Key Of
    #08:
      Begin
        edtExplorerFilter.Text :=
          Copy(edtExplorerFilter.Text, 1, Length(edtExplorerFilter.Text) - 1);
        Key := #0;
      End;
    #13:
      Begin
        tvExplorerClick(Sender);
        If Assigned(OnFocus) Then
          FFocus(Sender);
        Key := #0;
      End;
    #27:
      Begin
        edtExplorerFilter.Clear;
        Key := #0;
      End;
    #32..#128:
      Begin
        edtExplorerFilter.Text := edtExplorerFilter.Text + Key;
        Key := #0;
      End;
  End;
End;

{ TBADIVirtualStringTree }

(**

  This method overrides the DoGetNodeWidth of the tree view and calculates the
  modified node width.

  @precon  None.
  @postcon Returns the modified node width.

  @param   Node   as a PVirtualNode
  @param   Column as a TColumnIndex
  @param   Canvas as a TCanvas
  @return  an Integer

**)
function TBADIVirtualStringTree.DoGetNodeWidth(Node: PVirtualNode;
  Column: TColumnIndex; Canvas: TCanvas): Integer;

Var
  NodeData : PTreeData;
  sl: TStringList;
  i: Integer;

begin
  Result := 5;
  NodeData := GetNodeData(Node);
  Self.Canvas.Font.Name := TBADIOptions.BADIOptions.FontName;
  Self.Canvas.Font.Size := TBADIOptions.BADIOptions.FontSize;
  sl := NodeData.FNode.Tokens;
  For i := 0 To sl.Count - 1 Do
    Begin
      GetFontInfo(sl, i, NodeData.FNode.Title, Self.Canvas);
      Inc(Result, Self.Canvas.TextWidth(sl[i]) + 1);
    End;
end;

end.