(**

  This module contains a frame which holds all the functionality of the
  module browser so that it can be independant of the application specifics.

  @Date    23 Dec 2008
  @Author  David Hoyle
  @Version 1.0

**)
unit ModuleExplorerFrame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ComCtrls, ExtCtrls, Contnrs, BaseLanguageModule,
  ActnList, ToolWin, VirtualTrees;

{$INCLUDE CompilerDefinitions.Inc}

type
  (** This class represents information about each nodes. Used in a collection
      instead of hanging the data off the tree nodes.  **)
  TTreeNodeInfo = Class
  {$IFDEF D2005} Strict {$ENDIF} Private
    FText       : String;
    FLine       : Integer;
    FCol        : Integer;
    FComment    : TComment;
    FImageIndex : Integer;
    FTokens     : TStringList;
    FLevel      : Integer;
  {$IFDEF D2005} Strict {$ENDIF} Protected
    function GetTokens: TStringList;
  Public
    Constructor Create(strText : String; iLevel : Integer; iImageIndex : Integer;
      iLine : Integer = 0; iColumn : Integer = 0; AComment : TComment = Nil); Overload;
    Destructor Destroy; Override;
    (**
      This property returns the text representation of the tree element.
      @precon  None.
      @postcon Returns the text representation of the tree element.
      @return  a String
    **)
    Property Text : String Read FText;
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
    Function CanDrawSpecialTag(Comment : TComment; strSpecialTag : String) : Boolean;
  End;

  (** This is a procedure type for the positioning of the cursor in the
      current module. **)
  TSelectionChange = Procedure(iIdentLine, iIdentCol, iCommentLine,
    iCommentCol : Integer) Of Object;

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
    tvExplorer: TVirtualStringTree;
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
    procedure tvExplorerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure tvExplorerClick(Sender: TObject);
    procedure actLocalUpdate(Sender: TObject);
    procedure actLocalExecute(Sender: TObject);
    procedure tvExplorerKeyPress(Sender: TObject; var Key: Char);
    procedure tvExplorerGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure tvExplorerGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure tvExplorerBeforeItemPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; ItemRect: TRect;
      var CustomDraw: Boolean);
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
    { Private declarations }
    procedure GetBodyCommentTags(M : TBaseLanguageModule);
    Function AddNode(P : PVirtualNode; strText : String; iLevel : Integer;
      iImageIndex : Integer; iLine : Integer = 0; iColumn : Integer = 0;
      AComment : TComment = Nil) : PVirtualNode;
    procedure CreateSpecialTagNodes(M : TBaseLanguageModule);
    procedure ExpandNodes;
    procedure OutputModuleInfo(Container : TElementContainer);
    function FindTreeItem(strText: String): PVirtualNode;
    procedure GetExpandedNodes(StartNode : PVirtualNode);
    function GetNodePath(Node: PVirtualNode): String;
    procedure SetExpandedNodes(StartNode : PVirtualNode);
    Procedure RenderContainers(RootNode : PVirtualNode;
      Container : TElementContainer; iLevel : Integer);
    Procedure UpdateStatusBar(M : TBaseLanguageModule);
    Procedure ManageExpandedNodes;
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
  end;

implementation

Uses
  IniFiles, Types, Math, DGHLibrary, GenericTokenizer
  {$IFDEF PROFILECODE}, Profiler {$ENDIF};

Type
  (** This record described the data sorted in the virtual tree view. **)
  TTreeData = Record
    FNode : TTreeNodeInfo;
  End;

Resourcestring
  (** A format pattern for the statusbar text. **)
  strStatusbarText = '%1.0n Bytes, %1.0n Tokens and %1.0n Lines.';

{$R *.dfm}

Var
  (** A private variable which is assigned the key words array. **)
  strKeyWords : TKeyWords;

(**

  This procedure sets the font of the passed canvas to the appropiate style and
  colour for the words stored in the string list.

  @precon  sl is a string list of the tokenized word to display, i is the index
           of the word to change the canvas for, Level is the current
           indentation level of the tree node and Canvas is the canvas to be
           affected but the other parameters.
  @postcon Sets the font of the passed canvas to the appropiate style and
           colour for the words stored in the string list.

  @param   sl     as a TStringList
  @param   i      as an Integer
  @param   Level  as an Integer
  @param   Canvas as a TCanvas

**)
Procedure GetFontInfo(sl : TStringList; i : Integer; Level : Integer;
  Canvas : TCanvas);

Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('GetFontInfo');
  Try
  {$ENDIF}
  With BrowseAndDocItOptions Do
    Begin
      If Level <> 1 Then
        Begin
          Case TTokenType(sl.Objects[i]) Of
            ttIdentifier:
              Begin
                Canvas.Font.Color := TokenFontInfo[ttIdentifier].FColour;
                Canvas.Font.Style := TokenFontInfo[ttIdentifier].FStyles;
              End;
            ttReservedWord:
              Begin
                Canvas.Font.Color := TokenFontInfo[ttReservedWord].FColour;
                Canvas.Font.Style := TokenFontInfo[ttReservedWord].FStyles;
              End;
            ttDirective :
              Begin
                Canvas.Font.Color := TokenFontInfo[ttDirective].FColour;
                Canvas.Font.Style := TokenFontInfo[ttDirective].FStyles;
              End;
            ttSymbol :
              Begin
                Canvas.Font.Color := TokenFontInfo[ttSymbol].FColour;
                Canvas.Font.Style := TokenFontInfo[ttSymbol].FStyles;
              End;
            ttNumber :
              Begin
                Canvas.Font.Color := TokenFontInfo[ttNumber].FColour;
                Canvas.Font.Style := TokenFontInfo[ttNumber].FStyles;
              End;
            ttStringLiteral :
              Begin
                Canvas.Font.Color := TokenFontInfo[ttStringLiteral].FColour;
                Canvas.Font.Style := TokenFontInfo[ttStringLiteral].FStyles;
              End;
          Else
            Canvas.Font.Color := clBlack;
            Canvas.Font.Style := [];
          End;
        End Else
        Begin
          Canvas.Font.Color := TokenFontInfo[ttTreeHeader].FColour;
          Canvas.Font.Style := TokenFontInfo[ttTreeHeader].FStyles;
        End;
   End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
End;

(**

  This is the constructor method for the TTreeNodeInfo class.

  @precon  This line number of the comment, This column number of the comment
           and This comment to be added to the node info object.
  @postcon Initialises the class.

  @param   strText     as a String
  @param   iLevel      as an Integer
  @param   iImageIndex as an Integer
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   AComment    as a TComment

**)
Constructor TTreeNodeInfo.Create(strText : String; iLevel : Integer;
  iImageIndex : Integer; iLine : Integer = 0; iColumn : Integer = 0;
  AComment : TComment = Nil);

Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TTreeNodeInfo.Create');
  Try
  {$ENDIF}
  Inherited Create;
  FText := strText;
  FLevel := iLevel;
  FLine := iLine;
  FCol := iColumn;
  FComment := TComment.Create(AComment);
  FImageIndex:= iImageIndex;
  FTokens := Nil;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
End;

(**

  This is the destructor method for the TTreeNodeInfo class.

  @precon  None.
  @postcon Destroys the instance of the Tree node info class.

 **)
Destructor TTreeNodeInfo.Destroy;

Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TTreeNodeInfo.Destroy');
  Try
  {$ENDIF}
  FTokens.Free;
  FComment.Free;
  Inherited Destroy;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
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
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('GetTokens');
  Try
  {$ENDIF}
  If FTokens = Nil Then
    FTokens := Tokenize(FText, strKeyWords, BrowseAndDocItOptions.TokenLimit);
  Result := FTokens;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This method is the paint method for the customised hint window.

  @precon  None.
  @postcon Draws the custom hint window.

 **)
Procedure TCustomHintWindow.Paint;

Var
  sl : TStringList;
  iPos : Integer;
  i, j : Integer;
  R : TRect;
  str : String;
  iLines : Integer;
  iLine : Integer;
  iHeight: Integer;
  NodeData : ^TTreeData;

Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TCustomHintWindow.Paint');
  Try
  {$ENDIF}
  iLines := 1;
  iLine := 0;
  With Canvas Do
    Begin
      Font.Assign(FTreeView.Font);
      NodeData := FTreeView.GetNodeData(FNode);
      iPos := 2;
      If FCustomDraw Then
        Begin
          sl := NodeData.FNode.Tokens;
          For i := 0 To sl.Count - 1 Do
            Begin
              If FCustomDraw Then
                GetFontInfo(sl, i, FNodeLevel, Canvas)
              Else
                Begin
                  Refresh;
                  Font.Color := clInfoText;
                  Font.Style := [];
                End;
              If iPos + TextWidth(sl[i]) > Width Then
                Begin
                  iPos := 2 + 10; // Indent multiple lines
                  Inc(iLines);
                End;
              iLine := (iLines - 1) * TextHeight(sl[i]);
              TextOut(iPos, iLine, sl[i]);
              Inc(iPos, TextWidth(sl[i]) + 1);
            End;
          End Else
            TextOut(iPos, iLine, NodeData.FNode.Text);
      If (FComment <> Nil) And ((FComment.TokenCount > 0) Or
        (FComment.TagCount > 0)) Then
        Begin
          iHeight := TextHeight('Wp');
          FillRect(Rect(0, iHeight + 2 + iLine, Width, Height));
          Pen.Color := clMaroon;
          MoveTo(0, iHeight + 4 + iLine);
          Lineto(Width, iHeight + 4 + iLine);
          Refresh;
          Font.Style := [];
          Font.Color := clNavy;
          str := FComment.AsString(0, MaxInt, False);
          R := Rect(2, iHeight + 6 + iLine, Width - 2, Height);
          iPos := DrawText(Canvas.Handle, PChar(str), -1, R,
            DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
            DrawTextBiDiModeFlagsReadingOnly);
          R := Rect(2, iHeight + 6 + iLine + iPos, Width - 4, Height);
          For i := 0 To BrowseAndDocItOptions.SpecialTags.Count - 1 Do
            Begin
              If CanDrawSpecialTag(FComment, BrowseAndDocItOptions.SpecialTags.Names[i]) Then
                Begin
                  Refresh;
                  Font.Style := [fsBold, fsUnderline];
                  Font.Color := clPurple;
                  Inc(R.Top, 5);
                  R := Rect(2, R.Top, Width - 2, Height);
                  str := BrowseAndDocItOptions.SpecialTags.Values[BrowseAndDocItOptions.SpecialTags.Names[i]];
                  Inc(R.Top, DrawText(Canvas.Handle, PChar(str), -1, R,
                    DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
                    DrawTextBiDiModeFlagsReadingOnly) + 1);
                  For j := 0 To FComment.TagCount - 1 Do
                    If AnsiCompareText(BrowseAndDocItOptions.SpecialTags.Names[i], FComment.Tag[j].TagName) = 0 Then
                      Begin
                        Pen.Color := clBlack;
                        Brush.Color := clBlack;
                        Ellipse(3, R.Top + 5, 7, R.Top + 9);
                        Brush.Color := clInfoBk;
                        Refresh;
                        Font.Style := [];
                        Font.Color := clMaroon;
                        R := Rect(10, R.Top, Width - 2, Height);
                        str := FComment.Tag[j].AsString(False);
                        Inc(R.Top, DrawText(Canvas.Handle, PChar(str), -1, R,
                          DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
                          DrawTextBiDiModeFlagsReadingOnly));
                      End;
                End;
            End;
        End;
    End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
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

Var
  sl : TStringList;
  iPos, iMaxPos, iLastMax : Integer;
  i, j : Integer;
  str : String;
  R : TRect;
  NodeData : ^TTreeData;

Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TCustomHintWindow.CalcHintRect');
  Try
  {$ENDIF}
  Canvas.Font.Assign(FTreeView.Font);
  Result := FTreeView.GetDisplayRect(Node, NoColumn, True);
  Inc(Result.Left, 2); // Adjustment for 
  Inc(Result.Top, 1);
  iMaxPos := FTreeView.ScreenToClient(Point(Screen.WorkAreaRect.Right, 0)).X -
    Result.Left;
  If SyntaxHighlight Then
    Begin
      // Need to amend the width of the rectangle for the custom drawing
      iPos := 5;
      NodeData := FTreeView.GetNodeData(Node);
      sl := NodeData.FNode.Tokens;
      iLastmax := 0;
      For i := 0 To sl.Count - 1 Do
        Begin
          GetFontInfo(sl, i, NodeData.FNode.Level, Canvas);
          Inc(iPos, Canvas.TextWidth(sl[i]) + 1);
          If iPos > iMaxPos Then
            Begin
              Inc(Result.Bottom, Canvas.TextHeight(sl[i]) + 2);
              iPos := 5 + 10; // indent multiple lines
              iLastMax := iMaxPos;
            End Else
              If iPos > iLastMax Then
                iLastMax := iPos;
        End;
      Result.Right := Result.Left + iLastMax;
    End;
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
      str := Comment.AsString(0, MaxInt, False);
      R := Rect(Result.Left + 2, 0, Result.Right - 2, 0);
      Inc(Result.Bottom, DrawText(Canvas.Handle, PChar(str), -1, R,
        DT_CALCRECT Or DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
        DrawTextBiDiModeFlagsReadingOnly) + 2);
      For i := 0 To BrowseAndDocItOptions.SpecialTags.Count - 1 Do
        Begin
          If CanDrawSpecialTag(Comment, BrowseAndDocItOptions.SpecialTags.Names[i]) Then
            Begin
              Refresh;
              Canvas.Font.Style := [fsBold, fsUnderline];
              R := Rect(Result.Left + 2, 0, Result.Right - 2, 0);
              str := BrowseAndDocItOptions.SpecialTags.Values[BrowseAndDocItOptions.SpecialTags.Names[i]];
              Inc(Result.Bottom, 5 + DrawText(Canvas.Handle, PChar(str), -1, R,
                DT_CALCRECT Or DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
                DrawTextBiDiModeFlagsReadingOnly) + 2);
              For j := 0 To Comment.TagCount - 1 Do
                If AnsiCompareText(BrowseAndDocItOptions.SpecialTags.Names[i], Comment.Tag[j].TagName) = 0 Then
                  Begin
                    Refresh;
                    Canvas.Font.Style := [];
                    R := Rect(Result.Left + 2, 0, Result.Right - 12, 0);
                    str := Comment.Tag[j].AsString(False);
                    Inc(Result.Bottom, DrawText(Canvas.Handle, PChar(str), -1, R,
                      DT_CALCRECT Or DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
                      DrawTextBiDiModeFlagsReadingOnly) + 2);
                  End;
            End;
        End;
    End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
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
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TCustomHintWindow.Create');
  Try
  {$ENDIF}
  Inherited Create(AOwner);
  FTreeView := ATreeView;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
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
 NodeData : ^TTreeData;

Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TCustomHintWindow.ActivateHint');
  Try
  {$ENDIF}
  FComment := Comment;
  NodeData := FTreeView.GetNodeData(Node);
  FNodeLevel := NodeData.FNode.Level;
  FCustomDraw := SyntaxHighlight;
  FNode := Node;
  ActivateHint(Rect, NodeData.FNode.Text);
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
End;

(**

  This method determines if the special tag need to be rendered in the hint
  window.

  @precon  Comment is the comment to get tags from and strSpecialTag is the
           special tag information to look for.
  @postcon Return true is the special tag was found in the comment.

  @param   Comment       as a TComment
  @param   strSpecialTag as a String
  @return  a Boolean

 **)
Function TCustomHintWindow.CanDrawSpecialTag(Comment : TComment;
  strSpecialTag : String) : Boolean;
Var
  i : Integer;

Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TCustomHintWindow.DrawSpecialTag');
  Try
  {$ENDIF}
  Result := False;
  For i := 0 To Comment.TagCount - 1 Do
    If AnsiCompareText(strSpecialTag, Comment.Tag[i].TagName) = 0 Then
      Begin
        Result := True;
        Exit;
      End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
End;

(**

  This is the constructor method for the TframeModuleExplorer class.

  @precon  None.
  @postcon Initialises the class.

  @param   AOwner as a TComponent

**)
Constructor TframeModuleExplorer.Create(AOwner: TComponent);

Var
  i : TImageIndex;

begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TframeModuleExplorer.Create');
  Try
  {$ENDIF}
  Inherited;
  tvExplorer.NodeDataSize := SizeOf(TTreeData);
  FINIFileName := BuildRootKey(Nil, Nil);
  FNodeInfo := TObjectList.Create(True);
  FHintWin := TCustomHintWindow.Create(Self, tvExplorer);
  FHintWin.Color := clInfoBk;
  FHintWin.Canvas.Font.Assign(tvExplorer.Font);
  ilScopeImages.Clear;
  For i := Succ(Low(TImageIndex)) to High(TImageIndex) Do
    If Not ilScopeImages.GetInstRes(hInstance, rtBitmap, ImageList[i].FResourceName, 16,
      [lrDefaultColor], ImageList[i].FMaskColour) Then
      ShowMessage(Format('Resource "%s" not found.', [ImageList[i].FResourceName]))
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This is the destructor method for the TframeModuleExplorer class.

  @precon  None.
  @postcon destroy the instance of the dockable form.

**)
Destructor TframeModuleExplorer.Destroy;

begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TframeModuleExplorer.Destroy');
  Try
  {$ENDIF}
  FSpecialTagNodes := Nil;
  ManageExpandedNodes;
  GetExpandedNodes(FModule);
  FHintWin.Free;
  FNodeInfo.Free;
  Inherited;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

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
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TframeModuleExplorer.GetBodyCommentTags');
  Try
  {$ENDIF}
  For i := 0 To M.BodyCommentCount - 1 Do
    With M.BodyComment[i] Do
      For j := 0 To TagCount - 1 Do
        For k := Low(FSpecialTagNodes) To High(FSpecialTagNodes) Do
          If FSpecialTagNodes[k].boolShow Then
            If AnsiCompareText(Tag[j].TagName, FSpecialTagNodes[k].strTagName) = 0 Then
              AddNode(FSpecialTagNodes[k].Node, Tag[j].AsString(False), 2, 
                Integer(iiToDoItem) - 1, M.BodyComment[i].Tag[j].Line,
                M.BodyComment[i].Tag[j].Column, Nil);
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
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
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TframeModuleExplorer.RenderContainers');
  Try
  {$ENDIF}
  For i := 1 To Container.ElementCount Do
    If Container.Elements[i].Scope In BrowseAndDocItOptions.ScopesToRender +
      [scNone, scGlobal] Then
      Begin
        NewNode := AddNode(RootNode, Container.Elements[i].AsString, iLevel,
          Container.Elements[i].ImageIndexAdjustedForScope,
          Container.Elements[i].Line, Container.Elements[i].Column,
          Container.Elements[i].Comment);
        RenderContainers(NewNode, Container[i], iLevel + 1);
      End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
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
  NodeData : ^TTreeData;

Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TframeModuleExplorer.GetNodePath');
  Try
  {$ENDIF}
  str := '';
  P := Node;
  While P <> Nil Do
    Begin
      If tvExplorer.NodeParent[P] <> Nil Then
        Begin
          NodeData := tvExplorer.GetNodeData(P);
          str := NodeData.FNode.Text + '.' + str;
        End;
      P := tvExplorer.NodeParent[P];
    End;
  Result := str;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
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
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('ManageExpandedNodes');
  Try
  {$ENDIF}
  With BrowseAndDocItOptions.ExpandedNodes Do
  For i := Count - 1 DownTo 0 Do
    Begin
      dtDate := Integer(Objects[i]);
      If dtDate < Now - 90 Then
        Delete(i);
    End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
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
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TframeModuleExplorer.GetExpandedNodes');
  Try
  {$ENDIF}
  Node := tvExplorer.GetFirstChild(StartNode);
  While Node <> Nil Do
    Begin
      If Node.ChildCount > 0 Then
        Begin
          str := GetNodePath(Node);
          If tvExplorer.Expanded[Node] And (str <> '') Then
            Begin
              If Not BrowseAndDocItOptions.ExpandedNodes.Find(str, iIndex) Then
                iIndex := BrowseAndDocItOptions.ExpandedNodes.Add(str);
              BrowseAndDocItOptions.ExpandedNodes.Objects[iIndex] := TObject(Trunc(Now));
            End Else
              If BrowseAndDocItOptions.ExpandedNodes.Find(str, iIndex) Then
                BrowseAndDocItOptions.ExpandedNodes.Delete(iIndex);
          GetExpandedNodes(Node);
        End;
      Node := tvExplorer.GetNextSibling(Node);
    End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
End;

(**

  This method expands the tree view nodes if they are foudn in the list.. 

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
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TframeModuleExplorer.SetExpandedNodes');
  Try
  {$ENDIF}
  Node := tvExplorer.GetFirstChild(StartNode);
  While Node <> Nil Do
    Begin
      If Node.ChildCount > 0 Then
        Begin
          str := GetNodePath(Node);
          If BrowseAndDocItOptions.ExpandedNodes.Find(str, i) Then
            tvExplorer.Expanded[Node] := True;
          SetExpandedNodes(Node);
        End;
      Node := tvExplorer.GetNextSibling(Node);
    End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
End;

(**

  This function finds the tree node that has the path specified by the passed 
  text. 

  @precon  strText is the string representation of the node path to be found. 
  @postcon Returns tree node index of the item corresponding to the given path. 

  @param   strText as a String
  @return  a PVirtualNode

**)
Function TframeModuleExplorer.FindTreeItem(strText : String) : PVirtualNode;

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
    {$IFDEF PROFILECODE}
    CodeProfiler.Start('FindNode');
    Try
    {$ENDIF}
      Result := Nil;
      C := tvExplorer.GetFirstChild(Node);
      While C <> Nil Do
        Begin
          If C.ChildCount > 0 Then
            Result := FindNode(C);
          If Result <> Nil Then
            Exit;
          If AnsiCompareText(GetNodePath(C), strText) = 0 Then
            Begin
              Result := C;
              Exit;
            End;
          C := tvExplorer.GetNextSibling(C);
        End;
    {$IFDEF PROFILECODE}
    Finally
      CodeProfiler.Stop;
    End;
    {$ENDIF}
  End;

Begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TframeModuleExplorer.FindTreeItem');
  Try
  {$ENDIF}
  Result := Nil;
  If strText <> '' Then
    Result := FindNode(FModule);
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
End;

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
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TframeModuleExplorer.RenderModule');
  Try
  {$ENDIF}
  tvExplorer.Color := BrowseAndDocItOptions.BGColour;
  If M = Nil Then
    strKeyWords := Nil
  Else
    strKeyWords := M.KeyWords;
  If FRendering Then
    Exit;
  FRendering := True;
  Try
    FHintWin.ReleaseHandle; // Stop AV when refreshing the tree.
    tvExplorer.Font.Name := BrowseAndDocItOptions.FontName;
    tvExplorer.Font.Size := BrowseAndDocItOptions.FontSize;
    GetExpandedNodes(FModule);
    FModule := Nil;
    // Find and store the top item and the selected item in the tree view
    strTop := GetNodePath(tvExplorer.TopNode);
    strSelection := GetNodePath(tvExplorer.FocusedNode);
    tvExplorer.BeginUpdate;
    Try
      tvExplorer.Clear;
      FNodeInfo.Clear;
      If M = Nil Then
        Exit;
      M.AddTickCount('Clear');
      SetLength(FSpecialTagNodes, BrowseAndDocItOptions.SpecialTags.Count);
      // Create Root Tree Node
      FModule := AddNode(Nil, M.AsString, 0, M.ImageIndexAdjustedForScope,
        M.Line, M.Column, M.Comment);
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
            tvExplorer.TopNode := N;
        End Else
          tvExplorer.TopNode := FModule;
      N := FindTreeItem(strSelection);
      If N <> Nil Then
        Begin
          tvExplorer.FocusedNode := N;
          tvExplorer.Selected[tvExplorer.FocusedNode] := True;
        End;
    Finally
      If M <> Nil Then
        M.AddTickCount('Setup');
      tvExplorer.EndUpdate;
    End;
    M.AddTickCount('Render');
    UpdateStatusBar(M);
  Finally
    FRendering := False;
  End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
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
  NodeData : ^TTreeData;

begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TframeModuleExplorer.OutputModuleInfo');
  Try
  {$ENDIF}
  RenderContainers(FModule, Container, 1);
  GetBodyCommentTags(Container As TBaseLanguageModule);
  For i := Low(strPromotedLabels) To High(strPromotedLabels) Do
    Begin
      Node := tvExplorer.GetFirstChild(FModule);
      While Node <> Nil Do
        Begin
          NodeData := tvExplorer.GetNodeData(Node);
          If NodeData.FNode.Text = strPromotedLabels[i] Then
            tvExplorer.MoveTo(Node, FModule, amAddChildFirst, False);
          Node := tvExplorer.GetNextSibling(Node);
        End;
    End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;


(**

  This method expands, collapses or delete various nodes depending on the
  options and their contents.

  @precon  None.
  @postcon Expands, collapses or delete various nodes depending on the
           options and their contents.

**)
procedure TframeModuleExplorer.ExpandNodes;

Const
  strPromotedLabels : Array[1..4] Of String = (strDocumentationConflicts,
    strHints, strWarnings, strErrors);

Var
  i : Integer;
  Node: PVirtualNode;
  NodeData: ^TTreeData;
  N: PVirtualNode;

begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TframeModuleExplorer.ExpandNodes');
  Try
  {$ENDIF}
  tvExplorer.Expanded[FModule] := True;
  For i := Low(FSpecialTagNodes) to High(FSpecialTagNodes) Do
    If FSpecialTagNodes[i].Node.ChildCount = 0 Then
      Begin
        tvExplorer.DeleteNode(FSpecialTagNodes[i].Node);
        FSpecialTagNodes[i].Node := Nil;
      End;
  For i := Low(FSpecialTagNodes) to High(FSpecialTagNodes) Do
    If FSpecialTagNodes[i].Node <> Nil Then
      If FSpecialTagNodes[i].boolExpand Then
        tvExplorer.Expanded[FSpecialTagNodes[i].Node] := True;
  For i := Low(strPromotedLabels) To High(strPromotedLabels) Do
    Begin
      Node := tvExplorer.GetFirstChild(FModule);
      While Node <> Nil Do
        Begin
          NodeData := tvExplorer.GetNodeData(Node);
          If AnsiCompareText(NodeData.FNode.Text, strDocumentationConflicts) = 0 Then
            If doShowConflicts In BrowseAndDocItOptions.Options Then
              Begin
                tvExplorer.Expanded[Node] := True;
                N := tvExplorer.GetFirstChild(Node);
                While N <> Nil Do
                  Begin
                    tvExplorer.Expanded[N] := True;
                    N := tvExplorer.GetNextSibling(N);
                  End;
              End;
          If AnsiCompareText(NodeData.FNode.Text, strHints) = 0 Then
            If doShowHints In BrowseAndDocItOptions.Options Then
              tvExplorer.Expanded[Node] := True;
          If AnsiCompareText(NodeData.FNode.Text, strWarnings) = 0 Then
            If doShowWarnings In BrowseAndDocItOptions.Options Then
              tvExplorer.Expanded[Node] := True;
          If AnsiCompareText(NodeData.FNode.Text, strErrors) = 0 Then
            If doShowErrors In BrowseAndDocItOptions.Options Then
              tvExplorer.Expanded[Node] := True;
          Node := tvExplorer.GetNextSibling(Node);
        End;
    End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
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
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TframeModuleExplorer.CreateSpecialTagNodes');
  Try
  {$ENDIF}
  For i := Low(FSpecialTagNodes) to High(FSpecialTagNodes) Do
    Begin
      FSpecialTagNodes[i].strTagName := BrowseAndDocItOptions.SpecialTags.Names[i];
      FSpecialTagNodes[i].strTagDesc :=
        BrowseAndDocItOptions.SpecialTags.Values[BrowseAndDocItOptions.SpecialTags.Names[i]];
      FSpecialTagNodes[i].boolShow :=
        Integer(BrowseAndDocItOptions.SpecialTags.Objects[i]) And iShowInTree <> 0;
      FSpecialTagNodes[i].boolExpand :=
        Integer(BrowseAndDocItOptions.SpecialTags.Objects[i]) And iAutoExpand <> 0;
      FSpecialTagNodes[i].Node := AddNode(FModule, FSpecialTagNodes[i].strTagDesc,
        1, Integer(iiTodoFolder) - 1);
    End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
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
  procedure UpdateScopes(AScope : TScope);

  Begin
    {$IFDEF PROFILECODE}
    CodeProfiler.Start('UpdateScopes');
    Try
    {$ENDIF}
    With BrowseAndDocItOptions Do
      If AScope In ScopesToRender Then
        ScopesToRender := ScopesToRender - [AScope]
      Else
        ScopesToRender := ScopesToRender + [AScope];
    {$IFDEF PROFILECODE}
    Finally
      CodeProfiler.Stop;
    End;
    {$ENDIF}
  End;

  (**

    This procedure adds or removed the given option from the options set.

    @precon  None.
    @postcon Adds or removed the given option from the Options set.

    @param   Option as a TDocOption

  **)
  procedure UpdateOptions(Option : TDocOption);

  Begin
    {$IFDEF PROFILECODE}
    CodeProfiler.Start('UpdateOptions');
    Try
    {$ENDIF}
    With BrowseAndDocItOptions Do
      If Option In Options Then
        Options := Options - [Option]
      Else
        Options := Options + [Option];
    {$IFDEF PROFILECODE}
    Finally
      CodeProfiler.Stop;
    End;
    {$ENDIF}
  End;

begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TframeModuleExplorer.actLocalExecute');
  Try
  {$ENDIF}
  With BrowseAndDocItOptions Do
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
      UpdateOptions(doShowPropertyMissingDoc);
  If Assigned(FRefresh) Then
    FRefresh(Sender);
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This is an on update event handler for the sceop actions.

  @precon  None.
  @postcon Checks the action depending on the scopes in ScopesToRender.

  @param   Sender as a TObject

**)
procedure TframeModuleExplorer.actLocalUpdate(Sender: TObject);
begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TframeModuleExplorer.actLocalUpdate');
  Try
  {$ENDIF}
  If Sender = actLocal Then
    (Sender As TAction).Checked := scLocal In BrowseAndDocItOptions.ScopesToRender
  Else If Sender = actPrivate Then
    (Sender As TAction).Checked := scPrivate In BrowseAndDocItOptions.ScopesToRender
  Else If Sender = actProtected Then
    (Sender As TAction).Checked := scProtected In BrowseAndDocItOptions.ScopesToRender
  Else If Sender = actPublic Then
    (Sender As TAction).Checked := scPublic In BrowseAndDocItOptions.ScopesToRender
  Else If Sender = actPublished Then
    (Sender As TAction).Checked := scPublished In BrowseAndDocItOptions.ScopesToRender
  Else If Sender = actSyntax Then
    (Sender As TAction).Checked := doCustomDrawing In BrowseAndDocItOptions.Options
  Else If Sender = actShowHints Then
    (Sender As TAction).Checked := doShowCommentHints In BrowseAndDocItOptions.Options
  Else If Sender = actConflicts Then
    (Sender As TAction).Checked := doShowConflicts In BrowseAndDocItOptions.Options
  Else If Sender = actErrors Then
    (Sender As TAction).Checked := doShowErrors In BrowseAndDocItOptions.Options
  Else If Sender = actWarnings Then
    (Sender As TAction).Checked := doShowWarnings In BrowseAndDocItOptions.Options
  Else If Sender = actHints Then
    (Sender As TAction).Checked := doShowHints In BrowseAndDocItOptions.Options
  Else If Sender = actMethods Then
    (Sender As TAction).Checked := doShowMethodMissingDocs In BrowseAndDocItOptions.Options
  Else If Sender = actProperties Then
    (Sender As TAction).Checked := doShowPropertyMissingDoc In BrowseAndDocItOptions.Options;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This method adds a node to the treeview as a child of the give node. It 
  assigns the line, column and comment information to the noNode. 

  @precon  P is the parent node to attach this new child too, Element is the 
           parser node to render. 
  @postcon Returns a instance of the newly add / created tree node. 

  @param   P           as a PVirtualNode
  @param   strText     as a String
  @param   iLevel      as an Integer
  @param   iImageIndex as an Integer
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   AComment    as a TComment
  @return  a PVirtualNode

**)
Function TframeModuleExplorer.AddNode(P : PVirtualNode; strText : String;
  iLevel : Integer; iImageIndex : Integer; iLine : Integer = 0;
  iColumn : Integer = 0; AComment : TComment = Nil) : PVirtualNode;

Var
  NodeData : ^TTreeData;
  N : TTreeNodeInfo;

begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TframeModuleExplorer.AddNode');
  Try
  {$ENDIF}
  Result := tvExplorer.AddChild(P);
  NodeData := tvExplorer.GetNodeData(Result);
  N := TTreeNodeInfo.Create(strText, iLevel, iImageIndex, iLine, iColumn, AComment);
  FNodeInfo.Add(N);
  NodeData.FNode := N;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
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
  NodeData : ^TTreeData;

begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('tvExplorerGetImageIndex');
  Try
  {$ENDIF}
  NodeData := tvExplorer.GetNodeData(Node);
  ImageIndex := NodeData.FNode.ImageIndex;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
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
  @param   CellText as a WideString as a reference

**)
procedure TframeModuleExplorer.tvExplorerGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);

Var
  NodeData : ^TTreeData;

begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('tvExplorerGetText');
  Try
  {$ENDIF}
  NodeData := tvExplorer.GetNodeData(Node);
  CellText := NodeData.FNode.Text;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
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
  NodeData : ^TTreeData;

begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TframeModuleExplorer.tvExplorerMouseMove');
  Try
  {$ENDIF}
  C := Nil;
  tvExplorer.GetHitTestInfoAt(X, Y, True, HitInfo);
  If (hiOnItemLabel In HitInfo.HitPositions) Then
    Begin
      Node := HitInfo.HitNode;
      If (Node <> Nil) And (Node <> FLastNode) Then
        Begin
          FLastNode := Node;
          NodeData := tvExplorer.GetNodeData(Node);
          If doShowCommentHints In BrowseAndDocItOptions.Options Then
            C := NodeData.FNode.Comment;
          Rect := FHintWin.CalcHintRect(tvExplorer.ClientWidth, Screen.Width,
            Node, doCustomDrawing In BrowseAndDocItOptions.Options, C);
          If (Rect.Right <= tvExplorer.ClientWidth) And ((C = Nil) Or
            ((C.TokenCount = 0) And (C.TagCount = 0))) Then
            Begin
              FHintWin.ReleaseHandle;
              Exit;
            End;
          Rect.TopLeft := tvExplorer.ClientToScreen(Rect.TopLeft);
          Rect.BottomRight := tvExplorer.ClientToScreen(Rect.BottomRight);
          FHintWin.ActivateHint(Rect, Node,
            doCustomDrawing In BrowseAndDocItOptions.Options, C);
          FLastNode := Node;
        End Else
          If (Node <> FLastNode) Then
            FHintWin.ReleaseHandle;
    End Else
    Begin
      FHintWin.ReleaseHandle;
      FLastNode := Nil;
    End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
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
  iTicks: Integer;
  i : Integer;

begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TframeModuleExplorer.UpdateStatusBar');
  Try
  {$ENDIF}
  If doShowPerformanceCountersInModuleExplorer In BrowseAndDocItOptions.Options Then
    Begin
      stbStatusBar.SimpleText := '';
      For i := 1 To M.OpTickCounts - 1 Do
        Begin
          strTickLabel := M.OpTickCountName[i];
          If strTickLabel <> '' Then
            strTickLabel := strTickLabel + ':';
          iTicks := M.OpTickCountByIndex[i] - M.OpTickCountByIndex[i - 1];
          If iTicks > 0 Then
            Begin
              If stbStatusBar.SimpleText <> '' Then
                stbStatusBar.SimpleText := stbStatusBar.SimpleText + ', ';
              stbStatusBar.SimpleText := stbStatusBar.SimpleText +
                Format('%s%d', [strTickLabel, iTicks]);
            End;
        End;
      If stbStatusBar.SimpleText <> '' Then
        stbStatusBar.SimpleText := stbStatusBar.SimpleText + ', ';
      stbStatusBar.SimpleText := stbStatusBar.SimpleText +
        Format('%s: %d', ['Total', M.OpTickCountByIndex[M.OpTickCounts - 1] -
        M.OpTickCountByIndex[0]]);
    End Else
    Begin
      stbStatusBar.SimpleText := Format(strStatusbarText, [
        Int(Integer(M.Bytes)), Int(M.TokenCount), Int(M.Lines)]);
    End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
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
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TframeModuleExplorer.CMMouseLeave');
  Try
  {$ENDIF}
  FHintWin.ReleaseHandle;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
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


Var
  iPos: Integer;
  i: Integer;
  NodeData : ^TTreeData;
  R : TRect;
  iCentre: Integer;
  P: PVirtualNode;
  sl : TStringList;
  iOffset: Integer;
  iTreeColour : TColor;

begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TframeModuleExplorer.tvExplorerBeforeItemPaint');
  Try
  {$ENDIF}
  CustomDraw := (doCustomDrawing In BrowseAndDocItOptions.Options);
  With TargetCanvas Do
    If CustomDraw Then
      Begin
        TargetCanvas.Font.Name := BrowseAndDocItOptions.FontName;
        TargetCanvas.Font.Size := BrowseAndDocItOptions.FontSize;
        // Highlight selected item.
        iOffset := Sender.Left;
        Brush.Color := tvExplorer.Color;
        FillRect(ItemRect);
        NodeData := Sender.GetNodeData(Node);
        sl := NodeData.FNode.Tokens;
        If Sender.Selected[Node] Then
          Begin
            // Need to amend the width of the rectangle for the custom drawing
            iPos := 5;
            For i := 0 To sl.Count - 1 Do
              Begin
                GetFontInfo(sl, i, NodeData.FNode.Level, TargetCanvas);
                Inc(iPos, TextWidth(sl[i]) + 1);
              End;
            R := ItemRect;
            R.Left := (NodeData.FNode.Level + 1) * Integer(tvExplorer.Indent) -
              iOffset + ilScopeImages.Width + tvExplorer.Margin +
              tvExplorer.TextMargin - 2;
            R.Right := R.Left + iPos;
            If Node = Sender.FocusedNode Then
              Begin
                Brush.Color := clInfoBk;
                FillRect(R);
              End;
            Pen.Color := clBlack;
            Rectangle(R);
          End;
        R := ItemRect;
        iCentre := (R.Top + R.Bottom) Div 2;
        // Draw Tree
        R.Left := R.Left + (NodeData.FNode.Level * Integer(tvExplorer.Indent)) -
          iOffset;
        // Draw vertical tree lines
        P := Node.Parent;
        iTreeColour := BrowseAndDocItOptions.TreeColour;
        For i := NodeData.FNode.Level - 1 DownTo 0 Do
          Begin
            If (P <> Nil) And (P.Parent <> Nil) Then
              If P.Index < P.Parent.ChildCount - 1  Then
                Begin
                  Pen.Color := iTreeColour;
                  Pen.Style := psSolid;
                  MoveTo(Integer(tvExplorer.Indent) * i + 8 - iOffset, R.Top);
                  LineTo(Integer(tvExplorer.Indent) * i + 8 - iOffset, R.Bottom);
                End;
            P := P.Parent;
          End;
        // Draw top half of node connector
        Pen.Color := iTreeColour;
        Pen.Style := psSolid;
        MoveTo(R.Left + 8, iCentre);
        LineTo(R.Left + Integer(tvExplorer.Indent), iCentre);
        If Node.Parent <> Nil Then
          Begin
            // Draw connection to item
            Pen.Color := iTreeColour;
            Pen.Style := psSolid;
            MoveTo(R.Left + 8, R.Top);
            LineTo(R.Left + 8, iCentre);
            If Node.Index < Node.Parent.ChildCount - 1 Then
              Begin
                // Draw connector to next node.
                Pen.Color := iTreeColour;
                Pen.Style := psSolid;
                MoveTo(R.Left + 8, iCentre);
                LineTo(R.Left + 8, R.Bottom);
              End;
          End;
        If Node.ChildCount > 0 Then
          Begin
            // Draw button
            Pen.Color := iTreeColour;
            Pen.Style := psSolid;
            Rectangle(R.Left + 4, iCentre - 4,
              R.Left + 13, iCentre + 5);
            // Draw negative side
            Pen.Color := iTreeColour;
            MoveTo(R.Left + 6, iCentre);
            LineTo(R.Left + 11, iCentre);
            If Not Sender.Expanded[Node] Then
              Begin
                // Make positive sign
                MoveTo(R.Left + 8, iCentre - 2);
                LineTo(R.Left + 8, iCentre + 3);
              End;
          End;
        //Draw Image, Padding used to get custom tree in the same places as std.
        R.Left := R.Left + Integer(tvExplorer.Indent) + tvExplorer.Margin;
        Inc(R.Top);
        ilScopeImages.Draw(TargetCanvas, R.Left, R.Top, NodeData.FNode.ImageIndex);
        // Draw text
        R.Left := R.Left + ilScopeImages.Width + tvExplorer.TextMargin;
        iPos := R.Left + 2;
        For i := 0 To sl.Count - 1 Do
          Begin
            GetFontInfo(sl, i, NodeData.FNode.Level, TargetCanvas);
            TextOut(iPos, R.Top + 1, sl[i]);
            Inc(iPos, TextWidth(sl[i]) + 1);
          End;
      End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**

  This is an on click event handler for the explorer tree view.

  @precon  None.
  @postcon Fires a SelectionChange event for the specifically selected item.

  @param   Sender as a TObject

 **)
procedure TframeModuleExplorer.tvExplorerClick(Sender: TObject);

Var
  NodeData : ^TTreeData;

begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TframeModuleExplorer.tvExplorerClick');
  Try
  {$ENDIF}
  If FSelectionChanging Then
    Exit;
  FSelectionChanging := True;
  Try
    If tvExplorer.FocusedNode <> Nil Then
      If Assigned(FSelectionChange) And Not FRendering Then
        Begin
          NodeData := tvExplorer.GetNodeData(tvExplorer.FocusedNode);
          If NodeData.FNode <> Nil Then
            If NodeData.FNode.Comment = Nil Then
              FSelectionChange(NodeData.FNode.Line, NodeData.FNode.Col,
                NodeData.FNode.Line, NodeData.FNode.Col)
            Else
              FSelectionChange(NodeData.FNode.Line, NodeData.FNode.Col,
                NodeData.FNode.Comment.Line, NodeData.FNode.Comment.Col);
        End;
  Finally
    FSelectionChanging := False;
  End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

(**


  This method is an on key down event handler for the tree view.

  @precon  None.
  @postcon If an on focus event handler is assigned it is fired.

  @param   Sender as a TObject
  @param   Key    as a Char as a reference

**)
procedure TframeModuleExplorer.tvExplorerKeyPress(Sender: TObject; var Key: Char);

begin
  {$IFDEF PROFILECODE}
  CodeProfiler.Start('TframeModuleExplorer.tvExplorerKeyPress');
  Try
  {$ENDIF}
  If Key = #13 Then
    Begin
      tvExplorerClick(Sender);
      If Assigned(OnFocus) Then
        FFocus(Sender);
      Key := #0;
    End;
  {$IFDEF PROFILECODE}
  Finally
    CodeProfiler.Stop;
  End;
  {$ENDIF}
end;

end.
