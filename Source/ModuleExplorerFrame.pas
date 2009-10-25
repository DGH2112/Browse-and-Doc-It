(**

  This module contains a frame which holds all the functionality of the
  module browser so that it can be independant of the application specifics.

  @Date    25 Oct 2009
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
    Constructor Create(strText, strName : String; iLevel : Integer; iImageIndex : Integer;
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
    Function CanDrawSpecialTag(Comment : TComment; strSpecialTag : String) : Boolean;
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
    procedure tvExplorerMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure tvExplorerClick(Sender: TObject);
    procedure actLocalUpdate(Sender: TObject);
    procedure actLocalExecute(Sender: TObject);
    procedure tvExplorerKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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
    { Private declarations }
    procedure GetBodyCommentTags(M : TBaseLanguageModule);
    Function AddNode(P : PVirtualNode; strText, strName : String; iLevel : Integer;
      iImageIndex : Integer; iLine : Integer = 0; iColumn : Integer = 0;
      boolTitle : Boolean = False; AComment : TComment = Nil) : PVirtualNode;
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
  IniFiles, Types, Math, DGHLibrary, GenericTokenizer;

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

  @param   sl        as a TStringList
  @param   i         as an Integer
  @param   boolTitle as a Boolean
  @param   Canvas    as a TCanvas

**)
Procedure GetFontInfo(sl : TStringList; i : Integer; boolTitle : Boolean;
  Canvas : TCanvas);

Begin
  With BrowseAndDocItOptions Do
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
   End;
End;

(**

  This is the constructor method for the TTreeNodeInfo class.

  @precon  This line number of the comment, This column number of the comment
           and This comment to be added to the node info object.
  @postcon Initialises the class.

  @param   strText     as a String
  @param   strName     as a String
  @param   iLevel      as an Integer
  @param   iImageIndex as an Integer
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   boolTitle   as a Boolean
  @param   AComment    as a TComment

**)
Constructor TTreeNodeInfo.Create(strText, strName : String; iLevel : Integer;
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
    FTokens := Tokenize(FText, strKeyWords, BrowseAndDocItOptions.TokenLimit);
  Result := FTokens;
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
                Begin
                  GetFontInfo(sl, i, FTitle, Canvas);
                  If Brush.Color = clWindow Then
                    Brush.Color := clInfoBk;
                End Else
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
              iLine := (iLines - 1) * TextHeight(sl[i]) - 1;
              TextOut(iPos, iLine, sl[i]);
              Inc(iPos, TextWidth(sl[i]) + 1);
            End;
          End Else
            TextOut(iPos, iLine, NodeData.FNode.Text);
      Brush.Color := clInfoBk;
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
          str := FComment.AsString(MaxInt, False);
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
                    If CompareText(BrowseAndDocItOptions.SpecialTags.Names[i], FComment.Tag[j].TagName) = 0 Then
                      Begin
                        Pen.Color := clBlack;
                        Brush.Color := clBlack;
                        Ellipse(3, R.Top + 5, 7, R.Top + 9);
                        Brush.Color := clInfoBk;
                        Refresh;
                        Font.Style := [];
                        Font.Color := clMaroon;
                        R := Rect(10, R.Top, Width - 2, Height);
                        str := FComment.Tag[j].AsString(MaxInt, False);
                        Inc(R.Top, DrawText(Canvas.Handle, PChar(str), -1, R,
                          DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
                          DrawTextBiDiModeFlagsReadingOnly));
                      End;
                End;
            End;
        End;
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

Var
  sl : TStringList;
  iPos, iMaxPos, iLastMax : Integer;
  i, j : Integer;
  str : String;
  R : TRect;
  NodeData : ^TTreeData;

Begin
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
          GetFontInfo(sl, i, NodeData.FNode.Title, Canvas);
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
      str := Comment.AsString(MaxInt, False);
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
                If CompareText(BrowseAndDocItOptions.SpecialTags.Names[i], Comment.Tag[j].TagName) = 0 Then
                  Begin
                    Refresh;
                    Canvas.Font.Style := [];
                    R := Rect(Result.Left + 2, 0, Result.Right - 12, 0);
                    str := Comment.Tag[j].AsString(MaxInt, False);
                    Inc(Result.Bottom, DrawText(Canvas.Handle, PChar(str), -1, R,
                      DT_CALCRECT Or DT_LEFT Or DT_WORDBREAK Or DT_NOPREFIX Or
                      DrawTextBiDiModeFlagsReadingOnly) + 2);
                  End;
            End;
        End;
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
 NodeData : ^TTreeData;

Begin
  FComment := Comment;
  NodeData := FTreeView.GetNodeData(Node);
  FNodeLevel := NodeData.FNode.Level;
  FCustomDraw := SyntaxHighlight;
  FNode := Node;
  ActivateHint(Rect, NodeData.FNode.Text);
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

Var
  i : TImageIndex;

begin
  Inherited;
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
      TabOrder := 2;
      OnBeforeItemPaint := tvExplorerBeforeItemPaint;
      OnClick := tvExplorerClick;
      OnGetImageIndex := tvExplorerGetImageIndex;
      OnKeyDown := tvExplorerKeyDown;
      OnMeasureItem := tvExplorerMeasureItem;
      OnMouseMove := tvExplorerMouseMove;
    End;
  FExplorer.NodeDataSize := SizeOf(TTreeData);
  FINIFileName := BuildRootKey(Nil, Nil);
  FNodeInfo := TObjectList.Create(True);
  FHintWin := TCustomHintWindow.Create(Self, FExplorer);
  FHintWin.Color := clInfoBk;
  FHintWin.Canvas.Font.Assign(FExplorer.Font);
  ilScopeImages.Clear;
  For i := Succ(Low(TImageIndex)) to High(TImageIndex) Do
    If Not ilScopeImages.GetInstRes(hInstance, rtBitmap, ImageList[i].FResourceName, 16,
      [lrDefaultColor], ImageList[i].FMaskColour) Then
      ShowMessage(Format('Resource "%s" not found.', [ImageList[i].FResourceName]));
  FExplorer.OnGetText := tvExplorerGetText;
end;

(**

  This is the destructor method for the TframeModuleExplorer class.

  @precon  None.
  @postcon destroy the instance of the dockable form.

**)
Destructor TframeModuleExplorer.Destroy;

begin
  FExplorer.Free;
  FSpecialTagNodes := Nil;
  ManageExpandedNodes;
  GetExpandedNodes(FModule);
  FHintWin.Free;
  FNodeInfo.Free;
  Inherited;
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
  For i := 0 To M.BodyCommentCount - 1 Do
    With M.BodyComment[i] Do
      For j := 0 To TagCount - 1 Do
        For k := Low(FSpecialTagNodes) To High(FSpecialTagNodes) Do
          If FSpecialTagNodes[k].boolShow Then
            If CompareText(Tag[j].TagName, FSpecialTagNodes[k].strTagName) = 0 Then
              AddNode(FSpecialTagNodes[k].Node, Tag[j].AsString(MaxInt, False),
                Tag[j].Name, 2, Integer(iiToDoItem) - 1, M.BodyComment[i].Tag[j].Line,
                M.BodyComment[i].Tag[j].Column, False, Nil);
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
    If Container.Elements[i].Scope In BrowseAndDocItOptions.ScopesToRender +
      [scNone, scGlobal] Then
      Begin
        NewNode := AddNode(RootNode, Container.Elements[i].AsString(True, False),
          Container.Elements[i].Name, iLevel, Container.Elements[i].ImageIndexAdjustedForScope,
          Container.Elements[i].Line, Container.Elements[i].Column,
          Container.Elements[i] Is TLabelContainer, Container.Elements[i].Comment);
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
  NodeData : ^TTreeData;

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
  With BrowseAndDocItOptions.ExpandedNodes Do
  For i := Count - 1 DownTo 0 Do
    Begin
      dtDate := Integer(Objects[i]);
      If dtDate < Now - BrowseAndDocItOptions.ManagedNodesLife Then
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
              If Not BrowseAndDocItOptions.ExpandedNodes.Find(str, iIndex) Then
                iIndex := BrowseAndDocItOptions.ExpandedNodes.Add(str);
              BrowseAndDocItOptions.ExpandedNodes.Objects[iIndex] := TObject(Trunc(Now));
            End Else
              If BrowseAndDocItOptions.ExpandedNodes.Find(str, iIndex) Then
                BrowseAndDocItOptions.ExpandedNodes.Delete(iIndex);
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
          If BrowseAndDocItOptions.ExpandedNodes.Find(str, i) Then
            FExplorer.Expanded[Node] := True;
          SetExpandedNodes(Node);
        End;
      Node := FExplorer.GetNextSibling(Node);
    End;
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
  FExplorer.Color := BrowseAndDocItOptions.BGColour;
  If M = Nil Then
    strKeyWords := Nil
  Else
    strKeyWords := M.KeyWords;
  If FRendering Then
    Exit;
  FRendering := True;
  Try
    FHintWin.ReleaseHandle; // Stop AV when refreshing the tree.
    FExplorer.Font.Name := BrowseAndDocItOptions.FontName;
    FExplorer.Font.Size := BrowseAndDocItOptions.FontSize;
    GetExpandedNodes(FModule);
    FModule := Nil;
    // Find and store the top item and the selected item in the tree view
    strTop := GetNodePath(FExplorer.TopNode);
    strSelection := GetNodePath(FExplorer.FocusedNode);
    FExplorer.BeginUpdate;
    Try
      FExplorer.Clear;
      FNodeInfo.Clear;
      If M = Nil Then
        Exit;
      M.AddTickCount('Clear');
      SetLength(FSpecialTagNodes, BrowseAndDocItOptions.SpecialTags.Count);
      // Create Root Tree Node
      FModule := AddNode(Nil, M.AsString(True, False), M.Name, 0,
        M.ImageIndexAdjustedForScope, M.Line, M.Column, False, M.Comment);
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
  NodeData : ^TTreeData;

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

Const
  strPromotedLabels : Array[1..4] Of String = (strDocumentationConflicts,
    strHints, strWarnings, strErrors);

Var
  i : Integer;
  Node: PVirtualNode;
  NodeData: ^TTreeData;
  N: PVirtualNode;

begin
  FExplorer.Expanded[FModule] := True;
  For i := Low(FSpecialTagNodes) to High(FSpecialTagNodes) Do
    If FSpecialTagNodes[i].Node.ChildCount = 0 Then
      Begin
        FExplorer.DeleteNode(FSpecialTagNodes[i].Node);
        FSpecialTagNodes[i].Node := Nil;
      End;
  For i := Low(FSpecialTagNodes) to High(FSpecialTagNodes) Do
    If FSpecialTagNodes[i].Node <> Nil Then
      If FSpecialTagNodes[i].boolExpand Then
        FExplorer.Expanded[FSpecialTagNodes[i].Node] := True;
  For i := Low(strPromotedLabels) To High(strPromotedLabels) Do
    Begin
      Node := FExplorer.GetFirstChild(FModule);
      While Node <> Nil Do
        Begin
          NodeData := FExplorer.GetNodeData(Node);
          If CompareText(NodeData.FNode.Text, strDocumentationConflicts) = 0 Then
            If doShowConflicts In BrowseAndDocItOptions.Options Then
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
            If doShowHints In BrowseAndDocItOptions.Options Then
              FExplorer.Expanded[Node] := True;
          If CompareText(NodeData.FNode.Text, strWarnings) = 0 Then
            If doShowWarnings In BrowseAndDocItOptions.Options Then
              FExplorer.Expanded[Node] := True;
          If CompareText(NodeData.FNode.Text, strErrors) = 0 Then
            If doShowErrors In BrowseAndDocItOptions.Options Then
              FExplorer.Expanded[Node] := True;
          Node := FExplorer.GetNextSibling(Node);
        End;
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
      FSpecialTagNodes[i].strTagName := BrowseAndDocItOptions.SpecialTags.Names[i];
      FSpecialTagNodes[i].strTagDesc :=
        BrowseAndDocItOptions.SpecialTags.Values[BrowseAndDocItOptions.SpecialTags.Names[i]];
      FSpecialTagNodes[i].boolShow :=
        Integer(BrowseAndDocItOptions.SpecialTags.Objects[i]) And iShowInTree <> 0;
      FSpecialTagNodes[i].boolExpand :=
        Integer(BrowseAndDocItOptions.SpecialTags.Objects[i]) And iAutoExpand <> 0;
      FSpecialTagNodes[i].Node := AddNode(FModule, FSpecialTagNodes[i].strTagDesc,
        FSpecialTagNodes[i].strTagName, 1, Integer(iiTodoFolder) - 1);
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
  procedure UpdateScopes(AScope : TScope);

  Begin
    With BrowseAndDocItOptions Do
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
  procedure UpdateOptions(Option : TDocOption);

  Begin
    With BrowseAndDocItOptions Do
      If Option In Options Then
        Options := Options - [Option]
      Else
        Options := Options + [Option];
  End;

begin
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
    (Sender As TAction).Checked := doShowPropertyMissingDoc In BrowseAndDocItOptions.Options
  Else If Sender = actConstants Then
    (Sender As TAction).Checked := doShowUndocumentedConsts In BrowseAndDocItOptions.Options
  Else If Sender = actVariables Then
    (Sender As TAction).Checked := doShowUndocumentedVars In BrowseAndDocItOptions.Options
  Else If Sender = actTypes Then
    (Sender As TAction).Checked := doShowUndocumentedTypes In BrowseAndDocItOptions.Options
end;

(**

  This method adds a node to the treeview as a child of the give node. It
  assigns the line, column and comment information to the noNode.

  @precon  P is the parent node to attach this new child too, Element is the
           parser node to render.
  @postcon Returns a instance of the newly add / created tree node.

  @param   P           as a PVirtualNode
  @param   strText     as a String
  @param   strName     as a String
  @param   iLevel      as an Integer
  @param   iImageIndex as an Integer
  @param   iLine       as an Integer
  @param   iColumn     as an Integer
  @param   boolTitle   as a Boolean
  @param   AComment    as a TComment
  @return  a PVirtualNode

**)
Function TframeModuleExplorer.AddNode(P : PVirtualNode; strText, strName : String;
  iLevel : Integer; iImageIndex : Integer; iLine : Integer = 0;
  iColumn : Integer = 0; boolTitle : Boolean = False;
  AComment : TComment = Nil) : PVirtualNode;

Var
  NodeData : ^TTreeData;
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
  NodeData : ^TTreeData;

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
  @param   CellText as a WideString as a reference

**)
procedure TframeModuleExplorer.tvExplorerGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
{$IFNDEF D2009}
  var CellText: WideString);
{$ELSE}
  var CellText: string);
{$ENDIF}

Var
  NodeData : ^TTreeData;

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
  TargetCanvas.Font.Name := BrowseAndDocItOptions.FontName;
  TargetCanvas.Font.Size := BrowseAndDocItOptions.FontSize;
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
  NodeData : ^TTreeData;

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
          If doShowCommentHints In BrowseAndDocItOptions.Options Then
            C := NodeData.FNode.Comment;
          Rect := FHintWin.CalcHintRect(FExplorer.ClientWidth, Screen.Width,
            Node, doCustomDrawing In BrowseAndDocItOptions.Options, C);
          If (Rect.Right <= FExplorer.ClientWidth) And ((C = Nil) Or
            ((C.TokenCount = 0) And (C.TagCount = 0))) Then
            Begin
              FHintWin.ReleaseHandle;
              Exit;
            End;
          Rect.TopLeft := FExplorer.ClientToScreen(Rect.TopLeft);
          Rect.BottomRight := FExplorer.ClientToScreen(Rect.BottomRight);
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
  CustomDraw := (doCustomDrawing In BrowseAndDocItOptions.Options);
  With TargetCanvas Do
    If CustomDraw Then
      Begin
        TargetCanvas.Font.Name := BrowseAndDocItOptions.FontName;
        TargetCanvas.Font.Size := BrowseAndDocItOptions.FontSize;
        // Highlight selected item.
        iOffset := Sender.Left;
        Brush.Color := FExplorer.Color;
        FillRect(ItemRect);
        NodeData := Sender.GetNodeData(Node);
        sl := NodeData.FNode.Tokens;
        If Sender.Selected[Node] Then
          Begin
            // Need to amend the width of the rectangle for the custom drawing
            iPos := 5;
            For i := 0 To sl.Count - 1 Do
              Begin
                GetFontInfo(sl, i, NodeData.FNode.Title, TargetCanvas);
                Inc(iPos, TextWidth(sl[i]) + 1);
              End;
            R := ItemRect;
            R.Left := (NodeData.FNode.Level + 1) * Integer(FExplorer.Indent) -
              iOffset + ilScopeImages.Width + FExplorer.Margin +
              FExplorer.TextMargin - 2;
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
        R.Left := R.Left + (NodeData.FNode.Level * Integer(FExplorer.Indent)) -
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
                  MoveTo(Integer(FExplorer.Indent) * i + 8 - iOffset, R.Top);
                  LineTo(Integer(FExplorer.Indent) * i + 8 - iOffset, R.Bottom);
                End;
            P := P.Parent;
          End;
        // Draw top half of node connector
        Pen.Color := iTreeColour;
        Pen.Style := psSolid;
        MoveTo(R.Left + 8, iCentre);
        LineTo(R.Left + Integer(FExplorer.Indent), iCentre);
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
        R.Left := R.Left + Integer(FExplorer.Indent) + FExplorer.Margin;
        Inc(R.Top);
        ilScopeImages.Draw(TargetCanvas, R.Left, R.Top, NodeData.FNode.ImageIndex);
        // Draw text
        R.Left := R.Left + ilScopeImages.Width + FExplorer.TextMargin;
        iPos := R.Left + 2;
        For i := 0 To sl.Count - 1 Do
          Begin
            GetFontInfo(sl, i, NodeData.FNode.Title, TargetCanvas);
            If Node = Sender.FocusedNode Then
              If Brush.Color = clWindow Then
                Brush.Color := clinfoBk;
            TextOut(iPos, R.Top, sl[i]);
            Inc(iPos, TextWidth(sl[i]) + 1);
          End;
      End;
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

  This method is an on key down event handler for the tree view.

  @precon  None.
  @postcon If an on focus event handler is assigned it is fired.

  @param   Sender as a TObject
  @param   Key    as a Word as a reference
  @param   Shift  as a TShiftState

**)
procedure TframeModuleExplorer.tvExplorerKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

begin
  If Key = 13 Then
    Begin
      tvExplorerClick(Sender);
      If Shift = [] Then
        If Assigned(OnFocus) Then
          FFocus(Sender);
    End;
end;

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
  NodeData : ^TTreeData;
  sl: TStringList;
  i: Integer;

begin
  Result := 5;
  NodeData := GetNodeData(Node);
  Self.Canvas.Font.Name := BrowseAndDocItOptions.FontName;
  Self.Canvas.Font.Size := BrowseAndDocItOptions.FontSize;
  sl := NodeData.FNode.Tokens;
  For i := 0 To sl.Count - 1 Do
    Begin
      GetFontInfo(sl, i, NodeData.FNode.Title, Self.Canvas);
      Inc(Result, Self.Canvas.TextWidth(sl[i]) + 1);
    End;
end;

end.
