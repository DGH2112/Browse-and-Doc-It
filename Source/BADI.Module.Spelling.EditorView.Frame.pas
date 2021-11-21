(**
  
  This module contains a frame for displaying modules spelling mistakes.

  @Author  David Hoyle
  @Version 5.946
  @Date    21 Nov 2021

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

  @note    If vstStatistics.ScrollBarOptions.AlwaysVisible is not TRUE track pad scrolling AVs editor.

**)
Unit BADI.Module.Spelling.EditorView.Frame;

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
  VirtualTrees,
  BADI.Base.Module,
  BADI.ElementContainer,
  ImgList,
  ExtCtrls,
  Actions,
  ActnList,
  PlatformDefaultStyleActnCtrls,
  Menus,
  ActnPopup,
  ToolsAPI,
  Themes,
  BADI.Types,
  BADI.CustomVirtualStringTree,
  BADI.SpellingIssue,
  UITypes,
  System.ImageList;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A frame to display a modules methods and their Spelling. **)
  TframeBADIModuleSpellingEditorView = Class(TFrame)
    ilScopeImages: TImageList;
    tmFocusTimer: TTimer;
    pabContextMenu: TPopupActionBar;
    alActions: TActionList;
    actExpandAll: TAction;
    actCollapseAll: TAction;
    actExpand: TAction;
    actCollapse: TAction;
    ExpandAll1: TMenuItem;
    CollapseAll1: TMenuItem;
    N1: TMenuItem;
    Expand1: TMenuItem;
    Collapse1: TMenuItem;
    actExpandIssues: TAction;
    N2: TMenuItem;
    ExpandIssues1: TMenuItem;
    actShowAllColumns: TAction;
    N3: TMenuItem;
    ShowAllColumns1: TMenuItem;
    Procedure actExpandAllExecute(Sender: TObject);
    Procedure actCollapseAllExecute(Sender: TObject);
    Procedure actExpandUpdate(Sender: TObject);
    Procedure actExpandExecute(Sender: TObject);
    Procedure actCollapseExecute(Sender: TObject);
    Procedure actCollapseUpdate(Sender: TObject);
    Procedure actExpandIssuesExecute(Sender: TObject);
    Procedure vstStatisticsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    Procedure tmFocusTimerTimer(Sender: TObject);
    procedure actShowAllColumnsExecute(Sender: TObject);
  Strict Private
    Type
      (** A custom virtual string tree to stop a Dark Themed IDE raising Access Violations due to some
          sort of RTTI clash. **)
      TBADIEditorViewVirtualStringTree = Class(TBADICustomVirtualStringTree);
      (** A record to describe the data to be held in a tree node. **)
      TBADISpellingRecord = Record
        FNodeType        : TBADINodeType;
        FFileName        : String;
        FText            : String;
        FImageIndex      : Integer;
        FSpellingCount   : Array[TBADISpellingIssueType] Of Integer;
        FIdentLine       : Integer;
        FIdentColumn     : Integer;
        FCommentLine     : Integer;
        FCommentColumn   : Integer;
      End;
      (** A pointer to the above record. **)
      PBADISpellingRecord = ^TBADISpellingRecord;
      (** A record type to hold the return information from recursing nodes. **)
      TNodeResultRecord = Record
        FNode: PVirtualNode;
        FChildCount: Integer;
        FIssueCount: Integer;
        Class Operator Add(Const R1, R2: TNodeResultRecord): TNodeResultRecord;
        Constructor Create(Const Node: PVirtualNode);
      End;
  Strict Private
    FFileName       : String;
    FModuleCount    : Integer;
    FSpellingIssues : Array[TBADISpellingIssueType] of Integer;
    FVSTSpelling    : TBADIEditorViewVirtualStringTree;
    {$IFDEF RS102}
    FThemingServicesNotifierIndex : Integer;
    FStyleServices : TCustomStyleServices;
    {$ENDIF RS102}
  Strict Protected
    function CreateModule(const Module: TBaseLanguageModule): TNodeResultRecord;
    Procedure vstStatisticsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; Var CellText: String);
    Procedure vstStatisticsBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      Var ContentRect: TRect);
    Procedure vstStatisticsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; Var Ghosted: Boolean; Var ImageIndex: TImageIndex);
    Procedure vstStatisticsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
      Column: TColumnIndex; Var Result: Integer);
    Procedure vstStatisticsPaintText(Sender: TBaseVirtualTree; Const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    Function RecurseContainer(Const Container: TElementContainer;
      Const Parent: PVirtualNode): TNodeResultRecord;
    Procedure UpdateStats;
    Procedure DeleteExistingModuleNode(Const Module : TBaseLanguageModule);
    Procedure SortAndExpand(Const NodeResult : TNodeResultRecord;
      Const setRenderOptions : TBADIRenderOptions);
    Function  HasIssues(Const Node : PVirtualNode) : Boolean;
    Procedure CreateVirtualStringTree;
    Procedure HideZeroColumns;
    Procedure ExpandIssues;
    Procedure HookStyleServices(Sender : TObject);
    Procedure vstStatisticsDblClick(Sender : TObject);
    Function  GetSpellingIssue(Const eSpellingIssue : TBADISpellingIssueType) : Integer;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Procedure RenderModule(Const Module: TBaseLanguageModule;
      Const setRenderOptions: TBADIRenderOptions);
    Procedure CopyToClipboard;
    Procedure FocusResults;
    (**
      This property returns the number of spelling issues for the given enumerate.
      @precon  None.
      @postcon Returns the number of spelling issues for the given enumerate.
      @param   eSpellingIssue as a TBADISpellingIssueType as a constant
      @return  an Integer
    **)
    Property SpellingIssue[Const eSpellingIssue : TBADISpellingIssueType]: Integer
      Read GetSpellingIssue;
  Published
    (**
      This property returns the number of modules.
      @precon  None.
      @postcon Returns the number of modules.
      @return  an Integer
    **)
    Property ModuleCount: Integer Read FModuleCount;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF DEBUG}
  Vcl.ClipBrd, 
  BADI.ResourceStrings,
  BADI.Options,
  BADI.Functions,
  BADI.IDEThemingNotifier,
  BADI.ToolsAPIUtils,
  BADI.Constants;

{$R *.dfm}

Type
  (** An enumerate to define the columns in the report. **)
  TBADISpellingColumn = (
    scText,
    scComments,
    scTags,
    scResourceStrings,
    scConstants,
    scLiterals
  );
  (** A record to describe spelling column information. **)
  TSpellingColumnInfo = Record
    FName      : String;
    FWidth     : Integer;
    FMinWidth  : Integer;
    FAlignment : TAlignment;
  End;

Const
  (** A constant array of column information for rendering. **)
  SpellingColumns : Array[TBADISpellingColumn] Of TSpellingColumnInfo = (
    (FName: 'Module';          FWidth: 300; FMinWidth: 300; FAlignment: taLeftJustify),
    (FName: 'Comment';         FWidth: 100; FMinWidth: 0;   FAlignment: taCenter),
    (FName: 'Tag';             FWidth: 100; FMinWidth: 0;   FAlignment: taCenter),
    (FName: 'Resource String'; FWidth: 100; FMinWidth: 0;   FAlignment: taCenter),
    (FName: 'Constant';        FWidth: 100; FMinWidth: 0;   FAlignment: taCenter),
    (FName: 'Literal';         FWidth: 100; FMinWidth: 0;   FAlignment: taCenter)
  );


(**

  This is an operator overload for the Add operation.

  @precon  None.
  @postcon Adds the numeric fields of the 2 record to a new result.

  @param   R1 as a TNodeResultRecord as a constant
  @param   R2 as a TNodeResultRecord as a constant
  @return  a TNodeResultRecord

**)
Class Operator TframeBADIModuleSpellingEditorView.TNodeResultRecord.Add(Const R1,
  R2: TNodeResultRecord): TNodeResultRecord;

Begin
  Result.FNode := R1.FNode;
  Result.FChildCount := R1.FChildCount + R2.FChildCount;
  Result.FIssueCount := R1.FIssueCount + R2.FIssueCount;
End;

(**

  A constructor for the TNodeResultRecord class.

  @precon  None.
  @postcon Initialises the record.

  @param   Node as a PVirtualNode as a constant

**)
Constructor TframeBADIModuleSpellingEditorView.TNodeResultRecord.Create(Const Node: PVirtualNode);

Begin
  Inherited;
  FNode := Node;
  FChildCount := 0;
  FIssueCount := 0;
End;

(**

  This is an on execute event handler for the Collapse All action.

  @precon  None.
  @postcon Collapses all the treeview nodes.

  @param   Sender as a TObject

**)
Procedure TframeBADIModuleSpellingEditorView.actCollapseAllExecute(Sender: TObject);

Begin
  FVSTSpelling.FullCollapse();
End;

(**

  This is an on execute event handler for the Collapse action.

  @precon  None.
  @postcon Collapses the focused treeview node.

  @param   Sender as a TObject

**)
Procedure TframeBADIModuleSpellingEditorView.actCollapseExecute(Sender: TObject);

Begin
  FVSTSpelling.FullCollapse(FVSTSpelling.FocusedNode);
End;

(**

  This is an on update event handler for the Collapse Update action.

  @precon  None.
  @postcon Enables the Collapse action is there is a focused node, it has children and is currently
           expanded.

  @param   Sender as a TObject

**)
Procedure TframeBADIModuleSpellingEditorView.actCollapseUpdate(Sender: TObject);

Var
  A: TAction;

Begin
  If Sender Is TAction Then
    Begin
      A := Sender As TAction;
      A.Enabled :=
        Assigned(FVSTSpelling.FocusedNode) And
        (FVSTSpelling.ChildCount[FVSTSpelling.FocusedNode] > 0) And
        FVSTSpelling.Expanded[FVSTSpelling.FocusedNode];
    End;
End;

(**

  This is an on execute event handler for the Expand All action.

  @precon  None.
  @postcon Expands all the nodes in the treeview.

  @param   Sender as a TObject

**)
Procedure TframeBADIModuleSpellingEditorView.actExpandAllExecute(Sender: TObject);

Begin
  FVSTSpelling.FullExpand();
End;

(**

  This is an on execute event handler for the Expand action.

  @precon  None.
  @postcon Expands the focused node in the treeview.

  @param   Sender as a TObject

**)
Procedure TframeBADIModuleSpellingEditorView.actExpandExecute(Sender: TObject);

Begin
  FVSTSpelling.FullExpand(FVSTSpelling.FocusedNode);
End;

(**

  This is an on execute event handler for the ExpandIssues action.

  @precon  None.
  @postcon Expand all root nodes that have issues in them and collapses all others.

  @param   Sender as a TObject

**)
Procedure TframeBADIModuleSpellingEditorView.actExpandIssuesExecute(Sender: TObject);

Var
  N: PVirtualNode;

Begin
  N := FVSTSpelling.GetFirst();
  While Assigned(N) Do
    Begin
      If HasIssues(N) Then
        FVSTSpelling.FullExpand(N)
      Else
        FVSTSpelling.FullCollapse(N);
      N := FVSTSpelling.GetNextSibling(N);
    End;
End;

(**

  This is an on update event handler for the Expand action.

  @precon  None.
  @postcon Enables the expand action of there is a focused node, it has children and is collapsed.

  @param   Sender as a TObject

**)
Procedure TframeBADIModuleSpellingEditorView.actExpandUpdate(Sender: TObject);

Var
  A: TAction;

Begin
  If Sender Is TAction Then
    Begin
      A := Sender As TAction;
      A.Enabled :=
        Assigned(FVSTSpelling.FocusedNode) And
        (FVSTSpelling.ChildCount[FVSTSpelling.FocusedNode] > 0) And
        Not FVSTSpelling.Expanded[FVSTSpelling.FocusedNode];
    End;
End;

(**

  This is an on execute event handler for the Show All Columns action.

  @precon  None.
  @postcon Shows all the columns in the report.

  @param   Sender as a TObject

**)
Procedure TframeBADIModuleSpellingEditorView.actShowAllColumnsExecute(Sender: TObject);

Var
  iColumn: Integer;
  C: TVirtualTreeColumn;

Begin
  For iColumn := 0 To FVSTSpelling.Header.Columns.Count - 1 Do
    Begin
      C := FVSTSpelling.Header.Columns[iColumn];
      C.Options := C.Options + [coVisible];
    End;
End;

(**

  This method copies the treeview information to the clipboard.

  @precon  None.
  @postcon The treeview information is copied to the clipboard.

**)
Procedure TframeBADIModuleSpellingEditorView.CopyToClipboard;

  (**

    This method counts the number of parent nodes to the given node.

    @precon  Node must be a valid instance or Nil.
    @postcon Returns the number of parent nodes to the given node.

    @param   Node as a PVirtualNode as a constant
    @return  an Integer

  **)
  Function ParentCount(Const Node: PVirtualNode): Integer;

  Var
    P: PVirtualNode;

  Begin
    Result := 0;
    P := Node.Parent;
    While Assigned(P) Do
      Begin
        Inc(Result);
        P := P.Parent;
      End;
  End;

ResourceString
  strClipboardHeader = 'BADI Spelling for %s'#13#10;

Const
  iMultipler = 2;

Var
  CB: TClipBoard;
  strText: String;
  strLine: String;
  Node: PVirtualNode;
  iColumn: Integer;

Begin
  CB := TClipBoard.Create;
  Try
    CB.Open;
    Try
      strText := Format(strClipboardHeader, [FFileName]);
      Node := FVSTSpelling.GetFirst();
      While Assigned(Node) Do
        Begin
          strLine := '';
          For iColumn := 0 To FVSTSpelling.Header.Columns.Count - 1 Do
            Begin
              If strLine <> '' Then
                strLine := strLine + #9;
              Case iColumn Of
                0: strLine := strLine + StringOfChar(#32, iMultipler * ParentCount(Node)) +
                    FVSTSpelling.Text[Node, iColumn];
              Else
                strLine := strLine + FVSTSpelling.Text[Node, iColumn];
              End;
            End;
          strText := strText + strLine + #13#10;
          Node := FVSTSpelling.GetNext(Node);
        End;
      CB.SetTextBuf(PWideChar(strText));
    Finally
      CB.Close;
    End;
  Finally
    CB.Free;
  End;
End;

(**

  A constructor for the TframeBADIModuleSpellingEditorView class.

  @precon  None.
  @postcon Initialises the treeview and the image list.

  @nocheck MissingCONSTInParam

  @param   AOwner as a TComponent

**)
Constructor TframeBADIModuleSpellingEditorView.Create(AOwner: TComponent);

{$IFDEF RS102}
Var
  ITS : IOTAIDEThemingServices;
{$ENDIF RS102}

Begin
  Inherited Create(AOwner);
  CreateVirtualStringTree;
  FVSTSpelling.NodeDataSize := SizeOf(TBADISpellingRecord);
  LoadBADIImages(ilScopeImages);
  HookStyleServices(Nil);
  {$IFDEF RS102}
  FThemingServicesNotifierIndex := -1;
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
    FThemingServicesNotifierIndex := ITS.AddNotifier(TBADIIDEThemeNotifier.Create(HookStyleServices));
  {$ENDIF RS102}
End;

(**

  This method creates the module tree node for the spelling mistakes to sit under.

  @precon  Module must be a valid instance.
  @postcon The module node in the tree view is created.

  @param   Module as a TBaseLanguageModule as a constant
  @return  a TNodeResultRecord

**)
Function TframeBADIModuleSpellingEditorView.CreateModule(Const Module:
  TBaseLanguageModule): TNodeResultRecord;

Var
  NodeData : PBADISpellingRecord;
    
Begin
  Result.Create(FVSTSpelling.AddChild(Nil));
  NodeData := FVSTSpelling.GetNodeData(Result.FNode);
  NodeData.FNodeType := ntModule;
  NodeData.FFileName := Module.FileName;
  NodeData.FText := Module.AsString(True, False);
  NodeData.FImageIndex := BADIImageIndex(Module.ImageIndex, Module.Scope);
  NodeData.FIdentLine := Module.Line;
  NodeData.FIdentColumn := Module.Column;
  NodeData.FCommentLine := 0;
  NodeData.FCommentColumn := 0;
  If Assigned(Module.Comment) Then
    Begin
      NodeData.FCommentLine := Module.Comment.Line;
      NodeData.FCommentColumn := Module.Comment.Column;
    End;
  NodeData.FSpellingCount[sitComment] := 0;
  NodeData.FSpellingCount[sitTag] := 0;
  NodeData.FSpellingCount[sitResourceString] := 0;
  NodeData.FSpellingCount[sitConstant] := 0;
  NodeData.FSpellingCount[sitLiteral] := 0;
End;

(**

  This method creates the virtual string tree to display the Spelling.

  @precon  None.
  @postcon The virtual string tree is created.

  @nocheck HardCodedInteger HardCodedString
  @nometric LongMethod Toxicity

**)
Procedure TframeBADIModuleSpellingEditorView.CreateVirtualStringTree;

Var
  C: TVirtualTreeColumn;
  eColumn: TBADISpellingColumn;

Begin
  FVSTSpelling := TBADIEditorViewVirtualStringTree.Create(Self);
  FVSTSpelling.Name := 'vstSpelling';
  FVSTSpelling.Parent := Self;
  FVSTSpelling.Align := alClient;
  FVSTSpelling.EmptyListMessage := 'Nothing to see here....';
  FVSTSpelling.Header.AutoSizeIndex := 0;
  FVSTSpelling.Images := ilScopeImages;
  FVSTSpelling.PopupMenu := pabContextMenu;
  FVSTSpelling.ScrollBarOptions.AlwaysVisible := True;
  FVSTSpelling.TabOrder := 0;
  FVSTSpelling.OnBeforeCellPaint := vstStatisticsBeforeCellPaint;
  FVSTSpelling.OnCompareNodes := vstStatisticsCompareNodes;
  FVSTSpelling.OnFreeNode := vstStatisticsFreeNode;
  FVSTSpelling.OnGetText := vstStatisticsGetText;
  FVSTSpelling.OnPaintText := vstStatisticsPaintText;
  FVSTSpelling.OnGetImageIndex := vstStatisticsGetImageIndex;
  FVSTSpelling.OnDblClick := vstStatisticsDblClick;
  For eColumn := Low(TBADISpellingColumn) To High(TBADISpellingColumn) Do
    Begin
      C := FVSTSpelling.Header.Columns.Add;
      C.MinWidth := SpellingColumns[eColumn].FMinWidth;
      C.Position := Integer(eColumn);
      C.Style := vsOwnerDraw;
      C.Width := SpellingColumns[eColumn].FWidth;
      C.Text := SpellingColumns[eColumn].FName;
      C.Alignment := SpellingColumns[eColumn].FAlignment;
    End;
  FVSTSpelling.Header.Columns[0].Options := FVSTSpelling.Header.Columns[0].Options + [coFixed];
End;

(**

  This method deletes a root node from the treeview if it matches the given module name.

  @precon  Module must be a valid instance.
  @postcon The modules node it deleted if found.

  @param   Module as a TBaseLanguageModule as a constant

**)
Procedure TframeBADIModuleSpellingEditorView.DeleteExistingModuleNode(Const Module : TBaseLanguageModule);

Var
  Node: PVirtualNode;

Begin
  Node := FVSTSpelling.GetFirstChild(FVSTSpelling.RootNode);
  While Assigned(Node) Do
    Begin
      If FVSTSpelling.Text[Node, 0] = Module.AsString(True, False) Then
        Begin
          FVSTSpelling.DeleteNode(Node);
          Break;
        End;
      Node := FVSTSpelling.GetNextSibling(Node);
    End;
End;

(**

  A destructor for the TframeBADIModuleSpellingEditorView class.

  @precon  None.
  @postcon Does nothing.

**)
Destructor TframeBADIModuleSpellingEditorView.Destroy;

{$IFDEF RS102}
Var
  ITS : IOTAIDEThemingServices;
{$ENDIF RS102}

Begin
  {$IFDEF RS102}
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
    If FThemingServicesNotifierIndex > -1 Then
      ITS.RemoveNotifier(FThemingServicesNotifierIndex);
  {$ENDIF RS102}
  Inherited Destroy;
End;

(**

  This method expands methods with Spelling above their limits.

  @precon  None.
  @postcon Methods with Spelling above their limits are expanded / visible.

**)
Procedure TframeBADIModuleSpellingEditorView.ExpandIssues;

Var
  N: PVirtualNode;
  NodeData : PBADISpellingRecord;
  iValue : Integer;

Begin
  N := FVSTSpelling.GetFirst;
  While Assigned(N) Do
    Begin
      NodeData := FVSTSpelling.GetNodeData(N);
      If NodeData.FNodeType = ntMethod Then
        For iValue In NodeData.FSpellingCount Do
          If iValue > 0 Then
            Begin
              FVSTSpelling.VisiblePath[N] := True;
              Break;
            End;
      N := FVSTSpelling.GetNext(N);
    End;
End;

(**

  This method starts the time to focus the frames treeview to prevent AVs in the editor if there is no
  focused control.

  @precon  None.
  @postcon The timer is started to focus the treeview.

**)
Procedure TframeBADIModuleSpellingEditorView.FocusResults;

Begin
  HideZeroColumns;
  tmFocusTimer.Enabled := True;
End;

(**

  This is a getter method for the SpellingIssue property.

  @precon  None.
  @postcon Returns the number of spelling issues for the given enumerate.

  @param   eSpellingIssue as a TBADISpellingIssueType as a constant
  @return  an Integer

**)
Function TframeBADIModuleSpellingEditorView.GetSpellingIssue(
  Const eSpellingIssue: TBADISpellingIssueType): Integer;
  
Begin
  Result := FSpellingIssues[eSpellingIssue];
End;

(**

  This method counts the number of issues in all the given nodes children and returns true if that
  number is not zero.

  @precon  Node must be a valid node reference.
  @postcon Returns true if any child node has issues.

  @param   Node as a PVirtualNode as a constant
  @return  a Boolean

**)
Function TframeBADIModuleSpellingEditorView.HasIssues(Const Node: PVirtualNode): Boolean;

Var
  iCounter : Integer;

Begin
  iCounter := 0;
  FVSTSpelling.IterateSubtree(
    Node,
    Procedure(Sender: TBaseVirtualTree; ptrNode: PVirtualNode; ptrData: Pointer; Var boolAbort: Boolean)
    Var
      NodeData : PBADISpellingRecord;
      eValue : TBADISpellingIssueType;
    Begin
      NodeData := Sender.GetNodeData(ptrNode);
      For eValue := Low(TBADISpellingIssueType) To High(TBADISpellingIssueType) Do
        Inc(iCounter, NodeData.FSpellingCount[eValue]);
    End,
    Nil);
  Result := iCounter > 0;
End;

(**

  This method hides some of the columns if they have no issues to make the view narrower.

  @precon  None.
  @postcon Any column with no issues is hidden (except method and total).

**)
Procedure TframeBADIModuleSpellingEditorView.HideZeroColumns;

Var
  eColumn : TBADISpellingColumn;
  iCount: Integer;
  N: PVirtualNode;
  NodeData : PBADISpellingRecord;
  iValue : Integer;

Begin
  For eColumn := Succ(Low(TBADISpellingColumn)) To Pred(High(TBADISpellingColumn)) Do
    Begin
      If doAutoHideSpellingWithNoissues In TBADIOptions.BADIOptions.Options Then
        Begin
          iCount := 0;
          N := FVSTSpelling.GetFirst();
          While Assigned(N) Do
            Begin
              NodeData := FVSTSpelling.GetNodeData(N);
              For iValue In NodeData.FSpellingCount Do
                Inc(iCount, iValue);
              N := FVSTSpelling.GetNext(N);
            End;
          If iCount = 0 Then
            FVSTSpelling.Header.Columns[Integer(eColumn)].Options :=
              FVSTSpelling.Header.Columns[Integer(eColumn)].Options - [coVisible]
          Else
            FVSTSpelling.Header.Columns[Integer(eColumn)].Options :=
              FVSTSpelling.Header.Columns[Integer(eColumn)].Options + [coVisible];
        End Else
          FVSTSpelling.Header.Columns[Integer(eColumn)].Options :=
            FVSTSpelling.Header.Columns[Integer(eColumn)].Options + [coVisible];
    End;
End;

(**

  This method Hooks the IDEs Style Services if they are available and enabled.

  @precon  None.
  @postcon The IDEs style services are hooked if available and enabled else its set to nil.

  @param   Sender as a TObject

**)
Procedure TframeBADIModuleSpellingEditorView.HookStyleServices(Sender : TObject);

{$IFDEF RS102}
Var
  ITS : IOTAIDEThemingServices;
{$ENDIF RS102}

Begin
  {$IFDEF RS102}
  FStyleServices := Nil;
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
    If ITS.IDEThemingEnabled Then
      Begin
        FStyleServices := ITS.StyleServices;
        ITS.ApplyTheme(Self);
      End;
  {$ENDIF RS102}
End;

(**

  This method recursively walks the given container rendering its contents in the tree view. If the
  container is a generic function, Spelling are extracted from the method and displayed. Any branches with
  out methods are pruned.

  @precon  Container must be a valid instance.
  @postcon The Spelling of the module method are displayed.

  @param   Container as a TElementContainer as a constant
  @param   Parent    as a PVirtualNode as a constant
  @return  a TNodeResultRecord

**)
Function TframeBADIModuleSpellingEditorView.RecurseContainer(Const Container: TElementContainer;
  Const Parent: PVirtualNode): TNodeResultRecord;

Var
  NodeData: PBADISpellingRecord;
  iElement: Integer;

Begin
  If Container Is TBADISpellingIssue Then
    Begin
      Result.Create(FVSTSpelling.AddChild(Parent));
      Inc(Result.FIssueCount);
      NodeData := FVSTSpelling.GetNodeData(Result.FNode);
      NodeData.FNodeType := ntMethod;
      NodeData.FText := Container.AsString(True, False);
      NodeData.FImageIndex := BADIImageIndex(Container.ImageIndex, Container.Scope);
      NodeData.FIdentLine := Container.Line;
      NodeData.FIdentColumn := Container.Column;
      NodeData.FCommentLine := 0;
      NodeData.FCommentColumn := 0;
      If Assigned(Container.Comment) Then
        Begin
          NodeData.FCommentLine := Container.Comment.Line;
          NodeData.FCommentColumn := Container.Comment.Column;
        End;
      Inc(NodeData.FSpellingCount[(Container As TBADISpellingIssue).SpellingIssueType]);
      NodeData := FVSTSpelling.GetNodeData(Parent);
      Inc(NodeData.FSpellingCount[(Container As TBADISpellingIssue).SpellingIssueType]);
    End Else
      Result.Create(Nil);
  For iElement := 1 To Container.ElementCount Do
    Result := Result + RecurseContainer(Container.Elements[iElement], Parent);
End;

(**

  This method starts the process of rendering the module contents.

  @precon  Module must be valid.
  @postcon The modules methods and their Spelling are rendered.

  @param   Module           as a TBaseLanguageModule as a constant
  @param   setRenderOptions as a TBADIRenderOptions as a constant

**)
Procedure TframeBADIModuleSpellingEditorView.RenderModule(Const Module: TBaseLanguageModule;
  Const setRenderOptions: TBADIRenderOptions);

Var
  NodeResult: TNodeResultRecord;
  Spellings: TElementContainer;

Begin
  If Assigned(Module) And Assigned(FVSTSpelling) Then
    Begin
      FFileName := Module.FileName;
      FVSTSpelling.BeginUpdate;
      Try
        If roClear In setRenderOptions Then
          FVSTSpelling.Clear
        Else
          DeleteExistingModuleNode(Module);
        NodeResult := CreateModule(Module);
        Spellings := Module.FindElement(strSpellingMistakes);
        If Assigned(Spellings) Then
          NodeResult := NodeResult + RecurseContainer(Spellings, NodeResult.FNode);
        SortAndExpand(NodeResult, setRenderOptions);
        UpdateStats;
      Finally
        FVSTSpelling.EndUpdate;
      End;
    End;
  FocusResults;
End;

(**

  This method sorts and expands the rendered module.

  @precon  None.
  @postcon The rendered node (if valid) is sorted and expanded.

  @param   NodeResult       as a TNodeResultRecord as a constant
  @param   setRenderOptions as a TBADIRenderOptions as a constant

**)
Procedure TframeBADIModuleSpellingEditorView.SortAndExpand(Const NodeResult : TNodeResultRecord;
  Const setRenderOptions : TBADIRenderOptions);

Begin
  If Assigned(NodeResult.FNode) Then
    If roAutoExpand In setRenderOptions Then
      Begin
        If ((roAutoExpandOnError In setRenderOptions) And (NodeResult.FIssueCount > 0)) Or
          Not (roAutoExpandOnError In setRenderOptions) Then
          ExpandIssues;
      End;
End;

(**

  This treeview needs to be focus to stop mouse scrolling message being directed to the editor (which
  does not exists) so this timer waits for the treeview to be visible and then gives it focus.

  @precon  None.
  @postcon Focuses the treeview when it is visible.

  @param   Sender as a TObject

**)
Procedure TframeBADIModuleSpellingEditorView.tmFocusTimerTimer(Sender: TObject);

Begin
  If FVSTSpelling.CanFocus Then
    Begin
      tmFocusTimer.Enabled := False;
      FVSTSpelling.SetFocus;
    End;
End;

(**

  This method updates the module count, method count and under, at and over limits counts.

  @precon  None.
  @postcon The counts are updated.

**)
Procedure TframeBADIModuleSpellingEditorView.UpdateStats;

Var
  Node: PVirtualNode;
  NodeData: PBADISpellingRecord;
  eSpellingType: TBADISpellingIssueType;

Begin
  FModuleCount := 0;
  For eSpellingType := Low(TBADISpellingIssueType) To High(TBADISpellingIssueType) Do
    FSpellingIssues[eSpellingType] := 0;
  Node := FVSTSpelling.GetFirst();
  While Assigned(Node) Do
    Begin
      NodeData := FVSTSpelling.GetNodeData(Node);
      Case NodeData.FNodeType Of
        ntModule: Inc(FModuleCount);
        ntMethod:
          For eSpellingType := Low(TBADISpellingIssueType) To High(TBADISpellingIssueType) Do
            Inc(FSpellingIssues[eSpellingType], NodeData.FSpellingCount[eSpellingType]);
      End;
      Node := FVSTSpelling.GetNext(Node);
    End;
End;

(**

  This is an on before cell paint method.

  @precon  None.
  @postcon The metric cells are colours as follows: Light Green for up to 95% of the limit; Amber for
           95% to 105% of the limit and Light Red for over 105% of the limit.

  @param   Sender        as a TBaseVirtualTree
  @param   TargetCanvas  as a TCanvas
  @param   Node          as a PVirtualNode
  @param   Column        as a TColumnIndex
  @param   CellPaintMode as a TVTCellPaintMode
  @param   CellRect      as a TRect
  @param   ContentRect   as a TRect as a reference

**)
Procedure TframeBADIModuleSpellingEditorView.vstStatisticsBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode;
  CellRect: TRect; Var ContentRect: TRect);

Var
  NodeData: PBADISpellingRecord;

Begin
  NodeData := FVSTSpelling.GetNodeData(Node);
  TargetCanvas.Brush.Color := clWindow;
  {$IFDEF RS102}
  If Assigned(FStyleServices) Then
    TargetCanvas.Brush.Color := FStyleServices.GetSystemColor(clWindow);
  {$ENDIF RS102}
  Case TBADISpellingColumn(Column) Of
    scComments:
      If NodeData.FSpellingCount[sitComment] > 0 Then
        TargetCanvas.Brush.Color := iLightRed;
    scTags:
      If NodeData.FSpellingCount[sitTag] > 0 Then
        TargetCanvas.Brush.Color := iLightRed;
    scResourceStrings:
      If NodeData.FSpellingCount[sitResourceString] > 0 Then
        TargetCanvas.Brush.Color := iLightRed;
    scConstants:
      If NodeData.FSpellingCount[sitConstant] > 0 Then
        TargetCanvas.Brush.Color := iLightRed;
    scLiterals:
      If NodeData.FSpellingCount[sitLiteral] > 0 Then
        TargetCanvas.Brush.Color := iLightRed;
  End;
  TargetCanvas.FillRect(CellRect);
End;

(**

  This is an on compare node event handler for the treeview.

  @precon  None.
  @postcon Ensures the nodes are sorted.

  @param   Sender as a TBaseVirtualTree
  @param   Node1  as a PVirtualNode
  @param   Node2  as a PVirtualNode
  @param   Column as a TColumnIndex
  @param   Result as an Integer as a reference

**)
Procedure TframeBADIModuleSpellingEditorView.vstStatisticsCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; Var Result: Integer);

Begin
  Result := CompareText(FVSTSpelling.Text[Node1, 0], FVSTSpelling.Text[Node2, 0]);
End;

(**

  This method attempts to display the method that was double clicked on.

  @precon  None.
  @postcon The method that was clicked on is displayed if it can be shown.

  @param   Sender as a TObject

**)
Procedure TframeBADIModuleSpellingEditorView.vstStatisticsDblClick(Sender: TObject);

Var
  Node : PVirtualNode;
  NodeData : PBADISpellingRecord;
  strFileName : String;
  MS : IOTAModuleServices;
  Module: IOTAModule;

Begin
  Node := FVSTSpelling.FocusedNode;
  While Assigned(Node) Do
    Begin
      NodeData := FVSTSpelling.GetNodeData(Node);
      If Length(NodeData.FFileName) > 0 Then
        strFileName := NodeData.FFileName;
      Node := FVSTSpelling.NodeParent[Node];
    End;
  If Supports(BorlandIDEServices, IOTAModuleServices, MS) Then
    Begin
      Module := MS.FindModule(strFileName);
      If Not Assigned(Module) Then
        Module := MS.OpenModule(strFileName);
      If Assigned(Module) Then
        Begin
          Module.Show;
          Node := FVSTSpelling.FocusedNode;
          NodeData := FVSTSpelling.GetNodeData(Node);
          TBADIToolsAPIFunctions.PositionCursor(
            NodeData.FIdentLine,
            NodeData.FIdentColumn,
            NodeData.FCommentLine,
            TBADIOptions.BADIOptions.BrowsePosition
          );
        End;
    End;
End;

(**

  This is an on Free Node event handler for the treeview.

  @precon  None.
  @postcon Ensures that the managed types in the node are freed.

  @param   Sender as a TBaseVirtualTree
  @param   Node   as a PVirtualNode

**)
Procedure TframeBADIModuleSpellingEditorView.vstStatisticsFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);

Var
  NodeData : PBADISpellingRecord;

Begin
  NodeData := Sender.GetNodeData(Node);
  Finalize(NodeData^);
End;

(**

  This is an on get image index event handler for the treeview.

  @precon  None.
  @postcon Returns the node icon for the tree item.

  @param   Sender     as a TBaseVirtualTree
  @param   Node       as a PVirtualNode
  @param   Kind       as a TVTImageKind
  @param   Column     as a TColumnIndex
  @param   Ghosted    as a Boolean as a reference
  @param   ImageIndex as a TImageIndex as a reference

**)
Procedure TframeBADIModuleSpellingEditorView.vstStatisticsGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; Var Ghosted: Boolean;
  Var ImageIndex: TImageIndex);

Var
  NodeData: PBADISpellingRecord;

Begin
  If Column = 0 Then
    Begin
      NodeData := FVSTSpelling.GetNodeData(Node);
      If Kind In [ikNormal, ikSelected] Then
        ImageIndex := NodeData.FImageIndex;
    End;
End;

(**

  This is an on get text event handler for the treeview.

  @precon  None.
  @postcon Returns the text to be shown for the tree node and column.

  @param   Sender   as a TBaseVirtualTree
  @param   Node     as a PVirtualNode
  @param   Column   as a TColumnIndex
  @param   TextType as a TVSTTextType
  @param   CellText as a String as a reference

**)
Procedure TframeBADIModuleSpellingEditorView.vstStatisticsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; Var CellText: String);

Var
  NodeData: PBADISpellingRecord;

Begin
  NodeData := FVSTSpelling.GetNodeData(Node);
  CellText := NodeData.FText;
  Case TBADISpellingColumn(Column) Of
    scText:            CellText := NodeData.FText;
    scComments:        CellText := Format('%1.0n', [Int(NodeData.FSpellingCount[sitComment])]);
    scTags:            CellText := Format('%1.0n', [Int(NodeData.FSpellingCount[sitTag])]);
    scResourceStrings: CellText := Format('%1.0n', [Int(NodeData.FSpellingCount[sitResourceString])]);
    scConstants:       CellText := Format('%1.0n', [Int(NodeData.FSpellingCount[sitConstant])]);
    scLiterals:        CellText := Format('%1.0n', [Int(NodeData.FSpellingCount[sitLiteral])]);
  End;
End;

(**

  This is an on Paint Text event handler for the treeview.

  @precon  None.
  @postcon If the node has no parent (i.e. its a root node) then make it bold and blue.

  @param   Sender       as a TBaseVirtualTree
  @param   TargetCanvas as a TCanvas as a constant
  @param   Node         as a PVirtualNode
  @param   Column       as a TColumnIndex
  @param   TextType     as a TVSTTextType

**)
Procedure TframeBADIModuleSpellingEditorView.vstStatisticsPaintText(Sender: TBaseVirtualTree;
  Const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);

Var
  NodeData : PBADISpellingRecord;

Begin
  TargetCanvas.Font.Style := [];
  TargetCanvas.Font.Color := clWindowText;
  {$IFDEF RS102}
  If Assigned(FStyleServices) Then
    TargetCanvas.Font.Color := FStyleServices.GetSystemColor(clWindowText);
  {$ENDIF RS102}
  NodeData := FVSTSpelling.GetNodeData(Node);
  Case TBADISpellingColumn(Column) Of
    scComments:
      If NodeData.FSpellingCount[sitComment] > 0 Then
        TargetCanvas.Font.Color := clBlack;
    scTags:
      If NodeData.FSpellingCount[sitTag] > 0 Then
        TargetCanvas.Font.Color := clBlack;
    scResourceStrings:
      If NodeData.FSpellingCount[sitResourceString] > 0 Then
        TargetCanvas.Font.Color := clBlack;
    scConstants:
      If NodeData.FSpellingCount[sitConstant] > 0 Then
        TargetCanvas.Font.Color := clBlack;
    scLiterals:
      If NodeData.FSpellingCount[sitLiteral] > 0 Then
        TargetCanvas.Font.Color := clBlack;
  End;
  If Not (NodeData.FNodeType In [ntMethod]) Then
    If Column = 0 Then
      TargetCanvas.Font.Style := [fsBold];
End;

End.
