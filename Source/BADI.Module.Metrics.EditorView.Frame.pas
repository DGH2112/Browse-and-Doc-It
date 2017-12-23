(**
  
  This module contains a frame for dislpaying module methods and their metrics.

  @Author  David Hoyle
  @Version 1.0
  @Date    23 Dec 2017

  @todo    Add the ability for the issues to be overridden (show a different colour but does not add to
           issue list).

  @bug     If vstStatistics.ScrollBarOptions.AlwaysVisible is not TRUE track pad scrolling AVs editor.
  
**)
Unit BADI.Module.Metrics.EditorView.Frame;

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
  BADI.Generic.FunctionDecl,
  ImageList,
  ImgList,
  ExtCtrls, 
  Actions,
  ActnList,
  PlatformDefaultStyleActnCtrls,
  Menus,
  ActnPopup,
  ToolsAPI,
  Themes;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** An enumerate to define the type of information stored in each node - used for counting later. **)
  TBADINodeType = (ntUnkown, ntModule, ntMethod);

  (** An enumerate to define some options for when rendering the module metrics. **)
  TBADIStatsRenderOption = (
    sroClear, // Clears the treeview before rendering
    sroAutoExpand, // Auto expands the the treeview for the rendered module
    sroAutoExpandOnError // Auto expands the the treeview for the rendered module ONLY IF there are
                          // issued
    );
  (** A set of the above enumerate options. **)
  TBADIStatsRenderOptions = Set Of TBADIStatsRenderOption;

  (** A custom virtual string tree to stop a Dark Themed IDE raising Access Violations due to some
      sort of RTTI clash. **)
  TBADIEditorViewVirtualStringTree = Class(TVirtualStringTree);
  
  (** A frame to display a modules methods and their metrics. **)
  TframeBADIModuleMetricsEditorView = Class(TFrame)
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
    Procedure actExpandAllExecute(Sender: TObject);
    Procedure actCollapseAllExecute(Sender: TObject);
    Procedure actExpandUpdate(Sender: TObject);
    Procedure actExpandExecute(Sender: TObject);
    Procedure actCollapseExecute(Sender: TObject);
    Procedure actCollapseUpdate(Sender: TObject);
    Procedure actExpandIssuesExecute(Sender: TObject);
    Procedure vstStatisticsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    Procedure tmFocusTimerTimer(Sender: TObject);
  Strict Private
  Type
      (** A record to describe the data to be held in a tree node. **)
    TBADIStatisticsRecord = Record
      FNodeType: TBADINodeType;
      FText: String;
      FImageIndex: Integer;
      FLinesOfCode: Double;
      FParameterCount: Double;
      FVariableCount: Double;
      FNestedIFDepth: Double;
      FCyclometricComplexity: Double;
      FToxicity: Double;
      FIssueCount : Integer;
    End;
    (** A pointer to the above record. **)
    PBADIStatisticsRecord = ^TBADIStatisticsRecord;
    (** A record type to hold the return information from recursing nodes. **)
    TNodeResultRecord = Record
      FNode: PVirtualNode;
      FChildCount: Integer;
      FIssueCount: Integer;
      Class Operator Add(Const R1, R2: TNodeResultRecord): TNodeResultRecord;
      Constructor Create(Const Node: PVirtualNode);
    End;
  Strict Private
    FLimits        : TBADIStatisticsRecord;
    FLowThreshold  : Double;
    FHighThreshold : Double;
    FFileName      : String;
    FModuleCount   : Integer;
    FMethodCount   : Integer;
    FLinesOfCode   : Integer;
    FUnderLimit    : Integer;
    FAtLimit       : Integer;
    FOverLimit     : Integer;
    FVSTMetrics    : TBADIEditorViewVirtualStringTree;
    {$IFDEF DXE102}
    FStyleServices : TCustomStyleServices;
    FStyleServicesNotifier: Integer;
    {$ENDIF}
  Strict Protected
    Procedure vstStatisticsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; Var CellText: String);
    Procedure vstStatisticsBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      Var ContentRect: TRect);
    Procedure vstStatisticsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; Var Ghosted: Boolean; Var ImageIndex: Integer);
    Procedure vstStatisticsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
      Column: TColumnIndex; Var Result: Integer);
    Procedure vstStatisticsPaintText(Sender: TBaseVirtualTree; Const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    Procedure vstStatisticsHeaderDraw(Sender: TVTHeader;
  HeaderCanvas: TCanvas; Column: TVirtualTreeColumn; R: TRect; Hover,
    Pressed: Boolean; DropMark: TVTDropMarkMode);
    Function RecurseContainer(Const Container: TElementContainer;
      Const Parent: PVirtualNode): TNodeResultRecord;
    Procedure UpdateStats;
    Function ProcessFunction(Const NodeData : PBADIStatisticsRecord;
      Const AFunction : TGenericFunction) : TNodeResultRecord;
    Procedure InitaliseRenderingOptions;
    Procedure DeleteExistingModuleNode(Const Module : TBaseLanguageModule);
    Procedure SortAndExpand(Const NodeResult : TNodeResultRecord;
      Const setRenderOptions : TBADIStatsRenderOptions);
    Function  HasIssues(Const Node : PVirtualNode) : Boolean;
    Procedure ExtractMaxFromChildren(Const ParentNode : PVirtualNode);
    Procedure CreateVirtualStringTree;
    {$IFDEF DXE102}
    Procedure UpdateTreeViewColours;
    {$ENDIF}
  Public
    //: @nometric MissingCONSTInParam
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Procedure RenderModule(Const Module: TBaseLanguageModule;
      Const setRenderOptions: TBADIStatsRenderOptions);
    Procedure CopyToClipboard;
    Procedure FocusResults;
  Published
    (**
      This property returns the number of modules.
      @precon  None.
      @postcon Returns the number of modules.
      @return  an Integer
    **)
    Property ModuleCount: Integer Read FModuleCount;
    (**
      This property returns the number of methods.
      @precon  None.
      @postcon Returns the number of methods.
      @return  an Integer
    **)
    Property MethodCount: Integer Read FMethodCount;
    (**
      This property returns the number of lines of code in the methods.
      @precon  None.
      @postcon Returns the number of lines of code in the methods.
      @return  an Integer
    **)
    Property LinesOfCode: Integer Read FLinesOfCode;
    (**
      This property returns the number of method metrics which are under the limit.
      @precon  None.
      @postcon Returns the number of method metrics which are under the limit.
      @return  an Integer
    **)
    Property UnderLimit: Integer Read FUnderLimit;
    (**
      This property returns the number of method metrics which are at the limit.
      @precon  None.
      @postcon Returns the number of method metrics which are at the limit.
      @return  an Integer
    **)
    Property AtLimit: Integer Read FAtLimit;
    (**
      This property returns the number of method metrics which are over the limit.
      @precon  None.
      @postcon Returns the number of method metrics which are over the limit.
      @return  an Integer
    **)
    Property OverLimit: Integer Read FOverLimit;
  End;

Const
  (** A constant to define a light green colour for the columns if an item is under the limit. **)
  iLightGreen = $80FF80;
  (** A constant to define a light amber colour for the columns if an item is at the limit. **)
  iLightRed = $8080FF;
  (** A constant to define a light red colour for the columns if an item is over the limit. **)
  iLightAmber = $80CCFF;
  (** A constant to define a light aqua colour for the columns if an item is over the limit but which
      have been overridden. **)
  iLightAqua = $FFFF80;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  BADI.ResourceStrings,
  BADI.Options,
  BADI.Types,
  BADI.Functions,
  ClipBrd, 
  BADI.StyleServices.Notifier;

{$R *.dfm}


Type
  (** An enumerate to define the columns in the report. **)
  TBADIMetricColumns = (mcText, mcLength, mcParameters, mcVariables, mcNestIFDepth,
    mcCyclometricComplexity, mcToxicity);

{ TNodeResultRecord }

(**

  This is an operator overload for the Add optioation.

  @precon  None.
  @postcon Adds the numeric fields of the 2 record to a new result.

  @param   R1 as a TNodeResultRecord as a constant
  @param   R2 as a TNodeResultRecord as a constant
  @return  a TNodeResultRecord

**)
Class Operator TframeBADIModuleMetricsEditorView.TNodeResultRecord.Add(Const R1,
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
Constructor TframeBADIModuleMetricsEditorView.TNodeResultRecord.Create(Const Node: PVirtualNode);

Begin
  FNode := Node;
  FChildCount := 0;
  FIssueCount := 0;
End;

(**

  This is an on execute event handler for the CollapseAll action.

  @precon  None.
  @postcon Collapses all the treeview nodes.

  @param   Sender as a TObject

**)
Procedure TframeBADIModuleMetricsEditorView.actCollapseAllExecute(Sender: TObject);

Begin
  FVSTMetrics.FullCollapse();
End;

(**

  This is an on execute event handler for the Collapse action.

  @precon  None.
  @postcon Collapses the focused treeview node.

  @param   Sender as a TObject

**)
Procedure TframeBADIModuleMetricsEditorView.actCollapseExecute(Sender: TObject);

Begin
  FVSTMetrics.FullCollapse(FVSTMetrics.FocusedNode);
End;

(**

  This is an on update event handler for the CollapseUpdate action.

  @precon  None.
  @postcon Enables the Collapse action is there is a focused node, it has children and is currently
           expanded.

  @param   Sender as a TObject

**)
Procedure TframeBADIModuleMetricsEditorView.actCollapseUpdate(Sender: TObject);

Var
  A: TAction;

Begin
  If Sender Is TAction Then
    Begin
      A := Sender As TAction;
      A.Enabled :=
        Assigned(FVSTMetrics.FocusedNode) And
        (FVSTMetrics.ChildCount[FVSTMetrics.FocusedNode] > 0) And 
        FVSTMetrics.Expanded[FVSTMetrics.FocusedNode];
    End;
End;

(**

  This is an on execute event handler for the ExpandAll action.

  @precon  None.
  @postcon Expands all the nodes in the treeview.

  @param   Sender as a TObject

**)
Procedure TframeBADIModuleMetricsEditorView.actExpandAllExecute(Sender: TObject);

Begin
  FVSTMetrics.FullExpand();
End;

(**

  This is an on execute event handler for the Expand action.

  @precon  None.
  @postcon Expands the focused node in the treeview.

  @param   Sender as a TObject

**)
Procedure TframeBADIModuleMetricsEditorView.actExpandExecute(Sender: TObject);

Begin
  FVSTMetrics.FullExpand(FVSTMetrics.FocusedNode);
End;

(**

  This is an on execute event handler for the ExpandIssues action.

  @precon  None.
  @postcon Expand all root nodes that have issues in them and collapses all others.

  @param   Sender as a TObject

**)
Procedure TframeBADIModuleMetricsEditorView.actExpandIssuesExecute(Sender: TObject);

Var
  N: PVirtualNode;

Begin
  N := FVSTMetrics.GetFirst();
  While Assigned(N) Do
    Begin
      If HasIssues(N) Then
        FVSTMetrics.FullExpand(N)
      Else
        FVSTMetrics.FullCollapse(N);
      N := FVSTMetrics.GetNextSibling(N);
    End;
End;

(**

  This is an on update event handler for the Expand action.

  @precon  None.
  @postcon Enables the expand action of there is a focused node, it has children and is collapsed.

  @param   Sender as a TObject

**)
Procedure TframeBADIModuleMetricsEditorView.actExpandUpdate(Sender: TObject);

Var
  A: TAction;

Begin
  If Sender Is TAction Then
    Begin
      A := Sender As TAction;
      A.Enabled :=
        Assigned(FVSTMetrics.FocusedNode) And
        (FVSTMetrics.ChildCount[FVSTMetrics.FocusedNode] > 0) And 
        Not FVSTMetrics.Expanded[FVSTMetrics.FocusedNode];
    End;
End;

(**

  This method copies the treeview information to the clipboard.

  @precon  None.
  @postcon The treeview information is copied to the clipboard.

**)
Procedure TframeBADIModuleMetricsEditorView.CopyToClipboard;

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
  strClipboardHeader = 'BADI Statistics for %s'#13#10;

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
      Node := FVSTMetrics.GetFirst();
      While Assigned(Node) Do
        Begin
          strLine := '';
          For iColumn := 0 To FVSTMetrics.Header.Columns.Count - 1 Do
            Begin
              If strLine <> '' Then
                strLine := strLine + #9;
              Case iColumn Of
                0: strLine := strLine + StringOfChar(#32, iMultipler * ParentCount(Node)) +
                    FVSTMetrics.Text[Node, iColumn];
              Else
                strLine := strLine + FVSTMetrics.Text[Node, iColumn];
              End;
            End;
          strText := strText + strLine + #13#10;
          Node := FVSTMetrics.GetNext(Node);
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

  A constructor for the TframeBADIModuleStatistics class.

  @precon  None.
  @postcon Initialises the treeview and the image list.

  @nocheck MissingCONSTInParam

  @param   AOwner as a TComponent

**)
Constructor TframeBADIModuleMetricsEditorView.Create(AOwner: TComponent);

{$IFDEF DXE102}
Var
  ITS : IOTAIDEThemingServices;
{$ENDIF}
  
Begin
  Inherited Create(AOwner);
  CreateVirtualStringTree;
  FVSTMetrics.NodeDataSize := SizeOf(TBADIStatisticsRecord);
  LoadBADIImages(ilScopeImages);
  {$IFDEF DXE102}
  FStyleServicesNotifier := -1;
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
    FStyleServicesNotifier := ITS.AddNotifier(TBADIStyleServicesNotifier.Create(UpdateTreeViewColours));
  {$ENDIF}
End;

(**

  This method creates the virtual string tree to display the metrics.

  @precon  None.
  @postcon The virtual string tree is created.

  @nocheck HardCodedInteger HardCodedString
  @nometric LongMethod Toxicity

**)
Procedure TframeBADIModuleMetricsEditorView.CreateVirtualStringTree;

Var
  C: TVirtualTreeColumn;

Begin
  FVSTMetrics := TBADIEditorViewVirtualStringTree.Create(Self);
  FVSTMetrics.Name := 'vstMetrics';
  FVSTMetrics.Parent := Self;
  FVSTMetrics.Left := 0;
  FVSTMetrics.Top := 0;
  FVSTMetrics.Width := 894;
  FVSTMetrics.Height := 498;
  FVSTMetrics.Align := alClient;
  FVSTMetrics.EmptyListMessage := 'Nothing to see here....';
  FVSTMetrics.Header.AutoSizeIndex := 0;
  FVSTMetrics.Header.Options := [hoAutoResize, hoOwnerDraw, hoVisible];
  FVSTMetrics.Header.ParentFont := True;
  FVSTMetrics.HintAnimation := hatFade;
  FVSTMetrics.HintMode := hmTooltip;
  FVSTMetrics.Images := ilScopeImages;
  FVSTMetrics.PopupMenu := pabContextMenu;
  FVSTMetrics.ScrollBarOptions.AlwaysVisible := True;
  FVSTMetrics.SelectionBlendFactor := 64;
  FVSTMetrics.TabOrder := 0;
  FVSTMetrics.TreeOptions.MiscOptions := [toFullRepaintOnResize, toGridExtensions, toInitOnSave,
    toToggleOnDblClick, toWheelPanning];
  FVSTMetrics.TreeOptions.PaintOptions := [toShowButtons, toShowDropmark, toShowHorzGridLines,
    toShowTreeLines, toShowVertGridLines, toThemeAware, toUseBlendedImages, toGhostedIfUnfocused,
    toUseBlendedSelection];
  {$IFDEF DXE102}
  UpdateTreeViewColours;
  {$ENDIF}
  FVSTMetrics.TreeOptions.SelectionOptions := [toExtendedFocus, toFullRowSelect, toRightClickSelect];
  FVSTMetrics.OnBeforeCellPaint := vstStatisticsBeforeCellPaint;
  FVSTMetrics.OnCompareNodes := vstStatisticsCompareNodes;
  FVSTMetrics.OnFreeNode := vstStatisticsFreeNode;
  FVSTMetrics.OnGetText := vstStatisticsGetText;
  FVSTMetrics.OnPaintText := vstStatisticsPaintText;
  FVSTMetrics.OnGetImageIndex := vstStatisticsGetImageIndex;
  FVSTMetrics.OnHeaderDraw := vstStatisticsHeaderDraw;
  C := FVSTMetrics.Header.Columns.Add;
  C.MinWidth := 300;
  C.Position := 0;
  C.Style := vsOwnerDraw;
  C.Width := 423;
  C.Text := 'Method';
  C := FVSTMetrics.Header.Columns.Add;
  C.Alignment := taCenter;
  C.Position := 1;
  C.Style := vsOwnerDraw;
  C.Width := 75;
  C.Text := 'Lines';
  C := FVSTMetrics.Header.Columns.Add;
  C.Alignment := taCenter;
  C.Position := 2;
  C.Style := vsOwnerDraw;
  C.Width := 75;
  C.Text := 'Params';
  C := FVSTMetrics.Header.Columns.Add;
  C.Alignment := taCenter;
  C.Position := 3;
  C.Style := vsOwnerDraw;
  C.Width := 75;
  C.Text := 'Variables';
  C := FVSTMetrics.Header.Columns.Add;
  C.Alignment := taCenter;
  C.Position := 4;
  C.Style := vsOwnerDraw;
  C.Width := 75;
  C.Text := 'IF Depth';
  C := FVSTMetrics.Header.Columns.Add;
  C.Alignment := taCenter;
  C.Position := 5;
  C.Style := vsOwnerDraw;
  C.Width := 75;
  C.Text := 'Complexity';
  C := FVSTMetrics.Header.Columns.Add;
  C.Alignment := taCenter;
  C.Position := 6;
  C.Style := vsOwnerDraw;
  C.Width := 75;
  C.Text := 'Toxicity';
End;

(**

  This method deletes a root node from the treeview if it matches the given module name.

  @precon  Module must be a valid instance.
  @postcon The modules node it deleted if found.

  @param   Module as a TBaseLanguageModule as a constant

**)
Procedure TframeBADIModuleMetricsEditorView.DeleteExistingModuleNode(Const Module : TBaseLanguageModule);

Var
  Node: PVirtualNode;

Begin
  Node := FVSTMetrics.GetFirstChild(FVSTMetrics.RootNode);
  While Assigned(Node) Do
    Begin
      If FVSTMetrics.Text[Node, 0] = Module.AsString(True, False) Then
        Begin
          FVSTMetrics.DeleteNode(Node);
          Break;
        End;
      Node := FVSTMetrics.GetNextSibling(Node);
    End;
End;

(**

  A destructor for the TframeBADIModuleMetricsEditorView class.

  @precon  None.
  @postcon Does nothing.

**)
Destructor TframeBADIModuleMetricsEditorView.Destroy;

{$IFDEF DXE102}
Var
  ITS : IOTAIDEThemingServices;
{$ENDIF}
  
Begin
  {$IFDEF DXE102}
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
    ITS.RemoveNotifier(FStyleServicesNotifier);
  {$ENDIF}
  Inherited Destroy;
End;

(**

  This method searches the given nodes children and updates the parent node with the max values from the
  child nodes.

  @precon  ParentNode must be a valid reference.
  @postcon The parent node is updates with max values from the chlid nodes.

  @param   ParentNode as a PVirtualNode as a constant

**)
Procedure TframeBADIModuleMetricsEditorView.ExtractMaxFromChildren(Const ParentNode : PVirtualNode);

Var
  ParentNodeData : PBADIStatisticsRecord;
  NodeData : PBADIStatisticsRecord;
  Node: PVirtualNode;

Begin
  If Not Assigned(ParentNode) Then
    Exit;
  ParentNodeData := FVSTMetrics.GetNodeData(ParentNode);
  Node := FVSTMetrics.GetFirstChild(ParentNode);
  While Assigned(Node) Do
    Begin
      NodeData := FVSTMetrics.GetNodeData(Node);
      If NodeData.FLinesOfCode > ParentNodeData.FLinesOfCode Then
        ParentNodeData.FLinesOfCode := NodeData.FLinesOfCode;
      If NodeData.FParameterCount > ParentNodeData.FParameterCount Then
        ParentNodeData.FParameterCount := NodeData.FParameterCount;
      If NodeData.FVariableCount > ParentNodeData.FVariableCount Then
        ParentNodeData.FVariableCount := NodeData.FVariableCount;
      If NodeData.FNestedIFDepth > ParentNodeData.FNestedIFDepth Then
        ParentNodeData.FNestedIFDepth := NodeData.FNestedIFDepth;
      If NodeData.FCyclometricComplexity > ParentNodeData.FCyclometricComplexity Then
        ParentNodeData.FCyclometricComplexity := NodeData.FCyclometricComplexity;
      If NodeData.FToxicity > ParentNodeData.FToxicity Then
        ParentNodeData.FToxicity := NodeData.FToxicity;
      Node := FVSTMetrics.GetNextSibling(Node);
    End;
End;

(**

  This method starts the time to focus the frames treeview to prevent AVs in the editor if there is no
  focused control.

  @precon  None.
  @postcon The timer is started to focus the treeview.

**)
Procedure TframeBADIModuleMetricsEditorView.FocusResults;

Begin
  tmFocusTimer.Enabled := True;
End;

(**

  This method counts the numnber of issues in all the given nodes children and returns true if that
  number is not zero.

  @precon  Node must be a valid node reference.
  @postcon Returns true if any child node has issues.

  @param   Node as a PVirtualNode as a constant
  @return  a Boolean

**)
Function TframeBADIModuleMetricsEditorView.HasIssues(Const Node: PVirtualNode): Boolean;

Var
  iCounter : Integer;
  
Begin
  FVSTMetrics.IterateSubtree(
    Node,
    Procedure(Sender: TBaseVirtualTree; ptrNode: PVirtualNode; ptrData: Pointer; Var boolAbort: Boolean)
    Var
      NodeData : PBADIStatisticsRecord;
    Begin
      NodeData := Sender.GetNodeData(ptrNode);
      Inc(iCounter, NodeData.FIssueCount);
    End,
    Nil);
  Result := iCounter > 0;
End;

(**

  This method initialises the limits record in the class with the information from the options.

  @precon  None.
  @postcon The limits are initialised.

**)
Procedure TframeBADIModuleMetricsEditorView.InitaliseRenderingOptions;

Const
  dblPercentageDivisor = 100.0;

Begin
  FLimits.FLinesOfCode := TBADIOptions.BADIOptions.ModuleMetric[mmLongMethods].FLimit;
  FLimits.FParameterCount := TBADIOptions.BADIOptions.ModuleMetric[mmLongParameterLists].FLimit;
  FLimits.FVariableCount := TBADIOptions.BADIOptions.ModuleMetric[mmLongMethodVariableLists].FLimit;
  FLimits.FNestedIFDepth := TBADIOptions.BADIOptions.ModuleMetric[mmNestedIFDepth].FLimit;
  FLimits.FCyclometricComplexity :=
    TBADIOptions.BADIOptions.ModuleMetric[mmCyclometricComplexity].FLimit;
  FLimits.FToxicity := TBADIOptions.BADIOptions.ModuleMetric[mmToxicity].FLimit;
  FLowThreshold := TBADIOptions.BADIOptions.LowMetricMargin / dblPercentageDivisor;
  FHighThreshold := TBADIOptions.BADIOptions.HighMetricMargin / dblPercentageDivisor;
End;

(**

  This method processes the given function and returns its method information.

  @precon  None.
  @postcon The metric information for the given function is returned.

  @param   NodeData  as a PBADIStatisticsRecord as a constant
  @param   AFunction as a TGenericFunction as a constant
  @return  a TNodeResultRecord

**)
Function TframeBADIModuleMetricsEditorView.ProcessFunction(Const NodeData : PBADIStatisticsRecord;
  Const AFunction : TGenericFunction) : TNodeResultRecord;

Var
  E: TElementContainer;

Begin
  Result.Create(Nil);
  NodeData.FNodeType := ntMethod;
  NodeData.FLinesOfCode := 0;
  If AFunction.StmtCount > 0 Then
    NodeData.FLinesOfCode := AFunction.LineofCode;
  NodeData.FParameterCount := AFunction.ParameterCount;
  Inc(Result.FIssueCount, Integer(NodeData.FParameterCount > FLimits.FParameterCount));
  Inc(NodeData.FIssueCount, Integer(NodeData.FParameterCount > FLimits.FParameterCount));
  NodeData.FVariableCount := 0;
  E := AFunction.FindElement(strVarsLabel);
  If Assigned(E) Then
    NodeData.FVariableCount := E.ElementCount;
  Inc(Result.FIssueCount, Integer(NodeData.FVariableCount > FLimits.FVariableCount));
  Inc(NodeData.FIssueCount, Integer(NodeData.FVariableCount > FLimits.FVariableCount));
  NodeData.FNestedIFDepth := AFunction.NestedIFDepth;
  Inc(Result.FIssueCount, Integer(NodeData.FNestedIFDepth > FLimits.FNestedIFDepth));
  Inc(NodeData.FIssueCount, Integer(NodeData.FNestedIFDepth > FLimits.FNestedIFDepth));
  NodeData.FCyclometricComplexity := AFunction.CyclometricComplexity;
  Inc(Result.FIssueCount, Integer(NodeData.FCyclometricComplexity > FLimits.FCyclometricComplexity));
  Inc(NodeData.FIssueCount, Integer(NodeData.FCyclometricComplexity > FLimits.FCyclometricComplexity));
  NodeData.FToxicity := AFunction.Toxicity;
  Inc(Result.FIssueCount, Integer(NodeData.FToxicity > FLimits.FToxicity));
  Inc(NodeData.FIssueCount, Integer(NodeData.FToxicity > FLimits.FToxicity));
  Inc(Result.FChildCount);
End;

(**

  This method recursively walks the given container rendering its contents in the tree view. If the 
  container is a generic function, metrics are extracted from the method and dislpayed. Any branches with
  out methods are pruned.

  @precon  Container must be a valid instance.
  @postcon The metrics of the module method are displayed.

  @param   Container as a TElementContainer as a constant
  @param   Parent    as a PVirtualNode as a constant
  @return  a TNodeResultRecord

**)
Function TframeBADIModuleMetricsEditorView.RecurseContainer(Const Container: TElementContainer;
  Const Parent: PVirtualNode): TNodeResultRecord;

Var
  NodeData: PBADIStatisticsRecord;
  iElement: Integer;
  M: TGenericFunction;

Begin
  Result.Create(FVSTMetrics.AddChild(Parent));
  NodeData := FVSTMetrics.GetNodeData(Result.FNode);
  NodeData.FNodeType := ntUnkown;
  If Not Assigned(Container.Parent) Then
    NodeData.FNodeType := ntModule;
  NodeData.FText := Container.AsString(True, False);
  NodeData.FImageIndex := BADIImageIndex(Container.ImageIndex, Container.Scope);
  If Container Is TGenericFunction Then
    Begin
      M := Container As TGenericFunction;
      If Not M.IsDeclarationOnly Then
        Result := Result + ProcessFunction(NodeData, M);
    End;
  For iElement := 1 To Container.ElementCount Do
    Result := Result + RecurseContainer(Container.Elements[iElement], Result.FNode);
  If Result.FChildCount = 0 Then
    Begin
      FVSTMetrics.DeleteNode(Result.FNode);
      Result.FNode := Nil;
    End;
  If Not (Container Is TGenericFunction) Then
    ExtractMaxFromChildren(Result.FNode);
End;

(**

  This method starts the process of rendering the module contents.

  @precon  Module must be valid.
  @postcon The modules methods and their metrics are rendered.

  @param   Module           as a TBaseLanguageModule as a constant
  @param   setRenderOptions as a TBADIStatsRenderOptions as a constant

**)
Procedure TframeBADIModuleMetricsEditorView.RenderModule(Const Module: TBaseLanguageModule;
  Const setRenderOptions: TBADIStatsRenderOptions);

Var
  NodeResult: TNodeResultRecord;

Begin
  InitaliseRenderingOptions;
  If Assigned(Module) And Assigned(FVSTMetrics) Then
    Begin
      FFileName := Module.FileName;
      FVSTMetrics.BeginUpdate;
      Try
        If sroClear In setRenderOptions Then
          FVSTMetrics.Clear
        Else
          DeleteExistingModuleNode(Module);
        NodeResult := RecurseContainer(Module, Nil);
        SortAndExpand(NodeResult, setRenderOptions);
        UpdateStats;
      Finally
        FVSTMetrics.EndUpdate;
      End;
    End;
  FocusResults;
End;

(**

  This method sorts and expands the rendered module.

  @precon  None.
  @postcon The rendered node (if valid) is sorted and expanded.

  @param   NodeResult       as a TNodeResultRecord as a constant
  @param   setRenderOptions as a TBADIStatsRenderOptions as a constant

**)
Procedure TframeBADIModuleMetricsEditorView.SortAndExpand(Const NodeResult : TNodeResultRecord;
  Const setRenderOptions : TBADIStatsRenderOptions);

Begin
  If Assigned(NodeResult.FNode) Then
    If sroAutoExpand In setRenderOptions Then
      Begin
        If ((sroAutoExpandOnError In setRenderOptions) And (NodeResult.FIssueCount > 0)) Or
          Not (sroAutoExpandOnError In setRenderOptions) Then
        FVSTMetrics.FullExpand(NodeResult.FNode);
      End;
End;

(**

  This treeview needs to be focus to stop mouse scrolling message being directed to the editor (which
  does not exists) so this timer waits for the treeview to be visisble and then gives it focus.

  @precon  None.
  @postcon Focuses the treeview when it is visible.

  @param   Sender as a TObject

**)
Procedure TframeBADIModuleMetricsEditorView.tmFocusTimerTimer(Sender: TObject);

Begin
  If FVSTMetrics.CanFocus Then
    Begin
      tmFocusTimer.Enabled := False;
      FVSTMetrics.SetFocus;
    End;
End;

(**

  This method updates the module count, method count and under, at and over limits counts.

  @precon  None.
  @postcon The counts are updated.

**)
Procedure TframeBADIModuleMetricsEditorView.UpdateStats;

  (**

    This procedure updates the FUnderLimit, FAtLimit or FOverLimit fields depending upon the value
    of the metric compared to the limit.

    @precon  None.
    @postcon Updates the FUnderLimit, FAtLimit or FOverLimit fields depending upon the value
             of the metric compared to the limit.

    @param   dblValue as a Double as a constant
    @param   dblLimit as a Double as a constant

  **)
  Procedure Update(Const dblValue, dblLimit: Double);

  Var
    dblRatio: Double;

  Begin
    dblRatio := dblValue / dblLimit;
    If dblRatio > 0 Then
      If dblRatio < FLowThreshold Then
        Inc(FUnderLimit)
      Else If dblRatio > FHighThreshold Then
        Inc(FOverLimit)
      Else
        Inc(FAtLimit)
  End;

Var
  Node: PVirtualNode;
  NodeData: PBADIStatisticsRecord;

Begin
  FModuleCount := 0;
  FMethodCount := 0;
  FLinesOfCode := 0;
  FUnderLimit := 0;
  FAtLimit := 0;
  FOverLimit := 0;
  Node := FVSTMetrics.GetFirst();
  While Assigned(Node) Do
    Begin
      NodeData := FVSTMetrics.GetNodeData(Node);
      Case NodeData.FNodeType Of
        ntModule: Inc(FModuleCount);
        ntMethod:
          Begin
            Inc(FMethodCount);
            Inc(FLinesOfCode, Trunc(NodeData.FLinesOfCode));
            Update(NodeData.FLinesOfCode, FLimits.FLinesOfCode);
            Update(NodeData.FParameterCount, FLimits.FParameterCount);
            Update(NodeData.FVariableCount, FLimits.FVariableCount);
            Update(NodeData.FNestedIFDepth, FLimits.FNestedIFDepth);
            Update(NodeData.FCyclometricComplexity, FLimits.FCyclometricComplexity);
            Update(NodeData.FToxicity, FLimits.FToxicity);
          End;
      End;
      Node := FVSTMetrics.GetNext(Node);
    End;
End;

{$IFDEF DXE102}
(**

  This method updates the colours of the treeview (those that actually work) so that it takes on the
  theme style of the IDE.

  @precon  None.
  @postcon If the IDE is using style services then the colours of the treeview are updated.

**)
Procedure TframeBADIModuleMetricsEditorView.UpdateTreeViewColours;

Var
  ITS : IOTAIDEThemingServices;
  Clrs: TVTColors;

Begin
  FStyleServices := Nil;
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
    If ITS.IDEThemingEnabled Then
      FStyleServices := ITS.StyleServices;
  If Assigned(FStyleServices) And FStyleServices.Enabled Then
    Begin
      FVSTMetrics.Color := FStyleServices.GetSystemColor(FVSTMetrics.Color);
      FVSTMetrics.Font.Color := FStyleServices.GetSystemColor(FVSTMetrics.Font.Color);
      Clrs := FVSTMetrics.Colors;
      Clrs.BorderColor := FStyleServices.GetSystemColor(Clrs.BorderColor);
      Clrs.DisabledColor := FStyleServices.GetSystemColor(Clrs.DisabledColor);
      Clrs.DropMarkColor := FStyleServices.GetSystemColor(Clrs.DropMarkColor);
      Clrs.DropTargetColor := FStyleServices.GetSystemColor(Clrs.DropTargetColor);
      Clrs.DropTargetBorderColor := FStyleServices.GetSystemColor(Clrs.DropTargetBorderColor);
      Clrs.FocusedSelectionColor := FStyleServices.GetSystemColor(Clrs.FocusedSelectionColor);
      Clrs.FocusedSelectionBorderColor := FStyleServices.GetSystemColor(Clrs.FocusedSelectionBorderColor);
      Clrs.GridLineColor := FStyleServices.GetSystemColor(Clrs.GridLineColor);
      Clrs.HotColor := FStyleServices.GetSystemColor(Clrs.HotColor);
      Clrs.SelectionRectangleBlendColor := FStyleServices.GetSystemColor(Clrs.SelectionRectangleBlendColor);
      Clrs.SelectionRectangleBorderColor := FStyleServices.GetSystemColor(Clrs.SelectionRectangleBorderColor);
      Clrs.SelectionTextColor := FStyleServices.GetSystemColor(Clrs.SelectionTextColor);
      Clrs.TreeLineColor := FStyleServices.GetSystemColor(Clrs.TreeLineColor);
      Clrs.UnfocusedSelectionColor := FStyleServices.GetSystemColor(Clrs.UnfocusedSelectionColor);
      Clrs.UnfocusedSelectionBorderColor := FStyleServices.GetSystemColor(Clrs.UnfocusedSelectionBorderColor);
      FVSTMetrics.Invalidate;
    End;
End;
{$ENDIF}

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
Procedure TframeBADIModuleMetricsEditorView.vstStatisticsBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode;
  CellRect: TRect; Var ContentRect: TRect);

  (**

    This function returns the colours to be used based on the value and the limit.

    @precon  dblLimit cannot be zero.
    @postcon Returns the colours to be used based on the value and the limit.

    @param   dblValue as a Double as a constant
    @param   dblLimit as a Double as a constant
    @return  a TColor

  **)
  Function Colour(Const dblValue, dblLimit: Double): TColor;

  Const
    dblBlendRatioModule = 0.25;

  Var
    dblRatio: Double;

  Begin
    Result := TargetCanvas.Brush.Color;
    If dblValue > 0 Then
      Begin
        dblRatio := dblValue / dblLimit;
        If dblRatio < FLowThreshold Then
          Result := BlendColour(iLightGreen, Result, dblBlendRatioModule)
        Else If dblRatio > FHighThreshold Then
          Result := BlendColour(iLightRed, Result, dblBlendRatioModule)
        Else
          Result := BlendColour(iLightAmber, Result, dblBlendRatioModule);
      End;
  End;

Var
  NodeData: PBADIStatisticsRecord;

Begin
  NodeData := FVSTMetrics.GetNodeData(Node);
  TargetCanvas.Brush.Color := clWindow;
  {$IFDEF DXE102}
  If Assigned(FStyleServices) And FStyleServices.Enabled Then
    TargetCanvas.Brush.Color := FStyleServices.GetSystemColor(clWindow);
  {$ENDIF}
  Case TBADIMetricColumns(Column) Of
    mcLength: TargetCanvas.Brush.Color := Colour(NodeData.FLinesOfCode, FLimits.FLinesOfCode);
    mcParameters: TargetCanvas.Brush.Color := Colour(NodeData.FParameterCount, FLimits.FParameterCount);
    mcVariables: TargetCanvas.Brush.Color := Colour(NodeData.FVariableCount, FLimits.FVariableCount);
    mcNestIFDepth: TargetCanvas.Brush.Color := Colour(NodeData.FNestedIFDepth, FLimits.FNestedIFDepth);
    mcCyclometricComplexity: TargetCanvas.Brush.Color := Colour(NodeData.FCyclometricComplexity,
        FLimits.FCyclometricComplexity);
    mcToxicity: TargetCanvas.Brush.Color := Colour(NodeData.FToxicity, FLimits.FToxicity);
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
Procedure TframeBADIModuleMetricsEditorView.vstStatisticsCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; Var Result: Integer);

Begin
  Result := CompareText(FVSTMetrics.Text[Node1, 0], FVSTMetrics.Text[Node2, 0]);
End;

(**

  This is an on FreeNdoe event handler for the treeview.

  @precon  None.
  @postcon Ensures that the managed types in the node are freed.

  @param   Sender as a TBaseVirtualTree
  @param   Node   as a PVirtualNode

**)
Procedure TframeBADIModuleMetricsEditorView.vstStatisticsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);

Var
  NodeData : PBADIStatisticsRecord;
  
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
  @param   ImageIndex as an Integer as a reference

**)
Procedure TframeBADIModuleMetricsEditorView.vstStatisticsGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; Var Ghosted: Boolean;
  Var ImageIndex: Integer);

Var
  NodeData: PBADIStatisticsRecord;

Begin
  If Column = 0 Then
    Begin
      NodeData := FVSTMetrics.GetNodeData(Node);
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
Procedure TframeBADIModuleMetricsEditorView.vstStatisticsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; Var CellText: String);

Var
  NodeData: PBADIStatisticsRecord;

Begin
  NodeData := FVSTMetrics.GetNodeData(Node);
  CellText := '';
  Case TBADIMetricColumns(Column) Of
    mcText: CellText := NodeData.FText;
    mcLength:
      If NodeData.FLinesOfCode > 0 Then
        CellText := Format('%1.0n', [Int(NodeData.FLinesOfCode)]);
    mcParameters:
      If NodeData.FParameterCount > 0 Then
        CellText := Format('%1.0n', [Int(NodeData.FParameterCount)]);
    mcVariables:
      If NodeData.FVariableCount > 0 Then
        CellText := Format('%1.0n', [Int(NodeData.FVariableCount)]);
    mcNestIFDepth:
      If NodeData.FNestedIFDepth > 0 Then
        CellText := Format('%1.0n', [Int(NodeData.FNestedIFDepth)]);
    mcCyclometricComplexity:
      If NodeData.FCyclometricComplexity > 0 Then
        CellText := Format('%1.0n', [Int(NodeData.FCyclometricComplexity)]);
    mcToxicity:
      If NodeData.FToxicity > 0 Then
        CellText := Format('%1.3n', [NodeData.FToxicity]);
  End;
End;

(**

  This is an on header draw event handler for the treeview - the only way to get a themed header in the
  new 10.2.2 IDE.

  @precon  None.
  @postcon Draws the column headers.

  @param   Sender       as a TVTHeader
  @param   HeaderCanvas as a TCanvas
  @param   Column       as a TVirtualTreeColumn
  @param   R            as a TRect
  @param   Hover        as a Boolean
  @param   Pressed      as a Boolean
  @param   DropMark     as a TVTDropMarkMode

**)
Procedure TframeBADIModuleMetricsEditorView.vstStatisticsHeaderDraw(Sender: TVTHeader;
  HeaderCanvas: TCanvas; Column: TVirtualTreeColumn; R: TRect; Hover,
    Pressed: Boolean; DropMark: TVTDropMarkMode);

Const
  iTextPadding = 6;
  
Var
  strText : String;
  setTextFormats : TTextFormat;
  
Begin
  HeaderCanvas.Brush.Color := clBtnFace;
  {$IFDEF DXE102}
  If Assigned(FStyleServices) And FStyleServices.Enabled Then
    HeaderCanvas.Brush.Color := FStyleServices.GetSystemColor(clBtnFace);
  {$ENDIF}
  HeaderCanvas.FillRect(R);
  strText := Column.Text;
  InflateRect(R, -iTextPadding, 0);
  setTextFormats := [tfVerticalCenter, tfEndEllipsis];
  Case Column.Alignment Of
    taLeftJustify:  Include(setTextFormats, tfLeft);
    taRightJustify: Include(setTextFormats, tfRight);
    taCenter:       Include(setTextFormats, tfCenter);
  End;
  HeaderCanvas.TextRect(R, strText, setTextFormats);
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
Procedure TframeBADIModuleMetricsEditorView.vstStatisticsPaintText(Sender: TBaseVirtualTree;
  Const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);

Var
  NodeData : PBADIStatisticsRecord;
  
Begin
  TargetCanvas.Font.Style := [];
  TargetCanvas.Font.Color := clWindowText;
  {$IFDEF DXE102}
  If Assigned(FStyleServices) And FStyleServices.Enabled Then
    TargetCanvas.Font.Color := FStyleServices.GetSystemColor(clWindowText);
  {$ENDIF}
  Case TBADIMetricColumns(Column) Of
    mcLength..mcToxicity: TargetCanvas.Font.Color := clBlack;
  End;
  NodeData := FVSTMetrics.GetNodeData(Node);
  If Not (NodeData.FNodeType In [ntMethod]) Then
    Begin
      If Column = 0 Then
        TargetCanvas.Font.Style := [fsBold];
    End;
End;

End.
