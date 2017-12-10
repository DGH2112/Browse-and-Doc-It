(**
  
  This module contains a frame for dislpaying module methods and their metrics.

  @Author  David Hoyle
  @Version 1.0
  @Date    10 Dec 2017

  @todo    Add the ability for the issues to be overridden (show a different colour but does n0t add to
           issue list).

  @bug     If vstStatistics.ScrollBarOptions.AlwaysVisible is not TRUE track pad scrolling AVs editor.
  
**)
Unit BADI.Module.Statistics.Frame;

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
  ActnPopup;

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

  (** A frame to display a modules methods and their metrics. **)
  TframeBADIModuleStatistics = Class(TFrame)
    vstStatistics: TVirtualStringTree;
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
    Procedure vstStatisticsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; Var CellText: String);
    Procedure vstStatisticsBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      Var ContentRect: TRect);
    Procedure vstStatisticsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; Var Ghosted: Boolean; Var ImageIndex: Integer);
    Procedure tmFocusTimerTimer(Sender: TObject);
    Procedure vstStatisticsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
      Column: TColumnIndex; Var Result: Integer);
    Procedure vstStatisticsPaintText(Sender: TBaseVirtualTree; Const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure actExpandAllExecute(Sender: TObject);
    procedure actCollapseAllExecute(Sender: TObject);
    procedure actExpandUpdate(Sender: TObject);
    procedure actExpandExecute(Sender: TObject);
    procedure actCollapseExecute(Sender: TObject);
    procedure actCollapseUpdate(Sender: TObject);
    procedure actExpandIssuesExecute(Sender: TObject);
    procedure vstStatisticsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
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
  Strict Protected
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
  ClipBrd;

{$R *.dfm}


Type
  (** An enumerate to define the columns in the report. **)
  TBADIMetricColumns = (mcText, mcLength, mcParameters, mcVariables, mcNestIFDepth,
    mcCyclometricComplexity, mcToxicity);

(**

  This method blends the given colours by the given ratio and returns the blended colour.

  @precon  None.
  @postcon Returns the blended colour.

  @param   iBaseColour      as a TColor as a constant
  @param   iHighlightColour as a TColor as a constant
  @param   dblBlend         as a Double as a constant
  @return  a TColor

**)
Function BlendColour(Const iBaseColour, iHighlightColour : TColor; Const dblBlend : Double) : TColor;

  (**

    This method returns a pro rataed colours between the two input colours.

    @precon  None.
    @postcon Returns a pro rataed colours between the two input colours.

    @param   iBase as a TColor as a constant
    @param   iHigh as a TColor as a constant
    @return  a TColor

  **)
  Function ProRataColor(Const iBase, iHigh : TColor) : TColor;

  Begin
    Result := iBase + Trunc(Int(iHigh - iBase) * dblBlend);
  End;

Const
  iRed = $0000FF;
  iGreen = $00FF00;
  iBlue = $FF0000;
  iGreenShift = 8;
  iBlueShift = 16;

Var
  R, G, B : TColor;
  BC, HC : TColor;

Begin
  HC := ColorToRGB(iHighlightColour);
  BC := ColorToRGB(iBaseColour);
  R := ProRataColor(BC And iRed, HC And iRed);
  G := ProRataColor((BC And iGreen) Shr iGreenShift, (HC And iGreen) Shr iGreenShift);
  B := ProRataColor((BC And iBlue) Shr iBlueShift, (HC And iBlue) Shr iBlueShift);
  Result := RGB(R, G, B);
End;

{ TNodeResultRecord }

(**

  This is an operator overload for the Add optioation.

  @precon  None.
  @postcon Adds the numeric fields of the 2 record to a new result.

  @param   R1 as a TNodeResultRecord as a constant
  @param   R2 as a TNodeResultRecord as a constant
  @return  a TNodeResultRecord

**)
Class Operator TframeBADIModuleStatistics.TNodeResultRecord.Add(Const R1,
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
Constructor TframeBADIModuleStatistics.TNodeResultRecord.Create(Const Node: PVirtualNode);

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
Procedure TframeBADIModuleStatistics.actCollapseAllExecute(Sender: TObject);

Begin
  vstStatistics.FullCollapse();
End;

(**

  This is an on execute event handler for the Collapse action.

  @precon  None.
  @postcon Collapses the focused treeview node.

  @param   Sender as a TObject

**)
Procedure TframeBADIModuleStatistics.actCollapseExecute(Sender: TObject);

Begin
  vstStatistics.FullCollapse(vstStatistics.FocusedNode);
End;

(**

  This is an on update event handler for the CollapseUpdate action.

  @precon  None.
  @postcon Enables the Collapse action is there is a focused node, it has children and is currently
           expanded.

  @param   Sender as a TObject

**)
Procedure TframeBADIModuleStatistics.actCollapseUpdate(Sender: TObject);

Var
  A: TAction;

Begin
  If Sender Is TAction Then
    Begin
      A := Sender As TAction;
      A.Enabled :=
        Assigned(vstStatistics.FocusedNode) And
        (vstStatistics.ChildCount[vstStatistics.FocusedNode] > 0) And 
        vstStatistics.Expanded[vstStatistics.FocusedNode];
    End;
End;

(**

  This is an on execute event handler for the ExpandAll action.

  @precon  None.
  @postcon Expands all the nodes in the treeview.

  @param   Sender as a TObject

**)
Procedure TframeBADIModuleStatistics.actExpandAllExecute(Sender: TObject);

Begin
  vstStatistics.FullExpand();
End;

(**

  This is an on execute event handler for the Expand action.

  @precon  None.
  @postcon Expands the focused node in the treeview.

  @param   Sender as a TObject

**)
Procedure TframeBADIModuleStatistics.actExpandExecute(Sender: TObject);

Begin
  vstStatistics.FullExpand(vstStatistics.FocusedNode);
End;

(**

  This is an on execute event handler for the ExpandIssues action.

  @precon  None.
  @postcon Expand all root nodes that have issues in them and collapses all others.

  @param   Sender as a TObject

**)
Procedure TframeBADIModuleStatistics.actExpandIssuesExecute(Sender: TObject);

Var
  N: PVirtualNode;

Begin
  N := vstStatistics.GetFirst();
  While Assigned(N) Do
    Begin
      If HasIssues(N) Then
        vstStatistics.FullExpand(N)
      Else
        vstStatistics.FullCollapse(N);
      N := vstStatistics.GetNextSibling(N);
    End;
End;

(**

  This is an on update event handler for the Expand action.

  @precon  None.
  @postcon Enables the expand action of there is a focused node, it has children and is collapsed.

  @param   Sender as a TObject

**)
Procedure TframeBADIModuleStatistics.actExpandUpdate(Sender: TObject);

Var
  A: TAction;

Begin
  If Sender Is TAction Then
    Begin
      A := Sender As TAction;
      A.Enabled :=
        Assigned(vstStatistics.FocusedNode) And
        (vstStatistics.ChildCount[vstStatistics.FocusedNode] > 0) And 
        Not vstStatistics.Expanded[vstStatistics.FocusedNode];
    End;
End;

(**

  This method copies the treeview information to the clipboard.

  @precon  None.
  @postcon The treeview information is copied to the clipboard.

**)
Procedure TframeBADIModuleStatistics.CopyToClipboard;

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
      Node := vstStatistics.GetFirst();
      While Assigned(Node) Do
        Begin
          strLine := '';
          For iColumn := 0 To vstStatistics.Header.Columns.Count - 1 Do
            Begin
              If strLine <> '' Then
                strLine := strLine + #9;
              Case iColumn Of
                0: strLine := strLine + StringOfChar(#32, iMultipler * ParentCount(Node)) +
                    vstStatistics.Text[Node, iColumn];
              Else
                strLine := strLine + vstStatistics.Text[Node, iColumn];
              End;
            End;
          strText := strText + strLine + #13#10;
          Node := vstStatistics.GetNext(Node);
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

  @nometric MissingCONSTInParam

  @param   AOwner as a TComponent

**)
Constructor TframeBADIModuleStatistics.Create(AOwner: TComponent);

Begin
  Inherited Create(AOwner);
  vstStatistics.NodeDataSize := SizeOf(TBADIStatisticsRecord);
  LoadBADIImages(ilScopeImages);
End;

(**

  This method deletes a root node from the treeview if it matches the given module name.

  @precon  Module must be a valid instance.
  @postcon The modules node it deleted if found.

  @param   Module as a TBaseLanguageModule as a constant

**)
Procedure TframeBADIModuleStatistics.DeleteExistingModuleNode(Const Module : TBaseLanguageModule);

Var
  Node: PVirtualNode;

Begin
  Node := vstStatistics.GetFirstChild(vstStatistics.RootNode);
  While Assigned(Node) Do
    Begin
      If vstStatistics.Text[Node, 0] = Module.AsString(True, False) Then
        Begin
          vstStatistics.DeleteNode(Node);
          Break;
        End;
      Node := vstStatistics.GetNextSibling(Node);
    End;
End;

(**

  A destructor for the TframeBADIMOduleStatistics class.

  @precon  None.
  @postcon Does nothing.

**)
Destructor TframeBADIModuleStatistics.Destroy;

Begin
  Inherited Destroy;
End;

(**

  This method searches the given nodes children and updates the parent node with the max values from the
  child nodes.

  @precon  ParentNode must be a valid reference.
  @postcon The parent node is updates with max values from the chlid nodes.

  @param   ParentNode as a PVirtualNode as a constant

**)
Procedure TframeBADIModuleStatistics.ExtractMaxFromChildren(Const ParentNode : PVirtualNode);

Var
  ParentNodeData : PBADIStatisticsRecord;
  NodeData : PBADIStatisticsRecord;
  Node: PVirtualNode;

Begin
  If Not Assigned(ParentNode) Then
    Exit;
  ParentNodeData := vstStatistics.GetNodeData(ParentNode);
  Node := vstStatistics.GetFirstChild(ParentNode);
  While Assigned(Node) Do
    Begin
      NodeData := vstStatistics.GetNodeData(Node);
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
      Node := vstStatistics.GetNextSibling(Node);
    End;
End;

(**

  This method starts the time to focus the frames treeview to prevent AVs in the editor if there is no
  focused control.

  @precon  None.
  @postcon The timer is started to focus the treeview.

**)
Procedure TframeBADIModuleStatistics.FocusResults;

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
Function TframeBADIModuleStatistics.HasIssues(Const Node: PVirtualNode): Boolean;

Var
  iCounter : Integer;
  
Begin
  vstStatistics.IterateSubtree(
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
Procedure TframeBADIModuleStatistics.InitaliseRenderingOptions;

Const
  dblPercentageDivisor = 100.0;

Begin
  FLimits.FLinesOfCode := TBADIOptions.BADIOptions.ModuleMetric[mmLongMethods].FLimit;
  FLimits.FParameterCount := TBADIOptions.BADIOptions.ModuleMetric[mmLongParameterLists].FLimit;
  FLimits.FVariableCount := TBADIOptions.BADIOptions.ModuleMetric[mmLongMethodVariableLists].FLimit;
  FLimits.FNestedIFDepth := TBADIOptions.BADIOptions.ModuleMetric[mmMethodIFDepth].FLimit;
  FLimits.FCyclometricComplexity :=
    TBADIOptions.BADIOptions.ModuleMetric[mmMethodCyclometricComplexity].FLimit;
  FLimits.FToxicity := TBADIOptions.BADIOptions.ModuleMetric[mmMethodToxicity].FLimit;
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
Function TframeBADIModuleStatistics.ProcessFunction(Const NodeData : PBADIStatisticsRecord;
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
Function TframeBADIModuleStatistics.RecurseContainer(Const Container: TElementContainer;
  Const Parent: PVirtualNode): TNodeResultRecord;

Var
  NodeData: PBADIStatisticsRecord;
  iElement: Integer;
  M: TGenericFunction;

Begin
  Result.Create(vstStatistics.AddChild(Parent));
  NodeData := vstStatistics.GetNodeData(Result.FNode);
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
      vstStatistics.DeleteNode(Result.FNode);
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
Procedure TframeBADIModuleStatistics.RenderModule(Const Module: TBaseLanguageModule;
  Const setRenderOptions: TBADIStatsRenderOptions);

Var
  NodeResult: TNodeResultRecord;

Begin
  InitaliseRenderingOptions;
  If Assigned(Module) And Assigned(vstStatistics) Then
    Begin
      FFileName := Module.FileName;
      vstStatistics.BeginUpdate;
      Try
        If sroClear In setRenderOptions Then
          vstStatistics.Clear
        Else
          DeleteExistingModuleNode(Module);
        NodeResult := RecurseContainer(Module, Nil);
        SortAndExpand(NodeResult, setRenderOptions);
        UpdateStats;
      Finally
        vstStatistics.EndUpdate;
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
Procedure TframeBADIModuleStatistics.SortAndExpand(Const NodeResult : TNodeResultRecord;
  Const setRenderOptions : TBADIStatsRenderOptions);

Begin
  If Assigned(NodeResult.FNode) Then
    If sroAutoExpand In setRenderOptions Then
      Begin
        If ((sroAutoExpandOnError In setRenderOptions) And (NodeResult.FIssueCount > 0)) Or
          Not (sroAutoExpandOnError In setRenderOptions) Then
        vstStatistics.FullExpand(NodeResult.FNode);
      End;
End;

(**

  This treeview needs to be focus to stop mouse scrolling message being directed to the editor (which
  does not exists) so this timer waits for the treeview to be visisble and then gives it focus.

  @precon  None.
  @postcon Focuses the treeview when it is visible.

  @param   Sender as a TObject

**)
Procedure TframeBADIModuleStatistics.tmFocusTimerTimer(Sender: TObject);

Begin
  If vstStatistics.CanFocus Then
    Begin
      tmFocusTimer.Enabled := False;
      vstStatistics.SetFocus;
    End;
End;

(**

  This method updates the module count, method count and under, at and over limits counts.

  @precon  None.
  @postcon The counts are updated.

**)
Procedure TframeBADIModuleStatistics.UpdateStats;

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
  Node := vstStatistics.GetFirst();
  While Assigned(Node) Do
    Begin
      NodeData := vstStatistics.GetNodeData(Node);
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
      Node := vstStatistics.GetNext(Node);
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
Procedure TframeBADIModuleStatistics.vstStatisticsBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode;
  CellRect: TRect; Var ContentRect: TRect);

Const
  iModuleColour = clSkyBlue;

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
    dblBlendRatioModule = 0.75;
    dblBlendRatioNonModule = 0.00;

  Var
    dblRatio: Double;
    dblBlendRatio : Double;

  Begin
    Result := TargetCanvas.Brush.Color;
    // Different blendings for modal and non-module nodes.
    If Result = iModuleColour Then
      dblBlendRatio := dblBlendRatioModule
    Else
      dblBlendRatio := dblBlendRatioNonModule;
    If dblValue > 0 Then
      Begin
        dblRatio := dblValue / dblLimit;
        If dblRatio < FLowThreshold Then
          Result := BlendColour(iLightGreen, Result, dblBlendRatio)
        Else If dblRatio > FHighThreshold Then
          Result := BlendColour(iLightRed, Result, dblBlendRatio)
        Else
          Result := BlendColour(iLightAmber, Result, dblBlendRatio);
      End;
  End;

Var
  NodeData: PBADIStatisticsRecord;

Begin
  NodeData := vstStatistics.GetNodeData(Node);
  TargetCanvas.Brush.Color := clWindow;
  If NodeData.FNodeType = ntModule Then
    TargetCanvas.Brush.Color := iModuleColour;
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
Procedure TframeBADIModuleStatistics.vstStatisticsCompareNodes(Sender: TBaseVirtualTree; Node1,
  Node2: PVirtualNode; Column: TColumnIndex; Var Result: Integer);

Begin
  Result := CompareText(vstStatistics.Text[Node1, 0], vstStatistics.Text[Node2, 0]);
End;

(**

  This is an on FreeNdoe event handler for the treeview.

  @precon  None.
  @postcon Ensures that the managed types in the node are freed.

  @param   Sender as a TBaseVirtualTree
  @param   Node   as a PVirtualNode

**)
Procedure TframeBADIModuleStatistics.vstStatisticsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);

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
Procedure TframeBADIModuleStatistics.vstStatisticsGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; Var Ghosted: Boolean;
  Var ImageIndex: Integer);

Var
  NodeData: PBADIStatisticsRecord;

Begin
  If Column = 0 Then
    Begin
      NodeData := vstStatistics.GetNodeData(Node);
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
Procedure TframeBADIModuleStatistics.vstStatisticsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; Var CellText: String);

Var
  NodeData: PBADIStatisticsRecord;

Begin
  NodeData := vstStatistics.GetNodeData(Node);
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

  This is an on Paint Text event handler for the treeview.

  @precon  None.
  @postcon If the node has no parent (i.e. its a root node) then make it bold and blue.

  @param   Sender       as a TBaseVirtualTree
  @param   TargetCanvas as a TCanvas as a constant
  @param   Node         as a PVirtualNode
  @param   Column       as a TColumnIndex
  @param   TextType     as a TVSTTextType

**)
Procedure TframeBADIModuleStatistics.vstStatisticsPaintText(Sender: TBaseVirtualTree;
  Const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);

Var
  NodeData : PBADIStatisticsRecord;
  
Begin
  TargetCanvas.Font.Style := [];
  TargetCanvas.Font.Color := clWindowText;
  NodeData := vstStatistics.GetNodeData(Node);
  If Not (NodeData.FNodeType In [ntMethod]) Then
    Begin
      If Column = 0 Then
        TargetCanvas.Font.Style := [fsBold];
      If NodeData.FNodeType = ntModule Then
        TargetCanvas.Font.Color := clNavy;
    End;
End;

End.
