(**
  
  This module contains a frame for dislpaying module methods and their metrics.

  @Author  David Hoyle
  @Version 1.0
  @Date    03 Dec 2017
  
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
  Dialogs, VirtualTrees, 
  BADI.Base.Module,
  BADI.ElementContainer,
  ImageList,
  ImgList, Vcl.ExtCtrls;

Type
  (** A record type to hold the return information from recursing nodes. **)
  TNodeResultRecord = Record
    FNode       : PVirtualNode;
    FChildCount : Integer;
  End;

  (** An enumerate to define the type of information stored in each node - used for counting later. **)
  TBADINodeType = (ntUnkown, ntModule, ntMethod);

  (** A frame to display a modules methods and their metrics. **)
  TframeBADIModuleStatistics = Class(TFrame)
    vstStatistics: TVirtualStringTree;
    ilScopeImages: TImageList;
    tmFocusTimer: TTimer;
    Procedure vstStatisticsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; Var CellText: String);
    Procedure vstStatisticsBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      Var ContentRect: TRect);
    Procedure vstStatisticsGetImageIndex(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Kind: TVTImageKind; Column: TColumnIndex; Var Ghosted: Boolean; Var ImageIndex: Integer);
    procedure tmFocusTimerTimer(Sender: TObject);
    procedure vstStatisticsCompareNodes(Sender: TBaseVirtualTree; Node1, Node2: PVirtualNode;
      Column: TColumnIndex; var Result: Integer);
    procedure vstStatisticsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  Strict Private
    Type
      (** A record to describe the data to be held in a tree node. **)
      TBADIStatisticsRecord = Record
        FNodeType               : TBADINodeType;
        FText                   : String;
        FImageIndex             : Integer;
        FMethodLength           : Double;
        FParameterCount         : Double;
        FVariableCount          : Double;
        FNestedIFDepth          : Double;
        FCyclometricComplexity  : Double;
        FToxicity               : Double;
      End;
      (** A pointer to the above record. **)
      PBADIStatisticsRecord = ^TBADIStatisticsRecord;
  Strict Private
    FLimits      : TBADIStatisticsRecord;
    FFileName    : String;
    FModuleCount : Integer;
    FMethodCount : Integer;
    FUnderLimit  : Integer;
    FAtLimit     : Integer;
    FOverLimit   : Integer;
  Strict Protected
    Function RecurseContainer(Const Container: TElementContainer;
      Const Parent: PVirtualNode) : TNodeResultRecord;
    Procedure UpdateStats;
  Public
    //: @nometric MissingCONSTInParam
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
    Procedure RenderModule(Const Module : TBaseLanguageModule);
    Procedure CopyToClipboard;
  Published
    (**
      This property returns the number of modules.
      @precon  None.
      @postcon Returns the number of modules.
      @return  an Integer
    **)
    Property ModuleCount : Integer Read FModuleCount;
    (**
      This property returns the number of methods.
      @precon  None.
      @postcon Returns the number of methods.
      @return  an Integer
    **)
    Property MethodCount : Integer Read FMethodCount;
    (**
      This property returns the number of method metrics which are under the limit.
      @precon  None.
      @postcon Returns the number of method metrics which are under the limit.
      @return  an Integer
    **)
    Property UnderLimit : Integer Read FUnderLimit;
    (**
      This property returns the number of method metrics which are at the limit.
      @precon  None.
      @postcon Returns the number of method metrics which are at the limit.
      @return  an Integer
    **)
    Property AtLimit : Integer Read FAtLimit;
    (**
      This property returns the number of method metrics which are over the limit.
      @precon  None.
      @postcon Returns the number of method metrics which are over the limit.
      @return  an Integer
    **)
    Property OverLimit : Integer Read FOverLimit;
  End;

Const
  (** A constant to define a light green colour for the columns if an item is under the limit. **)
  iLightGreen = $80FF80;
  (** A constant to define a light amber colour for the columns if an item is at the limit. **)
  iLightRed = $8080FF;
  (** A constant to define a light red colour for the columns if an item is over the limit. **)
  iLightAmber = $80CCFF;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  BADI.Generic.FunctionDecl, 
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

Const
  (** A low threshold for the metrics. **)
  dblLowThreshold = 0.95;
  (** A High threshold for the metrics. **)
  dblHighThreshold = 1.05;

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
  Function ParentCount(Const Node : PVirtualNode) : Integer;

  Var
    P: PVirtualNode;

  Begin
    Result := 0;
    P := Node.Parent;
    While  Assigned(P) Do
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
  CB : TClipBoard;
  strText : String;
  strLine : String;
  Node: PVirtualNode;
  iColumn: Integer;
  
Begin
  CB := TClipboard.Create;
  Try
    CB.Open;
    Try
      strText := Format(strClipboardHeader, [FFileName]);
      Node := vstStatistics.GetFirst();
      While Assigned(Node) Do
        Begin
          strLine := '';
          For iColumn := 0 To vstStatistics.Header.Columns.Count - 1  Do
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

  A destructor for the TframeBADIMOduleStatistics class.

  @precon  None.
  @postcon Does nothing.

**)
Destructor TframeBADIModuleStatistics.Destroy;

Begin
  Inherited Destroy;
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
  Const Parent: PVirtualNode) : TNodeResultRecord;

Var
  NodeData : PBADIStatisticsRecord;
  iElement: Integer;
  M: TGenericFunction;
  E : TElementContainer;
  
Begin
  Result.FChildCount := 0;
  Result.FNode := vstStatistics.AddChild(Parent);
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
        Begin
          NodeData.FNodeType := ntMethod;
          NodeData.FMethodLength := M.LineofCode;
          NodeData.FParameterCount := M.ParameterCount;
          E := M.FindElement(strVarsLabel);
          If Assigned(E) Then
            NodeData.FVariableCount := E.ElementCount;
          NodeData.FNestedIFDepth := M.NestedIFDepth;
          NodeData.FCyclometricComplexity := M.CyclometricComplexity;
          NodeData.FToxicity := M.Toxicity;
          Inc(Result.FChildCount);
        End;
    End;
  For iElement := 1 To Container.ElementCount Do
    Inc(Result.FChildCount, RecurseContainer(Container.Elements[iElement], Result.FNode).FChildCount);
  If Result.FChildCount = 0 Then
    Begin
      vstStatistics.DeleteNode(Result.FNode);
      Result.FNode := Nil;
    End;
End;

(**

  This method starts the process of rendering the module contents.

  @precon  Module must be valid.
  @postcon The modules methods and their metrics are rendered.

  @param   Module as a TBaseLanguageModule as a constant

**)
Procedure TframeBADIModuleStatistics.RenderModule(Const Module: TBaseLanguageModule);

Var
  Node: PVirtualNode;
  NodeResult: TNodeResultRecord;

Begin
  FLimits.FMethodLength := TBADIOptions.BADIOptions.ModuleMetric[mmLongMethods].FLimit;
  FLimits.FParameterCount := TBADIOptions.BADIOptions.ModuleMetric[mmLongParameterLists].FLimit;
  FLimits.FVariableCount := TBADIOptions.BADIOptions.ModuleMetric[mmLongMethodVariableLists].FLimit;
  FLimits.FNestedIFDepth := TBADIOptions.BADIOptions.ModuleMetric[mmMethodIFDepth].FLimit;
  FLimits.FCyclometricComplexity :=
    TBADIOptions.BADIOptions.ModuleMetric[mmMethodCyclometricComplexity].FLimit;
  FLimits.FToxicity := TBADIOptions.BADIOptions.ModuleMetric[mmMethodToxicity].FLimit;
  If Assigned(Module) Then
    Begin
      FFileName := Module.FileName;
      vstStatistics.BeginUpdate;
      Try
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
        NodeResult := RecurseContainer(Module, Nil);
        If Assigned(NodeResult.FNode) Then
          Begin
            vstStatistics.Sort(NodeResult.FNode, 0, sdAscending);
            vstStatistics.FullExpand(NodeResult.FNode);
          End;
      Finally
        vstStatistics.EndUpdate;
      End;
    End;
  UpdateStats;
  tmFocusTimer.Enabled := True;
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
  If Assigned(Parent) And Visible And Assigned(vstStatistics.Parent) And vstStatistics.Visible Then
    Begin
      vstStatistics.SetFocus;
      tmFocusTimer.Enabled := False;
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
  Procedure Update(Const dblValue, dblLimit : Double);

  Var
    dblRatio : Double;
    
  Begin
    dblRatio := dblValue /  dblLimit;
    If dblRatio > 0 Then
      If dblRatio < dblLowThreshold Then
        Inc(FUnderLimit)
      Else If dblRatio > dblHighThreshold Then
        Inc(FOverLimit)
      Else 
        Inc(FAtLimit)
  End;

Var
  Node : PVirtualNode;
  NodeData : PBADIStatisticsRecord;

Begin
  FModuleCount := 0;
  FMethodCount := 0;
  FUnderLimit := 0;
  FAtLimit := 0;
  FOverLimit := 0;
  Node := vstStatistics.GetFirst();
  While Assigned(Node) Do
    Begin
      NodeData := vstStatistics.GetNodeData(Node);
      Case NodeData.FNodeType Of
        ntModule: Inc(FModuleCount);  
        ntMethod: Inc(FMethodCount);  
      End;
      Update(NodeData.FMethodLength, FLimits.FMethodLength);
      Update(NodeData.FParameterCount, FLimits.FParameterCount);
      Update(NodeData.FVariableCount, FLimits.FVariableCount);
      Update(NodeData.FNestedIFDepth, FLimits.FNestedIFDepth);
      Update(NodeData.FCyclometricComplexity, FLimits.FCyclometricComplexity);
      Update(NodeData.FToxicity, FLimits.FToxicity);
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

  (**

    This function returns the colours to be used based on the value and the limit.

    @precon  dblLimit cannot be zero.
    @postcon Returns the colours to be used based on the value and the limit.

    @param   dblValue as a Double as a constant
    @param   dblLimit as a Double as a constant
    @return  a TColor

  **)
  Function Colour(Const dblValue, dblLimit : Double) : TColor;

  Var
    dblRatio: Double;

  Begin
    Result := TargetCanvas.Brush.Color;
    If dblValue > 0 Then
      Begin
        dblRatio := dblValue / dblLimit;
        If dblRatio < dblLowThreshold Then
          Result := iLightGreen
        Else If dblRatio > dblHighThreshold Then
          Result := iLightRed
        Else
          Result := iLightAmber;
      End;
  End;

Var
  NodeData : PBADIStatisticsRecord;
  
Begin
  NodeData := vstStatistics.GetNodeData(Node);
  TargetCanvas.Brush.Color := clWindow;
  If Not Assigned(Sender.NodeParent[Node]) Then
    TargetCanvas.Brush.Color := clSkyBlue;
  Case TBADIMetricColumns(Column) Of
    mcLength: TargetCanvas.Brush.Color := Colour(NodeData.FMethodLength, FLimits.FMethodLength);  
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
  NodeData : PBADIStatisticsRecord;
  
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
  NodeData : PBADIStatisticsRecord;
  
Begin
  NodeData := vstStatistics.GetNodeData(Node);
  CellText := '';
  Case TBADIMetricColumns(Column) Of
    mcText: CellText := NodeData.FText;  
    mcLength:
      If NodeData.FMethodLength > 0 Then
        CellText := Format('%1.0n', [Int(NodeData.FMethodLength)]);  
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

Begin
  TargetCanvas.Font.Style := [];
  TargetCanvas.Font.Color := clWindowText;
  If Not Assigned(Sender.NodeParent[Node]) Then
    Begin
      TargetCanvas.Font.Style := [fsBold];
      TargetCanvas.Font.Color := clBlue;
    End;
End;

End.
