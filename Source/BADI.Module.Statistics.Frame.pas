(**
  
  This module contains a frame for dislpaying module methods and their metrics.

  @Author  David Hoyle
  @Version 1.0
  @Date    25 Nov 2017
  
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
  Strict Private
    Type
      (** A record to describe the data to be held in a tree node. **)
      TBADIStatisticsRecord = Record
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
    FLimits : TBADIStatisticsRecord;
  Strict Protected
    Function RecurseContainer(Const Container: TElementContainer; Const Parent: PVirtualNode) : Integer;
  Public
    //: @nometric MissingCONSTInParam
    Constructor Create(AOwner : TComponent); Override;
    Procedure RenderModule(Const Module : TBaseLanguageModule);
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  BADI.Generic.FunctionDecl, 
  BADI.ResourceStrings, 
  BADI.Options, 
  BADI.Types, 
  BADI.Functions;

{$R *.dfm}

Type
  (** An enumerate to define the columns in the report. **)
  TBADIMetricColumns = (mcText, mcLength, mcParameters, mcVariables, mcNestIFDepth,
    mcCyclometricComplexity, mcToxicity);

  
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

  This method recursively walks the given container rendering its contents in the tree view. If the
  container is a generic function, metrics are extracted from the method and dislpayed. Any branches with
  out methods are pruned.

  @precon  Container must be a valid instance.
  @postcon The metrics of the module method are displayed.

  @param   Container as a TElementContainer as a constant
  @param   Parent    as a PVirtualNode as a constant
  @return  an Integer

**)
Function TframeBADIModuleStatistics.RecurseContainer(Const Container: TElementContainer;
  Const Parent: PVirtualNode) : Integer;

Var
  Node : PVirtualNode;
  NodeData : PBADIStatisticsRecord;
  iElement: Integer;
  M: TGenericFunction;
  E : TElementContainer;
  
Begin
  Result := 0;
  Node := vstStatistics.AddChild(Parent);
  NodeData := vstStatistics.GetNodeData(Node);
  NodeData.FText := Container.AsString(True, False);
  NodeData.FImageIndex := BADIImageIndex(Container.ImageIndex, Container.Scope);
  If Container Is TGenericFunction Then
    Begin
      M := Container As TGenericFunction;
      If Not M.IsDeclarationOnly Then
        Begin
          NodeData.FMethodLength := M.LineofCode;
          NodeData.FParameterCount := M.ParameterCount;
          E := M.FindElement(strVarsLabel);
          If Assigned(E) Then
            NodeData.FVariableCount := E.ElementCount;
          NodeData.FNestedIFDepth := M.NestedIFDepth;
          NodeData.FCyclometricComplexity := M.CyclometricComplexity;
          NodeData.FToxicity := M.Toxicity;
          Inc(Result);
        End;
    End;
  For iElement := 1 To Container.ElementCount Do
    Inc(Result, RecurseContainer(Container.Elements[iElement], Node));
  If Result = 0 Then
    vstStatistics.DeleteNode(Node);
End;

(**

  This method starts the process of rendering the module contents.

  @precon  Module must be valid.
  @postcon The modules methods and their metrics are rendered.

  @param   Module as a TBaseLanguageModule as a constant

**)
Procedure TframeBADIModuleStatistics.RenderModule(Const Module: TBaseLanguageModule);

Begin
  FLimits.FMethodLength := TBADIOptions.BADIOptions.ModuleMetric[mmLongMethods].FLimit;
  FLimits.FParameterCount := TBADIOptions.BADIOptions.ModuleMetric[mmLongParameterLists].FLimit;
  FLimits.FVariableCount := TBADIOptions.BADIOptions.ModuleMetric[mmLongMethodVariableLists].FLimit;
  FLimits.FNestedIFDepth := TBADIOptions.BADIOptions.ModuleMetric[mmMethodIFDepth].FLimit;
  FLimits.FCyclometricComplexity :=
    TBADIOptions.BADIOptions.ModuleMetric[mmMethodCyclometricComplexity].FLimit;
  FLimits.FToxicity := TBADIOptions.BADIOptions.ModuleMetric[mmMethodToxicity].FLimit;
  vstStatistics.BeginUpdate;
  Try
    vstStatistics.Clear;
    RecurseContainer(Module, Nil);
    vstStatistics.FullExpand();
  Finally
    vstStatistics.EndUpdate;
  End;
  // Focus first node.
//  vstStatistics.FocusedNode := vstStatistics.GetFirst();
//  If Assigned(vstStatistics.FocusedNode) Then
//    vstStatistics.Selected[vstStatistics.FocusedNode] := True;
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
  If Visible And vstStatistics.Visible Then
    Begin
      vstStatistics.SetFocus;
      tmFocusTimer.Enabled := False;
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

  Const
    iLightGreen = $80FF80;
    iLightRed = $8080FF;
    iLightAmber = $80CCFF;
    dblLowThreshold = 0.95;
    dblHighThreshold = 1.05;

  Var
    dblRatio: Double;

  Begin
    Result := clWindow;
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

End.
