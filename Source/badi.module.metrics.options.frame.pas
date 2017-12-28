(**

  This module contains a class which represents a frame fo the IDE options dialogue to allow the user
  to select the metrics adn checks they wish to dispay.

  @Author  David Hoyle
  @Version 1.0
  @date    28 Dec 2017

**)
Unit BADI.Module.Metrics.Options.Frame;

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
  BADI.CustomOptionsFrame,
  VirtualTrees, 
  BADI.CustomVirtualStringTree;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A custom treeview for this frame to stop AVs due to RTTI clashes with TVirtualStrinTree. **)
  TBADIMetricsOptionsVirtualStringTree = Class(TBADICustomVirtualStringTree);
 
  (** A class to represent a frame for selecting the checks and metrics in the IDE options dialogue. **)
  TframeBADIModuleMetricsOptions = Class(TFrame, IBADIOptionsFrame)
    Procedure vstMetricsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; Var CellText: String);
    Procedure vstMetricsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      Var Allowed: Boolean);
    Procedure vstMetricsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      NewText: String);
    Procedure vstMetricsHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    Procedure vstMetricsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    Procedure vstMetricsPaintText(Sender: TBaseVirtualTree; Const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  Strict Private
    FVSTMetrics : TBADIMetricsOptionsVirtualStringTree;
  Strict Protected
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure RecurseNodes(Const vstTreeView: TBaseVirtualTree; Const Node: PVirtualNode;
      Const State: TCheckState);
    Procedure CreateVirtualStringTree;
  Public
    //: @nometric MissingCONSTInParam
    Constructor Create(AOwner : TComponent); Override;
  End;

Implementation

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  BADI.Types,
  BADI.Constants,
  BADI.Options, 
  BADI.Functions,
  Themes;

{$R *.dfm}

Type
  (** A record to describe the information stored in the virtual treview. **)
  TMetricNodeData = Record
    FModuleMetric    : TBADIModuleMetric;
    FMetricLimitType : TBADILimitType;
    FMetricLimit     : Double;
  End;
  (** A pointer to tbe above structure. **)
  PMetricNodeData = ^TMetricNodeData;

Const
  (** The column reference for the metric description. **)
  iMetricDescription = 0;
  (** The column reference for the metric name. **)
  iMetricName = 1;
  (** The column reference for the metric limit. **)
  iMetricLimit = 2;

(**

  A constructor for the TframeBADIMetrics class.

  @precon  AOwner must be a valid instance.
  @postcon Initialises the metrics frame for the IDE options dialogue.

  @nocheck MissingCONSTInParam

  @param   AOwner as a TComponent

**)
Constructor TframeBADIModuleMetricsOptions.Create(AOwner: TComponent);

  (**

    This method searches the tree view for a node with the given parent and if found returns the node 
    reference for that node else returns nil.

    @precon  None.
    @postcon Returns the node with the given parent else returns nil if not found.

    @param   eMetric as a TBADIModuleMetric as a constant
    @return  a PVirtualNode

  **)
  Function FindParent(Const eMetric : TBADIModuleMetric) : PVirtualNode; Overload;

  Var
    NodeData : PMetricNodeData;
    N: PVirtualNode;

  Begin
    Result := Nil;
    N := FVSTMetrics.GetFirst;
    While Assigned(N) Do
      Begin
        NodeData := FVSTMetrics.GetNodeData(N);
        If NodeData.FModuleMetric = eMetric Then
          Begin
            Result := N;
            Break;
          End;
        N := FVSTMetrics.GetNext(N);
      End;
  End;

Var
  eMetric: TBADIModuleMetric;
  N, P : PVirtualNode;
  NodeData : PMetricNodeData;

Begin
  Inherited Create(AOwner);
  CreateVirtualStringTree;
  FVSTMetrics.NodeDataSize := SizeOf(TMetricNodeData);
  For eMetric := Low(TBADIModuleMetric) To High(TBADIModuleMetric) Do
    Begin
      P := FindParent(ModuleMetrics[eMetric].FParent);
      N := FVSTMetrics.AddChild(P);
      FVSTMetrics.CheckType[N] := ctCheckBox;
      FVSTMetrics.CheckState[N] := csUncheckedNormal;
      NodeData := FVSTMetrics.GetNodeData(N);
      NodeData.FModuleMetric := eMetric;
      NodeData.FMetricLimitType := ltNone;
      NodeData.FMetricLimit := 0;
    End;
  FVSTMetrics.FullExpand;
End;

(**

  This method creates and initialises the treeview..

  @precon  None.
  @postcon The treeview is created and initialised.

  @nocheck HardCodedString HardCodedInteger

**)
Procedure TframeBADIModuleMetricsOptions.CreateVirtualStringTree;

Var
  C: TVirtualTreeColumn;

Begin
  FVSTMetrics := TBADIMetricsOptionsVirtualStringTree.Create(Self);
  FVSTMetrics.Name := 'vstMetrics';
  FVSTMetrics.Parent := Self;
  FVSTMetrics.AlignWithMargins := True;
  FVSTMetrics.Align := alClient;
  FVSTMetrics.EditDelay := 250;
  FVSTMetrics.TabOrder := 0;
  FVSTMetrics.OnChecked := vstMetricsChecked;
  FVSTMetrics.OnEditing := vstMetricsEditing;
  FVSTMetrics.OnGetText := vstMetricsGetText;
  FVSTMetrics.OnPaintText := vstMetricsPaintText;
  FVSTMetrics.OnHeaderClick := vstMetricsHeaderClick;
  FVSTMetrics.OnNewText := vstMetricsNewText;
  FVSTMetrics.Header.AutoSizeIndex := 0;
  FVSTMetrics.Header.Height := 20;
  FVSTMetrics.Header.Options := FVSTMetrics.Header.Options + [hoColumnResize, hoShowImages];
  FVSTMetrics.TreeOptions.AutoOptions := [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking,
    toAutoDeleteMovedNodes, toAutoChangeScale];
  FVSTMetrics.TreeOptions.MiscOptions := FVSTMetrics.TreeOptions.MiscOptions +
    [toCheckSupport, toEditable, toEditOnClick, toEditOnDblClick];
  C := FVSTMetrics.Header.Columns.Add;
  C.CheckBox := True;
  C.Position := 0;
  C.Width := 184;
  C.Text := 'Module Metrics and Checks';
  C := FVSTMetrics.Header.Columns.Add;
  C.Position := 1;
  C.Width := 150;
  C.Text := 'Name';
  C := FVSTMetrics.Header.Columns.Add;
  C.Alignment := taRightJustify;
  C.Position := 2;
  C.Width := 75;
  C.Text := 'Limit';
End;

(**

  This method loads the settings from the global BADIOptions into the treeview.

  @precon  None.
  @postcon The treeview is updated with the current metrics and checks settings.

**)
Procedure TframeBADIModuleMetricsOptions.LoadSettings;

Var
  N : PVirtualNode;
  NodeData : PMetricNodeData;
  BO: TBADIOptions;

Begin
  N := FVSTMetrics.GetFirst;
  BO := TBADIOptions.BADIOptions;
  While Assigned(N) Do
    Begin
      NodeData := FVSTMetrics.GetNodeData(N);
      If BO.ModuleMetric[NodeData.FModuleMetric].FEnabled Then
        FVSTMetrics.CheckState[N] := csCheckedNormal
      Else
        FVSTMetrics.CheckState[N] := csUncheckedNormal;
      NodeData.FMetricLimitType := ModuleMetrics[NodeData.FModuleMetric].FLimitType;
      NodeData.FMetricLimit := BO.ModuleMetric[NodeData.FModuleMetric].FLimit;
      N := FVSTMetrics.GetNext(N);
    End;
  vstMetricsChecked(Nil, Nil);
End;

(**

  This method recurses the nodes in the treeview and updates the checked state to that given.

  @precon  vstTreeView and Node must be valid instances.
  @postcon The checkded state of the treeview is updated.

  @param   vstTreeView as a TBaseVirtualTree as a constant
  @param   Node        as a PVirtualNode as a constant
  @param   State       as a TCheckState as a constant

**)
Procedure TframeBADIModuleMetricsOptions.RecurseNodes(Const vstTreeView: TBaseVirtualTree;
  Const Node: PVirtualNode; Const State: TCheckState);

Var
  N: PVirtualNode;

Begin
  N := Node;
  While N <> Nil Do
    Begin
      If State = csMixedNormal Then
        Begin
          If vstTreeView.CheckState[N] = csCheckedNormal Then
            vstTreeView.CheckState[N]  := csUncheckedNormal
          Else
            vstTreeView.CheckState[N] := csCheckedNormal;
        End
      Else
        vstTreeView.CheckState[N] := State;
      RecurseNodes(vstTreeView, N.FirstChild, State);
      N := N.NextSibling;
    End;
End;

(**

  This method saves the settings to the global BADIOptions from the treeview.

  @precon  None.
  @postcon The treeview metrics and checks settings are saved to the options.

**)
Procedure TframeBADIModuleMetricsOptions.SaveSettings;

Var
  N : PVirtualNode;
  NodeData : PMetricNodeData;
  R : TBADIMetricRecord;
  BO: TBADIOptions;

Begin
  N := FVSTMetrics.GetFirst();
  BO := TBADIOptions.BADIOptions;
  While Assigned(N) Do
    Begin
      NodeData := FVSTMetrics.GetNodeData(N);
      R := BO.ModuleMetric[NodeData.FModuleMetric];
      Case FVSTMetrics.CheckState[N] Of
        csUncheckedNormal: R.FEnabled := False;
        csCheckedNormal:   R.FEnabled := True;
      End;
      R.FLimit := NodeData.FMetricLimit;
      BO.ModuleMetric[NodeData.FModuleMetric] := R;
      N := FVSTMetrics.GetNext(N);
    End;
End;

(**

  This method updates the status of the header based on the number of nodee checked.

  @precon  None.
  @postcon Updates the status of the header based on the number of nodee checked.

  @param   Sender as a TBaseVirtualTree
  @param   Node   as a PVirtualNode

**)
procedure TframeBADIModuleMetricsOptions.vstMetricsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);

Var
  iTotal   : Integer;
  iCount   : Integer;
  N        : PVirtualNode;

Begin
  iCount := 0;
  iTotal := 0;
  N := FVSTMetrics.GetFirst;
  While N <> Nil Do
    Begin
      Inc(iTotal);
      Case Sender.CheckState[N] Of
        csCheckedNormal: Inc(iCount);
      End;
      N := FVSTMetrics.GetNext(N);
    End;
  If iCount = 0 Then
    FVSTMetrics.Header.Columns[0].CheckState := csUncheckedNormal
  Else If iCount = iTotal Then
    FVSTMetrics.Header.Columns[0].CheckState := csCheckedNormal
  Else
    FVSTMetrics.Header.Columns[0].CheckState := csMixedNormal;
end;

(**

  This method determines if the column / value can be edited.

  @precon  None.
  @postcon Return true in allows if the column and value can be edited.

  @param   Sender  as a TBaseVirtualTree
  @param   Node    as a PVirtualNode
  @param   Column  as a TColumnIndex
  @param   Allowed as a Boolean as a reference

**)
Procedure TframeBADIModuleMetricsOptions.vstMetricsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; Var Allowed: Boolean);

Var
  NodeData : PMetricNodeData;

Begin
  NodeData := Sender.GetNodeData(Node);
  Allowed := (Column = iMetricLimit) And (NodeData.FMetricLimitType <> ltNone);
End;

(**

  This method returns the description of the specified column.

  @precon  None.
  @postcon The desceription for the specified column is returned.

  @param   Sender   as a TBaseVirtualTree
  @param   Node     as a PVirtualNode
  @param   Column   as a TColumnIndex
  @param   TextType as a TVSTTextType
  @param   CellText as a String as a reference

**)
Procedure TframeBADIModuleMetricsOptions.vstMetricsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; Var
  CellText: String);

Const
  strIntegerFmt = '%1.0f';
  strFloatFmt = '%1.3f';

Var
  NodeData : PMetricNodeData;

Begin
  NodeData := Sender.GetNodeData(Node);
  Case Column Of
    iMetricDescription: CellText := ModuleMetrics[NodeData.FModuleMetric].FDescription;
    iMetricName:
      If (NodeData.FModuleMetric = ModuleMetrics[NodeData.FModuleMetric].FParent) Then
        CellText := ModuleMetrics[NodeData.FModuleMetric].FName
      Else
        CellText := '';
    iMetricLimit:
      Case ModuleMetrics[NodeData.FModuleMetric].FLimitType Of
        ltInteger: CellText := Format(strIntegerFmt, [NodeData.FMetricLimit]);
        ltFloat: CellText := Format(strFloatFmt, [NodeData.FMetricLimit]);
        ltNone: CellText := '';
      End;
  End;
End;

(**

  This method updates the checked status of the treeview based on the check status of the header.

  @precon  None.
  @postcon Updates the checked status of the treeview based on the check status of the header.

  @param   Sender  as a TVTHeader
  @param   HitInfo as a TVTHeaderHitInfo

**)
Procedure TframeBADIModuleMetricsOptions.vstMetricsHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);

Var
  ST: TBaseVirtualTree;

Begin
  If hhiOnCheckbox In HitInfo.HitPosition Then
    If Sender.Columns[0].CheckState In [csCheckedNormal, csMixedNormal] Then
      Begin
        ST := Sender.Treeview;
        RecurseNodes(ST, ST.RootNode.FirstChild, csCheckedNormal);
        ST.Invalidate;
      End Else
      Begin
        ST := Sender.Treeview;
        RecurseNodes(ST, ST.RootNode.FirstChild, csUncheckedNormal);
        ST.Invalidate;
      End;
End;

(**

  This method sets the new limit for a check / metric after validating the value provided.

  @precon  Noen.
  @postcon Sets the new limit for a check / metric after validating the value provided.

  @param   Sender  as a TBaseVirtualTree
  @param   Node    as a PVirtualNode
  @param   Column  as a TColumnIndex
  @param   NewText as a String

**)
Procedure TframeBADIModuleMetricsOptions.vstMetricsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; NewText: String);

ResourceString
  strNotAValidInteger = '%s is not a valid integer!';
  strNotAValidFloat = '%s is not a valid floting point number!';
  strIntegerMustBeGreaterThanZero = 'The limit value (%s) must be greater than zero!';
  strFloatMustBeGreaterThanZero = 'The limit value (%s) must be greater than zero!';

Var
  NodeData: PMetricNodeData;
  iInteger : Integer;
  dblDouble : Double;
  iErrorCode: Integer;

Begin
  NodeData := Sender.GetNodeData(Node);
  If Column = iMetricLimit Then
    Case NodeData.FMetricLimitType Of
      ltInteger:
        Begin
          Val(NewText, iInteger, iErrorCode);
          If iErrorCode > 0 Then
            MessageDlg(Format(strNotAValidInteger, [NewText]), mtError, [mbOK], 0)
          Else If iInteger <= 0 Then
            MessageDlg(Format(strIntegerMustBeGreaterThanZero, [NewText]), mtError, [mbOK], 0)
          Else
            NodeData.FMetricLimit := iInteger
        End;
      ltFloat:
        Begin
          Val(NewText, dblDouble, iErrorCode);
          If iErrorCode > 0 Then
            MessageDlg(Format(strNotAValidFloat, [NewText]), mtError, [mbOK], 0)
          Else If dblDouble <= 0.0 Then
            MessageDlg(Format(strFloatMustBeGreaterThanZero, [NewText]), mtError, [mbOK], 0)
          Else
            NodeData.FMetricLimit := dblDouble
        End;
    End;
End;

(**

  This is an on paint text event handler for the treeview.

  @precon  None.
  @postcon Changes the focused column/row text colour only.

  @param   Sender       as a TBaseVirtualTree
  @param   TargetCanvas as a TCanvas as a constant
  @param   Node         as a PVirtualNode
  @param   Column       as a TColumnIndex
  @param   TextType     as a TVSTTextType

**)
Procedure TframeBADIModuleMetricsOptions.vstMetricsPaintText(Sender: TBaseVirtualTree;
  Const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);

Begin
  TargetCanvas.Font.Color := clWindowText;
  {$IFDEF DXE102}
  If Assigned(FVSTMetrics.StyleServices) And FVSTMetrics.StyleServices.Enabled Then
    TargetCanvas.Font.Color := FVSTMetrics.StyleServices.GetSystemColor(clWindowText);
  {$ENDIF}
End;

End.
