(**

  This module contains a class which represents a frame fo the IDE options dialogue to allow the user
  to select the metrics adn checks they wish to dispay.

  @Author  David Hoyle
  @Version 1.0
  @date    15 Oct 2017

**)
Unit BADI.ModuleMetricsFrame;

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
  VirtualTrees;

Type
  (** A class to represent a frame for selecting the checks and metrics in the IDE options dialogue. **)
  TframeBADIModuleMetrics = Class(TFrame, IBADIOptionsFrame)
    vstMetrics: TVirtualStringTree;
    Procedure vstMetricsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; Var CellText: String);
    Procedure vstMetricsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      Var Allowed: Boolean);
    Procedure vstMetricsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      NewText: String);
    Procedure vstMetricsHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    procedure vstMetricsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
  Strict Private
  Strict Protected
    Procedure LoadSettings;
    Procedure SaveSettings;
    Procedure RecurseNodes(Const vstTreeView: TBaseVirtualTree; Const Node: PVirtualNode;
      Const State: TCheckState);
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
  BADI.Options;

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
  iMetricLimt = 2;

(**

  A constructor for the TframeBADIMetrics class.

  @precon  AOwner must be a valid instance.
  @postcon Initialises the metrics frame for the IDE options dialogue.

  @nometric MissingCONSTInParam

  @param   AOwner as a TComponent

**)
Constructor TframeBADIModuleMetrics.Create(AOwner: TComponent);

Var
  eMetric: TBADIModuleMetric;
  N, P : PVirtualNode;
  NodeData : PMetricNodeData;

Begin
  Inherited Create(AOwner);
  vstMetrics.NodeDataSize := SizeOf(TMetricNodeData);
  P := Nil;
  For eMetric := Low(TBADIModuleMetric) To High(TBADIModuleMetric) Do
    Begin
      If Not DefaultModuleMetrics[eMetric].FSubItem Then
        Begin
          N := vstMetrics.AddChild(Nil);
          P := N;
        End Else
          N := vstMetrics.AddChild(P);
      vstMetrics.CheckType[N] := ctCheckBox;
      vstMetrics.CheckState[N] := csUncheckedNormal;
      NodeData := vstMetrics.GetNodeData(N);
      NodeData.FModuleMetric := eMetric;
      NodeData.FMetricLimitType := ltNone;
      NodeData.FMetricLimit := 0;
    End;
  N := vstMetrics.GetFirstChild(vstMetrics.RootNode);
  While Assigned(N) Do
    Begin
      vstMetrics.Expanded[N] := True;
      N := vstMetrics.GetNextSibling(N);
    End;
End;

(**

  This method loads the settings from the global BADIOptions into the treeview.

  @precon  None.
  @postcon The treeview is updated with the current metrics and checks settings.

**)
Procedure TframeBADIModuleMetrics.LoadSettings;

Var
  N : PVirtualNode;
  NodeData : PMetricNodeData;
  BO: TBADIOptions;

Begin
  N := vstMetrics.GetFirst;
  BO := TBADIOptions.BADIOptions;
  While Assigned(N) Do
    Begin
      NodeData := vstMetrics.GetNodeData(N);
      If BO.ModuleMetric[NodeData.FModuleMetric].FEnabled Then
        vstMetrics.CheckState[N] := csCheckedNormal
      Else
        vstMetrics.CheckState[N] := csUncheckedNormal;
      NodeData.FMetricLimitType := DefaultModuleMetrics[NodeData.FModuleMetric].FLimitType;
      NodeData.FMetricLimit := BO.ModuleMetric[NodeData.FModuleMetric].FLimit;
      N := vstMetrics.GetNext(N);
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
Procedure TframeBADIModuleMetrics.RecurseNodes(Const vstTreeView: TBaseVirtualTree;
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
Procedure TframeBADIModuleMetrics.SaveSettings;

Var
  N : PVirtualNode;
  NodeData : PMetricNodeData;
  R : TBADIMetricRecord;
  BO: TBADIOptions;

Begin
  N := vstMetrics.GetFirst();
  BO := TBADIOptions.BADIOptions;
  While Assigned(N) Do
    Begin
      NodeData := vstMetrics.GetNodeData(N);
      R := BO.ModuleMetric[NodeData.FModuleMetric];
      Case vstMetrics.CheckState[N] Of
        csUncheckedNormal: R.FEnabled := False;
        csCheckedNormal:   R.FEnabled := True;
      End;
      R.FLimit := NodeData.FMetricLimit;
      BO.ModuleMetric[NodeData.FModuleMetric] := R;
      N := vstMetrics.GetNext(N);
    End;
End;

(**

  This method updates the status of the header based on the number of nodee checked.

  @precon  None.
  @postcon Updates the status of the header based on the number of nodee checked.

  @param   Sender as a TBaseVirtualTree
  @param   Node   as a PVirtualNode

**)
procedure TframeBADIModuleMetrics.vstMetricsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);

Var
  iTotal   : Integer;
  iCount   : Integer;
  N        : PVirtualNode;

Begin
  iCount := 0;
  iTotal := 0;
  N := vstMetrics.GetFirst;
  While N <> Nil Do
    Begin
      Inc(iTotal);
      Case Sender.CheckState[N] Of
        csCheckedNormal: Inc(iCount);
      End;
      N := vstMetrics.GetNext(N);
    End;
  If iCount = 0 Then
    vstMetrics.Header.Columns[0].CheckState := csUncheckedNormal
  Else If iCount = iTotal Then
    vstMetrics.Header.Columns[0].CheckState := csCheckedNormal
  Else
    vstMetrics.Header.Columns[0].CheckState := csMixedNormal;
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
Procedure TframeBADIModuleMetrics.vstMetricsEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; Var Allowed: Boolean);

Var
  NodeData : PMetricNodeData;

Begin
  NodeData := Sender.GetNodeData(Node);
  Allowed := (Column = iMetricLimt) And (NodeData.FMetricLimitType <> ltNone);
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
Procedure TframeBADIModuleMetrics.vstMetricsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; Var
  CellText: String);

Const
  strIntegerFmt = '%1.0f';
  strFloatFmt = '%1.3f';

Var
  NodeData : PMetricNodeData;
  BO: TBADIOptions;

Begin
  NodeData := Sender.GetNodeData(Node);
  BO := TBADIOptions.BADIOptions;
  Case Column Of
    iMetricDescription: CellText := DefaultModuleMetrics[NodeData.FModuleMetric].FDescription;
    iMetricName: CellText := DefaultModuleMetrics[NodeData.FModuleMetric].FName;
    iMetricLimt:
      Case DefaultModuleMetrics[NodeData.FModuleMetric].FLimitType Of
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
Procedure TframeBADIModuleMetrics.vstMetricsHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);

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
Procedure TframeBADIModuleMetrics.vstMetricsNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; NewText: String);

ResourceString
  strNotAValidInteger = '%s is not a valid integer';
  strNotAValidFloat = '%s is not a valid floting point number';

Var
  NodeData: PMetricNodeData;
  iInteger : Integer;
  dblDouble : Double;
  iErrorCode: Integer;

Begin
  NodeData := Sender.GetNodeData(Node);
  If Column = 1 Then
    Case NodeData.FMetricLimitType Of
      ltInteger:
        Begin
          Val(NewText, iInteger, iErrorCode);
          If iErrorCode = 0 Then
            NodeData.FMetricLimit := iInteger
          Else
            MessageDlg(Format(strNotAValidInteger, [NewText]), mtError, [mbOK], 0);
        End;
      ltFloat:
        Begin
          Val(NewText, dblDouble, iErrorCode);
          If iErrorCode = 0 Then
            NodeData.FMetricLimit := dblDouble
          Else
            MessageDlg(Format(strNotAValidFloat, [NewText]), mtError, [mbOK], 0);
        End;
    End;
End;

End.
