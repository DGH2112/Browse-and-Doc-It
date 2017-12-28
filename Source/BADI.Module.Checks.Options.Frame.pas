(**

  This module contains a class which represents a frame fo the IDE options dialogue to allow the user
  to select the metrics adn checks they wish to dispay.

  @Author  David Hoyle
  @Version 1.0
  @date    28 Dec 2017

**)
Unit BADI.Module.Checks.Options.Frame;

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
  TBADIChecksOptionsVirtualStringTree = Class(TBADICustomVirtualStringTree);

  (** A class to represent a frame for selecting the checks and metrics in the IDE options dialogue. **)
  TframeBADIModuleChecksOptions = Class(TFrame, IBADIOptionsFrame)
    Procedure vstChecksGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; Var CellText: String);
    Procedure vstChecksEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      Var Allowed: Boolean);
    Procedure vstChecksNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      NewText: String);
    Procedure vstChecksHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    Procedure vstChecksChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    Procedure vstChecksPaintText(Sender: TBaseVirtualTree; Const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  Strict Private
    FVSTChecks : TBADIChecksOptionsVirtualStringTree;
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
  BADI.Functions;

{$R *.dfm}

Type
  (** A record to describe the information stored in the virtual treview. **)
  TCheckNodeData = Record
    FModuleCheck     : TBADIModuleCheck;
    FMetricLimitType : TBADILimitType;
    FMetricLimit     : Double;
  End;
  (** A pointer to tbe above structure. **)
  PCheckNodeData = ^TCheckNodeData;

Const
  (** The column reference for the metric description. **)
  iCheckDescription = 0;
  (** The column reference for the metric name. **)
  iCheckName = 1;
  (** The column reference for the metric limit. **)
  iCheckLimit = 2;

(**

  A constructor for the TframeBADIMetrics class.

  @precon  AOwner must be a valid instance.
  @postcon Initialises the metrics frame for the IDE options dialogue.

  @nocheck MissingCONSTInParam

  @param   AOwner as a TComponent

**)
Constructor TframeBADIModuleChecksOptions.Create(AOwner: TComponent);

  (**

    This method searches the tree view for a node with the given parent and if found returns the node 
    reference for that node else returns nil.

    @precon  None.
    @postcon Returns the node with the given parent else returns nil if not found.

    @param   eCheck as a TBADIModuleCheck as a constant
    @return  a PVirtualNode

  **)
  Function FindParent(Const eCheck : TBADIModuleCheck) : PVirtualNode; Overload;

  Var
    NodeData : PCheckNodeData;
    N: PVirtualNode;

  Begin
    Result := Nil;
    N := FVSTChecks.GetFirst;
    While Assigned(N) Do
      Begin
        NodeData := FVSTChecks.GetNodeData(N);
        If NodeData.FModuleCheck = eCheck Then
          Begin
            Result := N;
            Break;
          End;
        N := FVSTChecks.GetNext(N);
      End;
  End;

Var
  eCheck: TBADIModuleCheck;
  N, P : PVirtualNode;
  NodeData : PCheckNodeData;

Begin
  Inherited Create(AOwner);
  CreateVirtualStringTree;
  FVSTChecks.NodeDataSize := SizeOf(TCheckNodeData);
  For eCheck := Low(TBADIModuleCheck) To High(TBADIModuleCheck) Do
    Begin
      P := FindParent(ModuleChecks[eCheck].FParent);
      N := FVSTChecks.AddChild(P);
      FVSTChecks.CheckType[N] := ctCheckBox;
      FVSTChecks.CheckState[N] := csUncheckedNormal;
      NodeData := FVSTChecks.GetNodeData(N);
      NodeData.FModuleCheck := eCheck;
      NodeData.FMetricLimitType := ltNone;
      NodeData.FMetricLimit := 0;
    End;
  FVSTChecks.FullExpand;
End;

(**

  This method creates and initialises the treeview..

  @precon  None.
  @postcon The treeview is created and initialised.

  @nocheck HardCodedString HardCodedInteger

**)
Procedure TframeBADIModuleChecksOptions.CreateVirtualStringTree;

Var
  C: TVirtualTreeColumn;

Begin
  FVSTChecks := TBADIChecksOptionsVirtualStringTree.Create(Self);
  FVSTChecks.Name := 'vstChecks';
  FVSTChecks.Parent := Self;
  FVSTChecks.AlignWithMargins := True;
  FVSTChecks.Left := 3;
  FVSTChecks.Top := 3;
  FVSTChecks.Width := 413;
  FVSTChecks.Height := 338;
  FVSTChecks.Align := alClient;
  FVSTChecks.EditDelay := 250;
  FVSTChecks.Header.AutoSizeIndex := 0;
  FVSTChecks.Header.Height := 20;
  FVSTChecks.Header.Options := FVSTChecks.Header.Options + [hoColumnResize, hoShowImages];
  FVSTChecks.TreeOptions.AutoOptions := [toAutoDropExpand, toAutoScrollOnExpand, toAutoTristateTracking,
    toAutoDeleteMovedNodes, toAutoChangeScale];
  FVSTChecks.TreeOptions.MiscOptions := FVSTChecks.TreeOptions.MiscOptions +
    [toCheckSupport, toEditable, toEditOnClick, toEditOnDblClick];
  FVSTChecks.OnChecked := vstChecksChecked;
  FVSTChecks.OnEditing := vstChecksEditing;
  FVSTChecks.OnGetText := vstChecksGetText;
  FVSTChecks.OnPaintText := vstChecksPaintText;
  FVSTChecks.OnHeaderClick := vstChecksHeaderClick;
  FVSTChecks.OnNewText := vstChecksNewText;
  C := FVSTChecks.Header.Columns.Add;
  C.CheckBox := True;
  C.Position := 0;
  C.Width := 184;
  C.Text := 'Module Checks';
  C := FVSTChecks.Header.Columns.Add;
  C.Position := 1;
  C.Width := 150;
  C.Text := 'Name';
//  C := FVSTChecks.Header.Columns.Add;
//  C.Alignment := taRightJustify;
//  C.Position := 2;
//  C.Width := 75;
//  C.Text := 'Limit';
End;

(**

  This method loads the settings from the global BADIOptions into the treeview.

  @precon  None.
  @postcon The treeview is updated with the current metrics and checks settings.

**)
Procedure TframeBADIModuleChecksOptions.LoadSettings;

Var
  N : PVirtualNode;
  NodeData : PCheckNodeData;
  BO: TBADIOptions;

Begin
  N := FVSTChecks.GetFirst;
  BO := TBADIOptions.BADIOptions;
  While Assigned(N) Do
    Begin
      NodeData := FVSTChecks.GetNodeData(N);
      If BO.ModuleCheck[NodeData.FModuleCheck].FEnabled Then
        FVSTChecks.CheckState[N] := csCheckedNormal
      Else
        FVSTChecks.CheckState[N] := csUncheckedNormal;
      NodeData.FMetricLimitType := ModuleChecks[NodeData.FModuleCheck].FLimitType;
      NodeData.FMetricLimit := BO.ModuleCheck[NodeData.FModuleCheck].FLimit;
      N := FVSTChecks.GetNext(N);
    End;
  vstChecksChecked(Nil, Nil);
End;

(**

  This method recurses the nodes in the treeview and updates the checked state to that given.

  @precon  vstTreeView and Node must be valid instances.
  @postcon The checkded state of the treeview is updated.

  @param   vstTreeView as a TBaseVirtualTree as a constant
  @param   Node        as a PVirtualNode as a constant
  @param   State       as a TCheckState as a constant

**)
Procedure TframeBADIModuleChecksOptions.RecurseNodes(Const vstTreeView: TBaseVirtualTree;
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
Procedure TframeBADIModuleChecksOptions.SaveSettings;

Var
  N : PVirtualNode;
  NodeData : PCheckNodeData;
  R : TBADICheckRecord;
  BO: TBADIOptions;

Begin
  N := FVSTChecks.GetFirst();
  BO := TBADIOptions.BADIOptions;
  While Assigned(N) Do
    Begin
      NodeData := FVSTChecks.GetNodeData(N);
      R := BO.ModuleCheck[NodeData.FModuleCheck];
      Case FVSTChecks.CheckState[N] Of
        csUncheckedNormal: R.FEnabled := False;
        csCheckedNormal:   R.FEnabled := True;
      End;
      R.FLimit := NodeData.FMetricLimit;
      BO.ModuleCheck[NodeData.FModuleCheck] := R;
      N := FVSTChecks.GetNext(N);
    End;
End;

(**

  This method updates the status of the header based on the number of nodee checked.

  @precon  None.
  @postcon Updates the status of the header based on the number of nodee checked.

  @param   Sender as a TBaseVirtualTree
  @param   Node   as a PVirtualNode

**)
procedure TframeBADIModuleChecksOptions.vstChecksChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);

Var
  iTotal   : Integer;
  iCount   : Integer;
  N        : PVirtualNode;

Begin
  iCount := 0;
  iTotal := 0;
  N := FVSTChecks.GetFirst;
  While N <> Nil Do
    Begin
      Inc(iTotal);
      Case Sender.CheckState[N] Of
        csCheckedNormal: Inc(iCount);
      End;
      N := FVSTChecks.GetNext(N);
    End;
  If iCount = 0 Then
    FVSTChecks.Header.Columns[0].CheckState := csUncheckedNormal
  Else If iCount = iTotal Then
    FVSTChecks.Header.Columns[0].CheckState := csCheckedNormal
  Else
    FVSTChecks.Header.Columns[0].CheckState := csMixedNormal;
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
Procedure TframeBADIModuleChecksOptions.vstChecksEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; Var Allowed: Boolean);

Var
  NodeData : PCheckNodeData;

Begin
  NodeData := Sender.GetNodeData(Node);
  Allowed := (Column = iCheckLimit) And (NodeData.FMetricLimitType <> ltNone);
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
Procedure TframeBADIModuleChecksOptions.vstChecksGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; Var
  CellText: String);

Const
  strIntegerFmt = '%1.0f';
  strFloatFmt = '%1.3f';

Var
  NodeData : PCheckNodeData;

Begin
  NodeData := Sender.GetNodeData(Node);
  Case Column Of
    iCheckDescription: CellText := ModuleChecks[NodeData.FModuleCheck].FDescription;
    iCheckName:
      If (NodeData.FModuleCheck = ModuleChecks[NodeData.FModuleCheck].FParent) Then
        CellText := ModuleChecks[NodeData.FModuleCheck].FName
      Else
        CellText := '';
    iCheckLimit:
      Case ModuleChecks[NodeData.FModuleCheck].FLimitType Of
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
Procedure TframeBADIModuleChecksOptions.vstChecksHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);

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
Procedure TframeBADIModuleChecksOptions.vstChecksNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; NewText: String);

ResourceString
  strNotAValidInteger = '%s is not a valid integer!';
  strNotAValidFloat = '%s is not a valid floting point number!';
  strIntegerMustBeGreaterThanZero = 'The limit value (%s) must be greater than zero!';
  strFloatMustBeGreaterThanZero = 'The limit value (%s) must be greater than zero!';

Var
  NodeData: PCheckNodeData;
  iInteger : Integer;
  dblDouble : Double;
  iErrorCode: Integer;

Begin
  NodeData := Sender.GetNodeData(Node);
  If Column = iCheckLimit Then
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
Procedure TframeBADIModuleChecksOptions.vstChecksPaintText(Sender: TBaseVirtualTree;
  Const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);

Begin
  TargetCanvas.Font.Color := clWindowText;
  {$IFDEF DXE102}
  If Assigned(FVSTChecks.StyleServices) And FVSTChecks.StyleServices.Enabled Then
    TargetCanvas.Font.Color := FVSTChecks.StyleServices.GetSystemColor(clWindowText);
  {$ENDIF}
End;

End.
