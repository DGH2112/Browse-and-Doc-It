(**

  This module contains a class which represents a frame fo the IDE options dialogue to allow the user
  to select the metrics adn checks they wish to dispay.

  @Author  David Hoyle
  @Version 1.0
  @date    17 Dec 2017

**)
Unit BADI.ModuleChecksFrame;

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
  TframeBADIModuleChecks = Class(TFrame, IBADIOptionsFrame)
    vstChecks: TVirtualStringTree;
    Procedure vstChecksGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; Var CellText: String);
    Procedure vstChecksEditing(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      Var Allowed: Boolean);
    Procedure vstChecksNewText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      NewText: String);
    Procedure vstChecksHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);
    Procedure vstChecksChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    Procedure vstChecksBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      Var ContentRect: TRect);
    procedure vstChecksPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
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

  @nometric MissingCONSTInParam

  @param   AOwner as a TComponent

**)
Constructor TframeBADIModuleChecks.Create(AOwner: TComponent);

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
    N := vstChecks.GetFirst;
    While Assigned(N) Do
      Begin
        NodeData := vstChecks.GetNodeData(N);
        If NodeData.FModuleCheck = eCheck Then
          Begin
            Result := N;
            Break;
          End;
        N := vstChecks.GetNext(N);
      End;
  End;

Var
  eCheck: TBADIModuleCheck;
  N, P : PVirtualNode;
  NodeData : PCheckNodeData;

Begin
  Inherited Create(AOwner);
  vstChecks.NodeDataSize := SizeOf(TCheckNodeData);
  For eCheck := Low(TBADIModuleCheck) To High(TBADIModuleCheck) Do
    Begin
      P := FindParent(ModuleChecks[eCheck].FParent);
      N := vstChecks.AddChild(P);
      vstChecks.CheckType[N] := ctCheckBox;
      vstChecks.CheckState[N] := csUncheckedNormal;
      NodeData := vstChecks.GetNodeData(N);
      NodeData.FModuleCheck := eCheck;
      NodeData.FMetricLimitType := ltNone;
      NodeData.FMetricLimit := 0;
    End;
  vstChecks.FullExpand;
End;

(**

  This method loads the settings from the global BADIOptions into the treeview.

  @precon  None.
  @postcon The treeview is updated with the current metrics and checks settings.

**)
Procedure TframeBADIModuleChecks.LoadSettings;

Var
  N : PVirtualNode;
  NodeData : PCheckNodeData;
  BO: TBADIOptions;

Begin
  N := vstChecks.GetFirst;
  BO := TBADIOptions.BADIOptions;
  While Assigned(N) Do
    Begin
      NodeData := vstChecks.GetNodeData(N);
      If BO.ModuleCheck[NodeData.FModuleCheck].FEnabled Then
        vstChecks.CheckState[N] := csCheckedNormal
      Else
        vstChecks.CheckState[N] := csUncheckedNormal;
      NodeData.FMetricLimitType := ModuleChecks[NodeData.FModuleCheck].FLimitType;
      NodeData.FMetricLimit := BO.ModuleCheck[NodeData.FModuleCheck].FLimit;
      N := vstChecks.GetNext(N);
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
Procedure TframeBADIModuleChecks.RecurseNodes(Const vstTreeView: TBaseVirtualTree;
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
Procedure TframeBADIModuleChecks.SaveSettings;

Var
  N : PVirtualNode;
  NodeData : PCheckNodeData;
  R : TBADICheckRecord;
  BO: TBADIOptions;

Begin
  N := vstChecks.GetFirst();
  BO := TBADIOptions.BADIOptions;
  While Assigned(N) Do
    Begin
      NodeData := vstChecks.GetNodeData(N);
      R := BO.ModuleCheck[NodeData.FModuleCheck];
      Case vstChecks.CheckState[N] Of
        csUncheckedNormal: R.FEnabled := False;
        csCheckedNormal:   R.FEnabled := True;
      End;
      R.FLimit := NodeData.FMetricLimit;
      BO.ModuleCheck[NodeData.FModuleCheck] := R;
      N := vstChecks.GetNext(N);
    End;
End;

(**

  This is an on before cell paint event handler for the treeview.

  @precon  None.
  @postcon Colours the focused column more than the whole row.

  @param   Sender        as a TBaseVirtualTree
  @param   TargetCanvas  as a TCanvas
  @param   Node          as a PVirtualNode
  @param   Column        as a TColumnIndex
  @param   CellPaintMode as a TVTCellPaintMode
  @param   CellRect      as a TRect
  @param   ContentRect   as a TRect as a reference

**)
Procedure TframeBADIModuleChecks.vstChecksBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode;
  CellRect: TRect; Var ContentRect: TRect);

Const
  dblBlendFactor = 0.50;

Begin
  If (Node = Sender.FocusedNode) And (Column = Sender.FocusedColumn) Then
    TargetCanvas.Brush.Color := BlendColour(TargetCanvas.Brush.Color, clHighlight, dblBlendFactor);
  TargetCanvas.FillRect(CellRect);
End;

(**

  This method updates the status of the header based on the number of nodee checked.

  @precon  None.
  @postcon Updates the status of the header based on the number of nodee checked.

  @param   Sender as a TBaseVirtualTree
  @param   Node   as a PVirtualNode

**)
procedure TframeBADIModuleChecks.vstChecksChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);

Var
  iTotal   : Integer;
  iCount   : Integer;
  N        : PVirtualNode;

Begin
  iCount := 0;
  iTotal := 0;
  N := vstChecks.GetFirst;
  While N <> Nil Do
    Begin
      Inc(iTotal);
      Case Sender.CheckState[N] Of
        csCheckedNormal: Inc(iCount);
      End;
      N := vstChecks.GetNext(N);
    End;
  If iCount = 0 Then
    vstChecks.Header.Columns[0].CheckState := csUncheckedNormal
  Else If iCount = iTotal Then
    vstChecks.Header.Columns[0].CheckState := csCheckedNormal
  Else
    vstChecks.Header.Columns[0].CheckState := csMixedNormal;
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
Procedure TframeBADIModuleChecks.vstChecksEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
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
Procedure TframeBADIModuleChecks.vstChecksGetText(Sender: TBaseVirtualTree;
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
Procedure TframeBADIModuleChecks.vstChecksHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);

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
Procedure TframeBADIModuleChecks.vstChecksNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
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
Procedure TframeBADIModuleChecks.vstChecksPaintText(Sender: TBaseVirtualTree;
  Const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);

Begin
  TargetCanvas.Font.Color := clWindowText;
  If (Node = Sender.FocusedNode) And (Column = Sender.FocusedColumn) Then
    TargetCanvas.Font.Color := clHighlightText;
End;

End.
