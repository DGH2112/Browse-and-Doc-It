(**

  This module contains a class which represents the Browse and Doc It general Options as
  a frame that can be inserted into a form or the IDEs main optiosn dialogue.

  @Author  David Hoyle
  @Version 1.0
  @Date    21 Jun 2019

  @license

    Browse and Doc It is a RAD Studio plug-in for browsing, checking and
    documenting your code.
    
    Copyright (C) 2019  David Hoyle (https://github.com/DGH2112/Browse-and-Doc-It/)

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

**)
Unit BADI.GeneralOptionsFrame;

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
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  CheckLst,
  BADI.CustomOptionsFrame,
  VirtualTrees;

Type
  (** A class to represent the frame interface. **)
  TfmBADIGeneralOptions = Class(TFrame, IBADIOptionsFrame)
    IntervalPanel: TPanel;
    lblRefreshInterval: TLabel;
    lblManagedNodesLife: TLabel;
    edtUpdateInterval: TEdit;
    udUpdateInterval: TUpDown;
    edtManagedNodesLife: TEdit;
    udManagedNodesLife: TUpDown;
    vstGeneralOptions: TVirtualStringTree;
    procedure vstGeneralOptionsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstGeneralOptionsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstGeneralOptionsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  Private
    { Private declarations }
  Public
    { Public declarations }
    Procedure LoadSettings;
    Procedure SaveSettings;
  End;

Implementation

{$R *.dfm}

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  BADI.Base.Module,
  BADI.Types,
  BADI.Constants,
  BADI.Options;

Type
  (** A record to describe the data to be stored in each tree node. **)
  TBADIGeneralOpsRec = Record
    FText        : String;
    FGroupHeader : Boolean;
    FDocOption   : TDocOption;
  End;
  (** A pointer to the above tree node record. **)
  PBADIGeneralOpsRec = ^TBADIGeneralOpsRec;

(**

  This method loads the BADI options into the frames controls.

  @precon  None.
  @postcon The frame is initialised with the BADI options.

**)
Procedure TfmBADIGeneralOptions.LoadSettings;

Var
  iGroup : TDocOptionGroup;
  iOption : TDocOption;
  ParentNode: PVirtualNode;
  NodeData : PBADIGeneralOpsRec;
  Node: PVirtualNode;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'LoadSettings', tmoTiming);{$ENDIF}
  vstGeneralOptions.NodeDataSize := SizeOf(TBADIGeneralOpsRec);
  For iGroup := Low(TDocOptionGroup) To High(TDocOptionGroup) Do
    Begin
      ParentNode := vstGeneralOptions.AddChild(Nil);
      NodeData := vstGeneralOptions.GetNodeData(ParentNode);
      NodeData.FGroupHeader := True;
      NodeData.FText := DocOptionGroups[iGroup];
      For iOption := Low(TDocOption) To High(TDocOption) Do
        If DocOptionInfo[iOption].FGroup = iGroup Then
          Begin
            Node := vstGeneralOptions.AddChild(ParentNode);
            NodeData := vstGeneralOptions.GetNodeData(Node);
            NodeData.FGroupHeader := False;
            NodeData.FText := DocOptionInfo[iOption].FDescription;
            vstGeneralOptions.CheckType[Node] := ctCheckBox;
            vstGeneralOptions.CheckState[Node] := csUncheckedNormal;
            If iOption In TBADIOptions.BADIOptions.Options Then
              vstGeneralOptions.CheckState[Node] := csCheckedNormal;
            NodeData.FDocOption := iOption;
          End;
    End;
  vstGeneralOptions.FullExpand();
  udUpdateInterval.Position := TBADIOptions.BADIOptions.UpdateInterval;
  udManagedNodesLife.Position := TBADIOptions.BADIOptions.ManagedNodesLife;
End;

(**

  This method saves the settings in the frames controls back to the BADI options class.

  @precon  None.
  @postcon The frames settings are captured and stored in the BADI options class.

**)
Procedure TfmBADIGeneralOptions.SaveSettings;

Var
  Node: PVirtualNode;
  NodeData : PBADIGeneralOpsRec;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'SaveSettings', tmoTiming);{$ENDIF}
  Node := vstGeneralOptions.GetFirst();
  While Assigned(Node) Do
    Begin
      NodeData := vstGeneralOptions.GetNodeData(Node);
      If Not NodeData.FGroupHeader Then
        If vstGeneralOptions.CheckState[Node] = csCheckedNormal Then
          TBADIOptions.BADIOptions.Options := TBADIOptions.BADIOptions.Options + [NodeData.FDocOption]
        Else
          TBADIOptions.BADIOptions.Options := TBADIOptions.BADIOptions.Options - [NodeData.FDocOption];
      Node := vstGeneralOptions.GetNext(Node);
    End;
  TBADIOptions.BADIOptions.UpdateInterval := udUpdateInterval.Position;
  TBADIOptions.BADIOptions.ManagedNodesLife := udManagedNodesLife.Position;
  TBADIOptions.BADIOptions.RequiresIDEEditorColoursUpdate;
End;

(**

  This is an on free node event handler for the gernal options treeview.

  @precon  None.
  @postcon Ensures that the strings are freed.

  @param   Sender as a TBaseVirtualTree
  @param   Node   as a PVirtualNode

**)
Procedure TfmBADIGeneralOptions.vstGeneralOptionsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);

Var
  NodeData: PBADIGeneralOpsRec;

Begin
  NodeData := Sender.GetNodeData(Node);
  Finalize(NodeData^);
End;

(**

  This is an on get text event handler for the generla options treeview.

  @precon  None.
  @postcon Returned the text from the node data.

  @param   Sender   as a TBaseVirtualTree
  @param   Node     as a PVirtualNode
  @param   Column   as a TColumnIndex
  @param   TextType as a TVSTTextType
  @param   CellText as a String as a reference

**)
Procedure TfmBADIGeneralOptions.vstGeneralOptionsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; Var CellText: String);

Var
  NodeData: PBADIGeneralOpsRec;

Begin
  NodeData := Sender.GetNodeData(Node);
  CellText := NodeData.FText;
End;

(**

  This is an on paint text event handler for the general optins treeview.

  @precon  None.
  @postcon Forces the group headings to be bold.

  @param   Sender       as a TBaseVirtualTree
  @param   TargetCanvas as a TCanvas as a constant
  @param   Node         as a PVirtualNode
  @param   Column       as a TColumnIndex
  @param   TextType     as a TVSTTextType

**)
Procedure TfmBADIGeneralOptions.vstGeneralOptionsPaintText(Sender: TBaseVirtualTree;
  Const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);

Var
  NodeData: PBADIGeneralOpsRec;

Begin
  NodeData := Sender.GetNodeData(Node);
  TargetCanvas.Font.Style := [];
  If NodeData.FGroupHeader Then
    TargetCanvas.Font.Style := [fsBold];
End;

End.
