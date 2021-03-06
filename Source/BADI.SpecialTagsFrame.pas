(**

  This module contains a frame for editing the BADI special tags.

  @Version 1.0
  @Author  David Hoyle
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
Unit BADI.SpecialTagsFrame;

Interface

{$INCLUDE CompilerDefinitions.inc}

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
  StdCtrls,
  Buttons,
  ComCtrls,
  Generics.Collections,
  BADI.CustomOptionsFrame,
  BADI.Types,
  VirtualTrees,
  Themes, 
  BADI.CustomVirtualStringTree, System.ImageList, Vcl.ImgList;

Type
  (** A descentand class for the virtual string tree to prevent AVs in the 10.2.2. IDE durin theming. **)
  TBADISpecialTagsOptionsVirtualStringTree = Class(TBADICustomVirtualStringTree);

  (** This is a class to represent the frame interface. **)
  TfmBADISpecialTagsFrame = Class(TFrame, IBADIOptionsFrame)
    btnDelete: TButton;
    btnEdit: TButton;
    btnMoveDown: TButton;
    btnMoveUp: TButton;
    btnAdd: TButton;
    ilButtonIcons: TImageList;
    Procedure btnAddClick(Sender: TObject);
    Procedure btnDeleteClick(Sender: TObject);
    Procedure btnEditClick(Sender: TObject);
    Procedure btnMoveUpClick(Sender: TObject);
    Procedure btnMoveDownClick(Sender: TObject);
    Procedure vstSpecialTagsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; Var CellText: String);
    Procedure vstSpecialTagsClick(Sender: TObject);
    Procedure vstSpecialTagsDblClick(Sender: TObject);
    Procedure vstSpecialTagsBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      Var ContentRect: TRect);
    Procedure vstSpecialTagsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    Procedure vstSpecialTagsPaintText(Sender: TBaseVirtualTree; Const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
  Strict Private
    FSpecialTags    : TList<TBADISpecialTag>;
    FVSTSpecialTags : TBADISpecialTagsOptionsVirtualStringTree;
    {$IFDEF DXE102}
    FStyleServices : TCustomStyleServices;
    {$ENDIF}
  Strict Protected
    Procedure PopulateTreeView;
    Procedure CreateVirtualStringTree;
  Public
    //: @nometric MissingCONSTInParam
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Procedure LoadSettings;
    Procedure SaveSettings;
  End;

Implementation

{$R *.dfm}


Uses
  {$IFNDEF STANDALONEAPP}
  ToolsAPI,
  {$ENDIF}
  BADI.Base.Module,
  BADI.SpecialTagForm,
  BADI.Constants,
  BADI.OptionsForm,
  BADI.Options;

Type
  (** A record to describe the data to be stoed in the treeview. **)
  TSpecialTagsNodeData = Record
    FSpecialTagIndex : Integer;
  End;
  (** A pointer to the above node data. **)
  PSpecialTagNodeData = ^TSpecialTagsNodeData;
  (** A enumerate to define the columns in the report. **)
  TColumnIndexes = (ciName, ciDescription, ciShowInTree, ciAutoExpand, ciShowInDocs, ciFixed, ciSyntax);

(**

  This is a TButton on click event. It allows the user to add a new tag.

  @precon  Sender is the control that invoked the event.
  @postcon Adds a tag to the list.

  @param   Sender as a TObject

**)
Procedure TfmBADISpecialTagsFrame.btnAddClick(Sender: TObject);

Const
  strTag = 'Tag';
  strTagDescription = 'Tag Description';

Var
  ST : TBADISpecialTag;

Begin
  ST.Create(strTag, strTagDescription, []);
  If TfrmSpecialTag.Execute(ST) Then
    Begin
      FSpecialTags.Add(ST);
      PopulateTreeView;
    End;
End;

(**

  This is a TButton on click event. It allows the user to delete a tag from
  the list.

  @precon  Sender is the control that invoked the event.
  @postcon The method deletes the selected tag from the list.

  @param   Sender as a TObject

**)
Procedure TfmBADISpecialTagsFrame.btnDeleteClick(Sender: TObject);

Var
  NodeData : PSpecialTagNodeData;

Begin
  If Assigned(FVSTSpecialTags.FocusedNode) Then
    Begin
      NodeData := FVSTSpecialTags.GetNodeData(FVSTSpecialTags.FocusedNode);
      FSpecialTags.Delete(NodeData.FSpecialTagIndex);
      PopulateTreeView;
    End;
End;

(**

  This is a TButton on click event. It allows the user to edit a tag in the
  list.

  @precon  Sender is the control that invoked the event.
  @postcon Allows the user to edit the selected tag.

  @param   Sender as a TObject

**)
Procedure TfmBADISpecialTagsFrame.btnEditClick(Sender: TObject);

Var
  ST: TBADISpecialTag;
  NodeData : PSpecialTagNodeData;

Begin
  If Assigned(FVSTSpecialTags.FocusedNode) Then
    Begin
      NodeData := FVSTSpecialTags.GetNodeData(FVSTSpecialTags.FocusedNode);
      ST := FSpecialTags[NodeData.FSpecialTagIndex];
      If TfrmSpecialTag.Execute(ST) Then
        Begin
          FSpecialTags[NodeData.FSpecialTagIndex] := ST;
          PopulateTreeView;
        End;
    End;
End;

(**

  This is a TButton on click event. It allows the user to move the item down
  the list.

  @precon  Sender is the control that invoked the event.
  @postcon Move the selected tag down the list.

  @param   Sender as a TObject

**)
Procedure TfmBADISpecialTagsFrame.btnMoveDownClick(Sender: TObject);

Var
  NodeData : PSpecialTagNodeData;

Begin
  If Assigned(FVSTSpecialTags.FocusedNode) Then
    Begin
      NodeData := FVSTSpecialTags.GetNodeData(FVSTSpecialTags.FocusedNode);
      If NodeData.FSpecialTagIndex < FSpecialTags.Count - 1 Then
        Begin
          FSpecialTags.Exchange(NodeData.FSpecialTagIndex, NodeData.FSpecialTagIndex + 1);
          FVSTSpecialTags.FocusedNode := FVSTSpecialTags.GetNextSibling(FVSTSpecialTags.FocusedNode);
          PopulateTreeView;
        End;
    End;
End;

(**

  This is a TButton on click event. It allows the user to move the item up the
  list.

  @precon  Sender is the control that invoked the event.
  @postcon Moves the selected tag up the list.

  @param   Sender as a TObject

**)
Procedure TfmBADISpecialTagsFrame.btnMoveUpClick(Sender: TObject);

Var
  NodeData : PSpecialTagNodeData;

Begin
  If Assigned(FVSTSpecialTags.FocusedNode) Then
    Begin
      NodeData := FVSTSpecialTags.GetNodeData(FVSTSpecialTags.FocusedNode);
      If NodeData.FSpecialTagIndex > 0 Then
        Begin
          FSpecialTags.Exchange(NodeData.FSpecialTagIndex, NodeData.FSpecialTagIndex - 1);
          FVSTSpecialTags.FocusedNode := FVSTSpecialTags.GetPreviousSibling(FVSTSpecialTags.FocusedNode);
          PopulateTreeView;
        End;
    End;
End;

(**

  A constructor for the TfmBADISpecialTagsFrame class.

  @precon  None.
  @postcon Updates the status of the buttons on the form.

  @nocheck MissingCONSTInParam
  
  @param   AOwner as a TComponent

**)
Constructor TfmBADISpecialTagsFrame.Create(AOwner: TComponent);

{$IFNDEF STANDALONEAPP}
{$IFDEF DXE102}
Var
  ITS : IOTAIDEThemingServices;
{$ENDIF}
{$ENDIF}

Begin
  Inherited Create(AOwner);
  CreateVirtualStringTree;
  {$IFNDEF STANDALONEAPP}
  {$IFDEF DXE102}
  FStyleServices := Nil;
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
    If ITS.IDEThemingEnabled Then
      FStyleServices := ITS.StyleServices;
  {$ENDIF}
  {$ENDIF}
  FVSTSpecialTags.NodeDataSize := SizeOf(TSpecialTagsNodeData);
  FSpecialTags := TList<TBADISpecialTag>.Create;
  PopulateTreeView;
End;

(**

  This method creates a descendant of the TVirtualStringtree for displaying the special tags information.

  @precon  None.
  @postcon The treeview is created and displayed in the parent frame.

  @nocheck HardCodedInteger HardCodedString
  @nometric LongMethod Toxicity

**)
Procedure TfmBADISpecialTagsFrame.CreateVirtualStringTree;

Var
  C: TVirtualTreeColumn;

Begin
  FVSTSpecialTags := TBADISpecialTagsOptionsVirtualStringTree.Create(Self);
  FVSTSpecialTags.Name := 'vstSpecialTags';
  FVSTSpecialTags.Parent := Self;
  FVSTSpecialTags.Left := 3;
  FVSTSpecialTags.Top := 3;
  FVSTSpecialTags.Width := 639;
  FVSTSpecialTags.Height := 386;
  FVSTSpecialTags.Anchors := [akLeft, akTop, akRight, akBottom];
  FVSTSpecialTags.Header.Height := 20;
  FVSTSpecialTags.TabOrder := 0;
  FVSTSpecialTags.OnBeforeCellPaint := vstSpecialTagsBeforeCellPaint;
  FVSTSpecialTags.OnClick := vstSpecialTagsClick;
  FVSTSpecialTags.OnDblClick := vstSpecialTagsDblClick;
  FVSTSpecialTags.OnGetText := vstSpecialTagsGetText;
  FVSTSpecialTags.OnPaintText := vstSpecialTagsPaintText;
  FVSTSpecialTags.OnMouseDown := vstSpecialTagsMouseDown;
  C := FVSTSpecialTags.Header.Columns.Add;
  C.Position := 0;
  C.Style := vsOwnerDraw;
  C.Width := 100;
  C.Text := 'Tag Name';
  C := FVSTSpecialTags.Header.Columns.Add;
  C.MinWidth := 200;
  C.Position := 1;
  C.Style := vsOwnerDraw;
  C.Width := 235;
  C.Text := 'Tag Description';
  C := FVSTSpecialTags.Header.Columns.Add;
  C.Alignment := taCenter;
  C.Position := 2;
  C.Style := vsOwnerDraw;
  C.Width := 60;
  C.Text := 'Tree';
  C := FVSTSpecialTags.Header.Columns.Add;
  C.Alignment := taCenter;
  C.Position := 3;
  C.Style := vsOwnerDraw;
  C.Width := 60;
  C.Text := 'Expand';
  C := FVSTSpecialTags.Header.Columns.Add;
  C.Alignment := taCenter;
  C.Position := 4;
  C.Style := vsOwnerDraw;
  C.Width := 60;
  C.Text := 'Docs';
  C := FVSTSpecialTags.Header.Columns.Add;
  C.Alignment := taCenter;
  C.Position := 5;
  C.Style := vsOwnerDraw;
  C.Width := 60;
  C.Text := 'Fixed';
  C := FVSTSpecialTags.Header.Columns.Add;
  C.Alignment := taCenter;
  C.Position := 6;
  C.Style := vsOwnerDraw;
  C.Width := 60;
  C.Text := 'Syntax';
  FVSTSpecialTags.Header.AutoSizeIndex := 1;
End;

(**

  A destructor for the TfmBADISpecialTagsFrame class.

  @precon  None.
  @postcon Frees the memory used by the frame.

**)
Destructor TfmBADISpecialTagsFrame.Destroy;

Begin
  FSpecialTags.Free;
  Inherited Destroy;
End;

(**

  This method loads the settings in the frame from the BAID Options class.

  @precon  None.
  @postcon The settings in the frame controls are loaded from the option class.

**)
Procedure TfmBADISpecialTagsFrame.LoadSettings;

Begin
  FSpecialTags.AddRange(TBADIOptions.BADIOptions.SpecialTags);
  PopulateTreeView;
End;

(**

  This method renders the list of special tags and options in the list view.

  @precon  None.
  @postcon The list view is populated with the special tags and their options.

**)
Procedure TfmBADISpecialTagsFrame.PopulateTreeView;

Var
  iSpecialTag: Integer;
  N: PVirtualNode;
  NodeData : PSpecialTagNodeData;
  strSelectedNode: String;

Begin
  FVSTSpecialTags.BeginUpdate;
  Try
    If Assigned(FVSTSpecialTags.FocusedNode) Then
      strSelectedNode := FVSTSpecialTags.Text[FVSTSpecialTags.FocusedNode, 0];
    FVSTSpecialTags.Clear;
    For iSpecialTag := 0 To FSpecialTags.Count - 1 Do
      Begin
        N := FVSTSpecialTags.AddChild(Nil);
        NodeData := FVSTSpecialTags.GetNodeData(N);
        NodeData.FSpecialTagIndex := iSpecialTag;
      End;
    N := FVSTSpecialTags.GetFirst();
    While Assigned(N) Do
      Begin
        If FVSTSpecialTags.Text[N, 0] = strSelectedNode Then
          Begin
            FVSTSpecialTags.FocusedNode := N;
            FVSTSpecialTags.Selected[N] := True;
            Break;
          End;
        N := FVSTSpecialTags.GetNext(N);
      End;
  Finally
    FVSTSpecialTags.EndUpdate;
  End;
  vstSpecialTagsClick(Nil);
End;

(**

  This method saves the settings in the frame back to the BAID Options class.

  @precon  None.
  @postcon The settings in the frame controls are saved to the option class.

**)
Procedure TfmBADISpecialTagsFrame.SaveSettings;

Begin
  TBADIOptions.BADIOptions.SpecialTags.Clear;
  TBADIOptions.BADIOptions.SpecialTags.AddRange(FSpecialTags);
End;

(**

  This is an on before cell paint event handler for the treeview.

  @precon  None.
  @postcon Colours the cells light red or light green depending upon whether they are true or false.

  @param   Sender        as a TBaseVirtualTree
  @param   TargetCanvas  as a TCanvas
  @param   Node          as a PVirtualNode
  @param   Column        as a TColumnIndex
  @param   CellPaintMode as a TVTCellPaintMode
  @param   CellRect      as a TRect
  @param   ContentRect   as a TRect as a reference

**)
Procedure TfmBADISpecialTagsFrame.vstSpecialTagsBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode;
  CellRect: TRect; Var ContentRect: TRect);

Const
  Colour : Array[False..True] Of TColor = ($8080FF, $80FF80);

Var
  NodeData : PSpecialTagNodeData;
  ST: TBADISpecialTag;
  
Begin
  NodeData := Sender.GetNodeData(Node);
  ST := FSpecialTags[NodeData.FSpecialTagIndex];
  Case TColumnIndexes(Column) Of
    ciShowInTree: TargetCanvas.Brush.Color := Colour[tpShowInTree In ST.FTagProperties];
    ciAutoExpand: TargetCanvas.Brush.Color := Colour[tpAutoExpand In ST.FTagProperties];
    ciShowInDocs: TargetCanvas.Brush.Color := Colour[tpShowInDoc  In ST.FTagProperties];
    ciFixed:      TargetCanvas.Brush.Color := Colour[tpFixed      In ST.FTagProperties];
    ciSyntax:     TargetCanvas.Brush.Color := Colour[tpSyntax     In ST.FTagProperties];
  Else
    TargetCanvas.Brush.Color := clWindow;
    {$IFDEF DXE102}
    If Assigned(FStyleServices) Then
      TargetCanvas.Brush.Color := FStyleServices.GetSystemColor(clWindow);
    {$ENDIF}
    If ST.FBackColour <> clNone Then
      TargetCanvas.Brush.Color := ST.FBackColour;
  End;
  TargetCanvas.FillRect(CellRect);
End;

(**

  This is an on click event handler for the treeview.

  @precon  None.
  @postcon Updates the enabled properties of the button depending upon what is selected.

  @param   Sender as a TObject

**)
Procedure TfmBADISpecialTagsFrame.vstSpecialTagsClick(Sender: TObject);

Var
  NodeData : PSpecialTagNodeData;
  
Begin
  btnDelete.Enabled := Assigned(FVSTSpecialTags.FocusedNode);
  btnEdit.Enabled := Assigned(FVSTSpecialTags.FocusedNode);
  NodeData := Nil;
  If Assigned(FVSTSpecialTags.FocusedNode) Then
    NodeData := FVSTSpecialTags.GetNodeData(FVSTSpecialTags.FocusedNode);
  btnMoveDown.Enabled := Assigned(FVSTSpecialTags.FocusedNode) And Assigned(NodeData) And
    (NodeData.FSpecialTagIndex > -1) And (NodeData.FSpecialTagIndex < FSpecialTags.Count - 1);
  btnMoveUp.Enabled := Assigned(FVSTSpecialTags.FocusedNode) And Assigned(NodeData) And
    (NodeData.FSpecialTagIndex > 0);
End;

(**

  This is an on double click event handler for the treview.

  @precon  None.
  @postcon Edits the double clicked item.

  @param   Sender as a TObject

**)
Procedure TfmBADISpecialTagsFrame.vstSpecialTagsDblClick(Sender: TObject);

Begin
  btnEditClick(Sender);
End;

(**

  This is an on get text event handler for the view tree.

  @precon  None.
  @postcon returns the text to be shown in each cell.

  @param   Sender   as a TBaseVirtualTree
  @param   Node     as a PVirtualNode
  @param   Column   as a TColumnIndex
  @param   TextType as a TVSTTextType
  @param   CellText as a String as a reference

**)
Procedure TfmBADISpecialTagsFrame.vstSpecialTagsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; Var CellText: String);

Const
  strBoolean : Array[False..True] Of String = ('False', 'True');

Var
  NodeData : PSpecialTagNodeData;
  ST : TBADISpecialTag;
  
Begin
  NodeData := Sender.GetNodeData(Node);
  ST := FSpecialTags[NodeData.FSpecialTagIndex];
  Case TColumnIndexes(Column) Of
    ciName:        CellText := ST.FName;
    ciDescription: CellText := ST.FDescription;
    ciShowInTree:  CellText := strBoolean[tpShowInTree In  ST.FTagProperties];
    ciAutoExpand:  CellText := strBoolean[tpAutoExpand In  ST.FTagProperties];
    ciShowInDocs:  CellText := strBoolean[tpShowInDoc  In  ST.FTagProperties];
    ciFixed:       CellText := strBoolean[tpFixed      In  ST.FTagProperties];
    ciSyntax:      CellText := strBoolean[tpSyntax     In  ST.FTagProperties];  
  End;
End;

(**

  This is an on mouse down event handler for the treeview.

  @precon  None.
  @postcon Toggles the true / false properties of the special tags when the mouse is clicked on them.

  @param   Sender as a TObject
  @param   Button as a TMouseButton
  @param   Shift  as a TShiftState
  @param   X      as an Integer
  @param   Y      as an Integer

**)
Procedure TfmBADISpecialTagsFrame.vstSpecialTagsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

  (**

    This procedure toggles the inclusion / exclusion of the given enumerate in the special tag
    properties.

    @precon  None.
    @postcon The property is togged with the opposite of the given enumerate.

    @param   ST                 as a TBADISpecialTag as a reference
    @param   BADISpecialTagProp as a TBADITagProperty as a constant

  **)
  Procedure ToggleSpecialTagProp(Var ST : TBADISpecialTag; Const BADISpecialTagProp : TBADITagProperty);

  Begin
    If BADISpecialTagProp In ST.FTagProperties Then
      Exclude(ST.FTagProperties, BADISpecialTagProp)
    Else
      Include(ST.FTagProperties, BADISpecialTagProp);
  End;

Var
  NodeData : PSpecialTagNodeData;
  HitInfo : THitInfo;
  ST: TBADISpecialTag;
  
Begin
  FVSTSpecialTags.GetHitTestInfoAt(X, Y, True, HitInfo);
  If Assigned(HitInfo.HitNode) Then
    Begin
      NodeData := FVSTSpecialTags.GetNodeData(HitInfo.HitNode);
      ST := FSpecialTags[NodeData.FSpecialTagIndex];
      Case TColumnIndexes(HitInfo.HitColumn) Of
        ciShowInTree: ToggleSpecialTagProp(ST, tpShowInTree);
        ciAutoExpand: ToggleSpecialTagProp(ST, tpAutoExpand);
        ciShowInDocs: ToggleSpecialTagProp(ST, tpShowInDoc);
        ciFixed:      ToggleSpecialTagProp(ST, tpFixed);
        ciSyntax:     ToggleSpecialTagProp(ST, tpSyntax);
      End;
      FSpecialTags[NodeData.FSpecialTagIndex] := ST;
      FVSTSpecialTags.Invalidate;
    End;
End;

(**

  This is an on paint text event handler for the treeview.

  @precon  None.
  @postcon Paints the text with the font colour, style and back colour specified by the node.

  @param   Sender       as a TBaseVirtualTree
  @param   TargetCanvas as a TCanvas as a constant
  @param   Node         as a PVirtualNode
  @param   Column       as a TColumnIndex
  @param   TextType     as a TVSTTextType

**)
Procedure TfmBADISpecialTagsFrame.vstSpecialTagsPaintText(Sender: TBaseVirtualTree;
  Const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);

Var
  NodeData : PSpecialTagNodeData;
  ST: TBADISpecialTag;
  
Begin
  NodeData := Sender.GetNodeData(Node);
  ST := FSpecialTags[NodeData.FSpecialTagIndex];
  TargetCanvas.Font.Style := ST.FFontStyles;
  TargetCanvas.Font.Color := clWindowText;
  Case TColumnIndexes(Column) Of
    ciShowInTree..ciSyntax: TargetCanvas.Font.Color := clBlack;
  Else
    {$IFDEF DXE102}
    If Assigned(FStyleServices) Then
      TargetCanvas.Font.Color := FStyleServices.GetSystemColor(clWindowText);
    {$ENDIF}
    If ST.FFontColour <> clNone Then
      TargetCanvas.Font.Color := ST.FFontColour;
  End;
End;

End.
