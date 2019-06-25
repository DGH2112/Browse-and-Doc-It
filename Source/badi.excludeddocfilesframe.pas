(**

  This module contains a class which represents a frame interface for excluded document
  files.

  @Version 1.0
  @Author  David Hoyle
  @Date    25 Jun 2019

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
Unit BADI.ExcludedDocFilesFrame;

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
  StdCtrls,
  ImgList,
  VirtualTrees,
  ExtCtrls,
  Themes,
  Generics.Collections,
  BADI.Types,
  BADI.Exclusions,
  BADI.CustomOptionsFrame;

{$INCLUDE CompilerDefinitions.inc}

Type
  (** A class to represent the frame interface. **)
  TfmBADIExcludedDocFilesFrame = Class(TFrame, IBADIOptionsFrame)
    pnlFugePanel: TPanel;
    vstExclusions: TVirtualStringTree;
    btnAdd: TButton;
    btnEdit: TButton;
    btnDelete: TButton;
    ilButtonImages: TImageList;
    procedure vstExclusionsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure vstExclusionsBeforeCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect;
      var ContentRect: TRect);
    procedure vstExclusionsPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);
    procedure vstExclusionsFocusChanged(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex);
    procedure btnAddClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure vstExclusionsMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure vstExclusionsColumnDblClick(Sender: TBaseVirtualTree; Column: TColumnIndex;
      Shift: TShiftState);
  Strict Private
    FExclusions : TList<TBADIExclusionRec>;
    {$IFDEF DXE102}
    FStyleServices : TCustomStyleServices;
    {$ENDIF}
    FCallBackProc : TInputCloseQueryFunc;
    FSelectedItem : String;
  Strict Protected
    // General Methods
    Procedure PopulateExclusions;
  Public
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
    // IBADIOptionsFrame
    Procedure LoadSettings;
    Procedure SaveSettings;
  End;

Implementation

{$R *.dfm}

Uses
  {$IFDEF DEBUG}
  CodeSiteLogging,
  {$ENDIF}
  {$IFNDEF STANDALONEAPP}
  ToolsAPI,
  {$ENDIF}
  RegularExpressions,
  RegularExpressionsCore,
  BADI.Base.Module,
  BADI.Options;

Type
  (** A record to describe the node data. **)
  TBADIExclusionNode = Record
    FIndex : Integer;
  End;
  (** A pointer to the above node data structure. **)
  PBADIExclusionNode = ^TBADIExclusionNode;
  (** An enumerate to describe the fields in the treeview. **)
  TBADIExclusionField = (efPattern, efDocConflicts, efMetrics, efChecks);

ResourceString
  (** A string for the InputQuery dialogue title / caption. **)
  strExclusions = 'Exclusions';
  (**  string for the input query regular expression prompt. **)
  strValidReEx = 'Regular Expression for the exclusion';

(**

  This is an on click event handler for the Add button.

  @precon  None.
  @postcon Display a prompt in which the user can add a regular expression for an exclusion.

  @param   Sender as a TObject

**)
Procedure TfmBADIExcludedDocFilesFrame.btnAddClick(Sender: TObject);

Const
  strDefaultRegExPattern = 'RegExPattern';

Var
  recExclusions : TBADIExclusionRec;
  astrPattern : Array Of String;
  
Begin
  SetLength(astrPattern, 1);
  astrPattern[0] := strDefaultRegExPattern;
  If Dialogs.InputQuery(strExclusions, [strValidReEx],
    astrPattern, FCallBackProc) Then
    Begin
      recExclusions.FExclusionPattern := astrPattern[0];
      recExclusions.FExclusions := [];
      FExclusions.Add(recExclusions);
      PopulateExclusions;
    End;
End;

(**

  This is an on click event handler for the Delete button.

  @precon  None.
  @postcon Deletes the selected exclusion from the list.

  @param   Sender as a TObject

**)
Procedure TfmBADIExcludedDocFilesFrame.btnDeleteClick(Sender: TObject);

Var
  NodeData : PBADIExclusionNode;
  
Begin
  NodeData := vstExclusions.GetNodeData(vstExclusions.FocusedNode);
  FExclusions.Delete(NodeData.FIndex);
  PopulateExclusions;
End;

(**

  This is an on click event handler for the Edit button.

  @precon  None.
  @postcon Display a prompt in which the user can edit a regular expression for an exclusion.

  @param   Sender as a TObject

**)
Procedure TfmBADIExcludedDocFilesFrame.btnEditClick(Sender: TObject);

Var
  NodeData : PBADIExclusionNode;
  astrPattern : Array Of String;
  recExclusion: TBADIExclusionRec;
  
Begin
  NodeData := vstExclusions.GetNodeData(vstExclusions.FocusedNode);
  SetLength(astrPattern, 1);
  recExclusion := FExclusions[NodeData.FIndex];
  astrPattern[0] := recExclusion.FExclusionPattern;
  If Dialogs.InputQuery(strExclusions, [strValidReEx], astrPattern, FCallBackProc) Then
    Begin
      recExclusion.FExclusionPattern := astrPattern[0];
      FExclusions[NodeData.FIndex] := recExclusion;
      PopulateExclusions;
    End;
End;

(**

  A constructor for the TfmBADIExcludedDocFilesFrame class.

  @precon  None.
  @postcon Creates a collection for the excclusions to be displayed.

  @nocheck MissingCONSTInParam

  @param   AOwner as a TComponent
  
**)
Constructor TfmBADIExcludedDocFilesFrame.Create(AOwner: TComponent);

{$IFNDEF STANDALONEAPP}
{$IFDEF DXE102}
Var
  ITS : IOTAIDEThemingServices;
{$ENDIF}
{$ENDIF}

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Create', tmoTiming);{$ENDIF}
  Inherited Create(AOwner);
  FExclusions := TList<TBADIExclusionRec>.Create;
  vstExclusions.NodeDataSize := SizeOf(TBADIExclusionNode);
  {$IFNDEF STANDALONEAPP}
  {$IFDEF DXE102}
  FStyleServices := Nil;
  If Supports(BorlandIDEServices, IOTAIDEThemingServices, ITS) Then
    If ITS.IDEThemingEnabled Then
      FStyleServices := ITS.StyleServices;
  {$ENDIF}
  {$ENDIF}
  FCallBackProc := 
    Function(Const astrPatterns : Array Of String) : Boolean
    ResourceString
      strMsg =
        'Error in regular expression'#13#10 +
        '  Regular Expression: %s'#13#10 +
        '  Message: %s';
    Var
      i : Integer;
      RE : TRegEx;
      strRegEx: String;
    Begin
      Result := True;
      For i := Low(astrPatterns) To High(astrPatterns) Do
        Begin
          Try
            strRegEx := astrPatterns[i];
            RE.Create(strRegEx, [roIgnoreCase, roCompiled, roSingleLine]);
            Result := Result And True;
          Except
            On E : ERegularExpressionError Do
              Begin
                Result := False;
                MessageDlg(Format(strMsg, [strRegEx, E.Message]), mtError, [mbOK], 0);
              End;
          End;
        End;
    End;
  FSelectedItem := '';
End;

(**

  A destructor for the TfmBADIExcludedDocFilesFrame class.

  @precon  None.
  @postcon Frees the memory used for the exclusions.

**)
Destructor TfmBADIExcludedDocFilesFrame.Destroy;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'Destroy', tmoTiming);{$ENDIF}
  FExclusions.Free;
  Inherited Destroy;
End;

(**

  This method loads the frame with the excluded document files from the options.

  @precon  None.
  @postcon The excluded document files list is loaded from the options.

**)
Procedure TfmBADIExcludedDocFilesFrame.LoadSettings;

Var
  i: Integer;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'LoadSettings', tmoTiming);{$ENDIF}
  For i := 0 To TBADIOptions.BADIOptions.Exclusions.Count - 1 Do
    FExclusions.Add(TBADIOptions.BADIOptions.Exclusions[i]);
  PopulateExclusions;
End;

(**

  This method updates the exclusions treeview with the contents of the FExclusions colection.

  @precon  None.
  @postcon The exclusions treeview is updated.

**)
Procedure TfmBADIExcludedDocFilesFrame.PopulateExclusions;

Var
  i: Integer;
  Node: PVirtualNode;
  NodeData : PBADIExclusionNode;
  iSelectedIndex : Integer;

Begin
  vstExclusions.BeginUpdate;
  Try
    iSelectedIndex := -1;
    For i := 0 To FExclusions.Count - 1 Do
      If FSelectedItem = FExclusions[i].FExclusionPattern Then
        iSelectedIndex := i;
    vstExclusions.Clear;
    For i := 0 To FExclusions.Count - 1 Do
      Begin
        Node := vstExclusions.AddChild(Nil);
        NodeData := vstExclusions.GetNodeData(Node);
        NodeData.FIndex := i;
        If iSelectedIndex = i Then
          Begin
            vstExclusions.FocusedNode := Node;
            vstExclusions.Selected[Node] := True;
          End;
      End;
  Finally
    vstExclusions.EndUpdate;
  End;
  vstExclusionsFocusChanged(vstExclusions, vstExclusions.FocusedNode, 0);
End;

(**

  This method saves the frame with the excluded document files to the options.

  @precon  None.
  @postcon The excluded document files list is saved to the options.

**)
Procedure TfmBADIExcludedDocFilesFrame.SaveSettings;

Var
  i : Integer;
  
Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'SaveSettings', tmoTiming);{$ENDIF}
  TBADIOptions.BADIOptions.Exclusions.Clear;
  For i := 0 To FExclusions.Count - 1 Do
    TBADIOptions.BADIOptions.Exclusions.Add(FExclusions[i]);
End;

(**

  This is an on before cell paint event handler for the exclusions treeview.

  @precon  None.
  @postcon Colours the cell background depending on the column and cell content.

  @param   Sender        as a TBaseVirtualTree
  @param   TargetCanvas  as a TCanvas
  @param   Node          as a PVirtualNode
  @param   Column        as a TColumnIndex
  @param   CellPaintMode as a TVTCellPaintMode
  @param   CellRect      as a TRect
  @param   ContentRect   as a TRect as a reference

**)
Procedure TfmBADIExcludedDocFilesFrame.vstExclusionsBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode;
  CellRect: TRect; Var ContentRect: TRect);

Const
  Colour : Array[False..True] Of TColor = ($8080FF, $80FF80);

Var
  NodeData : PBADIExclusionNode;
  recExclusion: TBADIExclusionRec;
  
Begin
  NodeData := vstExclusions.GetNodeData(Node);
  recExclusion := FExclusions[NodeData.FIndex];
  Case TBADIExclusionField(Column) Of
    efDocConflicts: TargetCanvas.Brush.Color := Colour[etDocumentation In recExclusion.FExclusions];
    efMetrics:      TargetCanvas.Brush.Color := Colour[etMetrics In recExclusion.FExclusions];
    efChecks:       TargetCanvas.Brush.Color := Colour[etChecks  In recExclusion.FExclusions];
  Else
    TargetCanvas.Brush.Color := clWindow;
    {$IFDEF DXE102}
    If Assigned(FStyleServices) Then
      TargetCanvas.Brush.Color := FStyleServices.GetSystemColor(clWindow);
    {$ENDIF}
  End;
  TargetCanvas.FillRect(CellRect);
End;

(**

  This method is an on double click event handler for the exclusions treeview.

  @precon  None.
  @postcon Edits the double clicked item.

  @param   Sender as a TBaseVirtualTree
  @param   Column as a TColumnIndex
  @param   Shift  as a TShiftState

**)
Procedure TfmBADIExcludedDocFilesFrame.vstExclusionsColumnDblClick(Sender: TBaseVirtualTree;
  Column: TColumnIndex; Shift: TShiftState);

Begin
  btnEditClick(Nil);
End;

(**

  This method is an on change focus event handler for the exclusions treeview.

  @precon  None.
  @postcon Updates the enabled property of the Edit and Delete buttons.

  @param   Sender as a TBaseVirtualTree
  @param   Node   as a PVirtualNode
  @param   Column as a TColumnIndex

**)
Procedure TfmBADIExcludedDocFilesFrame.vstExclusionsFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);

Begin
  btnEdit.Enabled := Assigned(Node);
  btnDelete.Enabled := Assigned(Node);
  If Assigned(Node) Then
    FSelectedItem := vstExclusions.Text[Node, 0];
End;

(**

  This method is an on get text event handler for the virtual treeview.

  @precon  None.
  @postcon Returns the appropriate text for the given column of data.

  @param   Sender   as a TBaseVirtualTree
  @param   Node     as a PVirtualNode
  @param   Column   as a TColumnIndex
  @param   TextType as a TVSTTextType
  @param   CellText as a String as a reference

**)
Procedure TfmBADIExcludedDocFilesFrame.vstExclusionsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; Var CellText: String);

ResourceString
  strYes = 'Yes';
  strNo = 'No';

Const
  strYesNo : Array[False..True] Of String = (strNo, strYes);

Var
  NodeData : PBADIExclusionNode;
  
Begin
  NodeData := vstExclusions.GetNodeData(Node);
  Case TBADIExclusionField(Column) Of
    efPattern: CellText := FExclusions[NodeData.FIndex].FExclusionPattern;
    efDocConflicts: CellText := strYesNo[etDocumentation in FExclusions[NodeData.FIndex].FExclusions];
    efMetrics: CellText := strYesNo[etMetrics in FExclusions[NodeData.FIndex].FExclusions];
    efChecks: CellText := strYesNo[etChecks in FExclusions[NodeData.FIndex].FExclusions];
  End;
End;

(**

  This method is an on mouse down event handler for the exclusions treeview.

  @precon  None.
  @postcon If the mouse is clicked over one of the exclusion types that type is toggled.

  @param   Sender as a TObject
  @param   Button as a TMouseButton
  @param   Shift  as a TShiftState
  @param   X      as an Integer
  @param   Y      as an Integer

**)
Procedure TfmBADIExcludedDocFilesFrame.vstExclusionsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

  (**

    This procedure toggles the inclusion / exclusion of the given enumerate in the exclusion property.

    @precon  None.
    @postcon The property is togged with the opposite of the given enumerate.

    @param   recExclusion      as a TBADIExclusionRec as a reference
    @param   BADIExclusionType as a TBADIExclusionType as a constant

  **)
  Procedure ToggleExclusion(Var recExclusion: TBADIExclusionRec;
    Const BADIExclusionType: TBADIExclusionType);

  Begin
    If BADIExclusionType In recExclusion.FExclusions Then
      Exclude(recExclusion.FExclusions, BADIExclusionType)
    Else
      Include(recExclusion.FExclusions, BADIExclusionType);
  End;

Var
  NodeData: PBADIExclusionNode;
  HitInfo: THitInfo;
  recExclusion: TBADIExclusionRec;

Begin
  vstExclusions.GetHitTestInfoAt(X, Y, True, HitInfo);
  If Assigned(HitInfo.HitNode) Then
    Begin
      NodeData := vstExclusions.GetNodeData(HitInfo.HitNode);
      recExclusion := FExclusions[NodeData.FIndex];
      Case TBADIExclusionField(HitInfo.HitColumn) Of
        efDocConflicts: ToggleExclusion(recExclusion, etDocumentation);
        efMetrics:      ToggleExclusion(recExclusion, etMetrics);
        efChecks:       ToggleExclusion(recExclusion, etChecks);
      End;
      FExclusions[NodeData.FIndex] := recExclusion;
      vstExclusions.Invalidate;
    End;
End;

(**

  This method is an on paint text event handler for the Exclusion treeview.

  @precon  None.
  @postcon Sets the text colour for the column of data.

  @param   Sender       as a TBaseVirtualTree
  @param   TargetCanvas as a TCanvas as a constant
  @param   Node         as a PVirtualNode
  @param   Column       as a TColumnIndex
  @param   TextType     as a TVSTTextType

**)
Procedure TfmBADIExcludedDocFilesFrame.vstExclusionsPaintText(Sender: TBaseVirtualTree;
  Const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType);

Var
  NodeData : PBADIExclusionNode;
  recExclusion: TBADIExclusionRec;
  
Begin
  NodeData := Sender.GetNodeData(Node);
  recExclusion := FExclusions[NodeData.FIndex];;
  TargetCanvas.Font.Color := clWindowText;
  Case TBADIExclusionField(Column) Of
    efDocConflicts .. efChecks: TargetCanvas.Font.Color := clBlack;
  Else
    {$IFDEF DXE102}
    If Assigned(FStyleServices) Then
      TargetCanvas.Font.Color := FStyleServices.GetSystemColor(clWindowText);
    {$ENDIF}
  End;
End;

End.

