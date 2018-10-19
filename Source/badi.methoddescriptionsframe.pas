(**

  This module contains a frame for editing the method descrptions.

  @Author  David Hoyle
  @Version 1.0
  @Date    19 Oct 2018

**)
Unit BADI.MethodDescriptionsFrame;

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
  Buttons,
  ComCtrls,
  BADI.CustomOptionsFrame,
  VirtualTrees,
  System.Generics.Collections;

Type
  (** This class represents the frame interface. **)
  TfmBADIMethodDescriptionsFrame = Class(TFrame, IBADIOptionsFrame)
    ilButtonIcons: TImageList;
    btnDeleteDesc: TButton;
    btnEditDesc: TButton;
    btnAddDesc: TButton;
    vstMethodDescriptions: TVirtualStringTree;
    procedure btnAddDescClick(Sender: TObject);
    procedure btnDeleteDescClick(Sender: TObject);
    procedure btnEditDescClick(Sender: TObject);
    procedure vstMethodDescriptionsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
  Strict Private
    Type
      (** A record to describe the pattern / description pairs to be displayed. **)
      TBADIMethodDescRec = Record
        FPattern     : String;
        FDescription : String;
        Constructor Create(Const strPattern, strDescription : String);
      End;
      (** A node record for the treeview with a collection index. @nohints  **)
      TBADINodeData = Record
        FCollectionIndex : Integer;
      End;
      (** A pointer to be above node record. **)
      PBADINodeData = ^TBADINodeData;
  Strict Private
    FMethods : TList<TBADIMethodDescRec>;
  Strict Protected
    Procedure PopulateList;
  Public
    Constructor Create(AOwner : TComponent); Override;
    Destructor Destroy; Override;
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
  BADI.MethodDescriptionForm,
  BADI.Options;

(**

  A constructor for the TBADIMethodDescRec class.

  @precon  None.
  @postcon Initialises the record.

  @param   strPattern     as a String as a constant
  @param   strDescription as a String as a constant

**)
Constructor TfmBADIMethodDescriptionsFrame.TBADIMethodDescRec.Create(Const strPattern,
  strDescription: String);

Begin
  FPattern := strPattern;
  FDescription := strDescription;
End;

(**


  This method is an on click event handler for the Add Description button.

  @precon  None.
  @postcon Aloows the user to add a method description to the list.


  @param   Sender as a TObject

**)
Procedure TfmBADIMethodDescriptionsFrame.btnAddDescClick(Sender: TObject);

Var
  strPattern, strDescription: String;

Begin
  If TfrmMethodDescriptions.Execute(strPattern, strDescription) Then
    Begin
      FMethods.Add(TBADIMethodDescrec.Create(strPattern, strDescription));
      PopulateList;
      vstMethodDescriptions.FocusedNode := vstMethodDescriptions.GetLast();
      vstMethodDescriptions.Selected[vstMethodDescriptions.FocusedNode] := True;
    End;
End;

(**


  This is an on click event handler for the delete description button.

  @precon  None.
  @postcon Delete the selected item from the method description list view.


  @param   Sender as a TObject

**)
Procedure TfmBADIMethodDescriptionsFrame.btnDeleteDescClick(Sender: TObject);

Var
  Node : PVirtualNode;
  NodeData : PBADINodeData;
  
Begin
  Node := vstMethodDescriptions.FocusedNode;
  NodeData := vstMethodDescriptions.GetNodeData(Node);
  FMethods.Delete(NodeData.FCollectionIndex);
  PopulateList;
End;

(**


  This method is an on click event handler for the Edit Description button.

  @precon  None.
  @postcon Allows the user to edit the current method description.


  @param   Sender as a TObject

**)
Procedure TfmBADIMethodDescriptionsFrame.btnEditDescClick(Sender: TObject);

Var
  Node : PVirtualNode;
  NodeData : PBADINodeData;
  R : TBADIMethodDescRec;
  strPattern, strDescription: String;
  
Begin
  Node := vstMethodDescriptions.FocusedNode;
  NodeData := vstMethodDescriptions.GetNodeData(Node);
  R := FMethods[NodeData.FCollectionIndex];
  strPattern := R.FPattern;
  strDescription := R.FDescription;
  If TfrmMethodDescriptions.Execute(strPattern, strDescription) Then
    Begin
      R.FPattern := strPattern;
      R.FDescription := strDescription;
      FMethods[NodeData.FCollectionIndex] := R;
      PopulateList;
    End;
End;

(**

  A constructor for the TfmBADIMethodDescriptionFrame class.

  @precon  None.
  @postcon Creates a generic list to temporarily store the method patterns and descriptions.

  @nocheck MissingCONSTInParam

  @param   AOwner as a TComponent

**)
Constructor TfmBADIMethodDescriptionsFrame.Create(AOwner: TComponent);

Begin
  Inherited Create(AOwner);
  FMethods := TList<TBADIMethodDescRec>.Create;
End;

(**

  A destructor for the TfmBADIMethodDescriptionFrame class.

  @precon  None.
  @postcon Frees the generic collection.

**)
Destructor TfmBADIMethodDescriptionsFrame.Destroy;

Begin
  FMethods.Free;
  Inherited Destroy;
End;

(**

  This method loads the method description options from the BADI options class.

  @precon  None.
  @postcon The method descrption options are loaded into the frame controls.

**)
Procedure TfmBADIMethodDescriptionsFrame.LoadSettings;

Var
  iMethodDesc: Integer;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'LoadSettings', tmoTiming);{$ENDIF}
  For iMethodDesc := 0 To TBADIOptions.BADIOptions.MethodDescriptions.Count - 1 Do
    FMethods.Add(TBADIMethodDescRec.Create(
      TBADIOptions.BADIOptions.MethodDescriptions.Names[iMethodDesc],
      TBADIOptions.BADIOptions.MethodDescriptions.ValueFromIndex[iMethodDesc]
    ));
  PopulateList;
End;

(**

  This method populates the virtual treeview with the list of patterns ands their descriptions.

  @precon  None.
  @postcon The list of patterns and their descriptions are rendered.

**)
Procedure TfmBADIMethodDescriptionsFrame.PopulateList;

Var
  iMethod: Integer;
  Node : PVirtualNode;
  NodeData : PBADINodeData;
  iSelected : Integer;

Begin
  vstMethodDescriptions.BeginUpdate;
  Try
    iSelected := -1;
    If Assigned(vstMethodDescriptions.FocusedNode) Then
      Begin
        NodeData := vstMethodDescriptions.GetNodeData(vstMethodDescriptions.FocusedNode);
        iSelected := NodeData.FCollectionIndex;
      End;
    vstMethodDescriptions.Clear;
    For iMethod := 0 To FMethods.Count - 1 Do
      Begin
        Node := vstMethodDescriptions.AddChild(Nil);
        NodeData := vstMethodDescriptions.GetNodeData(Node);
        NodeData.FCollectionIndex := iMethod;
        If iSelected = iMethod Then
          Begin
            vstMethodDescriptions.FocusedNode := Node;
            vstMethodDescriptions.Selected[Node] := True;
          End;
      End;
  Finally
    vstMethodDescriptions.EndUpdate;
  End;
End;

(**

  This method saves the method description options to the BADI options class.

  @precon  None.
  @postcon The method descrption options are saved from the frame controls.

**)
Procedure TfmBADIMethodDescriptionsFrame.SaveSettings;

Var
  iMethodDesc: Integer;

Begin
  {$IFDEF CODESITE}CodeSite.TraceMethod(Self, 'SaveSettings', tmoTiming);{$ENDIF}
  TBADIOptions.BADIOptions.MethodDescriptions.Clear;
  For iMethodDesc := 0 To FMethods.Count - 1 Do
    TBADIOptions.BADIOptions.MethodDescriptions.Add(Format('%s=%s', [
      FMethods[iMethodDesc].FPattern,
      FMethods[iMethodDesc].FDescription
    ]));
End;

(**

  This is an on get text event handler for the Method Descriptions virtual treeview.

  @precon  None.
  @postcon Returns the text for the columns of data for each record.

  @param   Sender   as a TBaseVirtualTree
  @param   Node     as a PVirtualNode
  @param   Column   as a TColumnIndex
  @param   TextType as a TVSTTextType
  @param   CellText as a string as a reference

**)
procedure TfmBADIMethodDescriptionsFrame.vstMethodDescriptionsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

Var
  NodeData : PBADINodeData;
  
begin
  NodeData := Sender.GetNodeData(Node);
  Case Column Of
    0: CellText := FMethods[NodeData.FCollectionIndex].FPattern;
    1: CellText := FMethods[NodeData.FCollectionIndex].FDescription;
  End;
end;

End.
