(**

  This module contains a frame for editing the BADI special tags.

  @Version 1.0
  @Author  David Hoyle
  @Date    13 Apr 2017

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
  {$IFDEF DXE100}
  ImageList,
  {$ENDIF}
  ImgList,
  BADI.CustomOptionsFrame;

Type
  (** This is a class to represent the frame interface. **)
  TfmBADISpecialTagsFrame = Class(TFrame, IBADIOptionsFrame)
    btnDelete: TBitBtn;
    btnEdit: TBitBtn;
    btnMoveDown: TBitBtn;
    btnMoveUp: TBitBtn;
    btnAdd: TBitBtn;
    lvSpecialTags: TListView;
    Procedure lvSpecialTagsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure btnAddClick(Sender: TObject);
    Procedure btnDeleteClick(Sender: TObject);
    Procedure btnEditClick(Sender: TObject);
    Procedure btnMoveUpClick(Sender: TObject);
    Procedure btnMoveDownClick(Sender: TObject);
    procedure lbSpecialTagsDblClick(Sender: TObject);
    procedure lvSpecialTagsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvSpecialTagsCustomDrawSubItem(Sender: TCustomListView; Item: TListItem;
      SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean);
  Private
    { Private declarations }
  Public
    { Public declarations }
    Constructor Create(AOwner: TComponent); Override;
    Procedure LoadSettings;
    Procedure SaveSettings;
  End;

Implementation

{$R *.dfm}


Uses
  CodeSiteLogging,
  BADI.Base.Module,
  BADI.SpecialTagForm,
  BADI.Constants,
  BADI.OptionsForm,
  BADI.Options, BADI.Types;

ResourceString
  (** This is a message to be displayed when a tag is not valid **)
  strInvalidTag = 'This is not a valid tag.';

Const
  (** A constant array of boolean strings for displaying option settings. **)
  strBoolean : Array[False..True] Of String = ('False', 'True');

{ TfmBADISpecialTagsFrame }

(**

  This is a TButton on click event. It allows the user to add a new tag.

  @precon  Sender is the control that invoked the event.
  @postcon Adds a tag to the list.

  @param   Sender as a TObject

**)
Procedure TfmBADISpecialTagsFrame.btnAddClick(Sender: TObject);

Var
  strName, strDesc: String;
  setTagProps : TBADITagProperties;
  Item: TListItem;

Begin
  Item := Nil;
  If TfrmSpecialTag.Execute(strName, strDesc, setTagProps) Then
    Begin
      If (strName = '') Or (strDesc = '') Then
        MessageDlg(strInvalidTag, mtWarning, [mbOK], 0)
      Else
        Begin
          Item := lvSpecialTags.Items.Add;
          Item.Caption := LowerCase(strName);
          Item.SubItems.Add(strDesc);
          Item.SubItems.Add(strBoolean[tpShowInTree In setTagProps]);
          Item.SubItems.Add(strBoolean[tpAutoExpand In setTagProps]);
          Item.SubItems.Add(strBoolean[tpShowInDoc In setTagProps]);
          Item.SubItems.Add(strBoolean[tpFixed In setTagProps]);
          lvSpecialTags.Selected := Item;
        End;
      lvSpecialTagsSelectItem(Sender, Item, Item <> Nil);
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

Begin
  If lvSpecialTags.ItemIndex <> - 1 Then
    Begin
      lvSpecialTags.Items.Delete(lvSpecialTags.ItemIndex);
      lvSpecialTagsSelectItem(Sender, Nil, False);
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
  strName, strDesc: String;
  setTPOps: TBADITagProperties;
  Item: TListItem;

Begin
  If lvSpecialTags.ItemIndex <> - 1 Then
    Begin
      Item := lvSpecialTags.Selected;
      strName := Item.Caption;
      strDesc := Item.SubItems[0];
      setTPOps := [];
      If StrToBool(Item.SubItems[1]) Then
        Include(setTPOps, tpShowInTree);
      If StrToBool(Item.SubItems[2]) Then
        Include(setTPOps, tpAutoExpand);
      If StrToBool(Item.SubItems[3]) Then
        Include(setTPOps, tpShowInDoc);
      If StrToBool(Item.SubItems[4]) Then
        Include(setTPOps, tpFixed);
      If TfrmSpecialTag.Execute(strName, strDesc, setTPOps) Then
        Begin
          If (strName = '') Or (strDesc = '') Then
            MessageDlg(strInvalidTag, mtWarning, [mbOK], 0)
          Else
            Begin
              Item.Caption := strName;
              Item.SubItems[0] := strDesc;
              Item.SubItems[1] := strBoolean[tpShowInTree In setTPOps];
              Item.SubItems[2] := strBoolean[tpAutoExpand In setTPOps];
              Item.SubItems[3] := strBoolean[tpShowInDoc In setTPOps];
              Item.SubItems[4] := strBoolean[tpFixed In setTPOps];
            End;
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
  Item: TListItem;

Begin
  If lvSpecialTags.ItemIndex < lvSpecialTags.Items.Count - 1 Then
    Begin
      lvSpecialTags.Items.BeginUpdate;
      Try
        Item := lvSpecialTags.Items.Insert(lvSpecialTags.ItemIndex + 2);
        Item.Caption := lvSpecialTags.Selected.Caption;
        Item.SubItems.Assign(lvSpecialTags.Selected.SubItems);
        lvSpecialTags.Items.Delete(lvSpecialTags.ItemIndex);
        lvSpecialTags.Selected := Item;
      Finally
        lvSpecialTags.Items.EndUpdate;
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
  Item: TListItem;

Begin
  If lvSpecialTags.ItemIndex > 0 Then
    Begin
      lvSpecialTags.Items.BeginUpdate;
      Try
        Item := lvSpecialTags.Items.Insert(lvSpecialTags.ItemIndex - 1);
        Item.Caption := lvSpecialTags.Selected.Caption;
        Item.SubItems.Assign(lvSpecialTags.Selected.SubItems);
        lvSpecialTags.Items.Delete(lvSpecialTags.ItemIndex);
        lvSpecialTags.Selected := Item;
      Finally
        lvSpecialTags.Items.EndUpdate;
      End;
    End;
End;

(**

  A constructor for the TfmBADISpecialTagsFrame class.

  @precon  None.
  @postcon Updates the status of the buttons on the form.

  @param   AOwner as a TComponent

**)
Constructor TfmBADISpecialTagsFrame.Create(AOwner: TComponent);

Begin
  Inherited Create(AOwner);
  lvSpecialTagsSelectItem(Nil, Nil, False);
End;

(**

  This method enables or disables the buttons depending upon the selection.

  @precon  None.
  @postcon The buttons are enabled or disabled depending upon the selection.

  @param   Sender   as a TObject
  @param   Item     as a TListItem
  @param   Selected as a Boolean

**)
Procedure TfmBADISpecialTagsFrame.lvSpecialTagsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);

Begin
  btnDelete.Enabled := Item <> Nil;
  btnEdit.Enabled := Item <> Nil;
  btnMoveDown.Enabled := (Item <> Nil) And (Item.Index > -1) And
    (Item.Index < lvSpecialTags.Items.Count - 1);
  btnMoveUp.Enabled := (Item <> Nil) And (Item.Index > 0);
End;

(**

  This is an on double click event handler for the list of special tags.

  @precon  None.
  @postcon On double clicking an item it is opened for editing.

  @param   Sender as a TObject

**)
Procedure TfmBADISpecialTagsFrame.lbSpecialTagsDblClick(Sender: TObject);

Begin
  btnEditClick(Sender);
End;

(**

  This method highlights the boolean options with a green background for true and red background for
  false options to make them stand out.

  @precon  None.
  @postcon The boolean options are highlighted.

  @param   Sender      as a TCustomListView
  @param   Item        as a TListItem
  @param   SubItem     as an Integer
  @param   State       as a TCustomDrawState
  @param   DefaultDraw as a Boolean as a reference

**)
Procedure TfmBADISpecialTagsFrame.lvSpecialTagsCustomDrawSubItem(Sender: TCustomListView;
  Item: TListItem; SubItem: Integer; State: TCustomDrawState; Var DefaultDraw: Boolean);

Begin
  Sender.Canvas.Brush.Color := clWindow;
  If SubItem > 1 Then
    If StrToBool(Item.SubItems[Pred(SubItem)]) Then
      Sender.Canvas.Brush.Color := $80FF80
    Else
      Sender.Canvas.Brush.Color := $8080FF;
End;

(**


  This is an on mouse down event handler for the special tags list box.

  @precon  None.
  @postcon Allows th euser to enabled/diaable items by clicking on them.


  @param   Sender as a TObject
  @param   Button as a TMouseButton
  @param   Shift  as a TShiftState
  @param   X      as an Integer
  @param   Y      as an Integer

**)
Procedure TfmBADISpecialTagsFrame.lvSpecialTagsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

Var
  Item: TListItem;
  iLeft, iRight : Integer;
  iOp: Integer;
  boolOp: Boolean;

Begin
  Item := lvSpecialTags.GetItemAt(X, Y);
  If Item <> Nil Then
    Begin
      iLeft := 0;
      Inc(iLeft, lvSpecialTags.Column[0].Width); // Tag Name
      Inc(iLeft, lvSpecialTags.Column[1].Width); // Tag Description
      For iOp := 1 To 4 Do // Subitems 1 to 4
        Begin
          iRight := iLeft + lvSpecialTags.Column[Succ(iOp)].Width;
          If (X >= iLeft) And (X <= iRight) Then
            Begin
              boolOp := StrToBool(Item.SubItems[iOp]);
              Item.SubItems[iOp] := strBoolean[Not boolOp];
              Break;
            End;
          iLeft := iRight;
        End;
    End;
End;

(**

  This method loads the settings in the frame from the BAID Options class.

  @precon  None.
  @postcon The settings in the frame controls are loaded from the option class.

**)
Procedure TfmBADISpecialTagsFrame.LoadSettings;

Var
  j: Integer;
  Item: TListItem;
  setTPOps: TBADITagProperties;

Begin
  For j := 0 To TBADIOptions.BADIOptions.SpecialTags.Count - 1 Do
    Begin
      Item := lvSpecialTags.Items.Add;
      Item.Caption := TBADIOptions.BADIOptions.SpecialTags.Names[j];
      Item.SubItems.Add(TBADIOptions.BADIOptions.SpecialTags.ValueFromIndex[j]);
      setTPOps := TBADITagProperties(Byte(TBADIOptions.BADIOptions.SpecialTags.Objects[j]));
      Item.SubItems.Add(strBoolean[tpShowInTree In setTPOps]);
      Item.SubItems.Add(strBoolean[tpAutoExpand In setTPOps]);
      Item.SubItems.Add(strBoolean[tpShowInDoc In setTPOps]);
      Item.SubItems.Add(strBoolean[tpFixed In setTPOps]);
    End;
End;

(**

  This method saves the settings in the frame back to the BAID Options class.

  @precon  None.
  @postcon The settings in the frame controls are saved to the option class.

**)
Procedure TfmBADISpecialTagsFrame.SaveSettings;

Var
  sl : TStringList;
  j: Integer;
  Item: TListItem;
  setTPOps: TBADITagProperties;

Begin
  sl := TBADIOptions.BADIOptions.SpecialTags;
  sl.Clear;
  For j := 0 To lvSpecialTags.Items.Count - 1 Do
    Begin
      Item := lvSpecialTags.Items[j];
      setTPOps := [];
      If StrToBool(Item.SubItems[1]) Then
        Include(setTPOps, tpShowInTree);
      If StrToBool(Item.SubItems[2]) Then
        Include(setTPOps, tpAutoExpand);
      If StrToBool(Item.SubItems[3]) Then
        Include(setTPOps, tpShowInDoc);
      If StrToBool(Item.SubItems[4]) Then
        Include(setTPOps, tpFixed);
      sl.AddObject(Item.Caption + '=' + Item.SubItems[0], TObject(Byte(setTPOps)));
    End;
End;

End.
