(**

  This module contains a frame for editing the BADI special tags.

  @Version 1.0
  @Author  David Hoyle
  @Date    17 Apr 2017

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
  BADI.Types;

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
    procedure lvSpecialTagsSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvSpecialTagsCustomDrawSubItem(Sender: TCustomListView; Item: TListItem;
      SubItem: Integer; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure lvSpecialTagsDblClick(Sender: TObject);
  Strict Private
    FSpecialTags : TList<TBADISpecialTag>;
  Strict Protected
    Procedure PopulateListView;
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;
    Procedure LoadSettings;
    Procedure SaveSettings;
  End;

Implementation

{$R *.dfm}


Uses
  BADI.Base.Module,
  BADI.SpecialTagForm,
  BADI.Constants,
  BADI.OptionsForm,
  BADI.Options;

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

Begin
  If TfrmSpecialTag.Execute(strName, strDesc, setTagProps) Then
    Begin
      FSpecialTags.Add(TBADISpecialTag.Create(strName, strDesc, setTagProps));
      PopulateListView;
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
      FSpecialTags.Delete(lvSpecialTags.ItemIndex);
      PopulateListView;
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

Begin
  If lvSpecialTags.ItemIndex <> - 1 Then
    Begin
      ST := FSpecialTags[lvSpecialTags.ItemIndex];
      If TfrmSpecialTag.Execute(ST.FName, ST.FDescription, ST.FTagProperties) Then
        Begin
          FSpecialTags[lvSpecialTags.ItemIndex] := ST;
          PopulateListView;
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

Begin
  If lvSpecialTags.ItemIndex < lvSpecialTags.Items.Count - 1 Then
    Begin
      FSpecialTags.Exchange(lvSpecialTags.ItemIndex, lvSpecialTags.ItemIndex + 1);
      lvSpecialTags.ItemIndex := lvSpecialTags.ItemIndex + 1;
      PopulateListView;
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

Begin
  If lvSpecialTags.ItemIndex > 0 Then
    Begin
      FSpecialTags.Exchange(lvSpecialTags.ItemIndex, lvSpecialTags.ItemIndex - 1);
      lvSpecialTags.ItemIndex := lvSpecialTags.ItemIndex - 1;
      PopulateListView;
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
  FSpecialTags := TList<TBADISpecialTag>.Create;
  lvSpecialTagsSelectItem(Nil, Nil, False);
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

  This method renders the list of special tags and options in the list view.

  @precon  None.
  @postcon The list view is populated with the special tags and their options.

**)
Procedure TfmBADISpecialTagsFrame.PopulateListView;

Const
  strBoolean : Array[False..True] Of String = ('False', 'True');

Var
  iSpecialTag: Integer;
  iItemIndex : Integer;
  Item: TListItem;
  setTPOps: TBADITagProperties;

Begin
  lvSpecialTags.Items.BeginUpdate;
  Try
    iItemIndex := lvSpecialTags.ItemIndex;
    lvSpecialTags.Clear;
    For iSpecialTag := 0 To FSpecialTags.Count - 1 Do
      Begin
        Item := lvSpecialTags.Items.Add;
        Item.Caption := FSpecialTags[iSpecialTag].FName;
        Item.SubItems.Add(FSpecialTags[iSpecialTag].FDescription);
        setTPOps := FSpecialTags[iSpecialTag].FTagProperties;
        Item.SubItems.Add(strBoolean[tpShowInTree In setTPOps]);
        Item.SubItems.Add(strBoolean[tpAutoExpand In setTPOps]);
        Item.SubItems.Add(strBoolean[tpShowInDoc In setTPOps]);
        Item.SubItems.Add(strBoolean[tpFixed In setTPOps]);
        Item.SubItems.Add(strBoolean[tpSyntax In setTPOps]);
      End;
    If iItemIndex >= lvSpecialTags.Items.Count Then
      iItemIndex := lvSpecialTags.Items.Count - 1;
    lvSpecialTags.ItemIndex := iItemIndex;
  Finally
    lvSpecialTags.Items.EndUpdate;
  End;
  lvSpecialTagsSelectItem(lvSpecialTags, lvSpecialTags.Selected, lvSpecialTags.Selected <> Nil)
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

  This is an on double click event handler for the special tags listview.

  @precon  None.
  @postcon Invokes editing the selected item.

  @param   Sender as a TObject

**)
Procedure TfmBADISpecialTagsFrame.lvSpecialTagsDblClick(Sender: TObject);

Begin
  btnEditClick(Sender);
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
  ST: TBADISpecialTag;
  eTagProperty: TBADITagProperty;

Begin
  Item := lvSpecialTags.GetItemAt(X, Y);
  If Item <> Nil Then
    Begin
      iLeft := 0;
      Inc(iLeft, lvSpecialTags.Column[0].Width); // Tag Name
      Inc(iLeft, lvSpecialTags.Column[1].Width); // Tag Description
      For iOp := 1 To 5 Do // Subitems 1 to 5
        Begin
          iRight := iLeft + lvSpecialTags.Column[Succ(iOp)].Width;
          If (X >= iLeft) And (X <= iRight) Then
            Begin
              ST := FSpecialTags[Item.Index];
              eTagProperty := TBADITagProperty(Pred(iOp));
              If eTagProperty In ST.FTagProperties Then
                Exclude(ST.FTagProperties, eTagProperty)
              Else
                Include(ST.FTagProperties, eTagProperty);
              FSpecialTags[Item.Index] := ST;
              Break;
            End;
          iLeft := iRight;
        End;
      PopulateListView;
    End;
End;

(**

  This method loads the settings in the frame from the BAID Options class.

  @precon  None.
  @postcon The settings in the frame controls are loaded from the option class.

**)
Procedure TfmBADISpecialTagsFrame.LoadSettings;

Begin
  FSpecialTags.AddRange(TBADIOptions.BADIOptions.SpecialTags);
  PopulateListView;
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

End.
