(**

  This module contains a frame for editing the BADI special tags.

  @Version 1.0
  @Author  David Hoyle
  @Date    09 Apr 2017

**)
Unit BADI.SpecialTagsFrame;

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
  {$IFDEF DXE100}
  ImageList,
  {$ENDIF}
  ImgList,
  BADI.CustomOptionsFrame;

Type
  (** This is a class to represent the frame interface. **)
  TfmBADISpecialTagsFrame = Class(TFrame, IBADIOptionsFrame)
    lbSpecialTags: TListBox;
    HeaderControl: THeaderControl;
    btnDelete: TBitBtn;
    btnEdit: TBitBtn;
    btnMoveDown: TBitBtn;
    btnMoveUp: TBitBtn;
    CheckedImages: TImageList;
    btnAdd: TBitBtn;
    Procedure lbSpecialTagsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    Procedure lbSpecialTagsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure btnAddClick(Sender: TObject);
    Procedure btnDeleteClick(Sender: TObject);
    Procedure btnEditClick(Sender: TObject);
    Procedure btnMoveUpClick(Sender: TObject);
    Procedure btnMoveDownClick(Sender: TObject);
    Procedure lbSpecialTagsClick(Sender: TObject);
    procedure lbSpecialTagsDblClick(Sender: TObject);
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
  BADI.Base.Module,
  BADI.SpecialTagForm,
  BADI.Constants,
  BADI.OptionsForm,
  BADI.Options;

ResourceString
  (** This is a message to be displayed when a tag is not valid **)
  strInvalidTag = 'This is not a valid tag.';

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
  boolShowInTree, boolExpand, boolShowInDoc: Boolean;

Begin
  If TfrmSpecialTag.Execute(strName, strDesc, boolShowInTree, boolExpand,
    boolShowInDoc) Then
    Begin
      If (strName = '') Or (strDesc = '') Then
        MessageDlg(strInvalidTag, mtWarning, [mbOK], 0)
      Else
        lbSpecialTags.Items.AddObject(Lowercase(strName) + '=' +
          strDesc, TObject(
          Integer(boolShowInTree) * iShowInTree +
          Integer(boolExpand) * iAutoExpand +
          Integer(boolShowInDoc) * iShowinDoc)
          );
      lbSpecialTagsClick(Sender);
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
  If lbSpecialTags.ItemIndex <> - 1 Then
    Begin
      lbSpecialTags.Items.Delete(lbSpecialTags.ItemIndex);
      lbSpecialTagsClick(Sender);
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
  boolShowInTree, boolExpand, boolShowInDoc: Boolean;
  iPos: Integer;

Begin
  If lbSpecialTags.ItemIndex <> - 1 Then
    Begin
      iPos := Pos('=', lbSpecialTags.Items[lbSpecialTags.ItemIndex]);
      strName := Copy(lbSpecialTags.Items[lbSpecialTags.ItemIndex], 1, iPos - 1);
      strDesc := Copy(lbSpecialTags.Items[lbSpecialTags.ItemIndex], iPos + 1,
        Length(lbSpecialTags.Items[lbSpecialTags.ItemIndex]) - iPos);
      boolShowInTree := Integer(lbSpecialTags.Items.Objects[lbSpecialTags.ItemIndex])
        And iShowInTree <> 0;
      boolExpand := Integer(lbSpecialTags.Items.Objects[lbSpecialTags.ItemIndex])
        And iAutoExpand <> 0;
      boolShowInDoc := Integer(lbSpecialTags.Items.Objects[lbSpecialTags.ItemIndex])
        And iShowinDoc <> 0;
      If TfrmSpecialTag.Execute(strName, strDesc, boolShowInTree, boolExpand,
        boolShowInDoc) Then
        Begin
          If (strName = '') Or (strDesc = '') Then
            MessageDlg(strInvalidTag, mtWarning, [mbOK], 0)
          Else
            lbSpecialTags.Items[lbSpecialTags.ItemIndex] := Lowercase(strName) +
              '=' + strDesc;
          lbSpecialTags.Items.Objects[lbSpecialTags.ItemIndex] :=
            TObject(
            Integer(boolShowInTree) * iShowInTree +
            Integer(boolExpand) * iAutoExpand +
            Integer(boolShowInDoc) * iShowinDoc
            );
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
  If lbSpecialTags.ItemIndex < lbSpecialTags.Items.Count - 1 Then
    Begin
      lbSpecialTags.Items.Exchange(lbSpecialTags.ItemIndex,
        lbSpecialTags.ItemIndex + 1);
      lbSpecialTagsClick(Sender);
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
  If lbSpecialTags.ItemIndex > 0 Then
    Begin
      lbSpecialTags.Items.Exchange(lbSpecialTags.ItemIndex,
        lbSpecialTags.ItemIndex - 1);
      lbSpecialTagsClick(Sender);
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
  lbSpecialTagsClick(Nil);
End;

(**

  This is an on click event handler for the special tags list.

  @precon  None.
  @postcon Updates the availability of the 5 buttons under the list.

  @param   Sender as a TObject

**)
Procedure TfmBADISpecialTagsFrame.lbSpecialTagsClick(Sender: TObject);

Begin
  btnEdit.Enabled := lbSpecialTags.ItemIndex > -1;
  btnDelete.Enabled := lbSpecialTags.ItemIndex > -1;
  btnMoveUp.Enabled := lbSpecialTags.ItemIndex > 0;
  btnMoveDown.Enabled := (lbSpecialTags.ItemIndex > -1) And
    (lbSpecialTags.ItemIndex < lbSpecialTags.Items.Count - 1);
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

  This method draws the items in the listbox.

  @precon  Control is the control that called the method, Index is the index of
           the item to be drawn, Rect is the rectangle of the item to be draw,
           and State is the state of the item to be drawn
  @postcon Draw the list boix item with check boxes for options.

  @param   Control as a TWinControl
  @param   Index   as an Integer
  @param   Rect    as a TRect
  @param   State   as a TOwnerDrawState

**)
Procedure TfmBADISpecialTagsFrame.lbSpecialTagsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);

Var
  lb: TListBox;
  iPos: Integer;

Begin
  lb := Control As TListBox;
  lb.Canvas.FillRect(Rect);
  CheckedImages.Draw(lb.Canvas, 37, Rect.Top,
    Integer(lb.Items.Objects[Index]) And iShowInTree);
  CheckedImages.Draw(lb.Canvas, 122, Rect.Top,
    (Integer(lb.Items.Objects[Index]) And iAutoExpand) Div 2);
  CheckedImages.Draw(lb.Canvas, 207, Rect.Top,
    (Integer(lb.Items.Objects[Index]) And iShowinDoc) Div 3);
  iPos := Pos('=', lb.Items[Index]);
  lb.Canvas.TextOut(Rect.Left + 296, Rect.Top, Copy(lb.Items[Index], 1, iPos - 1));
  lb.Canvas.TextOut(Rect.Left + 386, Rect.Top, Copy(lb.Items[Index], iPos + 1,
    Length(lb.Items[Index]) - iPos));
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
Procedure TfmBADISpecialTagsFrame.lbSpecialTagsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

Var
  iIndex: Integer;

Begin
  iIndex := lbSpecialTags.ItemAtPos(Point(X, Y), True);
  If iIndex > - 1 Then
    Begin
      If X In [37 .. 58] Then
        lbSpecialTags.Items.Objects[iIndex] := TObject(
          Integer(lbSpecialTags.Items.Objects[iIndex]) Xor iShowInTree);
      If X In [112 .. 128] Then
        lbSpecialTags.Items.Objects[iIndex] := TObject(
          Integer(lbSpecialTags.Items.Objects[iIndex]) Xor iAutoExpand);
      If X In [192 .. 208] Then
        lbSpecialTags.Items.Objects[iIndex] := TObject(
          Integer(lbSpecialTags.Items.Objects[iIndex]) Xor iShowinDoc);
      lbSpecialTags.Invalidate;
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

Begin
  For j := 0 To TBADIOptions.BADIOptions.SpecialTags.Count - 1 Do
    lbSpecialTags.Items.AddObject(TBADIOptions.BADIOptions.SpecialTags[j],
      TBADIOptions.BADIOptions.SpecialTags.Objects[j]);
End;

(**

  This method saves the settings in the frame back to the BAID Options class.

  @precon  None.
  @postcon The settings in the frame controls are saved to the option class.

**)
Procedure TfmBADISpecialTagsFrame.SaveSettings;

Var
  j: Integer;

Begin
  TBADIOptions.BADIOptions.SpecialTags.Clear;
  For j := 0 To lbSpecialTags.Items.Count - 1 Do
    TBADIOptions.BADIOptions.SpecialTags.AddObject(lbSpecialTags.Items[j],
      lbSpecialTags.Items.Objects[j]);
End;

End.
