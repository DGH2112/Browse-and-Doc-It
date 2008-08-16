(**

  This module provides an enumerate set for the visible display options and
  a dialogue for setting those options.

  @Date    16 Aug 2008
  @Version 1.0
  @Author  David Hoyle

**)
unit OptionsForm;

interface

uses
{$WARN UNIT_PLATFORM OFF} // For the FileCtrl unit
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls, CheckLst, ImgList, FileCtrl,
  BaseLanguageModule, PascalDocModule;
{$WARN UNIT_PLATFORM ON}

type
  (** This class represents an options dialogue where the user can change the
      display options of the application. **)
  TfrmOptions = class(TForm)
    bbtnOK: TBitBtn;
    bbtnCancel: TBitBtn;
    OptionTab: TPageControl;
    Page1: TTabSheet;
    clbOptions: TCheckListBox;
    Page2: TTabSheet;
    lbSpecialTags: TListBox;
    IntervalPanel: TPanel;
    lblRefreshInterval: TLabel;
    TagPanel: TPanel;
    HeaderControl1: THeaderControl;
    btnAdd: TBitBtn;
    btnDelete: TBitBtn;
    btnMoveUp: TBitBtn;
    btnMoveDown: TBitBtn;
    btnEdit: TBitBtn;
    HelpFilePage: TTabSheet;
    HelpFileDir: TDirectoryListBox;
    edtUpdateInterval: TEdit;
    udUpdateInterval: TUpDown;
    CheckedImages: TImageList;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    lblFontName: TLabel;
    cbxFontName: TComboBox;
    lblFontSize: TLabel;
    edtFontSize: TEdit;
    udFontSize: TUpDown;
    lbxTokenTypes: TListBox;
    lblTokenTypes: TLabel;
    cbxFontColour: TColorBox;
    gbxFontStyles: TGroupBox;
    chkBold: TCheckBox;
    chkItalic: TCheckBox;
    chkUnderline: TCheckBox;
    chkStrikeout: TCheckBox;
    rgpBrowsePosition: TRadioGroup;
    tabExcludeDocFiles: TTabSheet;
    mmoExcludeDocFiles: TMemo;
    tabMethodDescriptions: TTabSheet;
    lvMethodDescriptions: TListView;
    btnAddDesc: TBitBtn;
    btnEditDesc: TBitBtn;
    btnDeleteDesc: TBitBtn;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure lbSpecialTagsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
    procedure lbxTokenTypesClick(Sender: TObject);
    procedure cbxFontColourChange(Sender: TObject);
    procedure chkBoldClick(Sender: TObject);
    procedure chkItalicClick(Sender: TObject);
    procedure chkUnderlineClick(Sender: TObject);
    procedure chkStrikeoutClick(Sender: TObject);
    procedure lbSpecialTagsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnDeleteDescClick(Sender: TObject);
    procedure btnAddDescClick(Sender: TObject);
    procedure btnEditDescClick(Sender: TObject);
    { Private declarations }
  Private
    FTokenFontInfo : Array[Low(TTokenType)..High(TTokenType)] Of TTokenFontInfo;
  public
    { Public declarations }
    Class Function Execute : Boolean;
  end;

implementation

Uses
  SpecialTagForm, ModuleExplorerFrame, MethodDescriptionForm;

ResourceString
  (** This is a message to be displayed when a tag is not valid **)
  strInvalidTag = 'This is not a valid tag.';

{$R *.DFM}

(**

  This method creates an instance of the options dialogue and sets all the
  controls based on the passed Options parameter. If OK is selected then
  the Options parameter is updated to suit the new options.

  @precon  iInt is the timer interval to be represented in the dialogue,
           DocHelpFile is the directory of the modules help file.
  @postcon Returns true if the OK button on the dialogue was pressed.

  @return  a Boolean

**)
Class Function TfrmOptions.Execute : Boolean;

Var
  i : TDocOption;
  j : Integer;
  k : TTokenType;
  Item: TListItem;

Begin
  Result := False;
  With TfrmOptions.Create(Nil) Do
    Try
      For i := Low(TDocOption) To High(TDocOption) Do
        Begin
          clbOptions.Items.Add(DocOptionInfo[i].FDescription);
          clbOptions.Checked[Integer(i)] := i In BrowseAndDocItOptions.Options;
        End;
      For j := 0 To BrowseAndDocItOptions.SpecialTags.Count - 1 Do
        lbSpecialTags.Items.AddObject(BrowseAndDocItOptions.SpecialTags[j],
          BrowseAndDocItOptions.SpecialTags.Objects[j]);
      udUpdateInterval.Position := BrowseAndDocItOptions.UpdateInterval;
      If FileExists(BrowseAndDocItOptions.DocHelpFile) Then
        HelpFileDir.Directory := BrowseAndDocItOptions.DocHelpFile;
      For j := 0 To cbxFontName.Items.Count - 1 Do
        If cbxFontName.Items[j] = BrowseAndDocItOptions.FontName Then
          Begin
            cbxFontName.ItemIndex := j;
            Break;
          End;
      For k := Low(TTokenType) to High(TTokenType) Do
        FTokenFontInfo[k] := BrowseAndDocItOptions.TokenFontInfo[k];
      udFontSize.Position := BrowseAndDocItOptions.FontSize;
      rgpBrowsePosition.ItemIndex := Integer(BrowseAndDocItOptions.BrowsePosition);
      mmoExcludeDocFiles.Text := BrowseAndDocItOptions.ExcludeDocFiles.Text;
      For j := 0 To BrowseAndDocItOptions.MethodDescriptions.Count - 1 Do
        Begin
          Item := lvMethodDescriptions.Items.Add;
          Item.Caption := BrowseAndDocItOptions.MethodDescriptions.Names[j];
          Item.SubItems.Add(BrowseAndDocItOptions.MethodDescriptions.ValueFromIndex[j]);
        End;
      If ShowModal = mrOK Then
        Begin
          Result := True;
          BrowseAndDocItOptions.Options := [];
          For i := Low(TDocOption) To High(TDocOption) Do
            If clbOptions.Checked[Integer(i)] Then
              BrowseAndDocItOptions.Options := BrowseAndDocItOptions.Options + [i];
          BrowseAndDocItOptions.SpecialTags.Clear;
          For j := 0 To lbSpecialTags.Items.Count - 1 Do
            BrowseAndDocItOptions.SpecialTags.AddObject(lbSpecialTags.Items[j],
            lbSpecialTags.Items.Objects[j]);
          BrowseAndDocItOptions.UpdateInterval := udUpdateInterval.Position;
          BrowseAndDocItOptions.DocHelpFile := HelpFileDir.Directory;
          BrowseAndDocItOptions.FontName := cbxFontName.Text;
          BrowseAndDocItOptions.FontSize := udFontSize.Position;
          For k := Low(TTokenType) to High(TTokenType) Do
            BrowseAndDocItOptions.TokenFontInfo[k] := FTokenFontInfo[k];
          BrowseAndDocItOptions.BrowsePosition := TBrowsePosition(rgpBrowsePosition.ItemIndex);
          BrowseAndDocItOptions.ExcludeDocFiles.Text := mmoExcludeDocFiles.Text;
          BrowseAndDocItOptions.MethodDescriptions.Clear;
          For j := 0 To lvMethodDescriptions.Items.Count - 1 Do
            BrowseAndDocItOptions.MethodDescriptions.Add(Format('%s=%s', [
              lvMethodDescriptions.Items[j].Caption,
              lvMethodDescriptions.Items[j].SubItems[0]]));
          BrowseAndDocItOptions.SaveSettings;
        End;
    Finally
      Free;
    End;
End;

(**

  This is an on create event handler for the form.

  @precon  None.
  @postcon Initialises the font names drop down with font names.

  @param   Sender as a TObject

**)
procedure TfrmOptions.FormCreate(Sender: TObject);

Var
  i : Integer;
  j : TTokenType;

begin
  For i := 0 To Screen.Fonts.Count - 1 Do
    cbxFontName.Items.Add(Screen.Fonts[i]);
  For j := Low(TTokenType) to High(TTokenType) Do
    lbxTokenTypes.Items.Add(strTokenType[j]);
  lbxTokenTypes.ItemIndex := 0;
end;

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
procedure TfrmOptions.lbSpecialTagsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);

Var
  lb : TListBox;
  iPos : Integer;

begin
  lb := Control As TListBox;
  lb.Canvas.FillRect(Rect);
  CheckedImages.Draw(lb.Canvas, 32, Rect.Top,
    Integer(lb.Items.Objects[Index]) And iShowInTree);
  CheckedImages.Draw(lb.Canvas, 112, Rect.Top,
    (Integer(lb.Items.Objects[Index]) And iAutoExpand) Div 2);
  iPos := Pos('=', lb.Items[Index]);
  lb.Canvas.TextOut(Rect.Left + 160, Rect.Top, Copy(lb.Items[Index], 1, iPos - 1));
  lb.Canvas.TextOut(Rect.Left + 260, Rect.Top, Copy(lb.Items[Index], iPos + 1,
    Length(lb.Items[Index]) - iPos));
end;

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
procedure TfrmOptions.lbSpecialTagsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

Var
  iIndex: Integer;

begin
  iIndex := lbSpecialTags.ItemAtPos(Point(X, Y), True);
  If iIndex > -1 Then
    Begin
      If X In [32..48] Then
        lbSpecialTags.Items.Objects[iIndex] := TObject(
          Integer(lbSpecialTags.Items.Objects[iIndex]) Xor iShowInTree);
      If X In [112..128] Then
        lbSpecialTags.Items.Objects[iIndex] := TObject(
          Integer(lbSpecialTags.Items.Objects[iIndex]) Xor iAutoExpand);
      lbSpecialTags.Invalidate;
    End;
end;

(**

  This is an on click event handler for the token type list box control.

  @precon  None.
  @postcon Sets the Font Colour and style controls.

  @param   Sender as a TObject

**)
procedure TfrmOptions.lbxTokenTypesClick(Sender: TObject);
begin
  With lbxTokenTypes Do
    If ItemIndex > -1 Then
      Begin
        cbxFontColour.Selected := FTokenFontInfo[TTokenType(itemIndex)].FColour;
        chkBold.Checked := fsBold In FTokenFontInfo[TTokenType(itemIndex)].FStyles;
        chkItalic.Checked := fsItalic In FTokenFontInfo[TTokenType(itemIndex)].FStyles;
        chkUnderline.Checked := fsUnderline In FTokenFontInfo[TTokenType(itemIndex)].FStyles;
        chkStrikeout.Checked := fsStrikeout In FTokenFontInfo[TTokenType(itemIndex)].FStyles;
      End;
end;

(**

  This is a TButton on click event. It allows the user to add a new tag.

  @precon  Sender is the control that invoked the event.
  @postcon Adds a tag to the list.

  @param   Sender as a TObject

**)
procedure TfrmOptions.btnAddClick(Sender: TObject);

Var
  strName, strDesc : String;
  boolShow, boolExpand : Boolean;

begin
  If TfrmSpecialTag.Execute(strName, strDesc, boolShow, boolExpand) Then
    Begin
      If (strName = '') Or (strDesc = '') Then
        MessageDlg(strInvalidTag, mtWarning, [mbOK], 0)
      Else
        lbSpecialTags.Items.AddObject(Lowercase(strName) + '=' +
          strDesc, TObject(Integer(boolShow) * iShowInTree +
          Integer(boolExpand) * iAutoExpand));
    End;
end;

(**

  This method is an on click event handler for the Add Description button.

  @precon  None.
  @postcon Aloows the user to add a method description to the list.

  @param   Sender as a TObject

**)
procedure TfrmOptions.btnAddDescClick(Sender: TObject);

Var
  strPattern, strDescription : String;
  Item: TListItem;

begin
  If TfrmMethodDescriptions.Execute(strPattern, strDescription) Then
    Begin
      Item := lvMethodDescriptions.Items.Add;
      Item.Caption := strPattern;
      Item.SubItems.Add(strDescription);
    End;
end;

(**

  This is a TButton on click event. It allows the user to delete a tag from
  the list.

  @precon  Sender is the control that invoked the event.
  @postcon The method deletes the selected tag from the list.

  @param   Sender as a TObject

**)
procedure TfrmOptions.btnDeleteClick(Sender: TObject);
begin
  If lbSpecialTags.ItemIndex <> -1 Then
    lbSpecialTags.Items.Delete(lbSpecialTags.ItemIndex);
end;

(**

  This is an on click event handler for the delete description button.

  @precon  None.
  @postcon Delete the selected item from the method description list view.

  @param   Sender as a TObject

**)
procedure TfrmOptions.btnDeleteDescClick(Sender: TObject);
begin
  If lvMethodDescriptions.ItemIndex > -1 Then
    lvMethodDescriptions.Items.Delete(lvMethodDescriptions.ItemIndex);
end;

(**

  This is a TButton on click event. It allows the user to edit a tag in the
  list.

  @precon  Sender is the control that invoked the event.
  @postcon Allows the user to edit the selected tag.

  @param   Sender as a TObject

**)
procedure TfrmOptions.btnEditClick(Sender: TObject);

Var
  strName, strDesc : String;
  boolShow, boolExpand : Boolean;
  iPos : Integer;

begin
  If lbSpecialTags.ItemIndex <> -1 Then
    Begin
      iPos := Pos('=', lbSpecialTags.Items[lbSpecialTags.ItemIndex]);
      strName := Copy(lbSpecialTags.Items[lbSpecialTags.ItemIndex], 1, iPos - 1);
      strDesc := Copy(lbSpecialTags.Items[lbSpecialTags.ItemIndex], iPos + 1,
        Length(lbSpecialTags.Items[lbSpecialTags.ItemIndex]) - iPos);
      boolShow := Integer(lbSpecialTags.Items.Objects[lbSpecialTags.ItemIndex])
        And iShowInTree <> 0;
      boolExpand := Integer(lbSpecialTags.Items.Objects[lbSpecialTags.ItemIndex])
        And iAutoExpand <> 0;
      If TfrmSpecialTag.Execute(strName, strDesc, boolShow, boolExpand) Then
        Begin
          If (strName = '') Or (strDesc = '') Then
            MessageDlg(strInvalidTag, mtWarning, [mbOK], 0)
          Else
            lbSpecialTags.Items[lbSpecialTags.ItemIndex] := Lowercase(strName) +
              '=' + strDesc;
            lbSpecialTags.Items.Objects[lbSpecialTags.ItemIndex] :=
              TObject(Integer(boolShow) * iShowInTree +
              Integer(boolExpand) * iAutoExpand);
        End;
    End;
end;

(**

  This method is an on click event handler for the Edit Description button.

  @precon  None.
  @postcon Allows the user to edit the current method description.

  @param   Sender as a TObject

**)
procedure TfrmOptions.btnEditDescClick(Sender: TObject);

Var
  strPattern, strDescription : String;
  iIndex: Integer;

begin
  iIndex := lvMethodDescriptions.ItemIndex;
  If iIndex > -1 Then
    Begin
      strPattern := lvMethodDescriptions.Items[iIndex].Caption;
      strDescription := lvMethodDescriptions.Items[iIndex].SubItems[0];
      If TfrmMethodDescriptions.Execute(strPattern, strDescription) Then
        lvMethodDescriptions.Items[iIndex].Caption := strPattern;
        lvMethodDescriptions.Items[iIndex].SubItems[0] := strDescription;
    End;
end;

(**

  This is a TButton on click event. It allows the user to move the item up the
  list.

  @precon  Sender is the control that invoked the event.
  @postcon Moves the selected tag up the list.

  @param   Sender as a TObject

**)
procedure TfrmOptions.btnMoveUpClick(Sender: TObject);
begin
  If lbSpecialTags.ItemIndex > 0 Then
    lbSpecialTags.Items.Exchange(lbSpecialTags.ItemIndex,
      lbSpecialTags.ItemIndex - 1);
end;

(**

  This is an on change event handler for the Font Colour control.

  @precon  None.
  @postcon Updates the internal list of Token Font Information.

  @param   Sender as a TObject

**)
procedure TfrmOptions.cbxFontColourChange(Sender: TObject);
begin
  FTokenFontInfo[TTokenType(lbxTokenTypes.ItemIndex)].FColour := cbxFontColour.Selected;
end;

(**

  This is an on click event handler for the bold check box.

  @precon  None.
  @postcon Includes or Excludes the Bold option in the token font info style.

  @param   Sender as a TObject

**)
procedure TfrmOptions.chkBoldClick(Sender: TObject);
begin
  If chkBold.Checked Then
    Include(FTokenFontInfo[TTokenType(lbxTokenTypes.ItemIndex)].FStyles, fsBold)
  Else
    Exclude(FTokenFontInfo[TTokenType(lbxTokenTypes.ItemIndex)].FStyles, fsBold);
end;

(**

  This is an on click event handler for the italic check box.

  @precon  None.
  @postcon Includes or Excludes the Italic option in the token font info style.

  @param   Sender as a TObject

**)
procedure TfrmOptions.chkItalicClick(Sender: TObject);
begin
  If chkItalic.Checked Then
    Include(FTokenFontInfo[TTokenType(lbxTokenTypes.ItemIndex)].FStyles, fsItalic)
  Else
    Exclude(FTokenFontInfo[TTokenType(lbxTokenTypes.ItemIndex)].FStyles, fsItalic);
end;

(**

  This is an on click event handler for the Strikeout check box.

  @precon  None.
  @postcon Includes or Excludes the Strikeout option in the token font info style.

  @param   Sender as a TObject

**)
procedure TfrmOptions.chkStrikeoutClick(Sender: TObject);
begin
  If chkStrikeout.Checked Then
    Include(FTokenFontInfo[TTokenType(lbxTokenTypes.ItemIndex)].FStyles, fsStrikeOut)
  Else
    Exclude(FTokenFontInfo[TTokenType(lbxTokenTypes.ItemIndex)].FStyles, fsStrikeOut);
end;

(**

  This is an on click event handler for the Underline check box.

  @precon  None.
  @postcon Includes or Excludes the Underline option in the token font info style.

  @param   Sender as a TObject

**)
procedure TfrmOptions.chkUnderlineClick(Sender: TObject);
begin
  If chkUnderline.Checked Then
    Include(FTokenFontInfo[TTokenType(lbxTokenTypes.ItemIndex)].FStyles, fsUnderline)
  Else
    Exclude(FTokenFontInfo[TTokenType(lbxTokenTypes.ItemIndex)].FStyles, fsUnderline);
end;

(**

  This is a TButton on click event. It allows the user to move the item down
  the list.

  @precon  Sender is the control that invoked the event.
  @postcon Move the selected tag down the list.

  @param   Sender as a TObject

**)
procedure TfrmOptions.btnMoveDownClick(Sender: TObject);
begin
  If lbSpecialTags.ItemIndex < lbSpecialTags.Items.Count - 1 Then
    lbSpecialTags.Items.Exchange(lbSpecialTags.ItemIndex,
      lbSpecialTags.ItemIndex + 1);
end;

end.
