(**

  This module provides an enumerate set for the visible display options and
  a dialogue for setting those options.

  @Date    13 Aug 2010
  @Version 1.0
  @Author  David Hoyle

**)
unit OptionsForm;

interface

uses
{$WARN UNIT_PLATFORM OFF} // For the FileCtrl unit
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls, CheckLst, ImgList, FileCtrl,
  BaseLanguageModule;
{$WARN UNIT_PLATFORM ON}

type
  (** An enumerate to define the visisble tabs in the dialogue. **)
  TVisibleTab = (vtGeneralOptions, vtSpecialTags, vtModuleExplorer,
    vtCodeBrowsing, vtExcludeDocFiles, vtMethodDescriptions);
  (** A set of visible tabs. **)
  TVisibleTabs = Set of TVisibleTab;

  (** This class represents an options dialogue where the user can change the
      display options of the application. **)
  TfrmOptions = class(TForm)
    bbtnOK: TBitBtn;
    bbtnCancel: TBitBtn;
    OptionTab: TPageControl;
    tabGeneralOptions: TTabSheet;
    clbOptions: TCheckListBox;
    tabSpecialTags: TTabSheet;
    lbSpecialTags: TListBox;
    IntervalPanel: TPanel;
    lblRefreshInterval: TLabel;
    HeaderControl1: THeaderControl;
    btnAdd: TBitBtn;
    btnDelete: TBitBtn;
    btnMoveUp: TBitBtn;
    btnMoveDown: TBitBtn;
    btnEdit: TBitBtn;
    edtUpdateInterval: TEdit;
    udUpdateInterval: TUpDown;
    CheckedImages: TImageList;
    tabModuleExplorer: TTabSheet;
    tabCodeBrowsing: TTabSheet;
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
    btnAddDesc: TBitBtn;
    btnEditDesc: TBitBtn;
    btnDeleteDesc: TBitBtn;
    lblBackgroundColour: TLabel;
    cbxBGColour: TColorBox;
    lblTokenLimit: TLabel;
    edtTokenLimit: TEdit;
    udTokenLimit: TUpDown;
    hctlMethodDescriptions: THeaderControl;
    lbxMethodDescriptions: TListBox;
    lblManagedNodesLife: TLabel;
    edtManagedNodesLife: TEdit;
    udManagedNodesLife: TUpDown;
    lblTreeColour: TLabel;
    clbxTreeColour: TColorBox;
    lblForeColour: TLabel;
    lblBackColour: TLabel;
    cbxBackColour: TColorBox;
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
    procedure lbxMethodDescriptionsDblClick(Sender: TObject);
    procedure lbxMethodDescriptionsDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure cbxBackColourChange(Sender: TObject);
    { Private declarations }
  Private
    FTokenFontInfo : Array[Low(TBADITokenType)..High(TBADITokenType)] Of TTokenFontInfo;
  public
    { Public declarations }
    Class Function Execute(VisibleTabs : TVisibleTabs) : Boolean;
  end;

implementation

Uses
  SpecialTagForm, MethodDescriptionForm;

ResourceString
  (** This is a message to be displayed when a tag is not valid **)
  strInvalidTag = 'This is not a valid tag.';

{$R *.DFM}

(**

  This method creates an instance of the options dialogue and sets all the
  controls based on the passed Options parameter. If OK is selected then the
  Options parameter is updated to suit the new options.

  @precon  iInt is the timer interval to be represented in the dialogue,
           DocHelpFile is the directory of the modules help file.
  @postcon Returns true if the OK button on the dialogue was pressed.

  @param   VisibleTabs as a TVisibleTabs
  @return  a Boolean

**)
Class Function TfrmOptions.Execute(VisibleTabs : TVisibleTabs) : Boolean;

Var
  i : TDocOption;
  j : Integer;
  k : TBADITokenType;

Begin
  Result := False;
  With TfrmOptions.Create(Application.MainForm) Do
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
      For j := 0 To cbxFontName.Items.Count - 1 Do
        If cbxFontName.Items[j] = BrowseAndDocItOptions.FontName Then
          Begin
            cbxFontName.ItemIndex := j;
            Break;
          End;
      For k := Low(TBADITokenType) to High(TBADITokenType) Do
        FTokenFontInfo[k] := BrowseAndDocItOptions.TokenFontInfo[k];
      udFontSize.Position := BrowseAndDocItOptions.FontSize;
      rgpBrowsePosition.ItemIndex := Integer(BrowseAndDocItOptions.BrowsePosition);
      mmoExcludeDocFiles.Text := BrowseAndDocItOptions.ExcludeDocFiles.Text;
      For j := 0 To BrowseAndDocItOptions.MethodDescriptions.Count - 1 Do
        lbxMethodDescriptions.Items.Add(BrowseAndDocItOptions.MethodDescriptions[j]);
      cbxBGColour.Selected := BrowseAndDocItOptions.BGColour;
      udTokenLimit.Position := BrowseAndDocItOptions.TokenLimit;
      udManagedNodesLife.Position := BrowseAndDocItOptions.ManagedNodesLife;
      clbxTreeColour.Selected := BrowseAndDocItOptions.TreeColour;
      OptionTab.ActivePage := tabGeneralOptions;
      tabGeneralOptions.TabVisible := vtGeneralOptions In VisibleTabs;
      tabSpecialTags.TabVisible := vtSpecialTags In VisibleTabs;
      tabModuleExplorer.TabVisible := vtModuleExplorer In VisibleTabs;
      tabCodeBrowsing.TabVisible := vtCodeBrowsing In VisibleTabs;
      tabExcludeDocFiles.TabVisible := vtExcludeDocFiles In VisibleTabs;
      tabMethodDescriptions.TabVisible := vtMethodDescriptions In VisibleTabs;
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
          BrowseAndDocItOptions.FontName := cbxFontName.Text;
          BrowseAndDocItOptions.FontSize := udFontSize.Position;
          For k := Low(TBADITokenType) to High(TBADITokenType) Do
            BrowseAndDocItOptions.TokenFontInfo[k] := FTokenFontInfo[k];
          BrowseAndDocItOptions.BrowsePosition := TBrowsePosition(rgpBrowsePosition.ItemIndex);
          BrowseAndDocItOptions.ExcludeDocFiles.Text := mmoExcludeDocFiles.Text;
          BrowseAndDocItOptions.MethodDescriptions.Clear;
          For j := 0 To lbxMethodDescriptions.Items.Count - 1 Do
            BrowseAndDocItOptions.MethodDescriptions.Add(
            lbxMethodDescriptions.Items[j]);
          BrowseAndDocItOptions.BGColour := cbxBGColour.Selected;
          BrowseAndDocItOptions.TokenLimit := udTokenLimit.Position;
          BrowseAndDocItOptions.ManagedNodesLife := udManagedNodesLife.Position;
          BrowseAndDocItOptions.TreeColour := clbxTreeColour.Selected;
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
  j : TBADITokenType;

begin
  For i := 0 To Screen.Fonts.Count - 1 Do
    cbxFontName.Items.Add(Screen.Fonts[i]);
  For j := Low(TBADITokenType) to High(TBADITokenType) Do
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
  CheckedImages.Draw(lb.Canvas, 192, Rect.Top,
    (Integer(lb.Items.Objects[Index]) And iShowInDoc) Div 3);
  iPos := Pos('=', lb.Items[Index]);
  lb.Canvas.TextOut(Rect.Left + 246, Rect.Top, Copy(lb.Items[Index], 1, iPos - 1));
  lb.Canvas.TextOut(Rect.Left + 346, Rect.Top, Copy(lb.Items[Index], iPos + 1,
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
      If X In [192..208] Then
        lbSpecialTags.Items.Objects[iIndex] := TObject(
          Integer(lbSpecialTags.Items.Objects[iIndex]) Xor iShowInDoc);
      lbSpecialTags.Invalidate;
    End;
end;

(**

  This is an on draw item event handler for the method descriptions.

  @precon  None.
  @postcon Draws the str=str information in the list box as 2 columns of
           information without the = sign.

  @param   Control as a TWinControl
  @param   Index   as an Integer
  @param   Rect    as a TRect
  @param   State   as a TOwnerDrawState

**)
procedure TfrmOptions.lbxMethodDescriptionsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);

var
  lb: TListBox;
  iPos: Integer;

begin
  lb := Control As TListBox;
  lb.Canvas.FillRect(Rect);
  iPos := Pos('=', lb.Items[Index]);
  lb.Canvas.TextOut(Rect.Left + 4, Rect.Top, Copy(lb.Items[Index], 1, iPos - 1));
  lb.Canvas.TextOut(Rect.Left + 154, Rect.Top, Copy(lb.Items[Index], iPos + 1,
    Length(lb.Items[Index]) - iPos));
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
        cbxFontColour.Selected := FTokenFontInfo[TBADITokenType(itemIndex)].FForeColour;
        cbxBackColour.Selected := FTokenFontInfo[TBADITokenType(itemIndex)].FBackColour;
        chkBold.Checked := fsBold In FTokenFontInfo[TBADITokenType(itemIndex)].FStyles;
        chkItalic.Checked := fsItalic In FTokenFontInfo[TBADITokenType(itemIndex)].FStyles;
        chkUnderline.Checked := fsUnderline In FTokenFontInfo[TBADITokenType(itemIndex)].FStyles;
        chkStrikeout.Checked := fsStrikeout In FTokenFontInfo[TBADITokenType(itemIndex)].FStyles;
      End;
end;

(**


  This method is an on double click event handler for the Metho Description
  ListView.

  @precon  None.
  @postcon Edits the selected item.


  @param   Sender as a TObject

**)
procedure TfrmOptions.lbxMethodDescriptionsDblClick(Sender: TObject);
begin
  btnEditDescClick(Sender);
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
  boolShowInTree, boolExpand, boolShowInDoc : Boolean;

begin
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

begin
  If TfrmMethodDescriptions.Execute(strPattern, strDescription) Then
    lbxMethodDescriptions.Items.Add(Format('%s=%s', [strPattern, strDescription]));
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
  If lbxMethodDescriptions.ItemIndex > -1 Then
    lbxMethodDescriptions.Items.Delete(lbxMethodDescriptions.ItemIndex);
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
  boolShowInTree, boolExpand, boolShowInDoc : Boolean;
  iPos : Integer;

begin
  If lbSpecialTags.ItemIndex <> -1 Then
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
        And iShowInDoc <> 0;
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
                Integer(boolShowInDoc) * iShowInDoc
              );
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
  iIndex := lbxMethodDescriptions.ItemIndex;
  If iIndex > -1 Then
    Begin
      strPattern := lbxMethodDescriptions.Items.Names[iIndex];
      strDescription := lbxMethodDescriptions.Items.ValueFromIndex[iIndex];
      If TfrmMethodDescriptions.Execute(strPattern, strDescription) Then
        lbxMethodDescriptions.Items[iIndex] := Format('%s=%s', [strPattern,
          strDescription]);
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

  This is an on change event handler for the Back Colour control.

  @precon  None.
  @postcon Updates the background colour with the new selected colour.

  @param   Sender as a TObject

**)
procedure TfrmOptions.cbxBackColourChange(Sender: TObject);
begin
  FTokenFontInfo[TBADITokenType(lbxTokenTypes.ItemIndex)].FBackColour := cbxBackColour.Selected;
end;

(**


  This is an on change event handler for the Font Colour control.

  @precon  None.
  @postcon Updates the internal list of Token Font Information.


  @param   Sender as a TObject

**)
procedure TfrmOptions.cbxFontColourChange(Sender: TObject);
begin
  FTokenFontInfo[TBADITokenType(lbxTokenTypes.ItemIndex)].FForeColour := cbxFontColour.Selected;
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
    Include(FTokenFontInfo[TBADITokenType(lbxTokenTypes.ItemIndex)].FStyles, fsBold)
  Else
    Exclude(FTokenFontInfo[TBADITokenType(lbxTokenTypes.ItemIndex)].FStyles, fsBold);
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
    Include(FTokenFontInfo[TBADITokenType(lbxTokenTypes.ItemIndex)].FStyles, fsItalic)
  Else
    Exclude(FTokenFontInfo[TBADITokenType(lbxTokenTypes.ItemIndex)].FStyles, fsItalic);
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
    Include(FTokenFontInfo[TBADITokenType(lbxTokenTypes.ItemIndex)].FStyles, fsStrikeOut)
  Else
    Exclude(FTokenFontInfo[TBADITokenType(lbxTokenTypes.ItemIndex)].FStyles, fsStrikeOut);
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
    Include(FTokenFontInfo[TBADITokenType(lbxTokenTypes.ItemIndex)].FStyles, fsUnderline)
  Else
    Exclude(FTokenFontInfo[TBADITokenType(lbxTokenTypes.ItemIndex)].FStyles, fsUnderline);
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
