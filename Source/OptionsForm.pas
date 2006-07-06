(**

  This module provides an enumerate set for the visible display options and
  a dialogue for setting those options.

  @Date    06 Jul 2006
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
    BottomPanel: TPanel;
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
    TabSheet1: TTabSheet;
    rgEditorPosition: TRadioGroup;
    CheckedImages: TImageList;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure btnMoveUpClick(Sender: TObject);
    procedure btnMoveDownClick(Sender: TObject);
    procedure lbSpecialTagsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    { Private declarations }
  private
  public
    { Public declarations }
    Class Function Execute(var Options : TDocOptions; var iInt : Integer;
      var DocHelpFile : String; var iBrowsePosition : Integer) : Boolean;
  end;

implementation

Uses
  SpecialTagForm, ModuleExplorerFrame;

ResourceString
  (** This is a message to be displayed when a tag is not valid **)
  strInvalidTag = 'This is not a valid tag.';

{$R *.DFM}

(**

  This method creates an instance of the options dialogue and sets all the
  controls based on the passed Options parameter. If OK is selected then
  the Options parameter is updated to suit the new options.

  @precon  Options is a set of TDocOptions to be represented in the dialogue,
           iInt is the timer interval to be represented in the dialogue,
           DocHelpFile is the directory of the modules help file.
  @postcon Returns true if the OK button on the dialogue was pressed.

  @param   Options     as a TDocOptions as a reference
  @param   iInt        as an Integer as a reference
  @param   DocHelpFile as a String as a reference
  @param   iBrowsePosition as an Integer as a reference
  @return  a Boolean

**)
Class Function TfrmOptions.Execute(var Options : TDocOptions; var iInt : Integer;
  var DocHelpFile : String; var iBrowsePosition : Integer) : Boolean;

Var
  i : TDocOption;
  j : Integer;

Begin
  Result := False;
  With TfrmOptions.Create(Nil) Do
    Try
      For i := Low(TDocOption) To High(TDocOption) Do
        Begin
          clbOptions.Items.Add(DocOptionInfo[i].Description);
          clbOptions.Checked[Integer(i)] := i In Options;
        End;
      For j := 0 To SpecialTags.Count - 1 Do
        lbSpecialTags.Items.AddObject(SpecialTags[j], SpecialTags.Objects[j]);
      udUpdateInterval.Position := iInt;
      If FileExists(DocHelpFile) Then
        HelpFileDir.Directory := DocHelpFile;
      rgEditorPosition.ItemIndex := iBrowsePosition;
      If ShowModal = mrOK Then
        Begin
          Result := True;
          Options := [];
          For i := Low(TDocOption) To High(TDocOption) Do
            If clbOptions.Checked[Integer(i)] Then
              Options := Options + [i];
          SpecialTags.Clear;
          For j := 0 To lbSpecialTags.Items.Count - 1 Do
            SpecialTags.AddObject(lbSpecialTags.Items[j],
            lbSpecialTags.Items.Objects[j]);
          iInt := udUpdateInterval.Position;
          DocHelpFile := HelpFileDir.Directory;
          iBrowsePosition := rgEditorPosition.ItemIndex;
        End;
    Finally
      Free;
    End;
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
procedure TfrmOptions.lbSpecialTagsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);

Var
  lb : TListBox;
  iPos : Integer;

begin
  lb := Control As TListBox;
  lb.Canvas.FillRect(Rect);
  CheckedImages.Draw(lb.Canvas, 32, lb.ItemHeight * Index,
    Integer(lb.Items.Objects[Index]) And iShowInTree);
  CheckedImages.Draw(lb.Canvas, 112, lb.ItemHeight * Index,
    (Integer(lb.Items.Objects[Index]) And iAutoExpand) Div 2);
  iPos := Pos('=', lb.Items[Index]);
  lb.Canvas.TextOut(Rect.Left + 160, Rect.Top, Copy(lb.Items[Index], 1, iPos - 1));
  lb.Canvas.TextOut(Rect.Left + 260, Rect.Top, Copy(lb.Items[Index], iPos + 1,
    Length(lb.Items[Index]) - iPos));
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
