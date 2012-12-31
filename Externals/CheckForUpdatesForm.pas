(**

  This module contains a clas which represents a form for displaying software
  update information using a Singleton Form.

  @Version 1.0
  @Author  David Hoyle
  @Date    31 Dec 2012

**)
Unit CheckForUpdatesForm;

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
  ExtCtrls;

Type
  (** This class represents for the form interface. **)
  TfrmCheckForUpdates = Class(TForm)
    lbInformation: TListBox;
    btnOK: TBitBtn;
    tmFinish: TTimer;
    lblWebSite: TLabel;
    Procedure lbInformationDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    Procedure lbInformationMeasureItem(Control: TWinControl; Index: Integer;
      Var Height: Integer);
    Procedure tmFinishTimer(Sender: TObject);
    Procedure btnOKClick(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var Action: TCloseAction);
    Procedure lblWebSiteClick(Sender: TObject);
    Procedure lblWebSiteMouseEnter(Sender: TObject);
    Procedure lblWebSiteMouseLeave(Sender: TObject);
  Private
    { Private declarations }
    Function CountSpaces(strText: String): Integer;
  Public
    { Public declarations }
    Class Procedure ShowUpdates(strMsg, strURL: String);
    Class Procedure Finish(iSeconds: Integer);
    Class Procedure HideUpdates;
  End;

Implementation

{$R *.dfm}

Uses
  ShellAPI,
  Themes;

Var
  (** This is an private variable for the Singleton Form reference. **)
  frm: TfrmCheckForUpdates;

{ TfrmCheckForUpdatesForm }

(**

  This method frees the nils the form variable if the form is valid.

  @precon  None.
  @postcon Frees and Nils the form.

**)
Class Procedure TfrmCheckForUpdates.HideUpdates;

Begin
  If Assigned(frm) Then
    FreeAndNil(frm);
End;

(**

  This is an on Draw Item event handler for the list box control.

  @precon  None.
  @postcon Renders the text with the assigned colour (in .Objects[]).

  @param   Control as a TWinControl
  @param   Index   as an Integer
  @param   Rect    as a TRect
  @param   State   as a TOwnerDrawState

**)
Procedure TfrmCheckForUpdates.lbInformationDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);

Var
  strText: String;
  iIndent: Integer;
  lbx: TListBox;

Begin
  lbx := (Control As TListBox);
  lbx.Canvas.Brush.Color := StyleServices.GetSystemColor(clWindow);
  lbx.Canvas.Font.Color  := StyleServices.GetSystemColor(clWindowText);
  If odSelected In State Then
    Begin
      lbx.Canvas.Brush.Color := StyleServices.GetSystemColor(clHighlight);
      lbx.Canvas.Font.Color  := StyleServices.GetSystemColor(clHighlightText);
      lbx.Canvas.DrawFocusRect(Rect);
    End;
  strText := lbx.Items[Index];
  iIndent := CountSpaces(strText);
  strText := Trim(strText);
  lbx.Canvas.FillRect(Rect);
  Inc(Rect.Left, 4 + 5 * iIndent);
  Dec(Rect.Right, 4);
  DrawText(lbx.Canvas.Handle, PChar(strText), Length(strText), Rect,
    DT_LEFT Or DT_VCENTER Or DT_NOCLIP Or DT_NOPREFIX Or DT_WORDBREAK);
End;

(**

  This is an on Measure Item event handler for the list box control.

  @precon  None.
  @postcon Adjusts the height of the item if the text needs multiple lines.

  @param   Control as a TWinControl
  @param   Index   as an Integer
  @param   Height  as an Integer as a reference

**)
Procedure TfrmCheckForUpdates.lbInformationMeasureItem(Control: TWinControl;
  Index: Integer; Var Height: Integer);

Var
  strText: String;
  R      : TRect;
  iIndent : Integer;

Begin
  strText := (Control As TListBox).Items[Index];
  iIndent := CountSpaces(strText);
  strText := Trim(strText);
  R       := (Control As TListBox).ItemRect(Index);
  Inc(R.Left, 4 + 5 * iIndent);
  Dec(R.Right, 4);
  DrawText((Control As TListBox).Canvas.Handle, PChar(strText), Length(strText), R,
    DT_CALCRECT Or DT_LEFT Or DT_VCENTER Or DT_NOCLIP Or DT_NOPREFIX Or DT_WORDBREAK);
  //If R.Bottom - R.Top > Height Then
  Height := R.Bottom - R.Top + 4;
End;

(**

  This is an on click event handler for the WebSite label.

  @precon  None.
  @postcon Opens the URL on the label.

  @param   Sender as a TObject

**)
Procedure TfrmCheckForUpdates.lblWebSiteClick(Sender: TObject);

Begin
  ShellExecute(Application.Handle, 'OPEN', PChar(lblWebSite.Caption), '', '',
    SW_SHOWNORMAL);
End;

(**

  This is an on mouse enter event handler for the web site label.

  @precon  None.
  @postcon Adds the underlining to the label.

  @param   Sender as a TObject

**)
Procedure TfrmCheckForUpdates.lblWebSiteMouseEnter(Sender: TObject);

Begin
  lblWebSite.Font.Style := lblWebSite.Font.Style + [fsUnderline];
End;

(**

  This is an on mouse leave event handler for the web site label.

  @precon  None.
  @postcon Remove the underlining from the label.

  @param   Sender as a TObject

**)
Procedure TfrmCheckForUpdates.lblWebSiteMouseLeave(Sender: TObject);

Begin
  lblWebSite.Font.Style := lblWebSite.Font.Style - [fsUnderline];
End;

(**

  This method displays the message window and adds the message to the listbox.

  @precon  None.
  @postcon Displays the message window and adds the message to the listbox.

  @param   strMsg  as a String
  @param   strURL  as a String

**)
Class Procedure TfrmCheckForUpdates.ShowUpdates(strMsg, strURL: String);

Var
  sl: TStringList;
  i : Integer;

Begin
  If frm = Nil Then
    frm := TfrmCheckForUpdates.Create(Nil);
  If Not frm.Visible Then
    Begin
      frm.lbInformation.Clear;
      frm.Show;
    End;
  sl := TStringList.Create;
  Try
    sl.Text := strMsg;
    For i   := 0 To sl.Count - 1 Do
      If sl[i] <> '' Then
        frm.lbInformation.Items.Add(sl[i]);
  Finally
    sl.Free;
  End;
  frm.lbInformation.ItemIndex := frm.lbInformation.Items.Count - 1;
  frm.lblWebSite.Caption      := strURL;
  Application.ProcessMessages;
End;

(**

  This is an on timer event handler for the Timer control.

  @precon  None.
  @postcon Closes the dialogue.

  @param   Sender as a TObject

**)
Procedure TfrmCheckForUpdates.tmFinishTimer(Sender: TObject);

Begin
  Close;
End;

(**

  This is an on click event handler for the OK button.

  @precon  None.
  @postcon Closes the dialogue.

  @param   Sender as a TObject

**)
Procedure TfrmCheckForUpdates.btnOKClick(Sender: TObject);

Begin
  Close;
End;

(**

  This method returns the number of spaces at the start of a line of text.

  @precon  None.
  @postcon Returns the number of spaces at the start of a line of text.

  @param   strText as a String
  @return  an Integer

**)
Function TfrmCheckForUpdates.CountSpaces(strText: String): Integer;

Var
  i : Integer;
  
Begin
  Result := 0;
  For i := 1 To Length(strText) Do
    If strText[i] <>#32 Then
      Break
    Else
      Inc(Result);
End;

(**

  This method shows the dialogue in a modal form so the user can see that there
  is information to be read.

  @precon  None.
  @postcon Shows the dialogue in a modal form so the user can see that there
           is information to be read.

  @param   iSeconds as an Integer

**)
Class Procedure TfrmCheckForUpdates.Finish(iSeconds: Integer);

Begin
  frm.btnOK.Enabled     := True;
  frm.tmFinish.Interval := iSeconds * 1000;
  frm.tmFinish.Enabled  := True;
End;

(**

  This is an on close event handler for the form.

  @precon  None.
  @postcon Disables the timer.

  @param   Sender as a TObject
  @param   Action as a TCloseAction as a reference

**)
Procedure TfrmCheckForUpdates.FormClose(Sender: TObject; Var Action: TCloseAction);

Begin
  tmFinish.Enabled := False;
End;

(** Nothing the intialise. **)
Initialization

(** Make sure the form is freed is its been created. **)
Finalization
  If frm <> Nil Then
    frm.Free;
End.
