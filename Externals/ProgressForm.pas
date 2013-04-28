(**

  This is a generic progress dialogue for use in the ObjectPascalDocWizard.

  @version    0.9
  @date       28 Apr 2013
  @author     David Hoyle

**)
unit ProgressForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, DGHEllipsisLabel, StdCtrls, Buttons;

type
  (**

    This class represents a modeless progress dialogue for use throughout the
    application.

  **)
  TfrmProgress = class(TForm)
    pnlPanel1: TPanel;
    prbProgressBar1: TProgressBar;
    pnlInfo: TPanel;
    pnlButton: TPanel;
    btnCancel: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
    FEllipsisLabel : TDGHEllipsisLabel;
  public
    { Public declarations }
    Procedure Init(iMax : Integer; strTitle, strMsg : String);
    Procedure UpdateProgress(iPosition : Integer; strMsg : String);
  end;

implementation

{$R *.DFM}

(**

  This is an OnFormCreate Event Hanlder for the TfrmProgress class.

  @precon  None.
  @postcon Creates an ellipsis path control and aligns it to the client area.

  @param   Sender as a TObject

**)
procedure TfrmProgress.btnCancelClick(Sender: TObject);

Const
  strMsg = 'Are you sure you want to cancel the scanning and parsing?';

begin
  If MessageDlg(strMsg, mtConfirmation, [mbYes, mbNo], 0) = mrYes Then
    Abort;
end;

(**

  This is an on create event handler for the form.

  @precon  None.
  @postcon Creates a DGH Ellipsis label control.

  @param   Sender as a TObject

**)
procedure TfrmProgress.FormCreate(Sender: TObject);
begin
  FEllipsisLabel := TDGHEllipsisLabel.Create(Nil);
  FEllipsisLabel.Parent := pnlInfo;
  FEllipsisLabel.Align := alClient;
end;

(**

  This is an OnFormDestroy Event Hanlder for the TfrmProgress class.

  @precon  None.
  @postcon Frees the ellipsis path control.

  @param   Sender as a TObject

**)
procedure TfrmProgress.FormDestroy(Sender: TObject);
begin
  FEllipsisLabel.Free;
end;

(**

  This method initialises the progress dialogue by adding a message to the
  form and setting the maximum amount of progress, then show the dialogue
  on the screen.

  @precon  iMax is the maximum range of the progress meter, strTitle is the
           title of the dialogue and strMsg is the initial message in the
           dialogue.
  @postcon Initialises the progress form.

  @param   iMax     as an Integer
  @param   strTitle as a String
  @param   strMsg   as a String

**)
Procedure TfrmProgress.Init(iMax: Integer; strTitle, strMsg: String);
begin
  Caption := strTitle;
  FEllipsisLabel.Caption := strMsg;
  If iMax > 0 Then
    Begin
      prbProgressBar1.Style := pbstNormal;
      prbProgressBar1.Max := iMax;
    End Else
      prbProgressBar1.Style := pbstMarquee;
  prbProgressBar1.Position := 0;
  Show;
  Application.ProcessMessages;
end;

(**

  This method of the form updates the progress meter.

  @precon  iPosition is the updated position of the progress meter and strMsg is
           an updated message for the dialogue.
  @postcon Updates the display with progress.

  @param   iPosition as an Integer
  @param   strMsg    as a String

**)
procedure TfrmProgress.UpdateProgress(iPosition: Integer; strMsg : String);

begin
  If prbProgressBar1.Style = pbstNormal Then
    Begin
      prbProgressBar1.Position := iPosition;
      prbProgressBar1.Position := iPosition - 1;
      prbProgressBar1.Position := iPosition;
      FEllipsisLabel.Caption := strMsg;
    End;
  Application.ProcessMessages;
end;

end.
