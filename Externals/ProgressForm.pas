(**

  This is a generic progress dialogue for use in the ObjectPascalDocWizard.

  @version    0.9
  @date       29 Mar 2009
  @author     David Hoyle

**)
unit ProgressForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, DGHEllipsisLabel;

type
  (**

    This class represents a modeless progress dialogue for use throughout the
    application.

  **)
  TfrmProgress = class(TForm)
    pnlPanel1: TPanel;
    prbProgressBar1: TProgressBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
procedure TfrmProgress.FormCreate(Sender: TObject);
begin
  FEllipsisLabel := TDGHEllipsisLabel.Create(Nil);
  FEllipsisLabel.Parent := Self;
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
  prbProgressBar1.Max := iMax;
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
  prbProgressBar1.Position := iPosition;
  FEllipsisLabel.Caption := strMsg;
  Application.ProcessMessages;
end;

end.
