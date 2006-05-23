(**

  This is a generic progress dialogue for use in the ObjectPascalDocWizard.

  @version    0.9
  @date       18 May 2006
  @author     David Hoyle

**)
unit ProgressForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls;

type
  (**
    
    This class represents a modeless progress dialogue for use throughout the
    application.
    
  **)
  TfrmProgress = class(TForm)
    pnlPanel1: TPanel;
    pnlMsg: TPanel;
    prbProgressBar1: TProgressBar;
  private
    { Private declarations }
  public
    { Public declarations }
    Procedure Init(iMax : Integer; strTitle, strMsg : String);
    Procedure UpdateProgress(iPosition : Integer; strMsg : String);
  end;

implementation

{$R *.DFM}

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
  pnlMsg.Caption := strMsg;
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
  pnlMsg.Caption := strMsg;
  Application.ProcessMessages;
end;

end.
