Program LayeredExplorerImagesTest;

{$R 'LayeredExplorerImages.res' '..\LayeredExplorerImages.RC'}


Uses
  Vcl.Forms,
  LayeredExplorerImagesTestMainForm In 'Source\LayeredExplorerImagesTestMainForm.pas' {Form1};

{$R *.res}


Begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
End.
