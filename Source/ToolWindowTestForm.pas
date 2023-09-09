unit ToolWindowTestForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmMyTestForm = class(TForm, iDispatch)
    lblHello: TLabel;
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMyTestForm: TfrmMyTestForm;

implementation

uses
  CodeSiteLogging;

{$R *.dfm}

procedure TfrmMyTestForm.FormDestroy(Sender: TObject);
begin
  CodeSite.TraceMethod(Self, 'FormDestroy', tmoTiming);
end;

procedure TfrmMyTestForm.FormCreate(Sender: TObject);
begin
  CodeSite.TraceMethod(Self, 'FormCreate', tmoTiming);
end;

end.

